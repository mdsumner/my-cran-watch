# rOpenSci pkgcheck, opt-in ------------------------------------------------
#
# config `pkgcheck:` lists the packages to check (nothing runs otherwise).
# Each is a bare name or {package, repo, goodpractice}; repo defaults to
# repo_map.  pkgcheck needs a git checkout of the package (it inspects the
# repo, CI and GitHub settings, not just the source), so it runs from its
# own manual workflow and writes `sections$pkgcheck` into the same
# docs/pkg/<pkg>.json the details page renders.

pkgcheck_targets <- function(cfg) {
  pc <- cfg$pkgcheck %||% list()
  gp_default <- isTRUE(pc$goodpractice)
  rows <- lapply(pc$packages %||% list(), function(e) {
    if (is.character(e)) e <- list(package = e)
    repo <- e$repo %||% cfg$repo_map[[e$package]]
    if (is.null(repo)) {
      stop(sprintf("pkgcheck: %s has no repo; add it to repo_map or give `repo:`", e$package))
    }
    data.frame(package = e$package, repo = repo,
               goodpractice = if (is.null(e$goodpractice)) gp_default else isTRUE(e$goodpractice),
               stringsAsFactors = FALSE)
  })
  if (!length(rows)) {
    return(data.frame(package = character(), repo = character(),
                      goodpractice = logical(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out[!duplicated(out$package), , drop = FALSE]
}

# one row per check: check (pkgcheck's output name, e.g. left_assign,
# has_contrib, covr; "other" for R CMD check and unnamed lines), result
# (pass, fail, watch), message.  Uses pkgcheck's internal per-check
# summarisers for the names; the markdown summary is the authority for what
# was reported, so named lines it lacks are dropped and lines it has that
# no named check produced are kept as "other".
pkgcheck_rows <- function(chk) {
  ns <- asNamespace("pkgcheck")
  strip <- function(x) trimws(sub("^- :[a-z_]+: ", "", x))
  result_of <- function(x) {
    if (grepl(":eyes:", x, fixed = TRUE)) "watch"
    else if (grepl(":heavy_multiplication_x:", x, fixed = TRUE)) "fail"
    else "pass"
  }
  outs <- sub("^output_pkgchk_", "", grep("^output_pkgchk_", ls(ns), value = TRUE))
  if (!"covr" %in% names(chk$goodpractice)) outs <- setdiff(outs, "covr")
  named <- list()
  for (nm in outs) {
    line <- tryCatch(ns$summarise_check(chk, nm, ns), error = function(e) NULL)
    if (is.null(line)) next
    type <- attr(line, "check_type") %||% ""
    for (l in line) {
      res <- result_of(l)
      # pkgcheck's own "watch" types: shown, never a failure
      if (grepl("^watch", type) && res == "pass") res <- "watch"
      if (grepl("_watch$", type) && res == "fail") res <- "watch"
      named[[length(named) + 1L]] <- data.frame(check = nm, result = res,
                                                message = strip(l), stringsAsFactors = FALSE)
    }
  }
  named <- if (length(named)) do.call(rbind, named)
           else data.frame(check = character(), result = character(), message = character(),
                           stringsAsFactors = FALSE)
  md <- pkgcheck::checks_to_markdown(chk, render = FALSE)
  brk <- grep("^---", md)
  md <- md[seq_len(if (length(brk)) brk[1] - 1L else length(md))]
  md <- grep("^- :(heavy_check_mark|heavy_multiplication_x|eyes):", md, value = TRUE)
  named <- named[named$message %in% strip(md), , drop = FALSE]
  extra <- md[!strip(md) %in% named$message]
  rbind(named, data.frame(check = rep("other", length(extra)),
                          result = vapply(extra, result_of, character(1), USE.NAMES = FALSE),
                          message = strip(extra), stringsAsFactors = FALSE))
}

# rows -> a details section.  Failures are the problem rows and the count;
# watch items are listed after them but not counted; `ignore` drops checks
# by name.
pkgcheck_section <- function(rows, ignore = character(), sha = NA, report = NULL,
                             goodpractice = FALSE) {
  rows <- rows[!rows$check %in% ignore, , drop = FALSE]
  n <- table(factor(rows$result, levels = c("pass", "fail", "watch")))
  problems <- rows[rows$result %in% c("fail", "watch"), , drop = FALSE]
  problems <- problems[order(problems$result != "fail"), , drop = FALSE]
  rownames(problems) <- NULL
  list(summary = sprintf("%d passed, %d failed, %d to watch (commit %s, %s%s)",
                         n[["pass"]], n[["fail"]], n[["watch"]],
                         if (is.na(sha)) "unknown" else substr(sha, 1, 7), today(),
                         if (goodpractice) ", with goodpractice" else ""),
       problems = problems, count = n[["fail"]], report = report, sha = sha)
}

# write one section into docs/pkg/<pkg>.json, keeping the other sections,
# and its count into docs/pkg/index.json
write_details_section <- function(pkg, name, section, version = NULL, dir = "docs/pkg") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, paste0(pkg, ".json"))
  rec <- if (file.exists(path)) jsonlite::read_json(path) else list(package = pkg)
  rec$version <- rec$version %||% version
  rec$updated <- rec$updated %||% today()
  rec$sections <- rec$sections %||% list()
  rec$sections[[name]] <- section
  jsonlite::write_json(rec, path, auto_unbox = TRUE, pretty = TRUE, dataframe = "rows",
                       null = "null")
  idx_path <- file.path(dir, "index.json")
  index <- if (file.exists(idx_path)) jsonlite::read_json(idx_path) else list()
  index[[pkg]][[name]] <- section$count %||% nrow(section$problems)
  jsonlite::write_json(index, idx_path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}
