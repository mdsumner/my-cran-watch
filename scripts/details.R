#!/usr/bin/env Rscript
# per-package details for the dashboard: docs/pkg/<pkg>.json holds one
# entry per section (urlchecker, checktor), docs/pkg/index.json a count per
# package and section for the main table.  Runs from its own workflow on
# a slower cadence than the watcher, since it downloads every source
# tarball.  Another checker adds a section by adding a function to
# `sections` returning list(summary = <one line>, problems = <data.frame>),
# optionally with `count`, the number shown on the dashboard (default: rows).
#
# Run from the repo root:  Rscript scripts/details.R [pkg ...]

for (f in list.files("R", full.names = TRUE)) source(f)
cfg <- read_config()
db  <- tools::CRAN_package_db()
mine <- fetch_my_packages(cfg, db)
only <- commandArgs(trailingOnly = TRUE)
pkgs <- if (length(only)) only else mine$package[mine$role == "maintainer"]

sections <- list(
  urlchecker = function(dir) {
    # urlchecker >= 1.1 errors on flagged URLs unless fail = FALSE
    args <- list(dir, progress = FALSE)
    if ("fail" %in% names(formals(urlchecker::url_check))) args$fail <- FALSE
    res <- do.call(urlchecker::url_check, args)
    df <- data.frame(
      url     = as.character(res$URL),
      status  = as.character(res$Status),
      message = as.character(res$Message),
      new     = as.character(res$New),
      from    = vapply(res$From, paste, character(1), collapse = ", "),
      stringsAsFactors = FALSE)
    list(summary = if (nrow(df)) sprintf("%d URL problem(s)", nrow(df)) else "all URLs ok",
         problems = df)
  },
  # checktor: CRAN-reviewer requirements R CMD check does not surface.
  # issues() is its tidy table of findings (all severities, including
  # "opinion"); columns are kept as checktor names them
  checktor = function(dir) {
    res <- checktor::checktor(dir, verbose = FALSE, progress = FALSE)
    df <- as.data.frame(checktor::issues(res), stringsAsFactors = FALSE)
    for (col in names(df)) {
      if (is.list(df[[col]])) df[[col]] <- vapply(df[[col]], paste, character(1), collapse = ", ")
    }
    sev <- if ("severity" %in% names(df)) table(df$severity) else NULL
    list(summary = if (!nrow(df)) "no findings"
                   else if (length(sev)) paste(sprintf("%d %s", sev, names(sev)), collapse = ", ")
                   else sprintf("%d finding(s)", nrow(df)),
         problems = df,
         # opinion-tier findings are listed but not counted on the dashboard
         count = if ("severity" %in% names(df)) sum(df$severity %in% c("policy", "robustness"))
                 else nrow(df))
  }
)

out_dir <- "docs/pkg"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
idx_path <- file.path(out_dir, "index.json")
index <- if (file.exists(idx_path)) jsonlite::read_json(idx_path) else list()

work <- tempfile("details"); dir.create(work)
for (pkg in pkgs) {
  msg("details: %s", pkg)
  src <- tryCatch({
    dl <- utils::download.packages(pkg, destdir = work, type = "source",
                                   repos = "https://cloud.r-project.org", quiet = TRUE)
    utils::untar(dl[1, 2], exdir = work)
    file.path(work, pkg)
  }, error = function(e) {
    # ::warning:: shows on the run page, which is readable without the log
    msg("::warning::details: %s download failed: %s", pkg, conditionMessage(e)); NULL
  })
  if (is.null(src)) next
  # keep sections written by other workflows (pkgcheck)
  path <- file.path(out_dir, paste0(pkg, ".json"))
  prev <- if (file.exists(path)) jsonlite::read_json(path)$sections else NULL
  rec <- list(package = pkg,
              version = mine$version[match(pkg, mine$package)],
              updated = today(), sections = prev[setdiff(names(prev), names(sections))])
  for (s in names(sections)) {
    rec$sections[[s]] <- tryCatch(sections[[s]](src), error = function(e) {
      # a failed section must not read as clean: warn on the run page
      msg("::warning::details: %s %s failed: %s", pkg, s, conditionMessage(e))
      list(summary = sprintf("failed: %s", conditionMessage(e)), problems = data.frame())
    })
    index[[pkg]][[s]] <- rec$sections[[s]]$count %||% nrow(rec$sections[[s]]$problems)
  }
  jsonlite::write_json(rec, path,
                       auto_unbox = TRUE, pretty = TRUE, dataframe = "rows")
  unlink(src, recursive = TRUE)
}
jsonlite::write_json(index, idx_path, auto_unbox = TRUE, pretty = TRUE)
done <- intersect(pkgs, names(index))
msg("::notice::details: %d of %d package(s) written: %s", length(done), length(pkgs),
    paste(vapply(done, function(p) sprintf("%s (%s)", p,
      paste(sprintf("%s %s", names(index[[p]]), unlist(index[[p]])), collapse = ", ")),
      character(1)), collapse = "; "))
