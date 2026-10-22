# release board -------------------------------------------------------------
#
# Packages on their way to CRAN.  A package is tracked while it is listed
# under `releasing:` in config.yml or has an open issue titled
# "release: <pkg>" (or "release: <pkg>@<ref>").  For each one the board
# records, against the GitHub source at that ref:
#
#   GitHub CI       the check runs on the ref's head commit (read, not rerun)
#   checktor,       run here on a clone of that commit (R/sections.R),
#   urlchecker      only when the commit changes
#   win-builder,    result links pasted into the release issue (by you or
#   mac-builder     the local helper); the check logs are fetched and parsed
#   CRAN            incoming folder (docs/queue.json), then published when
#                   CRAN's version catches up with the ref's version
#
# docs/release.json is the board; each release issue carries one status
# comment, edited in place.  The issue is closed when the package is
# published.

release_marker <- "<!-- my-cran-watch:release-status -->"

# releasing: entries are a bare name or {package, repo, ref}
normalize_releasing <- function(x) {
  if (!length(x)) return(data.frame(package = character(), repo = character(),
                                    ref = character(), stringsAsFactors = FALSE))
  rows <- lapply(x, function(e) {
    if (is.character(e)) e <- list(package = e)
    data.frame(package = e$package, repo = e$repo %||% NA_character_,
               ref = e$ref %||% NA_character_, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[!duplicated(out$package), , drop = FALSE]
}

# "release: vapour" or "release: vapour@fix-branch" -> list(package, ref)
parse_release_title <- function(title) {
  m <- regmatches(title, regexec("^\\s*release:\\s*([A-Za-z0-9.]+)(@(\\S+))?\\s*$", title))[[1]]
  if (!length(m)) return(NULL)
  list(package = m[2], ref = if (nzchar(m[4])) m[4] else NA_character_)
}

# builder result links in issue text.  win-builder mails a link to a
# directory holding 00check.log; mac-builder's upload answers with a
# results link.  Returns data.frame(builder, url) in order of appearance.
builder_patterns <- c(
  "win-builder" = "https?://win-builder\\.r-project\\.org/[A-Za-z0-9]+/?",
  "mac-builder" = "https?://mac\\.r-project\\.org/macbuilder/results/[A-Za-z0-9_.-]+/?")

builder_urls <- function(texts) {
  out <- list()
  for (txt in texts) {
    for (b in names(builder_patterns)) {
      hits <- regmatches(txt, gregexpr(builder_patterns[[b]], txt))[[1]]
      for (u in hits) out[[length(out) + 1L]] <- data.frame(builder = b, url = u,
                                                            stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(data.frame(builder = character(), url = character(),
                                      stringsAsFactors = FALSE))
  out <- do.call(rbind, out)
  out[!duplicated(out$url), , drop = FALSE]
}

# an R CMD check log -> list(status, summary, version, r); status NA while
# the check has not finished (no "Status:" line yet)
parse_check_log <- function(lines) {
  st <- grep("^Status: ", lines, value = TRUE)
  ver <- regmatches(lines, regexpr("this is package .* version '[^']+'", lines))
  r <- grep("^\\* using R ", lines, value = TRUE)
  summary <- if (length(st)) sub("^Status: ", "", utils::tail(st, 1)) else NA_character_
  status <- if (is.na(summary)) NA_character_
            else if (grepl("ERROR", summary)) "ERROR"
            else if (grepl("WARNING", summary)) "WARNING"
            else if (grepl("NOTE", summary)) "NOTE"
            else if (grepl("^OK", summary)) "OK"
            else "ERROR"
  list(status = status, summary = summary,
       version = if (length(ver)) sub(".*version '([^']+)'.*", "\\1", ver[1]) else NA_character_,
       r = if (!length(r)) NA_character_
           else if (grepl("Under development", r[1])) "R-devel"
           else sub("^\\* using (R version [0-9.]+).*$", "\\1", r[1]))
}

read_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  suppressWarnings(readLines(con, warn = FALSE))
}

# hrefs in an HTML directory listing, resolved against base
listing_links <- function(html, base) {
  h <- unlist(regmatches(html, gregexpr('href="[^"?#]+"', html)))
  h <- sub('^href="', "", sub('"$', "", h))
  h <- h[!grepl("^(/|\\.\\.|mailto:)", h)]
  ifelse(grepl("^https?://", h), h, paste0(sub("/?$", "/", base), h))
}

# find and parse 00check.log under a builder results link: the log
# itself, a directory listing holding it, or one level down (a .Rcheck
# directory).  Returns parse_check_log() plus log_url; status NA = pending.
fetch_builder <- function(url, read = read_url) {
  pending <- list(status = NA_character_, summary = "waiting for results",
                  version = NA_character_, r = NA_character_, log_url = NA_character_)
  try_log <- function(u) {
    txt <- tryCatch(read(u), error = function(e) NULL)
    if (is.null(txt)) return(NULL)
    if (any(grepl("^Status: ", txt)) || any(grepl("^\\* using log directory", txt))) {
      res <- parse_check_log(txt)
      if (is.na(res$status)) res$summary <- "still running"
      return(c(res, log_url = u))
    }
    NULL
  }
  if (grepl("check\\.log$", url)) return(try_log(url) %||% pending)
  html <- tryCatch(read(url), error = function(e) NULL)
  if (is.null(html)) return(pending)
  # some results pages show the log inline
  if (any(grepl("^Status: ", html))) return(c(parse_check_log(html), log_url = url))
  links <- listing_links(html, url)
  for (u in links[grepl("check\\.log$", links)]) {
    res <- try_log(u)
    if (!is.null(res)) return(res)
  }
  for (d in links[grepl("\\.Rcheck/?$", links)]) {
    sub_html <- tryCatch(read(d), error = function(e) NULL)
    if (is.null(sub_html)) next
    sub_links <- listing_links(sub_html, d)
    for (u in sub_links[grepl("check\\.log$", sub_links)]) {
      res <- try_log(u)
      if (!is.null(res)) return(res)
    }
  }
  pending
}

# GitHub source of a ref: list(sha, version) or NULL
ref_info <- function(repo, ref) {
  path <- sprintf("repos/%s/commits/%s", repo, utils::URLencode(ref, reserved = TRUE))
  commit <- gh_json(c("api", path))
  if (is.null(commit$sha)) return(NULL)
  desc <- gh(c("api", "-H", shQuote("Accept: application/vnd.github.raw"),
               sprintf("repos/%s/contents/DESCRIPTION?ref=%s", repo, commit$sha)))
  version <- NA_character_
  if (desc$status == 0) {
    d <- tryCatch(read.dcf(textConnection(paste(desc$output, collapse = "\n")),
                           fields = "Version"), error = function(e) NULL)
    if (!is.null(d)) version <- unname(d[1, "Version"])
  }
  list(sha = commit$sha, version = version)
}

default_branch <- function(repo) {
  gh_json(c("api", sprintf("repos/%s", repo)))$default_branch %||% "main"
}

# GitHub Actions (and other check runs) on a commit, summarised
ci_status <- function(repo, sha) {
  res <- gh_json(c("api", sprintf("repos/%s/commits/%s/check-runs?per_page=100", repo, sha)))
  runs <- res$check_runs
  if (is.null(runs) || !NROW(runs)) return(list(status = "none", summary = "no checks on this commit"))
  concl <- ifelse(runs$status == "completed", runs$conclusion %||% "", "pending")
  bad <- concl %in% c("failure", "timed_out", "cancelled", "action_required", "startup_failure")
  status <- if (any(bad)) "fail" else if (any(concl == "pending")) "pending" else "pass"
  summary <- if (any(bad)) sprintf("%d of %d failing: %s", sum(bad), length(concl),
                                   paste(unique(runs$name[bad]), collapse = ", "))
             else if (status == "pending") sprintf("%d of %d still running",
                                                   sum(concl == "pending"), length(concl))
             else sprintf("%d checks passed", length(concl))
  list(status = status, summary = summary)
}

# shallow clone of one commit; returns the directory or NULL
clone_commit <- function(repo, sha, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  url <- sprintf("https://github.com/%s.git", repo)
  ok <- run("git", c("-C", dir, "init", "-q"), allow_fail = TRUE)$status == 0 &&
    run("git", c("-C", dir, "fetch", "-q", "--depth", "1", url, sha), allow_fail = TRUE)$status == 0 &&
    run("git", c("-C", dir, "checkout", "-q", "FETCH_HEAD"), allow_fail = TRUE)$status == 0
  if (ok) dir else NULL
}

# is version a newer than b (either may be NA)
version_newer <- function(a, b) {
  if (is.na(a)) return(FALSE)
  if (is.na(b)) return(TRUE)
  package_version(a) > package_version(b)
}

# where the package is: published > in queue > ready > preparing
release_stage <- function(rec) {
  if (isTRUE(rec$published)) return("published")
  if (!is.null(rec$queue)) return("submitted")
  ok <- function(s) !is.null(s) && !is.na(s) && s %in% c("OK", "NOTE")
  builders_ok <- all(vapply(c("win-builder", "mac-builder"), function(b) {
    x <- Filter(function(r) identical(r$builder, b), rec$builders %||% list())
    length(x) > 0 && ok(x[[length(x)]]$status)
  }, logical(1)))
  if (identical(rec$ci$status, "pass") && builders_ok) "ready" else "preparing"
}

# the status comment for a release issue (ASCII only)
release_comment <- function(rec, board_url) {
  cell <- function(s) if (is.null(s)) "not run" else s$summary %||% "not run"
  last_builder <- function(b) {
    x <- Filter(function(r) identical(r$builder, b), rec$builders %||% list())
    if (!length(x)) return("paste the results link here")
    r <- x[[length(x)]]
    txt <- r$summary %||% "waiting for results"
    if (!is.na(r$r %||% NA)) txt <- sprintf("%s (%s)", txt, r$r)
    if (!is.na(r$version %||% NA) && !identical(r$version, rec$version)) {
      txt <- sprintf("%s, but checked version %s", txt, r$version)
    }
    sprintf("%s [results](%s)", txt, r$log_url %||% r$url)
  }
  queue <- if (isTRUE(rec$published)) sprintf("published %s", rec$cran_version)
           else if (!is.null(rec$queue)) sprintf("in %s since %s", rec$queue$folder, rec$queue$first_seen)
           else "not submitted"
  paste(c(
    release_marker,
    sprintf("**%s %s** from `%s@%s` (%s), CRAN has %s. Stage: **%s**.",
            rec$package, rec$version %||% "?", rec$repo, rec$ref, substr(rec$sha, 1, 7),
            rec$cran_version %||% "nothing yet", rec$stage),
    "",
    "| step | result |",
    "|---|---|",
    sprintf("| GitHub CI | %s |", cell(rec$ci)),
    sprintf("| checktor | %s |", cell(rec$checks$checktor)),
    sprintf("| urlchecker | %s |", cell(rec$checks$urlchecker)),
    sprintf("| win-builder | %s |", last_builder("win-builder")),
    sprintf("| mac-builder | %s |", last_builder("mac-builder")),
    sprintf("| CRAN | %s |", queue),
    "",
    "Paste win-builder or mac-builder results links in a comment and they are picked up.",
    sprintf("Updated %s. [Release board](%s)", rec$updated, board_url)),
    collapse = "\n")
}
