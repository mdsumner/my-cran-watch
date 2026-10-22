#!/usr/bin/env Rscript
# release board: packages on their way to CRAN (see R/release.R).
# Writes docs/release.json and keeps one status comment on each
# "release: <pkg>" issue.
#
# Run from the repo root:  Rscript scripts/release.R [--dry-run] [--force] [--wait]
#   --dry-run  no issues opened, commented on or closed
#   --force    rerun checktor/urlchecker even if the commit has not changed
#   --wait     if a builder result is still pending, look again every 5
#              minutes for up to 30 minutes

for (f in list.files("R", full.names = TRUE)) source(f)
args  <- commandArgs(trailingOnly = TRUE)
dry   <- "--dry-run" %in% args
force <- "--force" %in% args
wait  <- "--wait" %in% args

cfg <- read_config()
self <- Sys.getenv("GITHUB_REPOSITORY", "mdsumner/my-cran-watch")
parts <- strsplit(self, "/")[[1]]
board_url <- sprintf("https://%s.github.io/%s/releases.html", tolower(parts[1]), parts[2])
board_path <- "docs/release.json"
keep_published_days <- 14

db <- tools::CRAN_package_db()
queue <- if (file.exists("docs/queue.json")) jsonlite::read_json("docs/queue.json") else list()
prev <- if (file.exists(board_path)) jsonlite::read_json(board_path)$packages else list()
names(prev) <- vapply(prev, `[[`, character(1), "package")

# issue helpers -----------------------------------------------------------

open_release_issues <- function() {
  res <- gh_json(c("issue", "list", "--state", "open", "--json", "number,title,author",
                   "--limit", "200"))
  if (is.null(res) || !NROW(res)) return(data.frame(number = integer(), package = character(),
                                                    ref = character(), stringsAsFactors = FALSE))
  login <- res$author$login
  rows <- lapply(seq_len(nrow(res)), function(i) {
    t <- parse_release_title(res$title[i])
    # only issues you (or this workflow) opened
    if (is.null(t) || !(identical(login[i], cfg$github_user) || grepl("github-actions", login[i]))) {
      return(NULL)
    }
    data.frame(number = res$number[i], package = t$package, ref = t$ref, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(data.frame(number = integer(), package = character(),
                                      ref = character(), stringsAsFactors = FALSE))
  out[!duplicated(out$package), , drop = FALSE]
}

issue_texts <- function(number) {
  iss <- gh_json(c("api", sprintf("repos/%s/issues/%s", self, number)))
  com <- gh_json(c("api", sprintf("repos/%s/issues/%s/comments?per_page=100", self, number)))
  body <- iss$body %||% ""
  status_id <- NULL
  texts <- body
  if (NROW(com)) {
    mine <- com$user$login == cfg$github_user
    texts <- c(texts, com$body[mine])
    hit <- which(startsWith(com$body, release_marker))
    if (length(hit)) status_id <- com$id[hit[1]]
  }
  list(texts = texts, status_id = status_id)
}

upsert_status <- function(number, status_id, body) {
  if (is.null(status_id)) {
    gh(c("api", "-X", "POST", sprintf("repos/%s/issues/%s/comments", self, number),
         "-f", shQuote(paste0("body=", body))))
  } else {
    gh(c("api", "-X", "PATCH", sprintf("repos/%s/issues/comments/%s", self, status_id),
         "-f", shQuote(paste0("body=", body))))
  }
}

open_issue <- function(pkg, version) {
  body <- paste(c(
    sprintf("Tracking %s %s on its way to CRAN (listed under `releasing:` in config.yml).", pkg, version),
    "",
    "Paste win-builder and mac-builder results links in comments here; the status",
    "comment below and the release board pick them up. Closed automatically when",
    "CRAN publishes this version."), collapse = "\n")
  gh(c("label", "create", "release", "--force", "--color", "ededed"))
  out <- gh(c("issue", "create", "--label", "release",
              "--title", shQuote(sprintf("release: %s", pkg)), "--body", shQuote(body)))
  if (out$status != 0) return(NA_integer_)
  suppressWarnings(as.integer(sub(".*/", "", utils::tail(out$output, 1))))
}

# one pass ------------------------------------------------------------------

targets <- function(issues) {
  conf <- normalize_releasing(cfg$releasing)
  pkgs <- unique(c(conf$package, issues$package))
  lapply(pkgs, function(p) {
    c_row <- conf[conf$package == p, , drop = FALSE]
    i_row <- issues[issues$package == p, , drop = FALSE]
    cran <- db[db$Package == p, , drop = FALSE]
    repo <- if (nrow(c_row) && !is.na(c_row$repo)) c_row$repo
            else cfg$repo_map[[p]] %||%
              (if (nrow(cran)) github_repo(cran$URL[1], cran$BugReports[1]))
    list(package = p, repo = repo,
         ref = if (nrow(i_row) && !is.na(i_row$ref)) i_row$ref
               else if (nrow(c_row) && !is.na(c_row$ref)) c_row$ref else NA_character_,
         issue = if (nrow(i_row)) i_row$number else NULL,
         configured = nrow(c_row) > 0,
         cran_version = if (nrow(cran)) cran$Version[1] else NA_character_)
  })
}

queue_entry <- function(pkg) {
  hit <- Filter(function(e) identical(e$package, pkg), queue$entries %||% list())
  if (!length(hit)) return(NULL)
  list(folder = paste(c(hit[[1]]$folder, if (nzchar(hit[[1]]$subfolder %||% "")) hit[[1]]$subfolder),
                      collapse = "/"),
       version = hit[[1]]$version, first_seen = hit[[1]]$first_seen %||% hit[[1]]$modified)
}

one_package <- function(t, old) {
  # rec$issue links the issue; t$issue (open issues only) is what gets
  # comments, so a closed issue is never touched again
  rec <- list(package = t$package, repo = t$repo, ref = t$ref, issue = t$issue %||% old$issue,
              cran_version = t$cran_version, updated = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"))
  if (is.null(t$repo)) {
    msg("::warning::release: %s has no GitHub repo (add it to repo_map or releasing:)", t$package)
    rec$error <- "no GitHub repo known"
    return(rec)
  }
  if (is.na(rec$ref)) rec$ref <- default_branch(t$repo)
  info <- ref_info(t$repo, rec$ref)
  if (is.null(info)) {
    msg("::warning::release: %s could not read %s@%s", t$package, t$repo, rec$ref)
    rec$error <- sprintf("could not read %s@%s", t$repo, rec$ref)
    return(rec)
  }
  rec$sha <- info$sha
  rec$version <- info$version
  rec$published <- !is.na(t$cran_version) && !version_newer(info$version, t$cran_version)
  rec$published_on <- if (rec$published) old$published_on %||% today()
  rec$queue <- if (!rec$published) queue_entry(t$package)

  # a configured package that is already on CRAN at this version, with
  # nothing tracked before: a leftover config entry, not a release
  if (rec$published && is.null(old) && is.null(t$issue)) {
    msg("::notice::release: %s %s at %s@%s is already on CRAN; nothing to track",
        t$package, info$version, t$repo, rec$ref)
    return(NULL)
  }
  if (rec$published && Sys.Date() - as.Date(rec$published_on) > keep_published_days) return(NULL)

  live <- t$issue
  # opened once per release: an issue you closed by hand stays closed
  if (is.null(live) && is.null(old$issue) && !rec$published && t$configured && !dry) {
    n <- open_issue(t$package, info$version)
    if (!is.na(n)) live <- rec$issue <- n
  }

  rec$ci <- ci_status(t$repo, info$sha)

  failed <- any(startsWith(vapply(old$checks %||% list(), function(x) x$summary %||% "", character(1)),
                           "failed"))
  if (!force && identical(old$sha, info$sha) && length(old$checks) && !failed) {
    rec$checks <- old$checks
  } else {
    src <- clone_commit(t$repo, info$sha, file.path(tempdir(), "release", t$package))
    rec$checks <- list()
    for (s in c("checktor", "urlchecker")) {
      rec$checks[[s]] <- if (is.null(src)) list(summary = "failed: could not clone", count = NA)
      else tryCatch({
        res <- detail_sections[[s]](src)
        list(summary = res$summary, count = res$count %||% NROW(res$problems))
      }, error = function(e) {
        msg("::warning::release: %s %s failed: %s", t$package, s, conditionMessage(e))
        list(summary = sprintf("failed: %s", conditionMessage(e)), count = NA)
      })
    }
    if (!is.null(src)) unlink(src, recursive = TRUE)
  }

  status_id <- NULL
  rec$builders <- list()
  if (!is.null(live)) {
    it <- issue_texts(live)
    status_id <- it$status_id
    urls <- builder_urls(it$texts)
    done <- Filter(function(b) !is.na(b$status %||% NA), old$builders %||% list())
    names(done) <- vapply(done, `[[`, character(1), "url")
    for (i in seq_len(nrow(urls))) {
      u <- urls$url[i]
      rec$builders[[i]] <- done[[u]] %||% c(list(builder = urls$builder[i], url = u),
                                             fetch_builder(u))
    }
  }
  rec$stage <- release_stage(rec)

  if (!is.null(live) && !dry) {
    body <- release_comment(rec, board_url)
    # skip the edit when only the timestamp would change
    strip <- function(x) sub("\nUpdated [^\n]*$", "", x %||% "")
    if (!identical(strip(body), strip(old$comment))) upsert_status(live, status_id, body)
    rec$comment <- body
    if (rec$published) {
      gh(c("issue", "close", live, "--comment",
           shQuote(sprintf("%s %s is on CRAN. Closing.", t$package, rec$cran_version))))
    }
  } else {
    rec$comment <- old$comment
  }
  rec
}

run_board <- function(prev) {
  issues <- open_release_issues()
  out <- list()
  for (t in targets(issues)) {
    msg("release: %s", t$package)
    rec <- one_package(t, prev[[t$package]])
    if (!is.null(rec)) out[[t$package]] <- rec
  }
  # recently published packages stay on the board for a while
  for (p in setdiff(names(prev), names(out))) {
    old <- prev[[p]]
    if (isTRUE(old$published) &&
        Sys.Date() - as.Date(old$published_on %||% today()) <= keep_published_days) {
      out[[p]] <- old
    }
  }
  out
}

board <- run_board(prev)
pending <- function(b) any(vapply(b, function(r)
  any(vapply(r$builders %||% list(), function(x) is.na(x$status %||% NA), logical(1))), logical(1)))
tries <- 0
while (wait && pending(board) && tries < 6) {
  tries <- tries + 1
  msg("release: builder results pending, looking again in 5 minutes (%d/6)", tries)
  Sys.sleep(300)
  board <- run_board(board)
}

jsonlite::write_json(list(generated = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
                          repo = self, packages = unname(board)),
                     board_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
msg("::notice::release: %d package(s) on the board: %s", length(board),
    paste(vapply(board, function(r) sprintf("%s %s (%s; CI %s; checktor %s; urlchecker %s; %d builder link(s))",
                                            r$package, r$version %||% "?", r$stage %||% r$error %||% "?",
                                            r$ci$status %||% "?", r$checks$checktor$summary %||% "?",
                                            r$checks$urlchecker$summary %||% "?", length(r$builders)),
                 character(1)),
          collapse = "; "))
if (.gh_failures$n > 0) {
  msg("::error::release: %d gh call(s) failed; see the log", .gh_failures$n)
  quit(status = 1)
}
