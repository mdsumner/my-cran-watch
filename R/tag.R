# tag on acceptance ------------------------------------------------------
#
# The one place the watcher ACTS rather than reports.  Requires a
# fine-grained PAT (secret WATCH_PAT) with contents:write on the target
# repos, because GITHUB_TOKEN only reaches this repo.
#
# Strategy for locating the commit to tag, in order of confidence:
#   1. CRAN-SUBMISSION file at HEAD of the default branch (usethis writes
#      version + SHA at submission time; exact, no inference)
#   2. newest commit whose diff introduced "Version: <ver>" in DESCRIPTION
#      (git log -S), i.e. the bump TO this version
# Refuses to act if neither yields a commit.

clone_shallow <- function(repo, dest) {
  pat <- Sys.getenv("WATCH_PAT")
  url <- if (nzchar(pat)) {
    sprintf("https://x-access-token:%s@github.com/%s.git", pat, repo)
  } else sprintf("https://github.com/%s.git", repo)
  run("git", c("clone", "--filter=blob:none", "--quiet", url, dest))
  dest
}

locate_release_commit <- function(dir, version) {
  sub_file <- file.path(dir, "CRAN-SUBMISSION")
  if (file.exists(sub_file)) {
    lines <- readLines(sub_file, warn = FALSE)
    v <- sub("^Version:\\s*", "", grep("^Version:", lines, value = TRUE))
    sha <- sub("^SHA:\\s*", "", grep("^SHA:", lines, value = TRUE))
    if (length(v) && length(sha) && identical(v, version)) {
      return(list(sha = sha, how = "CRAN-SUBMISSION"))
    }
  }
  out <- run("git", c("-C", dir, "log", "--format=%H",
                      "-S", shQuote(sprintf("Version: %s", version)),
                      "--", "DESCRIPTION"), allow_fail = TRUE)
  shas <- out$output[nzchar(out$output)]
  if (length(shas)) {
    # -S lists commits that add OR remove the string; the newest one that
    # currently *contains* it is the bump to this version
    for (sha in shas) {
      show <- run("git", c("-C", dir, "show", sprintf("%s:DESCRIPTION", sha)),
                  allow_fail = TRUE)
      if (any(grepl(sprintf("^Version: %s$", version), show$output))) {
        return(list(sha = sha, how = "git log -S"))
      }
    }
  }
  NULL
}

tag_release <- function(pkg, version, repo, dry_run = FALSE) {
  dir <- file.path(tempdir(), paste0("mcw-", pkg))
  unlink(dir, recursive = TRUE)
  clone_shallow(repo, dir)
  tag <- paste0("v", version)

  existing <- run("git", c("-C", dir, "tag", "-l", tag), allow_fail = TRUE)
  if (any(nzchar(existing$output))) {
    msg("%s: tag %s already exists, nothing to do", pkg, tag)
    return(invisible(NULL))
  }
  loc <- locate_release_commit(dir, version)
  if (is.null(loc)) {
    msg("%s: could not locate commit for %s; refusing to guess", pkg, version)
    return(invisible(NULL))
  }
  msg("%s: tagging %s at %s (via %s)", pkg, tag, substr(loc$sha, 1, 9), loc$how)
  if (dry_run) return(invisible(loc))

  run("git", c("-C", dir, "tag", "-a", tag, loc$sha,
               "-m", shQuote(sprintf("CRAN release %s", version))))
  run("git", c("-C", dir, "push", "--quiet", "origin", tag))
  run("gh", c("release", "create", tag, "--repo", repo,
              "--title", shQuote(sprintf("%s %s", pkg, version)),
              "--notes", shQuote(sprintf(
                "CRAN release %s, tagged automatically by my-cran-watch (%s).",
                version, loc$how))),
      allow_fail = TRUE)
  invisible(loc)
}
