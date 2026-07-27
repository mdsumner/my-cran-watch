#!/usr/bin/env Rscript
# retro_tag.R: one-shot debt clearance for missing release tags.
#
# For each package in the repo_map, walk the CRAN version history and tag
# the commit that each version corresponds to.  Deliberately interactive:
# you eyeball each proposed (version, commit) pair before anything is
# pushed.  Not on any schedule; once run, the daily watcher keeps tags
# current and this never needs to exist again.
#
# Version history sources, in order:
#   - current version from tools::CRAN_package_db()
#   - archived versions from the CRAN archive listing (Archive/<pkg>/)
# Cross-reference: the metacran mirror github.com/cran/<pkg> has one
# commit per CRAN release; we print its tree URL for a quick visual diff
# if a proposed commit looks doubtful.
#
# Usage: Rscript scripts/retro_tag.R [pkg ...]     (default: all in repo_map)

for (f in list.files("R", full.names = TRUE)) source(f)
cfg <- read_config()

args <- commandArgs(trailingOnly = TRUE)
pkgs <- if (length(args)) args else names(cfg$repo_map)

cran_versions <- function(pkg) {
  db <- tools::CRAN_package_db()
  cur <- db[db$Package == pkg, c("Version", "Published")]
  arch_url <- sprintf("https://cran.r-project.org/src/contrib/Archive/%s/", pkg)
  arch <- tryCatch({
    h <- readLines(arch_url, warn = FALSE)
    m <- regmatches(h, gregexpr(sprintf("%s_[0-9][^\"]*?\\.tar\\.gz", pkg), h))
    tarballs <- unique(unlist(m))
    sub("\\.tar\\.gz$", "", sub(sprintf("^%s_", pkg), "", tarballs))
  }, error = function(e) character())
  vers <- unique(c(arch, cur$Version))
  data.frame(version = vers,
             published = c(rep(NA_character_, length(vers) - nrow(cur)),
                           cur$Published),
             stringsAsFactors = FALSE)
}

ask <- function(prompt) {
  cat(prompt, "")
  tolower(trimws(readLines("stdin", n = 1)))
}

for (pkg in pkgs) {
  repo <- cfg$repo_map[[pkg]]
  if (is.null(repo)) { msg("%s: not in repo_map, skipping", pkg); next }
  msg("\n== %s (%s) ==", pkg, repo)

  dir <- file.path(tempdir(), paste0("mcw-retro-", pkg))
  unlink(dir, recursive = TRUE)
  clone_shallow(repo, dir)

  existing <- run("git", c("-C", dir, "tag", "-l"), allow_fail = TRUE)$output
  vers <- cran_versions(pkg)
  msg("%d CRAN versions, %d existing tags", nrow(vers), sum(nzchar(existing)))

  for (i in seq_len(nrow(vers))) {
    ver <- vers$version[i]
    tag <- paste0("v", ver)
    if (tag %in% existing || ver %in% existing) {
      msg("  %s: already tagged", ver); next
    }
    loc <- locate_release_commit(dir, ver)
    if (is.null(loc)) {
      msg("  %s: no commit found (pre-git history?), skipping", ver)
      msg("     metacran ref: https://github.com/cran/%s/tree/%s", pkg, ver)
      next
    }
    date <- run("git", c("-C", dir, "show", "-s", "--format=%ad",
                         "--date=short", loc$sha), allow_fail = TRUE)$output[1]
    msg("  %s: propose %s (%s, commit date %s%s)",
        ver, substr(loc$sha, 1, 9), loc$how, date,
        if (!is.na(vers$published[i])) sprintf(", CRAN published %s", vers$published[i]) else "")
    ans <- ask(sprintf("     tag %s here? [y/n/d(iff url)/q]", tag))
    if (ans == "q") quit(status = 0)
    if (ans == "d") {
      msg("     compare: https://github.com/cran/%s/tree/%s vs https://github.com/%s/tree/%s",
          pkg, ver, repo, substr(loc$sha, 1, 9))
      ans <- ask(sprintf("     tag %s here? [y/n]", tag))
    }
    if (ans == "y") {
      run("git", c("-C", dir, "tag", "-a", tag, loc$sha,
                   "-m", shQuote(sprintf("CRAN release %s (retro-tagged)", ver))))
      run("git", c("-C", dir, "push", "--quiet", "origin", tag))
      msg("     tagged and pushed %s", tag)
    }
  }
}
msg("\nretro-tagging pass complete")
