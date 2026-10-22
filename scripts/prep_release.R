# Local helper for the release board: run on your own machine, from the
# package directory, when you think a package is ready.
#
#   source("path/to/my-cran-watch/scripts/prep_release.R")
#   prep_release()            # win-builder R-devel + mac-builder R-devel
#
# It uploads the package to win-builder (results come by email to the
# Maintainer) and mac-builder (results link returned straight away), then
# posts the mac-builder link on the package's "release: <pkg>" issue in
# my-cran-watch, opening the issue if there is none.  The release
# workflow fetches the check log from that link.  When the win-builder
# email arrives, paste its link into the same issue.
#
# Needs devtools and the gh CLI (logged in: gh auth login).

prep_release <- function(path = ".", watch_repo = "mdsumner/my-cran-watch",
                         win = TRUE, mac = TRUE) {
  pkg <- unname(read.dcf(file.path(path, "DESCRIPTION"), fields = "Package")[1, 1])
  issue <- release_issue(pkg, watch_repo)
  if (win) devtools::check_win_devel(path, quiet = TRUE)
  if (mac) {
    fun <- if (exists("check_mac_devel", envir = asNamespace("devtools"))) devtools::check_mac_devel
           else devtools::check_mac_release
    url <- fun(path, quiet = TRUE)
    if (is.character(url) && length(url) == 1 && grepl("^https?://", url)) {
      gh_cli(c("issue", "comment", issue, "--repo", watch_repo,
               "--body", shQuote(sprintf("mac-builder: %s", url))))
      message("Posted the mac-builder link on ", watch_repo, "#", issue)
    } else {
      message("Paste the mac-builder results link into ", watch_repo, "#", issue)
    }
  }
  if (win) message("When the win-builder email arrives, paste its link into ", watch_repo, "#", issue)
  invisible(issue)
}

# number of the open "release: <pkg>" issue, opened if missing
release_issue <- function(pkg, watch_repo) {
  out <- gh_cli(c("issue", "list", "--repo", watch_repo, "--state", "open",
                  "--search", shQuote(sprintf("\"release: %s\" in:title", pkg)),
                  "--json", "number,title"))
  hits <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  if (NROW(hits)) {
    keep <- grepl(sprintf("^release:\\s*%s(@\\S+)?\\s*$", pkg), hits$title)
    if (any(keep)) return(hits$number[keep][1])
  }
  out <- gh_cli(c("issue", "create", "--repo", watch_repo,
                  "--title", shQuote(sprintf("release: %s", pkg)),
                  "--body", shQuote("Opened by prep_release().")))
  as.integer(sub(".*/", "", utils::tail(out, 1)))
}

gh_cli <- function(args) {
  out <- suppressWarnings(system2("gh", args, stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(out, "status"))) stop("gh ", args[1], " ", args[2], " failed:\n",
                                          paste(out, collapse = "\n"))
  out
}
