#!/usr/bin/env Rscript
# init_repo_map.R: generate a DRAFT repo_map from the URL and BugReports
# fields of your packages in the CRAN db.  Prints yaml to paste into
# config.yml.  Review by hand: these fields are close but not
# authoritative enough to push tags against unreviewed.

for (f in list.files("R", full.names = TRUE)) source(f)
cfg <- read_config()
mine <- fetch_my_packages(cfg)

extract_repo <- function(...) {
  txt <- paste(c(...), collapse = " ")
  m <- regmatches(txt, regexpr("github\\.com/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)", txt))
  if (!length(m)) return(NA_character_)
  sub("/issues.*$", "", sub("github\\.com/", "", m))
}

cat("repo_map:\n")
for (i in order(mine$package)) {
  repo <- extract_repo(mine$url[i], mine$bugreports[i])
  if (is.na(repo)) {
    cat(sprintf("  # %s: NO GITHUB URL FOUND - fill in by hand or omit\n",
                mine$package[i]))
  } else {
    cat(sprintf("  %s: %s\n", mine$package[i], repo))
  }
}
