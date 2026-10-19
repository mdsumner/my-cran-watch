#!/usr/bin/env Rscript
# per-package details for the dashboard: docs/pkg/<pkg>.json holds one
# entry per section (urlchecker for now), docs/pkg/index.json a count per
# package and section for the main table.  Runs from its own workflow on
# a slower cadence than the watcher, since it downloads every source
# tarball.  Another checker adds a section by adding a function to
# `sections` returning list(summary = <one line>, problems = <data.frame>).
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
    res <- urlchecker::url_check(dir, progress = FALSE)
    df <- data.frame(
      url     = as.character(res$URL),
      status  = as.character(res$Status),
      message = as.character(res$Message),
      new     = as.character(res$New),
      from    = vapply(res$From, paste, character(1), collapse = ", "),
      stringsAsFactors = FALSE)
    list(summary = if (nrow(df)) sprintf("%d URL problem(s)", nrow(df)) else "all URLs ok",
         problems = df)
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
  }, error = function(e) { msg("  download failed: %s", conditionMessage(e)); NULL })
  if (is.null(src)) next
  rec <- list(package = pkg,
              version = mine$version[match(pkg, mine$package)],
              updated = today(), sections = list())
  for (s in names(sections)) {
    rec$sections[[s]] <- tryCatch(sections[[s]](src), error = function(e)
      list(summary = sprintf("failed: %s", conditionMessage(e)), problems = data.frame()))
    index[[pkg]][[s]] <- nrow(rec$sections[[s]]$problems)
  }
  jsonlite::write_json(rec, file.path(out_dir, paste0(pkg, ".json")),
                       auto_unbox = TRUE, pretty = TRUE, dataframe = "rows")
  unlink(src, recursive = TRUE)
}
jsonlite::write_json(index, idx_path, auto_unbox = TRUE, pretty = TRUE)
msg("details written for %d package(s)", length(pkgs))
