#!/usr/bin/env Rscript
# per-package details for the dashboard: docs/pkg/<pkg>.json holds one
# entry per section (urlchecker, checktor), docs/pkg/index.json a count per
# package and section for the main table.  Runs from its own workflow on
# a slower cadence than the watcher, since it downloads every source
# tarball.  The sections themselves are in R/sections.R.
#
# Run from the repo root:  Rscript scripts/details.R [pkg ...]

for (f in list.files("R", full.names = TRUE)) source(f)
cfg <- read_config()
db  <- tools::CRAN_package_db()
mine <- fetch_my_packages(cfg, db)
only <- commandArgs(trailingOnly = TRUE)
pkgs <- if (length(only)) only else mine$package[mine$role == "maintainer"]

sections <- detail_sections

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
