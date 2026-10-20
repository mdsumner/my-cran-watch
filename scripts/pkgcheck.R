#!/usr/bin/env Rscript
# rOpenSci pkgcheck for the packages in config `pkgcheck:` (opt-in; see
# R/pkgcheck.R).  Clones each package's GitHub repo, installs its
# dependencies, runs pkgcheck::pkgcheck() and writes sections$pkgcheck into
# docs/pkg/<pkg>.json plus the full report as docs/pkg/<pkg>.pkgcheck.md.
# Meant for the pkgcheck workflow, whose container
# (ghcr.io/ropensci-review-tools/pkgcheck) has pkgcheck, ctags and the
# system libraries; runs anywhere those are installed.
#
# Run from the repo root:  Rscript scripts/pkgcheck.R [--goodpractice] [pkg ...]

for (f in list.files("R", full.names = TRUE)) source(f)
cfg <- read_config()
args <- commandArgs(trailingOnly = TRUE)
force_gp <- "--goodpractice" %in% args
only <- setdiff(args, "--goodpractice")

targets <- pkgcheck_targets(cfg)
if (length(only)) {
  unknown <- setdiff(only, targets$package)
  if (length(unknown)) {
    stop(sprintf("not in config pkgcheck.packages: %s", paste(unknown, collapse = ", ")))
  }
  targets <- targets[targets$package %in% only, , drop = FALSE]
}
if (!nrow(targets)) {
  msg("pkgcheck: no packages opted in (config pkgcheck.packages is empty)")
  quit(status = 0)
}
ignore <- unlist(cfg$pkgcheck$ignore %||% list())

dir.create("docs/pkg", showWarnings = FALSE, recursive = TRUE)
work <- tempfile("pkgcheck"); dir.create(work)
failed <- character()
for (i in seq_len(nrow(targets))) {
  pkg <- targets$package[i]
  gp <- force_gp || targets$goodpractice[i]
  msg("::group::pkgcheck: %s (%s)%s", pkg, targets$repo[i], if (gp) " with goodpractice" else "")
  src <- file.path(work, pkg)
  version <- NULL
  sec <- tryCatch({
    run("git", c("clone", "--quiet", sprintf("https://github.com/%s.git", targets$repo[i]), src))
    sha <- run("git", c("-C", src, "rev-parse", "HEAD"))$output[1]
    version <- unname(read.dcf(file.path(src, "DESCRIPTION"), "Version")[1, 1])
    remotes::install_deps(src, dependencies = TRUE, upgrade = "never")
    chk <- pkgcheck::pkgcheck(src, goodpractice = gp, use_cache = FALSE)
    print(chk)
    report <- paste0(pkg, ".pkgcheck.md")
    writeLines(pkgcheck::checks_to_markdown(chk, render = FALSE), file.path("docs/pkg", report))
    pkgcheck_section(pkgcheck_rows(chk), ignore, sha = sha, report = report, goodpractice = gp)
  }, error = function(e) {
    msg("  pkgcheck failed: %s", conditionMessage(e))
    failed <<- c(failed, pkg)
    list(summary = sprintf("failed to run: %s", conditionMessage(e)),
         problems = data.frame(), count = 0L)
  })
  write_details_section(pkg, "pkgcheck", sec, version)
  unlink(src, recursive = TRUE)
  msg("::endgroup::")
  msg("pkgcheck: %s: %s", pkg, sec$summary)
}
if (length(failed)) stop(sprintf("pkgcheck could not run for: %s", paste(failed, collapse = ", ")))
