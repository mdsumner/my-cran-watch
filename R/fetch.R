# fetchers ---------------------------------------------------------------
#
# Each fetcher returns a plain data.frame, normalized so diff.R never has
# to know where data came from.  Adding a capability to the watcher means
# adding a fetcher here and a diff rule in diff.R; nothing else changes.

# packages I maintain, from the canonical source; pass db in so a run
# fetches the package db exactly once (watch.R also needs it for
# upstream attribution)
fetch_my_packages <- function(cfg, db = tools::CRAN_package_db()) {
  pat <- paste(sprintf("<%s>", tolower(cfg$maintainer_emails)), collapse = "|")
  # Maintainer field is "Name <email>"; match on the address, not the name,
  # so name variants (Michael Sumner / Michael D. Sumner) are irrelevant
  mine <- db[grepl(pat, tolower(db$Maintainer), fixed = FALSE), , drop = FALSE]
  # explicit additions: packages whose maintainership has changed in
  # reality but not yet on CRAN (field updates at next accepted release)
  extra <- setdiff(cfg$extra_packages %||% character(), mine$Package)
  if (length(extra)) {
    add <- db[db$Package %in% extra, , drop = FALSE]
    missing <- setdiff(extra, add$Package)
    if (length(missing)) {
      msg("extra_packages not found on CRAN: %s", paste(missing, collapse = ", "))
    }
    mine <- rbind(mine, add)
  }
  data.frame(
    package   = mine$Package,
    version   = mine$Version,
    published = mine$Published,
    url       = mine$URL %||% NA_character_,
    bugreports = mine$BugReports %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

# per-flavor check results for a set of packages
fetch_check_results <- function(pkgs, cfg) {
  res <- tools::CRAN_check_results()
  res <- res[res$Package %in% pkgs, , drop = FALSE]
  if (length(cfg$ignore_flavors)) {
    drop <- Reduce(`|`, lapply(cfg$ignore_flavors, grepl, x = res$Flavor))
    res <- res[!drop, , drop = FALSE]
  }
  out <- data.frame(
    package = res$Package,
    flavor  = res$Flavor,
    status  = res$Status,
    stringsAsFactors = FALSE
  )
  # attach detail text where available, for the text-hash "changed
  # character" test and for issue bodies
  det <- tryCatch(tools::CRAN_check_details(),
                  error = function(e) NULL)
  if (!is.null(det)) {
    det <- det[det$Package %in% pkgs, , drop = FALSE]
    dtext <- vapply(split(det, paste(det$Package, det$Flavor, sep = "|")),
                    function(d) paste(unique(paste0(d$Check, ": ", d$Output)),
                                      collapse = "\n---\n"),
                    character(1))
    out$detail <- unname(dtext[paste(out$package, out$flavor, sep = "|")])
    out$detail[is.na(out$detail)] <- ""
  } else {
    out$detail <- ""
  }
  out
}

# r-universe presence + build status for configured universes
fetch_universe <- function(cfg) {
  do.call(rbind, lapply(cfg$universes, function(u) {
    url <- sprintf("https://%s.r-universe.dev/api/packages", u)
    dat <- tryCatch(jsonlite::fromJSON(url, simplifyDataFrame = TRUE),
                    error = function(e) NULL)
    if (is.null(dat) || NROW(dat) == 0) {
      return(data.frame(universe = character(), package = character(),
                        version = character(), ok = logical(),
                        stringsAsFactors = FALSE))
    }
    ok <- if ("_status" %in% names(dat)) !grepl("fail", tolower(dat$`_status`)) else TRUE
    data.frame(universe = u, package = dat$Package,
               version = dat$Version, ok = ok,
               stringsAsFactors = FALSE)
  }))
}

# CRAN incoming queue via foghorn, if installed (optional dependency).
# Watching your package move through newbies/pretest/waiting is useful
# during the days-to-weeks limbo, and "left the queue + version appeared
# in the db" is the acceptance moment.
fetch_incoming <- function(cfg) {
  if (!requireNamespace("foghorn", quietly = TRUE)) return(NULL)
  tryCatch(foghorn::cran_incoming(),
           error = function(e) NULL)
}
