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
  # nominated packages: watched and reported on, never tagged/released
  role <- rep("maintainer", nrow(mine))
  nom <- cfg$watch[!cfg$watch$package %in% mine$Package, , drop = FALSE]
  if (nrow(nom)) {
    add <- db[db$Package %in% nom$package, , drop = FALSE]
    missing <- setdiff(nom$package, add$Package)
    if (length(missing)) {
      msg("watch packages not found on CRAN: %s", paste(missing, collapse = ", "))
    }
    mine <- rbind(mine, add)
    role <- c(role, nom$role[match(add$Package, nom$package)])
  }
  # packages that list me as an author (not maintainer): "contribute".
  # Matched on my emails in Authors@R and on author_names in Author, the
  # plain-text field CRAN derives from Authors@R.
  if (length(cfg$author_names) || length(cfg$maintainer_emails)) {
    hit <- authored_packages(db, cfg)
    hit <- setdiff(hit, c(mine$Package, cfg$author_exclude %||% character()))
    if (length(hit)) {
      msg("authored packages (contribute): %s", paste(sort(hit), collapse = ", "))
      mine <- rbind(mine, db[db$Package %in% hit, , drop = FALSE])
      role <- c(role, rep("contribute", sum(db$Package %in% hit)))
    }
  }
  data.frame(
    package   = mine$Package,
    role      = role,
    version   = mine$Version,
    published = mine$Published,
    url       = mine$URL %||% NA_character_,
    bugreports = mine$BugReports %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

# per-flavor check results for a set of packages
fetch_check_results <- function(pkgs, cfg, res = tools::CRAN_check_results()) {
  res <- res[res$Package %in% pkgs, , drop = FALSE]
  if (length(cfg$ignore_flavors)) {
    drop <- Reduce(`|`, lapply(cfg$ignore_flavors, grepl, x = res$Flavor))
    res <- res[!drop, , drop = FALSE]
  }
  # as.character: a factor status never compares identical() to the
  # string stored in state, which resets every streak to 1 each run
  out <- data.frame(
    package = as.character(res$Package),
    flavor  = as.character(res$Flavor),
    status  = as.character(res$Status),
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

# Package names whose Authors@R carries one of maintainer_emails or whose
# Author field names one of author_names (case-insensitive, whole words).
authored_packages <- function(db, cfg) {
  hit <- rep(FALSE, nrow(db))
  ar <- tolower(db[["Authors@R"]] %||% rep(NA_character_, nrow(db)))
  for (e in tolower(cfg$maintainer_emails)) hit <- hit | grepl(e, ar, fixed = TRUE)
  au <- tolower(gsub("\\s+", " ", db$Author %||% rep(NA_character_, nrow(db))))
  for (n in tolower(cfg$author_names %||% character())) {
    pat <- paste0("(^|[^a-z])", gsub(".", "\\.", n, fixed = TRUE), "($|[^a-z])")
    hit <- hit | grepl(pat, au)
  }
  unique(db$Package[hit %in% TRUE])
}
