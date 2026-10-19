# further watchers --------------------------------------------------------
#
# Each watcher turns one source into observations keyed "<pkg>|<kind>"
# and hands them to diff_kind(), which runs the usual condition lifecycle
# (observed -> active -> snoozed) and clears what is no longer observed.
# kinds: checks, deadline, archived, revdeps, canary.

# condition namespaces other than per-flavor checks; diff_checks must not
# clear these, and reconcile/notify map them back to an event type
condition_kinds <- c(checks = "checks", universe = "universe_gap", metadata = "metadata",
                     deadline = "deadline", archived = "archived",
                     revdeps = "revdeps", canary = "canary")

key_kind <- function(key) sub("^.*\\|", "", key)

# obs: data.frame(package, status, detail); required: streak to activate
diff_kind <- function(state, obs, kind, required) {
  events <- list()
  type <- condition_kinds[[kind]]
  keys <- condition_key(obs$package, kind)
  for (i in seq_len(nrow(obs))) {
    stepped <- step_condition(state$conditions[[keys[i]]], obs$status[i],
                              obs$detail[i], required)
    state$conditions[[keys[i]]] <- stepped$cond
    if (!is.null(stepped$event)) {
      events[[length(events) + 1L]] <- list(
        type = type, event = stepped$event, key = keys[i],
        package = obs$package[i], status = obs$status[i],
        detail = obs$detail[i], issue_number = stepped$cond$issue_number)
    }
  }
  old <- names(state$conditions) %||% character()
  old <- old[key_kind(old) == kind]
  for (key in setdiff(old, keys)) {
    cleared <- clear_condition(state$conditions[[key]])
    if (!is.null(cleared$event)) {
      events[[length(events) + 1L]] <- list(
        type = type, event = "cleared", key = key,
        package = sub("\\|[^|]*$", "", key),
        issue_number = state$conditions[[key]]$issue_number)
    }
    state$conditions[[key]] <- NULL
  }
  list(state = state, events = events)
}

empty_obs <- function() {
  data.frame(package = character(), status = character(), detail = character(),
             stringsAsFactors = FALSE)
}

# CRAN "fix before" deadlines: the Deadline field CRAN sets in the package
# db when a package will be archived unless fixed.  Acts on first sight:
# a deadline is never noise.
watch_deadlines <- function(mine, db) {
  if (!"Deadline" %in% names(db)) return(empty_obs())
  row <- db[db$Package %in% mine$package & !is.na(db$Deadline) &
              nzchar(db$Deadline), , drop = FALSE]
  if (!nrow(row)) return(empty_obs())
  comment <- if ("X-CRAN-Comment" %in% names(row)) row[["X-CRAN-Comment"]] else NA
  data.frame(
    package = row$Package, status = "DEADLINE",
    detail = sprintf("CRAN deadline: fix before **%s**.%s", row$Deadline,
                     ifelse(is.na(comment), "", paste0("\n\n", comment))),
    stringsAsFactors = FALSE)
}

# archived: a package seen before (state$packages) that is gone from the
# db.  Confirmed against its CRAN page, so a package merely dropped from
# config is not reported as archived.
watch_archived <- function(state, db) {
  gone <- setdiff(names(state$packages), db$Package)
  out <- empty_obs()
  for (pkg in gone) {
    url <- sprintf("https://cran.r-project.org/web/packages/%s/index.html", pkg)
    page <- tryCatch(paste(readLines(url, warn = FALSE), collapse = "\n"),
                     error = function(e) "")
    if (!grepl("removed from the CRAN repository", page, fixed = TRUE)) next
    why <- regmatches(page, regexpr("Archived on [^<]*", page))
    out <- rbind(out, data.frame(
      package = pkg, status = "ARCHIVED",
      detail = sprintf("%s is no longer on CRAN. %s\n\n%s", pkg,
                       if (length(why)) why else "", url),
      stringsAsFactors = FALSE))
  }
  out
}

# reverse dependencies currently failing on CRAN, per package of mine:
# a revdep going ERROR is often my release breaking someone else
watch_revdeps <- function(mine, db, res = tools::CRAN_check_results()) {
  pkgs <- mine$package[mine$role == "maintainer"]
  if (!length(pkgs)) return(empty_obs())
  rd <- tools::package_dependencies(pkgs, db = as.matrix(db), reverse = TRUE,
                                    which = c("Depends", "Imports", "LinkingTo"))
  all_rd <- unique(unlist(rd))
  if (!length(all_rd)) return(empty_obs())
  res <- res[res$Package %in% all_rd &
               as.character(res$Status) %in% c("ERROR", "FAIL"), , drop = FALSE]
  out <- empty_obs()
  for (pkg in names(rd)) {
    bad <- res[res$Package %in% rd[[pkg]], , drop = FALSE]
    if (!nrow(bad)) next
    by_pkg <- split(as.character(bad$Flavor), as.character(bad$Package))
    lines <- sprintf("- %s: %s", names(by_pkg),
                     vapply(by_pkg, paste, character(1), collapse = ", "))
    out <- rbind(out, data.frame(
      package = pkg, status = "REVDEP",
      detail = paste(c(sprintf("%d of %d reverse dependencies ERROR on CRAN:",
                               length(by_pkg), length(rd[[pkg]])),
                       sort(lines)), collapse = "\n"),
      stringsAsFactors = FALSE))
  }
  out
}

# hypertidy/gdal-r-ci canary: open issues labelled "canary" are its
# report of GDAL/PROJ/GEOS breakage.  Read them rather than re-run them.
watch_canary <- function(cfg) {
  repo <- cfg$gdal_r_ci %||% "hypertidy/gdal-r-ci"
  res <- gh_json(c("api", sprintf("repos/%s/issues?labels=canary&state=open&per_page=100",
                                  repo)))
  if (is.null(res) || !NROW(res)) return(empty_obs())
  data.frame(
    package = sprintf("gdal-r-ci#%s", res$number), status = "CANARY",
    detail = sprintf("[%s](%s)", res$title, res$html_url),
    stringsAsFactors = FALSE)
}
