# diff engine ------------------------------------------------------------
#
# Compare fetched observations against state; emit a list of events.
# Event types:
#   new_version  : CRAN shows a version newer than last seen -> tag+release
#   check        : condition opened/changed/cleared -> issue open/comment/close
#   universe_gap : package on CRAN but missing/failing in its universe

diff_versions <- function(state, mine) {
  events <- list()
  for (i in seq_len(nrow(mine))) {
    pkg <- mine$package[i]; ver <- mine$version[i]
    prev <- state$packages[[pkg]]
    if (is.null(prev)) {
      # first sighting: record silently, do not retro-fire on bootstrap run
      state$packages[[pkg]] <- list(version = ver, first_seen = today())
    } else if (!identical(prev$version, ver)) {
      events[[length(events) + 1L]] <- list(
        type = "new_version", package = pkg,
        from = prev$version, to = ver, published = mine$published[i])
      state$packages[[pkg]]$version <- ver
    }
  }
  list(state = state, events = events)
}

diff_checks <- function(state, checks, cfg) {
  events <- list()
  seen_keys <- character()

  noteworthy <- checks[checks$status %in% cfg$notify_statuses, , drop = FALSE]
  for (i in seq_len(nrow(noteworthy))) {
    key <- condition_key(noteworthy$package[i], noteworthy$flavor[i])
    seen_keys <- c(seen_keys, key)
    stepped <- step_condition(state$conditions[[key]],
                              noteworthy$status[i],
                              noteworthy$detail[i],
                              cfg$consecutive_required)
    state$conditions[[key]] <- stepped$cond
    if (!is.null(stepped$event)) {
      events[[length(events) + 1L]] <- list(
        type = "check", event = stepped$event, key = key,
        package = noteworthy$package[i], flavor = noteworthy$flavor[i],
        status = noteworthy$status[i], detail = noteworthy$detail[i],
        issue_number = stepped$cond$issue_number)
    }
  }

  # conditions in state that are check-type and no longer observed -> clear
  # (other condition namespaces manage their own clearing)
  check_keys <- names(state$conditions)
  check_keys <- check_keys[!endsWith(check_keys, "|universe") &
                             !endsWith(check_keys, "|metadata")]
  for (key in setdiff(check_keys, seen_keys)) {
    cleared <- clear_condition(state$conditions[[key]])
    if (!is.null(cleared$event)) {
      parts <- strsplit(key, "|", fixed = TRUE)[[1]]
      events[[length(events) + 1L]] <- list(
        type = "check", event = "cleared", key = key,
        package = parts[1], flavor = parts[2],
        issue_number = state$conditions[[key]]$issue_number)
    }
    state$conditions[[key]] <- NULL
  }
  list(state = state, events = events)
}

diff_universe <- function(state, mine, uni, cfg) {
  events <- list()
  seen_keys <- character()
  if (is.null(uni) || nrow(uni) == 0) return(list(state = state, events = events))

  for (pkg in mine$package) {
    row <- uni[uni$package == pkg, , drop = FALSE]
    gap <- if (nrow(row) == 0) {
      "absent from all configured universes"
    } else if (!any(row$ok)) {
      sprintf("failing in %s", paste(row$universe[!row$ok], collapse = ", "))
    } else NA_character_
    key <- condition_key(pkg, "universe")
    if (!is.na(gap)) {
      seen_keys <- c(seen_keys, key)
      stepped <- step_condition(state$conditions[[key]], "GAP", gap,
                                cfg$consecutive_required)
      state$conditions[[key]] <- stepped$cond
      if (!is.null(stepped$event)) {
        events[[length(events) + 1L]] <- list(
          type = "universe_gap", event = stepped$event, key = key,
          package = pkg, detail = gap,
          issue_number = stepped$cond$issue_number)
      }
    }
  }
  ukeys <- names(state$conditions)
  ukeys <- ukeys[endsWith(ukeys, "|universe")]
  for (key in setdiff(ukeys, seen_keys)) {
    cleared <- clear_condition(state$conditions[[key]])
    if (!is.null(cleared$event)) {
      events[[length(events) + 1L]] <- list(
        type = "universe_gap", event = "cleared", key = key,
        package = sub("\\|universe$", "", key),
        issue_number = state$conditions[[key]]$issue_number)
    }
    state$conditions[[key]] <- NULL
  }
  list(state = state, events = events)
}

# metadata drift ----------------------------------------------------------
#
# The watcher holds two beliefs about where a package lives: the curated
# repo_map (the truth) and CRAN's published URL/BugReports fields (what
# the world sees).  When they disagree, DESCRIPTION needs updating at the
# next release; a low-urgency reminder condition, never an action.
diff_metadata <- function(state, mine, cfg) {
  events <- list()
  seen_keys <- character()
  for (i in seq_len(nrow(mine))) {
    pkg <- mine$package[i]
    repo <- cfg$repo_map[[pkg]]
    if (is.null(repo)) next
    fields <- tolower(paste(mine$url[i], mine$bugreports[i]))
    if (grepl(tolower(repo), fields, fixed = TRUE)) next
    gap <- sprintf(
      "repo_map says %s but CRAN URL/BugReports fields do not mention it (URL: %s). Update DESCRIPTION at the next release.",
      repo, mine$url[i] %||% "none")
    key <- condition_key(pkg, "metadata")
    seen_keys <- c(seen_keys, key)
    stepped <- step_condition(state$conditions[[key]], "DRIFT", gap,
                              cfg$consecutive_required)
    state$conditions[[key]] <- stepped$cond
    if (!is.null(stepped$event)) {
      events[[length(events) + 1L]] <- list(
        type = "metadata", event = stepped$event, key = key,
        package = pkg, detail = gap,
        issue_number = stepped$cond$issue_number)
    }
  }
  mkeys <- names(state$conditions)
  mkeys <- mkeys[endsWith(mkeys, "|metadata")]
  for (key in setdiff(mkeys, seen_keys)) {
    cleared <- clear_condition(state$conditions[[key]])
    if (!is.null(cleared$event)) {
      events[[length(events) + 1L]] <- list(
        type = "metadata", event = "cleared", key = key,
        package = sub("\\|metadata$", "", key),
        issue_number = state$conditions[[key]]$issue_number)
    }
    state$conditions[[key]] <- NULL
  }
  list(state = state, events = events)
}

# upstream attribution ----------------------------------------------------
#
# For a failing package, check whether any of its Imports/Depends/Suggests
# published a new version recently or is itself failing.  Turns an alert
# into an answer, and encodes the "sometimes you just wait" heuristic:
# upstream-attributed failures deserve a snooze-and-recheck default.
attribute_upstream <- function(pkg, db, all_checks, window_days = 10) {
  row <- db[db$Package == pkg, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  deps <- unlist(lapply(c("Depends", "Imports", "Suggests", "LinkingTo"),
                        function(f) row[[f]]))
  deps <- unique(trimws(sub("\\s*\\(.*\\)", "",
                            unlist(strsplit(deps[!is.na(deps)], ",")))))
  deps <- setdiff(deps, c("R", "", pkg))
  if (!length(deps)) return(NULL)

  sub_db <- db[db$Package %in% deps, c("Package", "Version", "Published")]
  recent <- sub_db[!is.na(sub_db$Published) &
                     as.Date(sub_db$Published) >= Sys.Date() - window_days, ,
                   drop = FALSE]
  failing <- unique(all_checks$package[all_checks$package %in% deps &
                                         all_checks$status %in% c("WARN", "WARNING", "ERROR", "FAIL")])
  if (nrow(recent) == 0 && !length(failing)) return(NULL)
  lines <- character()
  if (nrow(recent) > 0) {
    lines <- c(lines, sprintf("- %s %s published %s",
                              recent$Package, recent$Version, recent$Published))
  }
  if (length(failing)) {
    lines <- c(lines, sprintf("- %s currently WARN/ERROR on CRAN", failing))
  }
  paste(c("Possible upstream attribution:", lines,
          "", "If this looks upstream, snooze this issue (label: snoozed) and wait."),
        collapse = "\n")
}
