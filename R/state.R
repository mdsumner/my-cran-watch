# state layer ------------------------------------------------------------
#
# state/state.json is committed back to the repo after every run so the
# diff survives between runs and is itself under version control (the git
# history of the state file is a free audit log of everything observed).
#
# Shape:
#   packages: { <pkg>: { version: "x.y.z", first_seen: date } }
#   conditions: { <key>: { status, text_hash, streak, lifecycle,
#                          issue_number, first_seen, last_seen } }
#
# A condition key is "<pkg>|<flavor>" for check results, "<pkg>|universe"
# for r-universe gaps.  lifecycle is one of:
#   observed - seen at least once, not yet actionable (streak < required)
#   active   - actionable, issue open
#   snoozed  - human said ignore; respected until the condition changes
#              character (text hash changes) or is cleared

load_state <- function(path = "state/state.json") {
  if (!file.exists(path)) {
    return(list(packages = list(), conditions = list()))
  }
  jsonlite::read_json(path)
}

save_state <- function(state, path = "state/state.json") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(state, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

condition_key <- function(pkg, flavor) paste(pkg, flavor, sep = "|")

text_hash <- function(x) {
  # stable-enough content hash without adding a digest dependency
  ints <- utf8ToInt(paste(x, collapse = "\n"))
  as.character(sum(as.numeric(ints) * seq_along(ints)) %% 1e9)
}

# Advance one condition given the current observation.  Returns list of
# the updated condition record and an event (or NULL): one of
# "opened", "changed", "cleared".
step_condition <- function(cond, observed_status, observed_text, required) {
  now <- today()
  ev <- NULL
  h <- text_hash(observed_text)

  if (is.null(cond)) {
    cond <- list(status = observed_status, text_hash = h, streak = 1L,
                 lifecycle = "observed", issue_number = NULL,
                 first_seen = now, last_seen = now)
    return(list(cond = cond, event = NULL))
  }

  cond$last_seen <- now
  same <- identical(cond$status, observed_status) && identical(cond$text_hash, h)

  if (same) {
    cond$streak <- (cond$streak %||% 0L) + 1L
    if (identical(cond$lifecycle, "observed") && cond$streak >= required) {
      cond$lifecycle <- "active"
      ev <- "opened"
    }
  } else {
    # condition changed character: reset streak; a snoozed condition that
    # changes character wakes up (this is the "worsening" escape hatch)
    cond$status <- observed_status
    cond$text_hash <- h
    cond$streak <- 1L
    if (identical(cond$lifecycle, "snoozed")) {
      # wake up: the issue is already open, so back to active, and the
      # notify layer removes the snoozed label and comments
      cond$lifecycle <- "active"
      ev <- "changed"
    } else if (identical(cond$lifecycle, "active")) {
      ev <- "changed"
    }
  }
  list(cond = cond, event = ev)
}

clear_condition <- function(cond) {
  ev <- if (identical(cond$lifecycle, "active")) "cleared" else NULL
  list(cond = NULL, event = ev)
}
