#!/usr/bin/env Rscript
# the daily loop: fetch -> diff -> act/notify -> save state
#
# Run from the repo root:  Rscript scripts/watch.R [--dry-run]

for (f in list.files("R", full.names = TRUE)) source(f)
dry_run <- "--dry-run" %in% commandArgs(trailingOnly = TRUE)

cfg   <- read_config()
state <- load_state()

msg("== my-cran-watch run %s%s ==", today(), if (dry_run) " (dry run)" else "")

# fetch -------------------------------------------------------------------
db    <- tools::CRAN_package_db()
mine  <- fetch_my_packages(cfg, db)
msg("maintaining %d CRAN packages: %s", nrow(mine),
    paste(sort(mine$package), collapse = ", "))

checks <- fetch_check_results(mine$package, cfg)
uni    <- fetch_universe(cfg)
inc    <- fetch_incoming(cfg)
if (!is.null(inc) && NROW(inc) > 0) {
  ours <- inc[inc$package %in% c(mine$package,
                                 setdiff(inc$package, character(0))), , drop = FALSE]
  if (NROW(ours) > 0) {
    msg("incoming queue: %s",
        paste(sprintf("%s (%s)", ours$package, ours$subfolder), collapse = ", "))
  }
}

# diff --------------------------------------------------------------------
# reconcile ---------------------------------------------------------------
# transitions can be missed (dry runs advance streaks without creating
# issues; gh calls can fail) so heal rather than trust: any active
# condition with no issue gets an opened event, unless one is pending
reconcile_orphans <- function(state, events) {
  pending <- vapply(events, function(e) e$key %||% "", character(1))
  for (key in names(state$conditions)) {
    cond <- state$conditions[[key]]
    if (!identical(cond$lifecycle, "active") || !is.null(cond$issue_number) ||
        key %in% pending) next
    parts <- strsplit(key, "|", fixed = TRUE)[[1]]
    type <- switch(parts[2], universe = "universe_gap",
                   metadata = "metadata", "check")
    events[[length(events) + 1L]] <- list(
      type = type, event = "opened", key = key,
      package = parts[1], flavor = parts[2],
      status = cond$status, detail = cond$detail %||% "")
  }
  events
}

v <- diff_versions(state, mine);            state <- v$state
c1 <- diff_checks(state, checks, cfg);      state <- c1$state
u <- diff_universe(state, mine, uni, cfg);  state <- u$state
m <- diff_metadata(state, mine, cfg);       state <- m$state
events <- c(v$events, c1$events, u$events, m$events)
events <- reconcile_orphans(state, events)
msg("%d event(s)", length(events))

# act ---------------------------------------------------------------------
for (ev in events) {
  if (ev$type == "new_version") {
    msg("new version: %s %s -> %s (published %s)",
        ev$package, ev$from, ev$to, ev$published)
    repo <- cfg$repo_map[[ev$package]]
    if (is.null(repo)) {
      msg("  %s not in repo_map; reporting only, not tagging", ev$package)
    } else if (!dry_run) {
      tryCatch(tag_release(ev$package, ev$to, repo),
               error = function(e) msg("  tagging failed: %s", conditionMessage(e)))
    }
  } else {
    attribution <- if (ev$type == "check" && ev$event %in% c("opened", "changed") &&
                       ev$status %in% c("WARN", "WARNING", "ERROR", "FAIL")) {
      attribute_upstream(ev$package, db, checks)
    } else NULL
    if (!dry_run) {
      num <- notify_event(ev, attribution)
      if (!is.null(num) && !is.null(state$conditions[[ev$key]])) {
        state$conditions[[ev$key]]$issue_number <- num
      }
    } else {
      msg("dry run: would %s issue for %s", ev$event %||% "?", ev$key)
    }
  }
}

save_state(state)
msg("state saved; done")
