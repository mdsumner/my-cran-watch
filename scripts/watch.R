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
for (r in unique(mine$role)) {
  msg("%s: %d CRAN packages: %s", r, sum(mine$role == r),
      paste(sort(mine$package[mine$role == r]), collapse = ", "))
}

raw_checks <- tools::CRAN_check_results()
checks <- fetch_check_results(mine$package, cfg, raw_checks)
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
    type <- condition_kinds[key_kind(key)]
    if (is.na(type)) type <- "check"
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
# deadlines, archival and gdal-r-ci canaries act on first sight; failing
# revdeps flake like any check, so they wait for consecutive_required
watchers <- list(
  deadline = list(watch_deadlines(mine, db), 1L),
  archived = list(watch_archived(state, db), 1L),
  revdeps  = list(watch_revdeps(mine, db, raw_checks), cfg$consecutive_required),
  canary   = list(watch_canary(cfg), 1L))
w_events <- list()
for (kind in names(watchers)) {
  d <- diff_kind(state, watchers[[kind]][[1]], kind, watchers[[kind]][[2]])
  state <- d$state
  w_events <- c(w_events, d$events)
}
events <- c(v$events, c1$events, u$events, m$events, w_events)
events <- reconcile_orphans(state, events)
msg("%d event(s)", length(events))

# act ---------------------------------------------------------------------
if (!dry_run) ensure_labels()
for (ev in events) {
  if (ev$type == "new_version") {
    msg("new version: %s %s -> %s (published %s)",
        ev$package, ev$from, ev$to, ev$published)
    repo <- cfg$repo_map[[ev$package]]
    if (!identical(ev$role, "maintainer")) {
      msg("  %s is watched (%s), not mine; reporting only", ev$package, ev$role)
    } else if (is.null(repo)) {
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
    ev$role <- mine$role[match(ev$package, mine$package)]
    if (is.na(ev$role)) ev$role <- "maintainer"  # canaries, archived
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
if (.gh_failures$n > 0) {
  msg("%d gh call(s) failed; failing the job so it does not report green",
      .gh_failures$n)
  quit(status = 1)
}
