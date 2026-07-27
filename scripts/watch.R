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
v <- diff_versions(state, mine);            state <- v$state
c1 <- diff_checks(state, checks, cfg);      state <- c1$state
u <- diff_universe(state, mine, uni, cfg);  state <- u$state
m <- diff_metadata(state, mine, cfg);       state <- m$state
events <- c(v$events, c1$events, u$events, m$events)
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
