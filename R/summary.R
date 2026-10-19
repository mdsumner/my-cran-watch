# weekly summary ----------------------------------------------------------
#
# Most conditions are reported in one summary issue (label "summary") at
# most every summary_days, pointing at the dashboard; only the kinds in
# issue_kinds (default deadline, archived: rare and need action) still
# get an issue each.  The previous summary is closed when a new one opens.

site_url <- function() {
  repo <- strsplit(Sys.getenv("GITHUB_REPOSITORY", "mdsumner/my-cran-watch"), "/")[[1]]
  sprintf("https://%s.github.io/%s/", repo[1], repo[2])
}

# legacy per-flavor keys ("<pkg>|<flavor>") count as checks
condition_kind <- function(key) {
  k <- key_kind(key)
  ifelse(k %in% names(condition_kinds), k, "checks")
}

# close open issues for conditions now reported in the summary instead
close_superseded_issues <- function(state, cfg) {
  res <- gh_json(c("issue", "list", "--state", "open", "--json",
                   "number,body,labels", "--limit", "500"))
  if (is.null(res) || !NROW(res)) return(state)
  key <- regmatches(res$body, regexpr("condition-key: `[^`]+`", res$body))
  has <- grepl("condition-key: `[^`]+`", res$body)
  keys <- sub("condition-key: `([^`]+)`", "\\1", key)
  nums <- res$number[has]
  for (i in seq_along(keys)) {
    if (condition_kind(keys[i]) %in% cfg$issue_kinds) next
    gh(c("issue", "close", nums[i], "--comment",
         shQuote(sprintf("Now reported in the weekly summary and on the [dashboard](%s); closing.",
                         site_url()))))
    if (!is.null(state$conditions[[keys[i]]])) state$conditions[[keys[i]]]$issue_number <- NULL
  }
  msg("closed %d issue(s) superseded by the weekly summary",
      sum(!condition_kind(keys) %in% cfg$issue_kinds))
  state
}

summary_line <- function(key, cond, mine) {
  pkg <- sub("\\|[^|]*$", "", key)
  role <- mine$role[match(pkg, mine$package)]
  sprintf("- **%s** %s: %s%s", pkg, sub("^checks$", "CRAN checks", condition_kind(key)),
          cond$status %||% "?",
          if (!is.na(role) && role != "maintainer") sprintf(" _(%s)_", role) else "")
}

write_summary <- function(state, mine, checks, cfg, dry_run = FALSE) {
  last <- state$summary
  due <- is.null(last$date) ||
    as.numeric(Sys.Date() - as.Date(last$date)) >= (cfg$summary_days %||% 6)
  if (!due) return(state)

  conds <- state$conditions
  now <- vapply(conds, function(c) as.character(c$status %||% ""), character(1))
  before <- unlist(last$seen) %||% character()
  new <- names(now)[!names(now) %in% names(before) |
                      (names(now) %in% names(before) & now != before[names(now)])]
  gone <- setdiff(names(before), names(now))

  worst <- tapply(checks$status, checks$package, worst_status)
  tally <- function(s) sum(worst %in% s)
  lines <- function(keys) if (length(keys)) vapply(sort(keys), function(k)
    summary_line(k, conds[[k]] %||% list(status = before[[k]]), mine), "") else "- none"

  body <- c(
    sprintf("**[Dashboard](%s)** | %d packages | %d ERROR/FAIL | %d WARN | %d NOTE",
            site_url(), nrow(mine), tally(c("ERROR", "FAIL")),
            tally(c("WARN", "WARNING")), tally("NOTE")),
    "",
    sprintf("## New or changed since %s", last$date %||% "the first run"),
    lines(new), "",
    "## Cleared", lines(gone), "",
    sprintf("<details><summary>All %d current conditions</summary>", length(now)), "",
    lines(names(now)), "", "</details>", "",
    "---",
    sprintf("Opened automatically by my-cran-watch. %s still get their own issues.",
            paste(cfg$issue_kinds, collapse = " and ")))
  title <- sprintf("CRAN watch summary %s", today())

  if (dry_run) {
    msg("dry run: would open summary issue '%s' (%d new, %d cleared)",
        title, length(new), length(gone))
    return(state)
  }
  out <- gh(c("issue", "create", "--label", "summary", "--title", shQuote(title),
              "--body", shQuote(paste(body, collapse = "\n"))))
  if (out$status != 0) return(state)
  num <- suppressWarnings(as.integer(sub(".*/", "", utils::tail(out$output, 1))))
  if (!is.null(last$number)) {
    gh(c("issue", "close", last$number, "--comment",
         shQuote(sprintf("Superseded by #%s.", num))))
  }
  msg("opened summary issue #%s", num)
  state$summary <- list(date = today(), number = num, seen = as.list(now))
  state
}
