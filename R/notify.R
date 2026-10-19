# notify layer -----------------------------------------------------------
#
# All reporting is GitHub issues in this repo, via the gh CLI (present on
# GitHub Actions runners; authenticated by GH_TOKEN).  One issue per
# condition, closed automatically when the condition clears.  The issue
# history is the searchable log of every CRAN wobble.
#
# Snoozing: add the "snoozed" label to an issue.  The watcher stops
# commenting on it, but wakes it (removes the label, comments) if the
# condition changes character.  Correctly deciding to do nothing is a
# first-class state.

issue_title <- function(ev) {
  switch(ev$type,
    check        = sprintf("%s: %s on %s", ev$package, ev$status, ev$flavor),
    checks       = sprintf("%s: %s in CRAN checks", ev$package, ev$status),
    universe_gap = sprintf("%s: r-universe gap", ev$package),
    metadata     = sprintf("%s: CRAN metadata points away from repo", ev$package),
    deadline     = sprintf("%s: CRAN deadline", ev$package),
    archived     = sprintf("%s: archived on CRAN", ev$package),
    revdeps      = sprintf("%s: reverse dependencies failing", ev$package),
    canary       = sprintf("%s: %s", ev$package,
                           sub("^\\[(.*)\\]\\(.*$", "\\1", ev$detail %||% "GDAL canary failing")),
    stop("no issue title for event type ", ev$type))
}

# one label per role, so issues filter into mine / contribute / interest
role_labels <- c(maintainer = "mine", contribute = "contribute", interest = "interest")

ensure_labels <- function() {
  for (l in c(role_labels, "summary")) {
    gh(c("label", "create", l, "--force", "--color", "ededed"))
  }
}

issue_body <- function(ev, attribution = NULL) {
  body <- c(
    sprintf("condition-key: `%s`", ev$key),
    sprintf("role: %s", ev$role %||% "maintainer"),
    "",
    switch(ev$type,
      check = c(sprintf("Status: **%s** on `%s`", ev$status, ev$flavor), "",
                "```", substr(ev$detail %||% "", 1, 4000), "```"),
      checks = c(sprintf("Worst status: **%s**", ev$status), "",
                 "```", substr(ev$detail %||% "", 1, 6000), "```"),
      ev$detail),
    ""
  )
  if (!is.null(attribution)) body <- c(body, attribution, "")
  body <- c(body,
    "---",
    "Opened automatically by my-cran-watch. This issue will be closed",
    "automatically when the condition clears. Add the `snoozed` label to",
    "silence it until it changes character.")
  paste(body, collapse = "\n")
}

# gh failures are tolerated mid-run (state must still be saved) but
# counted, so watch.R can fail the job at the end instead of going green
.gh_failures <- new.env()
.gh_failures$n <- 0L

gh <- function(args) {
  out <- run("gh", args, allow_fail = TRUE)
  if (out$status != 0) {
    .gh_failures$n <- .gh_failures$n + 1L
    msg("gh %s failed (%s): %s", paste(args[1:2], collapse = " "), out$status,
        paste(out$output, collapse = " | "))
  }
  out
}

gh_json <- function(args) {
  out <- gh(args)
  if (out$status != 0) return(NULL)
  tryCatch(jsonlite::fromJSON(paste(out$output, collapse = "\n")),
           error = function(e) NULL)
}

find_issue <- function(key) {
  # search open issues for the condition key marker
  res <- gh_json(c("issue", "list", "--state", "open", "--json",
                   "number,title,body,labels", "--limit", "200"))
  if (is.null(res) || NROW(res) == 0) return(NULL)
  hit <- which(vapply(res$body, function(b) grepl(key, b, fixed = TRUE), logical(1)))
  if (!length(hit)) return(NULL)
  labels <- res$labels[[hit[1]]]
  list(number = res$number[hit[1]],
       snoozed = !is.null(labels) && NROW(labels) > 0 && "snoozed" %in% labels$name)
}

notify_event <- function(ev, attribution = NULL) {
  if (identical(ev$event, "opened")) {
    existing <- find_issue(ev$key)
    if (!is.null(existing)) return(existing$number)
    label <- role_labels[[ev$role %||% "maintainer"]]
    out <- gh(c("issue", "create", "--label", label,
                "--title", shQuote(issue_title(ev)),
                "--body", shQuote(issue_body(ev, attribution))))
    if (out$status != 0) return(NULL)
    num <- suppressWarnings(as.integer(sub(".*/", "", utils::tail(out$output, 1))))
    if (is.na(num)) return(NULL)
    msg("opened issue #%s for %s", num, ev$key)
    return(num)

  } else if (identical(ev$event, "changed")) {
    existing <- find_issue(ev$key)
    if (is.null(existing)) return(notify_event(modifyList(ev, list(event = "opened")),
                                              attribution))
    if (existing$snoozed) {
      gh(c("issue", "edit", existing$number, "--remove-label", "snoozed"))
    }
    gh(c("issue", "comment", existing$number,
         "--body", shQuote(paste0("Condition changed character:\n\n",
                                  issue_body(ev, attribution)))))
    msg("commented on issue #%s for %s", existing$number, ev$key)
    return(existing$number)

  } else if (identical(ev$event, "cleared")) {
    existing <- if (!is.null(ev$issue_number)) list(number = ev$issue_number)
                else find_issue(ev$key)
    if (!is.null(existing) && !is.null(existing$number)) {
      gh(c("issue", "close", existing$number,
           "--comment", shQuote("Condition no longer observed; closing.")))
      msg("closed issue #%s for %s", existing$number, ev$key)
    }
    return(NULL)
  }
  NULL
}
