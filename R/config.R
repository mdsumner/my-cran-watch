# config + shared utilities ---------------------------------------------

read_config <- function(path = "config.yml") {
  cfg <- yaml::read_yaml(path)
  stopifnot(length(cfg$maintainer_emails) > 0)
  cfg$consecutive_required <- cfg$consecutive_required %||% 2
  cfg$notify_statuses <- cfg$notify_statuses %||% c("WARN", "WARNING", "ERROR", "FAIL")
  cfg
}

`%||%` <- function(a, b) if (is.null(a)) b else a

msg <- function(...) cat(sprintf(...), "\n")

# run a command, capture output, tolerate failure
run <- function(cmd, args, allow_fail = FALSE) {
  res <- suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE))
  status <- attr(res, "status") %||% 0L
  if (status != 0 && !allow_fail) {
    stop(sprintf("command failed (%s): %s %s\n%s", status, cmd,
                 paste(args, collapse = " "), paste(res, collapse = "\n")))
  }
  invisible(list(status = status, output = res))
}

today <- function() format(Sys.Date(), "%Y-%m-%d")
