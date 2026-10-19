# config + shared utilities ---------------------------------------------

read_config <- function(path = "config.yml") {
  cfg <- yaml::read_yaml(path)
  stopifnot(length(cfg$maintainer_emails) > 0)
  cfg$consecutive_required <- cfg$consecutive_required %||% 2
  cfg$notify_statuses <- cfg$notify_statuses %||% c("WARN", "WARNING", "ERROR", "FAIL")
  cfg$watch <- normalize_watch(cfg$watch)
  cfg$issue_roles <- cfg$issue_roles %||% "maintainer"
  cfg$issue_kinds <- cfg$issue_kinds %||% c("deadline", "archived")
  cfg
}

# watch: packages maintained by someone else, each either a bare name or
# {package, role}; role is "contribute" (I send PRs) or "interest" (I
# depend on it / want to know).  Returns a data.frame(package, role).
watch_roles <- c("contribute", "interest")
normalize_watch <- function(x) {
  if (!length(x)) return(data.frame(package = character(), role = character(),
                                    stringsAsFactors = FALSE))
  rows <- lapply(x, function(e) {
    if (is.character(e)) e <- list(package = e)
    role <- e$role %||% "interest"
    if (!role %in% watch_roles) {
      stop(sprintf("watch: %s has role '%s'; use one of %s", e$package, role,
                   paste(watch_roles, collapse = ", ")))
    }
    data.frame(package = e$package, role = role, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[!duplicated(out$package), , drop = FALSE]
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
