# summary site ------------------------------------------------------------
#
# docs/status.json is rewritten every run and committed with the state;
# docs/index.html is static and renders it.  Serve docs/ with GitHub
# Pages (Settings > Pages > Deploy from branch: main, /docs).

status_rank <- c(OK = 0, NOTE = 1, WARN = 2, WARNING = 2, ERROR = 3, FAIL = 3)

worst_status <- function(s) {
  s <- as.character(s)
  if (!length(s)) return(NA_character_)
  r <- status_rank[s]
  r[is.na(r)] <- 0
  s[which.max(r)]
}

write_site <- function(state, mine, checks_all, uni, cfg, path = "docs/status.json") {
  repo <- Sys.getenv("GITHUB_REPOSITORY", "mdsumner/my-cran-watch")
  conds <- state$conditions
  cond_rows <- lapply(names(conds), function(key) {
    c <- conds[[key]]
    list(key = key, package = sub("\\|[^|]*$", "", key), kind = key_kind(key),
         status = c$status, lifecycle = c$lifecycle,
         issue = if (!is.null(c$issue_number))
           sprintf("https://github.com/%s/issues/%s", repo, c$issue_number),
         first_seen = c$first_seen)
  })
  pkg_rows <- lapply(seq_len(nrow(mine)), function(i) {
    pkg <- mine$package[i]
    ch <- checks_all[checks_all$package == pkg, , drop = FALSE]
    u <- if (is.null(uni)) data.frame(ok = logical()) else uni[uni$package == pkg, , drop = FALSE]
    list(package = pkg, role = mine$role[i], version = mine$version[i],
         published = mine$published[i], repo = cfg$repo_map[[pkg]],
         worst = worst_status(ch$status),
         flavors = as.list(stats::setNames(ch$status, ch$flavor)),
         universe = if (mine$role[i] != "maintainer") NULL
                    else if (!nrow(u)) "absent" else if (any(u$ok)) "ok" else "failing",
         conditions = Filter(function(r) identical(r$package, pkg), cond_rows))
  })
  other <- Filter(function(r) !r$package %in% mine$package, cond_rows)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(list(generated = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
                            repo = repo, packages = pkg_rows, other = other),
                       path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}
