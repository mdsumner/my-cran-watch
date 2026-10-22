# detail sections ---------------------------------------------------------
#
# Checks run on an unpacked package source directory, shared by the
# details workflow (CRAN tarballs) and the release board (GitHub source).
# Each returns list(summary = <one line>, problems = <data.frame>),
# optionally with `count`, the number shown on the dashboard (default: rows).

detail_sections <- list(
  urlchecker = function(dir) {
    # urlchecker >= 1.1 errors on flagged URLs unless fail = FALSE
    args <- list(dir, progress = FALSE)
    if ("fail" %in% names(formals(urlchecker::url_check))) args$fail <- FALSE
    res <- do.call(urlchecker::url_check, args)
    df <- data.frame(
      url     = as.character(res$URL),
      status  = as.character(res$Status),
      message = as.character(res$Message),
      new     = as.character(res$New),
      from    = vapply(res$From, paste, character(1), collapse = ", "),
      stringsAsFactors = FALSE)
    list(summary = if (nrow(df)) sprintf("%d URL problem(s)", nrow(df)) else "all URLs ok",
         problems = df)
  },
  # checktor: CRAN-reviewer requirements R CMD check does not surface.
  # issues() is its tidy table of findings (all severities, including
  # "opinion"); columns are kept as checktor names them
  checktor = function(dir) {
    res <- checktor::checktor(dir, verbose = FALSE, progress = FALSE)
    df <- as.data.frame(checktor::issues(res), stringsAsFactors = FALSE)
    for (col in names(df)) {
      if (is.list(df[[col]])) df[[col]] <- vapply(df[[col]], paste, character(1), collapse = ", ")
    }
    sev <- if ("severity" %in% names(df)) table(df$severity) else NULL
    list(summary = if (!nrow(df)) "no findings"
                   else if (length(sev)) paste(sprintf("%d %s", sev, names(sev)), collapse = ", ")
                   else sprintf("%d finding(s)", nrow(df)),
         problems = df,
         # opinion-tier findings are listed but not counted on the dashboard
         count = if ("severity" %in% names(df)) sum(df$severity %in% c("policy", "robustness"))
                 else nrow(df))
  }
)
