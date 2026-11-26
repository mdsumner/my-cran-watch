# check timings -----------------------------------------------------------
#
# CRAN publishes install/check times per package and flavor but keeps no
# history: each run of its check machines overwrites the last.  Every
# watcher run appends what it sees to state/timings.csv, so "is this
# package getting slower on Windows" has an answer.
#
# One row per package/flavor each time the recorded values change; `date`
# is the run that first saw them.  Timings are wall-clock seconds on
# shared machines and wobble from run to run, so read trends, not single
# rows.  Flavors in ignore_flavors are still recorded: a flaky status is
# no reason to lose the timing.

timing_cols <- c("date", "package", "version", "flavor",
                 "t_install", "t_check", "t_total", "status")

# timings for a set of packages from tools::CRAN_check_results()
fetch_timings <- function(pkgs, res = tools::CRAN_check_results()) {
  res <- res[res$Package %in% pkgs, , drop = FALSE]
  col <- function(name) {
    # T_install/T_check/T_total today; match loosely in case CRAN renames
    hit <- which(tolower(gsub("[^A-Za-z]", "", names(res))) == name)
    if (!length(hit)) return(rep(NA_real_, nrow(res)))
    suppressWarnings(as.numeric(as.character(res[[hit[1]]])))
  }
  out <- data.frame(
    date      = rep(today(), nrow(res)),
    package   = as.character(res$Package),
    version   = as.character(res$Version),
    flavor    = as.character(res$Flavor),
    t_install = col("tinstall"),
    t_check   = col("tcheck"),
    t_total   = col("ttotal"),
    status    = as.character(res$Status),
    stringsAsFactors = FALSE
  )
  out[order(out$package, out$flavor), , drop = FALSE]
}

read_timings <- function(path = "state/timings.csv") {
  if (!file.exists(path)) {
    empty <- as.data.frame(stats::setNames(
      replicate(length(timing_cols), character(), simplify = FALSE), timing_cols),
      stringsAsFactors = FALSE)
    for (n in c("t_install", "t_check", "t_total")) empty[[n]] <- numeric()
    return(empty)
  }
  # version must stay character: "0.10" is not 0.1
  utils::read.csv(path, stringsAsFactors = FALSE,
                  colClasses = c(date = "character", package = "character",
                                 version = "character", flavor = "character",
                                 t_install = "numeric", t_check = "numeric",
                                 t_total = "numeric", status = "character"))
}

# Append the rows of `new` whose values differ from the latest recorded row
# for the same package/flavor.  A rerun on the same day replaces that
# day's rows.  Returns the number of rows added.
record_timings <- function(new, path = "state/timings.csv") {
  old <- read_timings(path)
  old <- old[!old$date %in% unique(new$date), , drop = FALSE]
  key <- function(d) paste(d$package, d$flavor, sep = "|")
  sig <- function(d) paste(d$version, d$t_install, d$t_check, d$t_total,
                           d$status, sep = "|")
  # no timing at all (CRAN lists some flavors without one): nothing to keep
  new <- new[!(is.na(new$t_install) & is.na(new$t_check) & is.na(new$t_total)),
             , drop = FALSE]
  if (nrow(old)) {
    last <- old[!duplicated(key(old), fromLast = TRUE), , drop = FALSE]
    prev <- stats::setNames(sig(last), key(last))
    new <- new[is.na(prev[key(new)]) | prev[key(new)] != sig(new), , drop = FALSE]
  }
  if (nrow(new) || !file.exists(path)) {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(rbind(old[timing_cols], new[timing_cols]), path,
                     row.names = FALSE, na = "")
  }
  nrow(new)
}

# history for one package, e.g. timing_history("vapour", "windows")
timing_history <- function(pkg, flavor = "", path = "state/timings.csv") {
  x <- read_timings(path)
  x <- x[x$package == pkg & grepl(flavor, x$flavor), , drop = FALSE]
  x[order(x$flavor, x$date), , drop = FALSE]
}
