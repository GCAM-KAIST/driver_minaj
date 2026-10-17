# Timed driver_drake(stop_after = ...) in a fresh R session, as a user would run it.
# Usage (from <tree>/input/gcamdata; called by run_incelas_test.sh): Rscript <path>/run_gcamdata_incelas_drake.R <tag> <xml chunk>
#   (java on PATH; drake and devtools installed)
a <- commandArgs(trailingOnly = TRUE); tag <- a[1]; xch <- a[2]
d <- function(b, a) as.numeric(difftime(b, a, units = "secs"))
t0 <- Sys.time(); suppressMessages(devtools::load_all(".", quiet = TRUE)); t1 <- Sys.time()
invisible(suppressMessages(driver_drake(stop_after = xch, quiet = TRUE))); t2 <- Sys.time()
cat(sprintf("RESULT drake %s load=%.1f driver=%.1f total=%.1f\n", tag, d(t1, t0), d(t2, t1), d(t2, t0)))
