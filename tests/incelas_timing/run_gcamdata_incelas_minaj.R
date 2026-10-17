# Timed driver_minaj(stop_after = ...) in a fresh R session.
# Usage (from anywhere; called by run_incelas_test.sh): Rscript <path>/run_gcamdata_incelas_minaj.R <tag> <xml chunk> <tree or its input/gcamdata folder>   (java on PATH)
#   MINAJ_HOME: load minaj from this source folder instead of the installed package; MINAJ_STORE: store folder (default: the package default store)
a <- commandArgs(trailingOnly = TRUE); tag <- a[1]; xch <- a[2]
d <- function(b, a) as.numeric(difftime(b, a, units = "secs"))
t0 <- Sys.time(); if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
options(minaj.ask_secs = Inf, minaj.report_siblings = FALSE); if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))
suppressMessages(open_minaj(a[3])); t1 <- Sys.time()
invisible(suppressMessages(driver_minaj(stop_after = xch, ask = FALSE, quiet = TRUE))); t2 <- Sys.time()
cat(sprintf("RESULT minaj %s load=%.1f driver=%.1f total=%.1f\n", tag, d(t1, t0), d(t2, t1), d(t2, t0)))
