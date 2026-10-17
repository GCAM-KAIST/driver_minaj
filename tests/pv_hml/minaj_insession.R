# ONE open session: open once, then variants_minaj() makes low/mid/high in one call (and again, to show the second call).
# Usage (from the package root): Rscript tests/pv_hml/minaj_insession.R <tree> > tests/pv_hml/minaj_insession.log
#   (java on PATH; after drake_insession.R; writes tests/pv_hml/out/minaj/)
#   MINAJ_HOME: load minaj from this source folder instead of the installed package; MINAJ_STORE: store folder (default: the package default store)
tree <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/pv_hml/minaj_insession.R <GCAM tree>", call. = FALSE)
D <- normalizePath("tests/pv_hml", "/"); n <- "energy/A23.globaltech_capacity_factor"
el <- function(expr) { t0 <- proc.time()[["elapsed"]]; suppressMessages(force(expr)); proc.time()[["elapsed"]] - t0 }
cat(sprintf("RESULT minaj open once: %.1f s\n", el({ if (nzchar(Sys.getenv("MINAJ_HOME"))) pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE) else library(minaj)
  options(minaj.ask_secs = Inf); if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))
  open_minaj(tree) })))
v <- lapply(c(low = "low", mid = "mid", high = "high"), function(x) setNames(list(file.path(D, "inputs", paste0("A23_", x, ".csv"))), n))
cat(sprintf("RESULT minaj low+mid+high in one call: %.1f s\n", el(variants_minaj("electricity.xml", v, dest = file.path(D, "out/minaj"), ask = FALSE))))
cat(sprintf("RESULT minaj low+mid+high again: %.1f s\n", el(variants_minaj("electricity.xml", v, dest = file.path(D, "out/minaj"), ask = FALSE))))
for (x in c("low", "mid", "high")) cat(sprintf("RESULT %s: drake == minaj: %s\n", x, unname(tools::md5sum(file.path(D, "out/drake", paste0("electricity_", x, ".xml"))) == tools::md5sum(file.path(D, "out/minaj", paste0("electricity_", x, ".xml"))))))
cat(sprintf("RESULT mid == base electricity.xml: %s\n", unname(tools::md5sum(file.path(D, "out/minaj/electricity_mid.xml")) == tools::md5sum(file.path(info_minaj()$gcamdata, "xml/electricity.xml")))))
