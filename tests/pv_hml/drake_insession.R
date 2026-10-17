# ONE open session: load_all once, then for low/mid/high copy the CSV into place, driver_drake(stop_after), keep the XML. Then restore.
# Usage (from the package root): Rscript tests/pv_hml/drake_insession.R <tree> > tests/pv_hml/drake_insession.log
#   (java on PATH; drake and devtools installed; after make_csvs.R; writes tests/pv_hml/out/drake/)
tree <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/pv_hml/drake_insession.R <GCAM tree>", call. = FALSE)
D <- normalizePath("tests/pv_hml", "/"); dir.create(file.path(D, "out/drake"), recursive = TRUE, showWarnings = FALSE)
G <- file.path(tree, "input", "gcamdata"); setwd(G)
CSV <- "inst/extdata/energy/A23.globaltech_capacity_factor.csv"; bak <- readBin(CSV, "raw", file.size(CSV)); mt <- file.mtime(CSV)
tryCatch({
  el <- function(expr) { t0 <- proc.time()[["elapsed"]]; suppressMessages(force(expr)); proc.time()[["elapsed"]] - t0 }
  cat(sprintf("RESULT drake load_all once: %.1f s\n", el(devtools::load_all(".", quiet = TRUE))))
  tot <- 0
  for (v in c("low", "mid", "high")) {
    file.copy(file.path(D, "inputs", paste0("A23_", v, ".csv")), CSV, overwrite = TRUE)
    s <- el(driver_drake(stop_after = "module_energy_electricity_xml", quiet = TRUE)); tot <- tot + s
    file.copy("xml/electricity.xml", file.path(D, "out/drake", paste0("electricity_", v, ".xml")), overwrite = TRUE)
    cat(sprintf("RESULT drake %s: %.1f s\n", v, s))
  }
  cat(sprintf("RESULT drake low+mid+high: %.1f s\n", tot))
  writeBin(bak, CSV); Sys.setFileTime(CSV, mt)
  cat(sprintf("RESULT drake back to base (restore + driver_drake): %.1f s\n", el(driver_drake(stop_after = "module_energy_electricity_xml", quiet = TRUE))))
}, finally = { writeBin(bak, CSV); Sys.setFileTime(CSV, mt) })
