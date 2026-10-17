# Low / mid / high PV capacity-factor CSVs: future columns (initial-future-year, 2050, 2100) of PV and PV_storage x 0.9 / 1.0 / 1.1.
# Usage (from the package root): Rscript tests/pv_hml/make_csvs.R <tree>   (writes tests/pv_hml/inputs/, git-ignored; first of
#   make_csvs.R -> drake_insession.R -> minaj_insession.R -> persistent/run_pm_tests.sh)
tree <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/pv_hml/make_csvs.R <GCAM tree>", call. = FALSE)
src <- file.path(tree, "input/gcamdata/inst/extdata/energy/A23.globaltech_capacity_factor.csv")
dir.create("tests/pv_hml/inputs", showWarnings = FALSE)
raw <- readChar(src, file.size(src), useBytes = TRUE); eol <- if (grepl("\r\n", raw, fixed = TRUE)) "\r\n" else "\n"
lines <- strsplit(raw, eol, fixed = TRUE)[[1]]
hdr <- strsplit(lines[grep("^supplysector,", lines)], ",")[[1]]; fut <- match(c("initial-future-year", "2050", "2100"), hdr)
for (v in c(low = 0.9, mid = 1.0, high = 1.1)) {
  out <- lines
  for (i in grep("^electricity,solar,(PV|PV_storage),", lines)) { f <- strsplit(lines[i], ",")[[1]]; f[fut] <- as.character(as.numeric(f[fut]) * v); out[i] <- paste(f, collapse = ",") }
  nm <- names(which(c(low = 0.9, mid = 1.0, high = 1.1) == v))
  writeChar(paste0(paste(out, collapse = eol), if (grepl(paste0(eol, "$"), raw)) eol else ""), file.path("tests/pv_hml/inputs", paste0("A23_", nm, ".csv")), eos = NULL, useBytes = TRUE)
}
cat("PV rows high:", grep("^electricity,solar,PV", readLines("tests/pv_hml/inputs/A23_high.csv"), value = TRUE), sep = "\n")
