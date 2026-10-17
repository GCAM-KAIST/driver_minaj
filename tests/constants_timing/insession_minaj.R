# In ONE open R session (as in RStudio): open once, then edit constants.R on disk, rebuild, switch back, rebuild. No reloads.
# Edit: socioeconomics.GDP_DIGITS 0 -> 1 (GDP rounding in the socioeconomics XML). Target: module_socio_SSP_xml (6 files).
# Usage (from the package root): Rscript tests/constants_timing/insession_minaj.R <tree> > tests/constants_timing/insession_minaj.log
#   (java on PATH; run before insession_drake.R, which reads the md5 snapshot this writes to tests/constants_timing/minaj_edited_md5.rds,
#   git-ignored)
#   MINAJ_HOME: load minaj from this source folder instead of the installed package; MINAJ_STORE: store folder (default: the package default store)
tree <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/constants_timing/insession_minaj.R <GCAM tree>", call. = FALSE)
G <- file.path(tree, "input", "gcamdata"); F <- file.path(G, "R/constants.R"); XCH <- "module_socio_SSP_xml"
X <- c(paste0("socioeconomics_SSP", 1:5, ".xml"), "socioeconomics_CORE.xml")
el <- function(expr) { t0 <- proc.time()[["elapsed"]]; m <- character(0)
  withCallingHandlers(force(expr), message = function(x) { m <<- c(m, conditionMessage(x)); invokeRestart("muffleMessage") })
  list(s = proc.time()[["elapsed"]] - t0, msgs = m) }
md5 <- function(d) unname(tools::md5sum(file.path(d, X)))
bak <- readBin(F, "raw", file.size(F)); mt <- file.mtime(F)
tryCatch({
  r <- el({ if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
            options(minaj.ask_secs = Inf, minaj.report_siblings = FALSE); if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))
            open_minaj(G) })
  cat(sprintf("RESULT minaj open once: %.1f s\n", r$s))
  r <- el(driver_minaj(stop_after = XCH, ask = FALSE)); cat(sprintf("RESULT minaj nothing changed: %.1f s\n", r$s))
  orig <- md5(file.path(G, "xml_minaj"))

  txt <- readLines(F, warn = FALSE); i <- grep("^socioeconomics\\.GDP_DIGITS +<- +0", txt); stopifnot(length(i) == 1)
  txt[i] <- sub("<- +0", "<- 1", txt[i]); writeLines(txt, F)
  r <- el(driver_minaj(stop_after = XCH, ask = FALSE))
  cat(sprintf("RESULT minaj after constants edit (no reload): %.1f s | re-sourced constants.R: %s | package reloaded: %s | %s\n", r$s,
              any(grepl("re-sourced R/constants.R", r$msgs)), any(grepl("reloading gcamdata", r$msgs)), sub(".*[|] ", "", grep("chunks run", r$msgs, value = TRUE)[1])))
  ed <- md5(file.path(G, "xml_minaj")); cat("RESULT files changed by the edit:", sum(ed != orig), "of", length(X), "\n")
  saveRDS(ed, "tests/constants_timing/minaj_edited_md5.rds")

  writeBin(bak, F); Sys.setFileTime(F, mt)
  r <- el(driver_minaj(stop_after = XCH, ask = FALSE))
  cat(sprintf("RESULT minaj after switching back (no reload): %.1f s | %s\n", r$s, sub(".*[|] ", "", grep("chunks run", r$msgs, value = TRUE)[1])))
  cat("RESULT back to the original files:", identical(md5(file.path(G, "xml_minaj")), orig), "\n")
}, finally = { writeBin(bak, F); Sys.setFileTime(F, mt) })
