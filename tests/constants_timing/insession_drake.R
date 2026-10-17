# In ONE open R session: load_all once, then edit constants.R (GDP_DIGITS 0 -> 1) and call driver_drake(stop_after) as a user would.
# Checks whether drake wipes its cache and whether its files reflect the edit, with and without re-running load_all.
# Usage (from the package root): Rscript tests/constants_timing/insession_drake.R <tree> > tests/constants_timing/insession_drake.log
#   (java on PATH; drake and devtools installed; a tree with a full drake build; run after insession_minaj.R)
tree <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/constants_timing/insession_drake.R <GCAM tree>", call. = FALSE)
want <- readRDS("tests/constants_timing/minaj_edited_md5.rds")   # minaj's files after the same edit, written by insession_minaj.R
G <- file.path(tree, "input", "gcamdata"); setwd(G); F <- "R/constants.R"; XCH <- "module_socio_SSP_xml"
X <- c(paste0("socioeconomics_SSP", 1:5, ".xml"), "socioeconomics_CORE.xml")
md5 <- function() unname(tools::md5sum(file.path("xml", X)))
orig <- md5()
bak <- readBin(F, "raw", file.size(F)); mt <- file.mtime(F)
tryCatch({
  run <- function(label) { wiped <- FALSE; t0 <- proc.time()[["elapsed"]]
    withCallingHandlers(invisible(driver_drake(stop_after = XCH)), message = function(m) { if (grepl("constants.R has been changed", conditionMessage(m))) wiped <<- TRUE; invokeRestart("muffleMessage") })
    s <- proc.time()[["elapsed"]] - t0; m <- md5()
    cat(sprintf("RESULT drake %s: %.1f s | cache wiped: %s | files = original: %s | files = correct edited result: %s\n", label, s, wiped, identical(m, orig), identical(m, want))) }
  t0 <- proc.time()[["elapsed"]]; suppressMessages(devtools::load_all(".", quiet = TRUE)); cat(sprintf("RESULT drake load_all once: %.1f s\n", proc.time()[["elapsed"]] - t0))
  run("nothing changed")
  txt <- readLines(F, warn = FALSE); i <- grep("^socioeconomics\\.GDP_DIGITS +<- +0", txt); stopifnot(length(i) == 1); txt[i] <- sub("<- +0", "<- 1", txt[i]); writeLines(txt, F)
  run("after constants edit, no load_all")
  t0 <- proc.time()[["elapsed"]]; suppressMessages(devtools::load_all(".", quiet = TRUE)); cat(sprintf("RESULT drake load_all again: %.1f s\n", proc.time()[["elapsed"]] - t0))
  run("after constants edit + load_all")
  writeBin(bak, F); Sys.setFileTime(F, mt)
  t0 <- proc.time()[["elapsed"]]; suppressMessages(devtools::load_all(".", quiet = TRUE)); cat(sprintf("RESULT drake load_all after switching back: %.1f s\n", proc.time()[["elapsed"]] - t0))
  run("after switching back + load_all")
}, finally = { writeBin(bak, F); Sys.setFileTime(F, mt) })
