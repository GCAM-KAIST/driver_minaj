# driver_minaj / readd / loadd / outdated behave like their drake counterparts (a tree with a full drake build in input/gcamdata/.drake).
# Usage (from the package root): Rscript tests/test_drake_like.R <tree>   (drake installed)
#   MINAJ_HOME: load minaj from this source folder instead of the installed package; MINAJ_STORE: store folder (default: the package default store)
tree <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/test_drake_like.R <GCAM tree>", call. = FALSE)
if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
options(minaj.ask_secs = Inf); if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))
check <- function(label, ok) cat(if (isTRUE(ok)) "PASS" else "FAIL", label, "\n")
open_minaj(tree)
cache <- drake::drake_cache(file.path(info_minaj()$gcamdata, ".drake"))
n <- "L223.StubTechCapFactor_elec"
a <- readd(L223.StubTechCapFactor_elec); b <- drake::readd(n, character_only = TRUE, cache = cache)
check("readd(bare symbol) is identical() to drake::readd", identical(a, b))
check("readd(\"name\") and readd(drake target name, character_only = TRUE) give the same",
      identical(readd("energy/A23.globaltech_capacity_factor"), readd("energy.A23.globaltech_capacity_factor", character_only = TRUE)))
e <- new.env(); loadd(L223.StubTechCapFactor_elec, list = "energy/A23.globaltech_capacity_factor", envir = e)
check("loadd assigns under drake target names", setequal(ls(e), c("L223.StubTechCapFactor_elec", "energy.A23.globaltech_capacity_factor")) && identical(e$L223.StubTechCapFactor_elec, b))
o <- outdated(); check(sprintf("outdated() returns chunk names (%d chunks)", length(o)), is.character(o))
r <- driver_minaj(stop_after = "module_energy_L223.electricity", write_xml = FALSE)
dd <- gcamdata:::load_from_cache(names(r), cache = cache)
check(sprintf("driver_minaj(stop_after) returns the chunk's %d outputs like driver_drake (identical to load_from_cache)", length(r)),
      length(r) > 0 && identical(r, dd[names(r)]))
r2 <- driver_minaj(stop_before = "module_energy_electricity_xml", write_xml = FALSE)
check(sprintf("driver_minaj(stop_before) returns the chunk's %d inputs", length(r2)), setequal(names(r2), minaj:::.nm$by_chunk[["module_energy_electricity_xml"]]))
check("driver_minaj(user_modifications=) points to variants_minaj", inherits(try(driver_minaj(user_modifications = "x"), silent = TRUE), "try-error"))
