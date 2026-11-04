# driver_minaj / readd / loadd / outdated behave like their drake counterparts (a tree with a full drake build in input/gcamdata/.drake).
# First, an XML file is converted in this process (needs only the store).
# Usage (from the package root): Rscript tests/test_drake_like.R [tree]   (tree: default environment variable MINAJ_TEST_TREE)
#   The drake checks report SKIP when drake is not installed or the drake cache has no L223.StubTechCapFactor_elec.
#   MINAJ_HOME: load minaj from this source folder instead of the installed package; MINAJ_STORE: store folder (default: the package default store)
tree <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(tree)) tree <- Sys.getenv("MINAJ_TEST_TREE")
if (!nzchar(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/test_drake_like.R <GCAM tree>, or set MINAJ_TEST_TREE", call. = FALSE)
if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
options(minaj.ask_secs = Inf); if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))
check <- function(label, ok) cat(if (isTRUE(ok)) "PASS" else "FAIL", label, "\n")
open_minaj(tree)
# write_xml converting a file in this process (as with minaj.xml_workers = 1, and in make_minaj): a store holding only the
# file's stored value and no stored XML, so Java runs; the file must have the md5 recorded when the store first wrote it
x <- "heat.xml"; ctx <- minaj:::new_ctx(); h <- minaj:::value_hash(x, ctx)
rec <- minaj:::rec_get("xml", minaj:::h64(list("xml", h, minaj:::writer_key())))
st0 <- minaj:::.nm$store; st1 <- file.path(tempdir(), "store_one"); p1 <- sub(st0, st1, minaj:::obj_path(h), fixed = TRUE)
dir.create(dirname(p1), recursive = TRUE); invisible(file.copy(minaj:::obj_path(h), p1))
f <- file.path(tempdir(), "xml_one", x); assign("store", st1, envir = minaj:::.nm)
s <- tryCatch(minaj:::write_xml(x, h, f), error = function(e) conditionMessage(e)); assign("store", st0, envir = minaj:::.nm)
check(sprintf("write_xml converts %s in this process, md5 as recorded, no part file left (%s)", x, substr(s, 1, 60)),
      identical(s, "converted") && !is.null(rec) && identical(unname(tools::md5sum(f)), rec$md5) &&
        !length(list.files(st1, "[.]part[.]xml$", recursive = TRUE)))
n <- "L223.StubTechCapFactor_elec"
if (!requireNamespace("drake", quietly = TRUE)) { cat("SKIP drake checks: drake is not installed\n"); quit(status = 0) }
cache <- tryCatch(drake::drake_cache(file.path(info_minaj()$gcamdata, ".drake")), error = function(e) NULL)
if (is.null(cache) || !cache$exists(n)) { cat("SKIP drake checks: the drake cache in", file.path(info_minaj()$gcamdata, ".drake"), "has no", n, "\n"); quit(status = 0) }
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
