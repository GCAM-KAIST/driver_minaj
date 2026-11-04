# CSV files and the data map of driver_minaj() against gcamdata's own driver() on a tree (vanilla GCAM 9.1):
# (1) driver_minaj(write_outputs = TRUE) from the store: the same CSV files as driver(write_outputs = TRUE), each
#     byte-identical (md5); (2) a second call writes none; (3) driver_minaj(return_data_map_only = TRUE) identical to
#     driver(return_data_map_only = TRUE), both ordered by name and output.
# Usage (from the package root): Rscript tests/test_outputs.R [tree] [scratch folder]
#   tree: default environment variable MINAJ_TEST_TREE. The store (MINAJ_STORE, else the default store) should hold a full
#   build of the tree. The reference is <scratch>/reference: driver(write_outputs = TRUE, write_xml = FALSE,
#   return_data_map_only = TRUE), as gcamdata's test_oldnew.R runs it, in a separate R process (a full gcamdata build; its
#   time and peak memory are printed), unless <scratch>/reference/data_map.rds is already there. MINAJ_HOME as in the
#   other tests.
a <- commandArgs(trailingOnly = TRUE)
tree <- if (length(a) >= 1L) a[1] else Sys.getenv("MINAJ_TEST_TREE")
scr <- if (length(a) >= 2L) a[2] else file.path(tempdir(), "outputs")
if (!nzchar(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/test_outputs.R <GCAM tree> [scratch folder]", call. = FALSE)
gd <- normalizePath(file.path(tree, "input", "gcamdata"), "/")
dir.create(scr, recursive = TRUE, showWarnings = FALSE); scr <- normalizePath(scr, "/"); ref <- file.path(scr, "reference")
n_pass <- 0L; n_fail <- 0L
check <- function(ok, what) {
  if (isTRUE(ok)) { n_pass <<- n_pass + 1L; cat("PASS", what, "\n") } else { n_fail <<- n_fail + 1L; cat("FAIL", what, "\n") }
}

if (!file.exists(file.path(ref, "data_map.rds"))) {                       # the reference, from gcamdata's own driver()
  dir.create(ref, showWarnings = FALSE)
  code <- sprintf(paste('suppressMessages(pkgload::load_all("%s", quiet = TRUE))',
                        'm <- driver(write_outputs = TRUE, write_xml = FALSE, return_data_map_only = TRUE, outdir = "%s", xmldir = "%s", quiet = TRUE)',
                        'saveRDS(m, "%s")', sep = "; "), gd, file.path(ref, "outputs"), file.path(ref, "xml"), file.path(ref, "data_map.rds"))
  rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  t0 <- Sys.time(); p <- processx::process$new(rs, c("-e", code), wd = gd, stdout = file.path(ref, "driver.log"), stderr = "2>&1")
  peak <- 0
  while (p$is_alive()) { peak <- max(peak, tryCatch(ps::ps_memory_info(p$as_ps_handle())[["rss"]], error = function(e) 0)); Sys.sleep(5) }
  cat(sprintf("reference: gcamdata driver() exit %s in %.1f min, peak memory %.1f GB\n", p$get_exit_status(),
              as.numeric(difftime(Sys.time(), t0, units = "mins")), peak / 2^30))
  if (!file.exists(file.path(ref, "data_map.rds"))) { cat("FAIL reference not made; see", file.path(ref, "driver.log"), "\n"); quit(status = 1) }
}
rmap <- readRDS(file.path(ref, "data_map.rds")); rf <- list.files(file.path(ref, "outputs"), "[.]csv$")
cat(sprintf("reference: %d CSV files, %.2f GB; data map %d rows\n", length(rf), sum(file.size(file.path(ref, "outputs", rf))) / 2^30, nrow(rmap)))

if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
options(minaj.ask_secs = Inf); if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))
suppressMessages(open_minaj(tree)); mo <- file.path(scr, "minaj_outputs")
t1 <- Sys.time(); m1 <- capture.output(driver_minaj(write_xml = FALSE, write_outputs = TRUE, outdir = mo), type = "message")
cat(sprintf("driver_minaj(write_outputs = TRUE): %.1f min\n%s\n", as.numeric(difftime(Sys.time(), t1, units = "mins")), paste(grep("CSV files of", m1, value = TRUE), collapse = "\n")))
mf <- list.files(mo, "[.]csv$")
check(setequal(mf, rf), sprintf("the same CSV files as driver() (%d; missing %d: %s; extra %d: %s)", length(mf), length(setdiff(rf, mf)),
                                paste(head(setdiff(rf, mf), 3), collapse = ", "), length(setdiff(mf, rf)), paste(head(setdiff(mf, rf), 3), collapse = ", ")))
both <- intersect(mf, rf); same <- tools::md5sum(file.path(mo, both)) == tools::md5sum(file.path(ref, "outputs", both))
check(all(same), sprintf("every CSV file byte-identical to driver()'s (%d of %d; first differing: %s)", sum(same), length(both), paste(head(both[!same], 3), collapse = ", ")))
m2 <- capture.output(driver_minaj(write_xml = FALSE, write_outputs = TRUE, outdir = mo), type = "message")
check(any(grepl("\\| written 0,", m2)), sprintf("a second call writes no file (%s)", sub("^.*\\| ", "", grep("CSV files of", m2, value = TRUE)[1])))
t2 <- Sys.time(); mmap <- suppressMessages(driver_minaj(return_data_map_only = TRUE))
cat(sprintf("driver_minaj(return_data_map_only = TRUE): %.1f min\n", as.numeric(difftime(Sys.time(), t2, units = "mins"))))
rmap <- rmap[order(rmap$name, rmap$output, method = "radix"), ]
cols <- c("name", "output", "precursors", "title", "units", "comments", "flags")
ok <- identical(dim(mmap), dim(rmap)) && identical(names(mmap), names(rmap)) && all(vapply(cols, function(k) identical(mmap[[k]], rmap[[k]]), NA))
bad <- if (identical(dim(mmap), dim(rmap))) cols[!vapply(cols, function(k) identical(mmap[[k]], rmap[[k]]), NA)] else "rows"
check(ok, sprintf("data map identical to driver()'s (%d rows here, %d there; differing: %s)", nrow(mmap), nrow(rmap), if (ok) "none" else paste(bad, collapse = ", ")))
cat(sprintf("RESULT outputs: PASS %d FAIL %d\n", n_pass, n_fail))
quit(status = if (n_fail) 1L else 0L)
