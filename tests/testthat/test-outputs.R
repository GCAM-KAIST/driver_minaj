# driver_minaj(write_outputs = TRUE) and driver_minaj(return_data_map_only = TRUE). Calls with side effects run before the
# expectation: expect_match() evaluates its argument twice.

tables <- c("L100.base", "L200.Subsector", "L200.SubsectorShrwtFllt", "L200.StubTechCoef", "L201.SubsectorShrwtFllt")   # L201.Subsector: FLAG_NO_OUTPUT

# What the tree's own save_chunkdata() writes for these tables, as md5 by file name.
direct_md5 <- function(names) {
  d <- tempfile("direct_"); dir.create(d)
  for (n in names) get("save_chunkdata", asNamespace("gcamdata"))(setNames(list(get_minaj(n)), n), outputs_dir = d)
  tools::md5sum(file.path(d, paste0(names, ".csv")))
}

test_that("write_outputs writes each table as the tree's save_chunkdata does, and only once", {
  root <- open_mini(); out <- file.path(root, "outputs")
  m <- capture_messages(driver_minaj(write_xml = FALSE, write_outputs = TRUE))
  expect_match(m, "CSV files of 6 tables .* written 5, unchanged 0, not written by save_chunkdata \\(input data or FLAG_NO_OUTPUT\\) 1", all = FALSE)
  expect_setequal(list.files(out, all.files = TRUE, no.. = TRUE), paste0(tables, ".csv"))
  expect_identical(unname(tools::md5sum(file.path(out, paste0(tables, ".csv")))), unname(direct_md5(tables)))
  expect_false(file.exists(file.path(info_minaj()$dest, "mini.xml")))
  m <- capture_messages(driver_minaj(write_xml = FALSE, write_outputs = TRUE))
  expect_match(m, "written 0, unchanged 5, not written by save_chunkdata \\(input data or FLAG_NO_OUTPUT\\) 1", all = FALSE)
  unlink(file.path(out, "L100.base.csv")); write("x", file.path(out, "L200.Subsector.csv"), append = TRUE)
  edit_mini("inst/extdata/mini/A201.other.csv", function(x) sub("^South,cooling,electricity,1$", "South,cooling,electricity,2", x))
  m <- capture_messages(driver_minaj(write_xml = FALSE, write_outputs = TRUE))
  expect_match(m, "written 3, unchanged 2, not written by save_chunkdata \\(input data or FLAG_NO_OUTPUT\\) 1", all = FALSE)
  expect_identical(unname(tools::md5sum(file.path(out, paste0(tables, ".csv")))), unname(direct_md5(tables)))
})

test_that("write_outputs removes the file of a table save_chunkdata does not write, and leaves another call's folder alone", {
  root <- open_mini(); out <- file.path(root, "outputs"); dir.create(out)
  stale <- file.path(out, "L201.Subsector.csv"); other <- file.path(out, ".minaj_part1a2b", "L100.base.csv")
  writeLines("a,b", stale); dir.create(dirname(other)); writeLines("a,b", other)
  m <- capture_messages(driver_minaj(write_xml = FALSE, write_outputs = TRUE))
  expect_false(file.exists(stale))
  expect_true(file.exists(other))
  writeLines("a,b", stale)
  m <- capture_messages(driver_minaj(write_xml = FALSE, write_outputs = TRUE))   # from the table's record this time
  expect_match(m, "written 0, unchanged 5, not written by save_chunkdata \\(input data or FLAG_NO_OUTPUT\\) 1", all = FALSE)
  expect_false(file.exists(stale))
  expect_setequal(list.files(out, all.files = TRUE, no.. = TRUE), c(paste0(tables, ".csv"), ".minaj_part1a2b"))
})

test_that("write_outputs: other CSV files are listed, not removed; stop_after and outdir set the scope and the folder", {
  root <- open_mini(); out <- file.path(root, "outputs"); dir.create(out)
  writeLines("a,b", file.path(out, "L999.old.csv"))
  m <- capture_messages(driver_minaj(write_xml = FALSE, write_outputs = TRUE))
  expect_match(m, "1 other CSV files in .* are not tables of this tree: L999.old.csv", all = FALSE)
  expect_true(file.exists(file.path(out, "L999.old.csv")))
  od <- tempfile("out_"); dir.create(od); owd <- setwd(od)
  m <- capture_messages(driver_minaj(stop_after = "mini_L100.base", write_xml = FALSE, write_outputs = TRUE, outdir = "sub"))
  setwd(owd)
  expect_identical(list.files(file.path(od, "sub"), all.files = TRUE, no.. = TRUE), "L100.base.csv")
  expect_false(any(grepl("other CSV files", m)))
})

test_that("return_data_map_only returns the tree's tibbelize_outputs for inputs and outputs, ordered by name and output", {
  open_mini()
  m <- suppressMessages(driver_minaj(return_data_map_only = TRUE))
  expect_named(m, c("name", "output", "precursors", "title", "units", "comments", "flags"))
  expect_identical(nrow(m), 12L)
  expect_identical(m$output[m$name == "INPUT"], c("mini/A100.base", "mini/A100.extra", "mini/A200.coef", "mini/A201.other"))
  expect_identical(m$name, sort(m$name, method = "radix"))
  tb <- get("tibbelize_outputs", asNamespace("gcamdata"))
  ex <- rbind(tb(readd("L200.StubTechCoef", character_only = TRUE), "module_mini_L200.coef"), tb(readd("mini/A200.coef", character_only = TRUE), "INPUT"))
  expect_identical(as.data.frame(m[m$output %in% ex$output, ]), as.data.frame(ex[order(ex$name, ex$output, method = "radix"), ]), ignore_attr = TRUE)
  expect_identical(m$precursors[m$output == "mini.xml"], "L200.Subsector; L200.SubsectorShrwtFllt; L200.StubTechCoef")
  expect_identical(m$flags[m$output == "L201.Subsector"], "FLAG_NO_OUTPUT")
  expect_false(file.exists(file.path(info_minaj()$dest, "mini.xml")))
  s <- suppressMessages(driver_minaj(stop_after = "mini_L100.base", return_data_map_only = TRUE))
  expect_identical(paste(s$name, s$output), c("INPUT mini/A100.base", "INPUT mini/A100.extra", "module_mini_L100.base L100.base"))
  b <- suppressMessages(driver_minaj(stop_before = "mini_L100.base", return_data_map_only = TRUE))   # its inputs are CSV files only
  expect_named(b, c("name", "output", "precursors", "title", "units", "comments", "flags"))
  expect_identical(paste(b$name, b$output), c("INPUT mini/A100.base", "INPUT mini/A100.extra"))
  x <- suppressMessages(driver_minaj(stop_before = "mini_mini_xml", return_data_map_only = TRUE))
  expect_identical(unique(x$name), c("INPUT", "module_mini_L100.base", "module_mini_L200.coef"))
})
