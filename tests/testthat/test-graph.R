# Calls with side effects run before the expectation: expect_match() evaluates its argument twice.

test_that("open_minaj reads the chunks, CSV inputs and XML files of the tree", {
  root <- open_mini()
  i <- info_minaj()
  expect_identical(i[c("chunks", "xml", "csv_inputs")], list(chunks = 5L, xml = 2L, csv_inputs = 4L))
  expect_identical(i$gcamdata, root)
  expect_identical(i$dest, file.path(root, "xml_minaj"))
  m <- capture_messages(open_minaj(i$tree, store = i$store))
  expect_false(any(grepl("loading gcamdata", m)))
})

test_that("downstream_minaj follows the graph from CSV inputs, data and chunks", {
  open_mini()
  d <- downstream_minaj("mini/A201.other")
  expect_setequal(d$chunks, c("module_mini_L201.other", "module_mini_other_xml"))
  expect_identical(d$xml, "other.xml")
  expect_setequal(downstream_minaj("L100.base")$chunks, c("module_mini_L200.coef", "module_mini_mini_xml"))
  expect_identical(downstream_minaj("module_mini_L100.base")$xml, "mini.xml")
})

test_that("plan_minaj and why_minaj on an empty store; unknown names are refused", {
  open_mini()
  p <- suppressMessages(plan_minaj("mini.xml"))
  expect_setequal(p$chunks, c("module_mini_L100.base", "module_mini_L200.coef", "module_mini_mini_xml"))
  expect_setequal(p$reads, c("mini/A100.base", "mini/A100.extra", "mini/A200.coef"))
  w <- suppressMessages(why_minaj("mini_L100.base"))
  expect_identical(w, "an input would change")
  expect_error(why_minaj("nothing"), "not found")
  expect_error(make_minaj("nothing.xml"), "not an XML file, data object, CSV input or chunk")
})

test_that("a chunk file added to R/ joins the graph, and leaves it when removed", {
  root <- open_mini()
  f <- file.path(root, "R", "zmini_L300.more.R")
  writeLines(c('module_mini_L300.more <- function(command, ...) {',
               '  if(command == driver.DECLARE_INPUTS) return(c("L100.base"))',
               '  if(command == driver.DECLARE_OUTPUTS) return(c("L300.more"))',
               '  L300.more <- add_precursors(get_data(list(...)[[1]], "L100.base"), "L100.base")',
               '  return_data(L300.more)',
               '}'), f)
  m <- capture_messages(r <- ran("L300.more"))
  expect_match(m, "new names in R/ \\(module_mini_L300.more\\): reloading gcamdata", all = FALSE)
  expect_setequal(r, c("module_mini_L100.base", "module_mini_L300.more"))
  expect_identical(info_minaj()$chunks, 6L)
  expect_true("module_mini_L300.more" %in% downstream_minaj("mini/A100.base")$chunks)
  unlink(f)
  m <- capture_messages(plan_minaj("mini.xml"))
  expect_match(m, "files added or removed in R/: reloading gcamdata", all = FALSE)
  expect_identical(info_minaj()$chunks, 5L)
})
