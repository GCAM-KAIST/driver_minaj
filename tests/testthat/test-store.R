test_that("each chunk result and CSV read is recorded in the store", {
  open_mini(); st <- info_minaj()$store
  ran("mini.xml")
  res <- lapply(list.files(file.path(st, "rec", "res"), full.names = TRUE, recursive = TRUE), readRDS)
  expect_setequal(vapply(res, `[[`, "", "chunk"), c("module_mini_L100.base", "module_mini_L200.coef", "module_mini_mini_xml"))
  r <- Filter(function(x) x$chunk == "module_mini_L200.coef", res)[[1]]
  expect_named(r$inputs, c("L100.base", "mini/A200.coef"))
  expect_true(all(file.exists(minaj:::obj_path(r$out))))
  expect_true("mini_round" %in% names(r$reach))
  csv <- lapply(list.files(file.path(st, "rec", "csv"), full.names = TRUE, recursive = TRUE), readRDS)
  expect_setequal(vapply(csv, `[[`, "", "name"), c("mini/A100.base", "mini/A100.extra", "mini/A200.coef"))
  expect_identical(Filter(function(x) x$name == "mini/A100.extra", csv)[[1]]$file_hash, "missing")
})

test_that("a damaged stored value is removed, and the next call computes it again", {
  open_mini(); st <- info_minaj()$store
  base <- get_minaj("L100.base")
  res <- lapply(list.files(file.path(st, "rec", "res"), full.names = TRUE, recursive = TRUE), readRDS)
  p <- minaj:::obj_path(Filter(function(x) x$chunk == "module_mini_L100.base", res)[[1]]$out[["L100.base"]])
  writeBin(as.raw(1:50), p)
  expect_error(get_minaj("L100.base"), "unreadable and has been removed")
  expect_false(file.exists(p))
  expect_identical(get_minaj("L100.base"), base)
  expect_identical(ran("L100.base"), character(0))
})

test_that("gcamdata's output check: warnings pass through; a failure is recorded and reported on reuse", {
  open_mini()
  edit_mini("R/zmini_L100.base.R", function(x) x[!grepl("add_units(L100.base", x, fixed = TRUE)])
  w <- capture_warnings(ran("L100.base"))
  expect_identical(w, "minaj: module_mini_L100.base: No 'units' attached to L100.base - chunk module_mini_L100.base")
  edit_mini("R/zmini_L100.base.R", function(x) sub('add_precursors(L100.base, "mini/A100.base", "mini/A100.extra")',
                                                   'add_precursors(L100.base, "mini/A100.base", "mini/A100.extra", "L999.none")', x, fixed = TRUE))
  w <- capture_warnings(r <- suppressMessages(ran("L100.base")))
  expect_identical(r, "module_mini_L100.base")
  expect_match(w, "^minaj: output check for module_mini_L100.base failed: Some precursors for 'L100.base' aren't inputs", all = FALSE)
  w <- capture_warnings(r <- ran("L100.base"))
  expect_match(w, "reused result of module_mini_L100.base failed gcamdata's output check", all = FALSE)
  expect_length(r, 0)
})
