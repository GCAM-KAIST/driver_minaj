# Calls with side effects run before the expectation: expect_match() evaluates its argument twice.

test_that("a second call reuses every stored result", {
  open_mini()
  expect_setequal(ran("mini.xml", "other.xml"), all_chunks)
  expect_length(ran("mini.xml", "other.xml"), 0)
  p <- suppressMessages(plan_minaj("mini.xml", "other.xml"))
  expect_length(p$chunks, 0)
  expect_length(outdated(), 0)
})

test_that("an edited CSV input re-runs only the chunks downstream of it", {
  open_mini(); ran("mini.xml", "other.xml")
  edit_mini("inst/extdata/mini/A200.coef.csv", function(x) sub("^heat,electricity,heat pump,elect_td_bld,0.4$", "heat,electricity,heat pump,elect_td_bld,0.5", x))
  w1 <- suppressMessages(why_minaj("mini_L200.coef"))
  invisible(get_minaj("mini/A200.coef"))
  w2 <- suppressMessages(why_minaj("mini_L200.coef"))
  expect_identical(c(w1, w2), c("an input would change", "inputs changed: mini/A200.coef"))
  expect_setequal(ran("mini.xml", "other.xml"), c("module_mini_L200.coef", "module_mini_mini_xml"))
  d <- get_minaj("L200.StubTechCoef")
  expect_identical(d$coefficient[d$stub.technology == "heat pump"], c(0.475, 0.475, 0.45, 0.45))
})

test_that("a CSV edit that leaves the table the same runs nothing (early cutoff)", {
  open_mini(); ran("mini.xml")
  edit_mini("inst/extdata/mini/A100.base.csv", function(x) sub("^# Source: .*$", "# Source: made up for the minaj tests, second version", x))
  expect_length(ran("mini.xml"), 0)
})

test_that("chunk code: comments change nothing; a change with the same outputs stops there (early cutoff)", {
  open_mini(); ran("mini.xml")
  edit_mini("R/zmini_L100.base.R", function(x) sub("^(    yrs <- .*)$", "\\1   # a comment", x))
  m <- capture_messages(r <- ran("mini.xml"))
  expect_match(m, "re-sourced R/zmini_L100.base.R", all = FALSE)
  expect_length(r, 0)
  edit_mini("R/zmini_L100.base.R", function(x) sub("\\* mini.SCALE)$", "* mini.SCALE * 1)", x))
  w <- suppressMessages(why_minaj("mini_L100.base"))
  expect_identical(w, "code changed: module_mini_L100.base")
  expect_identical(suppressMessages(ran("mini.xml")), "module_mini_L100.base")
})

test_that("an edited helper function or constant re-runs the chunks that use it", {
  open_mini(); ran("mini.xml", "other.xml")
  edit_mini("R/utils.R", function(x) sub("round(x, 3)", "round(x, 2)", x, fixed = TRUE))
  w <- suppressMessages(why_minaj("mini_L200.coef"))
  expect_identical(w, "code changed: mini_round")
  expect_setequal(suppressMessages(ran("mini.xml", "other.xml")), c("module_mini_L200.coef", "module_mini_mini_xml"))
  edit_mini("R/constants.R", function(x) sub("^(mini.SCALE +<- )2", "\\13", x))
  w <- suppressMessages(why_minaj("mini_L100.base"))
  expect_identical(w, "code changed: mini.SCALE")
  # every value scaled alike: the shares in L200 stay the same, so mini.xml is not made again
  expect_setequal(suppressMessages(ran("mini.xml", "other.xml")), c("module_mini_L100.base", "module_mini_L200.coef"))
  expect_identical(get_minaj("L100.base")$value[1:2], c(30, 60))
})

test_that("new names in R/ reload gcamdata; a NAMESPACE edit re-runs every chunk", {
  open_mini(); ran("mini.xml", "other.xml")
  edit_mini("R/utils.R", function(x) c(x, "mini_unused <- 1"))
  m <- capture_messages(r <- ran("mini.xml", "other.xml"))
  expect_match(m, "new names in R/ \\(mini_unused\\): reloading gcamdata", all = FALSE)
  expect_length(r, 0)
  edit_mini("NAMESPACE", function(x) c(x, "# edited"))
  m <- capture_messages(r <- ran("mini.xml", "other.xml"))
  expect_match(m, "changed NAMESPACE: reloading gcamdata", all = FALSE)
  expect_setequal(r, all_chunks)
})
