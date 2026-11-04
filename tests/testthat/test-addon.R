test_that("addon_minaj writes an add-on that turns the base file into the override", {
  skip_if_no_java(); open_mini()
  ov <- list(L200.StubTechCoef = function(d) { d$coefficient[d$stub.technology == "heat pump"] <- 0.5; d },
             "mini/A100.base" = function(d) { d$`2015`[d$region == "South"] <- 9; d })
  r <- suppressMessages(addon_minaj("mini.xml", ov))
  expect_identical(r$status, "written")
  dest <- info_minaj()$dest
  b <- file.path(dest, "mini.xml"); o <- file.path(dest, "mini_override.xml")
  expect_true(minaj:::addon_sim(b, r$file, o)$ok)
  expect_lt(file.size(r$file), file.size(o))
  wrong <- sub("<coefficient>0.5</coefficient>", "<coefficient>0.6</coefficient>", readLines(r$file), fixed = TRUE)
  expect_false(identical(wrong, readLines(r$file)))
  writeLines(wrong, w <- file.path(tempdir(), "mini_wrong_addon.xml"))
  expect_false(minaj:::addon_sim(b, w, o)$ok)
  s1 <- suppressMessages(addon_minaj("mini.xml", ov))$status
  s2 <- suppressMessages(addon_minaj("mini.xml", list(L200.StubTechCoef = function(d) d)))$status
  expect_identical(c(s1, s2), c("unchanged", "no change"))
})

test_that("addon_minaj stops under the memory floor before any chunk runs", {
  open_mini()
  op <- options(minaj.ram_floor_gb = 1e6)
  e <- tryCatch(suppressMessages(addon_minaj("mini.xml", list("mini/A100.base" = 2))), error = conditionMessage)
  options(op)
  expect_match(e, "3 chunk\\(s\\) to run, but only .* under the floor")
  expect_setequal(outdated(), all_chunks)
})
