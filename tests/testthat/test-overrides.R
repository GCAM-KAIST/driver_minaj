test_that("overrides: a number, a function, a table or a CSV file, for this call only", {
  open_mini()
  base <- get_minaj("L100.base")
  expect_identical(base$value[base$region == "North" & base$sector == "heat"], c(20, 22, 24))
  expect_identical(get_minaj("L100.base", override = list("mini/A100.base" = 0.5))$value, base$value * 0.5)
  north <- get_minaj("L100.base", override = list("mini/A100.base" = function(d) d[d$region == "North", ]))
  expect_identical(unique(north$region), "North")
  tab <- base; tab$value <- 1
  expect_identical(get_minaj("L100.base", override = list(L100.base = tab))$value, rep(1, nrow(base)))
  expect_identical(get_minaj("L100.base", override = list(L100.base = 3))$value, base$value * 3)
  csv <- file.path(tempdir(), "A100_other.csv")
  writeLines(c("# Column types: ccnnn", "region,sector,2015,2020,2025", "East,heat,1,2,3"), csv)
  e <- get_minaj("L100.base", override = list("mini/A100.base" = csv))
  expect_identical(e$region, rep("East", 3)); expect_identical(e$value, c(2, 4, 6))
  expect_identical(get_minaj("L100.base"), base)
  expect_length(ran("L100.base"), 0)
})

test_that("an override reaches only the chunks downstream of it", {
  open_mini(); ran("mini.xml", "other.xml")
  p <- suppressMessages(plan_minaj("mini.xml", "other.xml", override = list("mini/A100.base" = 2)))
  expect_setequal(p$chunks, c("module_mini_L100.base", "module_mini_L200.coef", "module_mini_mini_xml"))
  expect_error(get_minaj("L200.StubTechCoef", override = list("mini/A200.coef" = 2)), "no year or 'value' column")
})

test_that("variant tables: scale, add and set, with filters and columns", {
  open_mini()
  base <- get_minaj("L100.base")
  tab <- data.frame(variant = c("low", "high", "high"), input = "mini/A100.base", op = c("scale", "set", "add"),
                    value = c("0.5", "7", "1"), region = c("*", "South", "North"), sector = c("", "heat", "power"),
                    column = c("", "2020", "2015;2025"), note = "")
  v <- minaj:::variants_from_table(tab)
  expect_identical(get_minaj("L100.base", override = v$low)$value, base$value * 0.5)
  h <- get_minaj("L100.base", override = v$high)
  i <- h$value != base$value
  expect_identical(paste(h$region, h$sector, h$year)[i], c("North power 2015", "South heat 2020", "North power 2025"))
  expect_identical(h$value[i], c(42, 14, 46))
  bad <- minaj:::variants_from_table(data.frame(variant = "x", input = "mini/A100.base", op = "scale", value = "2", region = "West"))
  expect_error(get_minaj("L100.base", override = bad$x), "no rows match region=West")
  expect_error(minaj:::variants_from_table(data.frame(variant = "x", input = "mini/A100.base")), "variant table needs columns")
})
