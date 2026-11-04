# Calls with side effects run before the expectation: expect_match() evaluates its argument twice.

make_status <- function(...) suppressMessages(make_minaj(...))$status

test_that("make_minaj converts an XML file once, then finds it unchanged or copies it from the store", {
  skip_if_no_java(); open_mini()
  r <- suppressMessages(make_minaj("mini.xml"))
  expect_identical(r$status, "converted")
  x <- readLines(r$file)
  expect_true(any(grepl("<coefficient>0.38</coefficient>", x, fixed = TRUE)))
  s1 <- make_status("mini.xml"); unlink(r$file); s2 <- make_status("mini.xml")
  expect_identical(c(s1, s2), c("unchanged", "copied from store"))
  expect_identical(readLines(r$file), x)
  m <- utils::read.csv(file.path(dirname(r$file), "minaj_manifest.csv"))
  expect_identical(m$how, c("converted", "copied from store"))
  expect_identical(m$md5[2], unname(tools::md5sum(r$file)))
})

test_that("an override is written as <stem>_override.xml and leaves the base file alone", {
  skip_if_no_java(); open_mini()
  b <- suppressMessages(make_minaj("mini.xml"))$file; md5 <- tools::md5sum(b)
  o <- suppressMessages(make_minaj("mini.xml", override = list("mini/A200.coef" = function(d) { d$coefficient <- d$coefficient * 2; d })))
  expect_identical(basename(o$file), "mini_override.xml")
  expect_true(any(grepl("<coefficient>0.76</coefficient>", readLines(o$file), fixed = TRUE)))
  expect_identical(tools::md5sum(b), md5)
})

test_that("status_minaj and promote_minaj", {
  skip_if_no_java(); root <- open_mini()
  suppressMessages(make_minaj("mini.xml"))
  s <- suppressMessages(status_minaj())
  expect_identical(c(s$results, s$file), c("current", "stale", "current", "unknown"))
  dir.create(file.path(root, "xml"))
  p <- suppressMessages(promote_minaj(c("mini.xml", "other.xml")))
  expect_identical(p, c(mini.xml = TRUE, other.xml = FALSE))
  good <- tools::md5sum(file.path(info_minaj()$dest, "mini.xml"))[[1]]
  expect_identical(tools::md5sum(file.path(root, "xml", "mini.xml"))[[1]], good)
  writeLines("<scenario/>", file.path(info_minaj()$dest, "mini.xml"))
  p <- suppressMessages(promote_minaj("mini.xml"))
  expect_identical(p, c(mini.xml = FALSE))
  expect_identical(tools::md5sum(file.path(root, "xml", "mini.xml"))[[1]], good)
  suppressMessages(make_minaj("mini.xml"))
  edit_mini("inst/extdata/mini/A200.coef.csv", function(x) sub(",2.6$", ",2.7", x))
  s <- suppressMessages(status_minaj(files = "mini.xml"))
  expect_identical(c(s$results, s$file, as.character(s$chunks_to_run)), c("stale", "unknown", "2"))
  p <- suppressMessages(promote_minaj("mini.xml"))
  expect_identical(p, c(mini.xml = FALSE))
})

test_that("a changed XML header file converts again; the memory floor stops a conversion and leaves no record", {
  skip_if_no_java(); open_mini()
  suppressMessages(make_minaj("mini.xml"))
  edit_mini("inst/extdata/mi_headers/ModelInterface_headers.txt", function(x) c(x, "##, one more comment line"))
  op <- options(minaj.ram_floor_gb = 1e6)
  e <- tryCatch(suppressMessages(make_minaj("mini.xml")), error = conditionMessage)
  options(op)
  expect_match(e, "mini.xml not written: only .* under the floor of 1e\\+06 GB")
  expect_identical(make_status("mini.xml"), "converted")
})

test_that("driver_minaj writes the same files one at a time and with background writers", {
  skip_if_no_java()
  build <- function(workers, floor = getOption("minaj.ram_floor_gb")) {
    open_mini(); op <- options(minaj.xml_workers = workers, minaj.ram_floor_gb = floor); on.exit(options(op))
    m <- capture_messages(driver_minaj())
    list(msgs = m, md5 = tools::md5sum(file.path(info_minaj()$dest, c("mini.xml", "other.xml"))))
  }
  one <- build(1L); two <- build(2L)
  expect_match(one$msgs, "converted 2, copied 0, unchanged 0, failed 0", all = FALSE)
  expect_false(any(grepl("XML files being written", one$msgs)))
  expect_match(two$msgs, "converted 2, copied 0, unchanged 0, failed 0", all = FALSE)
  expect_match(two$msgs, "waiting for 2 XML files being written", all = FALSE)
  expect_false(any(grepl("written one at a time", two$msgs)))
  expect_identical(unname(one$md5), unname(two$md5))
  m <- capture_messages(driver_minaj())
  expect_match(m, "converted 0, copied 0, unchanged 2, failed 0", all = FALSE)
  low <- build(2L, floor = 1e6)
  expect_match(low$msgs, "converted 0, copied 0, unchanged 0, failed 2", all = FALSE)
  m <- capture_messages(driver_minaj())
  expect_match(m, "converted 2, copied 0, unchanged 0, failed 0", all = FALSE)
})

test_that("background writers are used only when the tree's writer is gcamdata's own up to its Java heap", {
  skip_if_no_java()
  build <- function(edit) {
    open_mini(); op <- options(minaj.xml_workers = 2L); on.exit(options(op))
    edit_mini("R/xml.R", edit)
    m <- capture_messages(driver_minaj())
    list(msgs = m, xml = readLines(file.path(info_minaj()$dest, "mini.xml")))
  }
  # a step before the text loop that changes the tables: the tree's own writer, one file at a time
  pre <- build(function(x) sub("^(\\s*)tmpfn <- tempfile\\(\\)$", "\\1dot$data_tables <- dot$data_tables[1]\n\\1tmpfn <- tempfile()", x))
  expect_match(pre$msgs, "written one at a time: this tree's XML writer differs", all = FALSE)
  expect_match(pre$msgs, "converted 2, copied 0, unchanged 0, failed 0", all = FALSE)
  expect_false(any(grepl("<coefficient>", pre$xml, fixed = TRUE)))
  # a check after Java, as some trees have: background writers
  chk <- build(function(x) sub("^(\\s*)unlink\\(tmpfn\\)$", "\\1unlink(tmpfn)\n\\1if(!file.exists(dot$xml_file)) stop(\"no file: \", basename(dot$xml_file))", x))
  expect_false(any(grepl("written one at a time", chk$msgs)))
  expect_match(chk$msgs, "waiting for 2 XML files being written", all = FALSE)
  # a step after Java that changes the file: the tree's own writer
  post <- build(function(x) sub("^(\\s*)unlink\\(tmpfn\\)$", "\\1unlink(tmpfn)\n\\1write(\"<!-- x -->\", dot$xml_file, append = TRUE)", x))
  expect_match(post$msgs, "written one at a time", all = FALSE)
  expect_identical(utils::tail(post$xml, 1), "<!-- x -->")
})

test_that("variants_minaj writes one file per variant, from a list or a table", {
  skip_if_no_java(); open_mini(); ran("mini.xml")
  v <- suppressMessages(variants_minaj("mini.xml", list(same = list("mini/A100.base" = 1), high = list("mini/A200.coef" = function(d) { d$coefficient <- d$coefficient * 2; d }))))
  expect_identical(basename(v$file), c("mini_same.xml", "mini_high.xml"))
  expect_identical(v$identical_to_base, c(TRUE, FALSE))
  expect_true(any(grepl("<coefficient>0.76</coefficient>", readLines(v$file[2]), fixed = TRUE)))
  tab <- data.frame(variant = "south", input = "mini/A201.other", op = "set", value = "2", region = "South", column = "share.weight")
  t <- suppressMessages(variants_minaj("other.xml", tab))
  expect_true(any(grepl('<share-weight fillout="1" year="2015">2</share-weight>', readLines(t$file), fixed = TRUE)))
  expect_error(variants_minaj("other.xml", list(x = list("mini/A200.coef" = 2))), "none of its inputs is upstream of other.xml")
})
