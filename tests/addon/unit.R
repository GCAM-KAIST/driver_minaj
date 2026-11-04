# Unit tests for addon_minaj and its parts: the tokenizer, the parse registry, addon_write and addon_sim. At the end the
# naive simulator of tests/addon/mutation.R runs on every base, add-on and override file written here.
# Usage (from the package root): Rscript tests/addon/unit.R [vanilla 9.1 gcamdata xml folder] [all [max MB]]
#   xml folder: default input/gcamdata/xml of the tree in environment variable MINAJ_TEST_TREE.
#   all: also round-trip every file in the folder up to max MB (default 130) and look up each of its (parent class, tag)
#   pairs in the registry
#   MINAJ_HOME: load minaj from this source folder instead of the installed package. Test files go to tempdir() (TMPDIR).
#   Files over 50 MB are read only while at least 4 GB of memory is free.
# The writer and both checkers read the same registry and the same XML reader (xml_tokens), so a wrong row passes
# every add-on test here. The registry section rebuilds it with the same builder (which catches drift only) and checks
# rows against facts read from the C++ code apart from the builder; only a GCAM equivalence run checks the rest.
args <- commandArgs(trailingOnly = TRUE)
if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
ns <- asNamespace("minaj")
out_dir <- tempdir()
n_pass <- 0L; n_fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
  cat(if (isTRUE(ok)) "PASS" else "FAIL", label, "\n")
}
refuses <- function(expr, pattern) { m <- tryCatch({ force(expr); "" }, error = conditionMessage); grepl(pattern, m, fixed = TRUE) }
mem_ok <- function() ps::ps_system_memory()$avail / 2^30 >= 4

# ---- xml_tokens --------------------------------------------------------------------------------------
xml_tokens <- get("xml_tokens", ns)
doc <- paste0('<?xml version="1.0" encoding="UTF-8"?>\r\n<!-- note > with <tags> && -->\r\n<scenario>\r\n  <world>\r\n',
              '    <region name="A&amp;B">\r\n      <a year="1975" fillout="1">3</a><b  delete = "1" /><c></c>\r\n',
              '      <d>x &amp;&& y > z</d>\r\n    </region>\r\n  </world>\r\n</scenario>\r\n')
x <- xml_tokens(doc)
check("synthetic: tags, depths and parents", identical(x$tag, c("scenario", "world", "region", "a", "b", "c", "d")) &&
        identical(x$depth, c(1L, 2L, 3L, 4L, 4L, 4L, 4L)) && identical(x$parent, c(0L, 1L, 2L, 3L, 3L, 3L, 3L)))
check("synthetic: leaf text raw, NA for containers, \"\" for <b/> and <c></c>", identical(x$text, c(NA, NA, NA, "3", "", "", "x &amp;&& y > z")))
check("synthetic: attributes as written, one column each", identical(names(x), c("tag", "depth", "parent", "first", "last", "close", "stag", "text", "@name", "@year", "@fillout", "@delete")) &&
        identical(x[["@name"]], c(NA, NA, "A&amp;B", NA, NA, NA, NA)) && identical(x[["@year"]], c(NA, NA, NA, "1975", NA, NA, NA)) &&
        identical(x[["@fillout"]][4], "1") && identical(x[["@delete"]], c(NA, NA, NA, NA, "1", NA, NA)))
sb <- function(a, z) rawToChar(charToRaw(doc)[a:z])
check("synthetic: byte spans (start tag, subtree, end tag)", all(mapply(function(a, s) sb(a, a + nchar(s) - 1L), x$first, x$stag) == x$stag) &&
        sb(x$first[3], x$last[3]) == '<region name="A&amp;B">\r\n      <a year="1975" fillout="1">3</a><b  delete = "1" /><c></c>\r\n      <d>x &amp;&& y > z</d>\r\n    </region>' &&
        is.na(x$close[5]) && sb(x$close[4], x$last[4]) == "</a>" && sb(x$close[1], x$last[1]) == "</scenario>")
check("synthetic: file, text and raw input give the same table", {
  f <- file.path(out_dir, "tok_synthetic.xml"); writeBin(charToRaw(doc), f)
  identical(xml_tokens(f), x) && identical(xml_tokens(charToRaw(doc)), x)
})
check("UTF-8 byte-order mark and a declaration-free fragment with two top-level elements", {
  y <- xml_tokens(c(rawToChar(as.raw(c(0xef, 0xbb, 0xbf))), "<a/>", "<b><c>1</c></b>"))
  identical(y$tag, c("a", "b", "c")) && identical(y$parent, c(0L, 0L, 2L))
})
check("non-ASCII bytes kept as written in values and text", {
  u8 <- as.raw(c(0x43, 0xc3, 0xb4, 0x74, 0x65)); raw_doc <- c(charToRaw('<a name="'), u8, charToRaw('"><b>'), u8, charToRaw("</b></a>"))
  y <- xml_tokens(raw_doc)
  identical(charToRaw(y[["@name"]][1]), u8) && identical(charToRaw(y$text[2]), u8) && identical(y$first, c(1L, 17L)) && identical(y$last, c(32L, 28L))
})
check("refuses: non-ASCII start tag with single quotes (message still built)", refuses(xml_tokens(c(charToRaw("<a name='"), as.raw(c(0xc3, 0xa9)), charToRaw("'/>"))), "single quotes in <a name="))
bad <- list(
  "CDATA section"                = c("<a><![CDATA[x]]></a>", "CDATA section"),
  "DOCTYPE"                      = c("<!DOCTYPE a><a/>", "DOCTYPE"),
  "processing instruction"       = c("<a><?pi x?></a>", "processing instruction"),
  "declaration not first"        = c("<a/><?xml version=\"1.0\"?>", "processing instruction"),
  "entity &nbsp; in attribute"   = c("<a name=\"x&nbsp;y\"/>", "entity other than"),
  "numeric entity in attribute"  = c("<a name=\"x&#38;y\"/>", "entity other than"),
  "raw & in attribute"           = c("<a name=\"x & y\"/>", "entity other than"),
  "single-quoted attribute"      = c("<a name='x'/>", "single quotes"),
  "unquoted attribute"           = c("<a name=x/>", "malformed start tag"),
  "'>' inside an attribute"      = c("<a name=\"x>y\"/>", "unterminated value"),
  "'<' inside an attribute"      = c("<a name=\"x<y\"/>", "malformed start tag"),
  "space after <"                = c("< a/>", "malformed start tag"),
  "attributes without a space"   = c("<a x=\"1\"y=\"2\"/>", "malformed start tag"),
  "repeated attribute"           = c("<a name=\"x\" name=\"y\"/>", "attribute name repeated"),
  "mismatched end tag"           = c("<a><b></a></b>", "</a> closes <b>"),
  "unclosed element"             = c("<a><b></b>", "<a> is never closed"),
  "stray end tag"                = c("<a/></b>", "</b> without a start tag"),
  "malformed end tag"            = c("<a></a x>", "malformed end tag"),
  "text beside child elements"   = c("<a>t<b/></a>", "text outside a leaf: t"),
  "text at the top level"        = c("<a/>junk", "text outside a leaf: junk"),
  "comment inside a leaf"        = c("<a>1<!-- c -->2</a>", "comment inside leaf <a>"),
  "unterminated comment"         = c("<a/><!-- x", "text outside a leaf"),
  "lone <"                       = c("<a>1 < 2</a>", "malformed start tag"),
  "no elements"                  = c("  <!-- only a comment -->  ", "no elements"))
for (k in names(bad)) check(paste("refuses:", k), refuses(xml_tokens(bad[[k]][1]), bad[[k]][2]))
check("refuses: NUL byte", refuses(xml_tokens(as.raw(c(0x3c, 0x61, 0x2f, 0x3e, 0x00))), "NUL byte"))
check("refusal names the line", refuses(xml_tokens("<a>\n<b>\n<c x='1'/></b></a>"), "XML text, line 3 (byte 9)"))

# Reassemble the file from the table: start tags (stag), leaf text (text), end tags rebuilt from tag names; what lies
# between must be whitespace, comments or the declaration.
round_trip <- function(f, x) {
  b <- readBin(f, "raw", file.size(f)); s <- rawToChar(b); Encoding(s) <- "bytes"
  nz <- !is.na(x$close); lf <- nz & !is.na(x$text)
  pos <- c(x$first, x$first[lf] + nchar(x$stag[lf], "bytes"), x$close[nz])
  str <- c(x$stag, x$text[lf], paste0("</", x$tag[nz], ">"))
  o <- order(pos); pos <- pos[o]; str <- str[o]; end <- pos + nchar(str, "bytes") - 1L
  gaps <- substring(s, c(1L, end + 1L), c(pos - 1L, length(b)))
  gaps[1] <- sub("^\xef\xbb\xbf", "", sub("<\\?xml[^>]*\\?>", "", gaps[1], useBytes = TRUE), useBytes = TRUE)
  gaps_ok <- !any(grepl("[^ \t\r\n]", gsub("(?s)<!--.*?-->", "", gaps, perl = TRUE, useBytes = TRUE), useBytes = TRUE))
  re <- paste(c(rbind(c(substring(s, 1L, pos[1] - 1L), gaps[-c(1L, length(gaps))]), str), gaps[length(gaps)]), collapse = "")
  spans_ok <- all(x$last[nz] == x$close[nz] + nchar(x$tag[nz]) + 2L) && all(x$last[!nz] == x$first[!nz] + nchar(x$stag[!nz], "bytes") - 1L)
  ch <- x$parent > 0L
  nest_ok <- all(x$first[ch] > x$first[x$parent[ch]] & x$last[ch] < x$close[x$parent[ch]])
  all(pos[-1] > end[-length(end)]) && gaps_ok && spans_ok && nest_ok && identical(charToRaw(re), b)
}

xml_dir <- if (length(args) && !identical(args[1], "all")) args[1] else if (nzchar(Sys.getenv("MINAJ_TEST_TREE"))) file.path(Sys.getenv("MINAJ_TEST_TREE"), "input", "gcamdata", "xml") else
  stop("usage: Rscript tests/addon/unit.R <vanilla 9.1 gcamdata xml folder> [all [max MB]], or set MINAJ_TEST_TREE", call. = FALSE)
files <- file.path(xml_dir, c("electricity.xml", "en_supply.xml", "gas_trade.xml", "land_input_3_IRR.xml", "all_energy_emissions.xml"))
solver <- file.path(dirname(dirname(xml_dir)), "solution", "cal_broyden_config.xml")   # hand-written, with comments and raw && in text
solver <- solver[file.exists(solver)][1]
if (!is.na(solver)) files <- c(files, solver)
timing <- data.frame(file = character(), MB = numeric(), elements = integer(), elapsed_s = numeric(), cpu_s = numeric(), MB_per_cpu_s = numeric())
rules <- utils::read.csv(system.file("extdata", "gcam_parse_rules.csv", package = "minaj"), colClasses = "character", na.strings = character(0))
rk <- paste(rules$parent_class, rules$tag, sep = "\r")
unknown_pairs <- function(x) {                                    # (parent class, tag) pairs of a token table that the registry lacks
  cl <- rep(NA_character_, length(x$tag)); u <- character(0)
  for (d in seq_len(max(x$depth))) {
    w <- which(x$depth == d); pc <- if (d == 1L) rep("", length(w)) else cl[x$parent[w]]
    ri <- match(paste(pc, x$tag[w], sep = "\r"), rk); lv <- d > 1L & pc %in% ""   # lv: child of a value
    u <- c(u, paste(pc, x$tag[w])[!is.na(pc) & !lv & is.na(ri)], paste("(value)", x$tag[w])[lv])
    cl[w] <- ifelse(is.na(ri) | lv, NA, rules$creates[ri])
  }
  unique(u)
}
approved_unknown <- character(0)                                  # pairs GCAM skips on purpose; empty until a reviewed pair is added
for (f in files[file.exists(files)]) {
  mb <- file.size(f) / 2^20
  if (mb > 50 && !mem_ok()) { cat("SKIP", basename(f), "(less than 4 GB free)\n"); next }
  el <- cpu <- c()
  for (r in if (mb > 50) 1 else 3) { x <- NULL; invisible(gc()); t0 <- proc.time(); x <- xml_tokens(f); t1 <- proc.time() - t0; el <- c(el, t1[[3]]); cpu <- c(cpu, t1[[1]] + t1[[2]]) }
  timing[nrow(timing) + 1L, ] <- list(basename(f), round(mb, 1), nrow(x), round(min(el), 2), round(min(cpu), 2), round(mb / max(min(cpu), 0.01), 1))
  check(sprintf("round trip: %s (%.1f MB, %d elements) reproduces the file bytes", basename(f), mb, nrow(x)), round_trip(f, x))
  if (!identical(f, solver)) { u <- setdiff(unknown_pairs(x), approved_unknown)
                               check(sprintf("registry: every (parent class, tag) pair of %s is in the registry (%d not: %s)", basename(f), length(u), paste(head(u, 5), collapse = "; ")), !length(u)) }
  if (mb < 50 && !identical(f, solver)) {
    d <- xml2::read_xml(f); nd <- xml2::xml_find_all(d, "//*"); pth <- xml2::xml_path(nd)
    par <- match(sub("/[^/]*$", "", pth), pth); par[is.na(par)] <- 0L
    lf <- !is.na(x$text) & !is.na(x$close)
    na <- grep("^@", names(x))
    check(sprintf("xml2: %s element count, tags, depths and parent links equal", basename(f)),
          length(nd) == nrow(x) && identical(xml2::xml_name(nd), x$tag) && identical(par, x$parent) &&
            identical(lengths(regmatches(pth, gregexpr("/", pth))), x$depth))
    check(sprintf("xml2: %s attributes and leaf text equal", basename(f)),
          sum(lengths(xml2::xml_attrs(nd))) == sum(!is.na(unlist(x[na]))) &&
            all(vapply(na, function(j) identical(xml2::xml_attr(nd, substring(names(x)[j], 2)), x[[j]]), logical(1))) &&
            identical(xml2::xml_text(nd[lf]), x$text[lf]))
    rm(d, nd)
  }
  rm(x); invisible(gc())
}
print(timing, row.names = FALSE)
if ("all" %in% args) {
  cap <- suppressWarnings(as.numeric(args[match("all", args) + 1L])); if (is.na(cap)) cap <- 130
  all_f <- list.files(xml_dir, "\\.xml$", full.names = TRUE); all_f <- all_f[file.size(all_f) <= cap * 2^20]
  ok <- 0L; t_all <- 0; mb_all <- 0; un <- list()
  for (f in all_f) {
    if (file.size(f) > 50 * 2^20 && !mem_ok()) { cat("SKIP", basename(f), "(less than 4 GB free)\n"); next }
    t0 <- proc.time(); x <- tryCatch(xml_tokens(f), error = function(e) conditionMessage(e)); t1 <- proc.time() - t0; t_all <- t_all + t1[[1]] + t1[[2]]
    good <- is.data.frame(x) && round_trip(f, x)
    if (good) { ok <- ok + 1L; mb_all <- mb_all + file.size(f) / 2^20; u <- unknown_pairs(x); if (length(u)) un[[basename(f)]] <- u } else cat("FAIL round trip:", basename(f), if (is.character(x)) x, "\n")
    rm(x); invisible(gc())
  }
  check(sprintf("round trip: %d of %d files up to %g MB in %s (%.0f MB, %.0f MB per CPU second tokenizing)", ok, length(all_f), cap, basename(xml_dir), mb_all, mb_all / t_all),
        length(all_f) > 0L && ok == length(all_f))
  ua <- unique(unlist(un, use.names = FALSE))
  check(sprintf("registry: every (parent class, tag) pair in %d files of %s is in the registry (%d not: %s)", ok, basename(xml_dir), length(setdiff(ua, approved_unknown)),
                paste(head(setdiff(ua, approved_unknown), 10), collapse = "; ")), ok > 0L && all(ua %in% approved_unknown))
  for (k in names(un)) cat("  ", k, ": ", paste(un[[k]], collapse = "; "), "\n", sep = "")
}

# ---- registry -----------------------------------------------------------------------------------------
# Rebuilt from the C++ tree (same parse code, so the same tree hash), it must equal the packaged CSV; the tree hash
# leaves out climate/source/hector/, build/, CR bytes and files without parse code, and changes with the parse code;
# addon_minaj refuses a tree whose parse code differs; every XMLParse override needs hand-written rows; and rows must
# match facts read from the C++ code apart from the builder (kind, key, order, creates, file and lines, tag names).
cpp <- file.path(dirname(dirname(dirname(xml_dir))), "cvs", "objects"); cpp <- cpp[dir.exists(cpp)][1]
if (is.na(cpp)) cat("SKIP registry rebuild (no cvs/objects folder)\n") else {
  gb <- get("gcam_rules_build", ns); wn <- character(0)
  R <- withCallingHandlers(gb(cpp), warning = function(w) { wn <<- c(wn, conditionMessage(w)); invokeRestart("muffleWarning") })
  check(sprintf("registry: tree hash of %s equals the CSV's (%s; rebuilt %s)", cpp, rules$tree_hash[1], R$tree_hash[1]),
        identical(R$tree_hash[1], rules$tree_hash[1]) && identical(gb(cpp, out = rules), rules$tree_hash[1]))
  rw <- function(x) sort(do.call(paste, c(lapply(x, as.character), sep = "\t")))
  check(sprintf("registry: rebuilt from %s, identical to inst/extdata/gcam_parse_rules.csv (%d rows)", cpp, nrow(R)), identical(names(R), names(rules)) && identical(rw(R), rw(rules)))
  if (!identical(rw(R), rw(rules))) cat("  only rebuilt:", head(setdiff(rw(R), rw(rules)), 3), "\n  only packaged:", head(setdiff(rw(rules), rw(R)), 3), "\n")
  check("registry: every XMLParse override has hand-written rows", !any(grepl("XMLParse overrides without hand-written rows", wn, fixed = TRUE)))
  if (length(wn)) cat(paste0("  gcam_rules_build: ", wn, "\n"), sep = "")
  # tree hash on a copy of the C++ files, and addon_minaj's check of it (run before the tree is opened)
  tc <- file.path(out_dir, "tree_copy"); oc <- file.path(tc, "cvs", "objects"); unlink(tc, recursive = TRUE)
  rel <- list.files(cpp, "\\.(h|hpp|cpp)$", recursive = TRUE); rel <- rel[!grepl("^(climate/source/hector|build)/", rel)]
  for (x in unique(dirname(rel))) dir.create(file.path(oc, x), recursive = TRUE, showWarnings = FALSE)
  if (all(file.copy(file.path(cpp, rel), file.path(oc, rel)))) {
    nm <- get(".nm", ns); t0 <- nm$tree; nm$tree <- normalizePath(tc, "/")
    am <- function() tryCatch({ get("addon_minaj", ns)("electricity.xml", list(x = 1)); "" }, error = conditionMessage)
    check("tree hash: a copy of the parse code gives the CSV's hash; addon_minaj passes its check (then needs an open tree)",
          identical(gb(oc, out = rules), rules$tree_hash[1]) && grepl("run open_minaj", am(), fixed = TRUE))
    nc <- "containers/source/national_account.cpp"; b0 <- readBin(file.path(oc, nc), "raw", file.size(file.path(oc, nc)))
    for (x in c("climate/source/hector/src/extra.cpp", "build/vc10/extra.cpp")) { dir.create(dirname(file.path(oc, x)), recursive = TRUE, showWarnings = FALSE); writeLines("bool A::XMLParse() { return true; }", file.path(oc, x)) }
    ap <- file.path(oc, "containers/source/analyze_str.cpp"); cat("\n// changed\n", file = ap, append = TRUE)
    writeBin(charToRaw(gsub("\n", "\r\n", rawToChar(b0[b0 != as.raw(13L)]), fixed = TRUE)), file.path(oc, nc))
    check("tree hash: unchanged by files under climate/source/hector/ and build/, a file without parse code, and CR line ends",
          !grepl("analyze_str", paste(rules$source, collapse = " "), fixed = TRUE) && identical(gb(oc, out = rules), rules$tree_hash[1]))
    a0 <- readBin(ap, "raw", file.size(ap)); cat("// XMLParse\n", file = ap, append = TRUE); h1 <- gb(oc, out = rules); writeBin(a0, ap)
    tp <- file.path(oc, "util/base/include/tech_vector_parse_helper.h"); t1 <- readBin(tp, "raw", file.size(tp)); cat("\n// changed\n", file = tp, append = TRUE); h2 <- gb(oc, out = rules); writeBin(t1, tp)
    check("tree hash: changed by parse code in a file no row cites (XMLParse added) and by the array parse helper (tech_vector_parse_helper.h)",
          !identical(h1, rules$tree_hash[1]) && !identical(h2, rules$tree_hash[1]) && identical(gb(oc, out = rules), rules$tree_hash[1]))
    lc <- Sys.getlocale("LC_COLLATE"); h3 <- vapply(c("C", if (.Platform$OS.type == "windows") "English_United States.1252" else "en_US.UTF-8"),
                                                  function(l) { suppressWarnings(Sys.setlocale("LC_COLLATE", l)); gb(oc, out = rules) }, ""); suppressWarnings(Sys.setlocale("LC_COLLATE", lc))
    check("tree hash: the same under the C and English collation orders", all(h3 == rules$tree_hash[1]))
    writeBin(c(b0, charToRaw("\n// changed\n")), file.path(oc, nc))
    m <- am()
    check("tree hash: a changed parse file is refused, naming the file to rebuild and the call, without a reinstall step",
          !identical(gb(oc, out = rules), rules$tree_hash[1]) && grepl("differs from the code", m, fixed = TRUE) &&
            grepl(sprintf('gcam_rules_build("%s", out = "%s")', normalizePath(oc, "/"), system.file("extdata", "gcam_parse_rules.csv", package = "minaj")), m, fixed = TRUE) &&
            !grepl("and reinstall minaj", m, fixed = TRUE))
    nm$tree <- t0
  } else cat("SKIP tree hash on a copy (copy failed)\n")
  unlink(tc, recursive = TRUE)
  # NationalAccount hand rows: tag names from the enum and names array of national_account.cpp
  na <- rules[rules$parent_class == "NationalAccount" & startsWith(rules$source, "containers/source/national_account.cpp:"), ]
  hd <- readLines(file.path(cpp, "containers/include/national_account.h"), warn = FALSE); sc <- readLines(file.path(cpp, "containers/source/national_account.cpp"), warn = FALSE)
  e0 <- grep("enum AccountType", hd)[1]; e1 <- e0 + grep("^\\s*END\\b", hd[-seq_len(e0)])[1]
  en <- trimws(sub(",.*$", "", sub("//.*$", "", hd[(e0 + 1L):(e1 - 1L)]))); en <- en[nzchar(en)]
  a0 <- grep("const static gcamstr names\\[\\]", sc)[1]; a1 <- a0 + grep("};", sc[-seq_len(a0)], fixed = TRUE)[1]
  ns0 <- gsub('"', "", unlist(regmatches(sc[a0:a1], gregexpr('"[^"]*"', sc[a0:a1]))))
  id <- sub("^.*enumToXMLName\\(\\s*(\\w+)\\s*\\).*$", "\\1", sc[as.integer(sub("^[^:]*:([0-9]+).*$", "\\1", na$source))])
  check(sprintf("registry: the %d NationalAccount rows name the tags of national_account.cpp's names array", nrow(na)),
        nrow(na) == 9L && length(en) == length(ns0) && identical(na$tag, ns0[match(id, en)]))
}
facts <- utils::read.csv(text = "parent_class,tag,kind,key_attribute,order_policy,file,from,to
Scenario,modeltime,ONCE,,none,containers/source/scenario.cpp,151,161
Scenario,calc-base-price,APPEND,,sequence,containers/source/scenario.cpp,194,202
RegionMiniCAM,PrimaryFuelCO2Coef,KEYMAP,name,keyed,containers/source/region_minicam.cpp,143,145
Subsector,interpolation-rule,RULE,,sequence,sectors/source/subsector.cpp,128,141
TechnologyContainer<DefaultTechnology>,period,PERIOD,year,keyed,technologies/source/technology_container.cpp,143,218
TechnologyContainer<DefaultTechnology>,interpolation-rule,RULE,,sequence,technologies/source/technology_container.cpp,229,244
StubTechnologyContainer,period,STUB_QUEUE,year,keyed,technologies/source/stub_technology_container.cpp,86,89
StubTechnologyContainer,interpolation-rule,STUB_QUEUE,,sequence,technologies/source/stub_technology_container.cpp,86,89
GlobalTechnologyDatabase,location-info,GTD_LOCATION,sector-name;subsector-name,keyed,technologies/source/global_technology_database.cpp,86,126
EnergyInput,efficiency,REPLACE,,none,functions/source/energy_input.cpp,150,159
EnergyInput,coefficient,REPLACE,,none,functions/source/energy_input.cpp,150,159
EnergyInput,flag,OR,,none,functions/source/energy_input.cpp,160,161
MACControl,mac-reduction,KEYMAP,tax,keyed,emissions/source/mac_control.cpp,132,141
MACControl,no-zero-cost-reductions,SETTRUE,,none,emissions/source/mac_control.cpp,132,141
FractionalSecondaryOutput,fraction-produced,KEYMAP,price,keyed,technologies/source/fractional_secondary_output.cpp,126,130
ResidueBiomassOutput,fract-harvested,KEYMAP,price,keyed,technologies/source/residue_biomass_output.cpp,360,364
LandUseHistory,allocation,KEYMAP,year,keyed,land_allocator/source/land_use_history.cpp,66,75
CalcBasePrice,sector-map,KEYMAP,from-name,keyed,containers/source/calc_base_price.cpp,82,90
SubResource,grade,VEC_NAMED,name,critical,,,
AgStorageTechnology,minicam-energy-input,VEC_NAMED,name,critical,,,
ITechnology,minicam-energy-input,VEC_NAMED,name,critical,,,
", colClasses = "character")
for (i in seq_len(nrow(facts))) {
  r <- rules[rules$parent_class == facts$parent_class[i] & rules$tag == facts$tag[i], ]
  sf <- sub(":.*$", "", sub(";.*$", "", r$source)); sl <- suppressWarnings(as.integer(sub("^[^:]*:([0-9]+).*$", "\\1", r$source)))
  check(sprintf("registry row %s/%s: %s, key %s, %s%s", facts$parent_class[i], facts$tag[i], facts$kind[i], if (nzchar(facts$key_attribute[i])) facts$key_attribute[i] else "none", facts$order_policy[i],
                if (nzchar(facts$file[i])) sprintf(", %s:%s-%s", facts$file[i], facts$from[i], facts$to[i]) else ""),
        nrow(r) == 1L && r$kind == facts$kind[i] && r$key_attribute == facts$key_attribute[i] && r$order_policy == facts$order_policy[i] &&
          (!nzchar(facts$file[i]) || (sf == facts$file[i] && !is.na(sl) && sl >= as.integer(facts$from[i]) && sl <= as.integer(facts$to[i]))))
}
cre <- utils::read.csv(text = "parent_class,tag,creates
SupplySector,subsector,Subsector
SupplySector,relative-cost-logit,RelativeCostLogit
Subsector,technology,TechnologyContainer<DefaultTechnology>
Subsector,food-storage-technology,TechnologyContainer<AgStorageTechnology>
TechnologyContainer<DefaultTechnology>,period,DefaultTechnology
StubTechnologyContainer,period,ITechnology
SubResource,grade,Grade
RegionMiniCAM,nationalAccountContainer,NationalAccountContainer
World,region,RegionMiniCAM
", colClasses = "character")
check(sprintf("registry: the class each of %d rows creates (sector, subsector, technology, period, grade, region)", nrow(cre)),
      identical(rules$creates[match(paste(cre$parent_class, cre$tag, sep = "\r"), rk)], cre$creates))
vc <- unique(rules$creates[rules$tag == "period" & startsWith(rules$parent_class, "TechnologyContainer<")])   # technology classes
fm <- rules[rules$parent_class == "ITechnology", ]
crit <- vapply(fm$tag, function(t) "critical" %in% rules$order_policy[rules$parent_class %in% vc & rules$tag == t], NA)
check(sprintf("registry: ITechnology rows (stub periods) are order-critical exactly where a technology class has the tag order-critical (%d rows)", sum(crit)),
      any(crit) && identical(unname(fm$order_policy == "critical"), unname(crit)))
check("registry: value-factor-calculator inside a stub period (ITechnology) is a known tag", "ITechnology\rvalue-factor-calculator" %in% rk)
check("registry: a field only a handler reads (NationalAccount savings-rate) is cited in a .cpp file, so attributes do not set it",
      grepl("\\.cpp:", rules$source[match("NationalAccount\rsavings-rate", rk)]))
check("registry: a text default given by a C++ identifier (HectorModel hector-ini-file = def_ini_file) is NA (not known)", identical(rules$default[match("HectorModel\rhector-ini-file", rk)], "NA"))
if (!is.na(cpp)) {                                                # text defaults: each a double-quoted literal given to the member in the constructor cited
  sd <- rules[rules$value_type %in% c("gcamstr", "std::string") & nzchar(rules$default) & rules$default != "NA" & grepl("; default ", rules$source), ]
  qf <- vapply(seq_len(nrow(sd)), function(i) {
    s <- strsplit(sd$source[i], "; default ", fixed = TRUE)[[1]]; sf <- sub(":[0-9]+$", "", s); sl <- as.integer(sub("^.*:", "", s))
    mb <- sub("^.*DEFINE_VARIABLE\\([^,]*,[^,]*,[ \t]*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", readLines(file.path(cpp, sf[1]), warn = FALSE)[sl[1]])   # the member, from the DEFINE_VARIABLE line
    ct <- readLines(file.path(cpp, sf[2]), warn = FALSE)[-seq_len(sl[2] - 1L)]; e <- match(TRUE, startsWith(ct, "}"))   # the constructor, to its closing brace
    !is.na(e) && grepl(paste0("\\b", mb, "[ \t\r\n]*[=(][ \t\r\n]*\"", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", sd$default[i]), "\""), paste(ct[seq_len(e)], collapse = "\n"), perl = TRUE)
  }, NA)
  check(sprintf("registry: every text default taken from a constructor (%d rows) is the double-quoted literal the constructor it cites gives the member%s", nrow(sd),
                if (all(qf)) "" else paste0(" (not: ", paste(sd$parent_class[!qf], sd$tag[!qf], sd$default[!qf], collapse = "; "), ")")), nrow(sd) > 0L && all(qf))
}

# ---- addon_write -------------------------------------------------------------------------------------
# Synthetic base (B) and override (O) files: each case checks the exact add-on text (after the header comment) or the
# refusal message. Then capacity-factor values x1.1 in a real electricity.xml.
local({
  addon_write <- get("addon_write", ns)
  d <- file.path(out_dir, "addon"); dir.create(d, showWarnings = FALSE); k <- 0L; last <- NULL
  aw <- function(B, O, ...) {                                     # add-on body, or "REFUSED: <message>"
    k <<- k + 1L; f <- file.path(d, paste0(c("b", "o", "a"), k, ".xml"))
    writeBin(charToRaw(paste(B, collapse = "\n")), f[1]); writeBin(charToRaw(paste(O, collapse = "\n")), f[2])
    op <- options(minaj.addon_max_ratio = Inf, ...); on.exit(options(op))
    last <<- tryCatch(addon_write(f[1], f[2], f[3], "energy/A23.globaltech_capacity_factor"), error = function(e) conditionMessage(e))
    if (is.character(last)) paste("REFUSED:", last) else paste(readLines(f[3])[-(1:2)], collapse = "\n")
  }
  i4 <- function(n, x) paste0(strrep("    ", n), x)
  doc <- function(world, top = NULL) c('<?xml version="1.0" encoding="UTF-8"?>', "<scenario>", i4(1, "<world>"), world, i4(1, "</world>"), top, "</scenario>", "")
  reg <- function(name, ...) c(i4(2, paste0('<region name="', name, '">')), ..., i4(2, "</region>"))
  sec <- function(...) c(i4(3, '<supplysector name="elec">'), ..., i4(3, "</supplysector>"))
  sbs <- function(name, ...) c(i4(4, paste0('<subsector name="', name, '">')), ..., i4(4, "</subsector>"))
  tech <- function(name, ..., tag = "technology", at = "") c(i4(5, paste0("<", tag, ' name="', name, '"', at, ">")), ..., i4(5, paste0("</", tag, ">")))
  per <- function(y, ..., n = 6) c(i4(n, paste0('<period year="', y, '">')), ..., i4(n, "</period>"))
  lf <- function(n, tag, v, at = "") i4(n, paste0("<", tag, at, ">", v, "</", tag, ">"))
  chain <- function(inner, nc = ' nocreate="1"') paste(c("<scenario>", i4(1, "<world>"), i4(2, paste0('<region name="USA"', nc, ">")), i4(3, paste0('<supplysector name="elec"', nc, ">")),
                                                         i4(4, paste0('<subsector name="coal"', nc, ">")), inner, i4(4, "</subsector>"), i4(3, "</supplysector>"), i4(2, "</region>"), i4(1, "</world>"), "</scenario>"), collapse = "\n")
  top <- function(...) paste(c("<scenario>", ..., "</scenario>"), collapse = "\n")
  usa <- function(...) doc(reg("USA", sec(sbs("coal", ...))))
  has <- function(x, ...) all(vapply(c(...), function(p) grepl(p, x, fixed = TRUE), NA))
  step2 <- function(m) grepl("; config= can decide it from the configuration's earlier files", m, fixed = TRUE)     # refusals config= can decide, and those it cannot
  never <- function(m) !grepl("config=", m, fixed = TRUE)
  cf <- function(v, ...) tech("a", per(2015, lf(7, "capacity-factor", v)), ...)

  check("add-on: changed value in a stub, identical region left out, guard on wrappers",
        identical(aw(doc(c(reg("USA", sec(sbs("coal", cf(0.8, tag = "stub-technology")))), reg("China", sec(lf(4, "output-unit", "EJ"))))),
                     doc(c(reg("USA", sec(sbs("coal", cf(0.88, tag = "stub-technology")))), reg("China", sec(lf(4, "output-unit", "EJ")))))),
                  chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), per(2015, lf(7, "capacity-factor", 0.88)), i4(5, "</stub-technology>")))))
  check("add-on: header comment names the base file, its md5 and the override inputs; no temporary file left",
        { a <- file.path(d, paste0("a", k, ".xml")); h <- readLines(a, 2)
          h[1] == '<?xml version="1.0" encoding="UTF-8"?>' && has(h[2], "<!-- minaj add-on to b", ".xml (md5 ", "override inputs: energy/A23.globaltech_capacity_factor -->") &&
            !length(list.files(d, "[.]part[.]xml$")) && inherits(xml2::read_xml(a), "xml_document") })
  sw <- lf(5, "share-weight", 1, ' fillout="1" year="1975"'); sw2 <- c(sw, lf(5, "share-weight", 0.5, ' year="2025"'), lf(5, "fuelprefElasticity", 2, ' year="1975"'))
  check("add-on: array field whose base years are a subset is replayed whole, in the override's order", identical(aw(usa(sw), usa(sw2)), chain(sw2)))
  check("refuses: array year set only in the base file (step 2)", has(aw(usa(sw2), usa(sw)), "subsector[coal]/share-weight: years set only in the base file; config= can decide it from the configuration's earlier files"))
  check("add-on: new technology copied as written", identical(aw(usa(cf(0.8)), usa(cf(0.8), tech("b", per(2015, lf(7, "capacity-factor", 0.7))))), chain(tech("b", per(2015, lf(7, "capacity-factor", 0.7))))))
  check("refuses: technology only in the base file (step 2)", has(aw(usa(cf(0.8), tech("b")), usa(cf(0.8))), "technology[b]: only in the base file (removal); config= can decide it from the configuration's earlier files"))
  check("refuses: new region", { m <- aw(usa(cf(0.8)), doc(c(reg("USA", sec(sbs("coal", cf(0.8)))), reg("China")))); has(m, "region[China]: new region") && !grepl("new region;", m, fixed = TRUE) })
  dl <- c(i4(5, '<technology name="a" delete="1"/>'), tech("a", per(2020, lf(7, "capacity-factor", 0.5))))
  check("add-on: the override's own delete, then its content after the delete", identical(aw(usa(cf(0.8)), usa(dl)), chain(dl)))
  check("refuses: delete in the base file only (step 2)", has(aw(usa(dl), usa(cf(0.8))), "technology[a]: deleted in the base file but not in the override; config= can decide it from the configuration's earlier files"))
  check("refuses: type change (step 2)", has(aw(usa(cf(0.8)), usa(cf(0.8, tag = "intermittent-technology"))), "type change (technology to intermittent-technology); config= can decide it from the configuration's earlier files"))
  check("refuses: nocreate differs (step 2)", { m <- aw(usa(cf(0.8)), usa(cf(0.9, at = ' nocreate="1"'))); has(m, "technology[a]: nocreate differs between the base file and the override") && step2(m) })
  check("add-on: nocreate of the override kept, guard not repeated",
        identical(aw(usa(cf(0.8, tag = "stub-technology", at = ' nocreate="1"')), usa(cf(0.9, tag = "stub-technology", at = ' nocreate="1"'))),
                  chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), per(2015, lf(7, "capacity-factor", 0.9)), i4(5, "</stub-technology>")))))
  check("add-on: option minaj.addon_guard = FALSE writes no nocreate", identical(aw(usa(cf(0.8)), usa(cf(0.9)), minaj.addon_guard = FALSE), chain(cf(0.9), nc = "")))
  mix <- function(a, b) c(tech("a", per(1975, lf(7, "capacity-factor", a)), at = ' nocreate="1"'), tech("a", per(1975, lf(7, "share-weight", b)), tag = "stub-technology", at = ' nocreate="1"'))
  check("add-on: one technology written as technology and as stub-technology (emissions files): one wrapper per element, no guard inside",
        identical(aw(usa(mix(1, 1)), usa(mix(2, 3))), chain(c(tech("a", per(1975, lf(7, "capacity-factor", 2)), at = ' nocreate="1"'),
                                                              tech("a", per(1975, lf(7, "share-weight", 3)), tag = "stub-technology", at = ' nocreate="1"')))))
  rule <- function(f, t, fn = "linear", at = "") c(i4(5, paste0('<interpolation-rule apply-to="share-weight" from-year="', f, '" to-year="', t, '"', at, ">")),
                                                   i4(6, paste0('<interpolation-function name="', fn, '"/>')), i4(5, "</interpolation-rule>"))
  check("add-on: rule list extended: the new rules only", identical(aw(usa(rule(2020, 2050)), usa(rule(2020, 2050), rule(2050, 2100, "fixed"))), chain(rule(2050, 2100, "fixed"))))
  check("refuses: rule list changed, not extended (step 2)", has(aw(usa(rule(2020, 2050)), usa(rule(2020, 2060))), "interpolation-rule: rule list changed (not an extension of the base file's list); config= can decide it from the configuration's earlier files"))
  check("add-on: the override clears the rules: its rules from that clear", identical(aw(usa(rule(2020, 2050)), usa(rule(2020, 2050), rule(2020, 2060, at = ' delete="1"'))), chain(rule(2020, 2060, at = ' delete="1"'))))
  check("refuses: the base file clears the rules and the override does not (step 2)",
        has(aw(usa(rule(2020, 2050, at = ' delete="1"')), usa(rule(2020, 2050))), "the base file clears the rules and the override does not; config= can decide it from the configuration's earlier files"))
  kw <- function(...) usa(tech("a", per(2015, i4(7, paste0("<keyword", ..., "/>")))))
  check("add-on: keyword key only in the override",
        identical(aw(kw(' primary-renewable="x"'), kw(' primary-renewable="x" average-fossil-efficiency="0.4"')),
                  chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<keyword average-fossil-efficiency="0.4"/>'), i4(6, "</period>"), i4(5, "</technology>")))))
  check("refuses: keyword dropped or changed (step 2)", has(aw(kw(' primary-renewable="x"'), kw(' primary-renewable="y"')), "period[2015]/keyword: keyword dropped or changed (GCAM keeps the first value); config= can decide it from the configuration's earlier files"))
  srule <- c(i4(6, '<interpolation-rule apply-to="share-weight" from-year="2020" to-year="2050">'), i4(7, '<interpolation-function name="fixed"/>'), i4(6, "</interpolation-rule>"))
  check("add-on: new period and new rule inside a stub-technology (queued after the base file's)",
        identical(aw(usa(cf(0.8, tag = "stub-technology")), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), per(2020, lf(7, "capacity-factor", 0.85)), srule, tag = "stub-technology"))),
                  chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), per(2020, lf(7, "capacity-factor", 0.85)), srule, i4(5, "</stub-technology>")))))
  pf <- function(...) doc(reg("USA", ...))
  co2 <- function(n, v) lf(3, "PrimaryFuelCO2Coef", v, paste0(' name="', n, '"'))
  check("add-on: key map: changed and new keys", identical(aw(pf(co2("gas", 14.2), co2("coal", 20)), pf(co2("gas", 14.2), co2("coal", 21), co2("oil", 1))),
                                                          top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), co2("coal", 21), co2("oil", 1), i4(2, "</region>"), i4(1, "</world>"))))
  check("refuses: PrimaryFuelCO2Coef key removed (keys can only be overwritten)", { m <- aw(pf(co2("gas", 1), co2("oil", 1)), pf(co2("gas", 1))); has(m, "region[USA]/PrimaryFuelCO2Coef: key removed (GCAM can only overwrite these keys)") && !grepl("config=", m, fixed = TRUE) })
  inp <- function(...) usa(tech("a", per(2015, i4(7, '<minicam-energy-input name="coal">'), ..., i4(7, "</minicam-energy-input>"))))
  check("add-on: input flag added; efficiency replaces coefficient",
        identical(aw(inp(lf(8, "flag", "Energy"), lf(8, "coefficient", 2)), inp(lf(8, "flag", "Energy"), lf(8, "flag", "Resource"), lf(8, "efficiency", 0.4))),
                  chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<minicam-energy-input name="coal" nocreate="1">'),
                          lf(8, "flag", "Resource"), lf(8, "efficiency", 0.4), i4(7, "</minicam-energy-input>"), i4(6, "</period>"), i4(5, "</technology>")))))
  check("refuses: input flag set only in the base file (step 2)", { m <- aw(inp(lf(8, "flag", "Energy"), lf(8, "flag", "Resource")), inp(lf(8, "flag", "Energy"))); has(m, "minicam-energy-input[coal]/flag: input flag set only in the base file") && step2(m) })
  mt <- function(e) c(i4(1, "<modeltime>"), lf(2, "start-year", 1975), lf(2, "end-year", e), i4(1, "</modeltime>"))
  check("refuses: modeltime differs", { m <- aw(doc(reg("USA"), mt(2100)), doc(reg("USA"), mt(2300))); has(m, "scenario/modeltime: modeltime differs (GCAM reads modeltime only once)") && never(m) })
  cbp <- function(...) c(i4(1, "<calc-base-price>"), ..., i4(1, "</calc-base-price>"))
  smap <- function(f, t) i4(2, paste0('<sector-map from-name="', f, '" to-name="', t, '"/>'))
  cb1 <- cbp(smap("a", "b"), lf(2, "ignore-region", "USA")); cb2 <- cbp(smap("a", "c"), smap("x", "y")); cb4 <- cbp(smap("a", "b"), smap("x", "y")); cb5 <- cbp(smap("a", "b"))
  check("add-on: calc-base-price keeping every (from-name, to-name) pair: the override's whole block", identical(aw(doc(reg("USA"), cb1), doc(reg("USA"), cb4)), top(cb4)))
  check("refuses: calc-base-price with a to-name changed", { m <- aw(doc(reg("USA"), cb1), doc(reg("USA"), cb2)); has(m, "scenario/calc-base-price: calc-base-price changed") && never(m) })
  check("refuses: calc-base-price dropping a sector-map key", { m <- aw(doc(reg("USA"), cb2), doc(reg("USA"), cb1)); has(m, "scenario/calc-base-price: calc-base-price changed") && never(m) })
  check("add-on: calc-base-price with its ignore-region removed (ignore-region only gates a log line): the override's whole block", identical(aw(doc(reg("USA"), cb1), doc(reg("USA"), cb5)), top(cb5)))
  check("add-on: calc-base-price block only in the override", identical(aw(doc(reg("USA")), doc(reg("USA"), cb1)), top(cb1)))
  check("refuses: calc-base-price block only in the base file", { m <- aw(doc(reg("USA"), cb1), doc(reg("USA"))); has(m, "scenario/calc-base-price: calc-base-price changed") && never(m) })
  check("refuses: calc-base-price block with only an ignore-region in the base file, none in the override",
        { m <- aw(doc(reg("USA"), cbp(lf(2, "ignore-region", "USA"))), doc(reg("USA"))); has(m, "scenario/calc-base-price: calc-base-price changed") && never(m) })
  check("refuses: attributes of <scenario> differ", { m <- aw(sub("<scenario>", '<scenario name="a">', doc(reg("USA")), fixed = TRUE), sub("<scenario>", '<scenario name="b">', doc(reg("USA")), fixed = TRUE)); has(m, "scenario: attributes of <scenario> differ") && never(m) })
  check("refuses: tag unknown to GCAM (fail closed)", { m <- aw(doc(reg("USA", sec())), doc(reg("USA", sec(lf(4, "dummy-logit-tag", 1))))); has(m, "supplysector[elec]/dummy-logit-tag: tag unknown to GCAM in SupplySector") && never(m) })
  check("refuses: name with & (GCAM compares names as raw bytes)", { m <- aw(doc(reg("USA", sec())), doc(reg("USA", sec(sbs("a&amp;b"))))); has(m, "subsector[a&amp;b]: name holds & < > \" ' or a non-ASCII byte") && never(m) })
  gtd <- function(v) c(i4(2, "<global-technology-database>"), i4(3, '<location-info sector-name="elec" subsector-name="coal">'), i4(4, '<technology name="a">'),
                       per(2015, lf(6, "capacity-factor", v), n = 5), i4(4, "</technology>"), i4(3, "</location-info>"), i4(2, "</global-technology-database>"))
  gtx <- top(i4(1, "<world>"), i4(2, '<global-technology-database nocreate="1">'), i4(3, '<location-info sector-name="elec" subsector-name="coal">'), i4(4, '<technology name="a" nocreate="1">'),
             i4(5, '<period year="2015" nocreate="1">'), lf(6, "capacity-factor", 0.88), i4(5, "</period>"), i4(4, "</technology>"), i4(3, "</location-info>"), i4(2, "</global-technology-database>"), i4(1, "</world>"))
  check("add-on: global technology database: location-info without nocreate", identical(aw(doc(c(reg("USA"), gtd(0.8))), doc(c(reg("USA"), gtd(0.88)))), gtx))
  check("add-on: nothing differs: empty <scenario></scenario>", identical(aw(doc(reg("USA")), doc(reg("USA"))), "<scenario></scenario>") && isTRUE(last$empty))
  gr <- function(...) doc(reg("USA", i4(3, '<resource name="coal">'), i4(4, '<subresource name="c">'), ..., i4(4, "</subresource>"), i4(3, "</resource>")))
  g <- function(n, v) c(i4(5, paste0('<grade name="', n, '">')), lf(6, "extractioncost", v), i4(5, "</grade>"))
  check("add-on: grade appended after the base file's grades",
        identical(aw(gr(g("g1", 1), g("g2", 2)), gr(g("g1", 1), g("g2", 2), g("g3", 3))),
                  top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<resource name="coal" nocreate="1">'), i4(4, '<subresource name="c" nocreate="1">'), g("g3", 3),
                      i4(4, "</subresource>"), i4(3, "</resource>"), i4(2, "</region>"), i4(1, "</world>"))))
  check("refuses: grade inserted between grades (order-critical, step 2)", { m <- aw(gr(g("g1", 1), g("g3", 3)), gr(g("g1", 1), g("g2", 2), g("g3", 3))); has(m, "subresource[c]/grade: entry order changes in an order-critical list") && step2(m) })
  check("add-on: subsector inserted between subsectors: copied, order move listed",
        identical(aw(doc(reg("USA", sec(sbs("a"), sbs("c")))), doc(reg("USA", sec(sbs("a"), sbs("b"), sbs("c"))))),
                  top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<supplysector name="elec" nocreate="1">'), sbs("b"), i4(3, "</supplysector>"), i4(2, "</region>"), i4(1, "</world>"))) &&
          identical(last$moves, "scenario/world/region[USA]/supplysector[elec]/subsector (2 of 3: b, c)"))
  one <- function(v) paste0('<scenario><world><region name="USA"><supplysector name="elec"><output-unit>', v, "</output-unit></supplysector></region></world></scenario>")
  check("add-on: files not laid out as gcamdata writes them are read whole",
        identical(aw(one("EJ"), one("PJ")), top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<supplysector name="elec" nocreate="1">'), lf(4, "output-unit", "PJ"),
                                                i4(3, "</supplysector>"), i4(2, "</region>"), i4(1, "</world>"))))
  cd <- reg("China", sec(i4(4, "<!-- x --><![CDATA[x]]>")))
  check("add-on: blocks identical in both files are not read (block split)", identical(aw(doc(c(reg("USA", sec(lf(4, "output-unit", "EJ"))), cd)), doc(c(reg("USA", sec(lf(4, "output-unit", "PJ"))), cd))),
                                                                                    top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<supplysector name="elec" nocreate="1">'), lf(4, "output-unit", "PJ"),
                                                                                        i4(3, "</supplysector>"), i4(2, "</region>"), i4(1, "</world>"))))
  check("refuses: value set only in the base file (step 2)", has(aw(doc(reg("USA", sec(lf(4, "output-unit", "EJ")))), doc(reg("USA", sec()))), "supplysector[elec]/output-unit: value set only in the base file (un-set); config= can decide it from the configuration's earlier files"))
  lar <- function(n) doc(reg("USA", i4(3, paste0('<LandAllocatorRoot name="', n, '">')), i4(4, '<landAllocation year="1975">1</landAllocation>'), i4(3, "</LandAllocatorRoot>")))
  check("add-on: value given as an attribute goes on the wrapper", identical(aw(lar("root"), lar("root2")), top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<LandAllocatorRoot name="root2" nocreate="1"/>'), i4(2, "</region>"), i4(1, "</world>"))))
  check("refuses: every place listed in one error", { m <- aw(doc(c(reg("USA", sec(sbs("a"), sbs("b")))), mt(2100)), doc(c(reg("USA", sec(sbs("a")))), mt(2300)))
          has(m, "2 place(s) cannot be written as an add-on", "subsector[b]: only in the base file", "scenario/modeltime: modeltime differs", "o", ".xml in place of b") })
  check("refuses: add-on larger than option minaj.addon_max_ratio x the override", has(aw(usa(cf(0.8)), usa(cf(0.9)), minaj.addon_max_ratio = 0.5), "more than 0.5 x o", "minaj.addon_max_ratio = Inf"))
  ow <- function(...) top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), ..., i4(2, "</region>"), i4(1, "</world>"))
  os <- function(...) ow(i4(3, '<supplysector name="elec" nocreate="1">'), ..., i4(3, "</supplysector>"))
  check("add-on: value set only in the override copied", identical(aw(doc(reg("USA", sec())), doc(reg("USA", sec(lf(4, "output-unit", "EJ"))))), os(lf(4, "output-unit", "EJ"))))
  check("add-on: changed value in a technology period: guard nocreate on the technology and period wrappers",
        identical(aw(usa(cf(0.8)), usa(cf(0.9))), chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), lf(7, "capacity-factor", 0.9), i4(6, "</period>"), i4(5, "</technology>")))))
  check("add-on: new period in a technology: the period as written (no guard), the technology wrapper guarded",
        identical(aw(usa(cf(0.8)), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), per(2020, lf(7, "capacity-factor", 0.85))))),
                  chain(c(i4(5, '<technology name="a" nocreate="1">'), per(2020, lf(7, "capacity-factor", 0.85)), i4(5, "</technology>")))))
  swv <- function(v, at = ' fillout="1" year="1975"') lf(5, "share-weight", v, at)
  check("add-on: fillout value changed (same year and fillout): replayed", identical(aw(usa(sw), usa(swv(0.5))), chain(swv(0.5))))
  check("refuses: fillout in the base file, the same year without fillout in the override (step 2)", has(aw(usa(sw), usa(swv(1, ' year="1975"'))), "subsector[coal]/share-weight: years set only in the base file; config= can decide it from the configuration's earlier files"))
  check("refuses: fillout from another year in the override (step 2)", has(aw(usa(sw), usa(swv(1, ' fillout="1" year="1990"'))), "subsector[coal]/share-weight: years set only in the base file; config= can decide it from the configuration's earlier files"))
  check("add-on: rule lists equal: not written", identical(aw(usa(sw, rule(2020, 2050)), usa(swv(0.5), rule(2020, 2050))), chain(swv(0.5))))
  rsw <- sub("share-weight", "share.weight", rule(2050, 2100, at = ' delete="1"'), fixed = TRUE)
  check("add-on: rule with apply-to=\"share.weight\" and delete=\"1\" is not a clear: appended", identical(aw(usa(rule(2020, 2050)), usa(rule(2020, 2050), rsw)), chain(rsw)))
  check("add-on: the override's own delete without a rebuild: the delete only",
        identical(aw(usa(cf(0.8), tech("b")), usa(cf(0.8), i4(5, '<technology name="b" delete="1"/>'))), chain(i4(5, '<technology name="b" delete="1"/>'))))
  check("add-on: the same delete and rebuild in both files: not written", identical(aw(usa(dl, sw), usa(dl, swv(0.5))), chain(swv(0.5))))
  check("refuses: region deleted by the override (addon_minaj never deletes or rebuilds a region)", {
    m <- aw(doc(c(reg("USA"), reg("China", sec(lf(4, "output-unit", "EJ"))))), doc(c(reg("USA"), i4(2, '<region name="China" delete="1"/>'), reg("China", sec(lf(4, "output-unit", "PJ"))))))
    has(m, "region[China]: deleted by the override, and addon_minaj never deletes or rebuilds this container") && !grepl("config=", m, fixed = TRUE) && !grepl("GCAM must not", m, fixed = TRUE) })
  check("refuses: type change of a container addon_minaj never rebuilds (climate model)", {
    m <- aw(doc(c(reg("USA"), i4(2, "<no-climate-model/>"))), doc(c(reg("USA"), i4(2, "<HectorModel/>"))))
    has(m, "scenario/world/HectorModel: type change (no-climate-model to HectorModel)") && !grepl("config=", m, fixed = TRUE) })
  st2 <- function(...) usa(tech("a", ..., tag = "stub-technology"))
  check("refuses: period only in the base file's stub-technology (needs a stub replace; step 2)",
        has(aw(st2(per(2015, lf(7, "capacity-factor", 0.8)), per(2020, lf(7, "capacity-factor", 0.8))), st2(per(2015, lf(7, "capacity-factor", 0.8)))),
            "stub-technology[a]/period[2020]: only in the base file (removal); config= can decide it from the configuration's earlier files"))
  check("refuses: value set only in the base file inside a stub-technology (step 2)",
        has(aw(st2(per(2015, lf(7, "capacity-factor", 0.8), lf(7, "share-weight", 1))), st2(per(2015, lf(7, "capacity-factor", 0.8)))),
            "stub-technology[a]/period[2015]/share-weight: value set only in the base file (un-set); config= can decide it from the configuration's earlier files"))
  srule2 <- sub("2050", "2060", srule, fixed = TRUE)
  check("refuses: rule list inside a stub-technology changed (step 2)", has(aw(st2(per(2015, lf(7, "capacity-factor", 0.8)), srule), st2(per(2015, lf(7, "capacity-factor", 0.8)), srule2)),
                                                                             "stub-technology[a]/interpolation-rule: rule list changed (not an extension of the base file's list); config= can decide it from the configuration's earlier files"))
  cc <- function(v) st2(per(2015, i4(7, '<minicam-energy-input name="coal">'), lf(8, "current-coef", v, ' year="2015"'), i4(7, "</minicam-energy-input>")))
  check("refuses: current-coef differs inside a stub-technology (storage shared between regions)",
        { m <- aw(cc(1), cc(2)); has(m, "minicam-energy-input[coal]/current-coef: differs inside a stub-technology (storage shared between regions)") && !grepl("config=", m, fixed = TRUE) })
  luh <- function(...) doc(reg("USA", i4(3, '<LandAllocatorRoot name="root">'), i4(4, '<LandLeaf name="l">'), i4(5, "<land-use-history>"), ..., i4(5, "</land-use-history>"), i4(4, "</LandLeaf>"), i4(3, "</LandAllocatorRoot>")))
  al <- function(y, v) lf(6, "allocation", v, paste0(' year="', y, '"'))
  check("add-on: key map (land-use-history allocation): changed and new keys only",
        identical(aw(luh(al(1700, 5), al(1750, 6)), luh(al(1700, 5), al(1750, 7), al(1800, 8))),
                  ow(i4(3, '<LandAllocatorRoot nocreate="1">'), i4(4, '<LandLeaf name="l" nocreate="1">'), i4(5, '<land-use-history nocreate="1">'), al(1750, 7), al(1800, 8),
                     i4(5, "</land-use-history>"), i4(4, "</LandLeaf>"), i4(3, "</LandAllocatorRoot>"))))
  check("refuses: key-map key removed (step 2)", has(aw(luh(al(1700, 5), al(1750, 6)), luh(al(1700, 5))), "land-use-history/allocation: key removed; config= can decide it from the configuration's earlier files"))
  mac <- function(...) usa(tech("a", per(2015, i4(7, '<Non-CO2 name="SO2">'), i4(8, '<mac-control name="m">'), lf(9, "mac-reduction", 0.1, ' tax="10"'), ..., i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))))
  check("add-on: set-true flag (no-zero-cost-reductions) set only in the override",
        identical(aw(mac(), mac(i4(9, "<no-zero-cost-reductions/>"))),
                  chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<Non-CO2 name="SO2" nocreate="1">'), i4(8, '<mac-control name="m" nocreate="1">'),
                          i4(9, "<no-zero-cost-reductions/>"), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"), i4(6, "</period>"), i4(5, "</technology>")))))
  check("refuses: set-true flag set only in the base file (step 2)", has(aw(mac(i4(9, "<no-zero-cost-reductions/>")), mac()), "mac-control[m]/no-zero-cost-reductions: set only in the base file; config= can decide it from the configuration's earlier files"))
  check("refuses: coefficient set only in the base file (step 2)", has(aw(inp(lf(8, "flag", "Energy"), lf(8, "coefficient", 2)), inp(lf(8, "flag", "Energy"))),
                                                                       "minicam-energy-input[coal]/coefficient: coefficient set only in the base file; config= can decide it from the configuration's earlier files"))
  check("refuses: keyword key dropped (step 2)", { m <- aw(kw(' primary-renewable="x" average-fossil-efficiency="0.4"'), kw(' primary-renewable="x"')); has(m, "period[2015]/keyword: keyword dropped or changed") && step2(m) })
  check("add-on: calc-base-price identical: not written", identical(aw(doc(reg("USA", sec(lf(4, "output-unit", "EJ"))), cb1), doc(reg("USA", sec(lf(4, "output-unit", "PJ"))), cb1)), os(lf(4, "output-unit", "PJ"))))
  cb3 <- cbp(smap("a", "b"), lf(2, "ignore-region", "USA"), lf(2, "ignore-region", "China"))
  check("add-on: calc-base-price with an ignore-region added: the override's whole block", identical(aw(doc(reg("USA"), cb1), doc(reg("USA"), cb3)), top(cb3)))
  sol <- function(t) c(i4(1, '<user-configurable-solver year="1975">'), lf(2, "solution-tolerance", t), i4(1, "</user-configurable-solver>"))
  check("refuses: solver settings differ", { m <- aw(doc(reg("USA"), sol(0.001)), doc(reg("USA"), sol(1e-4))); has(m, "scenario/user-configurable-solver: solver settings differ") && !grepl("config=", m, fixed = TRUE) })
  check("refuses: tag unknown to GCAM in the base file only (fail closed)", { m <- aw(doc(reg("USA", sec(lf(4, "dummy-logit-tag", 1)))), doc(reg("USA", sec()))); has(m, "supplysector[elec]/dummy-logit-tag: tag unknown to GCAM in SupplySector") && never(m) })
  check("refuses: name with ' (GCAM compares names as raw bytes)", { m <- aw(doc(reg("USA", sec())), doc(reg("USA", sec(sbs("a'b"))))); has(m, "subsector[a'b]: name holds") && never(m) })
  check("refuses: name with a non-ASCII byte", { m <- aw(doc(reg("USA", sec())), doc(reg("USA", sec(sbs("Côte"))))); has(m, "/subsector[C", ": name holds") && never(m) })
  check("add-on: files that differ only in layout: empty <scenario></scenario>", identical(aw(one("EJ"), doc(reg("USA", sec(lf(4, "output-unit", "EJ"))))), "<scenario></scenario>") && isTRUE(last$empty))

  # nocreate patterns: the first element creates the slot in both files (MAC tables), or nocreate before it
  mn <- function(v, w) usa(tech("a", per(2015, i4(7, '<Non-CO2 name="HFC134a">'), i4(8, '<mac-control name="m">'), lf(9, "mac-reduction", v, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"),
                                     i4(7, '<Non-CO2 name="HFC134a" nocreate="1">'), i4(8, '<mac-control name="m" nocreate="1">'), lf(9, "tech-change", w, ' year="2020"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))))
  check("add-on: equal nocreate pattern whose first element creates the slot (MAC tables): the change, in wrappers",
        identical(aw(mn(0.3, 0.01), mn(0.4, 0.01)), chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<Non-CO2 name="HFC134a" nocreate="1">'),
                                                           i4(8, '<mac-control name="m" nocreate="1">'), lf(9, "mac-reduction", 0.4, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"), i4(6, "</period>"), i4(5, "</technology>")))))
  check("add-on: same pattern, option minaj.addon_guard = FALSE: one wrapper per override element, nocreate where the override has it",
        identical(aw(mn(0.3, 0.01), mn(0.4, 0.02), minaj.addon_guard = FALSE),
                  chain(c(i4(5, '<technology name="a">'), i4(6, '<period year="2015">'), i4(7, '<Non-CO2 name="HFC134a">'), i4(8, '<mac-control name="m">'), lf(9, "mac-reduction", 0.4, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"),
                          i4(7, '<Non-CO2 name="HFC134a" nocreate="1">'), i4(8, '<mac-control name="m" nocreate="1">'), lf(9, "tech-change", 0.02, ' year="2020"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"),
                          i4(6, "</period>"), i4(5, "</technology>")), nc = "")))
  nm2 <- function(v) usa(tech("a", per(2015, lf(7, "capacity-factor", v)), at = ' nocreate="1"'), tech("a", per(2015, lf(7, "share-weight", 1))))
  check("refuses: equal nocreate pattern with nocreate before the element that creates the slot (step 2)", { m <- aw(nm2(0.8), nm2(0.9)); has(m, "technology[a]: nocreate pattern") && step2(m) })
  # type-flags and input flags
  check("refuses: type-flags and input flags written in another order (the order decides the flags)",
        { m <- aw(inp(lf(8, "type-flags", 1), lf(8, "flag", "Resource")), inp(lf(8, "flag", "Resource"), lf(8, "type-flags", 1))); has(m, "minicam-energy-input[coal]/type-flags: type-flags and input flags differ") && never(m) })
  check("add-on: type-flags and input flags alike: the other change only",
        identical(aw(inp(lf(8, "type-flags", 1), lf(8, "flag", "Resource"), lf(8, "coefficient", 2)), inp(lf(8, "type-flags", 1), lf(8, "flag", "Resource"), lf(8, "coefficient", 3))),
                  chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<minicam-energy-input name="coal" nocreate="1">'),
                          lf(8, "coefficient", 3), i4(7, "</minicam-energy-input>"), i4(6, "</period>"), i4(5, "</technology>")))))
  # arrays
  check("add-on: array year without fillout in the base file, the same year with fillout in the override: replayed", identical(aw(usa(swv(1, ' year="1975"')), usa(swv(1))), chain(swv(1))))
  check("add-on: array element without a year attribute sets nothing (no un-set)", identical(aw(usa(sw, lf(5, "share-weight", 2)), usa(swv(0.5))), chain(swv(0.5))))
  check("add-on: array (year, fillout) written twice in the override: replayed in its order", identical(aw(usa(swv(1)), usa(swv(1), swv(2))), chain(c(swv(1), swv(2)))))
  # entries without their key attribute
  ut <- c(i4(5, "<technology>"), per(2015, lf(7, "capacity-factor", 0.7)), i4(5, "</technology>"))
  check("add-on: technology without a name attribute (a new entry each time): appended as a list entry", identical(aw(usa(cf(0.8)), usa(cf(0.8), ut)), chain(ut)))
  check("refuses: technology without a name attribute, with nocreate (GCAM skips it)",
        { m <- aw(usa(cf(0.8)), usa(cf(0.8), sub("<technology>", '<technology nocreate="1">', ut, fixed = TRUE))); has(m, "subsector[coal]/technology: delete or nocreate on an entry without its key attribute") && never(m) })
  check("refuses: period without a year attribute", { m <- aw(usa(cf(0.8)), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), i4(6, "<period>"), lf(7, "capacity-factor", 1), i4(6, "</period>"))))
                                                       has(m, "technology[a]/period: period without a year attribute") && never(m) })
  check("parse error inside a block names the block", { m <- aw(doc(c(reg("USA"), reg("China", sec(lf(4, "output-unit", "EJ"))))), doc(c(reg("USA"), reg("China", sec(i4(4, "<![CDATA[x]]>"))))))
                                                         has(m, "(block region China; line numbers count from the block)", "line 3 of the block", "CDATA section") })
  # containers addon_minaj never deletes or rebuilds
  check("refuses: region only in the base file (addon_minaj never deletes a region)", { m <- aw(doc(c(reg("USA"), reg("China"))), doc(reg("USA")))
                                                                                        has(m, "region[China]: region only in the base file (addon_minaj never deletes a region") && never(m) && !grepl("GCAM cannot", m, fixed = TRUE) })
  check("refuses: global technology database only in the base file", { m <- aw(doc(c(reg("USA"), gtd(0.8))), doc(reg("USA")))
                                                                       has(m, "scenario/world/global-technology-database: only in the base file (removal), and addon_minaj never deletes this container") && never(m) })
  check("refuses: region only in the base file, written there only with nocreate (step 2)",
        { m <- aw(doc(c(reg("USA"), i4(2, '<region name="China" nocreate="1">'), sec(lf(4, "output-unit", "EJ")), i4(2, "</region>"))), doc(reg("USA"))); has(m, "region[China]: only in the base file (removal)") && step2(m) })
  check("refuses: global technology database deleted by the override",
        { m <- aw(doc(c(reg("USA"), gtd(0.8))), doc(c(reg("USA"), i4(2, '<global-technology-database delete="1"/>'), gtd(0.88)))); has(m, "global-technology-database: deleted by the override, and addon_minaj never deletes or rebuilds this container") && never(m) })
  check("refuses: land allocator deleted by the override",
        { m <- aw(lar("root"), doc(reg("USA", i4(3, '<LandAllocatorRoot delete="1"/>'), i4(3, '<LandAllocatorRoot name="root">'), i4(4, '<landAllocation year="1975">2</landAllocation>'), i4(3, "</LandAllocatorRoot>"))))
          has(m, "LandAllocatorRoot: deleted by the override, and addon_minaj never deletes or rebuilds this container") && never(m) })
  check("add-on: location-info with delete=\"1\" in the override is merged (GCAM does not read its flags)",
        identical(aw(doc(c(reg("USA"), gtd(0.8))), doc(c(reg("USA"), sub("<location-info ", '<location-info delete="1" ', gtd(0.88), fixed = TRUE)))), gtx))
  # tags GCAM does not know, inside what the add-on copies whole
  check("refuses: tag unknown to GCAM inside a new container", { m <- aw(usa(cf(0.8)), usa(cf(0.8), tech("b", per(2015, lf(7, "capacity-factor", 0.7)), lf(6, "dummy-logit-tag", 1))))
                                                                  has(m, "technology[b]/dummy-logit-tag: tag unknown to GCAM in") && never(m) })
  check("refuses: tag unknown to GCAM inside the override's rebuild after its delete",
        { m <- aw(usa(cf(0.8)), usa(i4(5, '<technology name="a" delete="1"/>'), tech("a", per(2020, lf(7, "capacity-factor", 0.5), lf(7, "dummy-logit-tag", 1))))); has(m, "period[2020]/dummy-logit-tag: tag unknown to GCAM in") && never(m) })
  check("refuses: tag unknown to GCAM inside an appended rule",
        { m <- aw(usa(rule(2020, 2050)), usa(rule(2020, 2050), sub("interpolation-function", "interpolaton-function", rule(2050, 2100), fixed = TRUE))); has(m, "interpolation-rule/interpolaton-function[linear]: tag unknown to GCAM in InterpolationRule") && never(m) })
  # names GCAM compares as raw bytes
  check("refuses: year with ' in a new period", { m <- aw(usa(cf(0.8)), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), per("2020'", lf(7, "capacity-factor", 0.85)))), warning.length = 8170)
                                                has(m, "period[2020']: year holds", "period[2020']: year not written as a whole number") && never(m) })
  li <- function(sn, ss) c(i4(3, paste0('<location-info sector-name="', sn, '" subsector-name="', ss, '">')), i4(4, '<technology name="b">'), per(2015, lf(6, "capacity-factor", 1), n = 5), i4(4, "</technology>"), i4(3, "</location-info>"))
  gtd2 <- function(...) c(i4(2, "<global-technology-database>"), i4(3, '<location-info sector-name="elec" subsector-name="coal">'), i4(4, '<technology name="a">'), per(2015, lf(6, "capacity-factor", 0.8), n = 5),
                          i4(4, "</technology>"), i4(3, "</location-info>"), ..., i4(2, "</global-technology-database>"))
  check("refuses: sector-name with & in a new location-info", { m <- aw(doc(c(reg("USA"), gtd2())), doc(c(reg("USA"), gtd2(li("el&amp;ec", "coal"))))); has(m, ": sector-name holds") && never(m) })
  check("refuses: subsector-name with ' in a new location-info", { m <- aw(doc(c(reg("USA"), gtd2())), doc(c(reg("USA"), gtd2(li("elec", "co'al"))))); has(m, ": subsector-name holds") && never(m) })
  # stub-technologies
  check("add-on: new keyword key inside a stub-technology",
        identical(aw(st2(per(2015, i4(7, '<keyword primary-renewable="x"/>'))), st2(per(2015, i4(7, '<keyword primary-renewable="x" average-fossil-efficiency="0.4"/>')))),
                  chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), i4(6, '<period year="2015">'), i4(7, '<keyword average-fossil-efficiency="0.4"/>'), i4(6, "</period>"), i4(5, "</stub-technology>")))))
  check("add-on: the override's own period delete and rebuild inside a stub-technology",
        identical(aw(st2(per(2015, lf(7, "capacity-factor", 0.8))), st2(i4(6, '<period year="2015" delete="1"/>'), per(2015, lf(7, "capacity-factor", 0.9)))),
                  chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), i4(6, '<period year="2015" delete="1"/>'), per(2015, lf(7, "capacity-factor", 0.9)), i4(5, "</stub-technology>")))))
  sclr <- sub("2050", "2060", c(sub(">$", ' delete="1">', srule[1]), srule[-1]), fixed = TRUE)
  check("add-on: the override's own rule clear inside a stub-technology",
        identical(aw(st2(per(2015, lf(7, "capacity-factor", 0.8)), srule), st2(per(2015, lf(7, "capacity-factor", 0.8)), srule, sclr)),
                  chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), sclr, i4(5, "</stub-technology>")))))
  osd <- function(v) st2(per(2015, i4(7, '<exogenous-shutdown-decider name="x">'), lf(8, "output-scalar", v, ' year="2015"'), i4(7, "</exogenous-shutdown-decider>")))
  check("refuses: output-scalar differs inside a stub-technology", { m <- aw(osd(1), osd(2)); has(m, "exogenous-shutdown-decider[x]/output-scalar: differs inside a stub-technology") && never(m) })
  check("refuses: stub-technology only in the base file (a stub replace; step 2)",
        { m <- aw(usa(cf(0.8, tag = "stub-technology"), tech("b", tag = "stub-technology")), usa(cf(0.8, tag = "stub-technology"))); has(m, "stub-technology[b]: only in the base file (removal)") && step2(m) })
  # key maps
  macw <- function(...) chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), ..., i4(6, "</period>"), i4(5, "</technology>")))
  mac2 <- function(v) usa(tech("a", per(2015, i4(7, '<Non-CO2 name="SO2">'), i4(8, '<mac-control name="m">'), lf(9, "mac-reduction", v, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))))
  check("add-on: key map (mac-reduction, key tax): changed value",
        identical(aw(mac2(0.1), mac2(0.2)), macw(i4(7, '<Non-CO2 name="SO2" nocreate="1">'), i4(8, '<mac-control name="m" nocreate="1">'), lf(9, "mac-reduction", 0.2, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))))
  fso <- function(v) usa(tech("a", per(2015, i4(7, '<fractional-secondary-output name="x">'), lf(8, "fraction-produced", v, ' price="1"'), i4(7, "</fractional-secondary-output>"))))
  check("add-on: key map (fraction-produced, key price): changed value",
        identical(aw(fso(0.5), fso(0.6)), macw(i4(7, '<fractional-secondary-output name="x" nocreate="1">'), lf(8, "fraction-produced", 0.6, ' price="1"'), i4(7, "</fractional-secondary-output>"))))
  check("add-on: key map with a key written twice in the override: its last value",
        identical(aw(luh(al(1700, 5)), luh(al(1700, 6), al(1700, 7))),
                  ow(i4(3, '<LandAllocatorRoot nocreate="1">'), i4(4, '<LandLeaf name="l" nocreate="1">'), i4(5, '<land-use-history nocreate="1">'), al(1700, 7), i4(5, "</land-use-history>"), i4(4, "</LandLeaf>"), i4(3, "</LandAllocatorRoot>"))))
  # order-critical lists
  gra <- function(...) top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<resource name="coal" nocreate="1">'), i4(4, '<subresource name="c" nocreate="1">'), ...,
                           i4(4, "</subresource>"), i4(3, "</resource>"), i4(2, "</region>"), i4(1, "</world>"))
  fst <- function(...) usa(tech("f", per(2015, ...), tag = "food-storage-technology"))
  inx <- function(n) c(i4(7, paste0('<minicam-energy-input name="', n, '">')), lf(8, "coefficient", 1), i4(7, "</minicam-energy-input>"))
  check("refuses: input inserted before a food-storage technology's first input (order-critical, step 2)",
        { m <- aw(fst(inx("a"), inx("b")), fst(inx("c"), inx("a"), inx("b"))); has(m, "period[2015]/minicam-energy-input: entry order changes in an order-critical list") && step2(m) })
  check("add-on: input appended after a food-storage technology's inputs",
        identical(aw(fst(inx("a")), fst(inx("a"), inx("b"))), chain(c(i4(5, '<food-storage-technology name="f" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), inx("b"), i4(6, "</period>"), i4(5, "</food-storage-technology>")))))
  check("add-on: the override's own delete and rebuild of the last grade (final order unchanged)",
        identical(aw(gr(g("g1", 1), g("g2", 2), g("g3", 3)), gr(g("g1", 1), g("g2", 2), g("g3", 3), i4(5, '<grade name="g3" delete="1"/>'), g("g3", 4))), gra(i4(5, '<grade name="g3" delete="1"/>'), g("g3", 4))))
  check("refuses: the override's own delete and rebuild of a middle grade (final order differs; step 2)",
        { m <- aw(gr(g("g1", 1), g("g2", 2), g("g3", 3)), gr(g("g1", 1), i4(5, '<grade name="g2" delete="1"/>'), g("g2", 2.5), g("g3", 3))); has(m, "subresource[c]/grade: entry order changes in an order-critical list") && step2(m) })
  # entries without their key attribute (GCAM appends each one) in ordered lists
  ui <- c(i4(7, "<minicam-energy-input>"), lf(8, "coefficient", 2), i4(7, "</minicam-energy-input>"))
  check("refuses: input without a name inserted before a food-storage technology's first input (order-critical; config= cannot help)",
        { m <- aw(fst(inx("a")), fst(ui, inx("a"))); has(m, "period[2015]/minicam-energy-input: entry without its key attribute in an order-critical list", "1 place(s)") && never(m) && !grepl("entry order changes", m, fixed = TRUE) })
  check("refuses: the same input without a name, before a food-storage technology's input in the override and after it in the base file",
        { m <- aw(fst(inx("a"), ui), fst(ui, inx("a"))); has(m, "period[2015]/minicam-energy-input: entry order changes in an order-critical list") && never(m) })
  check("refuses: grade without a name inserted between grades",
        { m <- aw(gr(g("g1", 1), g("g3", 3)), gr(g("g1", 1), sub(' name="g2"', "", g("g2", 2), fixed = TRUE), g("g3", 3))); has(m, "subresource[c]/grade: entry without its key attribute in an order-critical list") && never(m) })
  check("add-on: technology without a name placed before a named one: appended, order move listed",
        identical(aw(usa(cf(0.8)), usa(ut, cf(0.8))), chain(ut)) && identical(last$moves, "scenario/world/region[USA]/supplysector[elec]/subsector[coal]/technology (2 of 2: #1, a)"))
  check("add-on: technology without a name appended after the named ones: no order move", identical(aw(usa(cf(0.8)), usa(cf(0.8), ut)), chain(ut)) && !length(last$moves))
  check("add-on: technology without a name placed before two named ones in a new order: the move names each entry",
        identical(aw(usa(cf(0.8), tech("b")), usa(ut, tech("b"), cf(0.8))), chain(ut)) && identical(last$moves, "scenario/world/region[USA]/supplysector[elec]/subsector[coal]/technology (3 of 3: #1, b, a)"))
  uo <- c(i4(7, "<secondary-output>"), lf(8, "output-ratio", 1), i4(7, "</secondary-output>"))
  check("refuses: output without a name added before a food-storage technology's inputs, named inputs reordered (step 2)",
        { m <- aw(fst(inx("a"), inx("b")), fst(uo, inx("b"), inx("a"))); has(m, "period[2015]/minicam-energy-input: entry order changes in an order-critical list") && step2(m) })
  dlu <- i4(5, '<technology delete="1"/>')
  check("add-on: the same technology without a name and with delete (GCAM skips it) placed elsewhere: nothing written, no order move",
        identical(aw(usa(cf(0.8), dlu), usa(dlu, cf(0.8))), "<scenario></scenario>") && !length(last$moves))
  check("refuses: input without a name inserted before another inside a stub-technology (the global technology decides; step 2)",
        { m <- aw(st2(per(2015, inx("a"))), st2(per(2015, ui, inx("a")))); has(m, "stub-technology[a]/period[2015]/minicam-energy-input: entry without its key attribute in an order-critical list") && step2(m) })
  # keys GCAM reads as a number (year) or matches to an entry made without one (name="")
  check("refuses: period year 02015 (GCAM reads year 2015)",
        { m <- aw(usa(cf(0.8)), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), per("02015", lf(7, "capacity-factor", 0.9))))); has(m, "technology[a]/period[02015]: year not written as a whole number without leading zeros") && never(m) })
  check("refuses: year 02015 inside a new technology", { m <- aw(usa(cf(0.8)), usa(cf(0.8), tech("b", per("02015", lf(7, "capacity-factor", 0.9))))); has(m, "technology[b]/period[02015]: year not written") && never(m) })
  check("refuses: technology with name=\"\" (GCAM merges it into an entry made without a name)",
        { m <- aw(usa(ut), usa(sub("<technology>", '<technology name="">', ut, fixed = TRUE), ut)); has(m, "subsector[coal]/technology[]: empty name") && never(m) })
  # inputs inside a stub-technology: its class comes from the global technology (possibly a food-storage technology)
  check("refuses: input inserted before another inside a stub-technology (step 2)",
        { m <- aw(st2(per(2015, inx("a"))), st2(per(2015, inx("c"), inx("a"))))
          has(m, "stub-technology[a]/period[2015]/minicam-energy-input: entry order changes in an order-critical list (grades, food-storage inputs; inside a stub-technology the global technology decides the class)") && step2(m) })
  check("add-on: input appended after the inputs inside a stub-technology",
        identical(aw(st2(per(2015, inx("a"))), st2(per(2015, inx("a"), inx("b")))), chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), i4(6, '<period year="2015">'), inx("b"), i4(6, "</period>"), i4(5, "</stub-technology>")))))
  # key-map keys as GCAM reads them (tax and price: double; year: unsigned int)
  macv <- function(...) usa(tech("a", per(2015, i4(7, '<Non-CO2 name="SO2">'), i4(8, '<mac-control name="m">'), ..., i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))))
  mr <- function(t, v) lf(9, "mac-reduction", v, paste0(' tax="', t, '"'))
  mrw <- function(...) macw(i4(7, '<Non-CO2 name="SO2" nocreate="1">'), i4(8, '<mac-control name="m" nocreate="1">'), ..., i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))
  check("add-on: tax 10.0 then tax 10 in the override, tax 10 in the base file with the same last value: nothing written",
        identical(aw(macv(mr("10", 0.3)), macv(mr("10.0", 0.5), mr("10", 0.3))), "<scenario></scenario>"))
  check("add-on: tax 1e+01 in the override for tax 10 in the base file, same value: nothing written", identical(aw(macv(mr("10", 0.3)), macv(mr("1e+01", 0.3))), "<scenario></scenario>"))
  check("add-on: tax 10.0 with a new value for tax 10: written as the override has it", identical(aw(macv(mr("10", 0.3)), macv(mr("10.0", 0.4))), mrw(mr("10.0", 0.4))))
  check("refuses: tax that is not a plain decimal number", { m <- aw(macv(mr("10", 0.3)), macv(mr("0x10", 0.3))); has(m, "mac-control[m]/mac-reduction: tax missing or not a plain decimal number") && never(m) })
  check("refuses: mac-reduction without its tax attribute (GCAM reads tax 0)", { m <- aw(macv(mr("10", 0.3)), macv(mr("10", 0.3), lf(9, "mac-reduction", 0.2))); has(m, "mac-reduction: tax missing or not a plain decimal number") && never(m) })
  check("refuses: allocation year 1700.0 (GCAM reads year 0 and skips it)", { m <- aw(luh(al(1700, 5)), luh(al("1700.0", 5))); has(m, "land-use-history/allocation: year missing or not a plain whole number above 0") && never(m) })
  check("add-on: allocation year 01700 is year 1700: nothing written", identical(aw(luh(al(1700, 5)), luh(al("01700", 5))), "<scenario></scenario>"))
  # MAC layout: created under one subresource element, nocreate under a second one
  rs2 <- function(v) doc(reg("USA", i4(3, '<resource name="coal">'),
                             i4(4, '<subresource name="c">'), i4(5, '<technology name="t">'), i4(6, '<period year="2015">'), i4(7, '<Non-CO2 name="SO2">'), i4(8, '<mac-control name="m">'),
                             lf(9, "mac-reduction", v, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"), i4(6, "</period>"), i4(5, "</technology>"), i4(4, "</subresource>"),
                             i4(4, '<subresource name="c" nocreate="1">'), i4(5, '<technology name="t" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<Non-CO2 name="SO2" nocreate="1">'),
                             i4(8, '<mac-control name="m" nocreate="1">'), lf(9, "tech-change", 0.01, ' year="2020"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"), i4(6, "</period>"), i4(5, "</technology>"),
                             i4(4, "</subresource>"), i4(3, "</resource>")))
  check("add-on: MAC layout over two subresource elements (created, then nocreate): the change, in wrappers of the first",
        identical(aw(rs2(0.3), rs2(0.4)), top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<resource name="coal" nocreate="1">'), i4(4, '<subresource name="c" nocreate="1">'),
                                               i4(5, '<technology nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<Non-CO2 name="SO2" nocreate="1">'), i4(8, '<mac-control name="m" nocreate="1">'),
                                               lf(9, "mac-reduction", 0.4, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"), i4(6, "</period>"), i4(5, "</technology>"),
                                               i4(4, "</subresource>"), i4(3, "</resource>"), i4(2, "</region>"), i4(1, "</world>"))))
  # long refusals: the list printed in full first, the error kept short; after 2000 places the rest is counted
  cap_msgs <- function(B, O) {                                    # error and messages of one addon_write call (warning.length 1000)
    k <<- k + 1L; f <- file.path(d, paste0(c("b", "o", "a"), k, ".xml"))
    writeBin(charToRaw(paste(B, collapse = "\n")), f[1]); writeBin(charToRaw(paste(O, collapse = "\n")), f[2])
    op <- options(warning.length = 1000); on.exit(options(op)); ms <- character(0)
    e <- tryCatch({ withCallingHandlers(addon_write(f[1], f[2], f[3]), message = function(m) { ms <<- c(ms, conditionMessage(m)); invokeRestart("muffleMessage") }); "" }, error = conditionMessage)
    list(e = e, m = paste(ms, collapse = "\n"))
  }
  ss <- sprintf("s%02d", 1:12)
  check("refuses: 12 places under warning.length 1000: every place and the reasons printed first, the error short with the first place", {
    r <- cap_msgs(doc(reg("USA", sec(unlist(lapply(ss, sbs))))), doc(reg("USA", sec())))
    all(vapply(ss, function(s) grepl(paste0("subsector[", s, "]: only in the base file (removal)"), r$m, fixed = TRUE), NA)) && has(r$m, "reasons:", "(12)") &&
      has(r$e, "12 place(s) cannot be written as an add-on (listed above)") && nchar(r$e, "bytes") <= 600 })
  check("refuses: more than 2000 places: the first 2000 listed, the rest counted, later blocks not compared", {
    rg <- function(n) reg(n, sec(unlist(lapply(sprintf("s%04d", 1:1100), sbs))))
    r <- cap_msgs(doc(c(rg("R1"), rg("R2"), rg("R3"))), doc(c(reg("R1", sec()), reg("R2", sec()), reg("R3", sec()))))
    has(r$e, "At least 2200 place(s) cannot be written as an add-on (blocks from region R3 on not compared)") && has(r$m, "  ... and 200 more") &&
      lengths(regmatches(r$m, gregexpr("]: only in the base file (removal)", r$m, fixed = TRUE))) == 2000L })
  # other readers: calc-base-price children, fields only a handler reads
  check("refuses: unknown child of calc-base-price (GCAM's calc-base-price reader ignores it)",
        { m <- aw(doc(reg("USA"), cb5), doc(reg("USA"), cbp(smap("a", "b"), lf(2, "note", "x")))); has(m, "calc-base-price/note: child that GCAM's calc-base-price reader ignores") && never(m) && !grepl("Unknown tag", m, fixed = TRUE) })
  nac <- function(at, v) doc(reg("USA", i4(3, "<nationalAccountContainer>"), i4(4, paste0('<nationalAccount year="1990"', at, ">")), lf(5, "savings-rate", v), i4(4, "</nationalAccount>"), i4(3, "</nationalAccountContainer>")))
  check("add-on: an attribute naming a field only GCAM's handler reads (savings-rate on nationalAccount) is ignored, as GCAM ignores it",
        identical(aw(nac("", 0.2), nac(' savings-rate="0.3"', 0.2)), "<scenario></scenario>"))
  check("refuses: override naming one input twice", refuses(get("addon_minaj", ns)("electricity.xml", list(a = 1, a = 2)), "override names a twice"))
  # rule lists
  check("add-on: both files clear the rules and keep the same rules from there: not written",
        identical(aw(usa(rule(2010, 2020), rule(2020, 2050, at = ' delete="1"')), usa(rule(2000, 2010), rule(2020, 2050, at = ' delete="1"'))), "<scenario></scenario>"))
  check("add-on: both files clear the rules, the override keeps more: its rules from its clear",
        identical(aw(usa(rule(2020, 2050, at = ' delete="1"')), usa(rule(2020, 2050, at = ' delete="1"'), rule(2050, 2100))), chain(c(rule(2020, 2050, at = ' delete="1"'), rule(2050, 2100)))))
  check("add-on: rule list only in the override: appended", identical(aw(usa(sw), usa(sw, rule(2020, 2050))), chain(rule(2020, 2050))))
  check("refuses: rule list emptied by the override (step 2)", { m <- aw(usa(rule(2020, 2050)), usa()); has(m, "interpolation-rule: rule list changed") && step2(m) })
  # values
  lar0 <- function(...) doc(reg("USA", i4(3, "<LandAllocatorRoot>"), ..., i4(4, '<landAllocation year="1975">1</landAllocation>'), i4(3, "</LandAllocatorRoot>")))
  check("refuses: value given as an attribute only in the base file (un-set; step 2)", { m <- aw(lar("root"), lar0()); has(m, "LandAllocatorRoot/name: value set only in the base file (un-set)") && step2(m) })
  check("add-on: the same value as an attribute in the base file and as an element in the override: not written", identical(aw(lar("root"), lar0(lf(4, "name", "root"))), "<scenario></scenario>"))
  check("refuses: value element with child elements",
        { m <- aw(doc(reg("USA", sec(lf(4, "output-unit", "EJ")))), doc(reg("USA", sec(i4(4, "<output-unit>"), lf(5, "x", 1), i4(4, "</output-unit>"))))); has(m, "supplysector[elec]/output-unit: value element with child elements") && never(m) })
  # size cap on what is read whole: a file in another layout, and one changed block
  cn <- reg("China", sec(rep(lf(4, "output-unit", "EJ"), 200)))
  ol <- function(v) paste0('<scenario><world><region name="USA"><supplysector name="elec"><output-unit>', v, '</output-unit></supplysector></region><region name="China"><supplysector name="elec">',
                           strrep("<output-unit>EJ</output-unit>", 200), "</supplysector></region></world></scenario>")
  check("refuses: files not laid out as gcamdata writes them, above option minaj.addon_parse_cap_mb (4 KB): both named",
        { m <- aw(ol("EJ"), ol("PJ"), minaj.addon_parse_cap_mb = 4096 / 2^20)
          has(m, paste0("b", k, ".xml and o", k, ".xml are not laid out as gcamdata writes them, and the larger file is over option minaj.addon_parse_cap_mb")) })
  check("refuses: only the override not laid out as gcamdata writes it, above the cap: only the override named",
        { m <- aw(doc(c(reg("USA", sec(lf(4, "output-unit", "EJ"))), cn)), ol("PJ"), minaj.addon_parse_cap_mb = 4096 / 2^20)
          has(m, paste0("addon_minaj: o", k, ".xml is not laid out as gcamdata writes it, and the larger file is over option minaj.addon_parse_cap_mb")) && !grepl(paste0("b", k, ".xml and"), m, fixed = TRUE) })
  check("add-on: the same content laid out as gcamdata writes it, same cap: blocks read one by one",
        identical(aw(doc(c(reg("USA", sec(lf(4, "output-unit", "EJ"))), cn)), doc(c(reg("USA", sec(lf(4, "output-unit", "PJ"))), cn)), minaj.addon_parse_cap_mb = 4096 / 2^20), os(lf(4, "output-unit", "PJ"))))
  check("refuses: one changed block above option minaj.addon_parse_cap_mb",
        has(aw(doc(c(reg("USA"), cn)), doc(c(reg("USA"), sub(">EJ<", ">PJ<", cn, fixed = TRUE))), minaj.addon_parse_cap_mb = 4096 / 2^20), "block region China of b", "is larger than option minaj.addon_parse_cap_mb"))
  check("refuses: one changed block above the cap only in the override: the override file is named",
        has(aw(doc(c(reg("USA"), reg("China", sec(lf(4, "output-unit", "EJ"))))), doc(c(reg("USA"), cn)), minaj.addon_parse_cap_mb = 4096 / 2^20), "block region China of o", "is larger than option minaj.addon_parse_cap_mb"))

  # one input written as tracking-non-energy-input, then minicam-non-energy-input (matched by name: the second parses onto the first's class)
  ne <- function(v, cr = lf(8, "capital-ratio", 0.5), ic = lf(8, "input-cost", v)) usa(tech("a", per(2015, i4(7, '<tracking-non-energy-input name="capital">'), cr, i4(7, "</tracking-non-energy-input>"),
                                                                                     i4(7, '<minicam-non-energy-input name="capital">'), ic, i4(7, "</minicam-non-energy-input>"))))
  m1 <- aw(ne(10), ne(11))
  check("add-on: input written as tracking- then minicam-non-energy-input (one input): the change in the second element's wrapper",
        identical(m1, chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<minicam-non-energy-input name="capital" nocreate="1">'),
                              lf(8, "input-cost", 11), i4(7, "</minicam-non-energy-input>"), i4(6, "</period>"), i4(5, "</technology>")))))
  m2 <- aw(ne(10), ne(10, ic = NULL))
  check("refuses: value of that input set only in the base file (step 2)", has(m2, "tracking-non-energy-input[capital]/input-cost: value set only in the base file (un-set)") && step2(m2))
  # subsector of the same name as a nesting-subsector (gas_trade.xml layout): merged into the nesting-subsector
  nst <- function(v) sec(i4(4, '<nesting-subsector name="d">'), i4(5, '<subsector name="d1"/>'), i4(4, "</nesting-subsector>"), sbs("d", lf(5, "share-weight", v, ' year="1975"')))
  m3 <- aw(doc(reg("USA", nst(1))), doc(reg("USA", i4(3, '<supplysector name="elec" delete="1"/>'), nst(2))))
  check("add-on: sector deleted and rebuilt by the override, a subsector merging into a nesting-subsector of its name: the delete and the rebuild",
        identical(m3, ow(i4(3, '<supplysector name="elec" delete="1"/>'), nst(2))))
  m4 <- aw(doc(reg("USA", nst(1))), doc(reg("USA", nst(2))))
  check("add-on: value in a subsector merging into a nesting-subsector of its name: written in the subsector's wrapper",
        identical(m4, os(i4(4, '<subsector name="d" nocreate="1">'), lf(5, "share-weight", 2, ' year="1975"'), i4(4, "</subsector>"))))
  # stub-technology, then technology of the same name (ag_capital_tracking.xml layout): the stub queues the technology's children
  sm2 <- function(b) c(tech("a", per(1975, lf(7, "share-weight", 1)), tag = "stub-technology"), tech("a", per(1975, lf(7, "capacity-factor", b))))
  m6 <- aw(usa(sm2(1)), usa(sm2(2)))
  check("add-on: stub-technology then technology of one name: the change in the technology's wrapper, the check passes",
        identical(m6, chain(c(i4(5, '<technology name="a" nocreate="1">'), per(1975, lf(7, "capacity-factor", 2)), i4(5, "</technology>")))))
  # value inside an entry without its key (s-curve-shutdown-decider without a name: GCAM never matches it to an existing entry)
  sd <- function(v) usa(tech("a", per(2015, i4(7, "<s-curve-shutdown-decider>"), lf(8, "half-life", v), i4(7, "</s-curve-shutdown-decider>"))))
  m9 <- aw(sd(25), sd(30))
  check("refuses: value changed inside an s-curve-shutdown-decider without a name", has(m9, "period[2015]/s-curve-shutdown-decider: list without name or year keys changed (not an extension)") && never(m9))
  # entry order: every entry whose order against another changes
  m8 <- aw(usa(cf(0.8), tech("b")), usa(tech("c"), tech("b"), cf(0.8)))
  check("add-on: named technologies in a new order: the move names each entry whose order changes", identical(m8, chain(tech("c"))) &&
          identical(last$moves, "scenario/world/region[USA]/supplysector[elec]/subsector[coal]/technology (3 of 3: c, b, a)"))
  # long refusals: the first place and its step-2 marker kept in the error when the full path of the override does not fit
  check("refuses: under warning.length 1000, header with the full path plus the first place too long: the error names the override by file name and keeps the first place", {
    nm <- strrep("s", 300); m <- aw(doc(reg("USA", sec(sbs(nm), sbs("t")))), doc(reg("USA", sec())), warning.length = 1000)
    has(m, paste0("subsector[", nm, "]: only in the base file (removal); config= can decide it from the configuration's earlier files")) && nchar(m, "bytes") <= 600 + nchar("REFUSED: ") })
  # temporary file next to the add-on: its name does not depend on the add-on's name (Windows paths stop at 260 characters unless long paths are enabled)
  lpn <- 200L - nchar(normalizePath(d)) - 1L                      # folder of 200 characters, add-on name of 54
  if (.Platform$OS.type != "windows" || lpn < 1L) cat("SKIP add-on path of 255 characters (not Windows, or the test folder has 199 characters or more)\n") else
    check("add-on: add-on path of 255 characters written, no temporary file left", {
      dd <- file.path(d, strrep("p", lpn)); dir.create(dd, showWarnings = FALSE); fa <- file.path(dd, paste0(strrep("a", 50), ".xml")); fb <- file.path(d, c("lb.xml", "lo.xml"))
      writeBin(charToRaw(paste(usa(cf(0.8)), collapse = "\n")), fb[1]); writeBin(charToRaw(paste(usa(cf(0.9)), collapse = "\n")), fb[2])
      op <- options(minaj.addon_max_ratio = Inf); r <- tryCatch(addon_write(fb[1], fb[2], fa), error = conditionMessage); options(op)
      is.list(r) && file.exists(fa) && !length(list.files(dd, "[.]part[.]xml$")) })
  lpq <- 240L - nchar(normalizePath(d)) - 1L                      # existing folder of 240 characters, file name of 29
  if (.Platform$OS.type != "windows" || lpq < 1L) cat("SKIP move_into_place with a path of 260 characters or more (not Windows, or the test folder has 239 characters or more)\n") else {
    dq <- file.path(d, strrep("q", lpq)); dir.create(dq, showWarnings = FALSE); tf <- tempfile(tmpdir = d); writeLines("x", tf); t0 <- proc.time()[[3]]
    m <- tryCatch({ get("move_into_place", ns)(tf, file.path(dq, paste0(strrep("a", 25), ".xml"))); "" }, error = conditionMessage); t1 <- proc.time()[[3]] - t0; unlink(tf)
    if (!nzchar(m)) cat("SKIP move_into_place with a path of 260 characters or more (long paths are enabled: the file was written)\n")
    else check("move_into_place: a destination of 260 characters or more in an existing folder is named as such, without the retry waits", dir.exists(dq) && has(m, "(path of 260 characters or more)") && t1 < 2)
  }

  rf <- file.path(xml_dir, "electricity.xml"); rf <- rf[file.exists(rf)]
  for (q in seq_along(rf)) {                                      # the tree's file, in gcamdata's layout (block split)
    f <- rf[q]; sfx <- if (q > 1L) q else ""
    b <- readBin(f, "raw", file.size(f)); s <- rawToChar(b); Encoding(s) <- "bytes"
    m <- gregexpr("<capacity-factor>[^<]*</capacity-factor>", s, useBytes = TRUE)
    regmatches(s, m) <- list(vapply(regmatches(s, m)[[1]], function(z) { v <- as.numeric(gsub("<[^>]*>", "", z)); paste0("<capacity-factor>", format(v * 1.1, digits = 10), "</capacity-factor>") }, ""))
    o <- file.path(d, paste0("electricity_cf", sfx, ".xml")); a <- file.path(d, paste0("electricity_cf", sfx, "_addon.xml")); writeBin(charToRaw(s), o)
    t0 <- proc.time(); r <- tryCatch(addon_write(f, o, a), error = function(e) conditionMessage(e)); t1 <- proc.time() - t0
    xb <- xml_tokens(f); xo <- xml_tokens(o); w <- xb$tag == "capacity-factor"; xa <- if (is.list(r)) xml_tokens(a)
    check(sprintf("add-on: electricity.xml of %s, capacity-factor x1.1: every changed value and nothing else (%.1f KB, %.1f%% of the override, %.1f s CPU)%s",
                  basename(dirname(dirname(dirname(f)))), if (is.list(r)) r$size / 1024 else NA, if (is.list(r)) 100 * r$size / file.size(o) else NA, t1[[1]], if (is.character(r)) paste(":", substr(r, 1, 300)) else ""),
          is.list(r) && sum(xa$tag == "capacity-factor") == sum(xb$text[w] != xo$text[w]) && r$n[["values"]] == sum(xb$text[w] != xo$text[w]) &&
            all(xa$tag[!is.na(xa$text) & xa$depth > 2] == "capacity-factor"))
  }

  # ---- config_components ----------------------------------------------------------------------------------------
  # A configuration read as GCAM reads it: sections and values, parse order, batch files, path checks, the base file and
  # the model years (model_time.cpp). Files: modeltime t<k>.xml, earlier files p<k>_<i>.xml, configurations c<k>.xml.
  config_components <- get("config_components", ns)
  k <- k + 1L; kc <- k; k <- k + 1L                               # c<kc + 1>.xml: a batch file
  ft <- file.path(d, paste0("t", kc, ".xml")); fb <- file.path(d, paste0("b", kc, ".xml")); fq <- file.path(d, paste0("c", kc + 1L, ".xml"))
  fp <- file.path(d, sprintf("p%d_%d.xml", kc, 1:4)); nb <- basename(fb); y7 <- c(1975L, 1990L, 2005L, 2010L, 2015L, 2020L, 2025L)
  writeLines(c("<scenario>", i4(1, "<modeltime>"), lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", 2005, ' time-step="5"'), lf(2, "end-year", 2025),
               lf(2, "final-calibration-year", 2021), i4(1, "</modeltime>"), "</scenario>"), ft)
  for (f in c(fb, fp[1:2])) writeLines("<scenario></scenario>", f)
  rt <- paste0("/", sub("^[A-Za-z]:/", "", normalizePath(d, "/")), "/", basename(fp[4]))   # starts with /: read from the configuration's drive
  writeLines(c("<BatchRunner>", i4(1, '<ComponentSet name="base">'), i4(2, '<FileSet name="GCAM_">'), sprintf('            <Value name="climate">%s</Value>', basename(fp[2])),
               sprintf('            <Value name="elec_water_base">%s</Value>', nb), i4(2, "</FileSet>"), i4(1, "</ComponentSet>"),
               i4(1, "<runner-set>"), sprintf("        <Value>%s</Value>", basename(fp[1])), i4(1, "</runner-set>"), "</BatchRunner>"), fq)
  c1 <- c('<?xml version="1.0" encoding="UTF-8"?>', "<Configuration>", i4(1, "<Files>"), sprintf('        <Value name="xmlInputFileName">%s</Value>', nb),
          sprintf('        <Value name="BatchFileName">%s</Value>', basename(fq)), i4(1, "</Files>"), i4(1, "<ScenarioComponents>"),
          sprintf('        <Value name = "climate">%s</Value>', basename(fp[1])), sprintf('        <Files name="resources">%s</Files>', basename(fp[2])), i4(1, "</ScenarioComponents>"),
          i4(1, "<Strings>"), '        <Value name="scenarioName"><Value>x</Value></Value>', i4(1, "</Strings>"),
          sprintf('    <!-- <Value name="climate">%s</Value> -->', nb), i4(1, "<ScenarioComponents>"), sprintf('        <Value name="climate">%s</Value>', basename(fp[1])),
          sprintf('        <Value name="energy_supply">%s</Value>', basename(fp[3])), sprintf('        <Value name="elec_water_base">%s</Value>', nb),
          sprintf('        <Value name="heat">%s</Value>', rt), i4(1, "</ScenarioComponents>"),
          i4(1, "<Files>"), sprintf('        <Value name="xmlInputFileName">%s</Value>', basename(ft)), i4(1, "</Files>"),
          i4(1, "<Bools>"), '        <Value name="BatchMode">1</Value>', '        <Value name="BatchMode">true</Value>', i4(1, "</Bools>"), "</Configuration>")
  k <- k + 1L; fc <- file.path(d, paste0("c", k, ".xml")); writeLines(c1, fc)
  v <- config_components(fc, nb); tx <- c(basename(c(ft, fp[1:2], fp[1], fp[3], fb)), rt)
  check("config_components: xmlInputFileName (last value wins), then every ScenarioComponents value in document order across two sections, any tag, duplicates and missing files kept",
        identical(v$text, tx) && identical(v$path, normalizePath(file.path(d, basename(c(ft, fp[1:2], fp[1], fp[3], fb, fp[4]))), "/", mustWork = FALSE)) && identical(v$b, 6L))
  check("config_components: model years from modeltime; BatchMode \"true\" is not batch mode (lexical_cast<bool>: ^[+]?0*1$); value with child elements in another section ignored",
        identical(v$years, y7) && identical(v$note, "") && identical(v$batch, character(0)))
  check("config_components: a path starting with / is read from the configuration's drive", identical(v$path[7], normalizePath(fp[4], "/", mustWork = FALSE)) && grepl("^[A-Za-z]:/", v$path[7]) == (.Platform$OS.type == "windows"))
  if (.Platform$OS.type == "windows") check("config_components: base file name compared without case on Windows", identical(config_components(fc, toupper(nb))$b, 6L))
  k <- k + 1L; fc <- file.path(d, paste0("c", k, ".xml")); writeLines(sub(">true<", ">1<", c1, fixed = TRUE), fc)
  check("config_components: batch mode: the batch file's ComponentSet/FileSet/Value files, resolved from the configuration's folder (runner-set files left out)",
        identical(config_components(fc)$batch, normalizePath(file.path(d, c(basename(fp[2]), nb)), "/")))
  check("config_components refuses: base file also in the batch file", refuses(config_components(fc, nb), paste0(nb, " is also loaded by the batch file ", normalizePath(fq, "/"), ", after the add-on")))
  k <- k + 1L; f <- file.path(d, paste0("c", k, ".xml")); writeLines(sub(">true<", ">1<", c1[c1 != sprintf('        <Value name="elec_water_base">%s</Value>', nb)], fixed = TRUE), f)
  check("config_components refuses: base file loaded only through the batch file", refuses(config_components(f, nb), paste0(basename(f), " loads ", nb, " only through its batch file ",
                                                                                                                         normalizePath(fq, "/"), "; addon_minaj needs the base file in ScenarioComponents")))
  bmv <- c("01" = TRUE, "+1" = TRUE, "001" = TRUE, "-1" = FALSE, " 1" = FALSE, "10" = FALSE)
  bmr <- vapply(names(bmv), function(z) { k <<- k + 1L; f <- file.path(d, paste0("c", k, ".xml")); writeLines(sub(">true<", paste0(">", z, "<"), c1, fixed = TRUE), f)
    length(config_components(f)$batch) > 0L }, NA)
  check(paste0("config_components: BatchMode ", paste(sprintf('"%s" %s', names(bmv), ifelse(bmv, "is", "is not")), collapse = ", "), " batch mode (lexical_cast<bool>)"), identical(unname(bmr), unname(bmv)))
  for (z in c("<Value>x</Value>", "")) {
    c2 <- sub(paste0(">", basename(fq), "<"), paste0(">", z, "<"), c1, fixed = TRUE)
    k <- k + 1L; fc <- file.path(d, paste0("c", k, ".xml")); writeLines(c2, fc); k <- k + 1L; f <- file.path(d, paste0("c", k, ".xml")); writeLines(sub(">true<", ">1<", c2, fixed = TRUE), f)
    check(sprintf("config_components: BatchFileName %s: read only in batch mode, refused there", if (nzchar(z)) "with child elements" else "empty"),
          identical(config_components(fc, nb)$b, 6L) && refuses(config_components(f, nb), paste0('Files value "BatchFileName": ', if (nzchar(z)) "configuration value with child elements" else "the path is empty")))
  }
  k <- k + 1L; fc <- file.path(d, paste0("c", k, ".xml"))
  writeLines(c("<Configuration>", i4(1, "<Files>"), sprintf('        <Value name="xmlInputFileName">%s</Value>', basename(ft)), i4(1, "</Files>"), i4(1, "<ScenarioComponents>"),
               '        <Value name="a">//fileserver/share/p.xml</Value>', '        <Value name="b">\\\\fileserver\\share\\q.xml</Value>',
               sprintf('        <Value name="elec_water_base">%s</Value>', nb), i4(1, "</ScenarioComponents>"), "</Configuration>"), fc)
  check("config_components: a path starting with // or \\\\ (UNC) is absolute, kept as written with / for \\",
        identical(config_components(fc, nb)$path[2:3], c("//fileserver/share/p.xml", "//fileserver/share/q.xml")))
  k <- k + 1L; fc <- file.path(d, paste0("c", k, ".xml")); writeLines(sub(basename(fq), basename(fp[3]), sub(">true<", ">1<", c1, fixed = TRUE), fixed = TRUE), fc)
  check("config_components: batch file missing: refused with the base file named, listed without batch files when no base file is given",
        refuses(config_components(fc, nb), paste0("runs in batch mode, and minaj cannot read its batch file the way GCAM does")) && identical(config_components(fc)$batch, character(0)))
  fs <- c(i4(1, "<Files>"), sprintf('        <Value name="xmlInputFileName">%s</Value>', basename(ft)), i4(1, "</Files>"))
  sc <- c(i4(1, "<ScenarioComponents>"), sprintf('        <Value name="elec_water_base">%s</Value>', nb), i4(1, "</ScenarioComponents>"))
  rc <- list(list(c(fs, sub(nb, paste0(" ", basename(fp[1])), sc, fixed = TRUE), sc), "GCAM would open the path with the surrounding spaces"),
             list(c(fs, sub(nb, paste0(basename(fp[1]), " "), sc, fixed = TRUE), sc), paste0('spaces ("', basename(fp[1]), ' ")')),
             list(c(fs, sub(nb, paste0(basename(fp[1]), "\n        "), sc, fixed = TRUE), sc), paste0('the surrounding spaces ("', basename(fp[1]))),
             list(c(fs, sub(nb, "p&amp;1.xml", sc, fixed = TRUE), sc), "entities are not translated by GCAM"),
             list(c(fs, sub(nb, paste0("C:", basename(fp[1])), sc, fixed = TRUE), sc), "a drive-relative path"),
             list(c(fs, sub(nb, "", sc, fixed = TRUE), sc), 'ScenarioComponents value "elec_water_base": the path is empty'),
             list(c(fs, sub(nb, paste0("<Value>", basename(fp[1]), "</Value>"), sc, fixed = TRUE), sc), 'ScenarioComponents value "elec_water_base": configuration value with child elements'),
             list(c(i4(1, "<Files>"), '        <Value name="xmlInputFileName"><Value>x</Value></Value>', i4(1, "</Files>"), sc), 'Files value "xmlInputFileName": configuration value with child elements'),
             list(c(i4(1, "<Bools>"), '        <Value name="BatchMode"></Value>', i4(1, "</Bools>"), fs, sc), 'Bools value "BatchMode": configuration value is empty'),
             list(sc, "has no Files value xmlInputFileName"),
             list(c(fs, sc, sc), paste0(" loads ", nb, " 2 times; it must load the base file exactly once")),
             list(c(fs, sub(nb, basename(fp[1]), sc, fixed = TRUE)), paste0(" loads ", nb, " 0 times")),
             list(c(i4(1, "<Files>"), sprintf('        <Value name="xmlInputFileName">%s</Value>', nb), i4(1, "</Files>")),
                  paste0(": the base file is the configuration's xmlInputFileName; load b", kc, "_override.xml there instead")),
             list(c(fs, "    <ScenarioComponents name='x'/>", sc), "the way GCAM does: "))
  for (x in rc) {
    k <- k + 1L; fc <- file.path(d, paste0("c", k, ".xml")); writeLines(c("<Configuration>", x[[1]], "</Configuration>"), fc)
    check(paste0("config_components refuses: ", sub("^[: ]+", "", x[[2]])), refuses(config_components(fc, nb), x[[2]]) && refuses(config_components(fc, nb), basename(fc)))
  }
  # model years: model_time.cpp:93-151 and 203-264, the C++ message when GCAM would stop
  mtc <- list("time step that does not divide the gap" = list(c(lf(2, "start-year", 1975, ' time-step="20"'), lf(2, "end-year", 2025)), "Specified time step of 20 does not evenly divide years from 1975 to 2025"),
              "time step 0" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", 2005, ' time-step="0"'), lf(2, "end-year", 2025)), "GCAM stops: time step 0 from 2005 to 2025"),
              "no time-step attribute (read as 0)" = list(c(lf(2, "start-year", 1975), lf(2, "end-year", 2025)), "GCAM stops: time step 0 from 1975 to 2025"),
              "negative time step that divides the gap" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", 2005, ' time-step="-5"'), lf(2, "end-year", 2025)),
                                                               "time step of -5 from 2005 to 2025 is negative; GCAM leaves out the years from 2005 up to 2025"),
              "negative time step that does not divide the gap" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", 2005, ' time-step="-15"'), lf(2, "end-year", 2025)),
                                                                       "Specified time step of -15 does not evenly divide years from 2005 to 2025"),
              "end-year written twice: the first keeps step -1" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", 2005, ' time-step="5"'), lf(2, "end-year", 2015), lf(2, "end-year", 2025)),
                                                                     "time step of -1 from 2015 to 2025 is negative"),
              "final-calibration-year before the start year" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "end-year", 2025), lf(2, "final-calibration-year", 1970)), "Parsed invalid final calibration year: 1970"),
              "single-quoted attribute (minaj cannot read it)" = list(c("        <start-year time-step='15'>1975</start-year>", lf(2, "end-year", 2025)), "minaj cannot read t"),
              "inter-year +2005 read as 2005" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", "+2005", ' time-step="5"'), lf(2, "end-year", 2025)), y7),
              "inter-year \" 2005\" read as 0" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", " 2005", ' time-step="5"'), lf(2, "end-year", 2025)), "Parsed invalid year: 0 which is before the start year: 1975"),
              "inter-year written twice: the later time step" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", 2005, ' time-step="10"'), lf(2, "inter-year", 2005, ' time-step="5"'), lf(2, "end-year", 2025)), y7),
              "final-calibration-year after the end year" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "end-year", 2025), lf(2, "final-calibration-year", 2030)), "Parsed invalid final calibration year: 2030"),
              "no final-calibration-year: 2021" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", 2005, ' time-step="5"'), lf(2, "end-year", 2020)), "Parsed invalid final calibration year: 2021"),
              "no end-year" = list(c(lf(2, "start-year", 1975, ' time-step="15"'), lf(2, "inter-year", 2005, ' time-step="5"')), "No end year information has been parsed in modeltime"),
              "no <modeltime>" = list(NULL, "no <modeltime> in t"))
  for (q in names(mtc)) {
    x <- mtc[[q]]; k <- k + 1L; f <- file.path(d, paste0(c("t", "c"), k, ".xml"))
    writeLines(if (is.null(x[[1]])) c("<scenario>", i4(1, "<world/>"), "</scenario>") else c("<scenario>", i4(1, "<modeltime>"), x[[1]], i4(1, "</modeltime>"), "</scenario>"), f[1])
    writeLines(c("<Configuration>", sub(basename(ft), basename(f[1]), fs, fixed = TRUE), sc, "</Configuration>"), f[2]); v <- config_components(f[2], nb)
    check(paste0("config_components: model years, ", q, ": ", if (is.character(x[[2]])) x[[2]] else paste(x[[2]], collapse = " ")),
          if (is.character(x[[2]])) is.null(v$years) && startsWith(v$note, x[[2]]) else identical(v$years, x[[2]]) && identical(v$note, ""))
  }
  k <- k + 1L; fc <- file.path(d, paste0("c", k, ".xml")); writeLines(c("<Configuration>", sub(basename(ft), basename(fp[3]), fs, fixed = TRUE), sc, "</Configuration>"), fc)
  check("config_components: xmlInputFileName missing: no model years, the note names it", { v <- config_components(fc, nb); is.null(v$years) && identical(v$note, paste(normalizePath(fp[3], "/", mustWork = FALSE), "does not exist")) })
  vx <- file.path(dirname(dirname(dirname(xml_dir))), "exe", paste0("configuration_", c("ref", "ssp"), ".xml"))
  if (!all(file.exists(vx))) cat("SKIP config_components on the vanilla configurations (", vx[1], " not found)\n", sep = "") else {
    v <- config_components(vx[1], "electricity_water.xml"); xd <- xml2::read_xml(vx[1])
    tx <- c(utils::tail(xml2::xml_text(xml2::xml_find_all(xd, "/*/Files/*[@name='xmlInputFileName']")), 1), xml2::xml_text(xml2::xml_find_all(xd, "/*/ScenarioComponents/*")))
    check(sprintf("config_components: %s: %d files in xml2's order, the base file, the absent solver file kept, vanilla model years", basename(vx[1]), length(tx)),
          identical(v$text, tx) && identical(v$path, normalizePath(file.path(dirname(vx[1]), tx), "/", mustWork = FALSE)) && basename(v$path[v$b]) == "electricity_water.xml" &&
            "cal_broyden_config.xml" %in% basename(v$path[!file.exists(v$path)]) && !length(v$batch) &&
            identical(v$years, c(1975L, 1990L, 2005L, 2010L, 2015L, 2021L, seq(2025L, 2100L, 5L))))
    xs <- xml2::read_xml(vx[2]); bf <- file.path(dirname(vx[2]), xml2::xml_text(xml2::xml_find_all(xs, "/*/Files/*[@name='BatchFileName']")))
    bt <- xml2::xml_text(xml2::xml_find_all(xml2::read_xml(bf), "/*/ComponentSet/FileSet/Value")); v <- config_components(vx[2])
    check(sprintf("config_components: %s (batch mode): no components, the %d files of %s", basename(vx[2]), length(bt), basename(bf)),
          length(v$path) == 1L && identical(v$batch, normalizePath(file.path(dirname(vx[2]), bt), "/", mustWork = FALSE)))
    check(sprintf("config_components refuses: %s loads electricity_water.xml only through its batch file", basename(vx[2])),
          refuses(config_components(vx[2], "electricity_water.xml"), "loads electricity_water.xml only through its batch file"))
  }
  # addon_write with config: a synthetic configuration case; add-on body, or "REFUSED: <message>"
  cfg <- function(P, B, O, years = c(1975, 1990, 2005, 2010, 2015, 2020, 2025)) {
    k <<- k + 1L; f <- file.path(d, paste0(c("t", "b", "o", "a", "c"), k, ".xml")); fp <- file.path(d, sprintf("p%d_%d.xml", k, seq_along(P)))
    n <- length(years); s <- diff(years); i <- which(c(TRUE, s[-1] != s[-length(s)]))   # start-year, then an inter-year where the step changes
    writeLines(if (is.null(years)) "<scenario></scenario>" else c("<scenario>", i4(1, "<modeltime>"), lf(2, "start-year", years[1], sprintf(' time-step="%d"', s[1])),
                                                                  if (length(i) > 1L) lf(2, "inter-year", years[i[-1]], sprintf(' time-step="%d"', s[i[-1]])), lf(2, "end-year", years[n]),
                                                                  lf(2, "final-calibration-year", min(max(2021, years[1]), years[n])), i4(1, "</modeltime>"), "</scenario>"), f[1])
    for (j in seq_along(P)) writeBin(charToRaw(paste(P[[j]], collapse = "\n")), fp[j])
    writeBin(charToRaw(paste(B, collapse = "\n")), f[2]); writeBin(charToRaw(paste(O, collapse = "\n")), f[3])
    writeLines(c("<Configuration>", i4(1, "<Files>"), sprintf('        <Value name="xmlInputFileName">%s</Value>', basename(f[1])), i4(1, "</Files>"), i4(1, "<ScenarioComponents>"),
                 sprintf('        <Value name="%s">%s</Value>', sub("[.]xml$", "", basename(c(fp, f[2]))), basename(c(fp, f[2]))), i4(1, "</ScenarioComponents>"), "</Configuration>"), f[5])
    op <- options(minaj.addon_max_ratio = Inf); on.exit(options(op))
    last <<- tryCatch(addon_write(f[2], f[3], f[4], "energy/A23.globaltech_capacity_factor", config = f[5]), error = function(e) conditionMessage(e))
    if (is.character(last)) paste("REFUSED:", last) else paste(readLines(f[4])[-(1:2)], collapse = "\n")
  }
  check("add-on with config: no earlier-file answer needed, the same add-on as without config", identical(cfg(list(usa(cf(0.5))), usa(cf(0.8)), usa(cf(0.88))), aw(usa(cf(0.8)), usa(cf(0.88)))))
  check("addon_write with config: the configured base file differs from the base file given: internal error, nothing written", {
    m <- cfg(list(usa(cf(0.5))), usa(cf(0.8)), usa(cf(0.88))); f <- file.path(d, paste0(c("b", "o", "a", "c"), k, ".xml")); fo <- file.path(out_dir, basename(f[1]))
    writeLines(usa(cf(0.7)), fo); unlink(f[3]); r <- tryCatch({ addon_write(fo, f[2], f[3], config = f[4]); "" }, error = conditionMessage)
    !startsWith(m, "REFUSED") && has(r, "addon_minaj: internal error, no add-on written; the base file ", basename(f[4]), " loads (", normalizePath(f[1], "/"), ") differs from ") && !file.exists(f[3]) })

  # ---- prefix_scan ----------------------------------------------------------------------------------------------
  # Answers about the files a configuration loads before the base file (P): each question kind, each cause of an unknown
  # answer, the 'xmlscan' records and the chunked block index. Store and state folder: temporary folders. In the expected
  # values "#" stands for "p<k>_" (the earlier files of the case).
  prefix_scan <- get("prefix_scan", ns); nm <- get(".nm", ns); s0 <- list(nm$store, nm$state_dir, nm$findex)
  nm$store <- tempfile(tmpdir = out_dir); nm$state_dir <- tempfile(tmpdir = file.path(nm$store, "trees")); nm$findex <- NULL; dir.create(nm$state_dir, recursive = TRUE, showWarnings = FALSE)
  pm <- sub(";.*", "", rules$source)[match(c("World\rregion", "RegionMiniCAM\rsupplysector", "SupplySector\rsubsector", "Subsector\rtechnology", "TechnologyContainer<DefaultTechnology>\rperiod",
                                             "DefaultTechnology\rminicam-energy-input", "DefaultTechnology\rNon-CO2", "NonCO2Emissions\rmac-control", "RegionMiniCAM\rresource", "Resource\rsubresource",
                                             "SubResource\rgrade", "World\rglobal-technology-database", "GlobalTechnologyDatabase\rlocation-info", "location-info\rtechnology"), rk)]
  pU <- paste0(pm[1], "\rUSA"); pS <- paste0(pU, "\n", pm[2], "\relec"); pC <- paste0(pS, "\n", pm[3], "\rcoal"); pA <- paste0(pC, "\n", pm[4], "\ra"); pP <- paste0(pA, "\n", pm[5], "\r2015")
  pI <- paste0(pP, "\n", pm[6], "\rcoal"); pN <- paste0(pP, "\n", pm[7], "\rSO2\n", pm[8], "\rm"); pR <- paste0(pU, "\n", pm[9], "\rcoal\n", pm[10], "\rc"); pG <- paste0(pm[12], "\r")
  pL <- paste0(pG, "\n", pm[13], "\relec\rcoal\n", pm[14], "\ra\n", pm[5], "\r2015")
  b0 <- usa(cf(0.8)); sk <- i4(4, '<subsector name="coal" delete="1"/>'); kyt <- doc(reg("USA", c(i4(3, '<supplysector name="elec">'), i4(4, "<keyword#/>"), i4(3, "</supplysector>"))))
  r20 <- rule(2020, 2050); r26 <- rule(2020, 2060, at = ' delete="1"'); r61 <- rule(2060, 2100); r2s <- sub("share-weight", "share.weight", r26, fixed = TRUE)
  pa <- c(i4(5, '<technology name="a">'), i4(6, '<period year="2015" capacity-factor="0.6">'), i4(6, "</period>"), i4(5, "</technology>"))
  cm1 <- doc(c("<!--", reg("USA", sec(sbs("coal", cf(0.4)))), "-->", reg("China", sec(lf(4, "output-unit", "EJ")))))   # a region commented out before a live one
  cm2 <- doc(c(reg("USA", sec(lf(4, "output-unit", "EJ")), "<!--"), reg("China", sec(sbs("coal", cf(0.4)))), reg("USA", "-->")))   # a comment from one USA block to the next, around China
  cm3 <- c('<?xml version="1.0" encoding="UTF-8"?>', "<!--", "<scenario>", i4(1, "<world>"), reg("USA", sec(sbs("coal", cf(0.4)))), "--><scenario><world>",   # a comment opened before <scenario>
           reg("China", sec(lf(4, "output-unit", "EJ"))), i4(1, "</world>"), "</scenario>", "")
  cmy <- doc(c(i4(2, "<!-- <year>2015</year> <name>x</name> -->"), reg("USA", sec(sbs("coal", cf(0.4))))))   # name and year tags inside a comment
  cases <- list(
    list("touched, never created (no supplysector in P): untouched", list(doc(reg("USA"))), paste0("touched\t", pP, "\tcapacity-factor"), "untouched", NULL),
    list("touched, nocreate only on the chain (never created): untouched", list(chain(cf(0.7))), paste0("touched\t", pP, "\tcapacity-factor"), "untouched", NULL),
    list("touched, only nocreate elements on the holder's technology (never created): untouched", list(usa(cf(0.7, at = ' nocreate="1"'))), paste0("touched\t", pP, "\tcapacity-factor"), "untouched", NULL),
    list("touched, SIMPLE: the last write and its file", list(usa(cf(0.5)), usa(cf(0.7))), paste0("touched\t", pP, "\tcapacity-factor"), "touched", c("#2.xml" = "0.7")),
    list("touched, SIMPLE given as an attribute (bytes as written)", list(usa(pa)), paste0("touched\t", pP, "\tcapacity-factor"), "touched", c("#1.xml" = "0.6")),
    list("touched, ARRAY: the writes in order (year, fillout, value); a write without a year sets nothing", list(usa(sw), usa(lf(5, "share-weight", 0.5, ' year="2025"'), lf(5, "share-weight", 9))),
         paste0("touched\t", pC, "\tshare-weight"), "touched", list(y = c(1975, 2025), f = c(TRUE, FALSE), v = c("1", "0.5"))),
    list("touched, REPLACE: the last coefficient or efficiency element, raw", list(inp(lf(8, "coefficient", 2), lf(8, "efficiency", 3))), paste0("touched\t", pI, "\tcoefficient"), "touched", c("#1.xml" = "<efficiency>3</efficiency>")),
    list("touched, SETTRUE", list(macv(i4(9, "<no-zero-cost-reductions/>"))), paste0("touched\t", pN, "\tno-zero-cost-reductions"), "touched", TRUE),
    list("touched, OR: the set of flags", list(inp(lf(8, "flag", "Energy")), inp(lf(8, "flag", "Resource"), lf(8, "flag", "Energy"))), paste0("touched\t", pI, "\tflag"), "touched", c("Energy", "Resource")),
    list("touched, global technology database: location-info and technology", list(doc(gtd(0.8))), paste0("touched\t", pL, "\tcapacity-factor"), "touched", c("#1.xml" = "0.8")),
    list("exists: never created: FALSE", list(usa(cf(0.5))), paste0("exists\t", pC, "\n", pm[4], "\rb"), "untouched", FALSE),
    list("creator: never created: untouched, no creator", list(usa(cf(0.5))), paste0("creator\t", pC, "\n", pm[4], "\rb"), "untouched", NULL),
    list("siblings: the technologies of a subsector", list(usa(cf(0.5))), paste0("siblings\t", pC, "\t", pm[4]), "touched", "a"),
    list("rules: none written: untouched", list(usa(cf(0.5))), paste0("rules\t", pC), "untouched", NULL),
    list("keymap: holder never created: untouched", list(usa(cf(0.5))), paste0("keymap\t", pN, "\tmac-reduction"), "untouched", NULL),
    list("exists: created: TRUE", list(doc(reg("China")), usa(cf(0.5))), paste0("exists\t", pA), "touched", TRUE),
    list("exists: the global technology database exists once <world> does", list(doc(reg("China"))), paste0("exists\t", pG), "touched", TRUE),
    list("exists: the global technology database without <world>: FALSE", list("<scenario></scenario>"), paste0("exists\t", pG), "untouched", FALSE),
    list("creator: the class of the first creating element; a later tag of the family parses onto it", list(usa(tech("a", per(2015, lf(7, "capacity-factor", 1)), tag = "intermittent-technology")), usa(cf(2))),
         paste0("creator\t", pP), "touched", setNames(paste0(c("RegionMiniCAM", "SupplySector", "Subsector", "TechnologyContainer<IntermittentTechnology>", "IntermittentTechnology"), "\t#1.xml"), strsplit(pP, "\n")[[1]])),
    list("creator: a vintage a later file creates takes that file's technology tag (DefaultTechnology inside an intermittent-technology)",
         list(usa(tech("a", per(2015, lf(7, "capacity-factor", 1)), tag = "intermittent-technology")), usa(tech("a", per(2020, lf(7, "capacity-factor", 2))))),
         paste0("creator\t", pA, "\n", pm[5], "\r2020"), "touched", setNames(paste0(c("RegionMiniCAM", "SupplySector", "Subsector", "TechnologyContainer<IntermittentTechnology>", "DefaultTechnology"),
                                                                                    c(rep("\t#1.xml", 4), "\t#2.xml")), c(strsplit(pA, "\n")[[1]], paste0(pm[5], "\r2020")))),
    list("rules: from the last clear on (clear included), raw", list(usa(r20), usa(r26, r61)), paste0("rules\t", pC), "touched",
         c("#2.xml" = paste(c(sub("^ +", "", r26[1]), r26[-1]), collapse = "\n"), "#2.xml" = paste(c(sub("^ +", "", r61[1]), r61[-1]), collapse = "\n"))),
    list("rules: a delete rule with apply-to=\"share.weight\" does not clear", list(usa(r20), usa(r2s)), paste0("rules\t", pC), "touched",
         c("#1.xml" = paste(c(sub("^ +", "", r20[1]), r20[-1]), collapse = "\n"), "#2.xml" = paste(c(sub("^ +", "", r2s[1]), r2s[-1]), collapse = "\n"))),
    list("stub: a childless stub-technology without flags: untouched, created", list(usa(i4(5, '<stub-technology name="a"/>'))), paste0("stub\t", pA), "untouched", "created"),
    list("stub: nodes queued", list(usa(cf(0.5, tag = "stub-technology"))), paste0("stub\t", pA), "touched", "#1.xml"),
    list("stub: never written: untouched", list(usa(cf(0.5)), usa()), paste0("stub\t", pC, "\n", pm[4], "\rz"), "untouched", NULL),
    list("keywords: the first value of each key stays", list(sub("#", ' a="x"', kyt, fixed = TRUE), sub("#", ' a="y" b="z"', kyt, fixed = TRUE)), paste0("keywords\t", pS), "touched", c(a = "x", b = "z")),
    list("keymap: the last write per key as GCAM reads it (tax 10 and 10.0 are one key)", list(macv(mr("10", 0.3)), macv(mr("10.0", 0.5), mr("20", 0.1))), paste0("keymap\t", pN, "\tmac-reduction"), "touched",
         c("0.1e2" = '<mac-reduction tax="10.0">0.5</mac-reduction>', "0.2e2" = '<mac-reduction tax="20">0.1</mac-reduction>')),
    list("keymap: keys in the form addon_write gives them (1e1 and 10 one key; 0.50, -0.25, 0.0)", list(macv(mr("1e1", 0.3), mr("0.50", 0.1)), macv(mr("-0.25", 0.2), mr("0.0", 0.4), mr("10", 0.6))),
         paste0("keymap\t", pN, "\tmac-reduction"), "touched", c("0.5e0" = '<mac-reduction tax="0.50">0.1</mac-reduction>', "-0.25e0" = '<mac-reduction tax="-0.25">0.2</mac-reduction>',
                                                                 "0" = '<mac-reduction tax="0.0">0.4</mac-reduction>', "0.1e2" = '<mac-reduction tax="10">0.6</mac-reduction>')),
    list("keywords: none written: untouched", list(usa(cf(0.5))), paste0("keywords\t", pS), "untouched", NULL),
    list("siblings: none written: untouched", list(usa(sw)), paste0("siblings\t", pC, "\t", pm[4]), "untouched", character(0)),
    list("touched container, created but not reached: untouched", list(usa(cf(0.5))), paste0("touched\t", pC, "\n", pm[4], "\rb"), "untouched", NULL),
    list("siblings, global technology database: flags on location-info not read, an entry needs a technology, byte order",
         list(doc(c(i4(2, "<global-technology-database>"), unlist(lapply(list(c("elec", "coal", ""), c("biomass", "y", ""), c("Elec", "x", ""), c("elec", "coal", ' delete="1"'), c("gas", "z", ' nocreate="1"'), c("oil", "w", "")), function(v)
           c(i4(3, sprintf('<location-info sector-name="%s" subsector-name="%s"%s>', v[1], v[2], v[3])), if (v[1] != "oil") c(i4(4, '<technology name="a">'), i4(4, "</technology>")), i4(3, "</location-info>")))),
           i4(2, "</global-technology-database>")))), paste0("siblings\t", pG, "\t", pm[13]), "touched", c("Elec\rx", "biomass\ry", "elec\rcoal", "gas\rz")),
    list("exists at a stub-technology: TRUE", list(usa(i4(5, '<stub-technology name="a"/>'))), paste0("exists\t", pA), "touched", TRUE),
    list("creator at a stub-technology", list(usa(i4(5, '<stub-technology name="a"/>'))), paste0("creator\t", pA), "touched",
         setNames(paste0(c("RegionMiniCAM", "SupplySector", "Subsector", "StubTechnologyContainer"), "\t#1.xml"), strsplit(pA, "\n")[[1]])),
    list("a region name with spaces around '=' (read whole)", list(sub('<region name="USA">', '<region name = "USA">', usa(cf(0.7)), fixed = TRUE)), paste0("touched\t", pP, "\tcapacity-factor"), "touched", c("#1.xml" = "0.7")),
    list("a root element other than <scenario> (GCAM reads its <world>)", list(sub("scenario>", "Scenario>", usa(cf(0.7)), fixed = TRUE)), paste0("touched\t", pP, "\tcapacity-factor"), "touched", c("#1.xml" = "0.7")),
    list("a file indented by 2 spaces (read whole)", list(gsub("    ", "  ", usa(cf(0.7)), fixed = TRUE)), paste0("touched\t", pP, "\tcapacity-factor"), "touched", c("#1.xml" = "0.7")),
    list("unknown: a region name in single quotes", list(sub('<region name="USA">', "<region name='USA'>", usa(cf(0.7)), fixed = TRUE)), paste0("touched\t", pP, "\tcapacity-factor"), "unknown",
         "#1.xml: minaj cannot read it the way GCAM does"),
    list("unknown: another question at a stub-technology", list(usa(i4(5, '<stub-technology name="a"/>'))), paste0("touched\t", pA), "unknown",
         "#1.xml writes <stub-technology name=\"a\">; at a stub-technology only the stub, exists and creator questions are answered"),
    list("unknown: siblings with nocreate on the chain", list(usa(cf(0.5)), chain(tech("b", per(2015, lf(7, "capacity-factor", 1))))), paste0("siblings\t", pC, "\t", pm[4]), "unknown",
         "#2.xml writes <region name=\"USA\"> with nocreate=\"1\" on the way"),
    list("unknown: a sibling without its key", list(usa(i4(5, "<technology>"), i4(5, "</technology>"))), paste0("siblings\t", pC, "\t", pm[4]), "unknown", "#1.xml writes <technology> without a key GCAM matches"),
    list("siblings: vector order; a deleted and recreated grade moves to the end; delete or nocreate of a missing key is skipped",
         list(gr(g("g1", 1), g("g2", 2), g("g3", 3)), gr(i4(5, '<grade name="g2" delete="1"/>'), g("g2", 2), i4(5, '<grade name="g8" delete="1"/>'), i4(5, '<grade name="g9" nocreate="1"/>'))),
         paste0("siblings\t", pR, "\t", pm[11]), "touched", c("g1", "g3", "g2")),
    list("unknown: a delete on a prefix after the last write", list(usa(cf(0.5)), usa(i4(5, '<technology name="a" delete="1"/>'))), paste0("touched\t", pP, "\tcapacity-factor"), "unknown",
         "#2.xml writes <technology name=\"a\"> with delete=\"1\""),
    list("unknown: a delete before the last write (delete, then the subsector again): array", list(usa(sw), doc(reg("USA", sec(sk, sbs("coal", lf(5, "share-weight", 3, ' year="2010"')))))),
         paste0("touched\t", pC, "\tshare-weight"), "unknown", "#2.xml writes <subsector name=\"coal\"> with delete=\"1\""),
    list("unknown: a delete before the last write: rules", list(usa(rule(2020, 2050)), doc(reg("USA", sec(sk, sbs("coal", rule(2020, 2060)))))), paste0("rules\t", pC), "unknown", "with delete=\"1\""),
    list("unknown: a delete before the last write: keywords", list(sub("#", ' a="x"', kyt, fixed = TRUE), doc(reg("USA", c(i4(3, '<supplysector name="elec" delete="1"/>'), i4(3, '<supplysector name="elec">'), i4(4, '<keyword a="y"/>'), i4(3, "</supplysector>"))))),
         paste0("keywords\t", pS), "unknown", "#2.xml writes <supplysector name=\"elec\"> with delete=\"1\""),
    list("unknown: nocreate on a chain that reaches the slot", list(usa(cf(0.5)), chain(cf(0.7))), paste0("touched\t", pP, "\tcapacity-factor"), "unknown", "#2.xml writes <region name=\"USA\"> with nocreate=\"1\" on the way"),
    list("unknown: a pair the registry lacks inside the holder", list(usa(tech("a", per(2015, lf(7, "capacity-factor", 0.5), lf(7, "no-such-tag", 1))))), paste0("touched\t", pP, "\tcapacity-factor"), "unknown",
         "#1.xml: <no-such-tag> in DefaultTechnology is not in the registry"),
    list("unknown: a pair the registry lacks on the chain, with the step's key", list(usa(i4(5, '<bogus-technology name="a"/>'))), paste0("touched\t", pP, "\tcapacity-factor"), "unknown", "#1.xml: <bogus-technology> in Subsector is not in the registry"),
    list("unknown: a keyword attribute on the path (keywords)", list(doc(reg("USA", c(i4(3, '<supplysector name="elec" keyword="x">'), i4(3, "</supplysector>"))))), paste0("keywords\t", pS), "unknown", "writes a keyword attribute on <supplysector name=\"elec\">"),
    list("unknown: below a stub-technology", list(usa(cf(0.5, tag = "stub-technology"))), paste0("touched\t", pP, "\tcapacity-factor"), "unknown", "#1.xml writes <stub-technology name=\"a\"> on the way"),
    list("unknown: a SIMPLE write with child elements", list(usa(tech("a", per(2015, i4(7, "<capacity-factor>"), lf(8, "x", 1), i4(7, "</capacity-factor>"))))), paste0("touched\t", pP, "\tcapacity-factor"), "unknown", "#1.xml: <x> is inside the value <capacity-factor>"),
    list("unknown: an input flag slot with type-flags", list(inp(lf(8, "type-flags", "Energy"))), paste0("touched\t", pI, "\tflag"), "unknown", "#1.xml writes type-flags"),
    list("unknown: a period year GCAM may read otherwise", list(usa(tech("a", per("2015.0", lf(7, "capacity-factor", 0.5))))), paste0("touched\t", pP, "\tcapacity-factor"), "unknown", "#1.xml: <period> with year \"2015.0\""),
    list("a region commented out is not read (the file is read whole)", list(cm1), paste0("touched\t", pP, "\tcapacity-factor"), "untouched", NULL),
    list("a comment from one block to a later one hides the block between", list(cm2), paste0("exists\t", pm[1], "\rChina"), "untouched", FALSE),
    list("a comment between blocks (read whole)", list(doc(c(reg("China"), "        <!-- x -->", reg("USA", sec(sbs("coal", cf(0.7))))))), paste0("touched\t", pP, "\tcapacity-factor"), "touched", c("#1.xml" = "0.7")),
    list("a comment opened before <scenario> and closed after it hides the blocks it holds (read whole)", list(cm3), paste0("touched\t", pP, "\tcapacity-factor"), "untouched", NULL),
    list("<year> and <name> inside a comment are not read (read whole)", list(cmy), paste0("touched\t", pP, "\tcapacity-factor"), "touched", c("#1.xml" = "0.4")),
    list("unknown: a region name attribute written twice (GCAM keeps the last)", list(sub('<region name="USA">', '<region name="X" name="USA">', usa(cf(0.7)), fixed = TRUE)), paste0("touched\t", pP, "\tcapacity-factor"), "unknown",
         "#1.xml: minaj cannot read it the way GCAM does"),
    list("unknown: a supplysector named by a <name> child", list(doc(reg("USA", c(i4(3, "<supplysector>"), lf(4, "name", "elec"), sbs("coal", cf(0.7)), i4(3, "</supplysector>"))))),
         paste0("touched\t", pP, "\tcapacity-factor"), "unknown", "#1.xml: it sets a name or year with a child element"),
    list("unknown: a region renamed by a <name> child", list(sub('<region name="USA">', '<region name="X">\n            <name>USA</name>', usa(cf(0.7)), fixed = TRUE)), paste0("touched\t", pP, "\tcapacity-factor"), "unknown",
         "#1.xml: it sets a name or year with a child element"),
    list("unknown: set-true flag written as an attribute", list(macv(i4(9, "<no-zero-cost-reductions/>")), sub('<mac-control name="m">', '<mac-control name="m" no-zero-cost-reductions="0">', macv(), fixed = TRUE)),
         paste0("touched\t", pN, "\tno-zero-cost-reductions"), "unknown", "#2.xml writes <no-zero-cost-reductions> as an attribute of <mac-control name=\"m\">"),
    list("stub: nodes queued: the file that queued them", list(usa(i4(5, '<stub-technology name="a"/>')), usa(cf(0.5, tag = "stub-technology"))), paste0("stub\t", pA), "touched", "#2.xml"))
  for (x in cases) {
    cfg(x[[2]], b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); hs <- sprintf("p%d_", k)
    r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), x[[3]]))[[1]]
    v <- r$value; if (!is.null(names(v)) && !is.list(v)) names(v) <- gsub(hs, "#", names(v), fixed = TRUE); if (is.character(v)) v[] <- gsub(hs, "#", v, fixed = TRUE)
    ok <- identical(r$state, x[[4]]) && if (x[[4]] == "unknown") has(gsub(hs, "#", r$why, fixed = TRUE), x[[5]]) else identical(v, x[[5]])
    check(paste0("prefix_scan: ", x[[1]]), ok); if (!ok) { cat("  got: "); str(r) }
  }
  cfg(list(usa(cf(0.5)), doc(reg("China"))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml"))
  r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), paste0("touched\t", pA)))[[1]]
  check("prefix_scan: touched container: the extract of each file that reaches it (chain start tags, the container whole)",
        identical(r$state, "touched") && identical(names(r$value), sprintf("p%d_1.xml", k)) &&
          identical(rawToChar(r$value[[1]]), paste(c("<scenario>", "<world>", '<region name="USA">', '<supplysector name="elec">', '<subsector name="coal">',
                                                    paste(sub("^ +", "", cf(0.5))[1], paste(cf(0.5)[-1], collapse = "\n"), sep = "\n"), "</subsector>", "</supplysector>", "</region>", "</world>", "</scenario>", ""), collapse = "\n")))
  cfg(list(usa(rule(2020, 2050))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); x <- readLines(fc); i <- grep(sprintf("p%d_1.xml", k), x, fixed = TRUE); writeLines(append(x, x[i], i), fc)
  r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), paste0("rules\t", pC)))[[1]]
  check("prefix_scan: a file listed twice is applied twice", identical(r$state, "touched") && length(r$value) == 2L && identical(unname(r$value[1]), unname(r$value[2])))
  cfg(list(usa(cf(0.5)), usa(cf(0.6))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); unlink(file.path(d, sprintf("p%d_2.xml", k)))
  r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), paste0("touched\t", pP, "\tcapacity-factor")))[[1]]
  check("prefix_scan: unknown: an earlier file is missing", identical(r$state, "unknown") && has(r$why, sprintf("p%d_2.xml: the file does not exist", k)))
  # records: answers from the cache, re-reads, extracts reused for paths below, record fields
  cfg(list(usa(cf(0.51))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); fp <- file.path(d, sprintf("p%d_1.xml", k)); qq <- paste0("touched\t", pP, "\tcapacity-factor")
  tm <- as.POSIXct(floor(as.numeric(Sys.time())) - 120, origin = "1970-01-01"); Sys.setFileTime(fp, tm)
  m1 <- utils::capture.output(r1 <- prefix_scan(fc, paste0("b", k, ".xml"), qq)[[1]], type = "message")
  writeBin(charToRaw(paste(usa(cf(0.52)), collapse = "\n")), fp); Sys.setFileTime(fp, tm)
  m2 <- utils::capture.output(r2 <- prefix_scan(fc, paste0("b", k, ".xml"), qq)[[1]], type = "message")
  check("prefix_scan: a second call answers from the records (file overwritten with other bytes of the same size, modification time restored: identical answer, nothing read)",
        identical(as.numeric(file.info(fp)$mtime), as.numeric(tm)) && identical(r1$value, setNames("0.51", basename(fp))) && identical(r1, r2) &&
          any(grepl("^minaj: indexing ([2-9]|[0-9]{2,}) earlier files of c[0-9]+[.]xml [(][0-9.]+ GB[)], once per file version$|^minaj: indexing 1 earlier file of c[0-9]+[.]xml [(][0-9.]+ GB[)], once per file version$", m1)) &&
            any(grepl("^minaj: read [0-9.]+ MB from 1 earlier file of c[0-9]+[.]xml for 1 question[(]s[)] in [0-9.]+ s$", m1)) && !length(m2))
  writeBin(charToRaw(paste(usa(cf(0.525)), collapse = "\n")), fp); m3 <- utils::capture.output(r3 <- prefix_scan(fc, paste0("b", k, ".xml"), qq)[[1]], type = "message")
  check("prefix_scan: a changed earlier file is indexed and read again", identical(r3$value, setNames("0.525", basename(fp))) && any(grepl("indexing 1 earlier file of", m3, fixed = TRUE)) && any(grepl("^minaj: read ", m3)))
  cfg(list(usa(cf(0.53))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); fp <- file.path(d, sprintf("p%d_1.xml", k))
  m1 <- utils::capture.output(r1 <- prefix_scan(fc, paste0("b", k, ".xml"), paste0("touched\t", pS))[[1]], type = "message"); m2 <- utils::capture.output(r2 <- prefix_scan(fc, paste0("b", k, ".xml"), qq)[[1]], type = "message")
  check("prefix_scan: a path below a path already read is answered from its extract, without reading", identical(r1$state, "touched") && identical(r2$value, setNames("0.53", basename(fp))) && !length(m2))
  rc <- lapply(list.files(file.path(nm$store, "rec", "xmlscan"), "[.]rds$", recursive = TRUE, full.names = TRUE), readRDS)
  rc <- rc[vapply(rc, function(z) identical(z$md5, unname(tools::md5sum(fp))), NA)]; bb <- vapply(rc, function(z) z$block, "")
  check("prefix_scan: 'xmlscan' records: md5, block, answer, date, source; one for the block index and one for the block (path -> extract)",
        length(rc) == 2L && all(vapply(rc, function(z) identical(names(z), c("md5", "block", "answer", "date", "source")) && identical(z$source, "xmlscan"), NA)) &&
          setequal(bb, c("index", paste("W", pm[1], "USA", sep = "\r"))) && identical(names(rc[[match("index", bb)]]$answer), c("head", "wtag", "lv", "a", "z", "tag", "name", "key")) &&
          identical(names(rc[[which(bb != "index")]]$answer), pS))
  cfg(list(doc(reg("China", lf(3, "dummy", k)))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); fp <- file.path(d, sprintf("p%d_1.xml", k))
  r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]]
  rc <- lapply(list.files(file.path(nm$store, "rec", "xmlscan"), "[.]rds$", recursive = TRUE, full.names = TRUE), readRDS)
  check("prefix_scan: no record for a block the index shows is absent (only the index record)", identical(r$state, "untouched") &&
          identical(vapply(rc[vapply(rc, function(z) identical(z$md5, unname(tools::md5sum(fp))), NA)], function(z) z$block, ""), "index"))
  cfg(list(usa(cf(0.5), unlist(lapply(sprintf("t%02d", 1:40), function(n) tech(n, per(2015, lf(7, "capacity-factor", 0.1))))))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml"))
  op <- options(minaj.addon_parse_cap_mb = 0.002); r1 <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]]; options(op); r2 <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]]
  check("prefix_scan: unknown: a needed block over the cap (base file blocks under it); not stored, so a larger cap answers",
        file.size(file.path(d, paste0("b", k, ".xml"))) < 0.002 * 2^20 && identical(r1$state, "unknown") && has(r1$why, sprintf("p%d_1.xml: its block region USA is larger than option minaj.addon_parse_cap_mb (0.002 MB)", k)) &&
          identical(r2$value, setNames("0.5", sprintf("p%d_1.xml", k))))
  s1 <- list(nm$store, nm$state_dir); n0 <- length(list.files(file.path(s1[[1]], "rec"), recursive = TRUE)); cfg(list(usa(cf(0.54))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml"))
  nm$store <- NULL; nm$state_dir <- NULL; r <- tryCatch(suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]], error = conditionMessage); nm$store <- s1[[1]]; nm$state_dir <- s1[[2]]
  check("prefix_scan without a store: answers, writes no record", identical(r$value, setNames("0.54", sprintf("p%d_1.xml", k))) && n0 == length(list.files(file.path(s1[[1]], "rec"), recursive = TRUE)))
  cfg(list(gsub("    ", "  ", usa(cf(0.5), unlist(lapply(sprintf("t%02d", 1:40), function(n) tech(n, per(2015, lf(7, "capacity-factor", 0.1)))))), fixed = TRUE)), b0, b0); fc <- file.path(d, paste0("c", k, ".xml"))
  op <- options(minaj.addon_parse_cap_mb = 0.002); r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]]; options(op)
  check("prefix_scan: unknown: a file not laid out as gcamdata writes it, over the cap",
        identical(r$state, "unknown") && has(r$why, sprintf("p%d_1.xml: it is not laid out as gcamdata writes it and is larger than option minaj.addon_parse_cap_mb (0.002 MB)", k)))
  cfg(list(usa(cf(0.56))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml"))
  op <- options(minaj.ram_floor_gb = 1e6); m1 <- tryCatch({ suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq)); "" }, error = conditionMessage); options(op)
  r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]]
  op <- options(minaj.ram_floor_gb = 1e6); m2 <- tryCatch({ suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq)); "" }, error = conditionMessage); options(op)
  check("prefix_scan: memory floor: stops before reading a block, and before reading the extracts when the block is in the records",
        has(m1, "addon_minaj: only ", "GB of memory is free, under the floor of 1e+06 GB (option minaj.ram_floor_gb) plus about ", sprintf(" GB for reading block region USA of p%d_1.xml; try again later", k)) &&
          identical(r$value, setNames("0.56", sprintf("p%d_1.xml", k))) &&
          has(m2, "addon_minaj: only ", "under the floor of 1e+06 GB (option minaj.ram_floor_gb) plus about ", sprintf(" GB for reading the extracts of 1 earlier file of c%d.xml; try again later", k)))
  cfg(list(usa(cf(0.57))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); fp <- file.path(d, sprintf("p%d_1.xml", k)); m0 <- unname(tools::md5sum(fp))
  r <- withCallingHandlers(prefix_scan(fc, paste0("b", k, ".xml"), qq)[[1]], message = function(e) { if (startsWith(conditionMessage(e), "minaj: indexing")) cat("\n", file = fp, append = TRUE); invokeRestart("muffleMessage") })
  rc <- Filter(function(z) identical(z$md5, m0), lapply(list.files(file.path(nm$store, "rec", "xmlscan"), "[.]rds$", recursive = TRUE, full.names = TRUE), readRDS))
  check("prefix_scan: an earlier file that changes while it is read: unknown, and nothing stored under the md5 taken before",
        identical(r$state, "unknown") && has(r$why, sprintf("p%d_1.xml: the file changed while minaj read it", k)) && !length(rc))
  cfg(list(usa(cf(0.58))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); fp <- file.path(d, sprintf("p%d_1.xml", k)); m0 <- unname(tools::md5sum(fp))
  suppressMessages(trace("xml_tokens", where = ns, print = FALSE, tracer = quote(if (is.raw(file_or_text) && length(grepRaw("<region", file_or_text, fixed = TRUE))) stop("cannot allocate vector of size 1 Gb"))))
  m1 <- tryCatch({ suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq)); "" }, error = conditionMessage); suppressMessages(untrace("xml_tokens", where = ns))
  rc <- Filter(function(z) identical(z$md5, m0), lapply(list.files(file.path(nm$store, "rec", "xmlscan"), "[.]rds$", recursive = TRUE, full.names = TRUE), readRDS))
  r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]]
  check("prefix_scan: a failed allocation reading a block stops in the memory form and is not stored as an answer; the next call answers",
        startsWith(m1, "addon_minaj: only ") && has(m1, sprintf("for reading block region USA of p%d_1.xml; try again later", k)) &&
          !any(vapply(rc, function(z) any(grepl("cannot allocate", unlist(Filter(is.character, as.list(z$answer))), fixed = TRUE)), NA)) && identical(r$value, setNames("0.58", basename(fp))))
  cfg(list(usa(cf(0.585))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml")); fp <- file.path(d, sprintf("p%d_1.xml", k)); m0 <- unname(tools::md5sum(fp))
  suppressMessages(trace("xml_tokens", where = ns, print = FALSE, tracer = quote(if (is.raw(file_or_text) && length(grepRaw("<world", file_or_text, fixed = TRUE)) &&   # the parts outside the blocks
                                                                                    !length(grepRaw("<region", file_or_text, fixed = TRUE))) stop("cannot allocate vector of size 1 Gb"))))
  m1 <- tryCatch({ suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq)); "" }, error = conditionMessage); suppressMessages(untrace("xml_tokens", where = ns))
  rc <- Filter(function(z) identical(z$md5, m0), lapply(list.files(file.path(nm$store, "rec", "xmlscan"), "[.]rds$", recursive = TRUE, full.names = TRUE), readRDS))
  r <- suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]]
  check("prefix_scan: a failed allocation while indexing stops in the memory form and nothing is stored; the next call answers",
        startsWith(m1, "addon_minaj: only ") && has(m1, sprintf("for reading the tag lines of p%d_1.xml; try again later", k)) && !length(rc) && identical(r$value, setNames("0.585", basename(fp))))
  op <- options(minaj.addon_parse_cap_mb = 2e-3)                             # 2 KB: over it, a comment, and parts outside the blocks
  cfg(list(doc(c(reg("China"), paste0("        <!-- ", strrep("y", 3000), " -->"), reg("USA", sec(sbs("coal", cf(0.71)))))), doc(c(reg("China"), strrep(" ", 3000), reg("USA", sec(sbs("coal", cf(0.72))))))), b0, b0)
  fc <- file.path(d, paste0("c", k, ".xml")); fp <- file.path(d, sprintf("p%d_%d.xml", k, 1:2))
  r <- tryCatch(suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), qq))[[1]], finally = options(op))
  im <- vapply(Filter(function(z) identical(z$block, "index"), lapply(list.files(file.path(nm$store, "rec", "xmlscan"), "[.]rds$", recursive = TRUE, full.names = TRUE), readRDS)), function(z) z$md5, "")
  check("prefix_scan: over the cap, an index that does not depend on it (a comment) is stored, one that does (the parts outside the blocks) is not",
        identical(r$state, "unknown") && unname(tools::md5sum(fp[1])) %in% im && !unname(tools::md5sum(fp[2])) %in% im)
  cfg(list(usa(cf(0.5))), b0, b0); fc <- file.path(d, paste0("c", k, ".xml"))
  bq <- c(paste0("touched\t", pU, "\nno/such/member.h:1\r2015\tcapacity-factor"), paste0("exists\t", pA, "\n", pm[5], "\r2015.0"), paste0("touched\t", pP, "\tno-such-field"),
          paste0("exists\t", pU, "\n", pm[3], "\rcoal"), paste0("exists\t", pm[12], "\rx"), paste0("exists\t", pG, "\n", pm[13], "\relec"), paste0("keymap\t", pP, "\tcapacity-factor"),
          paste0("siblings\t", pC, "\t", pm[11]))
  check("prefix_scan: a question it does not understand (member, year, field, key form, key map or list of another class) stops with an internal error",
        all(vapply(bq, function(q) startsWith(tryCatch({ suppressMessages(prefix_scan(fc, paste0("b", k, ".xml"), q)); "" }, error = conditionMessage), "addon_minaj: internal error, no add-on written; a question about the earlier files was not understood ("), NA)))
  # chunked block index: a file over 64 MB with a region across the first chunk boundary and its start line in the carried 64 KB
  if (!mem_ok()) cat("SKIP prefix_scan chunked index (less than 4 GB free)\n") else {
    k <- k + 1L; f <- file.path(d, paste0(c("p", "b", "c"), k, c("_1.xml", ".xml", ".xml")))
    h0 <- '<?xml version="1.0" encoding="UTF-8"?>\n<scenario>\n    <world>'; n1 <- (2^26 - 32768 - nchar(h0) - 47) %/% 100   # lines of 100 bytes
    writeBin(charToRaw(paste(c(h0, unlist(lapply(1:4, function(i) c(sprintf('        <region name="R%02d">', i), rep(paste0("            <x>", strrep("y", 80), "</x>"), c(n1, 40000, 40000, 40000)[i]), "        </region>"))),
                               "    </world>", "</scenario>", ""), collapse = "\n")), f[1]); writeLines("<scenario></scenario>", f[2])
    writeLines(c("<Configuration>", i4(1, "<Files>"), sprintf('        <Value name="xmlInputFileName">%s</Value>', basename(ft)), i4(1, "</Files>"), i4(1, "<ScenarioComponents>"),
                 sprintf('        <Value name="%s">%s</Value>', c("p", "b"), basename(f[1:2])), i4(1, "</ScenarioComponents>"), "</Configuration>"), f[3])
    fc <- f[3]; r <- suppressMessages(prefix_scan(fc, basename(f[2]), paste0("exists\t", pU)))[[1]]
    rc <- lapply(list.files(file.path(nm$store, "rec", "xmlscan"), "[.]rds$", recursive = TRUE, full.names = TRUE), readRDS)
    ix <- Filter(function(z) identical(z$md5, unname(tools::md5sum(f[1]))) && identical(z$block, "index"), rc)[[1]]$answer
    b <- readBin(f[1], "raw", file.size(f[1])); oa <- as.numeric(grepRaw("\n        <region ", b, all = TRUE, fixed = TRUE) + 1); oz <- as.numeric(grepRaw("</region>", b, all = TRUE, fixed = TRUE) + 8); rm(b)
    check(sprintf("prefix_scan: chunked block index of a %.0f MB file: block spans equal a whole-file search, none twice, region R02 starts %d bytes before the 64 MB boundary", file.size(f[1]) / 2^20, 2^26 - oa[2] + 1),
          file.size(f[1]) > 2^26 && oa[2] < 2^26 && oa[2] > 2^26 - 65536 && oz[2] > 2^26 && is.list(ix) && identical(ix$a, oa) && identical(ix$z, oz) && !anyDuplicated(ix$a) &&
            identical(ix$name, sprintf("R%02d", 1:4)) && identical(r$state, "untouched"))
    unlink(f[1]); invisible(gc())
  }

  # ---- addon_write with config: the places that depend on the earlier files -------------------------------------
  # One case per rule and per refusal: earlier files (P), base (B), override (O); the exact add-on body, or "REFUSED"
  # and parts of the message ("p" ... "_2.xml" stand for the case's earlier files). Model years 1975, 1990, 2005, 2010,
  # 2015, 2020, 2025 (cfg).
  u0 <- doc(reg("USA")); qd <- i4(5, '<technology name="b" delete="1"/>'); qb <- tech("b", per(2015, lf(7, "capacity-factor", 0.6)))
  qn <- tech("b", per(2015, lf(7, "capacity-factor", 0.6)), at = ' nocreate="1"'); qx <- tech("b", per(2015, lf(7, "capacity-factor", 0.5)))
  qs1 <- usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8), lf(7, "share-weight", 1)))); qs0 <- usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8))))
  qt1 <- st2(per(2015, lf(7, "capacity-factor", 0.8), lf(7, "share-weight", 1))); qt0 <- st2(per(2015, lf(7, "capacity-factor", 0.8)))
  qtr <- chain(c(i4(5, '<stub-technology name="a" delete="1"/>'), tech("a", per(2015, lf(7, "capacity-factor", 0.8)), tag = "stub-technology")))
  qr1 <- c(i4(5, '<interpolation-rule apply-to="share-weight" delete="1" from-year="2010" to-year="2020">'), i4(6, '<interpolation-function name="linear"/>'), i4(5, "</interpolation-rule>"))
  qky <- doc(reg("USA", c(i4(3, '<supplysector name="elec">'), i4(4, '<keyword a="#"/>'), i4(3, "</supplysector>"))))
  qtd <- i4(5, '<technology name="a" delete="1"/>')
  ql <- c("REFUSED", "technology[b]: only in the base file (removal); an earlier file (p", "has content there, which the add-on cannot restore without replaying earlier files (not supported yet)")
  cases <- list(
    list("9a: value only in the base file: the earlier file's value", list(usa(cf(0.7))), usa(cf(0.8)), usa(tech("a", per(2015))), macw(lf(7, "capacity-factor", 0.7))),
    list("9a: earlier files untouched: the constructor default (capacity-factor 1)", list(u0), usa(cf(0.8)), usa(tech("a", per(2015))), macw(lf(7, "capacity-factor", 1))),
    list("9a: an unset default with option minaj.addon_neg0 FALSE: the period rebuilt", list(u0), qs1, qs0,
         chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" delete="1"/>'), per(2015, lf(7, "capacity-factor", 0.8)), i4(5, "</technology>"))), list(minaj.addon_neg0 = FALSE)),
    list("9a: an unset default with option minaj.addon_neg0 TRUE: -0", list(u0), qs1, qs0, macw(lf(7, "share-weight", "-0")), list(minaj.addon_neg0 = TRUE)),
    list("9a: an unset default, option minaj.addon_neg0 not set (TRUE): -0", list(u0), qs1, qs0, macw(lf(7, "share-weight", "-0"))),
    list("9a: gcamstr default \"\" (output-unit)", list(u0), doc(reg("USA", sec(lf(4, "output-unit", "EJ")))), doc(reg("USA", sec())), os(lf(4, "output-unit", ""))),
    list("9a refuses: a delete in an earlier file (the class check on the technology names it)", list(usa(cf(0.5)), usa(qtd)), usa(cf(0.8)), usa(tech("a", per(2015))),
         c("REFUSED", "technology[a]: an earlier file may have created it with another class; p", "_2.xml writes <technology name=\"a\"> with delete=\"1\", so the state before the base file is not known")),
    list("9a refuses: an earlier file created the technology with another class", list(usa(tech("a", per(2015, lf(7, "capacity-factor", 1)), tag = "intermittent-technology"))), usa(cf(0.8)), usa(tech("a", per(2015))),
         c("REFUSED", "subsector[coal]/technology[a]: an earlier file (p", "_1.xml) created it as intermittent-technology (TechnologyContainer<IntermittentTechnology>)")),
    list("9b: years only the base file sets: defaults for the model years it alone covers, then the override's element",
         list(u0), usa(lf(5, "fuelprefElasticity", 2, ' fillout="1" year="1975"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"')),
         chain(c(lf(5, "fuelprefElasticity", 0, ' year="1975"'), lf(5, "fuelprefElasticity", 0, ' year="1990"'), lf(5, "fuelprefElasticity", 0, ' year="2005"'), lf(5, "fuelprefElasticity", 0, ' year="2015"'),
                 lf(5, "fuelprefElasticity", 0, ' year="2020"'), lf(5, "fuelprefElasticity", 0, ' year="2025"'), lf(5, "fuelprefElasticity", 3, ' year="2010"')))),
    list("9b: the earlier files' writes as targets (1975, and 2005 with fillout)", list(usa(lf(5, "fuelprefElasticity", 4, ' year="1975"'), lf(5, "fuelprefElasticity", 5, ' fillout="1" year="2005"'))),
         usa(lf(5, "fuelprefElasticity", 2, ' fillout="1" year="1975"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"')),
         chain(c(lf(5, "fuelprefElasticity", 4, ' year="1975"'), lf(5, "fuelprefElasticity", 0, ' year="1990"'), lf(5, "fuelprefElasticity", 5, ' year="2005"'), lf(5, "fuelprefElasticity", 5, ' year="2015"'),
                 lf(5, "fuelprefElasticity", 5, ' year="2020"'), lf(5, "fuelprefElasticity", 5, ' year="2025"'), lf(5, "fuelprefElasticity", 3, ' year="2010"')))),
    list("9b: a year that is not a model year sets nothing", list(u0), usa(lf(5, "fuelprefElasticity", 2, ' fillout="1" year="1980"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"')),
         chain(lf(5, "fuelprefElasticity", 3, ' year="2010"'))),
    list("9e: only in the base file, earlier files untouched: deleted", list(u0), usa(cf(0.8), tech("b")), usa(cf(0.8)), chain(qd)),
    list("9e: created by an earlier file and changed by the base file: a wrapper with the earlier value and the base file's new period deleted",
         list(usa(qx)), usa(cf(0.8), tech("b", per(2015, lf(7, "capacity-factor", 0.6)), per(2020, lf(7, "capacity-factor", 0.7)))), usa(cf(0.8)),
         chain(c(i4(5, '<technology name="b" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), lf(7, "capacity-factor", 0.5), i4(6, "</period>"), i4(6, '<period year="2020" delete="1"/>'), i4(5, "</technology>")))),
    list("9e: the base file's elements all nocreate, absent after the earlier files: nothing", list(u0), usa(cf(0.8), qn), usa(cf(0.8)), "<scenario></scenario>"),
    list("9e: stub-technology only in the base file, earlier files untouched: deleted", list(u0), usa(cf(0.8), tech("b", tag = "stub-technology")), usa(cf(0.8)), chain(i4(5, '<stub-technology name="b" delete="1"/>'))),
    list("9e: stub-technology an earlier file created without children: deleted, then created again", list(usa(i4(5, '<stub-technology name="b"/>'))), usa(cf(0.8), tech("b", tag = "stub-technology")), usa(cf(0.8)),
         chain(c(i4(5, '<stub-technology name="b" delete="1"/>'), i4(5, '<stub-technology name="b"/>')))),
    list("9e: base file [D], untouched: nothing", list(u0), usa(cf(0.8), qd), usa(cf(0.8)), "<scenario></scenario>"),
    list("9e refuses: base file [D], touched", list(usa(qb)), usa(cf(0.8), qd), usa(cf(0.8)), ql),
    list("9e: base file [D, M], untouched: deleted", list(u0), usa(cf(0.8), qd, qb), usa(cf(0.8)), chain(qd)),
    list("9e refuses: base file [D, M], touched", list(usa(qb)), usa(cf(0.8), qd, qb), usa(cf(0.8)), ql),
    list("9e: base file [M, D], untouched: nothing", list(u0), usa(cf(0.8), qb, qd), usa(cf(0.8)), "<scenario></scenario>"),
    list("9e refuses: base file [M, D], touched", list(usa(qb)), usa(cf(0.8), qb, qd), usa(cf(0.8)), ql),
    list("9e: base file [N, D], untouched: nothing", list(u0), usa(cf(0.8), qn, qd), usa(cf(0.8)), "<scenario></scenario>"),
    list("9e refuses: base file [N, D], touched", list(usa(qb)), usa(cf(0.8), qn, qd), usa(cf(0.8)), ql),
    list("9g: deleted in the base file only, earlier files untouched: the delete, then the override's elements", list(u0), usa(cf(0.8), qd, qb), usa(cf(0.8), tech("b", per(2015, lf(7, "capacity-factor", 0.7)))),
         chain(c(qd, tech("b", per(2015, lf(7, "capacity-factor", 0.7)))))),
    list("9g refuses: deleted in the base file only, an earlier file has content there (replay not supported)", list(usa(qx)), usa(cf(0.8), qd, qb), usa(cf(0.8), tech("b", per(2015, lf(7, "capacity-factor", 0.7)))),
         c("REFUSED", "technology[b]: deleted in the base file but not in the override; an earlier file (p", "has content there, which the add-on cannot restore without replaying earlier files (not supported yet)")),
    list("9f: type change, earlier files untouched: deleted with the base file's tag, then the override's element", list(u0), usa(cf(0.8)), usa(cf(0.8, tag = "intermittent-technology")),
         chain(c(qtd, cf(0.8, tag = "intermittent-technology")))),
    list("9f refuses: type change of a technology an earlier file created", list(usa(cf(0.5))), usa(cf(0.8)), usa(cf(0.8, tag = "intermittent-technology")),
         c("REFUSED", "type change (technology to intermittent-technology); an earlier file (p", "_1.xml) created it as technology (TechnologyContainer<DefaultTechnology>)")),
    list("9f refuses: a holder an earlier file touches is not rebuilt (refused there; option minaj.addon_neg0 FALSE)", list(usa(tech("a", per(2015, lf(7, "capacity-factor", 0.5))))), qs1, qs0,
         c("REFUSED", "period[2015]/share-weight: value set only in the base file (un-set); minaj restores an unset value only with option minaj.addon_neg0 = TRUE; scenario/world/region[USA]/supplysector[elec]/subsector[coal]/technology[a]/period[2015] is not rebuilt (an earlier file (p",
           "_1.xml) has content there)"), list(minaj.addon_neg0 = FALSE)),
    list("9n refuses: input flag only in the base file, an earlier file touches the input (depth 9; refused at the input)", list(inp(lf(8, "coefficient", 3))), inp(lf(8, "flag", "Energy"), lf(8, "flag", "Resource")), inp(lf(8, "flag", "Energy")),
         c("REFUSED", "minicam-energy-input[coal]/flag: input flag set only in the base file; scenario/world/region[USA]/supplysector[elec]/subsector[coal]/technology[a]/period[2015]/minicam-energy-input[coal] is not rebuilt (an earlier file (p")),
    list("9n refuses: set-true only in the base file, an earlier file touches the mac-control (depth 10; refused at the mac-control)", list(mac()), mac(i4(9, "<no-zero-cost-reductions/>")), mac(),
         c("REFUSED", "mac-control[m]/no-zero-cost-reductions: set only in the base file; scenario/world/region[USA]/supplysector[elec]/subsector[coal]/technology[a]/period[2015]/Non-CO2[SO2]/mac-control[m] is not rebuilt (an earlier file (p")),
    list("9a refuses: type-flags only in the base file, an earlier file writes <flag> (one int)", list(inp(lf(8, "flag", "Energy"))), inp(lf(8, "type-flags", 2), lf(8, "coefficient", 1)), inp(lf(8, "coefficient", 1)),
         c("REFUSED", "minicam-energy-input[coal]/type-flags: value set only in the base file (un-set); an earlier file writes the input flags (type-flags or <flag>), which the add-on cannot restore")),
    list("9a refuses: type-flags only in the base file, an earlier file writes type-flags", list(inp(lf(8, "type-flags", 4))), inp(lf(8, "type-flags", 2), lf(8, "coefficient", 1)), inp(lf(8, "coefficient", 1)),
         c("REFUSED", "minicam-energy-input[coal]/type-flags: value set only in the base file (un-set); p", "_1.xml writes type-flags (it resets the input flags), so the state before the base file is not known")),
    list("9a: type-flags only in the base file, earlier files write neither: 0", list(u0), inp(lf(8, "type-flags", 2), lf(8, "coefficient", 1)), inp(lf(8, "coefficient", 1)),
         macw(i4(7, '<minicam-energy-input name="coal" nocreate="1">'), lf(8, "type-flags", 0), i4(7, "</minicam-energy-input>"))),
    list("9a refuses: no default minaj can restore (social-discount-rate), in a region (never rebuilt)", list(u0), doc(reg("USA", lf(3, "social-discount-rate", 0.1))), doc(reg("USA")),
         c("REFUSED", "region[USA]/social-discount-rate: value set only in the base file (un-set); minaj knows no default it can restore")),
    list("9a refuses: technology-level value only in the base file (no default inside a technology container)", list(u0), usa(tech("a", lf(6, "initial-available-year", 1990), per(2015))), usa(tech("a", per(2015))),
         c("REFUSED", "technology[a]/initial-available-year: value set only in the base file (un-set); minaj restores no default inside TechnologyContainer<DefaultTechnology>")),
    list("9b refuses: no default minaj can restore (max-price), the policy touched by an earlier file",
         list(doc(reg("USA", i4(3, '<policy-portfolio-standard name="p">'), lf(4, "market", "USA"), i4(3, "</policy-portfolio-standard>")))),
         doc(reg("USA", i4(3, '<policy-portfolio-standard name="p">'), lf(4, "max-price", 5, ' fillout="1" year="1975"'), i4(3, "</policy-portfolio-standard>"))),
         doc(reg("USA", i4(3, '<policy-portfolio-standard name="p">'), lf(4, "max-price", 3, ' year="2010"'), i4(3, "</policy-portfolio-standard>"))),
         c("REFUSED", "policy-portfolio-standard[p]/max-price: years set only in the base file; minaj knows no default it can restore; scenario/world/region[USA]/policy-portfolio-standard[p] is not rebuilt (an earlier file (p")),
    list("9b: an unset default with option minaj.addon_neg0 FALSE: the subsector rebuilt", list(u0), usa(sw), usa(lf(5, "share-weight", 3, ' year="2010"')),
         os(i4(4, '<subsector name="coal" delete="1"/>'), sbs("coal", lf(5, "share-weight", 3, ' year="2010"'))), list(minaj.addon_neg0 = FALSE)),
    list("9b refuses: a year GCAM may read otherwise", list(u0), usa(lf(5, "fuelprefElasticity", 2, ' year="1975.0"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"')),
         c("REFUSED", "subsector[coal]/fuelprefElasticity: years set only in the base file; a year GCAM may read otherwise")),
    list("9e: the base file's elements all nocreate, present after the earlier files: the earlier value restored in a wrapper", list(usa(qx)), usa(cf(0.8), qn), usa(cf(0.8)),
         chain(c(i4(5, '<technology name="b" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), lf(7, "capacity-factor", 0.5), i4(6, "</period>"), i4(5, "</technology>")))),
    list("9e refuses: stub-technology only in the base file, an earlier file queued nodes on it", list(usa(tech("b", per(2015, lf(7, "capacity-factor", 0.5)), tag = "stub-technology"))),
         usa(cf(0.8), tech("b", tag = "stub-technology")), usa(cf(0.8)), c("REFUSED", "stub-technology[b]: only in the base file (removal); an earlier file (p", "_1.xml) queued nodes on it, which the add-on cannot restore")),
    list("9e: stub-technology only in the base file, written with nocreate, absent after the earlier files: nothing", list(u0), usa(cf(0.8), tech("b", tag = "stub-technology", at = ' nocreate="1"')), usa(cf(0.8)), "<scenario></scenario>"),
    list("9e: stub-technology only in the base file, deleted there, earlier files untouched: nothing", list(u0), usa(cf(0.8), i4(5, '<stub-technology name="b" delete="1"/>')), usa(cf(0.8)), "<scenario></scenario>"),
    list("9e refuses: restoring the earlier state needs a rebuild of a period an earlier file has content in (option minaj.addon_neg0 FALSE)", list(usa(qx)), usa(cf(0.8), tech("b", per(2015, lf(7, "share-weight", 1)))), usa(cf(0.8)),
         c("REFUSED", "technology[b]/period[2015]/share-weight: value set only in the base file (un-set); minaj restores an unset value only with option minaj.addon_neg0 = TRUE; scenario/world/region[USA]/supplysector[elec]/subsector[coal]/technology[b]/period[2015] is not rebuilt (an earlier file has content there)"), list(minaj.addon_neg0 = FALSE)),
    list("9e refuses: an earlier file created it with another class", list(usa(tech("b", per(2015, lf(7, "capacity-factor", 0.5)), tag = "intermittent-technology"))), usa(cf(0.8), qb), usa(cf(0.8)),
         c("REFUSED", "technology[b]: only in the base file (removal); an earlier file (p", "_1.xml) created it as intermittent-technology (TechnologyContainer<IntermittentTechnology>)")),
    list("9e refuses: written with different types", list(doc(reg("USA", sec(sbs("b"))))), doc(reg("USA", sec(sbs("coal", cf(0.8)), i4(4, '<tranSubsector name="b" nocreate="1">'), i4(4, "</tranSubsector>"), sbs("b")))),
         doc(reg("USA", sec(sbs("coal", cf(0.8))))), c("REFUSED", "supplysector[elec]/tranSubsector[b]: only in the base file (removal); written with different types (tranSubsector, subsector)")),
    list("9g refuses: a region deleted in the base file only (never deleted)", list(u0), doc(c(i4(2, '<region name="USA" delete="1"/>'), reg("USA", sec(sbs("coal", cf(0.8)))))), usa(cf(0.9)),
         c("REFUSED", "region[USA]: deleted in the base file but not in the override, and addon_minaj never deletes or rebuilds this container")),
    list("9h: [N] against [M], absent after the earlier files: the override's element copied", list(u0), usa(cf(0.8, at = ' nocreate="1"')), usa(cf(0.9)), chain(cf(0.9))),
    list("9k refuses: the rebuild reaches a grade an earlier file made before the base file's grades", list(gr(g("g1", 1), g("g2", 2))), gr(g("g1", 1), g("g3", 3)),
         gr(i4(5, '<grade name="g1" delete="1"/>'), g("g1", 1), g("g3", 3)),
         c("REFUSED", "subresource[c]/grade: entry order changes in an order-critical list (grades, food-storage inputs); the rebuild reaches g1, which an earlier file made before the base file's entries")),
    list("9k refuses: the rebuild reaches a grade an earlier file created (touched)", list(gr(g("g1", 1), g("g2", 2))), gr(i4(5, '<grade name="g1" delete="1"/>'), g("g3", 3), g("g1", 1)),
         gr(i4(5, '<grade name="g1" delete="1"/>'), g("g3", 3), g("g4", 4), g("g1", 1)),
         c("REFUSED", "subresource[c]/grade: entry order changes in an order-critical list (grades, food-storage inputs); the rebuild reaches g1, which an earlier file touches")),
    list("9n refuses: input flag only in the base file, the earlier file's flags not known", list(inp(lf(8, "type-flags", 4))), inp(lf(8, "flag", "Energy"), lf(8, "flag", "Resource")), inp(lf(8, "flag", "Energy")),
         c("REFUSED", "minicam-energy-input[coal]/flag: input flag set only in the base file; p", "_1.xml writes type-flags (it resets the input flags), so the state before the base file is not known")),
    list("9n refuses: key-map key only in the base file, the earlier file's keys not known", list(luh(al("17x0", 9))), luh(al(1700, 5), al(1750, 6)), luh(al(1700, 5)),
         c("REFUSED", "land-use-history/allocation: key removed; p", "_1.xml writes <allocation> with a year GCAM may read otherwise, so the state before the base file is not known")),
    list("9n refuses: coefficient only in the base file, a delete in an earlier file (the class check on the technology names it)", list(inp(lf(8, "coefficient", 3)), usa(qtd)),
         inp(lf(8, "flag", "Energy"), lf(8, "coefficient", 2)), inp(lf(8, "flag", "Energy")),
         c("REFUSED", "technology[a]: an earlier file may have created it with another class; p", "_2.xml writes <technology name=\"a\"> with delete=\"1\", so the state before the base file is not known")),
    list("9h: [M] against [N], present after the earlier files: the change in a wrapper", list(usa(cf(0.5))), usa(cf(0.8)), usa(cf(0.9, at = ' nocreate="1"')), macw(lf(7, "capacity-factor", 0.9))),
    list("9h: [M] against [N], absent after them: deleted", list(u0), usa(cf(0.8)), usa(cf(0.9, at = ' nocreate="1"')), chain(qtd)),
    list("9h refuses: [M] against [N], not known", list(usa(cf(0.5)), usa(qtd)), usa(cf(0.8)), usa(cf(0.9, at = ' nocreate="1"')),
         c("REFUSED", "technology[a]: nocreate differs between the base file and the override; p", "with delete=\"1\", so the state before the base file is not known")),
    list("9h: [N, M] against [N, M], present: the change in a wrapper", list(usa(cf(0.5))), nm2(0.8), nm2(0.9), macw(lf(7, "capacity-factor", 0.9))),
    list("9h: [N, M] against [N, M], absent: nothing", list(u0), nm2(0.8), nm2(0.9), "<scenario></scenario>"),
    list("9h: earlier files write the technology only with nocreate (never created): written", list(usa(cf(0.5, at = ' nocreate="1"'))), nm2(0.8), nm2(0.9), "<scenario></scenario>"),
    list("9h refuses: [N, M] against [N, M], not known", list(usa(cf(0.5)), usa(qtd)), nm2(0.8), nm2(0.9), c("REFUSED", "technology[a]: nocreate pattern", "so the state before the base file is not known")),
    list("9i: keyword changed in a period: the vintage rebuilt", list(u0), kw(' primary-renewable="x"'), kw(' primary-renewable="y"'),
         chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" delete="1"/>'), per(2015, i4(7, '<keyword primary-renewable="y"/>')), i4(5, "</technology>")))),
    list("9i refuses: keyword changed in a sector an earlier file touches", list(usa(cf(0.5))), sub("#", "x", qky, fixed = TRUE), sub("#", "y", qky, fixed = TRUE),
         c("REFUSED", "supplysector[elec]/keyword: keyword dropped or changed (GCAM keeps the first value)", "supplysector[elec] is not rebuilt (an earlier file (p")),
    list("9j: earlier rule, base rule, override rule: the earlier rule with the clear, then the override's", list(usa(rule(2010, 2020))), usa(rule(2020, 2050)), usa(rule(2020, 2060)), chain(c(qr1, rule(2020, 2060)))),
    list("9j: the base file clears the rules and the override does not", list(usa(rule(2010, 2020))), usa(rule(2020, 2050, at = ' delete="1"')), usa(rule(2020, 2050)), chain(c(qr1, rule(2020, 2050)))),
    list("9j: rule list emptied by the override: a rule that sets nothing over the first two model years", list(u0), usa(rule(2020, 2050)), usa(),
         chain(i4(5, '<interpolation-rule apply-to="share-weight" delete="1" from-year="1975" to-year="1990"><from-value>1</from-value><to-value>1</to-value><interpolation-function name="linear"/></interpolation-rule>'))),
    list("9j: an earlier rule written apply-to=\"share.weight\" delete=\"1\" gets the clear as GCAM reads it", list(usa(r2s)), usa(rule(2020, 2050)), usa(rule(2030, 2050)),
         chain(c(i4(5, '<interpolation-rule apply-to="share-weight" delete="1" from-year="2020" to-year="2060">'), r2s[-1], rule(2030, 2050)))),
    list("9j: rule list changed inside a stub-technology: stub replace", list(u0), st2(per(2015, lf(7, "capacity-factor", 0.8)), srule), st2(per(2015, lf(7, "capacity-factor", 0.8)), srule2),
         chain(c(i4(5, '<stub-technology name="a" delete="1"/>'), tech("a", per(2015, lf(7, "capacity-factor", 0.8)), srule2, tag = "stub-technology")))),
    list("9k: grade inserted: suffix rebuild", list(u0), gr(g("g1", 1), g("g3", 3)), gr(g("g1", 1), g("g2", 2), g("g3", 3)), gra(i4(5, '<grade name="g3" delete="1"/>'), g("g2", 2), g("g3", 3))),
    list("9k: base [g1, g2, delete g1, g1] against override [g1, g2, g3]: [g1, g2, g3]", list(u0), gr(g("g1", 1), g("g2", 2), i4(5, '<grade name="g1" delete="1"/>'), g("g1", 1)), gr(g("g1", 1), g("g2", 2), g("g3", 3)),
         gra(i4(5, '<grade name="g2" delete="1"/>'), i4(5, '<grade name="g1" delete="1"/>'), g("g1", 1), g("g2", 2), g("g3", 3))),
    list("9k refuses: the rebuild reaches a grade an earlier file deleted", list(gr(g("g3", 5), i4(5, '<grade name="g3" delete="1"/>'))), gr(g("g1", 1), g("g3", 3)), gr(g("g1", 1), g("g2", 2), g("g3", 3)),
         c("REFUSED", "subresource[c]/grade: entry order changes in an order-critical list (grades, food-storage inputs); the rebuild reaches g3, which an earlier file touches; p", "so the state before the base file is not known")),
    list("9k: input inserted before a food-storage technology's inputs: suffix rebuild", list(u0), fst(inx("a"), inx("b")), fst(inx("c"), inx("a"), inx("b")),
         chain(c(i4(5, '<food-storage-technology name="f" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<minicam-energy-input name="a" delete="1"/>'), i4(7, '<minicam-energy-input name="b" delete="1"/>'),
                 inx("c"), inx("a"), inx("b"), i4(6, "</period>"), i4(5, "</food-storage-technology>")))),
    list("9l: value only in the base file inside a stub-technology: stub replace (a request from the period settles at the subsector)", list(u0), qt1, qt0, qtr),
    list("9l refuses: an earlier file queued nodes on the stub-technology", list(st2(per(2015, lf(7, "capacity-factor", 0.5)))), qt1, qt0,
         c("REFUSED", "stub-technology[a]/period[2015]/share-weight: value set only in the base file (un-set); inside a stub-technology, only changed values, new rows, prefix rule extensions, new keyword keys and the override's own deletes can be written; a stub replace is not allowed because an earlier file (p", "_1.xml) queued nodes on it")),
    list("9l refuses: the base file and the override write the stub-technology with nocreate", list(u0), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8), lf(7, "share-weight", 1)), tag = "stub-technology", at = ' nocreate="1"')),
         usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), tag = "stub-technology", at = ' nocreate="1"')), c("REFUSED", "a stub replace is not allowed because the base file or the override writes it with nocreate")),
    list("9l: input order changed inside a stub-technology: stub replace", list(u0), st2(per(2015, inx("a"))), st2(per(2015, inx("c"), inx("a"))),
         chain(c(i4(5, '<stub-technology name="a" delete="1"/>'), tech("a", per(2015, inx("c"), inx("a")), tag = "stub-technology")))),
    list("9l: an earlier file created the stub-technology without children: stub replace", list(usa(i4(5, '<stub-technology name="a"/>'))), qt1, qt0, qtr),
    list("9n: input flag only in the base file, set by an earlier file: nothing", list(inp(lf(8, "flag", "Resource"))), inp(lf(8, "flag", "Energy"), lf(8, "flag", "Resource")), inp(lf(8, "flag", "Energy")), "<scenario></scenario>"),
    list("9n: input flag only in the base file, earlier files untouched: the input rebuilt", list(u0), inp(lf(8, "flag", "Energy"), lf(8, "flag", "Resource")), inp(lf(8, "flag", "Energy")),
         macw(i4(7, '<minicam-energy-input name="coal" delete="1"/>'), i4(7, '<minicam-energy-input name="coal">'), lf(8, "flag", "Energy"), i4(7, "</minicam-energy-input>"))),
    list("9n: key-map key only in the base file, set by an earlier file: its element", list(luh(al(1750, 9))), luh(al(1700, 5), al(1750, 6)), luh(al(1700, 5)),
         ow(i4(3, '<LandAllocatorRoot nocreate="1">'), i4(4, '<LandLeaf name="l" nocreate="1">'), i4(5, '<land-use-history nocreate="1">'), al(1750, 9), i4(5, "</land-use-history>"), i4(4, "</LandLeaf>"), i4(3, "</LandAllocatorRoot>"))),
    list("9n: key-map key only in the base file, earlier files untouched: the land-use-history rebuilt", list(u0), luh(al(1700, 5), al(1750, 6)), luh(al(1700, 5)),
         ow(i4(3, '<LandAllocatorRoot nocreate="1">'), i4(4, '<LandLeaf name="l" nocreate="1">'), i4(5, '<land-use-history delete="1"/>'), i4(5, "<land-use-history>"), al(1700, 5), i4(5, "</land-use-history>"),
            i4(4, "</LandLeaf>"), i4(3, "</LandAllocatorRoot>"))),
    list("9n: coefficient only in the base file, set by an earlier file: its element", list(inp(lf(8, "coefficient", 3))), inp(lf(8, "flag", "Energy"), lf(8, "coefficient", 2)), inp(lf(8, "flag", "Energy")),
         macw(i4(7, '<minicam-energy-input name="coal" nocreate="1">'), lf(8, "coefficient", 3), i4(7, "</minicam-energy-input>"))),
    list("9n: coefficient only in the base file, earlier files untouched: the input rebuilt", list(u0), inp(lf(8, "flag", "Energy"), lf(8, "coefficient", 2)), inp(lf(8, "flag", "Energy")),
         macw(i4(7, '<minicam-energy-input name="coal" delete="1"/>'), i4(7, '<minicam-energy-input name="coal">'), lf(8, "flag", "Energy"), i4(7, "</minicam-energy-input>"))),
    list("9n: set-true only in the base file, set by an earlier file: nothing", list(mac(i4(9, "<no-zero-cost-reductions/>"))), mac(i4(9, "<no-zero-cost-reductions/>")), mac(), "<scenario></scenario>"),
    list("9n: set-true only in the base file, earlier files untouched: the mac-control rebuilt", list(u0), mac(i4(9, "<no-zero-cost-reductions/>")), mac(),
         macw(i4(7, '<Non-CO2 name="SO2" nocreate="1">'), i4(8, '<mac-control name="m" delete="1"/>'), i4(8, '<mac-control name="m">'), lf(9, "mac-reduction", 0.1, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))),
    list("9a: an earlier file's region commented out is not read: the default", list(cm1), usa(cf(0.8)), usa(tech("a", per(2015))), macw(lf(7, "capacity-factor", 1))),
    list("9a: an earlier file's region inside a comment opened before <scenario> is not read: the default", list(cm3), usa(cf(0.8)), usa(tech("a", per(2015))), macw(lf(7, "capacity-factor", 1))),
    list("9a: <year> and <name> inside an earlier file's comment are not read: the earlier file's value", list(cmy), usa(cf(0.8)), usa(tech("a", per(2015))), macw(lf(7, "capacity-factor", 0.4))),
    list("9a refuses: a default given by a C++ identifier (hector-ini-file) is not restored", list(u0), doc(c(reg("USA"), i4(2, "<HectorModel>"), lf(3, "hector-ini-file", "x.ini"), i4(2, "</HectorModel>"))),
         doc(c(reg("USA"), i4(2, "<HectorModel/>"))), c("REFUSED", "HectorModel/hector-ini-file: value set only in the base file (un-set); minaj knows no default it can restore")),
    list("9n refuses: set-true only in the base file, an earlier file sets it as an attribute", list(mac(i4(9, "<no-zero-cost-reductions/>")), sub('<mac-control name="m">', '<mac-control name="m" no-zero-cost-reductions="0">', mac(), fixed = TRUE)),
         mac(i4(9, "<no-zero-cost-reductions/>")), mac(), c("REFUSED", "mac-control[m]/no-zero-cost-reductions: set only in the base file; p", "_2.xml writes <no-zero-cost-reductions> as an attribute of <mac-control name=\"m\">")),
    list("9b: no model year left to restore (the override's fillout covers it): written though an earlier file is not known", list(usa(sw), chain(lf(5, "share-weight", 2, ' year="1990"'))),
         usa(lf(5, "share-weight", 1, ' year="2025"')), usa(lf(5, "share-weight", 0.5, ' fillout="1" year="2020"')), chain(lf(5, "share-weight", 0.5, ' fillout="1" year="2020"'))),
    list("9b: the base file's only year is not a model year: written though an earlier file is not known", list(usa(sw), chain(lf(5, "share-weight", 2, ' year="1990"'))),
         usa(lf(5, "share-weight", 1, ' year="2023"')), usa(lf(5, "share-weight", 0.5, ' year="2010"')), chain(lf(5, "share-weight", 0.5, ' year="2010"'))),
    list("9b refuses: no question asked, an earlier file sets a name with a child element (the check after the earlier files does not follow it)",
         list(sub('<region name="USA">', '<region name="X">\n            <name>USA</name>', usa(sw), fixed = TRUE)), usa(lf(5, "share-weight", 1, ' year="2023"')), usa(lf(5, "share-weight", 0.5, ' year="2010"')),
         c("REFUSED", "REFUSED: addon_minaj: no add-on written; p", "_1.xml (an earlier file of c", ".xml) sets a name or year with a child element (GCAM matches the object by it later), which the check after the earlier files does not follow; use ")),
    list("9h: the first creating elements differ in type, absent after the earlier files: deleted, then the override's elements", list(u0),
         usa(tech("a", at = ' nocreate="1"'), cf(0.8, tag = "intermittent-technology")), usa(tech("a", at = ' nocreate="1"'), cf(0.8)), chain(c(i4(5, '<intermittent-technology name="a" delete="1"/>'), cf(0.8)))),
    list("9k: the override deletes and rebuilds a middle grade: suffix rebuild", list(u0), gr(g("g1", 1), g("g2", 2), g("g3", 3)), gr(g("g1", 1), i4(5, '<grade name="g2" delete="1"/>'), g("g2", 2.5), g("g3", 3)),
         gra(i4(5, '<grade name="g2" delete="1"/>'), i4(5, '<grade name="g3" delete="1"/>'), i4(5, '<grade name="g2" delete="1"/>'), g("g2", 2.5), g("g3", 3))),
    list("9k: a middle grade changed to accumulatedGrade: suffix rebuild", list(u0), gr(g("g1", 1), g("g2", 2), g("g3", 3)),
         gr(g("g1", 1), c(i4(5, '<accumulatedGrade name="g2">'), lf(6, "extractioncost", 2), i4(5, "</accumulatedGrade>")), g("g3", 3)),
         gra(i4(5, '<grade name="g2" delete="1"/>'), i4(5, '<grade name="g3" delete="1"/>'), i4(5, '<accumulatedGrade name="g2">'), lf(6, "extractioncost", 2), i4(5, "</accumulatedGrade>"), g("g3", 3))))
  for (x in cases) {
    op <- options(if (length(x) > 5L) x[[6]] else list()); m <- suppressMessages(cfg(x[[2]], x[[3]], x[[4]])); options(op)
    ok <- if (identical(x[[5]][1], "REFUSED")) startsWith(m, "REFUSED") && has(m, x[[5]][-1]) else identical(m, x[[5]]) && is.list(last) && !is.null(last$prefix_md5)
    check(paste0("add-on with config, ", x[[1]]), ok); if (!ok) cat("  got:", m, "\n")
  }
  m <- suppressMessages(cfg(list(u0), usa(lf(5, "fuelprefElasticity", 2, ' fillout="1" year="1975"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"')), years = NULL))
  check("add-on with config refuses: years only the base file sets, model years not known", has(m, "REFUSED", "fuelprefElasticity: years set only in the base file; the model years are not known (no <modeltime> in t", ".xml)"))
  m <- suppressMessages(cfg(list(usa(cf(0.7)), u0), doc(reg("USA", sec(sbs("coal", cf(0.8)), sbs("b")))), doc(reg("USA", sec(lf(4, "output-unit", "PJ"), sbs("b"), sbs("coal", tech("a", per(2015))))))))
  fp <- file.path(d, c(sprintf("t%d.xml", k), sprintf("p%d_%d.xml", k, 1:2)))
  check("add-on with config: a group answered in a later pass counts each piece and order move once; prefix_md5 by earlier file, in parse order",
        identical(m, os(lf(4, "output-unit", "PJ"), i4(4, '<subsector name="coal" nocreate="1">'), i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'),
                        lf(7, "capacity-factor", 0.7), i4(6, "</period>"), i4(5, "</technology>"), i4(4, "</subsector>"))) &&
          identical(last$n[c("values", "restores", "wrappers", "rebuilds", "removals")], c(values = 1, restores = 1, wrappers = 5, rebuilds = 0, removals = 0)) &&
          identical(last$moves, "scenario/world/region[USA]/supplysector[elec]/subsector (2 of 2: b, coal)") &&
          identical(last$prefix_md5, setNames(unname(tools::md5sum(fp)), normalizePath(fp, "/"))))
  m <- suppressMessages(cfg(list(usa(cf(0.5))), usa(cf(0.8)), usa(cf(0.88))))
  check("add-on with config, no answer needed: universal (prefix_md5 NULL)", !startsWith(m, "REFUSED") && is.list(last) && is.null(last$prefix_md5))
  op <- options(minaj.addon_neg0 = FALSE); m <- suppressMessages(cfg(list(usa(tech("a", per(2015, lf(7, "capacity-factor", 0.5))))), qs1, qs0)); options(op)
  check("add-on with config refuses: a holder an earlier file touches is refused there, its ancestors not named (option minaj.addon_neg0 FALSE)", has(m, "REFUSED", "period[2015] is not rebuilt") && !grepl("technology[a] is not rebuilt", m, fixed = TRUE))
  m <- suppressMessages(cfg(list(u0), usa(rule(2020, 2050)), usa(), years = NULL))
  check("add-on with config refuses: rule list emptied, model years not known", has(m, "REFUSED", "interpolation-rule: rule list changed (not an extension of the base file's list); the model years are not known (no <modeltime> in t", ".xml)"))
  op <- options(minaj.addon_neg0 = FALSE)
  m <- suppressMessages(cfg(list(u0), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8), lf(7, "share-weight", 1), inx("x"), inx("y")))), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.9), inx("y"), inx("x"))))))
  options(op)
  check("add-on with config: a rebuilt period replaces the counts and order moves of its inner comparison (option minaj.addon_neg0 FALSE)",
        identical(m, chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" delete="1"/>'), per(2015, lf(7, "capacity-factor", 0.9), inx("y"), inx("x")), i4(5, "</technology>")))) &&
          identical(last$n[c("values", "wrappers", "rebuilds")], c(values = 0, wrappers = 4, rebuilds = 1)) && !length(last$moves))
  m <- suppressMessages(cfg(list(u0), gr(g("g1", 1), g("g3", 3)), gr(g("g1", 1), g("g2", 2), g("g3", 3))))
  check("add-on with config: a suffix rebuild drops the counts of the pieces it replaces", !startsWith(m, "REFUSED") && identical(last$n[c("new", "rebuilds")], c(new = 0, rebuilds = 1)))
  m <- suppressMessages(cfg(list(u0), usa(lf(5, "fuelprefElasticity", 2, ' year="2010"')), usa()))
  check("add-on with config: an array field the override does not write: the restore only, no array count",
        identical(m, chain(lf(5, "fuelprefElasticity", 0, ' year="2010"'))) && identical(last$n[c("restores", "arrays")], c(restores = 1, arrays = 0)))
  m <- suppressMessages(cfg(list(u0), gr(g("g1", 1), g("g3", 3), g("g4", 4)), gr(g("g1", 1), g("g2", 2), g("g3", 3), g("g4", 4))))
  check("add-on with config: a suffix rebuild counts each entry it deletes and writes again",
        identical(m, gra(i4(5, '<grade name="g3" delete="1"/>'), i4(5, '<grade name="g4" delete="1"/>'), g("g2", 2), g("g3", 3), g("g4", 4))) && identical(last$n[["rebuilds"]], 2))
  m <- suppressMessages(cfg(list(usa(i4(5, '<stub-technology name="a"/>'), i4(5, '<stub-technology name="b"/>'))), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), tag = "stub-technology")), usa()))
  check("add-on with config: a stub-technology an earlier file created, deleted and created again (9e): its move against the earlier file's entry reported",
        identical(m, chain(c(i4(5, '<stub-technology name="a" delete="1"/>'), i4(5, '<stub-technology name="a"/>')))) &&
          identical(last$moves, "scenario/world/region[USA]/supplysector[elec]/subsector[coal]/stub-technology (2 of 2: a, b)"))
  m <- suppressMessages(cfg(list(usa(i4(5, '<stub-technology name="a"/>'), i4(5, '<stub-technology name="b"/>'))), qt1, qt0))
  check("add-on with config: a stub replace (9l) of a stub-technology an earlier file created: its move against the earlier file's entry reported",
        identical(m, qtr) && identical(last$moves, "scenario/world/region[USA]/supplysector[elec]/subsector[coal]/stub-technology (2 of 2: a, b)"))
  m <- suppressMessages(cfg(list(usa(i4(5, '<stub-technology name="b"/>'), i4(5, '<stub-technology name="a"/>'))), qt1, qt0))
  check("add-on with config: a stub replace of the earlier files' last stub-technology: no move", identical(m, qtr) && !length(last$moves))
  # restored defaults: one entry per value written from the rules file's default, none for an earlier file's value, none
  # left for what a rebuild or a suffix rebuild replaces, and each listed once when the group is compared again
  dp <- "scenario/world/region[USA]/supplysector[elec]/subsector[coal]"
  m <- suppressMessages(cfg(list(u0), usa(cf(0.8)), usa(tech("a", per(2015)))))
  check("add-on with config: restored defaults: a value only in the base file, earlier files untouched (9a)",
        identical(m, macw(lf(7, "capacity-factor", 1))) && identical(last$defaults, paste0(dp, "/technology[a]/period[2015] capacity-factor = 1")))
  m <- suppressMessages(cfg(list(usa(cf(0.7))), usa(cf(0.8)), usa(tech("a", per(2015)))))
  check("add-on with config: restored defaults: none for an earlier file's value", identical(m, macw(lf(7, "capacity-factor", 0.7))) && identical(last$defaults, character(0)) && identical(last$n[["restores"]], 1))
  op <- options(minaj.addon_neg0 = TRUE); m <- suppressMessages(cfg(list(u0), qs1, qs0)); options(op)
  check("add-on with config: restored defaults: an unset value written -0 (option minaj.addon_neg0 TRUE)",
        identical(m, macw(lf(7, "share-weight", "-0"))) && identical(last$defaults, paste0(dp, "/technology[a]/period[2015] share-weight = -0")))
  m <- suppressMessages(cfg(list(usa(lf(5, "fuelprefElasticity", 4, ' year="1975"'), lf(5, "fuelprefElasticity", 5, ' fillout="1" year="2005"'))),
                            usa(lf(5, "fuelprefElasticity", 2, ' fillout="1" year="1975"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"'))))
  check("add-on with config: restored defaults: array years, only those no earlier file sets (9b)",
        has(m, '<fuelprefElasticity year="1990">0</fuelprefElasticity>') && identical(last$defaults, paste0(dp, " fuelprefElasticity[1990] = 0")) && identical(last$n[["restores"]], 6))
  op <- options(minaj.addon_neg0 = FALSE); m <- suppressMessages(cfg(list(u0), qs1, usa(tech("a", per(2015))))); options(op)
  check("add-on with config: restored defaults: none left for a rebuilt period (capacity-factor restored before the unset share-weight asks for the rebuild; option minaj.addon_neg0 FALSE)",
        identical(m, chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" delete="1"/>'), per(2015), i4(5, "</technology>")))) &&
          identical(last$defaults, character(0)) && identical(last$n[c("restores", "rebuilds")], c(restores = 0, rebuilds = 1)))
  m <- suppressMessages(cfg(list(u0), qs1, usa(tech("a", per(2015)))))
  check("add-on with config: restored defaults: both values restored, no rebuild (option minaj.addon_neg0 not set: share-weight -0)",
        !startsWith(m, "REFUSED") && has(m, lf(7, "capacity-factor", 1), lf(7, "share-weight", "-0")) && !grepl("delete=", m, fixed = TRUE) &&
          setequal(last$defaults, paste0(dp, "/technology[a]/period[2015] ", c("capacity-factor = 1", "share-weight = -0"))) && identical(last$n[c("restores", "rebuilds")], c(restores = 2, rebuilds = 0)))
  m <- suppressMessages(cfg(list(u0), gr(g("g1", 1), c(i4(5, '<grade name="g3">'), lf(6, "available", 5), lf(6, "extractioncost", 3), i4(5, "</grade>"))), gr(g("g1", 1), g("g2", 2), g("g3", 3))))
  check("add-on with config: restored defaults: none left for a grade the suffix rebuild writes again (9k)",
        identical(m, gra(i4(5, '<grade name="g3" delete="1"/>'), g("g2", 2), g("g3", 3))) && identical(last$defaults, character(0)) && identical(last$n[["restores"]], 0))
  ev <- new.env(); ev$n <- 0L                                                   # prefix_scan calls: the exists question, then the creator questions asked once answers exist
  suppressMessages(trace("prefix_scan", where = ns, print = FALSE, tracer = bquote(assign("n", get("n", envir = .(ev)) + 1L, envir = .(ev)))))
  m <- suppressMessages(cfg(list(u0), usa(tech("a", at = ' nocreate="1"'), tech("a", per(2015, lf(7, "capacity-factor", 0.8)))), usa(tech("a", at = ' nocreate="1"'), tech("a", per(2015)))))
  suppressMessages(untrace("prefix_scan", where = ns))
  check("add-on with config: restored defaults: listed once when the group is compared again after it was restored (9h, absent after the earlier files)",
        identical(m, macw(lf(7, "capacity-factor", 1))) && identical(last$defaults, paste0(dp, "/technology[a]/period[2015] capacity-factor = 1")) && identical(last$n[["restores"]], 1) && ev$n >= 2L)
  ev <- new.env(); ev$q <- character(0)
  suppressMessages(trace("prefix_scan", where = ns, print = FALSE, tracer = bquote(assign("q", c(get("q", envir = .(ev)), questions), envir = .(ev)))))
  m <- suppressMessages(cfg(list(usa(cf(0.72))), usa(cf(0.8)), usa(tech("a", per(2015))))); suppressMessages(untrace("prefix_scan", where = ns))
  check("add-on with config: no creator question at a region (one class; it would read every earlier file's whole region)",
        identical(m, macw(lf(7, "capacity-factor", 0.72))) && length(ev$q) > 0L && !any(grepl("^creator\t[^\n]*$", ev$q)))
  m <- withCallingHandlers(cfg(list(usa(cf(0.73))), usa(cf(0.8)), usa(tech("a", per(2015)))), message = function(e) {
    if (startsWith(conditionMessage(e), "minaj: read ")) cat("\n", file = file.path(d, sprintf("p%d_1.xml", k)), append = TRUE); invokeRestart("muffleMessage") })
  check("addon_write with config: an earlier file that changes during the call stops it, nothing written",
        has(m, "REFUSED: addon_minaj: ", sprintf("p%d_1.xml changed while the add-on was being written; run the call again", k)) && !file.exists(file.path(d, sprintf("a%d.xml", k))))
  m <- aw(st2(per(2015, lf(7, "capacity-factor", 0.8), lf(7, "share-weight", 1))), st2(per(2015)), warning.length = 8170)
  check("refuses without config: each place inside one stub-technology listed",
        has(m, "stub-technology[a]/period[2015]/capacity-factor: value set only in the base file (un-set); config= can decide it from the configuration's earlier files",
            "stub-technology[a]/period[2015]/share-weight: value set only in the base file (un-set); config= can decide it from the configuration's earlier files", "2 place(s)"))

  # addon_minaj and status_minaj with config, the gcamdata side of the call replaced: the 'addon' record kept by the md5s
  # of the earlier files, repeat calls, the report and header comment, and status_minaj's lines on add-ons it loads
  ca <- suppressMessages(cfg(list(usa(cf(0.7)), u0), doc(reg("USA", sec(sbs("coal", cf(0.8)), sbs("b")))), doc(reg("USA", sec(lf(4, "output-unit", "PJ"), sbs("b"), sbs("coal", tech("a", per(2015))))))))
  ka <- k; ha <- readLines(file.path(d, sprintf("a%d.xml", ka)), 2)[2]
  cb <- suppressMessages(cfg(list(usa(cf(0.5))), usa(cf(0.8)), usa(cf(0.88)))); kb <- k; hb <- readLines(file.path(d, sprintf("a%d.xml", kb)), 2)[2]
  check("add-on with config: the header comment counts the earlier files of a configuration-specific add-on, and only then",
        endsWith(ha, "; override inputs: energy/A23.globaltech_capacity_factor; configuration-specific: 3 earlier files -->") && endsWith(hb, "; override inputs: energy/A23.globaltech_capacity_factor -->"))
  cd1 <- suppressMessages(cfg(list(), usa(lf(5, "fuelprefElasticity", 2, ' fillout="1" year="1975"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"'))))   # one earlier file: the modeltime file
  kd <- k; hd <- readLines(file.path(d, sprintf("a%d.xml", kd)), 2)[2]; dd <- paste0(dp, " fuelprefElasticity[", c(1975, 1990, 2005, 2015, 2020, 2025), "] = 0")
  check("add-on with config, one earlier file: the header comment says 1 earlier file; restored defaults listed per year",
        identical(cd1, chain(c(lf(5, "fuelprefElasticity", 0, sprintf(' year="%d"', c(1975, 1990, 2005, 2015, 2020, 2025))), lf(5, "fuelprefElasticity", 3, ' year="2010"')))) &&
          endsWith(hd, "; override inputs: energy/A23.globaltech_capacity_factor; configuration-specific: 1 earlier file -->") && identical(last$defaults, dd))
  am <- get("addon_minaj", ns); sm <- get("status_minaj", ns)
  mk <- list(enter = function() function() NULL, target_kind = function(x) "xml", plan_targets = function(targets, override = list()) list(chunks = character(0)),
             downstream_chunks = function(from) character(0), dest_dir = function(dest = NULL) d, xml_dir = function() d, writer_key = function() "unit",
             tree_head = function() NA_character_, xml_paths_in_config = function(config) character(0), value_hash = function(name, ctx) get("h64", ns)(list(name, ctx$over)),
             make_one = function(x, ctx, dest, file = NULL, override = FALSE) list(file = file.path(d, if (override) sub("^b", "o", x) else x)))
  o0 <- mget(names(mk), envir = ns); t0 <- nm$tree; nm$tree <- NULL; op <- options(minaj.ram_floor_gb = 0, minaj.addon_max_ratio = Inf)
  for (x in names(mk)) { if (bindingIsLocked(x, ns)) unlockBinding(x, ns); assign(x, mk[[x]], envir = ns) }
  fc <- normalizePath(file.path(d, sprintf("c%d.xml", c(ka, kb))), "/"); xa <- sprintf("b%d.xml", c(ka, kb)); fa <- file.path(d, sprintf("b%d_addon.xml", c(ka, kb)))
  cl <- lapply(fc, readLines); ib <- vapply(seq_along(fc), function(i) grep(paste0(">", xa[i], "<"), cl[[i]], fixed = TRUE), 1L)
  ad <- sprintf('        <Value name="b%d_addon">b%d_addon.xml</Value>', c(ka, kb), c(ka, kb))
  st5 <- list(quote(am(xa[1], list(x = 1), config = fc[1], dest = d, ask = FALSE)),             # 1: written, configuration-specific
              quote(writeLines(append(cl[[1]], ad[1], ib[1]), fc[1])),                         # 2: the add-on's line after the base file
              quote(am(xa[1], list(x = 1), config = fc[1], dest = d, ask = FALSE)),             # 3: unchanged
              quote(sm(fc[1])),                                                                # 4: current
              quote(cat("\n", file = file.path(d, sprintf("p%d_1.xml", ka)), append = TRUE)),  # 5: an earlier file changes
              quote(sm(fc[1])),                                                                # 6: stale
              quote(am(xa[1], list(x = 1), config = fc[1], dest = d, ask = FALSE)),             # 7: written again
              quote(sm(fc[1])),                                                                # 8: current
              quote(am(xa[1], list(x = 1), config = fc[1], dest = d, ask = FALSE)),             # 9: unchanged
              quote(writeLines(append(cl[[1]], ad[1], ib[1] - 1L), fc[1])),                    # 10: the add-on's line before the base file
              quote(sm(fc[1])),                                                                # 11: not after its base file
              quote(am(xa[2], list(x = 1), config = fc[2], dest = d, ask = FALSE)),             # 12: written, universal
              quote(writeLines(append(cl[[2]], ad[2], ib[2]), fc[2])),                         # 13
              quote(sm(fc[2])),                                                                # 14: universal, current
              quote(am(xa[2], list(x = 1), dest = d, ask = FALSE)))                            # 15: unchanged without config
  k <- k + 1L; kc <- k; k <- k + 1L; kp <- k                                    # c<kc>.xml: c<ka>.xml with its earlier files copied to t<kc>, p<kc>_<i>; c<kp>.xml: a padded path
  pk <- file.path(d, c(sprintf("t%d.xml", ka), sprintf("p%d_%d.xml", ka, 1:2))); pc <- file.path(d, c(sprintf("t%d.xml", kc), sprintf("p%d_%d.xml", kc, 1:3)))
  fcc <- normalizePath(file.path(d, sprintf("c%d.xml", kc)), "/", mustWork = FALSE); fcp <- file.path(d, sprintf("c%d.xml", kp))
  cc1 <- cl[[1]]; for (z in c(">t%d.xml<", ">p%d_", "\"p%d_")) cc1 <- gsub(sprintf(z, ka), sprintf(z, kc), cc1, fixed = TRUE)
  ic <- grep(sprintf(">p%d_1.xml<", kc), cc1, fixed = TRUE); cca <- append(cc1, ad[1], ib[1])
  st5 <- c(st5, list(quote({ a1 <- readLines(fa[1]); file.copy(pk, pc[1:3], overwrite = TRUE); writeLines(cc1, fcc) }),  # 16: byte copies of the earlier files at other paths
                     quote(am(xa[1], list(x = 1), config = fcc, dest = d, ask = FALSE)),            # 17: unchanged
                     quote(writeLines(cca, fcc)),                                                  # 18
                     quote(sm(fcc)),                                                               # 19: current
                     quote(writeLines(usa(cf(0.6)), pc[2])),                                       # 20: the copy's first earlier file sets another value
                     quote(am(xa[1], list(x = 1), config = fcc, dest = d, ask = FALSE)),            # 21: written, replaces the add-on of c<ka>.xml
                     quote(writeLines(append(cl[[1]], ad[1], ib[1]), fc[1])),                      # 22
                     quote(sm(fc[1])),                                                             # 23: stale for c<ka>.xml
                     quote(writeLines(append(cca, sprintf('        <Value name="p%d_1">p%d_1.xml</Value>', ka, ka), ic - 1L), fcc)),   # 24: a file inserted before the earlier files
                     quote(sm(fcc)),                                                               # 25: stale, the inserted file only
                     quote(writeLines(append(cca, sprintf('        <Value name="p%d_3">p%d_3.xml</Value>', kc, kc), ic), fcc)),        # 26: a missing earlier file
                     quote(sm(fcc)),                                                               # 27: missing file line, stale
                     quote(writeLines(sub(sprintf(">p%d_1.xml<", ka), sprintf("> p%d_1.xml<", ka), cl[[1]], fixed = TRUE), fcp)),
                     quote(sm(fcp)),                                                               # 29: add-ons not checked
                     quote({ tf <- tempfile(fileext = ".xml"); file.copy(fa[1], tf)                # 30: a second add-on to the same base file after it
                             writeLines(append(cl[[1]], c(ad[1], sprintf('        <Value name="b%d_addon">%s</Value>', ka, normalizePath(tf, "/"))), ib[1]), fc[1])
                             am(xa[1], list(x = 1), config = fc[1], dest = d, ask = FALSE) })))
  fcd <- normalizePath(file.path(d, sprintf("c%d.xml", kd)), "/"); cld <- readLines(fcd); ibd <- grep(sprintf(">b%d.xml<", kd), cld, fixed = TRUE)
  st5 <- c(st5, list(quote(am(sprintf("b%d.xml", kd), list(x = 1), config = fcd, dest = d, ask = FALSE)),    # 31: written, configuration-specific, one earlier file
                     quote(am(sprintf("b%d.xml", kd), list(x = 1), config = fcd, dest = d, ask = FALSE)),    # 32: unchanged (the record's result)
                     quote(writeLines(append(cld, sprintf('        <Value name="b%d_addon">b%d_addon.xml</Value>', kd, kd), ibd), fcd)),
                     quote(sm(fcd))))                                                                       # 34: current
  ms <- rv <- vector("list", length(st5))
  for (i in seq_along(st5)) {
    mi <- character(0)
    rv[i] <- list(withCallingHandlers(tryCatch(eval(st5[[i]]), error = function(e) paste("ERROR:", conditionMessage(e))),
                                      message = function(e) { mi <<- c(mi, conditionMessage(e)); invokeRestart("muffleMessage") }))
    ms[[i]] <- paste(mi, collapse = "")
  }
  for (x in names(o0)) assign(x, o0[[x]], envir = ns); nm$tree <- t0; options(op); for (i in seq_along(fc)) writeLines(cl[[i]], fc[i]); writeLines(cld, fcd)
  for (i in which(vapply(rv, is.character, NA))) cat("  step", i, rv[[i]], "\n")
  sv <- vapply(rv, function(r) if (is.data.frame(r)) r$status else "", "")
  ra <- Filter(function(r) identical(r$xml, xa[1]), lapply(list.files(file.path(nm$store, "rec", "addon"), "[.]rds$", recursive = TRUE, full.names = TRUE), readRDS))
  pa <- file.path(d, c(sprintf("t%d.xml", ka), sprintf("p%d_%d.xml", ka, 1:2)))
  check("addon_minaj with config: a configuration-specific add-on is written and reported as such, with the line for the configuration",
        identical(sv[1], "written") && identical(paste(a1[-(1:2)], collapse = "\n"), ca) && endsWith(a1[2], "; override inputs: x; configuration-specific: 3 earlier files -->") &&
          has(ms[[1]], sprintf("minaj: configuration-specific add-on, valid directly after this exact base file in c%d.xml while the 3 files loaded before it keep their content (status_minaj(config) checks this): ", ka),
              sprintf('Add this line directly after the base file in c%d.xml:\n  <Value name="b%d_addon">b%d_addon.xml</Value>', ka, ka, ka)) && !grepl("universal", ms[[1]], fixed = TRUE))
  check("addon_minaj with config: a repeat call is unchanged; after an earlier file changes, written again, then unchanged",
        identical(sv[c(3, 7, 9)], c("unchanged", "written", "unchanged")) && has(ms[[3]], "configuration-specific add-on, valid directly after"))
  check("addon_minaj with config: one 'addon' record per set of earlier-file md5s, prefix_md5 in result",
        length(ra) == 3L && all(vapply(ra, function(r) length(r$result$prefix_md5) == 3L, NA)) && !anyDuplicated(lapply(ra, function(r) unname(r$result$prefix_md5))) &&
          identical(sort(vapply(ra, function(r) if (identical(names(r$result$prefix_md5), normalizePath(pa, "/"))) "a" else if (identical(names(r$result$prefix_md5), normalizePath(pc[1:3], "/"))) "c" else "", "")), c("a", "a", "c")) &&
          all(vapply(ra, function(r) identical(unname(r$result$prefix_md5[c(1, 3)]), unname(tools::md5sum(pa[c(1, 3)]))), NA)))
  check("status_minaj: a configuration-specific add-on current, then stale naming the changed earlier file, then current again",
        has(ms[[4]], sprintf("minaj: b%d_addon.xml: configuration-specific add-on to b%d.xml, current (the 3 files loaded before the base file are unchanged)", ka, ka)) &&
          has(ms[[6]], sprintf("minaj: b%d_addon.xml: configuration-specific add-on to b%d.xml, stale: 1 file(s) loaded before the base file changed since it was written (p%d_1.xml); run addon_minaj() with config = \"%s\" again", ka, ka, ka, fc[1])) &&
          has(ms[[8]], "current (the 3 files loaded before the base file are unchanged)") && !grepl("stale", ms[[8]], fixed = TRUE))
  check("status_minaj: an add-on not loaded directly after its base file",
        has(ms[[11]], sprintf("minaj: b%d_addon.xml: add-on to b%d.xml, not loaded directly after that base file (or the base file changed)", ka, ka)) && !grepl("current", ms[[11]], fixed = TRUE))
  check("addon_minaj with config, no earlier-file answer needed: universal, reported as such; status_minaj current; a call without config finds its record",
        identical(sv[c(12, 15)], c("written", "unchanged")) && has(ms[[12]], "minaj: universal add-on, valid directly after this exact base file in any configuration that loads it: ") &&
          !grepl("configuration-specific", paste(ms[[12]], readLines(fa[2], 2)[2]), fixed = TRUE) && has(ms[[14]], sprintf("minaj: b%d_addon.xml: universal add-on to b%d.xml, current", kb, kb)))
  check("addon_minaj and status_minaj with config: byte copies of the earlier files at other paths: unchanged, current",
        identical(sv[17], "unchanged") && has(ms[[19]], sprintf("minaj: b%d_addon.xml: configuration-specific add-on to b%d.xml, current (the 3 files loaded before the base file are unchanged)", ka, ka)))
  check("addon_minaj with config: replacing a configuration-specific add-on says it is valid only after that configuration's earlier files",
        identical(sv[21], "written") && !identical(paste(readLines(fa[1])[-(1:2)], collapse = "\n"), ca) &&
          has(ms[[21]], sprintf("minaj: replaced the earlier b%d_addon.xml in %s (md5 ", ka, d),
              sprintf("); it is valid only after the earlier files of c%d.xml, so check any other configuration that loads it with status_minaj(config). Use dest = to keep add-ons for several overrides or one add-on per configuration", kc)) &&
          !grepl("every configuration that loads it now gets this override", ms[[21]], fixed = TRUE) &&
          has(ms[[23]], sprintf("minaj: b%d_addon.xml: configuration-specific add-on to b%d.xml, stale: 1 file(s) loaded before the base file changed since it was written (p%d_1.xml)", ka, ka, ka)))
  check("status_minaj: a file added to the earlier files counts once; a missing earlier file is listed",
        has(ms[[25]], sprintf("stale: 1 file(s) loaded before the base file changed since it was written (p%d_1.xml); run addon_minaj()", ka)) &&
          has(ms[[27]], sprintf("minaj: c%d.xml lists %s, which does not exist; GCAM stops when it cannot open a component", kc, normalizePath(pc[4], "/", mustWork = FALSE)),
              sprintf("stale: 1 file(s) loaded before the base file changed since it was written (p%d_3.xml)", kc)))
  check("status_minaj: a configuration minaj cannot read: add-ons not checked, no error",
        has(ms[[29]], sprintf('minaj: add-ons in c%d.xml not checked: c%d.xml, ScenarioComponents value "p%d_1": GCAM would open the path with the surrounding spaces (" p%d_1.xml")', kp, kp, ka, ka)) &&
          !is.character(rv[[29]]) && !grepl("add-on to", ms[[29]], fixed = TRUE))
  check("addon_minaj with config: refused when another add-on to the same base file is loaded after it",
        is.character(rv[[30]]) && startsWith(rv[[30]], "ERROR: ") && has(rv[[30]], paste0("another add-on to this base file is loaded after it (", normalizePath(tf, "/"), "); combine the overrides into one call")))
  ed <- paste0("restored values 6\nminaj: restored defaults (6): ", paste(dd[1:5], collapse = "; "), "; ...\n")
  check("addon_minaj with config: the restored defaults line after the counts (first 5, then ...), also for an add-on found in the records",
        identical(sv[c(31, 32)], c("written", "unchanged")) && has(ms[[31]], ed) && has(ms[[32]], ed))
  check("addon_minaj and status_minaj with config, one earlier file: the final message and status_minaj say 1 file",
        has(ms[[31]], sprintf("in c%d.xml while the 1 file loaded before it keeps its content (status_minaj(config) checks this): ", kd)) &&
          has(ms[[34]], sprintf("minaj: b%d_addon.xml: configuration-specific add-on to b%d.xml, current (the 1 file loaded before the base file is unchanged)", kd, kd)))
  unlink(tf)
  nm$store <- s0[[1]]; nm$state_dir <- s0[[2]]; nm$findex <- s0[[3]]

  # ---- addon_sim ----------------------------------------------------------------------------------------------
  # Every add-on above passed the check inside addon_write. Here base (B), add-on (A) and override (O) are given
  # directly: correct add-ons pass, deliberately wrong ones (negative controls) are caught at the place named.
  addon_sim <- get("addon_sim", ns)
  sim <- function(B, A, O) {                                      # "ok", or every "place: what" found
    k <<- k + 1L; f <- file.path(d, paste0(c("sb", "sa", "so"), k, ".xml"))
    for (i in 1:3) writeBin(charToRaw(paste(list(B, A, O)[[i]], collapse = "\n")), f[i])
    last <<- addon_sim(f[1], f[2], f[3])
    if (last$ok) "ok" else paste0(last$mismatches$target, ": ", last$mismatches$column, " (", last$mismatches$old, " | ", last$mismatches$new, ")", collapse = "\n")
  }
  stub <- function(v, at = "") chain(c(i4(5, paste0('<stub-technology name="a"', at, ">")), per(2015, lf(7, "capacity-factor", v)), i4(5, "</stub-technology>")))
  stb <- function(v) usa(cf(v, tag = "stub-technology"))
  check("check: correct add-on passes", identical(sim(stb(0.8), stub(0.88, ' nocreate="1"'), stb(0.88)), "ok"))
  check("check: verdict is ok and a mismatches table (target, column, old, new)", {
    m <- sim(stb(0.8), stub(0.89, ' nocreate="1"'), stb(0.88))
    !last$ok && identical(names(last$mismatches), c("target", "column", "old", "new")) &&
      identical(m, "scenario/world/region[USA]/supplysector[elec]/subsector[coal]/stub-technology[a]/period[2015]/capacity-factor: value (0.89 | 0.88)") })
  check("negative control: change missing (empty add-on)", has(sim(stb(0.8), c("<scenario>", "</scenario>"), stb(0.88)), "period[2015]/capacity-factor: value (0.8 | 0.88)"))
  check("negative control: new technology written with nocreate (skipped when nothing created it)",
        has(sim(usa(cf(0.8)), chain(tech("b", per(2015, lf(7, "capacity-factor", 0.7)), at = ' nocreate="1"')), usa(cf(0.8), tech("b", per(2015, lf(7, "capacity-factor", 0.7))))),
            "technology[b]: existence (if no earlier file creates it) (absent | created)"))
  em <- function(v) usa(tech("a", per(1975, lf(7, "capacity-factor", v)), tag = "stub-technology", at = ' nocreate="1"'))
  check("check: stub-technology from earlier files, wrapper with nocreate passes",
        identical(sim(em(1), chain(tech("a", per(1975, lf(7, "capacity-factor", 2)), tag = "stub-technology", at = ' nocreate="1"')), em(2)), "ok"))
  check("negative control: stub-technology from earlier files, wrapper without nocreate",
        has(sim(em(1), chain(tech("a", per(1975, lf(7, "capacity-factor", 2)), tag = "stub-technology")), em(2)), "stub-technology[a]: existence (if no earlier file creates it) (created | absent)"))
  check("negative control: rule list changed, add-on appends", has(sim(usa(rule(2020, 2050)), chain(rule(2020, 2060)), usa(rule(2020, 2060))), "interpolation-rule: rule list (2 rule(s) | 1 rule(s))"))
  check("negative control: rule list changed, add-on clears (rules from earlier files would go too)",
        has(sim(usa(rule(2020, 2050)), chain(rule(2020, 2060, at = ' delete="1"')), usa(rule(2020, 2060))), "interpolation-rule: rule list (if an earlier file creates it) (cleared; 1 rule(s) | 1 rule(s))"))
  kwa <- function(v) chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, paste0('<keyword primary-renewable="', v, '"/>')), i4(6, "</period>"), i4(5, "</technology>")))
  check("negative control: keyword changed (GCAM keeps the first value)", has(sim(kw(' primary-renewable="x"'), kwa("y"), kw(' primary-renewable="y"')), "period[2015]/keyword: keywords (primary-renewable=x | primary-renewable=y)"))
  check("negative control: grade inserted by the override, appended by the add-on", has(sim(gr(g("g1", 1), g("g3", 3)), gra(g("g2", 2)), gr(g("g1", 1), g("g2", 2), g("g3", 3))), "subresource[c]/grade: entry order (g1; g3; g2 | g1; g2; g3)"))
  check("check: grade appended passes", identical(sim(gr(g("g1", 1), g("g2", 2)), gra(g("g3", 3)), gr(g("g1", 1), g("g2", 2), g("g3", 3))), "ok"))
  swf <- function(v, fo = ' fillout="1"') lf(5, "share-weight", v, paste0(fo, ' year="1975"'))
  check("negative control: fillout value written without fillout (array replay off)", has(sim(usa(swf(1)), chain(swf(2, "")), usa(swf(2))), "subsector[coal]/share-weight: values by year (1975+=1; 1975=2 | 1975+=2)"))
  check("check: year written after a fillout, alone, passes (any model years)", identical(sim(usa(sw), chain(lf(5, "share-weight", 0.5, ' year="2025"')), usa(sw, lf(5, "share-weight", 0.5, ' year="2025"'))), "ok"))
  check("negative control: delete and rebuild where the override merges (earlier-file check off)",
        has(sim(usa(cf(0.8)), chain(c(i4(5, '<technology name="a" delete="1"/>'), cf(0.9))), usa(cf(0.9))), "technology[a]: existence (if an earlier file creates it) (created | merged into the earlier one)"))
  check("negative control: type change written as a merge", has(sim(usa(cf(0.8)), chain(cf(0.8, tag = "intermittent-technology", at = ' nocreate="1"')), usa(cf(0.8, tag = "intermittent-technology"))),
                                                                "intermittent-technology[a]: created as (if no earlier file creates it) (technology | intermittent-technology)"))
  cf1 <- cbp(smap("Food", "Agri"), smap("Grains", "Agri")); cf2 <- cbp(smap("Food", "FoodAgg"), smap("Grains", "Agri"))
  check("negative control: calc-base-price with a to-name changed, override block added (the base block's price for Grains stays)",
        has(sim(doc(reg("USA"), cf1), top(cf2), doc(reg("USA"), cf2)), "scenario/calc-base-price: list without keys (2 element(s) | 1 element(s))"))
  check("check: calc-base-price keeping every (from-name, to-name) pair, override block added, passes", identical(sim(doc(reg("USA"), cb1), top(cb4), doc(reg("USA"), cb4)), "ok"))
  check("negative control: modeltime in the add-on (GCAM reads it once)", has(sim(doc(reg("USA"), mt(2100)), top(mt(2300)), doc(reg("USA"), mt(2300))), "scenario/modeltime: element read once"))
  check("negative control: input flag missing", has(sim(inp(lf(8, "flag", "Energy")), chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), i4(7, '<minicam-energy-input name="coal" nocreate="1">'),
                                                         lf(8, "coefficient", 2), i4(7, "</minicam-energy-input>"), i4(6, "</period>"), i4(5, "</technology>"))), inp(lf(8, "flag", "Energy"), lf(8, "flag", "Resource"), lf(8, "coefficient", 2))),
                                                    "minicam-energy-input[coal]/flag: input flags"))
  check("negative control: global technology value wrong", has(sim(doc(c(reg("USA"), gtd(0.8))), doc(gtd(0.85)), doc(c(reg("USA"), gtd(0.88)))), "location-info[elec, coal]/technology[a]/period[2015]/capacity-factor: value (0.85 | 0.88)"))
  check("negative control: input without a name appended by the add-on where the override puts it first (order-critical)",
        has(sim(fst(inx("a")), chain(c(i4(5, '<food-storage-technology name="f" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), ui, i4(6, "</period>"), i4(5, "</food-storage-technology>"))),
                fst(ui, inx("a"))), "period[2015]/minicam-energy-input: entry order"))
  check("negative control: input inserted before another inside a stub-technology, appended by the add-on",
        has(sim(st2(per(2015, inx("a"))), chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), i4(6, '<period year="2015">'), inx("c"), i4(6, "</period>"), i4(5, "</stub-technology>"))),
                st2(per(2015, inx("c"), inx("a")))), "minicam-energy-input: entry order"))
  check("negative control: tax 10.0 appended by the add-on with the override's first value (GCAM keeps the last value per number)",
        has(sim(macv(mr("10", 0.3)), mrw(mr("10.0", 0.5)), macv(mr("10.0", 0.5), mr("10", 0.3))), "mac-control[m]/mac-reduction: key map"))
  check("check: tax 1e1 in the add-on, 10.0 in the override (one number) passes", identical(sim(macv(mr("10", 0.3)), mrw(mr("1e1", 0.4)), macv(mr("10.0", 0.4))), "ok"))
  check("negative control: period year 02015 in the add-on", has(sim(usa(cf(0.8)), chain(c(i4(5, '<technology name="a" nocreate="1">'), per("02015", lf(7, "capacity-factor", 0.9)), i4(5, "</technology>"))),
                                                                     usa(tech("a", per(2015, lf(7, "capacity-factor", 0.9))))), "period[02015]: year key not a whole number"))
  check("check: more than 2000 differences: a last row counts the rest", {
    tt <- function(v) usa(unlist(lapply(sprintf("t%04d", 1:2010), function(n) tech(n, per(2015, lf(7, "capacity-factor", v))))))
    sim(tt(0.8), c("<scenario>", "</scenario>"), tt(0.9)); m <- last$mismatches
    !last$ok && nrow(m) > 1L && grepl("^\\([0-9]+ more differences not listed\\)$", m$target[nrow(m)]) })
  check("negative control: tag unknown to GCAM in the add-on", has(sim(usa(cf(0.8)), chain(lf(5, "dummy-logit-tag", 1)), usa(cf(0.8))), "subsector[coal]/dummy-logit-tag: tag unknown to GCAM (skipped) in the add-on"))
  check("check: technology and stub-technology of one name (emissions files) passes; one value missing is caught",
        identical(sim(usa(mix(1, 1)), chain(mix(2, 3)), usa(mix(2, 3))), "ok") &&
          has(sim(usa(mix(1, 1)), chain(tech("a", per(1975, lf(7, "capacity-factor", 2)), at = ' nocreate="1"')), usa(mix(2, 3))), "technology[a]/period[1975]/share-weight: value (1 | 3)"))
  s5 <- sim(doc(reg("USA", nst(1))), os(i4(4, '<subsector name="d" nocreate="1">'), lf(5, "share-weight", 3, ' year="1975"'), i4(4, "</subsector>")), doc(reg("USA", nst(2))))
  check("negative control: wrong value in a subsector merging into a nesting-subsector", has(s5, "subsector[d]/share-weight: values by year") && !grepl("tags of different types", s5, fixed = TRUE))
  # rule lists of the same length that differ in content
  s7 <- sim(usa(rule(2020, 2050)), chain(rule(2020, 2070, at = ' delete="1"')), usa(rule(2020, 2050), rule(2020, 2060, at = ' delete="1"')))
  check("negative control: clearing rule with another to-year (same count, both cleared)", has(s7, "interpolation-rule: rule list"))
  check("check: the override's own clearing rule passes", identical(sim(usa(rule(2020, 2050)), chain(rule(2020, 2060, at = ' delete="1"')), usa(rule(2020, 2050), rule(2020, 2060, at = ' delete="1"'))), "ok"))
  # one technology written with two tags; the add-on appends input w where the override puts it before y (input order critical for food-storage and for any technology a stub queues)
  ipn <- function(n, v) c(i4(7, paste0('<minicam-non-energy-input name="', n, '">')), lf(8, "input-cost", v), i4(7, "</minicam-non-energy-input>"))
  two <- function(t1, at, ...) usa(tech("a", per(1975, ipn("x", 1)), tag = t1, at = at), tech("a", per(1975, ...), at = at))
  aww <- chain(c(i4(5, '<technology name="a" nocreate="1">'), per(1975, ipn("w", 3)), i4(5, "</technology>"))); nc <- ' nocreate="1"'
  check("negative control: food-storage-technology then technology of one name (input order critical in the first class only)",
        has(sim(two("food-storage-technology", nc, ipn("y", 2)), aww, two("food-storage-technology", nc, ipn("w", 3), ipn("y", 2))), "food-storage-technology[a]/period[1975]: written with tags of different types"))
  # subresource and technology merging into a reserve-subresource and resource-reserve-technology of one name (emissions files after resources.xml)
  s8 <- doc(reg("USA", c(i4(3, '<resource name="r">'), i4(4, '<reserve-subresource name="s">'), tech("a", per(1975, lf(7, "capacity-factor", 1)), tag = "resource-reserve-technology"),
                         i4(4, "</reserve-subresource>"), i4(4, '<subresource name="s">'),
                         tech("a", per(1975, i4(7, '<Non-CO2 name="CH4">'), i4(8, '<mac-control name="m">'), lf(9, "mac-reduction", 3, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))),
                         i4(4, "</subresource>"), i4(3, "</resource>"))))
  a8 <- top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<resource name="r" nocreate="1">'), i4(4, '<subresource name="s" nocreate="1">'),
            tech("a", per(1975, i4(7, '<Non-CO2 name="CH4" nocreate="1">'), i4(8, '<mac-control name="m" nocreate="1">'), lf(9, "mac-reduction", 4, ' tax="10"'), i4(8, "</mac-control>"), i4(7, "</Non-CO2>")), at = ' nocreate="1"'),
            i4(4, "</subresource>"), i4(3, "</resource>"), i4(2, "</region>"), i4(1, "</world>"))
  s9 <- sim(s8, a8, sub(">3<", ">5<", s8, fixed = TRUE))
  check("check: subresource/technology merging into reserve-subresource/resource-reserve-technology (emissions files after resources.xml) passes; a wrong value is caught at the value",
        identical(sim(s8, a8, sub(">3<", ">4<", s8, fixed = TRUE)), "ok") && has(s9, "mac-control[m]/mac-reduction: key map") && !grepl("tags of different types", s9, fixed = TRUE))
  check("negative control: a year new to the technology, made under technology by the base file and under resource-reserve-technology by the override",
        has(sim(doc(reg("USA", c(i4(3, '<resource name="r">'), i4(4, '<reserve-subresource name="s">'), tech("a", per(1975, lf(7, "capacity-factor", 1)), tag = "resource-reserve-technology"),
                                 i4(4, "</reserve-subresource>"), i4(4, '<subresource name="s">'), tech("a", per(2010, lf(7, "capacity-factor", 3))), i4(4, "</subresource>"), i4(3, "</resource>")))),
                top(i4(1, "<world>"), i4(2, '<region name="USA" nocreate="1">'), i4(3, '<resource name="r" nocreate="1">'), i4(4, '<subresource name="s" nocreate="1">'),
                    tech("a", per(2010, lf(7, "capacity-factor", 4)), at = nc), i4(4, "</subresource>"), i4(3, "</resource>"), i4(2, "</region>"), i4(1, "</world>")),
                doc(reg("USA", c(i4(3, '<resource name="r">'), i4(4, '<reserve-subresource name="s">'),
                                 tech("a", per(1975, lf(7, "capacity-factor", 1)), per(2010, lf(7, "capacity-factor", 4)), tag = "resource-reserve-technology"), i4(4, "</reserve-subresource>"), i4(3, "</resource>"))))),
            "period[2010]: created as (if no earlier file creates it) (DefaultTechnology | ResourceReserveTechnology)"))
  i9 <- usa(tech("a", per(2015, lf(7, "capacity-factor", 1))), tech("a", per(2020, lf(7, "electric-sector-name", "x")), tag = "intermittent-technology"))
  check("check: a year new to a technology takes the class of the technology tag around it (intermittent-technology merging into a technology), correct add-on passes",
        identical(sim(i9, chain(tech("a", per(2020, lf(7, "electric-sector-name", "y")), tag = "intermittent-technology", at = nc)), sub(">x<", ">y<", i9, fixed = TRUE)), "ok"))
  # a subresource's one technology container: a later technology tag reuses it and its name attribute renames it; each period read takes the name it has then
  s10 <- gr(tech("b", per(2015, lf(7, "capacity-factor", 0.8)), per(2020, lf(7, "capacity-factor", 0.9))))
  check("negative control: a subresource's technology renamed by the add-on, the period read before keeps the base file's name",
        identical(sim(gr(cf(0.8)), gra(tech("b", per(2020, lf(7, "capacity-factor", 0.9)), at = nc)), s10),
                  "scenario/world/region[USA]/resource[coal]/subresource[c]/technology/period[2015]/name: value (a | b)"))
  check("check: a subresource's technology renamed by the add-on with every period read again passes; a wrapper without a name keeps the base file's name; a period's own name attribute is not read",
        identical(sim(gr(cf(0.8)), gra(tech("b", per(2015), per(2020, lf(7, "capacity-factor", 0.9)), at = nc)), s10), "ok") &&
          identical(sim(gr(cf(0.8)), gra(i4(5, '<technology nocreate="1">'), per(2015, lf(7, "capacity-factor", 0.9)), i4(5, "</technology>")),
                        sub('<period year="2015">', '<period year="2015" name="x">', gr(cf(0.9)), fixed = TRUE)), "ok"))
  check("refuses: a subresource's technology renamed by the override", has(aw(gr(cf(0.8)), gr(tech("b", per(2015, lf(7, "capacity-factor", 0.8))))), "technology/name: technology renamed"))
  check("refuses: a later technology tag of the override renames a subresource's technology before reading a new period",
        { m <- aw(gr(cf(0.8)), gr(cf(0.8), tech("b", per(2020, lf(7, "capacity-factor", 0.9))))); has(m, "technology/name: technology renamed") && never(m) })
  check("refuses: a subresource's technology renamed by the override, which also writes it with a second technology class",
        { m <- aw(gr(cf(0.8)), gr(tech("b", per(2015, lf(7, "capacity-factor", 0.8))), tech("b", per(2025, lf(7, "capacity-factor", 0.8)), tag = "intermittent-technology")))
          has(m, "technology/name: technology renamed") && never(m) })
  check("refuses: a <name> child renames a sector's technology", { m <- aw(usa(cf(0.8)), usa(tech("a", lf(6, "name", "b"), per(2015, lf(7, "capacity-factor", 0.8))))); has(m, "technology[a]/name: technology renamed") && never(m) })
  check("refuses: a <name> child inside a technology's period",
        { m <- aw(usa(tech("a", per(2015, lf(7, "name", "x"), lf(7, "capacity-factor", 0.8)))), usa(tech("a", per(2015, lf(7, "name", "x"), lf(7, "capacity-factor", 0.9)))))
          has(m, "technology[a]/period[2015]/name: <name> inside a technology's period") && never(m) })
  check("add-on: a period's own name attribute is not read (the same add-on as without it)",
        identical(aw(sub('<period year="2015">', '<period year="2015" name="x">', usa(cf(0.8)), fixed = TRUE), usa(cf(0.9))), aw(usa(cf(0.8)), usa(cf(0.9)))))
  t0 <- c(i4(5, "<technology>"), per(2015, lf(7, "capacity-factor", 0.9)), i4(5, "</technology>"))
  check("refuses: a subresource's technology named only in the override (config= can decide it)", { m <- aw(gr(t0), gr(cf(0.95))); has(m, "technology/name: technology renamed") && step2(m) })
  check("add-on with config: a subresource's technology named only in the override, with the name an earlier file gave it",
        { m <- cfg(list(gr(cf(0.8))), gr(t0), gr(cf(0.95))); !startsWith(m, "REFUSED") && has(m, "<capacity-factor>0.95</capacity-factor>") && !grepl("<name>", m, fixed = TRUE) })
  check("add-on with config: a subresource's technology only in the base file, restored without writing its name",
        { m <- cfg(list(gr(g("g1", 1), cf(0.5))), gr(g("g1", 1), cf(0.8)), gr(g("g1", 1))); !startsWith(m, "REFUSED") && has(m, "<capacity-factor>0.5</capacity-factor>") && !grepl("<name>", m, fixed = TRUE) })
  check("refuses with config: the base file and the override both rename a subresource's technology an earlier file named, before any period read",
        { m <- cfg(list(gr(cf(0.5))), gr(tech("b", per(2015, lf(7, "capacity-factor", 0.8)), per(2020, lf(7, "capacity-factor", 0.8)))), gr(tech("b", per(2020, lf(7, "capacity-factor", 0.9)))))
          has(m, "technology/name: technology renamed") && never(m) })
  check("add-on with config: a subresource's technology with the name an earlier file gave it",
        { m <- cfg(list(gr(cf(0.5))), gr(cf(0.8)), gr(cf(0.9))); !startsWith(m, "REFUSED") && has(m, "<capacity-factor>0.9</capacity-factor>") })
  check("add-on with config: a subresource's technology an earlier file also writes with nocreate on the way (no question asked: no year only in the base file)",
        identical(cfg(list(gr(cf(0.5)), gra(tech("a", per(2015, lf(7, "capacity-factor", 0.6)), at = nc))), gr(cf(0.8)), gr(cf(0.88))), aw(gr(cf(0.8)), gr(cf(0.88)))))
  check("add-on with config: the base file and the override both rename a subresource's technology an earlier file named, reading the same periods",
        identical(cfg(list(gr(cf(0.5))), gr(tech("b", per(2015, lf(7, "capacity-factor", 0.8)))), gr(tech("b", per(2015, lf(7, "capacity-factor", 0.9))))),
                  aw(gr(tech("b", per(2015, lf(7, "capacity-factor", 0.8)))), gr(tech("b", per(2015, lf(7, "capacity-factor", 0.9)))))))
  check("add-on with config: a year only in the base file, of a subresource's technology no earlier file made (deleted, nothing asked about its name)",
        { m <- cfg(list(usa(cf(0.5))), gr(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), per(2020, lf(7, "capacity-factor", 0.8)))), gr(tech("a", per(2020, lf(7, "capacity-factor", 0.9)))))
          !startsWith(m, "REFUSED") && has(m, '<period year="2015" delete="1"/>', "<capacity-factor>0.9</capacity-factor>") })
  check("refuses with config: a year only in the base file, of a subresource's technology an earlier file made without a name",
        { m <- cfg(list(gr(t0)), gr(tech("b", per(2015, lf(7, "capacity-factor", 0.8)), per(2020, lf(7, "capacity-factor", 0.8)))), gr(tech("b", per(2020, lf(7, "capacity-factor", 0.9)))))
          has(m, "technology/name: technology renamed") && never(m) })
  check("refuses with config: a subresource's technology named by a <name> child after its period, where no earlier file made it",
        { m <- cfg(list(usa(cf(0.5))), gr(cf(0.8)), gr(c(i4(5, "<technology>"), per(2015, lf(7, "capacity-factor", 0.8)), lf(6, "name", "a"), i4(5, "</technology>"))))
          has(m, "technology/name: technology renamed") && never(m) })
  check("check: a <name> child of a subresource technology's period, reset by the add-on's later read of the period, passes",
        identical(sim(gr(tech("a", per(2015, lf(7, "capacity-factor", 0.8), lf(7, "name", "q")))), gra(tech("a", per(2015, lf(7, "capacity-factor", 0.8)), at = nc)), gr(cf(0.8))), "ok"))
  check("negative control: a <name> child of a sector technology's period, reset by the add-on's later read of the period",
        has(sim(usa(tech("a", per(2015, lf(7, "name", "x"), lf(7, "capacity-factor", 0.8)))), chain(tech("a", per(2015, lf(7, "capacity-factor", 0.9)), at = nc)),
                usa(tech("a", per(2015, lf(7, "name", "x"), lf(7, "capacity-factor", 0.9))))), "technology[a]/period[2015]/name: value (a | x)"))
  check("negative control: a <name> child of a sector technology's period, reset by the override's later read of the period",
        has(sim(usa(cf(0.8)), chain(tech("a", per(2015, lf(7, "name", "q")), at = nc)), usa(tech("a", per(2015, lf(7, "capacity-factor", 0.8), lf(7, "name", "q")), per(2015)))),
            "technology[a]/period[2015]/name: value (q | a)"))
  check("negative control: stub-technology then technology of one name, input appended where the override puts it before another",
        has(sim(two("stub-technology", "", ipn("y", 2)), aww, two("stub-technology", "", ipn("w", 3), ipn("y", 2))), "stub-technology[a]/period[1975]/minicam-non-energy-input: entry order (x; y; w | x; w; y)"))
  check("negative control: the same with every element nocreate",
        has(sim(two("stub-technology", nc, ipn("y", 2)), aww, two("stub-technology", nc, ipn("w", 3), ipn("y", 2))), "period[1975]/minicam-non-energy-input: entry order (x; y; w | x; w; y)"))
  check("check: stub-technology then technology of one name, input appended in the override's order, passes",
        identical(sim(two("stub-technology", "", ipn("y", 2)), aww, two("stub-technology", "", ipn("y", 2), ipn("w", 3))), "ok"))
  check("addon_write: an add-on the check rejects is deleted, with an internal error naming the place", {
    fw <- file.path(d, c("wb.xml", "wo.xml", "wa.xml")); writeBin(charToRaw(paste(stb(0.8), collapse = "\n")), fw[1]); writeBin(charToRaw(paste(stb(0.88), collapse = "\n")), fw[2])
    suppressMessages(trace("addon_sim", where = ns, print = FALSE, tracer = quote(writeLines(sub(">0.88<", ">0.89<", readLines(a_file), fixed = TRUE), a_file))))
    m <- tryCatch({ addon_write(fw[1], fw[2], fw[3]); "" }, error = conditionMessage); suppressMessages(untrace("addon_sim", where = ns))
    has(m, "addon_minaj: internal error, no add-on written; 1 place(s)", "stub-technology[a]/period[2015]/capacity-factor: value (0.89 | 0.88)", "wo.xml in place of the base file") &&
      !file.exists(fw[3]) && !length(list.files(d, "[.]part[.]xml$")) })
  check("addon_write: the check stopping on the add-on names the add-on, not its temporary file", {
    suppressMessages(trace("addon_sim", where = ns, print = FALSE, tracer = quote(writeLines("<scenario></scenario><scenario></scenario>", a_file))))
    m <- tryCatch({ addon_write(fw[1], fw[2], fw[3]); "" }, error = conditionMessage); suppressMessages(untrace("addon_sim", where = ns))
    has(m, "addon_minaj: internal error, no add-on written; the check stopped: wa.xml must hold one <scenario> element") && !grepl("part.xml", m, fixed = TRUE) && !file.exists(fw[3]) })
  check("addon_write: an internal error with 12 places under warning.length 1000: every place printed first, the error short", {
    t12 <- function(v) usa(unlist(lapply(sprintf("t%02d", 1:12), function(n) tech(n, per(2015, lf(7, "capacity-factor", v))))))
    writeBin(charToRaw(paste(t12(0.8), collapse = "\n")), fw[1]); writeBin(charToRaw(paste(t12(0.88), collapse = "\n")), fw[2])
    suppressMessages(trace("addon_sim", where = ns, print = FALSE, tracer = quote(writeLines(gsub(">0.88<", ">0.89<", readLines(a_file), fixed = TRUE), a_file))))
    op <- options(warning.length = 1000); ms <- character(0)
    m <- tryCatch({ withCallingHandlers(addon_write(fw[1], fw[2], fw[3]), message = function(x) { ms <<- c(ms, conditionMessage(x)); invokeRestart("muffleMessage") }); "" }, error = conditionMessage)
    options(op); suppressMessages(untrace("addon_sim", where = ns)); ms <- paste(ms, collapse = "\n")
    all(vapply(sprintf("technology[t%02d]/period[2015]/capacity-factor: value (0.89 | 0.88)", 1:12), grepl, NA, x = ms, fixed = TRUE)) &&
      has(m, "addon_minaj: internal error, no add-on written; 12 place(s)", "(listed above", "wo.xml in place of the base file") && nchar(m, "bytes") <= 600 && !file.exists(fw[3]) })
  if (file.exists(file.path(d, "electricity_cf_addon.xml"))) {   # the real capacity-factor add-on, corrupted
    ra <- readLines(file.path(d, "electricity_cf_addon.xml")); fa <- file.path(d, "electricity_cf_neg.xml")
    neg <- function(x) { writeLines(x, fa); addon_sim(file.path(xml_dir, "electricity.xml"), fa, file.path(d, "electricity_cf.xml")) }
    i <- grep("<capacity-factor>", ra)[100]; v <- neg(replace(ra, i, sub(">[^<]*<", ">0.123<", ra[i])))
    check("negative control: electricity.xml add-on with one value changed: caught at that one place", !v$ok && nrow(v$mismatches) == 1L && endsWith(v$mismatches$target, "/capacity-factor"))
    i <- grep("global-technology-database", ra); v <- neg(ra[-(i[1]:i[2])])
    check(sprintf("negative control: electricity.xml add-on without its global technology database: caught (%d places)", nrow(v$mismatches)),
          !v$ok && all(startsWith(v$mismatches$target, "scenario/world/global-technology-database/")))
  }

  # ---- addon_sim with config: after the earlier files ---------------------------------------------------------
  # Every configuration-specific add-on above passed the check inside addon_write after its earlier files. Here wrong
  # add-ons (written to tempfile() names, which mutation.R's triple mode does not read) must differ at the place named.
  m <- suppressMessages(cfg(list(usa(cf(0.7))), usa(cf(0.8)), usa(tech("a", per(2015))))); f <- file.path(d, paste0(c("b", "o", "a", "c", "t"), k, ".xml"))
  v0 <- addon_sim(f[1], f[3], f[2]); v1 <- addon_sim(f[1], f[3], f[2], config = f[4])
  check("check with config: a configuration-specific add-on differs from no earlier state alone, and passes after the earlier files",
        identical(m, macw(lf(7, "capacity-factor", 0.7))) && !v0$ok && v1$ok && !is.null(nm$sim_index[[unname(tools::md5sum(file.path(d, sprintf("p%d_1.xml", k))))]]))
  file.copy(file.path(xml_dir, "modeltime.xml"), f[5], overwrite = TRUE); ev <- new.env()
  suppressMessages(trace("addon_sim", where = ns, print = FALSE, exit = bquote(assign("y", yv, envir = .(ev)))))
  v <- tryCatch(get("addon_sim", ns)(f[1], f[3], f[2], config = f[4]), error = conditionMessage); suppressMessages(untrace("addon_sim", where = ns))
  check("check with config: its own model years of vanilla modeltime.xml equal config_components' (1975, 1990, 2005, 2010, 2015, 2021, 2025, ..., 2100)",
        is.list(v) && v$ok && identical(ev$y, get("config_components", ns)(f[4], basename(f[1]))$years) && identical(ev$y, as.integer(c(1975, 1990, 2005, 2010, 2015, 2021, seq(2025, 2100, 5)))))
  op <- options(minaj.ram_floor_gb = 1e6); v <- tryCatch(addon_sim(f[1], f[3], f[2], config = f[4]), error = conditionMessage); options(op)
  suppressMessages(trace("addon_sim", where = ns, print = FALSE, tracer = quote(options(minaj.ram_floor_gb = 1e6))))
  w <- tryCatch({ suppressMessages(addon_write(f[1], f[2], tempfile(fileext = ".xml"), config = f[4])); "" }, error = conditionMessage); suppressMessages(untrace("addon_sim", where = ns)); options(op)
  check("check with config: memory floor: stops before reading the earlier files' blocks; addon_write passes the stop on as it is",
        has(v, "addon_minaj: only ", " GB of memory is free, under the floor of 1e+06 GB (option minaj.ram_floor_gb) plus about ", sprintf(" GB for reading block region USA of p%d_1.xml; try again later", k)) &&
          startsWith(w, "addon_minaj: only ") && has(w, sprintf("for reading block region USA of p%d_1.xml", k)))
  psn <- asNamespace("ps"); pf <- psn$ps_system_memory; unlockBinding("ps_system_memory", psn)   # free memory: ample until the earlier files' parts are held
  assign("ps_system_memory", function(...) list(avail = if (length(get0("pd", parent.frame(), inherits = FALSE))) 0 else 2^60), envir = psn)
  v <- tryCatch(addon_sim(f[1], f[3], f[2], config = f[4]), error = conditionMessage); assign("ps_system_memory", pf, envir = psn); lockBinding("ps_system_memory", psn)
  check("check with config: memory floor: checked again before each group is read whole (base, add-on, override and the earlier files' parts)",
        is.character(v) && has(v, "addon_minaj: only 0.0 GB of memory is free, under the floor of ", sprintf(" GB for reading block region USA of b%d.xml, a%d.xml and o%d.xml with the parts of 1 earlier file; try again later", k, k, k)))
  fa <- tempfile(fileext = ".xml"); unlockBinding("ps_system_memory", psn)
  assign("ps_system_memory", function(...) list(avail = if (length(get0("pd", parent.frame(), inherits = FALSE))) 0 else 2^60), envir = psn)
  w <- tryCatch({ suppressMessages(addon_write(f[1], f[2], fa, config = f[4])); "" }, error = conditionMessage); assign("ps_system_memory", pf, envir = psn); lockBinding("ps_system_memory", psn)
  check("addon_write with config: the memory stop of the check after the earlier files names the add-on, not its temporary file",
        has(w, "addon_minaj: only 0.0 GB of memory is free", paste0(basename(fa), " and o", k, ".xml with the parts of 1 earlier file;")) && !grepl("part.xml", w, fixed = TRUE) && !file.exists(fa))
  m <- suppressMessages(cfg(list(u0), doc(reg("USA")), doc(c(reg("USA"), gtd(0.88))))); f <- file.path(d, paste0(c("b", "o", "c"), k, ".xml")); fa <- tempfile(fileext = ".xml")
  writeBin(charToRaw(top(i4(1, "<world>"), sub(">", ' nocreate="1">', gtd(0.88)[1], fixed = TRUE), gtd(0.88)[-1], i4(1, "</world>"))), fa)
  v0 <- addon_sim(f[1], fa, f[2]); v1 <- addon_sim(f[1], fa, f[2], config = f[3]); unlink(fa)
  check("check with config: the global technology database exists with <world>, so a wrapper with nocreate applies (and differs from no earlier state alone)",
        !v0$ok && any(v0$mismatches$column == "existence (if no earlier file creates it)") && v1$ok)
  suppressMessages(cfg(list(u0), doc(c(reg("USA"), i4(2, "<global-technology-database/>"))), doc(reg("USA")))); f <- file.path(d, paste0(c("b", "o", "c"), k, ".xml")); fa <- tempfile(fileext = ".xml")
  writeBin(charToRaw(top()), fa); v0 <- addon_sim(f[1], fa, f[2]); v1 <- addon_sim(f[1], fa, f[2], config = f[3])
  suppressMessages(cfg(list(u0), doc(c(reg("USA"), gtd(0.88))), doc(reg("USA")))); f <- file.path(d, paste0(c("b", "o", "c"), k, ".xml"))
  v2 <- addon_sim(f[1], fa, f[2], config = f[3]); unlink(fa)
  check("check with config: the global technology database exists with <world> in both runs, written in one run only: no existence difference, its children compared",
        !v0$ok && any(v0$mismatches$target == "scenario/world/global-technology-database") && v1$ok && !v2$ok &&
          all(startsWith(v2$mismatches$target, "scenario/world/global-technology-database/")))
  ng <- list(
    list("the default restored where an earlier file sets the value", list(usa(cf(0.7))), usa(cf(0.8)), usa(tech("a", per(2015))), macw(lf(7, "capacity-factor", 1)), "technology[a]/period[2015]/capacity-factor", "value"),
    list("a delete where an earlier file created the technology", list(usa(qx)), usa(cf(0.8), qb), usa(cf(0.8)), chain(qd), "subsector[coal]/technology[b]", "existence"),
    list("the earlier rule copied without the clear", list(usa(rule(2010, 2020))), usa(rule(2020, 2050)), usa(rule(2020, 2060)), chain(c(rule(2010, 2020), rule(2020, 2060))), "subsector[coal]/interpolation-rule", "rule list"),
    list("restores for other model years (1980 for 1990)", list(u0), usa(lf(5, "fuelprefElasticity", 2, ' fillout="1" year="1975"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"')),
         chain(c(lf(5, "fuelprefElasticity", 0, sprintf(' year="%d"', c(1975, 1980, 2005, 2015, 2020, 2025))), lf(5, "fuelprefElasticity", 3, ' year="2010"'))), "subsector[coal]/fuelprefElasticity", "values by year"),
    list("a restore year missing (2020)", list(u0), usa(lf(5, "fuelprefElasticity", 2, ' fillout="1" year="1975"')), usa(lf(5, "fuelprefElasticity", 3, ' year="2010"')),
         chain(c(lf(5, "fuelprefElasticity", 0, sprintf(' year="%d"', c(1975, 1990, 2005, 2015, 2025))), lf(5, "fuelprefElasticity", 3, ' year="2010"'))), "subsector[coal]/fuelprefElasticity", "values by year"),
    list("a stub replace where an earlier file queued nodes", list(st2(per(2015, lf(7, "share-weight", 0.5)))), qt1, qt0, qtr, "stub-technology[a]/period[2015]/share-weight", "value"),
    list("an emptied rule list with a rule over 1975 to 2005 (years between: it sets values)", list(u0), usa(rule(2020, 2050)), usa(),
         chain(i4(5, '<interpolation-rule apply-to="share-weight" delete="1" from-year="1975" to-year="2005"><from-value>1</from-value><to-value>1</to-value><interpolation-function name="linear"/></interpolation-rule>')),
         "subsector[coal]/interpolation-rule", "rule list"),
    list("-0 for an unset value with option minaj.addon_neg0 FALSE", list(u0), qs1, qs0, macw(lf(7, "share-weight", "-0")), "technology[a]/period[2015]/share-weight", "value", list(minaj.addon_neg0 = FALSE)),
    list("the default restored in a stub-technology period deleted and recreated (no default fill in a stub queue)", list(u0),
         st2(i4(6, '<period year="2015" delete="1"/>'), per(2015, lf(7, "capacity-factor", 0.8))), st2(i4(6, '<period year="2015" delete="1"/>'), per(2015)),
         chain(c(i4(5, '<stub-technology name="a" nocreate="1">'), i4(6, '<period year="2015" nocreate="1">'), lf(7, "capacity-factor", 1), i4(6, "</period>"), i4(5, "</stub-technology>"))),
         "stub-technology[a]/period[2015]/capacity-factor", "value"),
    list("the default of the writing tag's class (CO2: MTC) where an earlier file created the object with another tag (Non-CO2: Tg)", list(usa(tech("a", per(2015, i4(7, '<Non-CO2 name="CO2"/>'))))),
         usa(tech("a", per(2015, i4(7, '<CO2 name="CO2">'), lf(8, "emissions-unit", "X"), i4(7, "</CO2>")))), usa(tech("a", per(2015, i4(7, '<CO2 name="CO2"/>')))),
         macw(i4(7, '<CO2 name="CO2" nocreate="1">'), lf(8, "emissions-unit", "MTC"), i4(7, "</CO2>")), "technology[a]/period[2015]/Non-CO2[CO2]/emissions-unit", "value"),
    list("the value of an earlier file's region that is commented out", list(cm1), usa(cf(0.8)), usa(tech("a", per(2015))), macw(lf(7, "capacity-factor", 0.4)), "technology[a]/period[2015]/capacity-factor", "value"),
    list("the value of an earlier file's region inside a comment opened before <scenario>", list(cm3), usa(cf(0.8)), usa(tech("a", per(2015))), macw(lf(7, "capacity-factor", 0.4)),
         "technology[a]/period[2015]/capacity-factor", "value"),
    list("a period rebuilt where an earlier file set its lifetime", list(usa(tech("a", per(2015, lf(7, "lifetime", 30))))), qs1, qs0,
         chain(c(i4(5, '<technology name="a" nocreate="1">'), i4(6, '<period year="2015" delete="1"/>'), per(2015, lf(7, "capacity-factor", 0.8)), i4(5, "</technology>"))), "technology[a]/period[2015]/lifetime", "value"),
    list("a grade an earlier file made, deleted and created again by the add-on", list(gr(g("g1", 1), g("g2", 2))), gr(g("g1", 1), g("g3", 3)), gr(g("g1", 1), g("g3", 3), g("g2", 2)),
         gra(i4(5, '<grade name="g2" delete="1"/>'), g("g2", 2)), "subresource[c]/grade", "entry order"),
    list("a period an earlier file made, read again by the add-on under another name", list(gr(cf(0.8))), gr(tech("a", per(2020, lf(7, "capacity-factor", 1)))),
         gr(tech("a", per(2020, lf(7, "capacity-factor", 2)))), gra(tech("b", per(2015), at = nc), tech("a", per(2020, lf(7, "capacity-factor", 2)), at = nc)), "technology/period[2015]/name", "value"))
  for (x in ng) {
    op <- options(if (length(x) > 7L) x[[8]] else list())
    suppressMessages(cfg(x[[2]], x[[3]], x[[4]])); f <- file.path(d, paste0(c("b", "o", "c"), k, ".xml")); fa <- tempfile(fileext = ".xml"); writeBin(charToRaw(x[[5]]), fa)
    v <- tryCatch(addon_sim(f[1], fa, f[2], config = f[3]), error = conditionMessage); unlink(fa); options(op)
    ok <- is.list(v) && !v$ok && any(endsWith(v$mismatches$target, x[[6]]) & v$mismatches$column == sprintf("%s (for the earlier files of c%d.xml)", x[[7]], k))
    check(paste0("negative control with config: ", x[[1]], ": differs after the earlier files at ", x[[6]]), ok)
    if (!ok) cat("  got:", if (is.list(v)) paste(v$mismatches$target, v$mismatches$column, collapse = "; ") else v, "\n")
  }
  suppressMessages(cfg(list(sub('<region name="USA">', '<region name="X">\n            <name>USA</name>', usa(cf(0.7)), fixed = TRUE)), usa(cf(0.8)), usa(tech("a", per(2015)))))
  f <- file.path(d, paste0(c("b", "o", "c"), k, ".xml")); fa <- tempfile(fileext = ".xml"); writeBin(charToRaw(macw(lf(7, "capacity-factor", 1))), fa)
  v <- tryCatch(addon_sim(f[1], fa, f[2], config = f[3]), error = conditionMessage); unlink(fa)
  check("check with config: an earlier file that names a region with a <name> child stops the check in addon_minaj's form",
        is.character(v) && startsWith(v, "addon_minaj: no add-on written; ") && has(v, sprintf("p%d_1.xml (an earlier file of c%d.xml) sets a name or year with a child element", k, k), paste0("use ", f[2], " in place of the base file")))
  fw <- c(tempfile(fileext = ".xml"), tempfile(fileext = ".xml"), tempfile(fileext = ".xml"))   # tempfile() names: the naive simulator reads no set-true attribute
  writeBin(charToRaw(paste(mac(), collapse = "\n")), fw[1]); writeBin(charToRaw(paste(sub('<mac-control name="m">', '<mac-control name="m" no-zero-cost-reductions="1">', mac(), fixed = TRUE), collapse = "\n")), fw[3])
  writeBin(charToRaw(top()), fw[2]); v0 <- addon_sim(fw[1], fw[2], fw[3])
  writeBin(charToRaw(macw(i4(7, '<Non-CO2 name="SO2" nocreate="1">'), i4(8, '<mac-control name="m" nocreate="1">'), i4(9, "<no-zero-cost-reductions/>"), i4(8, "</mac-control>"), i4(7, "</Non-CO2>"))), fw[2])
  v1 <- addon_sim(fw[1], fw[2], fw[3]); unlink(fw)
  check("negative control: set-true flag given as an attribute in the override, missing from the add-on (and passes when the add-on sets it)",
        !v0$ok && any(endsWith(v0$mismatches$target, "mac-control[m]/no-zero-cost-reductions") & startsWith(v0$mismatches$column, "set-true flag")) && v1$ok)
  fw <- c(tempfile(fileext = ".xml"), tempfile(fileext = ".xml"), tempfile(fileext = ".xml"))
  writeBin(charToRaw(paste(sub('<mac-control name="m">', '<mac-control name="m" no-zero-cost-reductions="true">', mac(), fixed = TRUE), collapse = "\n")), fw[1])
  writeBin(charToRaw(top()), fw[2]); writeBin(charToRaw(paste(mac(i4(9, "<no-zero-cost-reductions/>")), collapse = "\n")), fw[3]); v0 <- addon_sim(fw[1], fw[2], fw[3]); unlink(fw)
  check("negative control: set-true attribute text other than 0 or 1 (\"true\", GCAM stops on it) differs from the element written in the override",
        !v0$ok && any(endsWith(v0$mismatches$target, "mac-control[m]/no-zero-cost-reductions") & startsWith(v0$mismatches$column, "set-true flag") & grepl("\"true\"", v0$mismatches$old, fixed = TRUE)))
  # a file over 50 MB read in chunks, with a region across the first chunk boundary and its start line in the carried 64 KB
  if (!mem_ok()) cat("SKIP addon_sim chunked locator (less than 4 GB free)\n") else {
    h0 <- c('<?xml version="1.0" encoding="UTF-8"?>', "<scenario>", i4(1, "<world>"), i4(2, '<region name="R01">')); fx <- lf(3, "social-discount-rate", 0.1)   # a region value written again and again
    n1 <- (2^26 - 32768 - sum(nchar(h0) + 1) - 18) %/% (nchar(fx) + 1)
    big <- c(h0, rep(fx, n1), i4(2, "</region>"), i4(2, '<region name="USA">'), rep(fx, 40000), sec(sbs("coal", cf(0.7))), i4(2, "</region>"), i4(1, "</world>"), "</scenario>", "")
    m <- suppressMessages(cfg(list(big), usa(cf(0.8)), usa(tech("a", per(2015))))); rm(big); fp <- file.path(d, sprintf("p%d_1.xml", k))
    T <- nm$sim_index[[unname(tools::md5sum(fp))]]
    b <- readBin(fp, "raw", file.size(fp)); oa <- as.numeric(grepRaw('<region name="', b, all = TRUE, fixed = TRUE)); oz <- as.numeric(grepRaw("</region>", b, all = TRUE, fixed = TRUE) + 8); rm(b)
    check(sprintf("check with config: chunked locator of a %.0f MB earlier file: block spans equal a whole-file search, region USA starts %d bytes before the 64 MB boundary; the add-on passes",
                  file.size(fp) / 2^20, 2^26 - oa[2] + 1),
          identical(m, macw(lf(7, "capacity-factor", 0.7))) && file.size(fp) > 2^26 && oa[2] < 2^26 && oa[2] > 2^26 - 65536 && oz[2] > 2^26 &&
            is.list(T) && identical(T$k$a, oa) && identical(T$k$z, oz) && identical(T$k$n, c("R01", "USA")))
    unlink(c(fp, file.path(d, sprintf("a%d.xml", k)))); invisible(gc())
    L <- nchar(fx) + 1; r0 <- 2^26 - 65536 - sum(nchar(h0) + 1) - nchar(i4(2, "</region>")) - 1; n1 <- r0 %/% L - 1; pl <- r0 - n1 * L   # region USA's line: first byte where the carry starts
    big <- c(h0, rep(fx, n1), i4(3, paste0("<social-discount-rate>0.1", strrep("0", pl - L), "</social-discount-rate>")), i4(2, "</region>"),
             i4(2, '<region name="USA">'), rep(fx, 40000), sec(sbs("coal", cf(0.7))), i4(2, "</region>"), i4(1, "</world>"), "</scenario>", "")
    m <- suppressMessages(cfg(list(big), usa(cf(0.8)), usa(tech("a", per(2015))))); rm(big); fp <- file.path(d, sprintf("p%d_1.xml", k))
    T <- nm$sim_index[[unname(tools::md5sum(fp))]]
    b <- readBin(fp, "raw", file.size(fp)); oa <- as.numeric(grepRaw('<region name="', b, all = TRUE, fixed = TRUE)); oz <- as.numeric(grepRaw("</region>", b, all = TRUE, fixed = TRUE) + 8); rm(b)
    check("check with config: chunked locator: a tag line starting on the first carried byte (0-based offset 2^26 - 65536) is found (byte offsets as doubles, not the whole-file fallback)",
          identical(m, macw(lf(7, "capacity-factor", 0.7))) && oa[2] == 2^26 - 65536 + 9 && is.list(T) && is.double(T$k$a) && identical(T$k$a, oa) && identical(T$k$z, oz) && identical(T$k$n, c("R01", "USA")))
    unlink(c(fp, file.path(d, sprintf("a%d.xml", k)))); invisible(gc())
  }

  # the naive full-file simulator of mutation.R on every base, add-on and override file above (design S4)
  me <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1]
  mu <- if (!is.na(me)) file.path(dirname(me), "mutation.R") else "tests/addon/mutation.R"
  if (file.exists(mu)) {
    o <- suppressWarnings(system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"), c(shQuote(mu), shQuote(d)), stdout = TRUE, stderr = TRUE))
    cat(paste0("  | ", o, "\n"), sep = "")
    check("naive simulator (mutation.R) on every file triple above: agrees with each written add-on and with addon_sim on each check",
          is.null(attr(o, "status")) && any(grepl("^[0-9]+ passed, 0 failed$", o)))
  } else cat("SKIP naive simulator on the unit files (", mu, " not found)\n", sep = "")
})

cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail))
if (n_fail) quit(status = 1)
