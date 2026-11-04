# End-to-end cases for addon_minaj on the vanilla 9.1 gcamdata tree (design S5): overrides of real CSV inputs and data
# objects, written as <stem>_override.xml by gcamdata's own Java writer, then compared with the base file. Records per
# case the verdict, the counts per rule, the add-on size against the override, the run time and the process's peak
# memory so far, and, from the lines printed while the configuration's earlier files are read, the number of questions
# about them, of block reads, the seconds and the GB read. Also checks the store record, the manifest row, the printed
# base file and configuration line, a repeat call ("unchanged"), "no change" (replacing the earlier add-on), a refusal
# while an earlier add-on is in place, dest=, config= (a comment over several lines, a relative path, a base file loaded
# twice, a configured base file that differs or is missing), the memory floor before chunks run, and the long form of a
# refusal. Configuration cases use copies of the tree's exe/configuration_ref.xml with absolute paths, the base file's
# line pointing to <scratch>/xml: configuration-specific add-ons (a value restored, a rule list, a keyword, a grade
# inserted, a type change, a value un-set with option minaj.addon_neg0 off and on), a missing earlier file, a repeat
# call, status_minaj(config) (current, stale after an earlier file changes, an add-on not after its base file), a
# universal add-on, and another add-on to the same base file loaded after it.
# Usage (from the package root): Rscript tests/addon/corpus.R [config] [GCAM tree] [scratch folder]
#   config: only the configuration cases. Defaults: the tree in environment variable MINAJ_TEST_TREE and tempdir()/corpus. XML files go to
#   <scratch>/xml (open_minaj(dest =)), never into the tree; stored results go to the store MINAJ_STORE (else the default store). Needs Java. Does
#   not start while less than 4 GB of memory is free; cases on files over 50 MB are skipped while less than 4 GB is free.
#   MINAJ_HOME as in unit.R.
args <- commandArgs(trailingOnly = TRUE)
if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
only <- "config" %in% args; args <- args[args != "config"]
tree <- if (length(args) >= 1L) args[1] else Sys.getenv("MINAJ_TEST_TREE")
if (!nzchar(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/addon/corpus.R [config] <GCAM tree> [scratch folder], or set MINAJ_TEST_TREE", call. = FALSE)
if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))
scr <- if (length(args) >= 2L) args[2] else file.path(tempdir(), "corpus")
dir.create(scr, recursive = TRUE, showWarnings = FALSE); scr <- normalizePath(scr, "/")
free <- function() ps::ps_system_memory()$avail / 2^30
if (free() < 4) { cat(sprintf("SKIP corpus: %.1f GB of memory free, under 4 GB\n", free())); quit(status = 0) }
n_pass <- 0L; n_fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
  cat(if (isTRUE(ok)) "PASS" else "FAIL", label, "\n")
}
ns <- asNamespace("minaj")
open_minaj(tree, dest = file.path(scr, "xml"))
xd <- file.path(scr, "xml"); ad <- file.path(xd, "addon")
peak <- function() tryCatch(ps::ps_memory_info()[["peak_wset"]] / 2^20, error = function(e) NA_real_)
res <- data.frame(case = character(), xml = character(), verdict = character(), rules = character(), KB = numeric(), pct = numeric(), secs = numeric(), peak_MB = numeric(),
                  questions = numeric(), reads = numeric(), scan_secs = numeric(), scan_gb = numeric(), stringsAsFactors = FALSE)

# one call: verdict "written", "unchanged", "no change", "refused" or "INTERNAL ERROR"; expect: verdicts that pass; need:
# texts the messages (or the refusal) must hold; avoid: texts they must not hold
run <- function(case, xml, override, expect, need = character(0), avoid = character(0), config = NULL, dest = NULL, big = FALSE) {
  if (big && free() < 4) { cat(sprintf("SKIP %s (%.1f GB free, under 4 GB)\n", case, free())); return(invisible(NULL)) }
  msgs <- character(0); t0 <- proc.time()[["elapsed"]]
  r <- tryCatch(withCallingHandlers(addon_minaj(xml, override, config = config, dest = dest, ask = FALSE),
                                    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
                                    warning = function(w) { msgs <<- c(msgs, paste("warning:", conditionMessage(w))); invokeRestart("muffleWarning") }),
                error = function(e) conditionMessage(e))
  secs <- proc.time()[["elapsed"]] - t0
  v <- if (is.character(r)) { if (grepl("internal error", r, fixed = TRUE)) "INTERNAL ERROR" else "refused" } else r$status
  txt <- paste(c(msgs, if (is.character(r)) r), collapse = "\n")
  a <- if (is.data.frame(r)) r$file else NA_character_; o <- file.path(xd, sub("[.]xml$", "_override.xml", xml))
  ru <- regmatches(txt, regexpr("\\| [^\n]*", txt)); ru <- if (length(ru)) sub("^\\| ", "", ru) else if (v == "refused") sub("^.*?\n  - ", "", substr(r, 1, 400)) else ""
  ml <- sub("\n+$", "", msgs); rl <- grep("^minaj: read [0-9.]+ MB from [0-9]+ earlier files? of ", ml, value = TRUE)   # prefix_scan's progress lines
  il <- grep("^minaj: indexing [0-9]+ earlier files? of .* GB[)], once per file version$", ml, value = TRUE)
  res[nrow(res) + 1L, ] <<- list(case, xml, v, substr(gsub("\n", " ", ru), 1, 160), if (!is.na(a) && file.exists(a)) round(file.size(a) / 1024, 1) else NA,
                                 if (!is.na(a) && file.exists(a) && file.exists(o)) round(100 * file.size(a) / file.size(o), 2) else NA, round(secs, 1), round(peak()),
                                 sum(as.numeric(sub("^.* for ([0-9]+) question[(]s[)] in .*$", "\\1", rl))), length(rl), round(sum(as.numeric(sub("^.* in ([0-9.]+) s$", "\\1", rl))), 1),
                                 round(sum(as.numeric(sub("^minaj: read ([0-9.]+) MB.*$", "\\1", rl))) / 1024 + sum(as.numeric(sub("^.*[(]([0-9.]+) GB[)], once per file version$", "\\1", il))), 3))
  ok <- v %in% expect && all(vapply(need, grepl, NA, x = txt, fixed = TRUE)) && !any(vapply(avoid, grepl, NA, x = txt, fixed = TRUE))
  check(sprintf("%s (%s): %s%s", case, xml, v, if (!ok) paste0("; expected ", paste(expect, collapse = " or "), "\n    ", gsub("\n", "\n    ", substr(txt, 1, 1500))) else ""), ok)
  invisible(list(r = r, txt = txt, file = a))
}
names_ok <- function(a, xml, ...) {                                # every name in the add-on is, byte for byte, a name in the base or the override file, or in one of the files given in ... (searched by bytes)
  xt <- get("xml_tokens", ns); na <- xt(a)[["@name"]]; if (is.null(na)) return(TRUE)
  nb <- c(xt(file.path(xd, xml))[["@name"]], xt(file.path(xd, sub("[.]xml$", "_override.xml", xml)))[["@name"]])
  na <- unique(na[!is.na(na) & !na %in% nb])
  for (p in c(...)) {
    if (!length(na) || !file.exists(p)) next
    con <- file(p, "rb"); cr <- raw(0)
    while (length(na) && length(b <- readBin(con, "raw", 2^26))) { bb <- c(cr, b); na <- na[!vapply(na, function(v) length(grepRaw(paste0(' name="', v, '"'), bb, fixed = TRUE)) > 0L, NA)]; cr <- bb[max(1L, length(bb) - 65535L):length(bb)] }
    close(con)
  }
  !length(na)
}
cf <- list("energy/A23.globaltech_capacity_factor" = 1.1)
grd <- function(mid) function(d) {                                 # one more grade for the first subresource, between its first two grades or after its last
  i <- which(d$region == d$region[1] & d$resource == d$resource[1] & d$subresource == d$subresource[1]); if (length(i) < 2L) return(d)
  x <- d[i[1], ]; x$grade <- paste(x$grade, "b"); j <- if (mid) i[1] else max(i)
  x$extractioncost <- if (mid) (d$extractioncost[i[1]] + d$extractioncost[i[2]]) / 2 else 2 * max(d$extractioncost[i])
  rbind(d[seq_len(j), ], x, d[-seq_len(j), ])
}

if (!only) {
  # ---- the entry function --------------------------------------------------------------------------------------------
  w <- run("capacity factor x1.1", "electricity.xml", cf, "written", need = c("universal add-on, valid directly after this exact base file", file.path(xd, "electricity.xml"), "md5 ",
                                                                         '<Value name="electricity_addon">', file.path(ad, "electricity_addon.xml")))
  if (!is.null(w) && !is.na(w$file)) {
    md <- unname(tools::md5sum(w$file)); st <- get(".nm", ns)$store
    recs <- lapply(list.files(file.path(st, "rec", "addon"), "\\.rds$", recursive = TRUE, full.names = TRUE), function(f) tryCatch(readRDS(f), error = function(e) NULL))
    check("store: an 'addon' record holds the add-on's md5 and the base file's md5", any(vapply(recs, function(x) identical(x$md5, md) && identical(x$xml, "electricity.xml") &&
                                                                                                  identical(x$base_md5, unname(tools::md5sum(file.path(xd, "electricity.xml")))), NA)))
    mf <- utils::read.csv(file.path(ad, "minaj_manifest.csv"), colClasses = "character"); l <- mf[nrow(mf), ]
    check("manifest: last row is the add-on, how = addon", identical(l$file, "electricity_addon.xml") && identical(l$how, "addon") && identical(l$md5, md) && identical(l$xml, "electricity.xml"))
    check("add-on: every name attribute is byte-identical to one in the base or override file", names_ok(w$file, "electricity.xml"))
  }
  run("capacity factor x1.1, again", "electricity.xml", cf, "unchanged")
  run("capacity-factor row dropped, an earlier add-on in place", "electricity.xml", list(L223.GlobalTechCapFac_elec = function(d) d[-1, ]), "refused",
      need = c("was written by an earlier call", "keep it out of the configuration", "config= can decide it from the configuration's earlier files"))
  run("override that changes nothing", "electricity.xml", list("energy/A23.globaltech_capacity_factor" = function(d) d), "no change",
      need = c("electricity_addon.xml no change in", "replaced the earlier electricity_addon.xml", "(the add-on is now empty)"))
  check("no change: the add-on is an empty <scenario></scenario>", file.exists(file.path(ad, "electricity_addon.xml")) &&
          any(grepl("<scenario></scenario>", readLines(file.path(ad, "electricity_addon.xml")), fixed = TRUE)))
  owd <- setwd(scr)
  run("dest= relative to the working directory", "electricity.xml", cf, c("written", "unchanged"), need = file.path(scr, "addon_dest", "electricity_addon.xml"), dest = "addon_dest")
  setwd(owd)
  check("dest=: the add-on is in that folder", file.exists(file.path(scr, "addon_dest", "electricity_addon.xml")))
}

# ---- config= -----------------------------------------------------------------------------------------------------------
cd <- file.path(scr, "cfg"); dir.create(cd, showWarnings = FALSE)
cfg <- function(f, comps) { writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', "<Configuration>", "  <Files>", sprintf('    <Value name="xmlInputFileName">%s</Value>', normalizePath(file.path(tree, "input", "gcamdata", "xml", "modeltime.xml"), "/")),
                                         "  </Files>", "  <ScenarioComponents>", comps, "  </ScenarioComponents>", "</Configuration>"), f); f }
c1 <- cfg(file.path(cd, "configuration_one.xml"), c("    <!-- an older line, commented out over several lines:", '    <Value name="electricity">../xml/electricity.xml</Value>', "    -->",
                                                   '    <Value name="electricity">../xml/electricity.xml</Value>'))
run("config= with a comment over several lines and a relative path", "electricity.xml", cf, c("written", "unchanged"), config = c1,
    need = c(paste("loads the base file from", file.path(xd, "electricity.xml")), "read from the configuration's folder", '<Value name="electricity_addon">../xml/addon/electricity_addon.xml</Value>',
             paste("as the base file's line is; absolute path", file.path(ad, "electricity_addon.xml"))))
c2 <- cfg(file.path(cd, "configuration_two.xml"), rep('    <Value name="electricity">../xml/electricity.xml</Value>', 2))
run("config= loading the base file twice", "electricity.xml", cf, "refused", need = "loads electricity.xml 2 times", config = c2)
dir.create(file.path(cd, "other"), showWarnings = FALSE); file.copy(file.path(xd, "electricity.xml"), file.path(cd, "other", "electricity.xml"), overwrite = TRUE)
cat("<!-- changed -->\n", file = file.path(cd, "other", "electricity.xml"), append = TRUE)
c3 <- cfg(file.path(cd, "configuration_three.xml"), '    <Value name="electricity">other/electricity.xml</Value>')
run("config= whose base file differs from the current base result", "electricity.xml", cf, "refused", need = "is not the current base result", config = c3)
dir.create(file.path(cd, "sub"), showWarnings = FALSE)
c4 <- cfg(file.path(cd, "sub", "configuration_four.xml"), '    <Value name="electricity">../xml/electricity.xml</Value>')
run("config= in a subfolder whose relative path does not resolve from there", "electricity.xml", cf, "refused", config = c4,
    need = c("does not exist", "read from the configuration's folder", "the folder GCAM runs in"))
if (!only) {
  op <- options(minaj.ram_floor_gb = 1e6)
  run("memory floor: chunks to run while free memory is under option minaj.ram_floor_gb", "electricity.xml", list("energy/A23.globaltech_capacity_factor" = 1.21), "refused",
      need = c("chunk(s) to run, but only", "minaj.ram_floor_gb"))
  options(op)
}

# ---- configuration-specific add-ons: copies of configuration_ref.xml ---------------------------------------------------
rc <- get("config_components", ns)(normalizePath(file.path(tree, "exe", "configuration_ref.xml"), "/"))
cv <- sprintf('    <Value name="%s">%s</Value>', sub("[.]xml$", "", basename(rc$path[-1])), rc$path[-1])   # its components, absolute paths
ib <- match(c("electricity_water.xml", "resources.xml", "hector.xml", "socioeconomics_incelas_ssp2.xml"), basename(rc$path[-1]))
k1 <- cv; k1[ib[1]] <- sub(rc$path[ib[1] + 1L], file.path(xd, "electricity_water.xml"), cv[ib[1]], fixed = TRUE)
k2 <- cv; k2[ib[2]] <- sub(rc$path[ib[2] + 1L], file.path(xd, "resources.xml"), cv[ib[2]], fixed = TRUE)
q1 <- cfg(file.path(cd, "configuration_ref_copy_1.xml"), k1); q2 <- cfg(file.path(cd, "configuration_ref_copy_2.xml"), k2)
q3 <- cfg(file.path(cd, "configuration_ref_copy_3.xml"), append(k1, sprintf('    <Value name="electricity_water_addon">%s</Value>', file.path(ad, "electricity_water_addon.xml")), ib[1]))
file.copy(rc$path[ib[4] + 1L], file.path(cd, basename(rc$path[ib[4] + 1L])), overwrite = TRUE); cat("<!-- changed -->\n", file = file.path(cd, basename(rc$path[ib[4] + 1L])), append = TRUE)
k4 <- append(k1, sprintf('    <Value name="electricity_water_addon">%s</Value>', file.path(ad, "electricity_water_addon.xml")), ib[1]); k4[ib[4]] <- sub(rc$path[ib[4] + 1L], file.path(cd, basename(rc$path[ib[4] + 1L])), cv[ib[4]], fixed = TRUE)
q4 <- cfg(file.path(cd, "configuration_ref_copy_4.xml"), k4)
unlink(file.path(cd, "hector.xml")); k5 <- k1; k5[ib[3]] <- sub(rc$path[ib[3] + 1L], file.path(cd, "hector.xml"), cv[ib[3]], fixed = TRUE)   # an earlier file that does not exist
q5 <- cfg(file.path(cd, "configuration_ref_copy_5.xml"), k5)
q6 <- cfg(file.path(cd, "configuration_ref_copy_6.xml"), c(k1, sprintf('    <Value name="electricity_water_addon">%s</Value>', file.path(scr, "addon_dest", "electricity_water_addon.xml"))))
pw <- rc$path[seq_len(ib[1])]                                      # files loaded before electricity_water.xml
rd <- list(L2233.GlobalTechCapFac_elec_cool = function(d) d[-1, ])
w <- run("capacity-factor row dropped", "electricity_water.xml", rd, "written", config = q1,
         need = c("configuration-specific add-on, valid directly after this exact base file in configuration_ref_copy_1.xml", sprintf("while the %d %s", length(pw), if (length(pw) == 1L) "file loaded before it keeps its content" else "files loaded before it keep their content")))
if (!is.null(w) && !is.na(w$file)) {
  ef <- if (length(pw) == 1L) "file" else "files"
  check(sprintf("configuration-specific add-on: header names the %d earlier %s", length(pw), ef), any(grepl(sprintf("; configuration-specific: %d earlier %s", length(pw), ef), readLines(w$file, n = 3L), fixed = TRUE)))
  check("configuration-specific add-on: every name attribute is in the base file, the override or an earlier file", names_ok(w$file, "electricity_water.xml", pw))
}
run("capacity-factor row dropped, again", "electricity_water.xml", rd, "unchanged", config = q1)
for (z in list(list(q3, sprintf("configuration-specific add-on to electricity_water.xml, current (the %d %s)", length(pw), if (length(pw) == 1L) "file loaded before the base file is unchanged" else "files loaded before the base file are unchanged"), "current"),
               list(q4, "configuration-specific add-on to electricity_water.xml, stale: 1 file(s) loaded before the base file changed since it was written (socioeconomics_incelas_ssp2.xml)", "stale after an earlier file changed"))) {
  sm <- character(0); e <- tryCatch({ withCallingHandlers(status_minaj(z[[1]]), message = function(m) { sm <<- c(sm, conditionMessage(m)); invokeRestart("muffleMessage") }); "" }, error = conditionMessage)
  check(sprintf("status_minaj(%s): %s%s", basename(z[[1]]), z[[3]], if (nzchar(e)) paste(":", e) else ""), !nzchar(e) && any(grepl(sub("%d", length(pw), z[[2]], fixed = TRUE), sm, fixed = TRUE)))
}
su <- list(L2233.GlobalTechShrwt_elec_cool = function(d) d[-1, ])
op <- options(minaj.addon_neg0 = FALSE)
run("share-weight row dropped, option minaj.addon_neg0 FALSE", "electricity_water.xml", su, c("written", "refused"), config = q1, avoid = "internal error")
options(op)
w <- run("share-weight row dropped, option minaj.addon_neg0 TRUE (the default)", "electricity_water.xml", su, "written", config = q1, need = "configuration-specific add-on")
if (!is.null(w) && !is.na(w$file)) check("option minaj.addon_neg0 TRUE: the add-on writes -0", any(grepl(">-0<", readLines(w$file), fixed = TRUE)))
run("rule to-year changed", "electricity_water.xml", list("energy/A23.subsector_interp" = function(d) { d$to.year[d$supplysector == "electricity" & d$subsector == "coal"] <- 2250; d }),
    "written", config = q1, need = "configuration-specific add-on")
run("global technology keyword changed", "electricity_water.xml", list("energy/A23.globaltech_keyword" = function(d) { d$primary.renewable[d$technology == "hydro"] <- "geothermal-elect"; d }),
    "written", config = q1, need = "configuration-specific add-on")
lg <- run("sector logit type changed", "electricity_water.xml", list("energy/A23.sector" = function(d) { d$logit.type[d$supplysector == "electricity"] <- "absolute-cost-logit"; d }),
          c("written", "refused"), config = q1, avoid = "internal error")
if (!is.null(lg) && is.character(lg$r)) check("sector logit type changed: refused for an earlier file", grepl("earlier file", lg$r, fixed = TRUE))
run("grade inserted between grades", "resources.xml", list(L210.RsrcCurves_fos = grd(TRUE)), "written", config = q2, need = "configuration-specific add-on")
run("capacity-factor row dropped, an earlier file missing", "electricity_water.xml", rd, "refused", config = q5,
    need = c(paste0("lists ", file.path(cd, "hector.xml"), ", which does not exist; GCAM stops when it cannot open a component"), "so the state before the base file is not known"))
run("capacity factor x1.1, an earlier file missing (no question about it)", "electricity_water.xml", cf, c("written", "unchanged"), config = q5, need = "universal add-on, valid directly after this exact base file")
run("capacity factor x1.1 with config= stays universal", "electricity_water.xml", cf, c("written", "unchanged"), config = q1, need = "universal add-on, valid directly after this exact base file in any configuration")
run("capacity factor x1.1, universal add-on in another folder", "electricity_water.xml", cf, c("written", "unchanged"), config = q1, dest = file.path(scr, "addon_dest"), need = "universal add-on")
sm <- character(0); e <- tryCatch({ withCallingHandlers(status_minaj(q6), message = function(m) { sm <<- c(sm, conditionMessage(m)); invokeRestart("muffleMessage") }); "" }, error = conditionMessage)
check(sprintf("status_minaj(%s): a universal add-on not after its base file is flagged%s", basename(q6), if (nzchar(e)) paste(":", e) else ""),
      !nzchar(e) && any(grepl("electricity_water_addon.xml: add-on to electricity_water.xml, not loaded directly after that base file (or the base file changed)", sm, fixed = TRUE)))
run("another add-on to the same base file loaded after it", "electricity_water.xml", rd, "refused", config = q6,
    need = c("another add-on to this base file is loaded after it", file.path(scr, "addon_dest", "electricity_water_addon.xml")))

if (!only) {
  # ---- overrides of the design's corpus --------------------------------------------------------------------------------
  lg <- run("sector logit type changed", "electricity.xml", list("energy/A23.sector" = function(d) { d$logit.type[d$supplysector == "electricity"] <- "absolute-cost-logit"; d }), "refused",
            need = c("type change", "config= can decide it from the configuration's earlier files"))
  if (!is.null(lg) && is.character(lg$r))
    check(sprintf("long refusal: every region listed in the messages, the error within warning.length (%d bytes, %d regions)", nchar(lg$r, "bytes"), lengths(regmatches(lg$txt, gregexpr("region\\[", lg$txt)))),
          lengths(regmatches(lg$txt, gregexpr("region\\[", lg$txt))) >= 30L && grepl("(listed above)", lg$r, fixed = TRUE) && nchar(lg$r, "bytes") <= getOption("warning.length", 1000))
  run("rule to-year changed", "electricity.xml", list("energy/A23.subsector_interp" = function(d) { d$to.year[d$supplysector == "electricity" & d$subsector == "coal"] <- 2250; d }), "refused",
      need = c("interpolation-rule: rule list changed", "config= can decide it from the configuration's earlier files"))
  run("rule row added (CSVToXML position of a new rule)", "electricity.xml",
      list("energy/A23.subsector_interp" = function(d) { x <- d[d$supplysector == "electricity" & d$subsector == "biomass", ][1, ]
                                                         x$from.year <- if (is.character(d$from.year)) "2100" else 2100; x$to.year <- if (is.character(d$to.year)) "2150" else 2150; rbind(d, x) }), c("written", "refused"))
  run("rule to-value changed, gas_trade.xml as the base", "gas_trade.xml", list("L2392.SubsectorAll_reg_NG" = function(d) { d$to.value <- d$to.value * 0.9; d }), c("refused", "no change"))
  run("global technology keyword changed", "electricity.xml", list("energy/A23.globaltech_keyword" = function(d) { d$primary.renewable[d$technology == "hydro"] <- "geothermal-elect"; d }), "refused",
      need = c("keyword dropped or changed", "config= can decide it from the configuration's earlier files"))
  run("grade inserted between grades", "resources.xml", list(L210.RsrcCurves_fos = grd(TRUE)), "refused", need = c("entry order changes in an order-critical list", "config= can decide it from the configuration's earlier files"))
  run("grade appended after the last grade", "resources.xml", list(L210.RsrcCurves_fos = grd(FALSE)), "written")
  run("override with a Delete table in both files (bio_trade.xml)", "bio_trade.xml", list(L243.SubsectorShrwtFllt_TradedBio = function(d) { d$share.weight <- d$share.weight * 0.5; d }), c("written", "no change"))
  run("calc-base-price: pair added", "socioeconomics_macro.xml", list(L281.BasePriceSectorMapping = function(d) { x <- d[1, ]; x$from.sector <- paste0(x$from.sector, "_x"); rbind(d, x) }), "written")
  run("calc-base-price: to-name changed", "socioeconomics_macro.xml", list(L281.BasePriceSectorMapping = function(d) { d$to.sector[1] <- paste0(d$to.sector[1], "_x"); d }), "refused",
      need = "calc-base-price changed", avoid = "config=")
  run("modeltime end year changed", "modeltime.xml", list(L200.ModelTime = function(d) { j <- grep("end", names(d))[1]; d[[j]] <- d[[j]] + 5; d }), "refused",
      need = "modeltime differs", avoid = "config=")
  run("land allocation x1.1 in every region (time of a broad change)", "land_input_3_IRR.xml", list(L2231.LN3_HistUnmgdAllocation = function(d) { d$allocation <- d$allocation * 1.1; d }), "written")
  run("emission input x1.1 in a nocreate emissions file", "all_energy_emissions.xml", list(L201.en_ghg_emissions = function(d) { d$input.emissions <- d$input.emissions * 1.1; d }),
      c("written", "refused"), avoid = "internal error", big = TRUE)
  mac1 <- function(col) function(d) { w <- d$region == d$region[1]; d[[col]][w] <- d[[col]][w] * 0.9; d }   # one region: the add-on stays under minaj.addon_max_ratio
  run("MAC curve x0.9 in one region (the subresource created, then nocreate)", "all_energy_emissions_MAC.xml", list(L252.ResMAC_fos = mac1("mac.reduction")), "written",
      avoid = c("internal error", "nocreate pattern"))
  run("MAC tech change x0.9 in one region (the nocreate part)", "all_energy_emissions_MAC.xml", list(L252.ResMAC_fos_tc_average = mac1("tech.change")), "written",
      avoid = c("internal error", "nocreate pattern"))
}

cat("\n"); options(width = 260); print(res, row.names = FALSE)
cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail))
if (n_fail) quit(status = 1)
