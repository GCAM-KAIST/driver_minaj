# Verify minaj against a tree's full drake build. Temporary edits to one CSV and one chunk file are restored
# byte for byte (and their modification times). XML goes to the tree's xml_minaj/ folder (git-ignored).
# Usage (from the package root): Rscript tests/verify_tree.R <tree> <csv input> <data chunk> <xml> [n chunks to re-run]
#   (java on PATH; drake installed)
#   MINAJ_HOME: load minaj from this source folder instead of the installed package; MINAJ_STORE: store folder (default: the package default store)
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 4 || !dir.exists(file.path(a[1], "input", "gcamdata")))
  stop("usage: Rscript tests/verify_tree.R <GCAM tree> <csv input> <data chunk> <xml> [n chunks to re-run]", call. = FALSE)
TREE <- a[1]; CSV <- a[2]; CHUNK <- a[3]; XML <- a[4]; NRE <- if (length(a) >= 5) as.integer(a[5]) else 25
if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
say <- function(...) { cat(sprintf(...), "\n"); flush.console() }
el <- function(expr) { t0 <- proc.time()[["elapsed"]]; v <- force(expr); list(v = v, s = round(proc.time()[["elapsed"]] - t0, 1)) }
check <- function(label, ok) { say("%s %s", if (isTRUE(ok)) "PASS" else "FAIL", label); invisible(ok) }
options(minaj.ask_secs = Inf); if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))

r <- el(open_minaj(TREE)); say("open_minaj: %.1f s (%d chunks, %d XML)", r$s, r$v$chunks, r$v$xml)
r <- el(open_minaj(TREE)); say("open_minaj again (graph from disk): %.1f s", r$s)
G <- minaj:::.nm$root; built <- function(x) file.path(G, "xml", x)
r <- el(drake_to_minaj()); say("drake_to_minaj: %.1f s", r$s)

r <- el(st <- status_minaj(where = file.path(G, "xml"))); say("status_minaj over all XML vs xml/: %.1f s", r$s)
check(sprintf("all XML results current after import (%d stale)", sum(st$results == "stale")), all(st$results == "current"))
check(sprintf("all files in xml/ match their records (%d not)", sum(st$file != "current")), all(st$file == "current"))
if (any(st$results == "stale")) print(utils::head(st[st$results == "stale", setdiff(names(st), "path")], 10))

r <- el(make_minaj(XML)); out <- file.path(minaj:::dest_dir(), XML)
check(sprintf("make_minaj(%s) with nothing changed (%.1f s, %s) gives the built file", XML, r$s, r$v$status), tools::md5sum(out) == tools::md5sum(built(XML)))
r <- el(make_minaj(XML)); check(sprintf("second make_minaj is 'unchanged' (%.2f s)", r$s), r$v$status == "unchanged")

# Edit a file temporarily; it is restored byte for byte (and its time) even if a step fails.
with_edit <- function(path, body) {
  bak <- readBin(path, "raw", file.size(path)); mt <- file.mtime(path)
  on.exit({ writeBin(bak, path); Sys.setFileTime(path, mt) }, add = TRUE)
  body()
}

fq <- get("find_csv_file", asNamespace("gcamdata"))(CSV, FALSE, quiet = TRUE); md5_csv0 <- unname(tools::md5sum(fq))
with_edit(fq, function() {
  # touched but unchanged CSV: nothing runs
  Sys.setFileTime(fq, Sys.time()); p <- plan_minaj(XML)
  check("touched-but-unchanged CSV: nothing to run", length(p$chunks) == 0 && length(p$reads) == 0)
  # real CSV edit: change one number, rebuild
  txt <- readLines(fq, warn = FALSE); i <- grep("^[^#]", txt)[2]
  f <- strsplit(txt[i], ",", fixed = TRUE)[[1]]; num <- which(suppressWarnings(!is.na(as.numeric(f))))[1]
  fac <- 0.9 - (as.numeric(Sys.time()) %% 1000) / 1e5          # a new value every run, so the store cannot already know it
  f[num] <- format(as.numeric(f[num]) * fac, digits = 10); txt[i] <- paste(f, collapse = ","); writeLines(txt, fq)
  p <- plan_minaj(XML); msgs <- character(0)
  r <- el(withCallingHandlers(make_minaj(XML), message = function(m) msgs <<- c(msgs, conditionMessage(m))))
  check(sprintf("CSV edit: ran %d chunk(s) [%s] in %.1f s, file differs from built", length(p$chunks), paste(p$chunks, collapse = ", "), r$s),
        length(p$chunks) >= 1 && tools::md5sum(out) != tools::md5sum(built(XML)))
  sib <- grep("also changed", msgs, value = TRUE)
  say("   sibling report: %s", if (length(sib)) sub("\n.*", "", sib[1]) else "(none)")
})
check("CSV restored byte for byte", identical(unname(tools::md5sum(fq)), md5_csv0))
r <- el(make_minaj(XML)); check(sprintf("CSV restored: back to the built file without running (%.1f s, %s)", r$s, r$v$status), tools::md5sum(out) == tools::md5sum(built(XML)))

# chunk code edits: comment only (nothing runs), then a no-op statement (chunk runs, outputs same, XML reused)
rf <- file.path(G, "R", names(Filter(function(d) minaj:::chunk_name(CHUNK) %in% d, minaj:::.nm$file_defs))[1])
src <- readLines(rf, warn = FALSE); j <- grep("command == driver.MAKE", src, fixed = TRUE)[1]
with_edit(rf, function() {
  writeLines(append(src, "    # minaj test comment", after = j), rf)
  p <- plan_minaj(XML); check("comment-only chunk edit: nothing to run", length(p$chunks) == 0)
  writeLines(append(src, "    minaj_test_noop <- 1L", after = j), rf)
  p <- plan_minaj(XML); r <- el(make_minaj(XML))
  check(sprintf("no-op code edit: plan says %d chunk(s) [%s]; after the run the XML is unchanged (%s, %.1f s)", length(p$chunks), paste(p$chunks, collapse = ", "), r$v$status, r$s),
        tools::md5sum(out) == tools::md5sum(built(XML)))
})
p <- plan_minaj(XML); check("chunk file restored: nothing to run", length(p$chunks) == 0)

# low / mid / high
r <- el(v <- variants_minaj(XML, list(low = setNames(list(0.9), CSV), mid = setNames(list(1), CSV), high = setNames(list(1.1), CSV))))
check(sprintf("variants in %.1f s: mid identical to base, low and high differ", r$s), isTRUE(v$identical_to_base[2]) && !v$identical_to_base[1] && !v$identical_to_base[3] &&
        tools::md5sum(v$file[2]) == tools::md5sum(built(XML)))
r <- el(v2 <- variants_minaj(XML, list(low = setNames(list(0.9), CSV), mid = setNames(list(1), CSV), high = setNames(list(1.1), CSV))))
check(sprintf("variants again: %.1f s, all unchanged", r$s), all(v2$status == "unchanged"))

# variant table form, with a filter; a filter that matches nothing must stop
tab <- data.frame(variant = c("pvlow", "pvlow"), input = CSV, op = "scale", value = c("0.8", "0.9"), technology = c("PV", "*"))
tf <- file.path(tempdir(), "variants.csv"); utils::write.csv(tab, tf, row.names = FALSE)
r <- el(v3 <- variants_minaj(XML, tf))
check(sprintf("variant table (filter technology=PV) in %.1f s, differs from base", r$s), !isTRUE(v3$identical_to_base[1]))
utils::write.csv(transform(tab, technology = c("no_such_tech", "*")), tf, row.names = FALSE)
check("variant table with a filter matching no rows stops", inherits(try(variants_minaj(XML, tf), silent = TRUE), "try-error"))

# promote refuses a file that is not the current base result
dst <- file.path(minaj:::dest_dir(), XML); bak_dst <- readBin(dst, "raw", file.size(dst))
invisible(file.copy(v$file[1], dst, overwrite = TRUE))
check("promote_minaj refuses a variant placed under the base name", !isTRUE(suppressMessages(promote_minaj(XML))))
writeBin(bak_dst, dst)

# configuration check: exact files the configuration loads
cfgs <- list.files(file.path(minaj:::.nm$tree, "exe"), pattern = "^configuration.*\\.xml$", full.names = TRUE)
cfgs <- cfgs[vapply(cfgs, function(f) any(grepl("\\.\\./input/gcamdata/xml/", readLines(f, warn = FALSE))), logical(1))]
if (length(cfgs)) {
  r <- el(sc <- status_minaj(config = cfgs[1]))
  check(sprintf("status_minaj(%s): %d files, all results current and files identical (%.1f s)", basename(cfgs[1]), nrow(sc), r$s), all(sc$results == "current") && all(sc$file == "current"))
}

# independent re-runs of sampled chunks: outputs must hash exactly as drake stored them
co <- minaj:::.nm$co; secs <- minaj:::.nm$secs
cand <- names(secs)[secs < 15 & names(secs) %in% co$name]
set.seed(17); pick <- sample(cand, min(NRE, length(cand)))
ok <- vapply(pick, function(ch) {
  ctx <- minaj:::new_ctx(); r <- minaj:::chunk_result(ch, ctx)
  out <- run_minaj(ch, force = TRUE)
  same <- all(vapply(names(out), function(n) identical(minaj:::h64(out[n]), unname(r$out[n])), logical(1)))
  if (!same) say("   re-run differs: %s", ch)
  same
}, logical(1))
check(sprintf("forced re-runs of %d sampled chunks hash identically to drake's outputs (%d differ)", length(ok), sum(!ok)), all(ok))

# Java conversion from scratch for a few small XML files must reproduce drake's files
xs <- st$xml[file.size(built(st$xml)) < 2e6]; set.seed(3); xs <- sample(xs, min(5, length(xs)))
okx <- vapply(xs, function(x) {
  h <- minaj:::value_hash(x, minaj:::new_ctx()); tmp <- file.path(tempdir(), x)
  k <- minaj:::h64(list("xml", h, minaj:::writer_key())); rec <- minaj:::rec_get("xml", k)
  unlink(minaj:::rec_path("xml", k)); unlink(minaj:::xml_store_path(k))
  owd <- setwd(G); on.exit(setwd(owd)); minaj:::write_xml(x, h, tmp)
  same <- tools::md5sum(tmp) == tools::md5sum(built(x)); if (!is.null(rec)) minaj:::rec_put("xml", k, rec)
  same
}, logical(1))
check(sprintf("fresh Java conversions of %d XML files match drake's files", length(okx)), all(okx))
say("== done; peak memory %s MB", round(sum(gc()[, 6])))
