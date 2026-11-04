# Background XML writers (minaj.xml_workers = 3) on real files of a tree, from a copy of a store without XML records:
# (1) memory floor set very high: nothing converted, each file reported FAILED with the floor message, no file, no record;
# (2) a worker killed during a file (the R process only, so its Java is left behind): that file written again in the main
#     process, byte-identical, no Java left running;
# (3) a conversion made to fail (one file's worker gets a header file that does not exist; Java reports it and writes an empty
#     file): that file reported FAILED, no file and no record for it; the other files written, byte-identical;
# (4) no part files left in the store, records for exactly the files written.
# Usage (from the package root): Rscript tests/test_xml_workers.R [tree] [scratch folder]
#   tree: default environment variable MINAJ_TEST_TREE. The store (MINAJ_STORE, else the default store) must hold results
#   for the tree and the XML files written from them at least once (driver_minaj()); 8 of those files (7 over 0.2 MB, one
#   near 60 MB) are written again to <scratch>/ref with it, as the reference. The copy of the store (values hard-linked
#   where the drive allows, else copied) and the files written in the checks go to <scratch> and are removed when all pass.
#   MINAJ_HOME: load minaj from this source folder instead of the installed package. Needs Java and about 4 GB free.
a <- commandArgs(trailingOnly = TRUE)
tree <- if (length(a) >= 1L) a[1] else Sys.getenv("MINAJ_TEST_TREE")
out <- if (length(a) >= 2L) a[2] else file.path(tempdir(), "xml_workers")
if (!nzchar(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/test_xml_workers.R <GCAM tree> [scratch folder]", call. = FALSE)
if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
options(minaj.ask_secs = Inf); if (nzchar(Sys.getenv("MINAJ_STORE"))) options(minaj.store = Sys.getenv("MINAJ_STORE"))
ns <- asNamespace("minaj"); nm <- get(".nm", ns)
st <- file.path(out, "store_chk"); dest <- file.path(out, "xml_chk"); ref <- file.path(out, "ref")
if (dir.exists(st) || dir.exists(dest)) stop("already there: ", st, " or ", dest, call. = FALSE)

# the files: sizes from the store's records of the current XML files
suppressMessages(open_minaj(tree)); s0 <- info_minaj()$store
xs <- nm$co$output[nm$co$to_xml]; ctx0 <- ns$new_ctx(dry = TRUE)
sz <- vapply(xs, function(x) {
  h <- ns$value_hash(x, ctx0); r <- if (is.na(h)) NULL else ns$rec_get("xml", ns$h64(list("xml", h, ns$writer_key())))
  if (is.null(r)) NA_real_ else as.numeric(r$size)
}, 0)
small <- head(names(sort(sz[!is.na(sz) & sz > 0.2 * 2^20])), 7)
mid <- if (any(!is.na(sz) & sz > 20 * 2^20)) names(sz)[which.min(abs(ifelse(is.na(sz), Inf, sz) - 60 * 2^20))] else NA
if (length(small) < 7L || is.na(mid)) { cat(sprintf("SKIP xml workers: the store %s has no written XML files of this tree to compare with; run driver_minaj() with it first\n", s0)); quit(status = 0) }
cat(sprintf("files: %s; killed: %s (%.0f MB)\n", paste(small, collapse = ", "), mid, sz[[mid]] / 2^20))
r0 <- suppressMessages(make_minaj(c(small, mid), dest = ref))
if (any(!r0$status %in% c("converted", "copied from store", "unchanged"))) stop("reference files not written: ", paste(r0$target, r0$status, collapse = "; "))

# the copy of the store: values, results and CSV reads, the tree's graph; no XML records
for (d in c("rec/res", "rec/csv", "trees")) {
  dir.create(file.path(st, dirname(d)), recursive = TRUE, showWarnings = FALSE)
  if (!all(file.copy(file.path(s0, d), file.path(st, dirname(d)), recursive = TRUE))) stop("copy failed: ", d)
}
of <- list.files(file.path(s0, "obj"), recursive = TRUE)
for (dd in unique(dirname(of))) dir.create(file.path(st, "obj", dd), recursive = TRUE, showWarnings = FALSE)
lk <- suppressWarnings(file.link(file.path(s0, "obj", of), file.path(st, "obj", of)))
if (any(!lk)) file.copy(file.path(s0, "obj", of[!lk]), file.path(st, "obj", of[!lk]))
cat(sprintf("store copy: %d values (%d hard-linked)\n", length(of), sum(lk)))
options(minaj.xml_workers = 3)
suppressMessages(open_minaj(tree, store = st))
n_pass <- 0L; n_fail <- 0L
check <- function(ok, what) {
  if (isTRUE(ok)) { n_pass <<- n_pass + 1L; cat("PASS", what, "\n") } else { n_fail <<- n_fail + 1L; cat("FAIL", what, "\n") }
}
ctx <- ns$new_ctx()
same <- function(f) file.exists(file.path(dest, f)) && identical(unname(tools::md5sum(file.path(dest, f))), unname(tools::md5sum(file.path(ref, f))))
recs <- function() vapply(list.files(file.path(st, "rec", "xml"), recursive = TRUE, full.names = TRUE), function(f) readRDS(f)$name, "")
queue <- function(files) do.call(rbind, lapply(files, function(x) { ns$xml_pool(); ns$make_one(x, ctx, dest) }))
final <- function(rows) {                                                  # as driver_minaj(): the statuses once all are written
  force(rows)                                                              # final(queue(...)): the files are queued before the wait
  ns$xml_pool(wait = TRUE); s <- nm$pool$status
  setNames(vapply(seq_len(nrow(rows)), function(i) if (rows$status[i] != "queued") rows$status[i] else if (is.null(s[[rows$target[i]]])) "FAILED: not written" else s[[rows$target[i]]], ""), rows$target)
}
java_pids <- function(pattern) {                                           # java processes whose command line holds pattern
  if (.Platform$OS.type == "windows") {                                    # a CIM query takes about 1 s; listing processes with ps takes 10-25 s on Windows
    jcmd <- "Get-CimInstance Win32_Process -Filter \"Name='java.exe'\" | ForEach-Object { \"$($_.ProcessId)`t$($_.CommandLine)\" }"
    o <- suppressWarnings(system2("powershell", c("-NoProfile", "-NonInteractive", "-Command", "-"), input = jcmd, stdout = TRUE, stderr = FALSE))
    return(as.integer(sub("\t.*$", "", o[grepl(pattern, o, fixed = TRUE)])))
  }
  p <- ps::ps(); p <- p[grepl("^java", p$name), , drop = FALSE]
  p$pid[vapply(p$ps_handle, function(h) any(grepl(pattern, tryCatch(ps::ps_cmdline(h), error = function(e) ""), fixed = TRUE)), NA)]
}

# (1) memory floor
options(minaj.ram_floor_gb = 1000)
check(ns$xml_pool(start = TRUE), "pool started")
s1 <- final(queue(small[1:2]))
check(all(grepl("^FAILED: .*under the floor of 1000 GB", s1)), sprintf("floor: both reported FAILED with the floor message (%s)", paste(substr(s1, 1, 60), collapse = " / ")))
check(!any(file.exists(file.path(dest, small[1:2]))) && !length(recs()), "floor: no file, no record")
ns$xml_pool(stop = TRUE); options(minaj.ram_floor_gb = 4)

# (2) a worker killed during a file
ns$xml_pool(start = TRUE)
rows <- queue(c(mid, small[3]))
killed <- FALSE; t0 <- Sys.time()
while (!killed && as.numeric(difftime(Sys.time(), t0, units = "secs")) < 120) {
  for (r in nm$pool$run) if (r$job$name == mid && r$proc$is_alive() && length(jp <- java_pids(r$job$out))) {
    Sys.sleep(1); r$proc$kill(); killed <- TRUE                             # the R worker only: its Java keeps running
    cat(sprintf("  killed worker %d; its Java %s still running: %s\n", r$proc$get_pid(), paste(jp, collapse = " "), length(java_pids(r$job$out)) > 0)); break
  }
  if (!killed) Sys.sleep(0.2)
}
check(killed, "kill: worker killed while its Java was running")
s2 <- final(rows)
check(identical(unname(s2[mid]), "converted") && same(mid), sprintf("kill: %s written again in this process, byte-identical (%s)", mid, s2[mid]))
check(identical(unname(s2[small[3]]), "converted") && same(small[3]), sprintf("kill: %s written, byte-identical (%s)", small[3], s2[small[3]]))
Sys.sleep(2); check(!length(java_pids(st)), "kill: no Java left running")
ns$xml_pool(stop = TRUE)

# (3) a conversion made to fail
ns$xml_pool(start = TRUE)
good_hdr <- nm$pool$hdr; nm$pool$hdr <- file.path(tempdir(), "no_such_headers.txt")
r1 <- queue(small[4])                                                      # starts at once; Java reports the missing header file
check(length(nm$pool$run) == 1L, "fail: the file's worker started")
nm$pool$hdr <- good_hdr
s3 <- final(rbind(r1, queue(small[5:7])))
check(grepl("^FAILED", s3[small[4]]), sprintf("fail: %s reported FAILED (%s)", small[4], substr(s3[small[4]], 1, 120)))
check(!file.exists(file.path(dest, small[4])) && !small[4] %in% recs(), "fail: no file, no record for it")
check(all(s3[small[5:7]] == "converted") && all(vapply(small[5:7], same, NA)), "fail: the other 3 files written, byte-identical")
ns$xml_pool(stop = TRUE)

# (4) leftovers
check(!length(list.files(file.path(st, "xml"), pattern = "[.]part[.]xml$", recursive = TRUE)), "no part files left in the store")
check(setequal(recs(), c(mid, small[c(3, 5:7)])), sprintf("records for exactly the 5 written files (%d)", length(recs())))

cat(sprintf("RESULT xml workers: PASS %d FAIL %d\n", n_pass, n_fail))
if (n_fail == 0L) unlink(c(st, dest), recursive = TRUE)
quit(status = if (n_fail) 1L else 0L)
