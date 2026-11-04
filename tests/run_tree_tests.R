# Runs the tests that need a GCAM tree, one after another, each in its own process, and writes one summary.
# Usage (from the package root): Rscript tests/run_tree_tests.R [tree] [store] [scratch folder] [test ...]
#   tree: a vanilla GCAM 9.1 tree with its XML files in input/gcamdata/xml (default: environment variable MINAJ_TEST_TREE);
#   store: a minaj store holding a full build of that tree, its XML files written at least once with driver_minaj()
#   (default: MINAJ_TEST_STORE, else minaj's default store: option minaj.store, else minaj_store in
#   tools::R_user_dir("minaj", "cache")); scratch folder: where the tests write (default: a new folder in tempdir());
#   test: run only these (unit, corpus, xml_workers, drake_like, pm, mcp, mcp_floor, outputs).
#   outputs first builds its reference with gcamdata's own driver(), a full gcamdata build (about 25 min and 9.5 GB at peak
#   on vanilla 9.1, more than its memory gate waits for), unless <scratch folder>/outputs/reference is already there.
#   The tests load minaj from this source folder (MINAJ_HOME) and use the store (MINAJ_STORE). Each starts only while free
#   memory is at least 4 GB plus its measured peak (1-2 GB), waiting up to 10 minutes; otherwise it is reported as not run. Needs Java;
#   pm needs bash and curl; drake_like needs drake and the tree's drake cache. unit also reads the tree's C++ parse code
#   (cvs/objects; without it, SKIP for those checks) and input/solution/cal_broyden_config.xml when it is there.
#   Writes <scratch>/<test>.log and <scratch>/summary.csv; exits with status 1 when a check failed or a test did not finish.
a <- commandArgs(trailingOnly = TRUE)
tests <- c("unit", "corpus", "xml_workers", "drake_like", "pm", "mcp", "mcp_floor", "outputs")
only <- a[a %in% tests]; a <- a[!a %in% tests]
tree <- if (length(a) >= 1L) a[1] else Sys.getenv("MINAJ_TEST_TREE")
store <- if (length(a) >= 2L) a[2] else Sys.getenv("MINAJ_TEST_STORE")
scr <- if (length(a) >= 3L) a[3] else tempfile("minaj_tree_tests_")
if (!nzchar(tree) || !dir.exists(file.path(tree, "input", "gcamdata"))) stop("usage: Rscript tests/run_tree_tests.R <GCAM tree> [store] [scratch folder] [test ...]", call. = FALSE)
if (!file.exists("tests/run_tree_tests.R")) stop("run from the minaj package root", call. = FALSE)
tree <- normalizePath(tree, "/"); dir.create(scr, recursive = TRUE, showWarnings = FALSE); scr <- normalizePath(scr, "/")
Sys.setenv(MINAJ_HOME = normalizePath(".", "/"), MINAJ_TEST_TREE = tree)
if (!nzchar(store)) store <- getOption("minaj.store", file.path(tools::R_user_dir("minaj", "cache"), "minaj_store"))
Sys.setenv(MINAJ_STORE = normalizePath(store, "/", mustWork = FALSE))
rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
bash <- Sys.which("bash")
if (.Platform$OS.type == "windows" && (!nzchar(bash) || grepl("system32", bash, ignore.case = TRUE))) {   # Git's bash, not WSL's
  git <- Sys.which("git"); bash <- if (nzchar(git)) file.path(dirname(dirname(git)), "bin", "bash.exe") else ""
}
steps <- list(
  unit        = list(gb = 1.5, cmd = rs, args = c("tests/addon/unit.R", file.path(tree, "input", "gcamdata", "xml"))),
  corpus      = list(gb = 2, cmd = rs, args = c("tests/addon/corpus.R", tree, file.path(scr, "corpus"))),
  xml_workers = list(gb = 1.5, cmd = rs, args = c("tests/test_xml_workers.R", tree, file.path(scr, "xml_workers"))),
  drake_like  = list(gb = 1, cmd = rs, args = c("tests/test_drake_like.R", tree)),
  pm          = list(gb = 1.5, cmd = bash, args = c("tests/persistent/test_pm_addon.sh", tree, file.path(scr, "pm"))),
  mcp         = list(gb = 1.5, cmd = rs, args = c("tests/persistent/test_mcp_addon.R", tree, file.path(scr, "mcp"))),
  mcp_floor   = list(gb = 1, cmd = rs, args = c("tests/persistent/test_mcp_addon.R", tree, file.path(scr, "mcp_floor"), "1000")),
  outputs     = list(gb = 3, cmd = rs, args = c("tests/test_outputs.R", tree, file.path(scr, "outputs"))))
if (length(only)) steps <- steps[only]
free <- function() ps::ps_system_memory()$avail / 2^30
cat(sprintf("minaj tree tests | tree %s | store %s | writing to %s\n", tree, Sys.getenv("MINAJ_STORE"), scr))
rows <- list()
for (k in names(steps)) {
  s <- steps[[k]]; log <- file.path(scr, paste0(k, ".log")); need <- 4 + s$gb
  t0 <- Sys.time()
  while (free() < need && as.numeric(difftime(Sys.time(), t0, units = "mins")) < 10) Sys.sleep(15)
  if (free() < need) { note <- sprintf("not run: %.1f GB free, under %.1f GB for 10 minutes", free(), need); st <- NA }
  else if (!nzchar(s$cmd) || !file.exists(s$cmd)) { note <- "not run: bash not found"; st <- NA }
  else {
    cat(sprintf("%s %s ...\n", format(Sys.time(), "%H:%M:%S"), k)); flush.console()
    t1 <- Sys.time(); st <- system2(s$cmd, shQuote(s$args, type = if (.Platform$OS.type == "windows") "cmd" else "sh"), stdout = log, stderr = log)
    note <- sprintf("%.1f min", as.numeric(difftime(Sys.time(), t1, units = "mins")))
  }
  l <- if (file.exists(log)) readLines(log, warn = FALSE) else character(0)
  n <- function(w) sum(startsWith(l, paste0(w, " ")))
  rows[[k]] <- data.frame(test = k, pass = n("PASS"), fail = n("FAIL"), skip = n("SKIP"), exit = st, note = note)
  cat(sprintf("  %-12s PASS %4d  FAIL %3d  SKIP %3d  exit %s  %s\n", k, n("PASS"), n("FAIL"), n("SKIP"), st, note))
  if (n("FAIL")) cat(paste0("    ", head(l[startsWith(l, "FAIL ")], 10)), sep = "\n")
}
res <- do.call(rbind, rows)
utils::write.csv(res, file.path(scr, "summary.csv"), row.names = FALSE)
ok <- all(res$fail == 0) && all(res$exit %in% 0)
cat(sprintf("\n%s: %d tests, PASS %d, FAIL %d, SKIP %d; not finished: %s\nlogs and summary.csv in %s\n", if (ok) "ALL PASSED" else "FAILURES",
            nrow(res), sum(res$pass), sum(res$fail), sum(res$skip), if (any(!res$exit %in% 0)) paste(res$test[!res$exit %in% 0], collapse = ", ") else "none", scr))
quit(status = if (ok) 0L else 1L)
