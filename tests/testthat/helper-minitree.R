# The tests run on copies of the made-up gcamdata tree in minitree/: five chunks, three CSV inputs (one more optional
# and missing) and two XML files, mini.xml and other.xml.
#   mini/A100.base -> module_mini_L100.base -> L100.base \
#   mini/A200.coef -----------------------> module_mini_L200.coef -> L200.* -> module_mini_mini_xml -> mini.xml
#   mini/A201.other -> module_mini_L201.other -> L201.* -> module_mini_other_xml -> other.xml

options(minaj.ask_secs = Inf, minaj.ram_floor_gb = 0.5)     # the XML files here are tiny (Java heap 256 MB)

# Copies the made-up tree to a new temporary folder and opens it with an empty store; returns its gcamdata folder.
open_mini <- function() {
  d <- tempfile("minaj_"); dir.create(d)
  file.copy(test_path("minitree"), d, recursive = TRUE)
  suppressMessages(open_minaj(file.path(d, "minitree"), store = file.path(d, "store")))
  info_minaj()$gcamdata
}

# Rewrites a file of the open tree (path relative to its gcamdata folder): f(lines) returns the new lines. The
# modification time moves on by at least a second, so that an edit of the same size is seen as well.
edit_mini <- function(rel, f) {
  p <- file.path(info_minaj()$gcamdata, rel); old <- file.mtime(p)
  writeLines(f(readLines(p, warn = FALSE)), p)
  Sys.setFileTime(p, max(Sys.time(), old + 1))
}

# Computes targets as make_minaj() does, without writing XML files; returns the chunks that ran.
ran <- function(...) {
  restore <- minaj:::enter(); on.exit(minaj:::leave(restore))
  ctx <- minaj:::new_ctx()
  for (x in c(...)) minaj:::value_hash(x, ctx)
  unique(ctx$ran)
}

skip_if_no_java <- function() skip_if_not(nzchar(Sys.which("java")), "Java not found")

all_chunks <- c("module_mini_L100.base", "module_mini_L200.coef", "module_mini_L201.other", "module_mini_mini_xml", "module_mini_other_xml")
