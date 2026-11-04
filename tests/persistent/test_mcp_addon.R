# MCP checks for the addon tool: persistent_minaj(mcp = TRUE) started here on <tree>, spoken to over standard input and output.
# Usage (from the package root): Rscript tests/persistent/test_mcp_addon.R <GCAM tree> <scratch folder> [floor]
#   Uses electricity.xml and energy/A23.globaltech_capacity_factor (GCAM 9.1 names). The server runs in <scratch folder>
#   (relative dest is read from there), with the store MINAJ_STORE (else the default store). With floor given, the server
#   starts with options(minaj.ram_floor_gb = floor) and only the floor check runs. MINAJ_HOME as in the other tests.
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 2L || !dir.exists(file.path(a[1], "input", "gcamdata"))) stop("usage: Rscript tests/persistent/test_mcp_addon.R <GCAM tree> <scratch folder> [floor]", call. = FALSE)
tree <- normalizePath(a[1], "/"); floor <- if (length(a) >= 3L) a[3] else NA
dir.create(a[2], recursive = TRUE, showWarnings = FALSE); scr <- normalizePath(a[2], "/")
load <- if (nzchar(Sys.getenv("MINAJ_HOME"))) sprintf("suppressMessages(pkgload::load_all('%s', quiet = TRUE))", normalizePath(Sys.getenv("MINAJ_HOME"), "/")) else "library(minaj)"
store <- if (nzchar(Sys.getenv("MINAJ_STORE"))) sprintf(", store = '%s'", normalizePath(Sys.getenv("MINAJ_STORE"), "/", mustWork = FALSE)) else ""
expr <- sprintf("%s; %sminaj::persistent_minaj('%s'%s, mcp = TRUE)", load, if (is.na(floor)) "" else sprintf("options(minaj.ram_floor_gb = %s); ", floor), tree, store)
rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
p <- processx::process$new(rs, c("-e", expr), stdin = "|", stdout = "|", stderr = file.path(scr, "server.log"), wd = scr)
bad <- character(0); id <- 0L; n_pass <- 0L; n_fail <- 0L
send <- function(x) p$write_input(paste0(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), "\n"))
req <- function(method, params, wait_min = 30) {                           # one request; its reply (other lines are kept in bad)
  id <<- id + 1L; send(list(jsonrpc = "2.0", id = id, method = method, params = params))
  t0 <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t0, units = "mins")) < wait_min) {
    if (!p$is_alive() && !length(p$read_output_lines())) break
    p$poll_io(1000)
    for (l in sub("\r$", "", p$read_output_lines())) {
      if (!nzchar(trimws(l))) next
      m <- tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(m)) { bad <<- c(bad, l); next }
      if (identical(m$id, id)) return(m)
      bad <<- c(bad, paste("unsolicited:", l))
    }
  }
  list(error = list(message = "no reply"))
}
call <- function(name, args) req("tools/call", list(name = name, arguments = args))
text <- function(m) if (!is.null(m$result$content)) strsplit(m$result$content[[1]]$text, "\n", fixed = TRUE)[[1]][1] else as.character(jsonlite::toJSON(if (is.null(m$error)) m$result else m$error, auto_unbox = TRUE))
body <- function(m) tryCatch(jsonlite::fromJSON(paste(strsplit(m$result$content[[1]]$text, "\n", fixed = TRUE)[[1]][-1], collapse = "\n"), simplifyVector = FALSE), error = function(e) list())
check <- function(label, m, ok) {
  good <- isTRUE(tryCatch(ok(m), error = function(e) FALSE))
  if (good) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
  cat(sprintf("%s %-58s %s\n", if (good) "PASS" else "FAIL", label, substr(text(m), 1, 200)))
}
is_err <- function(m) isTRUE(m$result$isError)
is_ok <- function(m) identical(m$result$isError, FALSE)
has <- function(m, s) grepl(s, text(m), fixed = TRUE)
cf <- "energy/A23.globaltech_capacity_factor"; ov <- function(v) setNames(list(v), cf)
dest <- file.path(scr, "addon_abs"); line <- 'add directly after the base file: <Value name="electricity_addon">'

check("initialize", req("initialize", list(protocolVersion = "2025-06-18", capabilities = setNames(list(), character(0)), clientInfo = list(name = "t", version = "0"))),
      function(m) grepl("addon writes only what an override changes", m$result$instructions, fixed = TRUE))
send(list(jsonrpc = "2.0", method = "notifications/initialized"))
check("tools/list: 10 tools, addon requires xml", req("tools/list", setNames(list(), character(0))), function(m) {
  t <- m$result$tools; ad <- Filter(function(x) x$name == "addon", t)[[1]]
  length(t) == 10L && identical(unlist(ad$inputSchema$required), "xml") && all(c("xml", "override", "config", "dest", "confirm") %in% names(ad$inputSchema$properties))
})
if (!is.na(floor)) {
  check("addon under the memory floor: refused (or unchanged, nothing run)", call("addon", list(xml = "electricity.xml", override = ov(1.07), dest = dest)),
        function(m) is_err(m) && has(m, "minaj.ram_floor_gb") || is_ok(m) && has(m, "unchanged"))
} else {
  check("addon without xml -> error", call("addon", list(override = ov(1.1))), function(m) is_err(m) && has(m, "give xml"))
  check("addon with an unknown input -> error", call("addon", list(xml = "electricity.xml", override = list("energy/no_such_input" = 1.1))), function(m) is_err(m) && has(m, "not a CSV input or data object"))
  check("addon without override, empty sandbox -> error", call("addon", list(xml = "electricity.xml")), function(m) is_err(m) && has(m, "give override"))
  check("addon on a name that is not an XML file -> error", call("addon", list(xml = "no_such.xml", override = ov(1.1))), function(m) is_err(m) && has(m, "not an XML file"))
  check("addon with a config that does not exist -> error", call("addon", list(xml = "electricity.xml", override = ov(1.1), config = file.path(scr, "no_such_config.xml"))), is_err)
  m1 <- call("addon", list(xml = "electricity.xml", override = ov(1.1), dest = dest))
  check("addon x1.1, absolute dest: written or unchanged, universal, config line", m1,
        function(m) is_ok(m) && grepl("electricity_addon\\.xml (written|unchanged) \\(universal add-on\\)", text(m)) && has(m, line) && file.exists(file.path(dest, "electricity_addon.xml")))
  check("  results row: status and file", m1, function(m) { r <- body(m)$results; x <- if (!is.null(r$status)) r else r[[1]]; grepl("written|unchanged", x$status) && grepl("electricity_addon\\.xml$", x$file) })
  check("addon x1.1 again: unchanged", call("addon", list(xml = "electricity.xml", override = ov(1.1), dest = dest)), function(m) is_ok(m) && has(m, "electricity_addon.xml unchanged"))
  check("addon x1.1, relative dest: read from the server folder", call("addon", list(xml = "electricity.xml", override = ov(1.1), dest = "addon_rel")),
        function(m) is_ok(m) && file.exists(file.path(scr, "addon_rel", "electricity_addon.xml")))
  check("try x1.1", call("try", list(set = ov(1.1))), function(m) is_ok(m) && has(m, "sandbox active"))
  check("addon without override uses the sandbox: unchanged (same override)", call("addon", list(xml = "electricity.xml", dest = dest)),
        function(m) is_ok(m) && has(m, "electricity_addon.xml unchanged") && identical(paste(unlist(body(m)$sandbox), collapse = ","), cf))
  check("addon with override ignores the sandbox (x1.2 written)", call("addon", list(xml = "electricity.xml", override = ov(1.2), dest = dest)),
        function(m) is_ok(m) && has(m, "electricity_addon.xml written") && !"sandbox" %in% names(body(m)))
  check("try clear", call("try", list(clear = TRUE)), function(m) is_ok(m) && has(m, "sandbox empty"))
  check("unknown tool still -32602, lists addon", call("stop", setNames(list(), character(0))), function(m) identical(m$error$code, -32602L) && grepl("addon", m$error$message, fixed = TRUE))
}
close(p$get_input_connection()); p$wait(60000); if (p$is_alive()) p$kill_tree()   # end of input: the server stops
cat(sprintf("non-protocol lines on standard output: %s\n", if (length(bad)) paste(bad, collapse = " | ") else "none"))
if (length(bad)) { n_fail <- n_fail + 1L; cat("FAIL standard output holds only protocol messages\n") }
cat(sprintf("RESULT mcp addon%s: PASS %d FAIL %d\n", if (is.na(floor)) "" else " (floor)", n_pass, n_fail))
quit(status = if (n_fail) 1L else 0L)
