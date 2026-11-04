# persistent_minaj: one long-running R process that keeps gcamdata loaded and answers short commands over
# HTTP on 127.0.0.1 (token-protected), so a caller (a script or another program) pays no R start-up or
# package load per call. Every answer is JSON with a one-line `summary`. The `pm` script in inst/bin is the client.
#
# Commands (POST /<command>, JSON body):
#   status | make {targets, confirm} | plan {targets} | get {name, n, filter} | diff {name, against, top}
#   trace {name, direction, depth, value, filter} | downstream {from} | variants {xml, variants}
#   addon {xml, override (default: the sandbox), config, dest, confirm}
#   try {set, clear} (sandbox overrides used by make/plan/get/diff/addon until cleared) | stop
# With mcp = TRUE the same commands (without stop) are MCP tools, served as JSON-RPC lines on standard input and output.

#' Start the persistent minaj worker for a tree
#'
#' Keeps gcamdata loaded in one R process and answers commands until stopped, so run it in its own R
#' process. By default it answers `pm` commands over HTTP on 127.0.0.1 and writes the port and a token to
#' <store>/server/current.json and <store>/server/<tree>_<id>.json, where `pm` reads them; it stops on
#' `pm stop`. With `mcp = TRUE` it is an MCP server on standard input and output instead, with the tools
#' status, make, plan, get, diff, trace, downstream, variants, addon and try (the `pm` commands); it stops
#' when the client closes its input.
#' @param tree GCAM tree (folder holding input/gcamdata) or the gcamdata folder itself.
#' @param port TCP port on 127.0.0.1 (default: a free one). Not used with `mcp = TRUE`.
#' @param store Store folder (default: option minaj.store, else
#'   file.path(tools::R_user_dir("minaj", "cache"), "minaj_store")).
#' @param mcp Serve the Model Context Protocol on standard input and output instead of HTTP.
#' @return Invisibly TRUE, once stopped.
#' @export
persistent_minaj <- function(tree, port = NULL, store = nm_opt("store", file.path(tools::R_user_dir("minaj", "cache"), "minaj_store")),
                             mcp = FALSE) {
  options(minaj.ask_secs = Inf, minaj.report_siblings = FALSE)
  if (isTRUE(mcp)) {
    # The tree is opened at the first tool call, so the handshake is answered at once. Standard output carries
    # only the protocol: anything printed while opening goes to standard error.
    opened <- FALSE
    open_tree <- function() {
      printed <- utils::capture.output(
        tryCatch(open_minaj(tree, store = store), error = function(e) { .nm$rfiles <- NULL; stop(e) }))   # a failed open reopens fully next time
      if (length(printed)) cat(printed, sep = "\n", file = stderr())
      .nm$sandbox <- list(); .nm$last_hash <- list(); .nm$prev_hash <- list(); .nm$started <- Sys.time()
      opened <<- TRUE
    }
    obj <- function(...) { x <- list(...); if (length(x)) x else structure(list(), names = character(0)) }
    tool <- function(name, description, properties = obj(), required = character(0)) {
      schema <- list(type = "object", properties = properties)
      if (length(required)) schema$required <- I(required)
      list(name = name, description = description, inputSchema = schema)
    }
    targets <- list(type = "array", items = list(type = "string"),
                    description = "XML file names (electricity.xml), data names, CSV input names or chunk names (full, or a unique suffix such as energy_L223.electricity)")
    table <- list(type = "string", description = "Data name (e.g. L223.StubTechCapFactor_elec) or CSV input name (e.g. energy/A23.globaltech_capacity_factor)")
    filter <- list(type = "object", additionalProperties = TRUE, description = "Column name -> value, or array of values, to keep")
    override <- paste("a number (multiplies every year column, else the value column); for a CSV input only, the absolute path of a CSV file",
                      "with the same columns; or an array of row operations {op: scale|add|set, value, column (default: every year column, else",
                      "value), <filter column>: value}, with several columns or filter values separated by ;")
    tools <- list(
      tool("status", "gcamdata tree, store, XML folder and active sandbox of this minaj worker."),
      tool("make", paste("Build GCAM input XML files from gcamdata, or gcamdata tables or chunks, reusing every stored result that is still current.",
                         "XML files go to the XML folder (status shows it; default input/gcamdata/xml_minaj/), with an active sandbox as",
                         "<stem>_override.xml. Reports what was converted, copied or unchanged, what ran, and which other XML files the same edit",
                         "put out of date. Refuses to run chunks or the Java XML writer when free memory is under option minaj.ram_floor_gb (default 4 GB)."),
           obj(targets = targets, confirm = list(type = "boolean", description = "Set true to run when the estimate exceeds option minaj.server_ask_secs (default 300 s)")), "targets"),
      tool("plan", "What make would run in gcamdata for these targets, and why, without running anything.", obj(targets = targets), "targets"),
      tool("get", paste("Columns, value ranges and first rows of a gcamdata table, from the store or computed (runs the chunks it needs;",
                        "refuses when free memory is under option minaj.ram_floor_gb)."),
           obj(name = table, n = list(type = "integer", description = "Rows to return (default 5)"), filter = filter), "name"),
      tool("diff", "How a gcamdata table or XML file changed: rows changed, added and removed, largest relative change per column, regions and years affected.",
           obj(name = list(type = "string", description = "Data name, CSV input name or XML file name (e.g. electricity.xml)"),
               against = list(type = "string", enum = c("previous", "base"),
                              description = "previous (default): the two latest different versions that make produced in this session (needs a make before and after the edit); base: the sandbox against the tree"),
               top = list(type = "integer", description = "Number of largest changes to list (default 10)")), "name"),
      tool("trace", paste("Lineage of a gcamdata table: the chunks and CSV files it comes from (upstream) or the tables and XML files it feeds",
                          "(downstream). With value: up to 25 rows of the already-built tables on that path, CSV inputs included, whose number",
                          "matches value to within 1e-9; pass the unrounded number."),
           obj(name = table, direction = list(type = "string", enum = c("upstream", "downstream"), description = "Default upstream"),
               depth = list(type = "integer", description = "Number of steps to follow from the table (default 6)"),
               value = list(type = "number", description = "Number to find"), filter = filter), "name"),
      tool("downstream", "gcamdata chunks and GCAM input XML files that an edit to these CSV inputs, data objects or chunks reaches.",
           obj(from = list(type = "array", items = list(type = "string"),
                           description = "CSV input names (e.g. energy/A23.globaltech_capacity_factor), data names or chunk names (full, or a unique suffix)")), "from"),
      tool("variants", paste("Write versions of one GCAM input XML file (for example low / mid / high) in one call; each variant needs at least one",
                             "override of an input upstream of the XML (an override of 1 writes an unchanged copy of the base); files go to",
                             "xml_minaj/variants/<stem>_<variant>.xml. Refuses when free memory is under option minaj.ram_floor_gb."),
           obj(xml = list(type = "string", description = "XML file name, e.g. electricity.xml"),
               variants = list(type = "object", additionalProperties = TRUE, description = paste("Variant name -> {CSV input or data name -> override}; an override is", override))),
           c("xml", "variants")),
      tool("addon", paste("Write <stem>_addon.xml with only what an override changes, to load in a GCAM configuration directly after this exact",
                          "base file (addon_minaj); also writes the base file and <stem>_override.xml. With config, places that depend on the",
                          "files the configuration loads before the base file are decided from those files, and the add-on can be",
                          "configuration-specific. Changes an add-on cannot carry are refused with each place and reason (use",
                          "<stem>_override.xml for those). Refuses when free memory is under option minaj.ram_floor_gb. The summary gives the",
                          "configuration line to add directly after the base file."),
           obj(xml = list(type = "string", description = "XML file name, e.g. electricity_water.xml"),
               override = list(type = "object", additionalProperties = TRUE, description = paste("CSV input or data name -> override (default: the sandbox); an override is", override)),
               config = list(type = "string", description = "Absolute path (forward slashes) of a GCAM configuration that loads the base file exactly once"),
               dest = list(type = "string", description = "Absolute path (forward slashes) of the folder for the add-on (default: addon/ in the XML folder)"),
               confirm = list(type = "boolean", description = "Set true to run when the estimate exceeds option minaj.server_ask_secs (default 300 s)")),
           "xml"),
      tool("try", "Sandbox overrides of gcamdata inputs without editing any file: make, plan, get, diff and addon use them until cleared, and make writes <stem>_override.xml.",
           obj(set = list(type = "object", additionalProperties = TRUE, description = paste("CSV input or data name -> override; an override is", override)),
               clear = list(type = "boolean", description = "Empty the sandbox first"))))
    names_ok <- vapply(tools, `[[`, "", "name")
    versions <- c("2025-06-18", "2024-11-05")
    instructions <- paste0("minaj keeps one gcamdata tree (the GCAM input data system) loaded: ", normalizePath(tree, "/", mustWork = FALSE), ". ",
                           "make and variants build GCAM input XML files one at a time, reusing stored results; addon writes only what an override ",
                           "changes, to load directly after the base file; plan, get, diff, trace and downstream ",
                           "inspect gcamdata tables, CSV inputs and chunks; try sets overrides in a sandbox without editing files. After editing a CSV ",
                           "input or chunk file, call make on the XML files it feeds (downstream lists them). Names: XML files (electricity.xml), data ",
                           "names (L223.StubTechCapFactor_elec), CSV inputs (energy/A23.globaltech_capacity_factor), chunks (module_energy_L223.electricity).")
    send <- function(x) {
      cat(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, dataframe = "rows")), "\n", sep = "")
      flush(stdout())
    }
    con <- file("stdin", open = "r", encoding = "UTF-8"); on.exit(close(con), add = TRUE)
    message("persistent_minaj: MCP server for ", tree, ", store ", normalizePath(store, "/", mustWork = FALSE), " (pid ", Sys.getpid(),
            "); the tree opens at the first tool call")
    repeat {
      line <- readLines(con, n = 1, warn = FALSE)
      if (!length(line)) break
      if (!nzchar(trimws(line))) next
      msg <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(msg)) { send(list(jsonrpc = "2.0", id = NULL, error = list(code = -32700L, message = "parse error"))); next }
      if (!is.list(msg) || is.null(names(msg))) {
        send(list(jsonrpc = "2.0", id = NULL, error = list(code = -32600L, message = "invalid request: one JSON-RPC object per line"))); next
      }
      if (is.null(msg$id)) next                       # notifications need no answer
      if (!is.null(msg$params) && !is.list(msg$params)) {
        send(list(jsonrpc = "2.0", id = msg$id, error = list(code = -32602L, message = "params must be an object"))); next
      }
      reply <- tryCatch({
        res <- NULL; err <- NULL
        if (identical(msg$method, "initialize")) {
          v <- msg$params$protocolVersion
          res <- list(protocolVersion = if (is.character(v) && v %in% versions) v else versions[1],
                      capabilities = list(tools = list(listChanged = FALSE)),
                      serverInfo = list(name = "minaj", version = tryCatch(as.character(utils::packageVersion("minaj")), error = function(e) "0")),
                      instructions = instructions)
        } else if (identical(msg$method, "ping")) {
          res <- obj()
        } else if (identical(msg$method, "tools/list")) {
          res <- list(tools = tools)
        } else if (identical(msg$method, "tools/call")) {
          cmd <- msg$params$name
          if (!is.character(cmd) || !cmd %in% names_ok) {
            err <- list(code = -32602L, message = paste0("unknown tool '", paste(cmd, collapse = ""), "'; tools: ", paste(names_ok, collapse = " ")))
          } else {
            a <- msg$params$arguments
            a <- if (length(a)) jsonlite::fromJSON(as.character(jsonlite::toJSON(a, auto_unbox = TRUE, null = "null", digits = NA)), simplifyVector = TRUE) else list()
            t0 <- proc.time()[["elapsed"]]; msgs <- character(0); warns <- character(0)
            printed <- utils::capture.output(
              out <- tryCatch(withCallingHandlers({ if (!opened) open_tree(); dispatch(cmd, a) },
                                                  message = function(m) { msgs <<- c(msgs, trimws(conditionMessage(m))); invokeRestart("muffleMessage") },
                                                  warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
                              error = function(e) list(ok = FALSE, summary = paste("error:", conditionMessage(e)))))
            if (is.null(out$ok)) out$ok <- TRUE
            out$secs <- round(proc.time()[["elapsed"]] - t0, 2)
            if (length(msgs)) out$messages <- utils::tail(msgs, 12)
            if (length(warns)) out$warnings <- utils::tail(unique(warns), 8)
            if (length(printed)) out$printed <- utils::tail(printed, 20)
            out <- c(list(ok = out$ok, summary = out$summary, secs = out$secs), out[setdiff(names(out), c("ok", "summary", "secs"))])
            txt <- as.character(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", na = "null", digits = NA, dataframe = "rows"))
            res <- list(content = list(list(type = "text", text = paste0(out$summary, "\n", txt))), isError = !isTRUE(out$ok))
          }
        } else {
          err <- list(code = -32601L, message = paste0("method not found: ", paste(msg$method, collapse = "")))
        }
        c(list(jsonrpc = "2.0", id = msg$id), if (is.null(err)) list(result = res) else list(error = err))
      }, error = function(e) list(jsonrpc = "2.0", id = msg$id, error = list(code = -32603L, message = paste("internal error:", conditionMessage(e)))))
      tryCatch(send(reply), error = function(e)
        send(list(jsonrpc = "2.0", id = msg$id, error = list(code = -32603L, message = paste("internal error:", conditionMessage(e))))))
    }
    return(invisible(TRUE))
  }
  open_minaj(tree, store = store)
  if (is.null(port)) port <- httpuv::randomPort()
  token <- paste(sample(c(letters, LETTERS, 0:9), 32, TRUE), collapse = "")
  info <- list(port = port, token = token, pid = Sys.getpid(), tree = .nm$tree, store = .nm$store, started = format(Sys.time()))
  sdir <- file.path(.nm$store, "server"); dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  for (f in c("current.json", paste0(.nm$tree_id, ".json"))) {
    writeLines(jsonlite::toJSON(info, auto_unbox = TRUE, pretty = TRUE), file.path(sdir, f))
    Sys.chmod(file.path(sdir, f), "600")
  }
  .nm$sandbox <- list(); .nm$last_hash <- list(); .nm$prev_hash <- list(); .nm$serve <- TRUE; .nm$started <- Sys.time()
  srv <- httpuv::startServer("127.0.0.1", port, list(call = function(req) serve_request(req, token)))
  on.exit({
    httpuv::stopServer(srv)
    for (f in file.path(sdir, c("current.json", paste0(.nm$tree_id, ".json"))))   # another worker may have written them since
      if (identical(tryCatch(jsonlite::fromJSON(f)$token, error = function(e) NULL), token)) unlink(f)
  }, add = TRUE)
  message("persistent_minaj: serving ", .nm$tree, " on 127.0.0.1:", port, " (pid ", Sys.getpid(), ")")
  while (isTRUE(.nm$serve)) httpuv::service(200)
  invisible(TRUE)
}

serve_request <- function(req, token) {
  reply <- function(status, x) list(status = status, headers = list("Content-Type" = "application/json"),
                                    body = as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, dataframe = "rows")))
  if (!identical(req$HTTP_X_MINAJ_TOKEN, token)) return(reply(403L, list(ok = FALSE, summary = "bad token")))
  cmd <- sub("^/", "", req$PATH_INFO)
  body <- if (identical(req$REQUEST_METHOD, "POST")) rawToChar(req$rook.input$read()) else ""
  args <- if (nzchar(body)) tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE), error = function(e) NULL) else list()
  if (is.null(args)) return(reply(400L, list(ok = FALSE, summary = "body is not valid JSON")))
  t0 <- proc.time()[["elapsed"]]; msgs <- character(0); warns <- character(0)
  out <- tryCatch(withCallingHandlers(dispatch(cmd, args),
                                      message = function(m) { msgs <<- c(msgs, trimws(conditionMessage(m))); invokeRestart("muffleMessage") },
                                      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
                  error = function(e) list(ok = FALSE, summary = paste("error:", conditionMessage(e))))
  if (is.null(out$ok)) out$ok <- TRUE
  out$secs <- round(proc.time()[["elapsed"]] - t0, 2)
  if (length(msgs)) out$messages <- utils::tail(msgs, 12)
  if (length(warns)) out$warnings <- utils::tail(unique(warns), 8)
  out <- c(list(ok = out$ok, summary = out$summary, secs = out$secs), out[setdiff(names(out), c("ok", "summary", "secs"))])
  reply(if (isTRUE(out$ok)) 200L else 500L, out)
}

dispatch <- function(cmd, a) {
  switch(cmd,
    status = svc_status(),
    make = svc_make(a[["targets"]], isTRUE(a[["confirm"]])),
    plan = svc_plan(a[["targets"]]),
    get = svc_get(a[["name"]], if (is.null(a[["n"]])) 5L else as.integer(a[["n"]]), a[["filter"]]),
    diff = svc_diff(a[["name"]], if (is.null(a[["against"]])) "previous" else a[["against"]], if (is.null(a[["top"]])) 10L else as.integer(a[["top"]])),
    trace = svc_trace(a[["name"]], if (is.null(a[["direction"]])) "upstream" else a[["direction"]], if (is.null(a[["depth"]])) 6L else as.integer(a[["depth"]]),
                      a[["value"]], a[["filter"]]),
    downstream = {
      if (!length(a[["from"]])) stop("give from: CSV input names, data names or chunk names")
      from <- vapply(a[["from"]], function(x) if (target_kind(x) == "chunk") chunk_name(x) else x, "", USE.NAMES = FALSE)
      d <- downstream_minaj(from)
      list(summary = sprintf("%d chunks, %d XML files", length(d$chunks), length(d$xml)), chunks = d$chunks, xml = d$xml)
    },
    variants = svc_variants(a[["xml"]], a[["variants"]]),
    addon = {
      xml <- a[["xml"]]
      if (!is.character(xml) || length(xml) != 1L) stop("give xml: one XML file name, e.g. electricity_water.xml")
      o <- a[["override"]]
      if (length(o)) {
        if (!is.list(o) || is.null(names(o))) stop("override must be an object: CSV input or data name -> override")
        bad <- setdiff(names(o), c(csv_inputs(), names(.nm$producer)))
        if (length(bad)) stop("'", bad[1], "' is not a CSV input or data object of this tree")
        ov <- mapply(parse_override, names(o), o, SIMPLIFY = FALSE)
      } else ov <- .nm$sandbox
      if (!length(ov)) stop("give override (CSV input or data name -> override), or set the sandbox with try")
      restore <- enter()                                                     # the estimate of addon_minaj; it is then called from the worker's folder, where relative paths start
      p <- tryCatch({
        if (target_kind(xml) != "xml") stop(xml, " is not an XML file of this tree")
        pb <- plan_targets(xml, list()); po <- plan_targets(xml, ov)
        ch <- unique(c(pb$chunks, po$chunks))
        list(chunks = ch, secs = sum(vapply(c(ch, intersect(intersect(pb$chunks, po$chunks), downstream_chunks(names(ov)))), est_secs, numeric(1))))
      }, finally = leave(restore))
      if (length(p$chunks) && is.finite(p$secs) && p$secs > nm_opt("server_ask_secs", 300) && !isTRUE(a[["confirm"]]))
        return(list(ok = FALSE, needs_confirm = TRUE, summary = sprintf("would run %d chunks (about %s); repeat with confirm=true", length(p$chunks), fmt_secs(p$secs)),
                    chunks = p$chunks))
      lines <- character(0)
      r <- withCallingHandlers(addon_minaj(xml, ov, config = a[["config"]], dest = a[["dest"]], ask = FALSE),
                               message = function(m) lines <<- c(lines, conditionMessage(m)))
      fin <- utils::tail(grep("Add this line directly after the base file", lines, fixed = TRUE, value = TRUE), 1)
      vl <- regmatches(fin, regexpr("<Value name=[^\n]*</Value>", fin))
      out <- list(summary = sprintf("%s %s (%s add-on)%s", basename(r$file), r$status,
                                    if (length(fin) && grepl("configuration-specific add-on", fin, fixed = TRUE)) "configuration-specific" else "universal",
                                    if (length(vl)) paste0("; add directly after the base file: ", vl) else ""),
                  results = r)
      if (!length(o)) out$sandbox <- names(ov)
      out
    },
    try = svc_try(a[["set"]], isTRUE(a[["clear"]])),
    stop = { .nm$serve <- FALSE; list(summary = "persistent_minaj stopping") },
    stop("unknown command '", cmd, "'; commands: status make plan get diff trace downstream variants addon try stop"))
}

# ---- sandbox overrides ---------------------------------------------------------------------------------

# An override sent as JSON: a number (scale the year columns), a CSV path, or a list of row operations
# [{op, value, column?, <filter columns>}] as in variant tables.
parse_override <- function(name, spec) {
  if (is.numeric(spec) && length(spec) == 1) return(spec)
  if (is.character(spec) && length(spec) == 1) {
    if (!file.exists(spec) && !is.na(suppressWarnings(as.numeric(spec)))) return(as.numeric(spec))   # a number sent as text
    return(user_path(spec))
  }
  if (is.list(spec) && !is.data.frame(spec) && !is.null(names(spec))) spec <- list(spec)          # one row operation sent as an object
  if (is.data.frame(spec) || is.list(spec)) {
    cell <- function(v) if (is.null(v) || all(is.na(v))) NA_character_ else paste(as.character(unlist(v)), collapse = ";")
    rows <- if (is.data.frame(spec)) spec else do.call(rbind, lapply(spec, function(r) as.data.frame(lapply(r, cell), stringsAsFactors = FALSE, check.names = FALSE)))
    rows[] <- lapply(rows, function(x) if (is.list(x)) vapply(x, cell, "") else as.character(x))
    rows$variant <- "sandbox"; rows$input <- name
    return(function(d) apply_rows(d, rows, name))
  }
  stop("override for ", name, " must be a number, a CSV path or a list of row operations")
}

svc_try <- function(set = NULL, clear = FALSE) {
  sb <- if (clear) list() else .nm$sandbox
  for (n in names(set)) {
    if (!n %in% c(csv_inputs(), names(.nm$producer))) stop("'", n, "' is not a CSV input or data object of this tree; the sandbox is unchanged")
    sb[[n]] <- parse_override(n, set[[n]])
  }
  .nm$sandbox <- sb
  list(summary = if (length(.nm$sandbox)) sprintf("sandbox active: %s (make writes <stem>_override.xml; files in the tree are not edited)", paste(names(.nm$sandbox), collapse = ", ")) else "sandbox empty",
       sandbox = names(.nm$sandbox))
}

remember <- function(ctx) {
  for (n in names(ctx$h)) {
    h <- ctx$h[[n]]; if (is.null(h) || is.na(h)) next
    if (!identical(.nm$last_hash[[n]], h)) { if (!is.null(.nm$last_hash[[n]])) .nm$prev_hash[[n]] <- .nm$last_hash[[n]]; .nm$last_hash[[n]] <- h }
  }
}

# ---- commands ------------------------------------------------------------------------------------------

svc_status <- function() {
  i <- info_minaj()
  list(summary = sprintf("%s: %d chunks, %d XML, up %.0f min, sandbox %s", i$tree, i$chunks, i$xml, as.numeric(difftime(Sys.time(), .nm$started, units = "mins")),
                         if (length(.nm$sandbox)) paste(names(.nm$sandbox), collapse = ",") else "empty"),
       tree = i$tree, store = i$store, dest = i$dest, pid = Sys.getpid(), sandbox = names(.nm$sandbox))
}

svc_plan <- function(targets) {
  if (!length(targets)) stop("give targets: XML files, data names, CSV names or chunk names")
  restore <- enter(); on.exit(leave(restore))
  p <- plan_targets(targets, .nm$sandbox)
  list(summary = if (!length(p$chunks) && !length(p$reads)) "nothing to run" else sprintf("%d chunks to run (about %s), %d CSVs to read", length(p$chunks), fmt_secs(p$secs), length(p$reads)),
       chunks = lapply(p$chunks, function(ch) list(chunk = ch, why = p$why[[ch]], est_secs = round(est_secs(ch), 1))), reads = p$reads, est_secs = round(p$secs, 1))
}

svc_make <- function(targets, confirm = FALSE) {
  if (!length(targets)) stop("give targets: XML files, data names, CSV names or chunk names")
  restore <- enter(); on.exit(leave(restore))
  ov <- .nm$sandbox
  p <- plan_targets(targets, ov)
  if (length(p$chunks) && p$secs > nm_opt("server_ask_secs", 300) && !confirm)
    return(list(ok = FALSE, needs_confirm = TRUE, summary = sprintf("would run %d chunks (about %s); repeat with confirm=true", length(p$chunks), fmt_secs(p$secs)),
                chunks = p$chunks))
  if (length(p$chunks)) {
    free <- ps::ps_system_memory()$avail / 2^30; floor <- nm_opt("ram_floor_gb", 4)
    if (free < floor) return(list(ok = FALSE, low_memory = TRUE, chunks = p$chunks,
                                  summary = sprintf("not run: %d chunks to run, but only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb); try again later",
                                                    length(p$chunks), free, floor)))
  }
  ctx <- if (!length(p$chunks) && !length(p$reads)) { p$ctx$dry <- FALSE; p$ctx } else new_ctx(ov)
  rows <- do.call(rbind, lapply(targets, function(x) make_one(x, ctx, dest_dir(), override = length(ov) > 0)))
  remember(ctx)
  ran <- unique(ctx$ran)
  sib <- if (length(ran) && !length(ov)) { xs <- setdiff(downstream_minaj(unique(c(ran, ctx$read)))$xml, targets); dc <- new_ctx(dry = TRUE); xs[vapply(xs, function(x) is.na(value_hash(x, dc)), logical(1))] } else character(0)
  list(summary = sprintf("%s | ran %d, reused %d, read %d CSVs%s", paste(sprintf("%s %s", basename(ifelse(is.na(rows$file), rows$target, rows$file)), rows$status), collapse = "; "),
                         length(ran), length(unique(ctx$reused)), length(unique(ctx$read)), if (length(sib)) sprintf("; %d other XML now out of date", length(sib)) else ""),
       results = rows, ran = ran, sandbox = names(ov), also_out_of_date = sib)
}

svc_get <- function(name, n = 5L, filter = NULL) {
  restore <- enter(); on.exit(leave(restore))
  p <- plan_targets(name, .nm$sandbox)
  if (length(p$chunks)) {
    free <- ps::ps_system_memory()$avail / 2^30; floor <- nm_opt("ram_floor_gb", 4)
    if (free < floor) return(list(ok = FALSE, low_memory = TRUE, chunks = p$chunks,
                                  summary = sprintf("not run: %s needs %d chunks to run, but only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb); try again later",
                                                    name, length(p$chunks), free, floor)))
  }
  d <- obj_get(value_hash(name, new_ctx(.nm$sandbox)))[[1]]
  if (!is.data.frame(d)) return(list(summary = sprintf("%s is not a table (class %s)", name, paste(class(d), collapse = "/"))))
  if (length(filter) && is.null(names(filter))) stop("filter must be an object: column -> value or array of values")
  bad <- setdiff(names(filter), names(d))
  if (length(bad)) stop(name, ": filter column ", paste(bad, collapse = ", "), " is not in the table (columns: ", paste(names(d), collapse = ", "), ")")
  for (f in names(filter)) d <- d[as.character(d[[f]]) %in% as.character(filter[[f]]), , drop = FALSE]
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  list(summary = sprintf("%s: %d rows x %d columns%s", name, nrow(d), ncol(d), if (length(filter)) " (filtered)" else ""),
       columns = vapply(d, function(x) class(x)[1], character(1)),
       ranges = lapply(stats::setNames(num, num), function(v) signif(range(d[[v]], na.rm = TRUE), 6)),
       head = utils::head(as.data.frame(d), n))
}

svc_diff <- function(name, against = "previous", top = 10L) {
  if (!against %in% c("previous", "base")) stop("against must be 'previous' or 'base'")
  restore <- enter(); on.exit(leave(restore))
  if (identical(against, "base")) {
    if (!length(.nm$sandbox)) stop("against = 'base' compares the sandbox with the tree; the sandbox is empty")
    old_h <- value_hash(name, new_ctx()); new_h <- value_hash(name, new_ctx(.nm$sandbox))
  } else {
    new_h <- .nm$last_hash[[name]]; old_h <- .nm$prev_hash[[name]]
    if (is.null(old_h) || is.null(new_h)) stop("no earlier version of ", name, " in this session; run make (before and after the edit) first, or use against='base' with the sandbox")
  }
  d <- value_diff(obj_get(old_h), obj_get(new_h), top)
  d$summary <- paste0(name, ": ", d$summary); d$against <- against
  d
}

svc_trace <- function(name, direction = "upstream", depth = 6L, value = NULL, filter = NULL) {
  restore <- enter(); on.exit(leave(restore))
  if (!name %in% c(csv_inputs(), names(.nm$producer))) stop("'", name, "' is not a CSV input or data object of this tree")
  if (!direction %in% c("upstream", "downstream")) stop("direction must be 'upstream' or 'downstream'")
  tr <- trace_lineage(name, direction, depth)
  out <- list(summary = sprintf("%s %s: %d links, %d CSV files, %d XML files%s", name, direction, length(tr$edges), length(tr$csv), length(tr$xml),
                                if (tr$truncated) sprintf(" (stopped at depth %d)", depth) else ""),
              edges = tr$edges, csv = tr$csv, xml = tr$xml)
  if (name %in% .nm$co$output[.nm$co$to_xml]) out$xml_headers <- xml_headers(name)
  if (!is.null(value)) {
    tabs <- unique(c(name, unlist(lapply(tr$edges, `[[`, "from"))))
    out$value_hits <- value_hits(tabs, as.numeric(value), if (is.null(filter)) list() else filter)
    out$summary <- paste0(out$summary, sprintf("; value %s found in %d rows (%d in CSV files)", value, length(out$value_hits), sum(vapply(out$value_hits, function(h) isTRUE(h$csv), logical(1)))))
  }
  out
}

svc_variants <- function(xml, variants) {
  free <- ps::ps_system_memory()$avail / 2^30; floor <- nm_opt("ram_floor_gb", 4)
  if (free < floor) return(list(ok = FALSE, low_memory = TRUE,
                                summary = sprintf("not run: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb); try again later", free, floor)))
  vs <- lapply(variants, function(v) { if (is.list(v) && !is.data.frame(v)) mapply(parse_override, names(v), v, SIMPLIFY = FALSE) else stop("each variant must map input names to overrides") })
  r <- variants_minaj(xml, vs, ask = FALSE)
  list(summary = paste(sprintf("%s %s%s", basename(r$file), r$status, ifelse(r$identical_to_base, " (= base)", "")), collapse = "; "), results = r)
}
