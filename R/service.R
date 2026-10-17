# persistent_minaj: one long-running R process that keeps gcamdata loaded and answers short commands over
# HTTP on 127.0.0.1 (token-protected), so a caller (a script or another program) pays no R start-up or
# package load per call. Every answer is JSON with a one-line `summary`. The `pm` script in inst/bin is the client.
#
# Commands (POST /<command>, JSON body):
#   status | make {targets, confirm} | plan {targets} | get {name, n, filter} | diff {name, against, top}
#   trace {name, direction, depth, value, filter} | downstream {from} | variants {xml, variants}
#   try {set, clear} (sandbox overrides used by make/plan/get/diff until cleared) | stop

#' Start the persistent minaj worker for a tree
#'
#' Blocks until `pm stop`, so run it in its own R process. Answers `pm` commands over HTTP on 127.0.0.1
#' and writes the port and a token to <store>/server/current.json and <store>/server/<tree>_<id>.json,
#' where `pm` reads them.
#' @param tree GCAM tree (folder holding input/gcamdata) or the gcamdata folder itself.
#' @param port TCP port on 127.0.0.1 (default: a free one).
#' @param store Store folder (default: option minaj.store, else
#'   file.path(tools::R_user_dir("minaj", "cache"), "minaj_store")).
#' @return Invisibly TRUE, once stopped.
#' @export
persistent_minaj <- function(tree, port = NULL, store = nm_opt("store", file.path(tools::R_user_dir("minaj", "cache"), "minaj_store"))) {
  options(minaj.ask_secs = Inf, minaj.report_siblings = FALSE)
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
  on.exit({ httpuv::stopServer(srv); unlink(file.path(sdir, "current.json")) }, add = TRUE)
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
    make = svc_make(a$targets, isTRUE(a$confirm)),
    plan = svc_plan(a$targets),
    get = svc_get(a$name, if (is.null(a$n)) 5L else as.integer(a$n), a$filter),
    diff = svc_diff(a$name, if (is.null(a$against)) "previous" else a$against, if (is.null(a$top)) 10L else as.integer(a$top)),
    trace = svc_trace(a$name, if (is.null(a$direction)) "upstream" else a$direction, if (is.null(a$depth)) 6L else as.integer(a$depth), a$value, a$filter),
    downstream = { d <- downstream_minaj(a$from); list(summary = sprintf("%d chunks, %d XML files", length(d$chunks), length(d$xml)), chunks = d$chunks, xml = d$xml) },
    variants = svc_variants(a$xml, a$variants),
    try = svc_try(a$set, isTRUE(a$clear)),
    stop = { .nm$serve <- FALSE; list(summary = "persistent_minaj stopping") },
    stop("unknown command '", cmd, "'; commands: status make plan get diff trace downstream variants try stop"))
}

# ---- sandbox overrides ---------------------------------------------------------------------------------

# An override sent as JSON: a number (scale the year columns), a CSV path, or a list of row operations
# [{op, value, column?, <filter columns>}] as in variant tables.
parse_override <- function(name, spec) {
  if (is.numeric(spec) && length(spec) == 1) return(spec)
  if (is.character(spec) && length(spec) == 1) return(user_path(spec))
  if (is.data.frame(spec) || is.list(spec)) {
    rows <- as.data.frame(if (is.data.frame(spec)) spec else do.call(rbind, lapply(spec, as.data.frame)), stringsAsFactors = FALSE)
    rows[] <- lapply(rows, as.character); rows$variant <- "sandbox"; rows$input <- name
    return(function(d) apply_rows(d, rows, name))
  }
  stop("override for ", name, " must be a number, a CSV path or a list of row operations")
}

svc_try <- function(set = NULL, clear = FALSE) {
  if (clear) .nm$sandbox <- list()
  for (n in names(set)) {
    if (!n %in% c(csv_inputs(), names(.nm$producer))) stop("'", n, "' is not a CSV input or data object of this tree")
    .nm$sandbox[[n]] <- parse_override(n, set[[n]])
  }
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
  d <- obj_get(value_hash(name, new_ctx(.nm$sandbox)))[[1]]
  if (!is.data.frame(d)) return(list(summary = sprintf("%s is not a table (class %s)", name, paste(class(d), collapse = "/"))))
  for (f in names(filter)) if (f %in% names(d)) d <- d[as.character(d[[f]]) %in% as.character(filter[[f]]), , drop = FALSE]
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  list(summary = sprintf("%s: %d rows x %d columns%s", name, nrow(d), ncol(d), if (length(filter)) " (filtered)" else ""),
       columns = vapply(d, function(x) class(x)[1], character(1)),
       ranges = lapply(stats::setNames(num, num), function(v) signif(range(d[[v]], na.rm = TRUE), 6)),
       head = utils::head(as.data.frame(d), n))
}

svc_diff <- function(name, against = "previous", top = 10L) {
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
  vs <- lapply(variants, function(v) { if (is.list(v) && !is.data.frame(v)) mapply(parse_override, names(v), v, SIMPLIFY = FALSE) else stop("each variant must map input names to overrides") })
  r <- variants_minaj(xml, vs, ask = FALSE)
  list(summary = paste(sprintf("%s %s%s", basename(r$file), r$status, ifelse(r$identical_to_base, " (= base)", "")), collapse = "; "), results = r)
}
