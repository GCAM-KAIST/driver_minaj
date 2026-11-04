# Commands named and shaped like the ones gcamdata users already know from drake.

#' Build gcamdata outputs, like driver_drake()
#'
#' With no arguments, builds every XML file of the tree (or, with config, the gcamdata XML files a GCAM
#' configuration loads). stop_before, stop_after and the return_* arguments work as in driver_drake():
#' only the chunks upstream of stop_after (or of stop_before's inputs) run, and the requested data are
#' returned as a named list of tables. Results still current from earlier runs are reused, so after the
#' first build only what changed runs. Unlike driver_drake(), a failing XML does not stop the others.
#' write_outputs and return_data_map_only work as in driver(), with the tree's own save_chunkdata() and
#' tibbelize_outputs(), on the tables of the chunks the call covers (all, those upstream of stop_after or of
#' stop_before's inputs, or those the configuration's XML files need).
#'
#' @param stop_before,stop_after Chunk names (full, or a unique suffix such as "energy_L223.electricity").
#' @param return_inputs_of,return_outputs_of,return_data_names As in driver_drake().
#' @param return_data_map_only Return only the data map (as driver(): name, output, precursors, title, units,
#'   comments and flags, one row per CSV input and per chunk output, ordered by name and output).
#' @param write_outputs Write each table to `<outdir>/<name>.csv` as driver() does. A file whose content is
#'   current is not written again; the folder is not cleared.
#' @param write_xml Write XML files (default TRUE, FALSE with return_data_map_only).
#' @param xmldir Folder for XML files (default: the folder set in open_minaj, xml_minaj/ in the gcamdata folder).
#'   A relative path is taken from the working directory.
#' @param outdir Folder for the CSV files (default: the tree's OUTPUTS_DIR, outputs/ in the gcamdata folder). A
#'   relative path is taken from the working directory.
#' @param config Optional GCAM configuration: build only the gcamdata XML files it loads.
#' @param quiet Fewer messages.
#' @param user_modifications,xml_suffix Not supported: use variants_minaj() for modified versions of XML files.
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @param ... Other driver_drake() arguments (such as return_plan_only, or arguments for drake::make); ignored,
#'   with a warning.
#' @return Invisibly, a named list of the requested data (empty when nothing was requested), the data map with
#'   return_data_map_only, or invisibly NULL when the run is declined.
#' @export
driver_minaj <- function(stop_before = NULL, stop_after = NULL,
                         return_inputs_of = stop_before, return_outputs_of = stop_after, return_data_names = NULL,
                         return_data_map_only = FALSE, write_outputs = FALSE, write_xml = !return_data_map_only,
                         xmldir = NULL, outdir = NULL, config = NULL, quiet = FALSE,
                         user_modifications = NULL, xml_suffix = NULL, ask = interactive(), ...) {
  if (!is.null(user_modifications) || !is.null(xml_suffix)) stop("driver_minaj has no user_modifications / xml_suffix; use variants_minaj() for modified versions of an XML file")
  dots <- list(...)
  if (length(dots)) warning("minaj: arguments not supported, ignored: ", paste(names(dots), collapse = ", "), call. = FALSE)
  if (!is.null(config)) config <- user_path(config)
  if (!is.null(xmldir) && !grepl("^([A-Za-z]:)?[/\\\\]", xmldir)) xmldir <- file.path(getwd(), xmldir)
  if (!is.null(outdir) && !grepl("^([A-Za-z]:)?[/\\\\]", outdir)) outdir <- file.path(getwd(), outdir)
  restore <- enter(); on.exit(leave(restore))
  say <- function(...) if (!quiet) message(...)
  dest <- if (is.null(xmldir)) dest_dir() else dest_dir(xmldir)
  sa <- if (length(stop_after)) vapply(stop_after, chunk_name, character(1)) else character(0)
  sb <- if (length(stop_before)) vapply(stop_before, chunk_name, character(1)) else character(0)
  ctx <- new_ctx(); t0 <- proc.time()[["elapsed"]]

  if (!length(sa) && !length(sb)) {                   # the whole data system (or what a configuration loads)
    xs <- .nm$co$output[.nm$co$to_xml]
    if (!is.null(config)) xs <- intersect(basename(xml_paths_in_config(config)), xs)
    scope <- if (is.null(config)) unique(.nm$co$name) else upstream_chunks(unique(unname(.nm$producer[xs])))
    tabs <- if (write_outputs || return_data_map_only) .nm$co$output[.nm$co$name %in% scope & !.nm$co$to_xml] else character(0)
    if (!write_xml) {
      if (length(tabs) && is.null(ask_plan(c(xs, tabs), list(), ask))) return(invisible(NULL))
      for (x in xs) value_hash(x, ctx)
    } else {
      p <- ask_plan(c(xs, tabs), list(), ask); if (is.null(p)) return(invisible(NULL))
      say(sprintf("minaj: building %d XML files (%d chunks to run, about %s)", length(xs), length(p$chunks), fmt_secs(p$secs)))
      if (xml_pool(start = TRUE)) on.exit(xml_pool(stop = TRUE), add = TRUE, after = FALSE)
      rows <- lapply(seq_along(xs), function(i) {
        xml_pool()
        r <- tryCatch(make_one(xs[i], ctx, dest), error = function(e) data.frame(target = xs[i], kind = "xml", status = paste("FAILED:", conditionMessage(e)), file = NA))
        if (i %% 25 == 0) say(sprintf("minaj: %d/%d done, %s", i, length(xs), fmt_secs(proc.time()[["elapsed"]] - t0)))
        r
      })
      rows <- do.call(rbind, rows)
      if (!is.null(.nm$pool)) {                       # files still being written in the background
        q <- rows$status == "queued"
        if (any(q)) say(sprintf("minaj: waiting for %d XML files being written", sum(q)))
        xml_pool(wait = TRUE)
        st <- .nm$pool$status
        rows$status[q] <- vapply(rows$target[q], function(t) if (is.null(st[[t]])) "FAILED: not written" else st[[t]], "")
      }
      bad <- grepl("^FAILED", rows$status)
      say(sprintf("minaj: built %d XML files in %s to %s | converted %d, copied %d, unchanged %d, failed %d | chunks run %d",
                  nrow(rows), fmt_secs(proc.time()[["elapsed"]] - t0), dest, sum(rows$status == "converted"), sum(rows$status == "copied from store"),
                  sum(rows$status == "unchanged"), sum(bad), length(unique(ctx$ran))))
      if (any(bad)) message("minaj: failed: ", paste(rows$target[bad], collapse = ", "))
    }
  } else {                                            # stop_after: the chunk and everything upstream; stop_before: everything upstream of it
    pre <- unique(unlist(.nm$by_chunk[sb], use.names = FALSE))
    targets <- c(sa, pre)
    p <- ask_plan(targets, list(), ask); if (is.null(p)) return(invisible(NULL))
    for (ch in sa) chunk_result(ch, ctx)
    for (n in pre) value_hash(n, ctx)
    closure <- unique(c(upstream_chunks(sa), if (length(pre)) upstream_chunks(unique(unname(unlist(lapply(pre, chunk_of))))) )); scope <- closure
    if (write_xml) {
      xs <- .nm$co$output[.nm$co$to_xml & .nm$co$name %in% closure]
      for (x in xs) tryCatch(make_one(x, ctx, dest), error = function(e) message("minaj: ", x, " failed: ", conditionMessage(e)))
    }
    say(sprintf("minaj: done in %s | chunks run %d, reused %d", fmt_secs(proc.time()[["elapsed"]] - t0), length(unique(ctx$ran)), length(unique(ctx$reused))))
  }
  if (write_outputs) {
    od <- if (is.null(outdir)) file.path(.nm$root, sub("/$", "", get0("OUTPUTS_DIR", envir = .nm$ns, inherits = FALSE) %||% "outputs")) else outdir
    write_tables(scope, ctx, od, others = !length(sa) && !length(sb) && is.null(config), say = say)
  }
  if (return_data_map_only) return(invisible(data_map(scope, ctx, if (length(sb)) pre)))

  want <- unique(c(return_data_names, unlist(.nm$by_chunk[vapply(return_inputs_of %||% character(0), chunk_name, character(1))], use.names = FALSE),
                   unlist(.nm$out_by_chunk[vapply(return_outputs_of %||% character(0), chunk_name, character(1))], use.names = FALSE)))
  if (!length(want)) return(invisible(list()))
  invisible(setNames(lapply(want, function(n) obj_get(value_hash(n, ctx), ctx)[[1]]), want))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# driver_minaj(write_outputs = TRUE): the tables of these chunks, each written with the tree's own save_chunkdata() into a
# hidden folder of this call in dir, then moved into place. A file is recorded under (table hash + code key of
# save_chunkdata); a file with the recorded md5 is not written again. A table save_chunkdata does not write (input data,
# FLAG_NO_OUTPUT) has its file in dir removed, as save_chunkdata does. others: list the CSV files in dir that are not
# tables of the tree.
write_tables <- function(chunks, ctx, dir, others = FALSE, say = message) {
  sc <- get0("save_chunkdata", envir = .nm$ns, inherits = FALSE)
  if (!is.function(sc)) stop("this tree's gcamdata has no save_chunkdata(), which write_outputs uses", call. = FALSE)
  tabs <- .nm$co$output[.nm$co$name %in% chunks & !.nm$co$to_xml]
  tmp <- tempfile(".minaj_part", tmpdir = dir); dir.create(tmp, recursive = TRUE, showWarnings = FALSE); on.exit(unlink(tmp, recursive = TRUE))
  wk <- code_key("save_chunkdata"); t0 <- proc.time()[["elapsed"]]
  st <- vapply(tabs, function(n) {
    h <- value_hash(n, ctx); key <- h64(list("table", h, wk)); rec <- rec_get("table", key); f <- file.path(dir, paste0(n, ".csv"))
    if (!is.null(rec) && is.na(rec$md5)) { unlink(f); return("not written") }
    if (!is.null(rec) && file.exists(f) && identical(file_md5(f), rec$md5)) return("unchanged")
    t <- file.path(tmp, paste0(n, ".csv")); unlink(t)
    sc(obj_get(h, ctx), write_outputs = TRUE, write_xml = FALSE, outputs_dir = tmp)
    if (!file.exists(t)) { unlink(f); rec_put("table", key, list(name = n, md5 = NA_character_, date = Sys.time())); return("not written") }
    md5 <- unname(tools::md5sum(t)); unlink(f); move_into_place(t, f)
    rec_put("table", key, list(name = n, md5 = md5, size = file.size(f), date = Sys.time()))
    "written"
  }, "")
  say(sprintf("minaj: CSV files of %d tables in %s to %s | written %d, unchanged %d, not written by save_chunkdata (input data or FLAG_NO_OUTPUT) %d",
              length(st), fmt_secs(proc.time()[["elapsed"]] - t0), normalizePath(dir, "/"), sum(st == "written"), sum(st == "unchanged"), sum(st == "not written")))
  if (others) {
    x <- setdiff(list.files(dir, "[.]csv$"), paste0(tabs, ".csv"))
    if (length(x)) message(sprintf("minaj: %d other CSV files in %s are not tables of this tree: %s%s", length(x), normalizePath(dir, "/"),
                                   paste(head(x, 8), collapse = ", "), if (length(x) > 8) ", ..." else ""))
  }
  invisible(st)
}

# driver_minaj(return_data_map_only = TRUE): the tree's own tibbelize_outputs() on the CSV inputs (name "INPUT") of these
# chunks and the CSV files among ins (stop_before: that chunk's inputs), and on the outputs of each of these chunks, ordered
# by name and output. With no rows, an empty tibble, as driver() returns.
data_map <- function(chunks, ctx, ins = NULL) {
  tb <- get0("tibbelize_outputs", envir = .nm$ns, inherits = FALSE)
  if (!is.function(tb)) stop("this tree's gcamdata has no tibbelize_outputs(), which return_data_map_only uses", call. = FALSE)
  ins <- unique(c(unlist(.nm$by_chunk[chunks], use.names = FALSE), ins)); ins <- ins[vapply(ins, is_csv, NA)]
  get1 <- function(n) obj_get(value_hash(n, ctx), ctx)
  rows <- c(lapply(ins, function(n) tb(get1(n), "INPUT")),
            lapply(chunks, function(ch) tb(do.call(c, lapply(.nm$out_by_chunk[[ch]], get1)), ch)))
  if (!length(rows)) return(tibble::tibble())
  m <- tibble::as_tibble(do.call(rbind, rows))
  m[order(m$name, m$output, method = "radix"), ]
}

# drake target name ("energy.A23.globaltech_capacity_factor") or gcamdata name -> gcamdata name.
data_name <- function(x) {
  all <- c(.nm$co$output, csv_inputs())
  if (x %in% all) return(x)
  hit <- all[make.names(all) == x]
  if (length(hit) == 1) return(hit)
  stop("'", x, "' is not a data object, CSV input or XML file of this tree", call. = FALSE)
}

#' Read one stored object, like drake::readd()
#'
#' Returns the same value drake::readd() would (a one-element named list), computed or taken from the store
#' so that it is current. Accepts gcamdata names ("L223.StubTechCapFactor_elec"), drake target names, or bare
#' symbols. get_minaj() returns the table itself.
#' @param target gcamdata name, drake target name, or a bare symbol.
#' @param character_only If TRUE, `target` is evaluated and must give the name as a character string.
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return A one-element named list holding the value, or invisibly NULL when the run is declined.
#' @export
readd <- function(target, character_only = FALSE, ask = interactive()) {
  tg <- if (character_only) target else { s <- substitute(target); if (is.character(s)) s else deparse(s) }
  restore <- enter(); on.exit(leave(restore))
  nm <- data_name(tg)
  if (is.null(ask_plan(nm, list(), ask))) return(invisible(NULL))
  obj_get(value_hash(nm, new_ctx()))
}

#' Load stored objects into an environment, like drake::loadd()
#'
#' Each object is assigned under its drake target name (make.names of the gcamdata name), holding the same
#' value drake::loadd() would.
#' @param ... gcamdata names, drake target names or bare symbols.
#' @param list Character vector of further names.
#' @param envir Environment to assign into (default: the global environment).
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return Invisibly, the names assigned, or invisibly NULL when the run is declined.
#' @export
loadd <- function(..., list = character(0), envir = globalenv(), ask = interactive()) {
  tg <- c(vapply(as.list(substitute(list(...)))[-1], function(s) if (is.character(s)) s else deparse(s), character(1)), list)
  restore <- enter(); on.exit(leave(restore))
  nms <- vapply(tg, data_name, character(1))
  if (is.null(ask_plan(nms, list(), ask))) return(invisible(NULL))
  ctx <- new_ctx()
  for (n in nms) assign(make.names(n), obj_get(value_hash(n, ctx), ctx), envir = envir)
  invisible(make.names(nms))
}

#' Chunks that would run, like drake::outdated()
#'
#' @param config Optional GCAM configuration: consider only the gcamdata XML files it loads.
#' @return Character vector of chunk names that would run to bring every XML file (or those config loads) up to date.
#' @export
outdated <- function(config = NULL) {
  if (!is.null(config)) config <- user_path(config)
  restore <- enter(); on.exit(leave(restore))
  xs <- .nm$co$output[.nm$co$to_xml]
  if (!is.null(config)) xs <- intersect(basename(xml_paths_in_config(config)), xs)
  plan_targets(xs)$chunks
}
