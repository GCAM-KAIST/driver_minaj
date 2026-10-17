# Commands named and shaped like the ones gcamdata users already know from drake.

#' Build gcamdata outputs, like driver_drake()
#'
#' With no arguments, builds every XML file of the tree (or, with config, the gcamdata XML files a GCAM
#' configuration loads). stop_before, stop_after and the return_* arguments work as in driver_drake():
#' only the chunks upstream of stop_after (or of stop_before's inputs) run, and the requested data are
#' returned as a named list of tables. Results still current from earlier runs are reused, so after the
#' first build only what changed runs. Unlike driver_drake(), a failing XML does not stop the others.
#'
#' @param stop_before,stop_after Chunk names (full, or a unique suffix such as "energy_L223.electricity").
#' @param return_inputs_of,return_outputs_of,return_data_names As in driver_drake().
#' @param write_xml Write XML files (default TRUE).
#' @param xmldir Folder for XML files (default: the folder set in open_minaj, xml_minaj/ in the gcamdata folder).
#'   A relative path is taken from the working directory.
#' @param config Optional GCAM configuration: build only the gcamdata XML files it loads.
#' @param quiet Fewer messages.
#' @param user_modifications,xml_suffix Not supported: use variants_minaj() for modified versions of XML files.
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @param ... Other driver_drake() arguments (such as return_data_map_only, return_plan_only, or arguments
#'   for drake::make); ignored, with a warning.
#' @return Invisibly, a named list of the requested data (empty when nothing was requested), or invisibly
#'   NULL when the run is declined.
#' @export
driver_minaj <- function(stop_before = NULL, stop_after = NULL,
                         return_inputs_of = stop_before, return_outputs_of = stop_after, return_data_names = NULL,
                         write_xml = TRUE, xmldir = NULL, config = NULL, quiet = FALSE,
                         user_modifications = NULL, xml_suffix = NULL, ask = interactive(), ...) {
  if (!is.null(user_modifications) || !is.null(xml_suffix)) stop("driver_minaj has no user_modifications / xml_suffix; use variants_minaj() for modified versions of an XML file")
  dots <- list(...)
  if (length(dots)) warning("minaj: arguments not supported, ignored: ", paste(names(dots), collapse = ", "), call. = FALSE)
  if (!is.null(config)) config <- user_path(config)
  if (!is.null(xmldir) && !grepl("^([A-Za-z]:)?[/\\\\]", xmldir)) xmldir <- file.path(getwd(), xmldir)
  restore <- enter(); on.exit(leave(restore))
  say <- function(...) if (!quiet) message(...)
  dest <- if (is.null(xmldir)) dest_dir() else dest_dir(xmldir)
  sa <- if (length(stop_after)) vapply(stop_after, chunk_name, character(1)) else character(0)
  sb <- if (length(stop_before)) vapply(stop_before, chunk_name, character(1)) else character(0)
  ctx <- new_ctx(); t0 <- proc.time()[["elapsed"]]

  if (!length(sa) && !length(sb)) {                   # the whole data system (or what a configuration loads)
    xs <- .nm$co$output[.nm$co$to_xml]
    if (!is.null(config)) xs <- intersect(basename(xml_paths_in_config(config)), xs)
    if (!write_xml) { for (x in xs) value_hash(x, ctx) }
    else {
      p <- ask_plan(xs, list(), ask); if (is.null(p)) return(invisible(NULL))
      say(sprintf("minaj: building %d XML files (%d chunks to run, about %s)", length(xs), length(p$chunks), fmt_secs(p$secs)))
      rows <- lapply(seq_along(xs), function(i) {
        r <- tryCatch(make_one(xs[i], ctx, dest), error = function(e) data.frame(target = xs[i], kind = "xml", status = paste("FAILED:", conditionMessage(e)), file = NA))
        if (i %% 25 == 0) say(sprintf("minaj: %d/%d done, %s", i, length(xs), fmt_secs(proc.time()[["elapsed"]] - t0)))
        r
      })
      rows <- do.call(rbind, rows); bad <- grepl("^FAILED", rows$status)
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
    closure <- unique(c(upstream_chunks(sa), if (length(pre)) upstream_chunks(unique(unname(unlist(lapply(pre, chunk_of))))) ))
    if (write_xml) {
      xs <- .nm$co$output[.nm$co$to_xml & .nm$co$name %in% closure]
      for (x in xs) tryCatch(make_one(x, ctx, dest), error = function(e) message("minaj: ", x, " failed: ", conditionMessage(e)))
    }
    say(sprintf("minaj: done in %s | chunks run %d, reused %d", fmt_secs(proc.time()[["elapsed"]] - t0), length(unique(ctx$ran)), length(unique(ctx$reused))))
  }

  want <- unique(c(return_data_names, unlist(.nm$by_chunk[vapply(return_inputs_of %||% character(0), chunk_name, character(1))], use.names = FALSE),
                   unlist(.nm$out_by_chunk[vapply(return_outputs_of %||% character(0), chunk_name, character(1))], use.names = FALSE)))
  if (!length(want)) return(invisible(list()))
  invisible(setNames(lapply(want, function(n) obj_get(value_hash(n, ctx), ctx)[[1]]), want))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

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
