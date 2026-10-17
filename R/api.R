# The commands.

#' Build XML files, data objects or chunks, re-running only what changed
#'
#' @param ... XML file names ("electricity.xml"), data names ("L223.StubTechCapFactor_elec"), CSV input
#'   names ("energy/A23.globaltech_capacity_factor") or chunk names ("module_energy_L223.electricity").
#' @param override Named list replacing CSV inputs or data objects for this call (see variants_minaj). The
#'   XML is then written as <stem>_override.xml, never under the base name.
#' @param dest Folder for XML files (default: the folder set in open_minaj, xml_minaj/ in the gcamdata folder).
#'   A relative path is taken from the working directory.
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return Invisibly, a data frame with one row per target, or invisibly NULL when the run is declined.
#' @export
make_minaj <- function(..., override = list(), dest = NULL, ask = interactive()) {
  targets <- c(...)
  if (!is.null(dest) && grepl("^([A-Za-z]:)?[/\\\\]", dest) == FALSE) dest <- file.path(getwd(), dest)
  restore <- enter(); on.exit(leave(restore))
  dest <- if (is.null(dest)) dest_dir() else dest_dir(dest)
  p <- ask_plan(targets, override, ask)
  if (is.null(p)) return(invisible(NULL))
  ctx <- if (!length(p$chunks) && !length(p$reads)) { p$ctx$dry <- FALSE; p$ctx } else new_ctx(override)   # nothing to run: reuse the plan's work
  t0 <- proc.time()[["elapsed"]]
  rows <- do.call(rbind, lapply(targets, function(x) make_one(x, ctx, dest, override = length(override) > 0)))
  secs <- proc.time()[["elapsed"]] - t0
  message(sprintf("minaj: %s in %s | chunks run %d, reused %d, CSVs read %d%s", paste(sprintf("%s %s", basename(ifelse(is.na(rows$file), rows$target, rows$file)), rows$status), collapse = "; "),
                  fmt_secs(secs), length(unique(ctx$ran)), length(unique(ctx$reused)), length(unique(ctx$read)),
                  if (length(ctx$ran)) paste0(" [", paste(unique(ctx$ran), collapse = ", "), "]") else ""))
  if (any(rows$kind == "xml")) message("minaj: written to ", dest, if (!identical(dest, xml_dir())) " (configurations usually read xml/; promote_minaj() copies there)" else "")
  if (length(ctx$ran) && !length(override) && nm_opt("report_siblings", TRUE)) report_siblings(unique(c(ctx$ran, ctx$read)), targets)
  invisible(rows)
}

xml_dir <- function() normalizePath(file.path(.nm$root, sub("/$", "", get("XML_DIR", .nm$ns))), "/", mustWork = FALSE)

make_one <- function(x, ctx, dest, file = NULL, override = FALSE) {
  kind <- target_kind(x)
  if (kind == "chunk") { chunk_result(chunk_name(x), ctx); return(data.frame(target = x, kind = kind, status = "ok", file = NA)) }
  h <- value_hash(x, ctx)
  if (kind != "xml") return(data.frame(target = x, kind = kind, status = "ok", file = NA))
  if (is.null(file)) file <- file.path(dest, if (override) sub("[.]xml$", "_override.xml", x) else x)
  st <- write_xml(x, h, file)
  if (st != "unchanged") manifest_add(dirname(file), data.frame(time = format(Sys.time()), file = basename(file), xml = x, xml_object = h,
                                                                md5 = file_md5(file), tree_head = tree_head(),
                                                                override = paste(names(ctx$over), collapse = " "), how = st))
  data.frame(target = x, kind = kind, status = st, file = file)
}

# Other XML files that the chunks or CSVs just re-run/read feed, whose stored results are now out of date.
report_siblings <- function(changed, targets) {
  xs <- setdiff(downstream_minaj(changed)$xml, targets)
  if (!length(xs)) return(invisible())
  ctx <- new_ctx(dry = TRUE)
  stale <- xs[vapply(xs, function(x) is.na(value_hash(x, ctx)), logical(1))]
  if (length(stale)) message(sprintf("minaj: also changed by this edit, not written (%d): %s%s", length(stale), paste(head(stale, 12), collapse = ", "),
                                     if (length(stale) > 12) ", ..." else ""), "\n       status_minaj(config) lists which of them your configuration loads")
  invisible(stale)
}

# Dry run: which chunks would run, and roughly how long.
plan_targets <- function(targets, override = list()) {
  ctx <- new_ctx(override, dry = TRUE)
  for (x in targets) {
    k <- target_kind(x)
    if (k == "chunk") chunk_result(chunk_name(x), ctx) else value_hash(x, ctx)
  }
  w <- ctx$would[!grepl("^(read|override) ", ctx$would)]
  ch <- unique(w)
  secs <- sum(vapply(ch, est_secs, numeric(1)))
  list(chunks = ch, reads = sub("^read ", "", grep("^read ", ctx$would, value = TRUE)), secs = secs, why = ctx$why[ch], ctx = ctx)
}

# Plan, and ask first when the estimate is long. Returns NULL if the user declines.
ask_plan <- function(targets, override = list(), ask = interactive()) {
  p <- plan_targets(targets, override)
  if (length(p$chunks) && ask && is.finite(p$secs) && p$secs > nm_opt("ask_secs", 120)) {
    ok <- utils::askYesNo(sprintf("minaj: %d chunk(s) to run in this R session, about %s (%s). Run now?", length(p$chunks), fmt_secs(p$secs),
                                  paste(head(p$chunks, 4), collapse = ", ")))
    if (!isTRUE(ok)) { message("minaj: not run"); return(NULL) }
  }
  p
}

est_secs <- function(ch) {
  if (is.null(.nm$secs)) { p <- file.path(.nm$state_dir, "secs.rds"); .nm$secs <- if (file.exists(p)) readRDS(p) else numeric(0) }
  l <- last_get(ch)
  if (!is.null(l)) return(l$secs)
  s <- .nm$secs[ch]
  if (is.na(s)) 30 else s
}

fmt_secs <- function(s) if (s < 90) sprintf("%.1f s", s) else if (s < 5400) sprintf("%.1f min", s / 60) else sprintf("%.1f h", s / 3600)

#' What would run, and why, without running anything
#'
#' @param ... Targets, as in make_minaj().
#' @param override Named list replacing CSV inputs or data objects for this call, as in make_minaj().
#' @return Invisibly, a list with chunks (chunks that would run), reads (CSV inputs that would be read),
#'   secs (estimated seconds), why (reason per chunk) and ctx (the planning context, for internal use).
#' @export
plan_minaj <- function(..., override = list()) {
  restore <- enter(); on.exit(leave(restore))
  p <- plan_targets(c(...), override)
  if (!length(p$chunks) && !length(p$reads)) { message("minaj: nothing to run; all stored results are current"); return(invisible(p)) }
  message(sprintf("minaj: %d chunk(s) to run (about %s), %d CSV(s) to read", length(p$chunks), fmt_secs(p$secs), length(p$reads)))
  for (ch in p$chunks) message(sprintf("  %-55s %s", ch, p$why[[ch]]))
  invisible(p)
}

#' Why a chunk would run again
#'
#' @param chunk Chunk name (full, or a unique suffix such as "energy_L223.electricity").
#' @return Invisibly, the reason, or NULL when the stored result is current.
#' @export
why_minaj <- function(chunk) {
  restore <- enter(); on.exit(leave(restore))
  ch <- chunk_name(chunk); ctx <- new_ctx(dry = TRUE); chunk_result(ch, ctx)
  w <- ctx$why[[ch]]
  message("minaj: ", ch, ": ", if (is.null(w)) "stored result is current" else w)
  invisible(w)
}

#' One data object (a table), computed or taken from the store
#'
#' @param name Data name ("L223.StubTechCapFactor_elec") or CSV input name ("energy/A23.globaltech_capacity_factor").
#' @param override Named list replacing CSV inputs or data objects for this call, as in make_minaj().
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return The table, or invisibly NULL when the run is declined.
#' @export
get_minaj <- function(name, override = list(), ask = interactive()) {
  restore <- enter(); on.exit(leave(restore))
  if (is.null(ask_plan(name, override, ask))) return(invisible(NULL))
  obj_get(value_hash(name, new_ctx(override)))[[1]]
}

chunk_inputs_data <- function(ch, ctx) {
  inputs <- .nm$by_chunk[[ch]]; if (is.null(inputs)) inputs <- character(0)
  ih <- vapply(inputs, value_hash, character(1), ctx = ctx)
  list(inputs = inputs, all_data = do.call(c, c(list(list()), unname(lapply(ih, obj_get, ctx = ctx)))))
}

#' Run one chunk and return its outputs
#'
#' Reuses the stored result when nothing the chunk depends on changed.
#' @param chunk Chunk name (full, or a unique suffix such as "energy_L223.electricity").
#' @param override Named list replacing CSV inputs or data objects for this call, as in make_minaj().
#' @param force Run the chunk even if its stored result is current; the result is then not stored.
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return Invisibly, a named list of the chunk's outputs, or invisibly NULL when the run is declined.
#' @export
run_minaj <- function(chunk, override = list(), force = FALSE, ask = interactive()) {
  restore <- enter(); on.exit(leave(restore))
  ch <- chunk_name(chunk)
  if (is.null(ask_plan(.nm$by_chunk[[ch]], override, ask))) return(invisible(NULL))
  ctx <- new_ctx(override)
  if (force) { d <- chunk_inputs_data(ch, ctx); return(invisible(exec_chunk(ch, d$all_data, d$inputs)$out)) }
  r <- chunk_result(ch, ctx)
  invisible(lapply(setNames(names(r$out), names(r$out)), function(n) obj_get(r$out[[n]], ctx)[[1]]))
}

#' Put a chunk's inputs in the workspace as `all_data`, to run its code line by line
#'
#' Open the chunk's R file and run the lines inside its MAKE branch (Ctrl+Enter), skipping its first
#' line `all_data <- list(...)[[1]]`; `get_data(all_data, ...)` then returns the real inputs.
#' @param chunk Chunk name (full, or a unique suffix such as "energy_L223.electricity").
#' @param override Named list replacing CSV inputs or data objects for this call, as in make_minaj().
#' @param envir Environment that receives `all_data` (default: the global environment).
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return Invisibly, `all_data`, or invisibly NULL when the run is declined.
#' @export
inputs_minaj <- function(chunk, override = list(), envir = globalenv(), ask = interactive()) {
  restore <- enter(); on.exit(leave(restore))
  ch <- chunk_name(chunk)
  if (is.null(ask_plan(.nm$by_chunk[[ch]], override, ask))) return(invisible(NULL))
  d <- chunk_inputs_data(ch, new_ctx(override))
  assign("all_data", d$all_data, envir = envir)
  f <- names(Filter(function(x) ch %in% x, .nm$file_defs))
  message(sprintf("minaj: all_data holds the %d inputs of %s. Open %s and run the lines of its MAKE branch, skipping the first (all_data <- list(...)[[1]]).",
                  length(d$all_data), ch, file.path(.nm$root, "R", f[1])))
  invisible(d$all_data)
}

#' Run one chunk in the debugger (browser at its first line)
#'
#' @param chunk Chunk name (full, or a unique suffix such as "energy_L223.electricity").
#' @param override Named list replacing CSV inputs or data objects for this call, as in make_minaj().
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return Invisibly, the chunk's outputs (not stored), or invisibly NULL when the run is declined.
#' @export
debug_minaj <- function(chunk, override = list(), ask = interactive()) {
  restore <- enter(); on.exit(leave(restore))
  ch <- chunk_name(chunk)
  if (is.null(ask_plan(.nm$by_chunk[[ch]], override, ask))) return(invisible(NULL))
  d <- chunk_inputs_data(ch, new_ctx(override))
  debugonce(get(ch, envir = .nm$ns))
  invisible(exec_chunk(ch, d$all_data, d$inputs)$out)
}

#' Are the XML files current?
#'
#' With a GCAM configuration, checks the exact gcamdata XML files that configuration loads (paths resolved
#' from the configuration's folder). Otherwise checks the given files (default: all) in `where`.
#' @param config Path to a GCAM configuration file.
#' @param files XML file names to check when config is NULL (default: all XML files of the tree).
#' @param where Folder holding the files when config is NULL (default: the open_minaj destination). A relative
#'   path is taken from the working directory.
#' @return A data frame, one row per XML file: xml, results ("current" or "stale"), chunks_to_run, file
#'   ("current", "differs", "missing" or "unknown"), est (estimated run time) and path.
#' @export
status_minaj <- function(config = NULL, files = NULL, where = NULL) {
  if (!is.null(config)) config <- user_path(config)
  if (!is.null(where) && !grepl("^([A-Za-z]:)?[/\\\\]", where)) where <- file.path(getwd(), where)
  restore <- enter(); on.exit(leave(restore))
  outs <- .nm$co$output[.nm$co$to_xml]
  if (!is.null(config)) {
    paths <- xml_paths_in_config(config)
    known <- basename(paths) %in% outs
    if (any(!known)) message("minaj: not checked (not gcamdata outputs of this tree): ", paste(basename(paths[!known]), collapse = ", "))
    paths <- paths[known]; files <- basename(paths)
  } else {
    if (is.null(files)) files <- outs
    where <- if (is.null(where)) dest_dir() else dest_dir(where)
    paths <- file.path(where, files)
  }
  ctx <- new_ctx(dry = TRUE)
  rows <- lapply(seq_along(files), function(i) {
    x <- files[i]; f <- paths[i]
    h <- value_hash(x, ctx)
    ch <- intersect(upstream_chunks(chunk_of(x)), ctx$would)
    file_ok <- NA
    if (!is.na(h)) { rec <- rec_get("xml", h64(list("xml", h, writer_key()))); file_ok <- !is.null(rec) && file.exists(f) && identical(file_md5(f), rec$md5) }
    data.frame(xml = x, results = if (length(ch) || is.na(h)) "stale" else "current", chunks_to_run = length(ch),
               file = if (is.na(file_ok)) "unknown" else if (file_ok) "current" else if (file.exists(f)) "differs" else "missing",
               est = fmt_secs(sum(vapply(ch, est_secs, numeric(1)))), path = f)
  })
  rows <- do.call(rbind, rows)
  message(sprintf("minaj: %d XML checked%s: results current %d, stale %d; files current %d, differ %d, missing %d, unknown %d", nrow(rows),
                  if (is.null(config)) paste(" in", where) else paste(" from", basename(config)),
                  sum(rows$results == "current"), sum(rows$results == "stale"), sum(rows$file == "current"), sum(rows$file == "differs"),
                  sum(rows$file == "missing"), sum(rows$file == "unknown")))
  rows
}

#' Copy XML files from the minaj folder into the tree's xml/ folder (where configurations usually point)
#'
#' Refuses a file that is not the current base result (a variant or override, or out of date).
#' @param files XML file names, e.g. "electricity_water.xml".
#' @param from Folder holding the files (default: the open_minaj destination). A relative path is taken from
#'   the working directory.
#' @return Invisibly, a named logical vector: TRUE for each file copied.
#' @export
promote_minaj <- function(files, from = NULL) {
  if (!is.null(from) && !grepl("^([A-Za-z]:)?[/\\\\]", from)) from <- file.path(getwd(), from)
  restore <- enter(); on.exit(leave(restore))
  from <- if (is.null(from)) dest_dir() else dest_dir(from)
  to <- xml_dir(); ctx <- new_ctx(dry = TRUE)
  ok <- vapply(files, function(x) {
    if (target_kind(x) != "xml") { message("minaj: ", x, " is not an XML file of this tree"); return(FALSE) }
    h <- value_hash(x, ctx); rec <- if (is.na(h)) NULL else rec_get("xml", h64(list("xml", h, writer_key())))
    src <- file.path(from, x)
    if (is.null(rec) || !file.exists(src) || !identical(file_md5(src), rec$md5)) {
      message("minaj: not promoted, ", x, " in ", from, " is not the current base result (run make_minaj(\"", x, "\") first)"); return(FALSE)
    }
    file.copy(src, file.path(to, x), overwrite = TRUE) && identical(file_md5(file.path(to, x)), rec$md5)
  }, logical(1))
  message("minaj: copied to ", to, ": ", paste(files[ok], collapse = ", "), if (any(!ok)) paste(" | not copied:", paste(files[!ok], collapse = ", ")) else "")
  invisible(ok)
}
