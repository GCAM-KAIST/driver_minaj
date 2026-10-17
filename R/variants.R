# Several versions of one XML file in one session.

#' Write low / mid / high (or any) versions of one XML file
#'
#' Each variant replaces one or more CSV inputs or data objects; only the chunks between those inputs
#' and the XML run, and each variant's results are stored under their own fingerprints (they never mix
#' with the base). Files are named <stem>_<variant>.xml.
#'
#' @param xml XML file name, e.g. "electricity.xml".
#' @param variants Either a named list, one element per variant, each a named list of overrides
#'   (input name = a number that scales every year column, or the value column; a function(table)
#'   returning a table; a table; or, for a CSV input, the path of a CSV file with the same columns), or a
#'   data frame / CSV path with columns variant, input, op (scale, add or set), value, and optionally
#'   column (default: every year column, else value; several separated by ;) plus filter columns named
#'   like the table's columns (e.g. region, technology; empty or * = all rows; several values separated
#'   by ;). Columns note, family, mode and file are ignored.
#' @param dest Folder for the variant files (default: variants/ in the minaj XML folder). A relative path
#'   is taken from the working directory.
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return Invisibly, a data frame with one row per variant: variant, file, status and identical_to_base, or
#'   invisibly NULL when the run is declined.
#' @export
variants_minaj <- function(xml, variants, dest = NULL, ask = interactive()) {
  if (is.character(variants) && length(variants) == 1) {
    variants <- utils::read.csv(user_path(variants), colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8-BOM", na.strings = character(0))
  }
  if (is.list(variants) && !is.data.frame(variants)) variants <- lapply(variants, function(ov) lapply(ov, function(o) if (is.character(o) && length(o) == 1) user_path(o) else o))
  if (!is.null(dest) && !grepl("^([A-Za-z]:)?[/\\\\]", dest)) dest <- file.path(getwd(), dest)
  restore <- enter(); on.exit(leave(restore))
  if (target_kind(xml) != "xml") stop(xml, " is not an XML file of this tree")
  if (is.data.frame(variants)) variants <- variants_from_table(variants)
  if (is.null(names(variants)) || any(!nzchar(names(variants)))) stop("variants must be named, e.g. list(low = ..., high = ...)")
  dest <- if (is.null(dest)) dest_dir(file.path(dest_dir(), "variants")) else dest_dir(dest)
  stem <- sub("[.]xml$", "", xml)
  clash <- tolower(paste0(stem, "_", names(variants), ".xml")) %in% tolower(.nm$co$output)
  if (any(clash)) stop("variant file name(s) ", paste(paste0(stem, "_", names(variants)[clash], ".xml"), collapse = ", "), " are real outputs of this tree; choose other variant names")
  up_ch <- upstream_chunks(chunk_of(xml)); up_in <- unique(c(unlist(.nm$by_chunk[up_ch], use.names = FALSE), .nm$co$output[.nm$co$name %in% up_ch]))
  for (v in names(variants)) {
    bad <- setdiff(names(variants[[v]]), c(csv_inputs(), names(.nm$producer)))
    if (length(bad)) stop("variant ", v, ": not an input or data object of this tree: ", paste(bad, collapse = ", "))
    if (!length(intersect(names(variants[[v]]), up_in))) stop("variant ", v, ": none of its inputs is upstream of ", xml)
  }
  # ask once, for all variants together
  plans <- lapply(variants, function(ov) plan_targets(xml, ov))
  secs <- sum(vapply(plans, `[[`, numeric(1), "secs"))
  if (ask && is.finite(secs) && secs > nm_opt("ask_secs", 120) &&
      !isTRUE(utils::askYesNo(sprintf("minaj: %d variants need about %s of chunk runs in this R session. Run now?", length(variants), fmt_secs(secs))))) {
    message("minaj: not run"); return(invisible(NULL))
  }
  base_h <- value_hash(xml, new_ctx(dry = TRUE))
  rows <- lapply(names(variants), function(v) {
    ctx <- new_ctx(variants[[v]]); t0 <- proc.time()[["elapsed"]]
    r <- make_one(xml, ctx, dest, file = file.path(dest, paste0(stem, "_", v, ".xml")))
    same <- !is.na(base_h) && identical(ctx$h[[xml]], base_h)
    message(sprintf("minaj: %s_%s.xml %s in %s (chunks run %d)%s", stem, v, r$status, fmt_secs(proc.time()[["elapsed"]] - t0),
                    length(unique(ctx$ran)), if (same) " - identical to the base" else ""))
    data.frame(variant = v, file = r$file, status = r$status, identical_to_base = same)
  })
  message("minaj: variants written to ", dest)
  invisible(do.call(rbind, rows))
}

variants_from_table <- function(tab) {
  need <- c("variant", "input", "op", "value")
  if (!all(need %in% names(tab))) stop("variant table needs columns: ", paste(need, collapse = ", "), " (has: ", paste(names(tab), collapse = ", "), ")")
  out <- list()
  for (v in as.character(unique(tab$variant))) {
    tv <- tab[tab$variant == v, , drop = FALSE]
    out[[v]] <- lapply(split(tv, tv$input), function(rows) { force(rows); nm <- rows$input[1]; function(d) apply_rows(d, rows, nm) })
  }
  out
}

apply_rows <- function(d, rows, name) {
  filt <- setdiff(names(rows), c("variant", "input", "op", "value", "column", "note", "family", "mode", "file"))
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    sel <- rep(TRUE, nrow(d)); used <- character(0)
    for (f in filt) {
      val <- r[[f]]
      if (is.na(val) || val %in% c("", "*")) next
      if (!f %in% names(d)) stop(name, ": filter column ", f, " is not in the table")
      sel <- sel & as.character(d[[f]]) %in% strsplit(as.character(val), ";", fixed = TRUE)[[1]]
      used <- c(used, paste0(f, "=", val))
    }
    if (!any(sel)) stop(name, ": no rows match ", paste(used, collapse = ", "), " (variant ", r$variant, ")")
    cols <- if ("column" %in% names(r) && !is.na(r$column) && nzchar(r$column)) strsplit(as.character(r$column), ";", fixed = TRUE)[[1]]
            else { yc <- year_cols(d); if (length(yc)) yc else if ("value" %in% names(d)) "value" else stop(name, ": no year or value column; give column=") }
    for (cn in cols) {
      if (!cn %in% names(d)) stop(name, ": column ", cn, " is not in the table")
      x <- suppressWarnings(as.numeric(r$value))
      if (r$op %in% c("scale", "add") && is.na(x)) stop(name, ": op ", r$op, " needs a number, got '", r$value, "'")
      d[[cn]][sel] <- switch(r$op, scale = d[[cn]][sel] * x, add = d[[cn]][sel] + x,
                             set = if (is.numeric(d[[cn]])) { if (is.na(x)) stop(name, ": column ", cn, " is numeric; value '", r$value, "' is not") else rep(x, sum(sel)) } else rep(r$value, sum(sel)),
                             stop(name, ": op must be scale, add or set"))
    }
  }
  d
}
