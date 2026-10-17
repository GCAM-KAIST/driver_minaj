# The runner. To get a data object, minaj works out its fingerprint from the fingerprints of everything
# upstream (CSV file contents, chunk code keys), then reuses the stored result for that fingerprint or
# runs the chunk. A re-run chunk whose outputs hash the same as before leaves everything downstream
# reusable (early cutoff), because downstream fingerprints are built from those hashes.

new_ctx <- function(override = list(), dry = FALSE) {
  e <- new.env(parent = emptyenv())
  e$over <- override; e$dry <- dry
  e$h <- list(); e$cres <- list(); e$val <- list(); e$val_mb <- setNames(numeric(0), character(0))
  e$ran <- character(); e$reused <- character(); e$read <- character(); e$would <- character(); e$why <- list()
  e
}

# Hash of one data object (CSV input or chunk output) under this context.
value_hash <- function(name, ctx) {
  h <- ctx$h[[name]]
  if (!is.null(h)) return(h)
  h <- if (name %in% names(ctx$over)) override_hash(name, ctx) else base_hash(name, ctx)
  ctx$h[[name]] <- h
  h
}

base_hash <- function(name, ctx) {
  if (is_csv(name)) return(csv_hash(name, ctx))
  ch <- chunk_of(name)
  if (is.null(ch)) stop("'", name, "' is neither a CSV input nor produced by any chunk of this tree", call. = FALSE)
  unname(chunk_result(ch, ctx)$out[name])
}

csv_hash <- function(name, ctx) {
  optional <- isTRUE(.nm$optional[[name]])
  fq <- get("find_csv_file", .nm$ns)(name, optional, quiet = TRUE)
  fh <- if (is.null(fq)) "missing" else file_hash(normalizePath(fq, "/"))
  key <- h64(list("csv", name, fh, optional, reader_key(), .nm$env_key))
  rec <- rec_get("csv", key)
  if (!is.null(rec) && obj_exists(rec$h)) return(rec$h)
  if (ctx$dry) { ctx$would <- c(ctx$would, paste("read", name)); return(NA_character_) }
  v <- get("load_csv_files", .nm$ns)(name, optional, quiet = TRUE)
  h <- obj_put(v, ctx)
  rec_put("csv", key, list(name = name, h = h, file_hash = fh, date = Sys.time(), source = "read"))
  ctx$read <- c(ctx$read, name)
  h
}

chunk_key <- function(ch, inputs, ih) h64(list("res", ch, code_key(ch), .nm$env_key, as.character(inputs), as.character(unname(ih))))

chunk_result <- function(ch, ctx) {
  r <- ctx$cres[[ch]]
  if (!is.null(r)) return(r)
  inputs <- .nm$by_chunk[[ch]]
  if (is.null(inputs)) inputs <- character(0)
  ih <- vapply(inputs, value_hash, character(1), ctx = ctx)
  outs <- .nm$out_by_chunk[[ch]]
  if (anyNA(ih)) {                                   # dry run: something upstream would run
    ctx$would <- c(ctx$would, ch); ctx$why[[ch]] <- "an input would change"
    r <- list(out = setNames(rep(NA_character_, length(outs)), outs)); ctx$cres[[ch]] <- r; return(r)
  }
  key <- chunk_key(ch, inputs, ih)
  rec <- rec_get("res", key)
  if (!is.null(rec) && all(vapply(rec$out, obj_exists, logical(1)))) {
    if (!is.null(rec$check) && !ctx$dry) warning("minaj: reused result of ", ch, " failed gcamdata's output check when it ran: ", rec$check, call. = FALSE)
    ctx$reused <- c(ctx$reused, ch); ctx$cres[[ch]] <- rec; return(rec)
  }
  ctx$why[[ch]] <- why_text(ch, inputs, ih)
  if (ctx$dry) {
    ctx$would <- c(ctx$would, ch)
    r <- list(out = setNames(rep(NA_character_, length(outs)), outs)); ctx$cres[[ch]] <- r; return(r)
  }
  all_data <- do.call(c, c(list(list()), unname(lapply(ih, obj_get, ctx = ctx))))
  t0 <- proc.time()[["elapsed"]]
  x <- exec_chunk(ch, all_data, inputs)
  out <- x$out; check <- x$check
  secs <- proc.time()[["elapsed"]] - t0
  hashes <- vapply(names(out), function(n) obj_put(out[n], ctx), character(1))
  rm(all_data, x, out)
  rec <- list(chunk = ch, key = key, out = hashes, inputs = setNames(unname(ih), inputs), code = code_key(ch),
              reach = setNames(vapply(reach(ch), name_hash, character(1)), reach(ch)), env = .nm$env_key,
              secs = secs, date = Sys.time(), source = "run")
  if (!is.null(check)) rec$check <- check
  rec_put("res", key, rec)
  save_rds(rec[c("chunk", "key", "inputs", "code", "reach", "env", "secs", "date")], last_path(ch), level = 0L)
  ctx$ran <- c(ctx$ran, ch); ctx$cres[[ch]] <- rec
  rec
}

# Run one chunk function. Returns list(out = outputs, check = NULL or the output-check failure).
exec_chunk <- function(ch, all_data, inputs) {
  ns <- .nm$ns
  restore <- stash_globals(ch)
  on.exit(restore(), add = TRUE)
  before <- search()
  on.exit({ for (p in setdiff(search(), before)) try(detach(p, character.only = TRUE), silent = TRUE) }, add = TRUE)
  out <- get(ch, envir = ns)(get("driver.MAKE", envir = ns), all_data)
  promised <- .nm$out_by_chunk[[ch]]
  if (!identical(sort(names(out)), sort(promised))) stop("chunk ", ch, " returned ", paste(setdiff(names(out), promised), collapse = ", "),
                                                         " / did not return ", paste(setdiff(promised, names(out)), collapse = ", "))
  check <- NULL
  if (nm_opt("check_outputs", TRUE) && exists("check_chunk_outputs", envir = ns, inherits = FALSE)) {
    to_xml <- .nm$co$to_xml[match(promised, .nm$co$output)]
    check <- tryCatch(withCallingHandlers(suppressMessages(get("check_chunk_outputs", envir = ns)(ch, out, inputs, promised, to_xml)),
                                          warning = function(w) { warning("minaj: ", ch, ": ", conditionMessage(w), call. = FALSE); invokeRestart("muffleWarning") }),
                      error = function(e) { warning("minaj: output check for ", ch, " failed: ", conditionMessage(e), call. = FALSE); conditionMessage(e) })
    if (!is.character(check)) check <- NULL
  }
  list(out = out, check = check)
}

# Why a chunk must run: compare with the last time it ran in this tree.
why_text <- function(ch, inputs, ih) {
  last <- last_get(ch)
  if (is.null(last)) return("no stored result for this chunk")
  r <- c()
  if (!identical(last$env, .nm$env_key)) r <- c(r, "R, package versions or NAMESPACE changed")
  if (!identical(last$code, code_key(ch))) {
    now <- vapply(reach(ch), name_hash, character(1))
    chg <- names(now)[is.na(last$reach[names(now)]) | last$reach[names(now)] != now]
    r <- c(r, paste0("code changed: ", paste(head(chg, 6), collapse = ", "), if (length(chg) > 6) " ..."))
  }
  ci <- inputs[is.na(last$inputs[inputs]) | last$inputs[inputs] != ih]
  if (length(ci)) r <- c(r, paste0("inputs changed: ", paste(head(ci, 6), collapse = ", "), if (length(ci) > 6) " ..."))
  if (!length(r)) r <- "stored result missing"
  paste(r, collapse = "; ")
}

# ---- overrides (variants) -----------------------------------------------------------------------------

# An override replaces one CSV input or data object for this call only. It can be
#   a function(table) -> table, applied to the table as read (for CSVs, before any horizon extension the tree applies),
#   a data frame (the whole table),
#   a path to a CSV file with the same columns (read by the tree's own reader), or
#   a number, which multiplies every year column (or the value column).
override_hash <- function(name, ctx) {
  v <- ctx$over[[name]]
  if (is.numeric(v) && length(v) == 1) { f <- v; v <- function(d) scale_years(d, f, name) }
  value <- if (is_csv(name)) override_csv(name, v) else {
    bh <- base_hash(name, ctx)
    if (is.na(bh)) { ctx$would <- c(ctx$would, paste("override", name)); return(NA_character_) }
    base <- obj_get(bh, ctx)
    if (is.function(v)) setNames(list(keep_attrs(v(base[[1]]), base[[1]])), name)
    else if (is.data.frame(v)) setNames(list(keep_attrs(v, base[[1]])), name)
    else stop("override for ", name, " must be a function, a table or a number")
  }
  obj_put(value, ctx)
}

override_csv <- function(name, v) {
  ns <- .nm$ns; optional <- isTRUE(.nm$optional[[name]])
  reader <- function() get("load_csv_files", ns)(name, optional, quiet = TRUE)
  if (is.character(v)) {
    if (!file.exists(v)) stop("override file not found: ", v)
    base_path <- get("find_csv_file", ns)(name, optional, quiet = TRUE)
    tmp <- file.path(tempdir(), "minaj_csv", digest::digest(file = v, algo = "xxhash64"), basename(base_path))
    if (!file.exists(tmp)) { dir.create(dirname(tmp), recursive = TRUE, showWarnings = FALSE); if (!file.copy(v, tmp)) stop("could not copy ", v) }
    orig <- get("find_csv_file", ns)
    return(with_binding("find_csv_file", function(filename, optional, quiet = FALSE) if (identical(filename, name)) tmp else orig(filename, optional, quiet), reader()))
  }
  if (is.data.frame(v)) { tab <- v; v <- function(d) tab }
  if (!is.function(v)) stop("override for ", name, " must be a function, a table, a number or a CSV path")
  if (exists("extend_horizon_assumptions", envir = ns, inherits = FALSE)) {
    orig <- get("extend_horizon_assumptions", ns)
    return(with_binding("extend_horizon_assumptions", function(fd, f) orig(if (identical(f, name)) keep_class(v(fd), fd) else fd, f), reader()))
  }
  x <- reader(); x[[1]] <- keep_attrs(v(x[[1]]), x[[1]]); x
}

keep_class <- function(new, old) {
  if (!inherits(new, "tbl_df")) new <- tibble::as_tibble(new)
  if (identical(names(new), names(old))) class(new) <- class(old)
  new
}

keep_attrs <- function(new, old) {
  new <- keep_class(new, old)
  a <- attributes(old); a <- a[setdiff(names(a), c("names", "row.names", "class", "groups"))]
  for (n in names(a)) if (is.null(attr(new, n))) attr(new, n) <- a[[n]]
  new
}

# Year columns: 4-digit names and gcamdata's year keywords (final-calibration-year, ...).
year_cols <- function(d) {
  kw <- names(get0("YEAR_RECODE", envir = .nm$ns, inherits = FALSE))
  names(d)[grepl("^[0-9]{4}$", names(d)) | names(d) %in% kw]
}

# Multiply every year column (or the value column) by f.
scale_years <- function(d, f, name = "table") {
  yc <- year_cols(d)
  if (!length(yc)) {
    if (!"value" %in% names(d)) stop("override of ", name, ": no year or 'value' column to scale; use a function or the table form with column=")
    yc <- "value"
  }
  d[yc] <- lapply(d[yc], function(x) if (is.numeric(x)) x * f else x)
  d
}
