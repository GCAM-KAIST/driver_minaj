# Adopting an existing drake cache as starting values, so the first make_minaj needs no long recompute.
#
# drake files each stored value under the same xxhash64 the store uses, so adopted values are hard links
# to drake's files (no copy). Nothing is adopted unless:
#   * constants.R is the file drake last built with (drake wipes its cache when it changes), no build is
#     writing the cache, and it holds no user-modification targets;
#   * a spot check agrees: every chunk without inputs, re-run now, and 25 CSVs, re-read now, hash exactly as
#     drake stored them (this catches a different environment or CSV reader);
# and a value is adopted only when, in addition:
#   * for a CSV: its content is what drake read;
#   * for a chunk: its code is the text drake stored, every R or package-data file it reaches (its own file
#     included) is older than the OLDEST chunk build in the cache, every input is exactly (by hash) what
#     drake fed it, no input was rebuilt after it, and each output was stored after it;
#   * for an XML file in xml/: it is the file drake wrote, written after the XML object it came from, and
#     the writer code, header file and jar are older than that.
# Anything else is left to be recomputed. Adopted results are labelled "drake-import".

#' Adopt the tree's drake cache as starting values
#'
#' Needs the drake package. Adopts a stored drake value only when it matches the current code, inputs
#' and package data. Nothing is adopted when R/constants.R differs from the one the cache was built with,
#' the cache holds user-modification targets, it was written in the last 3 minutes, or a spot check
#' (chunks without inputs re-run, 25 CSV files re-read) gives values other than the cache holds.
#' Everything not adopted is recomputed when needed.
#' @param cache The drake cache folder (default: .drake in the gcamdata folder).
#' @return Invisibly, a list: csv, chunks and xml (counts adopted or already stored) and skipped (a table
#'   of reasons).
#' @export
drake_to_minaj <- function(cache = NULL) {
  if (!is.null(cache)) cache <- user_path(cache)
  restore <- enter(); on.exit(leave(restore))
  if (is.null(cache)) cache <- file.path(.nm$root, ".drake")
  if (!requireNamespace("drake", quietly = TRUE)) stop("drake is needed to read its cache")
  dd <- normalizePath(cache, "/", mustWork = TRUE); data_dir <- file.path(dd, "data")
  kh <- new.env(); mt <- new.env()
  khash <- function(nsp, k) { id <- paste(nsp, k); if (!is.null(kh[[id]])) return(kh[[id]])
    f <- file.path(dd, "keys", nsp, k); kh[[id]] <- if (file.exists(f)) readLines(f, n = 1L, warn = FALSE) else NA_character_; kh[[id]] }
  dval <- function(h) readRDS(file.path(data_dir, paste0(h, ".rds")))
  meta <- function(k) { if (exists(k, envir = mt, inherits = FALSE)) return(mt[[k]])
    h <- khash("meta", k); m <- if (is.na(h)) NULL else tryCatch(dval(h), error = function(e) NULL); assign(k, m, envir = mt); m }
  mdate <- function(m) as.POSIXct(sub(" GMT$", "", m$date), format = "%Y-%m-%d %H:%M:%OS %z")

  objs <- list.files(file.path(dd, "keys", "objects"), full.names = TRUE)
  if (any(grepl("__0$", objs))) stop("this cache holds user-modification targets (__0); not adopted")
  newest <- max(file.mtime(c(objs, list.files(file.path(dd, "keys", "meta"), full.names = TRUE))), na.rm = TRUE)
  if (difftime(Sys.time(), newest, units = "mins") < 3) stop("the cache was written in the last 3 minutes; a build may be running")
  m <- meta("constants")
  if (is.null(m) || !identical(m$input_file_hash, h64s(digest::digest(file = "R/constants.R", algo = "xxhash64"))))
    stop("R/constants.R differs from the one this drake cache was built with; nothing adopted")
  spot_check(khash, meta)

  # names -> defining files (R/*.R; package data in data/*.rda and R/sysdata.rda)
  name_file <- unlist(lapply(names(.nm$file_defs), function(f) setNames(rep(file.path("R", f), length(.nm$file_defs[[f]])), .nm$file_defs[[f]])))
  lz <- lazy_env()
  rda <- file.path("data", list.files(file.path(.nm$root, "data"), pattern = "\\.(rda|RData)$"))
  if (is.environment(lz)) for (n in ls(lz, all.names = TRUE)) {
    own <- rda[sub("\\.(rda|RData)$", "", basename(rda)) == n]
    name_file[n] <- if (length(own)) own[1] else rda[which.max(file.mtime(file.path(.nm$root, rda)))]
  }
  for (n in setdiff(ns_names(), names(name_file))) if (file.exists(file.path(.nm$root, "R", "sysdata.rda"))) name_file[n] <- "R/sysdata.rda"
  fmtime <- function(rel) { p <- file.path(.nm$root, unique(rel)); x <- file.mtime(p); if (!length(x) || all(is.na(x))) as.POSIXct(0, origin = "1970-01-01") else max(x, na.rm = TRUE) }
  newest_file <- function(n) { f <- unique(name_file[reach(n)]); f <- f[!is.na(f)]
    fd <- unique(unlist(file_deps_table()[intersect(reach(n), names(file_deps_table()))], use.names = FALSE))
    fmtime(c(f, fd)) }
  chunk_dates <- vapply(unique(.nm$co$name), function(ch) { x <- meta(ch); if (is.null(x)) NA_real_ else as.numeric(mdate(x)) }, numeric(1))
  oldest_build <- as.POSIXct(min(chunk_dates, na.rm = TRUE), origin = "1970-01-01")

  link <- function(h) {
    src <- file.path(data_dir, paste0(h, ".rds")); dst <- obj_path(h)
    if (file.exists(dst)) return(TRUE); if (!file.exists(src)) return(FALSE)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (suppressWarnings(file.link(src, dst))) return(TRUE)
    tmp <- paste0(dst, ".part"); file.copy(src, tmp, overwrite = TRUE); move_into_place(tmp, dst); TRUE
  }
  skip <- character(0); have <- list(); n_csv <- 0; n_ch <- 0; n_xml <- 0; secs <- numeric(0)

  # CSV reads
  reader_new <- newest_file("load_csv_files")
  for (nm in csv_inputs()) {
    optional <- isTRUE(.nm$optional[[nm]])
    fq <- get("find_csv_file", .nm$ns)(nm, optional, quiet = TRUE)
    fh <- if (is.null(fq)) "missing" else file_hash(normalizePath(fq, "/"))
    prev <- rec_get("csv", h64(list("csv", nm, fh, optional, reader_key(), .nm$env_key)))
    if (!is.null(prev) && obj_exists(prev$h)) { have[[nm]] <- prev$h; n_csv <- n_csv + 1; next }   # already stored
    k <- make.names(nm); mm <- meta(k); if (is.null(mm)) { skip <- c(skip, "csv: not in cache"); next }
    if (is.null(fq)) { if (!any(grepl("missing_data", mm$command))) { skip <- c(skip, "csv: file now missing"); next } }
    else if (!identical(mm$input_file_hash, h64s(fh))) { skip <- c(skip, "csv: file changed since the build"); next }
    if (reader_new > oldest_build) { skip <- c(skip, "csv: reader code newer than the build"); next }
    h <- khash("objects", k); if (is.na(h) || !link(h)) { skip <- c(skip, "csv: value file missing"); next }
    key <- h64(list("csv", nm, fh, optional, reader_key(), .nm$env_key))
    rec_put("csv", key, list(name = nm, h = h, file_hash = fh, date = Sys.time(), source = "drake-import"))
    have[[nm]] <- h; n_csv <- n_csv + 1
  }

  # chunks, upstream first
  std <- get("standardize_imported_function", asNamespace("drake")); renc <- get("reencode_namespaced", asNamespace("drake"))
  for (ch in topo_all()) {
    inputs <- .nm$by_chunk[[ch]]; if (is.null(inputs)) inputs <- character(0)
    if (!all(inputs %in% names(have))) { skip <- c(skip, "chunk: an input was not adopted"); next }
    ih <- setNames(vapply(inputs, function(n) have[[n]], character(1)), inputs)
    prev <- rec_get("res", chunk_key(ch, inputs, ih))
    if (!is.null(prev) && all(vapply(prev$out, obj_exists, logical(1)))) { for (o in names(prev$out)) have[[o]] <- prev$out[[o]]; n_ch <- n_ch + 1; next }
    mm <- meta(ch); if (is.null(mm)) { skip <- c(skip, "chunk: not in cache"); next }
    dh <- vapply(make.names(inputs), function(k) khash("objects", k), character(1))
    if (!identical(unname(ih), unname(dh))) { skip <- c(skip, "chunk: inputs differ from what drake used"); next }
    stored <- tryCatch(dval(khash("objects", renc(paste0("gcamdata:::", ch)))), error = function(e) NULL)
    if (is.null(stored) || !identical(stored[1], std(get(ch, envir = .nm$ns)))) { skip <- c(skip, "chunk: code changed since the build"); next }
    built <- mdate(mm)
    if (newest_file(ch) > oldest_build) { skip <- c(skip, "chunk: its code or data files are newer than the oldest build"); next }
    idates <- vapply(make.names(inputs), function(k) { x <- meta(k); if (is.null(x)) NA_real_ else as.numeric(mdate(x)) }, numeric(1))
    if (any(is.na(idates)) || any(idates > as.numeric(built) + 1)) { skip <- c(skip, "chunk: an input was rebuilt after it"); next }
    outs <- .nm$out_by_chunk[[ch]]
    odates <- vapply(make.names(outs), function(k) { x <- meta(k); if (is.null(x)) NA_real_ else as.numeric(mdate(x)) }, numeric(1))
    if (any(is.na(odates)) || any(odates < as.numeric(built) - 1)) { skip <- c(skip, "chunk: an output was not stored after it"); next }
    oh <- vapply(make.names(outs), function(k) khash("objects", k), character(1))
    if (anyNA(oh) || !all(vapply(oh, link, logical(1)))) { skip <- c(skip, "chunk: output values missing"); next }
    oh <- setNames(unname(oh), outs)
    key <- chunk_key(ch, inputs, ih)
    rec_put("res", key, list(chunk = ch, key = key, out = oh, inputs = setNames(unname(ih), inputs), code = code_key(ch), env = .nm$env_key,
                             secs = as.numeric(mm$time_command$elapsed), date = built, source = "drake-import"))
    secs[ch] <- as.numeric(mm$time_command$elapsed)
    for (o in outs) have[[o]] <- oh[[o]]
    n_ch <- n_ch + 1
  }

  # XML files drake wrote
  xdir <- sub("/$", "", get("XML_DIR", .nm$ns))
  writer_new <- max(newest_file("run_xml_conversion"), newest_file("set_xml_file_helper"),
                    fmtime(c("inst/extdata/mi_headers/ModelInterface_headers.txt", "inst/extdata/ModelInterface/CSVToXML.jar")))
  for (x in .nm$co$output[.nm$co$to_xml]) {
    if (is.null(have[[x]])) next
    if (!is.null(rec_get("xml", h64(list("xml", have[[x]], writer_key()))))) { n_xml <- n_xml + 1; next }
    mm <- meta(make.names(file.path(xdir, x))); f <- file.path(.nm$root, xdir, x); om <- meta(make.names(x))
    if (is.null(mm) || is.null(om) || !file.exists(f)) next
    if (!identical(mm$output_file_hash, h64s(digest::digest(file = f, algo = "xxhash64")))) { skip <- c(skip, "xml: file changed since drake wrote it"); next }
    if (mdate(mm) < mdate(om) - 1 || file.mtime(f) < mdate(om) - 1) { skip <- c(skip, "xml: file older than its XML object"); next }
    if (writer_new > oldest_build) { skip <- c(skip, "xml: writer code, header or jar newer than the oldest build"); next }
    rec_put("xml", h64(list("xml", have[[x]], writer_key())), list(name = x, md5 = file_md5(f), size = file.size(f), date = mdate(mm),
                                                                     source = "drake-import", source_file = normalizePath(f, "/")))
    n_xml <- n_xml + 1
  }

  old <- if (file.exists(p <- file.path(.nm$state_dir, "secs.rds"))) readRDS(p) else numeric(0)
  old[names(secs)] <- secs; save_rds(old, p, level = 0L); .nm$secs <- old
  message(sprintf("minaj: adopted from %s: %d of %d CSV reads, %d of %d chunks, %d of %d XML files", dd, n_csv, length(csv_inputs()),
                  n_ch, length(unique(.nm$co$name)), n_xml, sum(.nm$co$to_xml)))
  if (length(skip)) { t <- sort(table(skip), decreasing = TRUE); for (i in seq_along(t)) message(sprintf("  not adopted: %4d  %s", t[[i]], names(t)[i])) }
  invisible(list(csv = n_csv, chunks = n_ch, xml = n_xml, skipped = table(skip)))
}

# Re-run every chunk without inputs and re-read 25 CSVs; all must hash as drake stored them.
spot_check <- function(khash, meta) {
  noin <- unique(.nm$co$name)[!unique(.nm$co$name) %in% names(.nm$by_chunk)]
  for (ch in noin) {
    out <- exec_chunk(ch, list(), character(0))$out
    for (o in names(out)) {
      d <- khash("objects", make.names(o))
      if (!is.na(d) && !identical(h64(out[o]), d))
        stop("spot check failed: ", ch, " now gives a different ", o, " than the drake cache holds (environment variables or constants differ?); nothing adopted")
    }
  }
  csv <- csv_inputs(); csv <- csv[unique(round(seq(1, length(csv), length.out = min(25, length(csv)))))]
  for (nm in csv) {
    opt <- isTRUE(.nm$optional[[nm]])
    d <- khash("objects", make.names(nm)); m <- meta(make.names(nm))
    fq <- get("find_csv_file", .nm$ns)(nm, opt, quiet = TRUE)
    if (is.na(d) || is.null(m) || is.null(fq)) next
    if (!identical(m$input_file_hash, h64s(file_hash(normalizePath(fq, "/"))))) next      # edited since the build: not evidence
    v <- get("load_csv_files", .nm$ns)(nm, opt, quiet = TRUE)
    if (!identical(h64(v), d))
      stop("spot check failed: ", nm, " (unchanged since the build) reads differently now than in the drake cache ",
           "(CSV reader code, readr version or environment variables differ); nothing adopted")
  }
  invisible(TRUE)
}

topo_all <- function() {
  chunks <- unique(.nm$co$name)
  dep <- lapply(setNames(chunks, chunks), function(ch) { ins <- .nm$by_chunk[[ch]]; unique(unname(.nm$producer[ins[ins %in% names(.nm$producer)]])) })
  out <- character(0)
  while (length(dep)) {
    ready <- names(dep)[vapply(dep, function(d) all(d %in% out), logical(1))]
    if (!length(ready)) stop("dependency cycle among: ", paste(head(names(dep)), collapse = ", "))
    out <- c(out, ready); dep[ready] <- NULL
  }
  out
}
