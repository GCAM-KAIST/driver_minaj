# The store: values filed under the hash of their content, and small records that map a fingerprint
# (chunk + code + environment + input hashes, or CSV file + reader) to the hashes of its results.
# Values are kept as one-element named lists, exactly as drake keeps chunk outputs, so a drake cache can
# be adopted by hard-linking its files (same hash, same bytes).

# Write via a temporary file in the same folder, then rename (retrying while Windows holds the file).
save_rds <- function(x, path, level = nm_opt("compress", 1L)) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".part")
  ok <- FALSE
  on.exit(if (!ok) unlink(tmp), add = TRUE)
  con <- if (level > 0) gzfile(tmp, "wb", compression = level) else file(tmp, "wb")
  tryCatch(saveRDS(x, con), finally = close(con))
  move_into_place(tmp, path)
  ok <- TRUE
  invisible(path)
}

move_into_place <- function(tmp, path) {
  for (i in 1:20) {
    if (suppressWarnings(file.rename(tmp, path))) return(invisible(TRUE))
    Sys.sleep(0.1 * i)
  }
  stop("could not write ", path, " (file in use?)")
}

obj_path <- function(h) file.path(.nm$store, "obj", substr(h, 1, 2), paste0(h, ".rds"))

obj_exists <- function(h) !is.na(h) && file.exists(obj_path(h))

# Store a one-element named list; returns its hash.
obj_put <- function(v, ctx = NULL) {
  h <- h64(v)
  if (!is.null(ctx)) cache_val(ctx, h, v)
  p <- obj_path(h)
  if (!file.exists(p)) save_rds(v, p)
  h
}

obj_get <- function(h, ctx = NULL) {
  if (!is.null(ctx) && !is.null(ctx$val[[h]])) return(ctx$val[[h]])
  if (!obj_exists(h)) stop("value ", h, " missing from the store")
  v <- tryCatch(readRDS(obj_path(h)), error = function(e) {
    unlink(obj_path(h)); stop("stored value ", h, " was unreadable and has been removed; run the call again", call. = FALSE)
  })
  if (!is.null(ctx)) cache_val(ctx, h, v)
  v
}

# Values used within one call are kept in memory up to minaj.mem_mb (default 1500 MB), oldest dropped first.
cache_val <- function(ctx, h, v) {
  if (!is.null(ctx$val[[h]])) return(invisible())
  sz <- as.numeric(utils::object.size(v)) / 2^20
  ctx$val[[h]] <- v; ctx$val_mb[h] <- sz
  cap <- nm_opt("mem_mb", 1500)
  while (sum(ctx$val_mb) > cap && length(ctx$val_mb) > 1) {
    old <- names(ctx$val_mb)[1]; ctx$val[[old]] <- NULL; ctx$val_mb <- ctx$val_mb[-1]
  }
  invisible()
}

rec_path <- function(kind, key) file.path(.nm$store, "rec", kind, substr(key, 1, 2), paste0(key, ".rds"))
rec_get <- function(kind, key) { p <- rec_path(kind, key); if (file.exists(p)) tryCatch(readRDS(p), error = function(e) NULL) else NULL }
rec_put <- function(kind, key, rec) save_rds(rec, rec_path(kind, key), level = 0L)

# The last record per chunk in this tree, to say why a chunk had to run again.
last_path <- function(chunk) file.path(.nm$state_dir, "last", paste0(chunk, ".rds"))
last_get <- function(chunk) { p <- last_path(chunk); if (file.exists(p)) tryCatch(readRDS(p), error = function(e) NULL) else NULL }

# ---- file hashes ------------------------------------------------------------------------------------

findex_load <- function() {
  if (is.null(.nm$findex)) {
    p <- file.path(.nm$state_dir, "files.rds")
    .nm$findex <- if (file.exists(p)) tryCatch(readRDS(p), error = function(e) list()) else list()
  }
}

# Content hash of a file. Files over 1 MB keep their hash while size and modification time stay the same;
# smaller files are always hashed (microseconds), so a touched but unchanged file is never "changed".
file_hash <- function(path) {
  findex_load()
  info <- file.info(path, extra_cols = FALSE)
  sz <- info$size; mt <- as.numeric(info$mtime)
  hit <- .nm$findex[[path]]
  if (!is.null(hit) && !is.null(hit$hash) && sz > 1e6 && hit$size == sz && hit$mtime == mt) return(hit$hash)
  h <- digest::digest(file = path, algo = "xxhash64")
  .nm$findex[[path]] <- list(size = sz, mtime = mt, hash = h)
  .nm$findex_dirty <- TRUE
  h
}

# md5 of an XML file, cached while size and modification time stay the same.
file_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  findex_load()
  key <- paste0("md5:", normalizePath(path, "/"))
  info <- file.info(path, extra_cols = FALSE); sz <- info$size; mt <- as.numeric(info$mtime)
  hit <- .nm$findex[[key]]
  if (!is.null(hit) && hit$size == sz && hit$mtime == mt) return(hit$md5)
  m <- unname(tools::md5sum(path))
  .nm$findex[[key]] <- list(size = sz, mtime = mt, md5 = m)
  .nm$findex_dirty <- TRUE
  m
}

save_findex <- function() {
  if (isTRUE(.nm$findex_dirty) && !is.null(.nm$state_dir)) { save_rds(.nm$findex, file.path(.nm$state_dir, "files.rds"), level = 0L); .nm$findex_dirty <- FALSE }
}
