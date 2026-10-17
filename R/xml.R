# Writing XML files. A written file is recorded under (XML object hash + writer key); a request for an
# unchanged XML costs a checksum, or a file copy from the store, instead of a Java conversion.

xml_store_path <- function(key) file.path(.nm$store, "xml", substr(key, 1, 2), paste0(key, ".xml"))

copy_checked <- function(src, dst, md5) {
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  for (i in 1:10) {
    if (isTRUE(suppressWarnings(file.copy(src, dst, overwrite = TRUE))) && identical(file_md5(dst), md5)) return(TRUE)
    Sys.sleep(0.2 * i)
  }
  FALSE
}

write_xml <- function(name, obj_h, file) {
  key <- h64(list("xml", obj_h, writer_key()))
  rec <- rec_get("xml", key)
  if (!is.null(rec)) {
    if (file.exists(file) && identical(file_md5(file), rec$md5)) return("unchanged")
    for (src in c(xml_store_path(key), rec$source_file)) {
      if (!is.null(src) && file.exists(src) && identical(file_md5(src), rec$md5) && copy_checked(src, file, rec$md5)) return("copied from store")
    }
  }
  if (!nzchar(Sys.which("java"))) stop("java is not on PATH; cannot write ", name)
  if (!isTRUE(getOption("gcamdata.use_java"))) stop("option gcamdata.use_java is not TRUE; XML conversion would be skipped")
  ns <- .nm$ns
  x <- obj_get(obj_h)[[1]]
  hdr <- file.path(.nm$root, "inst", "extdata", "mi_headers", "ModelInterface_headers.txt")
  if (!is.null(x$mi_header) && file.exists(hdr)) x$mi_header <- normalizePath(hdr, "/")   # this tree's header, whatever tree made the object
  out <- xml_store_path(key); dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(out, ".part.xml"); unlink(tmp)
  t0 <- Sys.time(); msgs <- character(0)
  withCallingHandlers(get("run_xml_conversion", ns)(get("set_xml_file_helper", ns)(x, tmp)),
                      warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
  if (!file.exists(tmp) || file.size(tmp) == 0 || file.mtime(tmp) < t0 - 1) stop("XML conversion wrote no file for ", name, "\n", paste(utils::tail(msgs, 10), collapse = "\n"))
  if (any(grepl("Exception|\\bError\\b", msgs))) { unlink(tmp); stop("XML conversion reported an error for ", name, ":\n", paste(utils::tail(msgs, 10), collapse = "\n")) }
  move_into_place(tmp, out)
  md5 <- unname(tools::md5sum(out))
  if (!copy_checked(out, file, md5)) stop("could not write ", file, " (file in use?); the converted file is kept in the store")
  if (file.size(out) > nm_opt("keep_xml_mb", 200) * 2^20) unlink(out)
  rec_put("xml", key, list(name = name, md5 = md5, size = file.size(file), date = Sys.time(), source = "converted"))
  "converted"
}

manifest_add <- function(dir, rows) {
  f <- file.path(dir, "minaj_manifest.csv")
  tryCatch(utils::write.table(rows, f, sep = ",", row.names = FALSE, col.names = !file.exists(f), append = file.exists(f), qmethod = "double"),
           error = function(e) warning("minaj: manifest not updated (", conditionMessage(e), ")", call. = FALSE))
}

tree_head <- function() {
  h <- tryCatch(system2("git", c("-C", shQuote(.nm$tree), "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE), error = function(e) NA, warning = function(w) NA)
  if (length(h)) h[1] else NA_character_
}

# Absolute paths of the gcamdata XML files a GCAM configuration loads (comments removed).
xml_paths_in_config <- function(config) {
  txt <- paste(readLines(config, warn = FALSE), collapse = "\n")
  txt <- gsub("<!--.*?-->", "", txt, perl = TRUE)
  vals <- regmatches(txt, gregexpr("<Value[^>]*>[^<]*</Value>", txt))[[1]]
  vals <- trimws(sub("^<Value[^>]*>([^<]*)</Value>$", "\\1", vals))
  vals <- vals[grepl("gcamdata/xml[^/]*/[^/]+\\.xml$", vals)]
  unique(normalizePath(ifelse(grepl("^([A-Za-z]:)?[/\\\\]", vals), vals, file.path(dirname(config), vals)), "/", mustWork = FALSE))
}
