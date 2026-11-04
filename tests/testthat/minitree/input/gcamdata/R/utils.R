# Chunk declarations, CSV reading and data helpers, after gcamdata's R/utils.R and R/utils-data.R.

find_chunks <- function(pattern = "^module_[a-zA-Z\\.]*_.*$") {
  data.frame(name = ls(name = parent.env(environment()), pattern = pattern), stringsAsFactors = FALSE)
}

chunk_inputs <- function(chunks = find_chunks()$name, call_flag = driver.DECLARE_INPUTS) {
  rows <- lapply(chunks, function(ch) {
    req <- eval(call(ch, call_flag))
    if (is.null(req)) return(NULL)
    nm <- if (is.null(names(req))) rep("", length(req)) else names(req)
    data.frame(name = ch, input = unname(req), from_file = nm %in% c("FILE", "OPTIONAL_FILE"), optional = nm == "OPTIONAL_FILE",
               stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(data.frame(name = character(), input = character(), from_file = logical(), optional = logical())), rows))
}

chunk_outputs <- function(chunks = find_chunks()$name, call_flag = driver.DECLARE_OUTPUTS) {
  rows <- lapply(chunks, function(ch) {
    req <- eval(call(ch, call_flag))
    if (is.null(req)) return(NULL)
    nm <- if (is.null(names(req))) rep("", length(req)) else names(req)
    data.frame(name = ch, output = unname(req), to_xml = nm == "XML", stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(data.frame(name = character(), output = character(), to_xml = logical())), rows))
}

find_csv_file <- function(filename, optional, quiet = FALSE) {
  for (ex in c(".csv", ".csv.gz", "")) {
    fqfn <- system.file("extdata", paste0(filename, ex), package = "gcamdata")
    if (fqfn != "") {
      if (!quiet) cat("Found", fqfn, "\n")
      return(fqfn)
    }
  }
  if (optional) NULL else stop("Couldn't find required data ", filename)
}

# A CSV input: comment lines "# Title: ...", "# Units: ...", "# Column types: ..." (c character, n numeric, i integer), then the table.
load_csv_files <- function(filenames, optionals, quiet = FALSE) {
  filedata <- list()
  for (i in seq_along(filenames)) {
    f <- filenames[i]
    fqfn <- find_csv_file(f, optionals[i], quiet = quiet)
    if (is.null(fqfn)) {
      filedata[[f]] <- missing_data()
      next
    }
    header <- grep("^#", readLines(fqfn, warn = FALSE), value = TRUE)
    info <- function(label) {
      x <- grep(paste0("^#\\s*", label, ":"), header, value = TRUE)
      if (length(x)) trimws(sub(paste0("^#\\s*", label, ":"), "", x[1])) else NULL
    }
    types <- c(c = "character", n = "numeric", i = "integer")[strsplit(info("Column types"), "")[[1]]]
    fd <- utils::read.csv(fqfn, comment.char = "#", colClasses = unname(types), check.names = FALSE, na.strings = "")
    fd <- tibble::as_tibble(fd)
    fd <- add_title(fd, if (is.null(info("Title"))) f else info("Title"))
    fd <- add_units(fd, info("Units"))
    fd <- add_comments(fd, paste("Read from", gsub("^.*extdata", "extdata", fqfn)))
    filedata[[f]] <- add_flags(fd, FLAG_INPUT_DATA)
  }
  filedata
}

mini_round <- function(x) round(x, 3)     # used by module_mini_L200.coef only

ATTR_TITLE <- "title"
ATTR_UNITS <- "units"
ATTR_COMMENTS <- "comments"
ATTR_PRECURSORS <- "precursors"

add_title <- function(x, title) { attr(x, ATTR_TITLE) <- title; x }
add_units <- function(x, units) { attr(x, ATTR_UNITS) <- units; x }
add_comments <- function(x, comments) { attr(x, ATTR_COMMENTS) <- c(attr(x, ATTR_COMMENTS), comments); x }
add_precursors <- function(x, ...) { attr(x, ATTR_PRECURSORS) <- c(...); x }
add_flags <- function(x, ...) { attr(x, "flags") <- unique(c(attr(x, "flags"), ...)); x }
get_flags <- function(x) attr(x, "flags")

missing_data <- function() {
  x <- tibble::tibble(x = NA_real_)
  add_flags(add_comments(add_units(add_title(x, "Data not created"), "Data not created"), "Data not created"), FLAG_NO_OUTPUT)
}

get_data <- function(all_data, name) {
  if (is.null(all_data[[name]])) stop("Data system: couldn't find ", name)
  if (nrow(all_data[[name]]) > 0 && all(is.na(all_data[[name]]))) return(NULL)     # an optional input that is missing
  all_data[[name]]
}

# Writes chunk tables to <outputs_dir>/<name>.csv: a line of flags, lines of comments, then the table.
save_chunkdata <- function(chunkdata, write_inputs = FALSE, write_outputs = TRUE, write_xml = write_outputs,
                           outputs_dir = OUTPUTS_DIR, xml_dir = XML_DIR) {
  for (cn in names(chunkdata)) {
    cd <- chunkdata[[cn]]
    if (is.null(cd)) next
    if (FLAG_XML %in% get_flags(cd)) {
      if (write_xml) run_xml_conversion(set_xml_file_helper(cd, file.path(xml_dir, cd$xml_file)))
    } else if (write_outputs) {
      fqfn <- file.path(outputs_dir, paste0(cn, ".csv"))
      suppressWarnings(file.remove(fqfn))
      flags <- get_flags(cd)
      if (FLAG_NO_OUTPUT %in% flags || FLAG_INPUT_DATA %in% flags && !write_inputs) next
      if (!is.null(flags)) cat(paste(COMMENT_CHAR, paste(flags, collapse = " ")), file = fqfn, sep = "\n")
      if (!is.null(attr(cd, ATTR_COMMENTS))) cat(paste(COMMENT_CHAR, attr(cd, ATTR_COMMENTS)), file = fqfn, sep = "\n", append = TRUE)
      suppressWarnings(utils::write.table(cd, fqfn, sep = ",", row.names = FALSE, append = TRUE, qmethod = "double"))
    }
  }
}

# One row per data object: chunk, name, and its precursors, title, units, comments and flags.
tibbelize_outputs <- function(chunk_data, chunk_name) {
  rows <- lapply(names(chunk_data), function(cd) {
    x <- chunk_data[[cd]]
    if (is.null(x) || !length(x)) return(NULL)
    tibble::tibble(name = chunk_name, output = cd, precursors = paste(attr(x, ATTR_PRECURSORS), collapse = data.SEPARATOR),
                   title = paste(attr(x, ATTR_TITLE), collapse = data.SEPARATOR), units = paste(attr(x, ATTR_UNITS), collapse = data.SEPARATOR),
                   comments = paste(attr(x, ATTR_COMMENTS), collapse = data.SEPARATOR), flags = paste(get_flags(x), collapse = data.SEPARATOR))
  })
  do.call(rbind, rows)
}

return_data <- function(...) {
  dots <- list(...)
  names(dots) <- vapply(as.list(substitute(list(...)))[-1L], deparse, "")
  dots
}
