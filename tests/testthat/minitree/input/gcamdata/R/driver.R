# gcamdata's check of a chunk's outputs (R/driver.R), shortened.
check_chunk_outputs <- function(chunk, chunk_data, chunk_inputs, promised_outputs, outputs_xml) {
  for (obj in names(chunk_data)) {
    if (!outputs_xml[which(obj == promised_outputs)]) {
      if (!tibble::is_tibble(chunk_data[[obj]])) stop(obj, " is not a tibble")
      for (at in c(ATTR_TITLE, ATTR_UNITS, ATTR_COMMENTS)) {
        if (is.null(attr(chunk_data[[obj]], at))) warning("No '", at, "' attached to ", obj, " - chunk ", chunk)
      }
    } else if (!FLAG_XML %in% get_flags(chunk_data[[obj]])) {
      stop(obj, " is not flagged as XML")
    }
    pc <- attr(chunk_data[[obj]], ATTR_PRECURSORS)
    if (!all(pc %in% c(chunk_inputs, promised_outputs))) stop("Some precursors for '", obj, "' aren't inputs - chunk ", chunk)
  }
}
