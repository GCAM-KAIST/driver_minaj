# gcamdata's XML pipeline (R/xml.R), shortened; run_xml_conversion is gcamdata's code with a smaller Java heap.

create_xml <- function(xml_file, mi_header = NULL) {
  if(is.null(mi_header)) {
    mi_header <- system.file("extdata/mi_headers", "ModelInterface_headers.txt", package = "gcamdata")
  }
  add_flags(list(xml_file = xml_file, mi_header = mi_header, data_tables = list()), FLAG_XML)
}

set_xml_file_helper <- function(xml, fq_name) {
  xml$xml_file <- fq_name
  xml.XML_PREFIX = getOption("gcamdata.xml.XML_PREFIX")
  xml.XML_SUFFIX = getOption("gcamdata.xml.XML_SUFFIX")
  if(!is.null(xml.XML_SUFFIX)) {
    xml$xml_file <- paste0(gsub('\\.xml$', '', xml$xml_file), xml.XML_SUFFIX, '.xml')
  }
  if(!is.null(xml.XML_PREFIX)) {
    xml$xml_file <- paste0(xml.XML_PREFIX, xml$xml_file)
  }
  invisible(xml)
}

add_xml_data <- function(dot, data, header) {
  dot$data_tables[[length(dot$data_tables)+1]] <- list(data = data, header = header)
  invisible(dot)
}

make_run_xml_conversion <- function() {
  XML_WARNING_GIVEN <- FALSE
  function(dot) {
    use_java <- getOption("gcamdata.use_java")
    if(!isTRUE(use_java) && !XML_WARNING_GIVEN) {
      message("Skipping XML conversion as global option gcamdata.use_java is not TRUE")
      XML_WARNING_GIVEN <<- TRUE
    } else if(isTRUE(use_java)) {
      java_cp <- system.file("extdata/ModelInterface", "CSVToXML.jar",
                             package = "gcamdata")
      tmpfn <- tempfile()
      tmp_conn <- file(tmpfn, open = "w")
      for(i in seq_along(dot$data_tables)) {
        table <- dot$data_tables[[i]]
        cat("INPUT_TABLE", file = tmp_conn, sep = "\n")
        cat("Variable ID", file = tmp_conn, sep = "\n")
        cat(table$header, file = tmp_conn, sep = "\n")
        cat("", file = tmp_conn, sep = "\n")
        utils::write.table(table$data, file = tmp_conn, sep = ", ", row.names = FALSE, col.names = TRUE, quote = FALSE)
        cat("", file = tmp_conn, sep = "\n")
      }
      close(tmp_conn)
      args <- c(
        "-cp", shQuote(java_cp),
        "-Xmx256m",
        "ModelInterface.ModelGUI2.csvconv.CSVToXMLMain",
        tmpfn,
        shQuote(dot$mi_header),
        shQuote(dot$xml_file)
      )
      warning_msgs <- system2("java", args, stdout = TRUE, stderr = TRUE)
      unlink(tmpfn)
      if(!is.null(warning_msgs) && length(warning_msgs) > 0) {
        warning(warning_msgs)
      }
    }
    invisible(dot)
  }
}

run_xml_conversion <- make_run_xml_conversion()
