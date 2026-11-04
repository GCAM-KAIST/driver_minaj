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

# Returns the status, after passing it to `after` (if given). While driver_minaj() has background writers (xml_pool), a file
# that needs Java is queued instead: the status is "queued", and `after` gets the final status when the file is written.
write_xml <- function(name, obj_h, file, after = NULL) {
  fin <- function(st) if (is.null(after)) st else after(st)
  key <- h64(list("xml", obj_h, writer_key()))
  rec <- rec_get("xml", key)
  if (!is.null(rec)) {
    if (file.exists(file) && identical(file_md5(file), rec$md5)) return(fin("unchanged"))
    for (src in c(xml_store_path(key), rec$source_file)) {
      if (!is.null(src) && file.exists(src) && identical(file_md5(src), rec$md5) && copy_checked(src, file, rec$md5)) return(fin("copied from store"))
    }
  }
  if (!nzchar(Sys.which("java"))) stop("java not found (PATH, option minaj.java_bin, JAVA_HOME, usual install folders); set one and run open_minaj() again; cannot write ", name)
  if (!isTRUE(getOption("gcamdata.use_java"))) stop("option gcamdata.use_java is not TRUE; XML conversion would be skipped")
  out <- xml_store_path(key); dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(out, ".part.xml"); unlink(tmp)
  t0 <- Sys.time()
  here <- function() {                                                       # gcamdata's own writer, in this process
    free <- ps::ps_system_memory()$avail / 2^30; floor <- nm_opt("ram_floor_gb", 4)
    if (free < floor) stop(sprintf("%s not written: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb); try again later", name, free, floor), call. = FALSE)
    ns <- .nm$ns
    x <- obj_get(obj_h)[[1]]
    hdr <- file.path(.nm$root, "inst", "extdata", "mi_headers", "ModelInterface_headers.txt")
    if (!is.null(x$mi_header) && file.exists(hdr)) x$mi_header <- normalizePath(hdr, "/")   # this tree's header, whatever tree made the object
    msgs <- character(0)
    withCallingHandlers(get("run_xml_conversion", ns)(get("set_xml_file_helper", ns)(x, tmp)),
                        warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
    msgs
  }
  done <- function(msgs) {
    force(msgs)                                                              # done(here()): the conversion runs before the file is checked
    if (!file.exists(tmp) || file.size(tmp) == 0 || file.mtime(tmp) < t0 - 1) { unlink(tmp); stop("XML conversion wrote no file for ", name, "\n", paste(utils::tail(msgs, 10), collapse = "\n")) }
    if (any(grepl("Exception|\\bError\\b", msgs))) { unlink(tmp); stop("XML conversion reported an error for ", name, ":\n", paste(utils::tail(msgs, 10), collapse = "\n")) }
    md5 <- unname(tools::md5sum(tmp)); size <- file.size(tmp)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    if (size > nm_opt("keep_xml_mb", 200) * 2^20 && isTRUE(suppressWarnings(file.rename(tmp, file))) && identical(file.size(file), size)) {
      # moved into its folder: a file this large is not kept in the store
    } else {
      move_into_place(tmp, out)
      if (!copy_checked(out, file, md5)) stop("could not write ", file, " (file in use?); the converted file is kept in the store")
      if (size > nm_opt("keep_xml_mb", 200) * 2^20) unlink(out)
    }
    rec_put("xml", key, list(name = name, md5 = md5, size = file.size(file), date = Sys.time(), source = "converted"))
    fin("converted")
  }
  if (!is.null(.nm$pool)) {
    xml_pool(list(name = name, obj = obj_path(obj_h), out = tmp, prev_gb = if (file.exists(file)) file.size(file) / 2^30 else NA,
                  done = function(r) { if (r$status != 0) r$msgs <- c(r$msgs, sprintf("Error: Java exited with status %d", r$status)); done(r$msgs) },
                  redo = function() done(here())))
    return("queued")
  }
  done(here())
}

# minaj's copy of gcamdata's XML writer (R/xml.R, make_run_xml_conversion): the temporary text file, then Java. It uses only
# base R, so a background R process can run it with nothing but the stored object; xml_pool uses it only when the tree's
# writer is gcamdata's own code up to its Java heap. `dot` is the XML object or the path of its stored value.
xml_convert <- function(dot, out, hdr, jar, java, heap, opts = list()) {
  if (is.character(dot)) dot <- readRDS(dot)[[1]]
  options(opts)
  if (!is.null(dot$mi_header)) dot$mi_header <- hdr
  dot$xml_file <- out
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
  args <- c("-cp", shQuote(jar), heap, "ModelInterface.ModelGUI2.csvconv.CSVToXMLMain", tmpfn, shQuote(dot$mi_header), shQuote(dot$xml_file))
  rm(dot, table); invisible(gc())                                           # the tables are not needed while Java runs
  msgs <- system2(java, args, stdout = TRUE, stderr = TRUE)
  unlink(tmpfn)
  list(msgs = as.character(msgs), status = if (is.null(attr(msgs, "status"))) 0L else as.integer(attr(msgs, "status")))
}

# Background XML writers for driver_minaj(): up to option minaj.xml_workers files at once, each in a fresh R process
# (xml_convert). A file starts only while free memory, less what the running files may take, stays above
# minaj.ram_floor_gb plus what the file needs (its Java heap, or less for a file known to be small); the largest go first.
# With no file running, the smallest starts while free memory is above minaj.ram_floor_gb, as when files are written one
# at a time. xml_pool(start = TRUE): set up for the open tree, TRUE if used (minaj.xml_workers above 1 and the tree's
# writer is gcamdata's own code up to its Java heap, followed only by checks and messages). xml_pool(job): queue a file.
# xml_pool(): start and collect files. xml_pool(wait = TRUE): until all are done. xml_pool(stop = TRUE): stop the running files. Final statuses are in .nm$pool$status, by XML name.
xml_pool <- function(job = NULL, wait = FALSE, start = FALSE, stop = FALSE) {
  if (start) {
    .nm$pool <- NULL
    n <- nm_opt("xml_workers", 3L)
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 2) return(FALSE)
    find <- function(e, ok) {
      if (ok(e)) return(e)
      if (is.call(e)) for (k in seq_along(e)) { if (identical(e[[k]], quote(expr = ))) next; r <- find(e[[k]], ok); if (!is.null(r)) return(r) }
      NULL
    }
    is_for <- function(e) is.call(e) && identical(e[[1]], as.name("for"))
    is_args <- function(e) is.call(e) && identical(e[[1]], as.name("c")) && length(e) == 8L && identical(e[[2]], "-cp")
    # gcamdata's writer (GCAM 9.1, run_xml_conversion) without its comments
    ref <- utils::removeSource(quote({
      use_java <- getOption("gcamdata.use_java")
      if(!isTRUE(use_java) && !XML_WARNING_GIVEN) {
        message("Skipping XML conversion as global option gcamdata.use_java is not TRUE")
        XML_WARNING_GIVEN <<- TRUE
      } else if(isTRUE(use_java)) {
        java_cp <- system.file("extdata/ModelInterface", "CSVToXML.jar", package = "gcamdata")
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
        args <- c("-cp", shQuote(java_cp), "-Xmx2g", "ModelInterface.ModelGUI2.csvconv.CSVToXMLMain", tmpfn, shQuote(dot$mi_header), shQuote(dot$xml_file))
        warning_msgs <- system2("java", args, stdout = TRUE, stderr = TRUE)
        unlink(tmpfn)
        if(!is.null(warning_msgs) && length(warning_msgs) > 0) {
          warning(warning_msgs)
        }
      }
      invisible(dot)
    }))
    # A writer up to and including its Java call, the heap masked; the statements after the Java call go to `after`.
    is_java <- function(s) is.call(s) && identical(s[[1]], as.name("<-")) && is.call(s[[3]]) && identical(s[[3]][[1]], as.name("system2"))
    after <- list()
    to_java <- function(e) {
      if (is_args(e)) { e[[4]] <- quote(heap); return(e) }
      if (!is.call(e)) return(e)
      l <- as.list(e)
      if (identical(l[[1]], as.name("{")) && !is.na(k <- Position(is_java, l))) { after <<- c(after, l[-seq_len(k)]); l <- l[seq_len(k)] }
      for (i in seq_along(l)[-1]) if (!identical(l[[i]], quote(expr = ))) l[[i]] <- to_java(l[[i]])
      as.call(l)
    }
    # The functions a piece of code calls; "(call)" for a function that is not named.
    calls <- function(e) {
      if (!is.call(e)) return(character(0))
      h <- e[[1]]
      r <- if (is.name(h)) as.character(h) else if (is.call(h) && as.character(h[[1]]) %in% c("::", ":::")) as.character(h[[3]]) else "(call)"
      for (i in seq_along(e)[-1]) if (!identical(e[[i]], quote(expr = ))) r <- c(r, calls(e[[i]]))
      r
    }
    reports <- c("{", "(", "if", "<-", "=", "$", "[", "[[", "!", "&&", "||", "==", "!=", ">", "<", ">=", "<=", "c", "is.null", "is.na",
                 "length", "attr", "warning", "stop", "message", "paste", "paste0", "sprintf", "format", "tail", "basename", "file.exists")
    f <- tryCatch(utils::removeSource(get("run_xml_conversion", .nm$ns)), error = function(e) NULL)
    if (!is.function(f)) return(FALSE)
    a <- find(body(f), is_args)
    heap <- if (is.null(a)) NULL else tryCatch(eval(a[[4]], .nm$ns), error = function(e) NULL)
    mine <- to_java(body(f)); checks <- after[!vapply(after, identical, NA, quote(unlink(tmpfn)))]
    same <- identical(deparse(mine), deparse(to_java(ref))) && all(unlist(lapply(checks, calls)) %in% reports) &&
      identical(deparse(find(body(f), is_for)), deparse(find(body(utils::removeSource(xml_convert)), is_for))) &&
      is.character(heap) && length(heap) == 1L && grepl("^-Xmx[0-9]+[kKmMgG]?$", heap) &&
      identical(a[[5]], "ModelInterface.ModelGUI2.csvconv.CSVToXMLMain") &&
      is.null(getOption("gcamdata.xml.XML_PREFIX")) && is.null(getOption("gcamdata.xml.XML_SUFFIX"))
    if (!isTRUE(same)) { message("minaj: XML files are written one at a time: this tree's XML writer differs from minaj's copy"); return(FALSE) }
    v <- as.numeric(sub("^-Xmx([0-9]+).*$", "\\1", heap)); u <- tolower(sub("^-Xmx[0-9]+", "", heap))
    heap_gb <- v / switch(u, k = 2^20, m = 2^10, g = 1, 2^30)
    .nm$pool <- list2env(list(n = as.integer(n), heap = heap, heap_gb = heap_gb, java = unname(Sys.which("java")),
                              jar = normalizePath(file.path(.nm$root, "inst", "extdata", "ModelInterface", "CSVToXML.jar"), "/"),
                              hdr = normalizePath(file.path(.nm$root, "inst", "extdata", "mi_headers", "ModelInterface_headers.txt"), "/"),
                              opts = list(scipen = getOption("scipen", 0L)), queue = list(), run = list(), status = list()))
    return(TRUE)
  }
  p <- .nm$pool
  if (is.null(p)) return(invisible(FALSE))
  if (stop) {
    for (r in p$run) { try(r$proc$kill_tree(), silent = TRUE); unlink(r$job$out) }
    .nm$pool <- NULL
    return(invisible(TRUE))
  }
  if (!is.null(job)) {                                                       # Java peaks near 4 times the XML size, up to its heap
    big <- p$heap_gb + 0.15
    job$gb <- 0.3 + if (is.na(job$prev_gb)) big else min(big, 0.3 + 4 * job$prev_gb)
    p$queue[[length(p$queue) + 1L]] <- job
  }
  floor <- nm_opt("ram_floor_gb", 4)
  repeat {
    for (k in rev(seq_along(p$run))) {                                       # collect finished files
      r <- p$run[[k]]
      if (r$proc$is_alive()) next
      p$run[[k]] <- NULL
      res <- tryCatch(r$proc$get_result(), error = function(e) e)
      if (inherits(res, "error")) try(r$proc$kill_tree(), silent = TRUE)    # a Java left behind by a killed worker
      p$status[[r$job$name]] <- tryCatch(if (inherits(res, "error")) r$job$redo() else r$job$done(res),   # a failed worker: once more in this process
                                         error = function(e) paste("FAILED:", conditionMessage(e)))
    }
    while (length(p$queue) && length(p$run) < p$n) {                        # start files while memory allows
      left <- sum(vapply(p$run, function(r) r$job$gb, 0))                   # what the running files may take (listing their processes takes seconds on Windows)
      free <- ps::ps_system_memory()$avail / 2^30
      gb <- vapply(p$queue, `[[`, 0, "gb")
      fit <- which(gb <= free - left - floor)
      if (!length(fit)) {
        if (length(p$run) || free >= floor) { if (!length(p$run)) fit <- which.min(gb) else break }   # nothing running: start the smallest above the floor
        else if (wait) {                                                     # under the floor with nothing running: as in this process
          for (j in p$queue) p$status[[j$name]] <- sprintf("FAILED: %s not written: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb); try again later", j$name, free, floor)
          p$queue <- list(); break
        } else break
      }
      k <- fit[which.max(gb[fit])]; j <- p$queue[[k]]; p$queue[[k]] <- NULL
      proc <- tryCatch(callr::r_bg(xml_convert, args = list(dot = j$obj, out = j$out, hdr = p$hdr, jar = p$jar, java = p$java, heap = p$heap, opts = p$opts),
                                   stdout = NULL, stderr = NULL, supervise = TRUE), error = function(e) e)
      if (inherits(proc, "error")) p$status[[j$name]] <- tryCatch(j$redo(), error = function(e) paste("FAILED:", conditionMessage(e)))
      else p$run[[length(p$run) + 1L]] <- list(proc = proc, job = j)
    }
    if (!wait || (!length(p$queue) && !length(p$run))) break
    Sys.sleep(0.2)
  }
  invisible(TRUE)
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
  txt <- gsub("(?s)<!--.*?-->", "", txt, perl = TRUE)
  vals <- regmatches(txt, gregexpr("<Value[^>]*>[^<]*</Value>", txt))[[1]]
  vals <- trimws(sub("^<Value[^>]*>([^<]*)</Value>$", "\\1", vals))
  vals <- vals[grepl("gcamdata/xml[^/]*/[^/]+\\.xml$", vals)]
  unique(normalizePath(ifelse(grepl("^([A-Za-z]:)?[/\\\\]", vals), vals, file.path(dirname(config), vals)), "/", mustWork = FALSE))
}
