# Compare XML files across folders: byte-identical (md5) first; for files that differ, a streaming line
# comparison that (1) treats "<?xml ...?><scenario>" on one line the same as on two lines (Java versions
# differ in this), (2) requires the non-numeric text to match exactly, (3) compares numbers by relative difference.
# Usage (from the package root): Rscript tests/compare_xml_dirs.R <out.csv> <label=dir> <label=dir> [<label=dir> ...]   (first dir is the reference)
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 3) stop("usage: Rscript tests/compare_xml_dirs.R <out.csv> <label=dir> <label=dir> [<label=dir> ...]", call. = FALSE)
out <- a[1]; dirs <- a[-1]
lab <- sub("=.*$", "", dirs); dirs <- setNames(sub("^[^=]*=", "", dirs), lab)
files <- sort(unique(unlist(lapply(dirs, function(d) list.files(d, pattern = "\\.xml$")))))
num_re <- "-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?"

# Reader of logical XML lines: indentation removed, an element wrapped over several lines joined into one
# (Java versions wrap long elements differently), a declaration glued to the first element split off.
reader <- function(f, chunk = 50000L) {
  con <- file(f, "r"); first <- TRUE; buf <- character(0); done <- FALSE; carry <- ""
  join <- function(x) {                              # a logical line ends with '>'; vectorised, carries a partial line to the next chunk
    x <- trimws(x)
    if (nzchar(carry)) { x <- c(carry, x); carry <<- "" }
    n <- length(x); if (!n) return(x)
    e <- grepl(">$", x)
    if (!e[n]) { k <- max(c(0L, which(e))); carry <<- paste(x[(k + 1L):n], collapse = " "); x <- x[seq_len(k)]; e <- e[seq_len(k)]; n <- k }
    if (!n || all(e)) return(x)
    g <- cumsum(c(1L, as.integer(e[-n])))         # a new logical line starts after each line ending with '>'
    keep <- !duplicated(g); res <- x[keep]; gk <- g[keep]
    multi <- g %in% g[duplicated(g)]
    pasted <- vapply(split(x[multi], g[multi]), paste, character(1), collapse = " ")
    res[match(as.integer(names(pasted)), gk)] <- pasted
    res
  }
  list(take = function(n) {
         while (!done && length(buf) < n) {
           x <- readLines(con, n = chunk, warn = FALSE)
           if (!length(x)) { done <<- TRUE; if (nzchar(carry)) { buf <<- c(buf, carry); carry <<- "" }; break }
           if (first) { first <<- FALSE; if (grepl("^<\\?xml[^>]*\\?>.+$", x[1])) x <- c(sub("^(<\\?xml[^>]*\\?>).*$", "\\1", x[1]), sub("^<\\?xml[^>]*\\?>", "", x[1]), x[-1]) }
           buf <<- c(buf, join(x))
         }
         k <- min(n, length(buf)); r <- buf[seq_len(k)]; buf <<- buf[-seq_len(k)]; r
       },
       close = function() close(con))
}

cmp_lines <- function(f1, f2, chunk = 50000L) {
  r1 <- reader(f1); r2 <- reader(f2); on.exit({ r1$close(); r2$close() })
  maxrel <- 0; ntxt <- 0L; ndiff <- 0L; n1 <- 0L; n2 <- 0L
  repeat {
    x <- r1$take(chunk); y <- r2$take(chunk); n1 <- n1 + length(x); n2 <- n2 + length(y)
    if (!length(x) && !length(y)) break
    if (length(x) != length(y)) return(list(text_same = FALSE, maxrel = NA, ndiff = NA, note = sprintf("line counts differ (%d vs %d+)", n1, n2)))
    d <- which(x != y)
    for (i in d) {
      if (gsub(num_re, "#", x[i]) != gsub(num_re, "#", y[i])) { ntxt <- ntxt + 1L; next }
      nx <- as.numeric(regmatches(x[i], gregexpr(num_re, x[i]))[[1]]); ny <- as.numeric(regmatches(y[i], gregexpr(num_re, y[i]))[[1]])
      rel <- abs(nx - ny) / pmax(abs(nx), abs(ny), 1e-12); rel[nx == ny] <- 0; maxrel <- max(maxrel, rel)
    }
    ndiff <- ndiff + length(d)
  }
  list(text_same = ntxt == 0L, maxrel = maxrel, ndiff = ndiff, note = if (ntxt) sprintf("%d lines differ in text", ntxt) else "")
}

ref <- lab[1]
rows <- lapply(files, function(f) {
  p <- setNames(file.path(dirs, f), lab); ex <- setNames(file.exists(p), lab)
  m <- setNames(ifelse(ex, unname(tools::md5sum(p)), NA), lab)
  r <- data.frame(file = f, stringsAsFactors = FALSE)
  for (k in lab[-1]) {
    col <- paste0(k, "_vs_", ref)
    if (!ex[[ref]] || !ex[[k]]) { r[[col]] <- if (!ex[[ref]]) "missing in reference" else "missing"; next }
    if (identical(m[[ref]], m[[k]])) { r[[col]] <- "identical"; next }
    t <- cmp_lines(p[[ref]], p[[k]])
    r[[col]] <- if (!isTRUE(t$text_same)) paste("TEXT DIFFERS:", t$note)
                else if (t$ndiff == 0) "same content (formatting only)"
                else sprintf("numbers differ: max rel %.2e in %d lines", t$maxrel, t$ndiff)
  }
  r
})
res <- do.call(rbind, rows); write.csv(res, out, row.names = FALSE)
for (k in setdiff(names(res), "file")) {
  v <- res[[k]]; num <- grepl("^numbers", v)
  cat(sprintf("%s: %d files | byte-identical %d | same content %d | numbers only %d (worst rel %s) | text differs %d | missing %d\n", k, length(v),
              sum(v == "identical"), sum(grepl("^same content", v)), sum(num),
              if (any(num)) format(max(as.numeric(sub(".*max rel ([0-9.e+-]+) .*", "\\1", v[num]))), digits = 3) else "-",
              sum(grepl("^TEXT", v)), sum(grepl("missing", v))))
}
