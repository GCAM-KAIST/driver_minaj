# Compact differences between two versions of a gcamdata table: rows added / removed / changed, the
# largest changes, and which regions and years they fall in. Character columns and `year` identify rows;
# other numeric columns are values.

table_diff <- function(old, new, top = 10) {
  old <- as.data.frame(old, stringsAsFactors = FALSE); new <- as.data.frame(new, stringsAsFactors = FALSE)
  out <- list(rows_old = nrow(old), rows_new = nrow(new))
  if (!setequal(names(old), names(new))) {
    out$columns_added <- setdiff(names(new), names(old)); out$columns_removed <- setdiff(names(old), names(new))
    out$summary <- sprintf("columns differ: +%d / -%d", length(out$columns_added), length(out$columns_removed))
    return(out)
  }
  num <- names(new)[vapply(new, is.numeric, logical(1))]
  vals <- setdiff(num, "year"); keys <- setdiff(names(new), vals)
  key_of <- function(d) if (length(keys)) do.call(paste, c(lapply(d[keys], as.character), sep = "\r")) else as.character(seq_len(nrow(d)))
  ko <- key_of(old); kn <- key_of(new)
  if (anyDuplicated(ko) || anyDuplicated(kn)) {                        # rows not identified by keys: compare by position
    if (nrow(old) != nrow(new)) { out$summary <- sprintf("rows %d -> %d (rows have no unique key)", nrow(old), nrow(new)); return(out) }
    ko <- kn <- as.character(seq_len(nrow(new)))
  }
  m <- match(kn, ko); both <- which(!is.na(m))
  out$rows_added <- sum(is.na(m)); out$rows_removed <- nrow(old) - length(both)
  changes <- list(); cols <- list()
  for (v in vals) {
    o <- old[[v]][m[both]]; n <- new[[v]][both]
    na_change <- xor(is.na(o), is.na(n)); d <- n - o
    ch <- na_change | (!is.na(d) & d != 0)
    if (!any(ch)) next
    rel <- abs(d) / pmax(abs(o), 1e-12); rel[na_change] <- Inf          # relative to the old value
    cols[[v]] <- list(changed = sum(ch), max_rel = signif(max(rel[ch]), 4), max_abs = signif(max(abs(d[ch]), na.rm = TRUE), 4))
    idx <- both[ch]
    changes[[v]] <- data.frame(row = idx, column = v, old = o[ch], new = n[ch], rel = rel[ch], stringsAsFactors = FALSE)
  }
  out$value_columns_changed <- cols
  ch_all <- if (length(changes)) do.call(rbind, changes) else NULL
  out$rows_changed <- if (is.null(ch_all)) 0L else length(unique(ch_all$row))
  if (!is.null(ch_all)) {
    ch_all <- ch_all[order(-ch_all$rel), , drop = FALSE]
    t <- utils::head(ch_all, top)
    out$largest <- lapply(seq_len(nrow(t)), function(i) c(as.list(new[t$row[i], keys, drop = FALSE]),
                                                          list(column = t$column[i], old = signif(t$old[i], 6), new = signif(t$new[i], 6), rel = signif(t$rel[i], 3))))
    if ("region" %in% keys) { r <- sort(table(new$region[unique(ch_all$row)]), decreasing = TRUE); out$regions_changed <- length(r); out$regions_top <- as.list(utils::head(r, 10)) }
    if ("year" %in% keys) { y <- range(new$year[unique(ch_all$row)]); out$years_changed <- y }
  }
  out$summary <- if (is.null(ch_all) && !out$rows_added && !out$rows_removed) "identical values"
                 else sprintf("%d rows changed (%s), %d added, %d removed%s%s", out$rows_changed,
                              paste(sprintf("%s max %s", names(cols), vapply(cols, function(z) fmt_rel(z$max_rel), "")), collapse = "; "),
                              out$rows_added, out$rows_removed,
                              if (!is.null(out$regions_changed)) sprintf("; %d regions", out$regions_changed) else "",
                              if (!is.null(out$years_changed)) sprintf("; years %d-%d", out$years_changed[1], out$years_changed[2]) else "")
  out
}

fmt_rel <- function(r) if (!is.finite(r)) "NA<->value" else if (r >= 0.01) sprintf("%.1f%%", 100 * r) else sprintf("%.2g", r)

# Diff of any stored value: a table, or an XML object (its tables compared one by one).
value_diff <- function(old, new, top = 10) {
  o <- old[[1]]; n <- new[[1]]
  if (is.data.frame(o) && is.data.frame(n)) return(table_diff(o, n, top))
  if (is.list(o) && !is.null(o$data_tables)) {
    hdr <- function(x) vapply(x$data_tables, function(t) t$header, character(1))
    ho <- hdr(o); hn <- hdr(n)
    res <- lapply(seq_along(n$data_tables), function(i) {
      j <- match(hn[i], ho)
      if (is.na(j)) return(list(header = hn[i], summary = "table added"))
      d <- table_diff(o$data_tables[[j]]$data, n$data_tables[[i]]$data, top); d$header <- hn[i]; d
    })
    changed <- Filter(function(d) !identical(d$summary, "identical values"), res)
    return(list(summary = sprintf("%d of %d XML tables changed", length(changed), length(res)), tables = changed))
  }
  list(summary = if (identical(o, n)) "identical" else "values differ (not a table)")
}
