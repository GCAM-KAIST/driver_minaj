# Provenance, in the spirit of gcamdata's dstrace() but from the live chunk graph and the stored results
# (dstrace reads a data map saved in the package, which goes stale after edits). Table-level lineage
# upstream or downstream, plus a value search: which rows of which upstream tables (CSV files included)
# hold a given number.

# Every input the producing chunk reads (the declared "precursors" attribute is often incomplete: for
# L223.StubTechCapFactor_elec it omits the capacity-factor CSV the chunk reads). Declared ones are marked.
feeders <- function(name, ctx) {
  ch <- chunk_of(name); if (is.null(ch)) return(list(inputs = character(0), declared = character(0)))
  ins <- .nm$by_chunk[[ch]]; if (is.null(ins)) ins <- character(0)
  pc <- character(0)
  h <- tryCatch(value_hash(name, ctx), error = function(e) NA_character_)
  if (!is.na(h) && obj_exists(h)) pc <- as.character(attr(obj_get(h, ctx)[[1]], "precursors"))
  list(inputs = ins, declared = intersect(ins, pc))
}

trace_lineage <- function(name, direction = c("upstream", "downstream"), depth = 6L) {
  direction <- match.arg(direction)
  ctx <- new_ctx(dry = TRUE)
  edges <- list(); seen <- character(0); frontier <- name; level <- 0L
  while (length(frontier) && level < depth) {
    nxt <- character(0)
    for (x in setdiff(frontier, seen)) {
      seen <- c(seen, x)
      if (direction == "upstream") {
        fd <- feeders(x, ctx)
        for (i in fd$inputs) { edges[[length(edges) + 1L]] <- list(from = i, to = x, chunk = chunk_of(x), kind = if (is_csv(i)) "csv" else "data", declared_precursor = i %in% fd$declared); nxt <- c(nxt, i) }
      } else {
        for (ch in unique(.nm$consumers[[x]])) for (o in .nm$out_by_chunk[[ch]]) {
          edges[[length(edges) + 1L]] <- list(from = x, to = o, chunk = ch, kind = if (o %in% .nm$co$output[.nm$co$to_xml]) "xml" else "data"); nxt <- c(nxt, o)
        }
      }
    }
    frontier <- unique(nxt[!nxt %in% seen]); level <- level + 1L
  }
  nodes <- unique(c(name, unlist(lapply(edges, `[[`, "from")), unlist(lapply(edges, `[[`, "to"))))
  list(name = name, direction = direction, depth = depth, edges = edges,
       csv = sort(nodes[vapply(nodes, is_csv, logical(1))]),
       xml = sort(intersect(nodes, .nm$co$output[.nm$co$to_xml])),
       truncated = length(frontier) > 0)
}

# Rows of `tables` (data or CSV names) whose numeric cells are within tol of value, optionally filtered by
# column = value pairs (e.g. technology = "PV").
value_hits <- function(tables, value, filter = list(), tol = 1e-9, max_hits = 25L) {
  ctx <- new_ctx(); hits <- list()
  for (t in tables) {
    h <- tryCatch(value_hash(t, new_ctx(dry = TRUE)), error = function(e) NA_character_)
    if (is.na(h) || !obj_exists(h)) next
    d <- obj_get(h, ctx)[[1]]; if (!is.data.frame(d)) next
    sel <- rep(TRUE, nrow(d))
    for (f in names(filter)) if (f %in% names(d)) sel <- sel & as.character(d[[f]]) %in% as.character(filter[[f]])
    if (!any(sel)) next
    for (v in setdiff(names(d)[vapply(d, is.numeric, logical(1))], "year")) {
      w <- which(sel & !is.na(d[[v]]) & abs(d[[v]] - value) <= tol * max(1, abs(value)))
      for (r in utils::head(w, max_hits - length(hits))) {
        keys <- names(d)[!vapply(d, is.numeric, logical(1)) | names(d) == "year"]
        hits[[length(hits) + 1L]] <- c(list(table = t, csv = is_csv(t), column = v), as.list(d[r, keys, drop = FALSE]))
      }
      if (length(hits) >= max_hits) break
    }
    if (length(hits) >= max_hits) break
  }
  hits
}

# XML element (ModelInterface header) of each table in an XML file.
xml_headers <- function(xml) {
  h <- tryCatch(value_hash(xml, new_ctx(dry = TRUE)), error = function(e) NA_character_)
  if (is.na(h) || !obj_exists(h)) return(NULL)
  x <- obj_get(h)[[1]]
  vapply(x$data_tables, function(t) t$header, character(1))
}
