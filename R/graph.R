# The chunk graph: which data each chunk reads and writes. Declarations are asked of each chunk once
# and kept on disk per R file content, so a new session reads the graph in well under a second.

graph_file <- function() file.path(.nm$state_dir, "graph.rds")

declare <- function(chunks) {
  ns <- .nm$ns
  ci <- get("chunk_inputs", ns)(chunks)
  co <- get("chunk_outputs", ns)(chunks)
  list(ci = as.data.frame(ci, stringsAsFactors = FALSE), co = as.data.frame(co, stringsAsFactors = FALSE))
}

graph_refresh <- function(files = NULL, rfiles = .nm$rfiles, rstamp = .nm$rstamp) {
  g <- if (file.exists(graph_file())) tryCatch(readRDS(graph_file()), error = function(e) NULL) else NULL
  if (is.null(g)) g <- list(per_file = list())
  enabled <- get("find_chunks", .nm$ns)()$name
  todo <- character(0)
  for (f in names(rfiles)) {
    pf <- g$per_file[[f]]
    defs <- .nm$file_defs[[f]]; chunks <- intersect(grep("^module_", defs, value = TRUE), enabled)
    same <- !is.null(pf) && identical(pf$hash, unname(rfiles[f])) && !(f %in% files) && identical(sort(pf$chunks), sort(chunks))
    dynamic <- if (same && !is.null(pf$dynamic)) pf$dynamic else any(grepl("list.files(", readLines(file.path(.nm$root, "R", f), warn = FALSE), fixed = TRUE))
    if (!same || dynamic) {
      d <- if (length(chunks)) declare(chunks) else list(ci = NULL, co = NULL)
      new <- list(hash = unname(rfiles[f]), stamp = unname(rstamp[f]), defs = defs, dynamic = dynamic, chunks = chunks, ci = d$ci, co = d$co)
      if (!identical(new, pf)) { g$per_file[[f]] <- new; todo <- c(todo, f) }
    } else if (!identical(pf$stamp, unname(rstamp[f]))) { g$per_file[[f]]$stamp <- unname(rstamp[f]); todo <- c(todo, f) }
  }
  g$per_file <- g$per_file[intersect(names(g$per_file), names(rfiles))]
  ci <- do.call(rbind, lapply(g$per_file, `[[`, "ci")); co <- do.call(rbind, lapply(g$per_file, `[[`, "co"))
  ci <- ci[ci$name %in% enabled, , drop = FALSE]; co <- co[co$name %in% enabled, , drop = FALSE]
  rownames(ci) <- NULL; rownames(co) <- NULL
  if (anyDuplicated(co$output)) stop("outputs declared by more than one chunk: ", paste(unique(co$output[duplicated(co$output)]), collapse = ", "))
  if (length(todo)) save_rds(g, graph_file(), level = 0L)
  .nm$ci <- ci; .nm$co <- co
  .nm$producer <- setNames(co$name, co$output)
  # a CSV that one chunk requires and another marks optional is required (as in driver_drake)
  fi <- ci[!ci$input %in% co$output, , drop = FALSE]
  .nm$optional <- tapply(fi$optional, fi$input, all)
  .nm$by_chunk <- split(ci$input, ci$name)
  .nm$out_by_chunk <- split(co$output, co$name)
  .nm$consumers <- split(ci$name, ci$input)
  invisible(length(todo))
}

csv_inputs <- function() names(.nm$optional)
is_csv <- function(name) name %in% names(.nm$optional)
chunk_of <- function(name) if (name %in% names(.nm$producer)) .nm$producer[[name]] else NULL

# Accept a unique suffix such as "energy_L223.electricity" for "module_energy_L223.electricity".
chunk_name <- function(x) {
  all <- unique(.nm$co$name)
  if (x %in% all) return(x)
  hit <- grep(paste0("_", gsub(".", "\\.", x, fixed = TRUE), "$"), all, value = TRUE)
  if (length(hit) != 1) stop("chunk '", x, "' ", if (length(hit)) paste("is ambiguous:", paste(hit, collapse = ", ")) else "not found")
  hit
}

upstream_chunks <- function(chunks) {
  seen <- chunks; frontier <- chunks
  while (length(frontier)) {
    ins <- unlist(.nm$by_chunk[frontier], use.names = FALSE)
    nxt <- setdiff(unique(unname(.nm$producer[ins[ins %in% names(.nm$producer)]])), seen)
    seen <- c(seen, nxt); frontier <- nxt
  }
  seen
}

downstream_chunks <- function(from) {
  ci <- .nm$ci; co <- .nm$co
  seen <- intersect(from, unique(co$name))
  frontier <- unique(c(from[!from %in% seen], co$output[co$name %in% seen]))
  while (length(frontier)) {
    nxt <- setdiff(unique(ci$name[ci$input %in% frontier]), seen)
    seen <- c(seen, nxt); frontier <- co$output[co$name %in% nxt]
  }
  seen
}

#' Chunks and XML files that an edit to these CSVs, data objects or chunks reaches
#'
#' @param from CSV input names, data names or full chunk names.
#' @return A list: chunks (chunk names) and xml (XML file names).
#' @export
downstream_minaj <- function(from) {
  need_open()
  ch <- downstream_chunks(from)
  list(chunks = ch, xml = .nm$co$output[.nm$co$name %in% ch & .nm$co$to_xml])
}

# Resolve a user target: XML file name, data name, CSV name, or chunk name.
target_kind <- function(x) {
  if (x %in% .nm$co$output[.nm$co$to_xml]) return("xml")
  if (x %in% .nm$co$output) return("data")
  if (is_csv(x)) return("csv")
  r <- tryCatch(chunk_name(x), error = function(e) e)
  if (!inherits(r, "error")) return("chunk")
  if (grepl("ambiguous", conditionMessage(r))) stop(conditionMessage(r), call. = FALSE)
  stop("'", x, "' is not an XML file, data object, CSV input or chunk of this tree", call. = FALSE)
}
