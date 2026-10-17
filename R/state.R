# Session state, opening a tree, and the checks every call runs under.

.nm <- new.env(parent = emptyenv())

nm_opt <- function(name, default) getOption(paste0("minaj.", name), default)

horizon_flag <- function() toupper(Sys.getenv("GCAM_EXTEND_HORIZON", "FALSE")) == "TRUE"

#' Open a gcamdata tree for this R session
#'
#' Loads gcamdata from the tree with pkgload::load_all (once per session) and reads or builds the
#' chunk graph. All later calls work on this tree.
#' @param tree GCAM tree (folder holding input/gcamdata) or the gcamdata folder itself.
#' @param store Folder for stored results, shared by all trees (default: option minaj.store, else
#'   file.path(tools::R_user_dir("minaj", "cache"), "minaj_store")). Created if missing.
#' @param dest Folder (relative to the gcamdata folder, or absolute) where make_minaj writes XML files
#'   (default: option minaj.dest, else "xml_minaj").
#' @return Invisibly, the list info_minaj() returns.
#' @importFrom stats setNames
#' @importFrom utils head
#' @export
open_minaj <- function(tree, store = nm_opt("store", file.path(tools::R_user_dir("minaj", "cache"), "minaj_store")),
                       dest = nm_opt("dest", "xml_minaj")) {
  tree <- normalizePath(tree, "/", mustWork = TRUE)
  root <- if (dir.exists(file.path(tree, "input", "gcamdata"))) file.path(tree, "input", "gcamdata") else tree
  if (!file.exists(file.path(root, "DESCRIPTION"))) stop("no gcamdata package found at ", root)
  root <- normalizePath(root, "/")
  loaded <- if ("gcamdata" %in% loadedNamespaces()) normalizePath(getNamespaceInfo("gcamdata", "path"), "/", mustWork = FALSE) else NA
  same_session <- identical(loaded, root) && identical(.nm$root, root) && !is.null(.nm$rfiles) && identical(.nm$ns, asNamespace("gcamdata"))
  flag_ok <- !identical(loaded, root) || {
    f <- get0("modeltime.EXTEND_HORIZON", envir = asNamespace("gcamdata"), inherits = FALSE)
    is.null(f) || identical(isTRUE(f), horizon_flag())
  }
  if (same_session && flag_ok) {                       # reopening: pick up edits saved since the last call
    .nm$dest <- dest
    with_dir(root, sync_code())
    return(invisible(info_minaj()))
  }
  if (!is.na(loaded)) pkgload::unload("gcamdata")
  message("minaj: loading gcamdata from ", root, " (once per session)")
  with_dir(root, suppressMessages(pkgload::load_all(root, quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)))
  if (!identical(normalizePath(getNamespaceInfo("gcamdata", "path"), "/"), root)) stop("gcamdata is loaded from another folder")
  if (!is.null(.nm$state_dir)) try(save_findex(), silent = TRUE)
  .nm$root <- root
  .nm$tree <- normalizePath(dirname(dirname(root)), "/")
  .nm$ns <- asNamespace("gcamdata")
  .nm$store <- normalizePath(store, "/", mustWork = FALSE)
  .nm$dest <- dest
  .nm$tree_id <- tree_id(root)
  .nm$state_dir <- file.path(.nm$store, "trees", .nm$tree_id)
  for (d in c("obj", "rec", "xml", file.path("trees", .nm$tree_id))) dir.create(file.path(.nm$store, d), recursive = TRUE, showWarnings = FALSE)
  forget_code()
  .nm$findex <- NULL; .nm$findex_dirty <- FALSE; .nm$secs <- NULL; .nm$rfiles <- NULL; .nm$rstamp <- NULL; .nm$watch <- NULL
  ensure_java()
  with_dir(root, { sync_code(initial = TRUE); graph_refresh() })
  .nm$env_key <- env_key()
  i <- info_minaj()
  message(sprintf("minaj: %s | %d chunks, %d XML | XML files go to %s", .nm$tree, i$chunks, i$xml, i$dest))
  invisible(i)
}

#' What minaj is working on
#'
#' @return A list: tree, gcamdata (folder), store, dest (XML folder), and the counts chunks, xml and
#'   csv_inputs.
#' @export
info_minaj <- function() {
  need_open()
  list(tree = .nm$tree, gcamdata = .nm$root, store = .nm$store, dest = dest_dir(),
       chunks = length(unique(.nm$co$name)), xml = sum(.nm$co$to_xml), csv_inputs = length(csv_inputs()))
}

need_open <- function() if (is.null(.nm$root)) stop("run open_minaj(\"<tree>\") first")

tree_id <- function(root) {
  tree <- basename(dirname(dirname(root)))
  paste0(gsub("[^A-Za-z0-9_.-]", "_", tree), "_", substr(digest::digest(root, algo = "xxhash64", serialize = FALSE), 1, 6))
}

dest_dir <- function(dest = .nm$dest) {
  d <- if (grepl("^([A-Za-z]:)?[/\\\\]", dest)) dest else file.path(.nm$root, dest)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  normalizePath(d, "/")
}

with_dir <- function(dir, expr) { owd <- setwd(dir); on.exit(setwd(owd)); force(expr) }

# User paths are resolved against the caller's folder, before enter() moves into the gcamdata folder.
user_path <- function(p) normalizePath(p, "/", mustWork = TRUE)

# Every public call: work from the gcamdata folder, pick up saved R edits, clear XML name options.
enter <- function() {
  need_open()
  if (!"gcamdata" %in% loadedNamespaces() || !identical(asNamespace("gcamdata"), .nm$ns) ||
      !identical(normalizePath(getNamespaceInfo("gcamdata", "path"), "/", mustWork = FALSE), .nm$root))
    stop("gcamdata was reloaded or unloaded outside minaj; run open_minaj(\"", .nm$tree, "\") again")
  owd <- setwd(.nm$root)
  op <- options(gcamdata.xml.XML_SUFFIX = NULL, gcamdata.xml.XML_PREFIX = NULL)
  restore <- function() { options(op); setwd(owd) }
  tryCatch(sync_code(), error = function(e) { restore(); stop(e) })
  restore
}

# on.exit handler for public calls: restore the session even if saving the file index fails.
leave <- function(restore) { try(save_findex(), silent = TRUE); restore() }

ensure_java <- function() {
  if (nzchar(Sys.which("java"))) return(invisible(TRUE))
  cands <- c(nm_opt("java_bin", character(0)),
             Sys.glob("C:/Program Files/RedHat/java-1.8.0-openjdk*/bin"),
             Sys.glob("C:/Program Files/Eclipse Adoptium/*/bin"),
             Sys.glob("C:/Program Files/Java/*/bin"))
  cands <- cands[file.exists(file.path(cands, "java.exe"))]
  if (!length(cands)) { warning("minaj: java not found; XML files cannot be written"); return(invisible(FALSE)) }
  Sys.setenv(PATH = paste(cands[1], Sys.getenv("PATH"), sep = .Platform$path.sep))
  invisible(TRUE)
}

# Objects in the global environment that a chunk could pick up through exists("<name>") (default
# inherits = TRUE) are moved aside while it runs: data names, names the chunk spells out, *_nesting tables.
stash_globals <- function(chunk) {
  g <- globalenv(); have <- ls(g, all.names = TRUE)
  if (!length(have)) return(function() invisible())
  b <- body(get(chunk, envir = .nm$ns))
  risky <- c(names(.nm$producer), csv_inputs(), all.names(b), lang_strings(b))
  hit <- have[have %in% risky | grepl("_nesting$", have)]
  hit <- hit[!vapply(hit, function(n) is.function(get(n, envir = g)), logical(1))]
  if (!length(hit)) return(function() invisible())
  saved <- mget(hit, envir = g); rm(list = hit, envir = g)
  loud <- hit[hit %in% c(names(.nm$producer), csv_inputs(), lang_strings(b)) | grepl("_nesting$", hit)]
  if (length(loud)) message("minaj: moved aside global objects named like data while ", chunk, " runs: ", paste(head(loud, 8), collapse = ", "), if (length(loud) > 8) " ...")
  function() list2env(saved, envir = g)
}
