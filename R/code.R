# Code fingerprints and hot reloading of saved R files.
#
# A chunk's code key covers the chunk function, every gcamdata function it reaches (transitively,
# through names in its body and string literals such as get("x")), the VALUE of every constant or
# package data object those functions name (including data/*.rda such as PREBUILT_DATA), and the content
# of files some functions read directly (see file_deps). Comments and formatting do not count.

h64 <- function(x) digest::digest(x, algo = "xxhash64")
h64s <- function(s) digest::digest(s, algo = "xxhash64", serialize = FALSE)

lazy_env <- function() .nm$ns$.__NAMESPACE__.$lazydata

ns_names <- function() {
  if (is.null(.nm$ns_names)) {
    lz <- lazy_env()
    .nm$ns_names <- unique(c(grep("^\\.__", ls(.nm$ns, all.names = TRUE), value = TRUE, invert = TRUE),
                             if (is.environment(lz)) ls(lz, all.names = TRUE)))
  }
  .nm$ns_names
}

ns_get <- function(n) {
  if (exists(n, envir = .nm$ns, inherits = FALSE)) return(get(n, envir = .nm$ns, inherits = FALSE))
  get(n, envir = lazy_env(), inherits = FALSE)
}

fn_text <- function(f) paste(deparse(f, control = c("keepNA", "keepInteger", "niceNames", "showAttributes")), collapse = "\n")

# Files that some names read directly (outside find_csv_file), relative to the gcamdata folder. Their
# content enters the code key of every chunk that reaches the name. Extend with option minaj.file_deps.
file_deps_table <- function() {
  c(list(xml_header_index = "inst/extdata/mi_headers/ModelInterface_headers.txt",
         create_xml = "inst/extdata/mi_headers/ModelInterface_headers.txt",
         module_energy_transportation_BEVratios = c("inst/extdata/energy/ATB_vehicles.csv", "inst/extdata/energy/mappings/ATB_UCD_mapping.csv")),
    nm_opt("file_deps", list()))
}

file_dep_hash <- function(rel) {
  p <- file.path(.nm$root, rel)
  if (file.exists(p)) file_hash(normalizePath(p, "/")) else "absent"
}

# Hash of one namespace object: function text, or the value of anything else, plus its file deps.
name_hash <- function(n) {
  h <- .nm$fn_hash[[n]]
  if (!is.null(h)) return(h)
  v <- ns_get(n)
  h <- if (is.function(v)) h64s(fn_text(v)) else h64(v)
  fd <- file_deps_table()[[n]]
  if (length(fd)) h <- h64(c(h, vapply(fd, file_dep_hash, character(1))))
  .nm$fn_hash[[n]] <- h
  h
}

lang_strings <- function(x) {
  if (is.character(x)) return(x)
  if (is.call(x) || is.pairlist(x) || is.expression(x) || (is.list(x) && !is.data.frame(x))) return(unlist(lapply(as.list(x), lang_strings), use.names = FALSE))
  character(0)
}

# Namespace names one object refers to directly.
name_deps <- function(n) {
  d <- .nm$deps[[n]]
  if (!is.null(d)) return(d)
  v <- ns_get(n)
  d <- if (is.function(v) && !is.primitive(v)) {
    fm <- as.list(formals(v)); b <- body(v)
    intersect(unique(c(all.names(b), unlist(lapply(fm, function(z) if (is.language(z)) all.names(z))), lang_strings(b), lang_strings(fm))), ns_names())
  } else character(0)
  .nm$deps[[n]] <- setdiff(d, n)
  .nm$deps[[n]]
}

# Everything an object reaches, itself included.
reach <- function(n) {
  r <- .nm$reach_memo[[n]]
  if (!is.null(r)) return(r)
  seen <- n; frontier <- n
  while (length(frontier)) {
    nxt <- setdiff(unique(unlist(lapply(frontier, name_deps), use.names = FALSE)), seen)
    seen <- c(seen, nxt); frontier <- nxt
  }
  .nm$reach_memo[[n]] <- sort(seen)
  .nm$reach_memo[[n]]
}

code_key <- function(n) {
  k <- .nm$key_memo[[n]]
  if (!is.null(k)) return(k)
  r <- reach(n)
  k <- h64(paste0(r, "=", vapply(r, name_hash, character(1))))
  .nm$key_memo[[n]] <- k
  k
}

# The CSV reader and the XML writer have their own keys: a change to either re-reads / re-writes.
reader_key <- function() code_key("load_csv_files")
writer_key <- function() {
  if (is.null(.nm$writer_key)) {
    files <- c("inst/extdata/mi_headers/ModelInterface_headers.txt", "inst/extdata/ModelInterface/CSVToXML.jar")
    nm <- intersect(c("run_xml_conversion", "make_run_xml_conversion", "set_xml_file_helper"), ns_names())
    .nm$writer_key <- h64(list(vapply(nm, code_key, character(1)), vapply(files, file_dep_hash, character(1))))
  }
  .nm$writer_key
}

# R version, versions of the packages gcamdata imports or calls (DESCRIPTION Imports, NAMESPACE imports,
# readr/vroom for CSV parsing), the NAMESPACE file itself, and the GCAM_EXTEND_HORIZON environment variable
# (empty unless a tree uses it).
env_key <- function() {
  desc <- tryCatch(read.dcf(file.path(.nm$root, "DESCRIPTION"), fields = "Imports")[1, 1], error = function(e) "")
  dimp <- trimws(sub("\\(.*$", "", strsplit(gsub("\n", " ", desc), ",")[[1]]))
  imps <- setdiff(unique(c(names(getNamespaceImports(.nm$ns)), dimp, "readr", "vroom", "vctrs", "tidyselect")), c("base", "", NA))
  v <- vapply(imps, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "NA"), character(1))
  nsf <- file.path(.nm$root, "NAMESPACE")
  h64(list(R = R.version.string, pkgs = sort(paste(imps, v)), namespace = if (file.exists(nsf)) digest::digest(file = nsf, algo = "xxhash64") else "",
           horizon = Sys.getenv("GCAM_EXTEND_HORIZON")))
}

forget_code <- function(names = NULL) {
  if (is.null(names)) { .nm$fn_hash <- list(); .nm$deps <- list() }
  else { .nm$fn_hash[intersect(names, names(.nm$fn_hash))] <- NULL; .nm$deps[intersect(names, names(.nm$deps))] <- NULL }
  .nm$key_memo <- list(); .nm$reach_memo <- list(); .nm$writer_key <- NULL; .nm$ns_names <- NULL
}

# ---- hot reload -------------------------------------------------------------------------------------

# Content hashes of R/*.R; a file is re-hashed only when its size or modification time changed.
r_files <- function() {
  f <- list.files(file.path(.nm$root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
  info <- file.info(f, extra_cols = FALSE); b <- basename(f)
  stamp <- setNames(paste(info$size, format(as.numeric(info$mtime), digits = 15)), b)
  h <- vapply(seq_along(f), function(i) {
    if (!is.null(.nm$rstamp) && identical(unname(.nm$rstamp[b[i]]), unname(stamp[i])) && !is.na(.nm$rfiles[b[i]])) unname(.nm$rfiles[b[i]])
    else digest::digest(file = f[i], algo = "xxhash64")
  }, character(1))
  .nm$rstamp_new <- stamp
  setNames(h, b)
}

# Files outside R/*.R whose change matters: package data and NAMESPACE/DESCRIPTION need a reload;
# the others (headers, jar, extra file deps) only invalidate keys.
watched_files <- function() {
  rel_reload <- c("NAMESPACE", "DESCRIPTION", "R/sysdata.rda", file.path("data", list.files(file.path(.nm$root, "data"), pattern = "\\.(rda|RData)$")))
  rel_keys <- unique(c("inst/extdata/mi_headers/ModelInterface_headers.txt", "inst/extdata/ModelInterface/CSVToXML.jar", unlist(file_deps_table(), use.names = FALSE)))
  list(reload = rel_reload, keys = rel_keys)
}

watch_stamps <- function(rel) {
  p <- file.path(.nm$root, rel); i <- file.info(p, extra_cols = FALSE)
  setNames(ifelse(is.na(i$size), "absent", paste(i$size, format(as.numeric(i$mtime), digits = 15))), rel)
}

# Top-level names a file assigns.
file_names <- function(path) {
  ex <- tryCatch(parse(path, keep.source = FALSE), error = function(e) expression())
  nm <- vapply(ex, function(e) {
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && length(e) >= 2 && is.name(e[[2]])) as.character(e[[2]]) else NA_character_
  }, character(1))
  unique(nm[!is.na(nm)])
}

reload_package <- function(why) {
  message("minaj: ", why, ": reloading gcamdata (once)")
  suppressMessages(pkgload::load_all(.nm$root, quiet = TRUE, helpers = FALSE, attach_testthat = FALSE))
  .nm$ns <- asNamespace("gcamdata")
  forget_code()
  now <- r_files(); .nm$rstamp <- .nm$rstamp_new
  defs <- lapply(setNames(names(now), names(now)), function(f) file_names(file.path(.nm$root, "R", f)))
  .nm$file_defs <- defs
  graph_refresh(files = names(now), rfiles = now, rstamp = .nm$rstamp)
  .nm$rfiles <- now
  .nm$env_key <- env_key()
  w <- watched_files(); .nm$watch <- watch_stamps(c(w$reload, w$keys))
  invisible(TRUE)
}

# Re-source R files saved since the last call, straight into the loaded namespace (no package reload).
# New or deleted top-level names, removed files, and changes to NAMESPACE, DESCRIPTION or package data
# need a full load_all, which is done automatically.
sync_code <- function(initial = FALSE) {
  w <- watched_files()
  if (initial) {                                     # seed hashes and parsed names from the graph cache
    g <- if (file.exists(graph_file())) tryCatch(readRDS(graph_file()), error = function(e) NULL) else NULL
    pf <- if (is.null(g)) list() else g$per_file
    .nm$rfiles <- vapply(pf, function(x) if (is.null(x$hash)) NA_character_ else x$hash, character(1))
    .nm$rstamp <- vapply(pf, function(x) if (is.null(x$stamp)) NA_character_ else x$stamp, character(1))
    now <- r_files(); .nm$rstamp <- .nm$rstamp_new
    .nm$file_defs <- lapply(setNames(names(now), names(now)), function(f) {
      x <- pf[[f]]
      if (!is.null(x) && identical(x$hash, unname(now[f])) && !is.null(x$defs)) x$defs else file_names(file.path(.nm$root, "R", f))
    })
    .nm$rfiles <- now
    .nm$watch <- watch_stamps(c(w$reload, w$keys))
    return(invisible(character(0)))
  }
  ws <- watch_stamps(c(w$reload, w$keys))
  wchg <- names(ws)[is.na(.nm$watch[names(ws)]) | .nm$watch[names(ws)] != ws]
  if (any(wchg %in% w$reload)) return(reload_package(paste("changed", paste(intersect(wchg, w$reload), collapse = ", "))))
  if (length(wchg)) {                                # header, jar or an extra file dep changed: drop memoised keys
    hx <- get0("xml_header_index", envir = .nm$ns, inherits = FALSE)
    if (is.function(hx) && exists("cache", envir = environment(hx), inherits = FALSE)) assign("cache", NULL, envir = environment(hx))
    forget_code(names(Filter(function(fd) any(fd %in% wchg), file_deps_table())))
    .nm$watch <- ws
  }
  now <- r_files(); stamp_new <- .nm$rstamp_new
  changed <- names(now)[is.na(.nm$rfiles[names(now)]) | .nm$rfiles[names(now)] != now]
  gone <- setdiff(names(.nm$rfiles), names(now))
  if (!length(changed) && !length(gone)) { .nm$rstamp <- stamp_new; return(invisible(character(0))) }
  defs <- lapply(setNames(changed, changed), function(f) file_names(file.path(.nm$root, "R", f)))
  new_names <- setdiff(unlist(defs, use.names = FALSE), ns_names())
  old_names <- unlist(.nm$file_defs[changed], use.names = FALSE)
  still <- unlist(c(defs, .nm$file_defs[setdiff(names(.nm$file_defs), changed)]), use.names = FALSE)
  dropped <- setdiff(old_names, still)
  if (length(gone) || length(new_names) || length(dropped) || any(grepl("^(zzz|onload|aaa)", changed, ignore.case = TRUE)))
    return(reload_package(if (length(new_names)) paste0("new names in R/ (", paste(head(new_names, 5), collapse = ", "), ")")
                          else if (length(dropped)) paste0("names removed from R/ (", paste(head(dropped, 5), collapse = ", "), ")")
                          else "files added or removed in R/"))
  ns <- .nm$ns
  pkg <- if ("package:gcamdata" %in% search()) as.environment("package:gcamdata") else NULL
  for (f in changed) {
    tmp <- new.env(parent = ns)
    for (e in parse(file.path(.nm$root, "R", f), keep.source = TRUE)) eval(e, tmp)
    for (n in ls(tmp, all.names = TRUE)) {
      v <- get(n, envir = tmp)
      if (is.function(v) && identical(environment(v), tmp)) environment(v) <- ns
      rebind(ns, n, v)
      if (!is.null(pkg) && exists(n, envir = pkg, inherits = FALSE)) rebind(pkg, n, v)
    }
    message("minaj: re-sourced R/", f)
  }
  forget_code(unlist(defs, use.names = FALSE))
  old_defs <- .nm$file_defs; .nm$file_defs[changed] <- defs
  tryCatch(graph_refresh(files = changed, rfiles = now, rstamp = stamp_new), error = function(e) { .nm$file_defs <- old_defs; stop(e) })
  .nm$rfiles <- now; .nm$rstamp <- stamp_new          # only after the graph accepted the new code
  invisible(changed)
}

rebind <- function(env, n, v) {
  locked <- exists(n, envir = env, inherits = FALSE) && bindingIsLocked(n, env)
  if (locked) unlockBinding(n, env)
  assign(n, v, envir = env)
  if (locked) lockBinding(n, env)
}

# Temporarily replace one namespace binding while evaluating expr.
with_binding <- function(n, v, expr) {
  ns <- .nm$ns; old <- get(n, envir = ns, inherits = FALSE)
  rebind(ns, n, v); on.exit(rebind(ns, n, old))
  force(expr)
}
