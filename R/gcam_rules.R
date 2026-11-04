# The GCAM XML parse registry: how the C++ parses each XML tag inside each class, one row per (parent class, tag),
# read from a GCAM cvs/objects tree. addon_minaj reads the copy in inst/extdata/gcam_parse_rules.csv.
#
# Columns:
#   parent_class   class (or context, below) whose element holds the tag; "" for the document root
#   tag            XML tag (also the attribute name for SIMPLE fields, which attributes can set too)
#   kind           VEC_NAMED, VEC_YEAR, PERIOD, SINGLE, SIMPLE, ARRAY, RULE, KEYWORD, REPLACE, OR, KEYMAP, SETTRUE,
#                  STUB_QUEUE, GTD_LOCATION, ONCE, APPEND
#   creates        class or context of the element's children; "" for leaves
#   key_attribute  attribute that finds an existing element or entry (";" between several); "" if none
#   value_type     C++ type of a leaf value; the member type for containers
#   default        constructor value: a literal, "unset" for a Value never set, "" for an empty string or an absent
#                  container, a C++ expression otherwise, NA if not found
#   deletable      delete="1" on this element is honoured and may be written
#   order_policy   none, append (new entries go last), keyed (found by key, position fixed), sequence (every element
#                  appends; order is the content), critical (sibling order changes results; a family context takes it
#                  when any of its classes has it)
#   never_rebuild  addon_minaj never deletes or rebuilds the element (most files write into it)
#   source         file:line under cvs/objects ("; default file:line" when the default comes from a constructor)
#   tree_hash      hash of the parse code: every file a row cites, util/base/include/definitions.h, and every file that
#                  holds DEFINE_DATA, DEFINE_VARIABLE, XMLParse, getXMLNameStatic or TechVectorParseHelper; CR bytes
#                  dropped, paths in byte order
# Files under climate/source/hector/ (a separate library) and build/ are not read. Contexts that are not C++ classes:
# TechnologyContainer<V> (a technology element whose new periods are class V, chosen by the element's own tag),
# location-info, and ITechnology / IInterpolationFunction (the family's fields, kept where every class that has the
# field parses it alike, for elements whose class is decided later: stub replay, the interpolation-function name
# attribute). #if branches follow util/base/include/definitions.h.
# With out = a registry (data frame), returns the tree hash of src for that registry's rows instead.
gcam_rules_build <- function(src, out = NULL) {
  src <- normalizePath(src, "/", mustWork = TRUE)
  rel <- list.files(src, "\\.(h|hpp|cpp)$", recursive = TRUE)
  rel <- sort(rel[!grepl("^(climate/source/hector|build)/", rel)], method = "radix")
  if (!length(rel)) stop("no C++ files under ", src)
  tree_hash <- function(rows) {
    cited <- unique(unlist(regmatches(rows$source, gregexpr("[^ ;:]+\\.(h|hpp|cpp)(?=:)", rows$source, perl = TRUE))))
    h <- vapply(rel, function(r) {
      b <- readBin(file.path(src, r), "raw", file.size(file.path(src, r))); b <- b[b != as.raw(13L)]
      if (r %in% cited || r == "util/base/include/definitions.h" || length(grepRaw("DEFINE_DATA|DEFINE_VARIABLE|XMLParse|getXMLNameStatic|TechVectorParseHelper", b)))
        digest::digest(b, algo = "xxhash64", serialize = FALSE) else NA_character_
    }, "")
    h64s(paste(rel[!is.na(h)], h[!is.na(h)], collapse = "\n"))
  }
  if (is.data.frame(out)) return(tree_hash(out))
  rx <- function(p, x) gregexpr(p, x, perl = TRUE)[[1]]
  caps <- function(m, x, k) { s <- attr(m, "capture.start")[, k]; substring(x, s, s + attr(m, "capture.length")[, k] - 1L) }
  ev <- function(s, o, c) {
    m <- rx(sprintf("[%s%s]", o, c), s)
    if (m[1] < 0) return(list(p = integer(), d = integer()))
    p <- as.integer(m); list(p = p, d = cumsum(ifelse(substring(s, p, p) == o, 1L, -1L)))
  }
  close_of <- function(e, at) { k <- match(at, e$p); j <- which(e$d[-seq_len(k)] == e$d[k] - 1L)[1]; e$p[k + j] }
  rd <- function(f) if (file.size(f)) iconv(readChar(f, file.size(f), useBytes = TRUE), "latin1", "ASCII", sub = "?") else ""
  dh <- file.path(src, "util/base/include/definitions.h")
  mac <- if (file.exists(dh)) { d <- regmatches(rd(dh), gregexpr("(?m)^\\s*#\\s*define\\s+\\w+\\s+\\d+", rd(dh), perl = TRUE))[[1]]
                                setNames(as.numeric(sub(".*\\s", "", d)), sub("^\\s*#\\s*define\\s+(\\w+).*$", "\\1", d)) } else numeric()
  cond <- function(w, a) {
    a <- trimws(a); if (w == "ifdef") return(a %in% names(mac)); if (w == "ifndef") return(!a %in% names(mac))
    m <- gregexpr("defined\\s*\\(?\\s*\\w+\\s*\\)?", a)
    regmatches(a, m) <- list(ifelse(gsub("defined|[\\s()]", "", regmatches(a, m)[[1]], perl = TRUE) %in% names(mac), "1", "0"))
    m <- gregexpr("\\b[A-Za-z_]\\w*\\b", a, perl = TRUE); id <- regmatches(a, m)[[1]]
    regmatches(a, m) <- list(ifelse(id %in% names(mac), as.character(mac[id]), "0"))
    isTRUE(tryCatch(eval(parse(text = a), baseenv()) != 0, error = function(e) FALSE))
  }
  pp <- function(code, nostr) {                                  # blank lines in inactive #if branches (positions kept)
    lines <- function(x) { L <- strsplit(x, "\n", fixed = TRUE)[[1]]; if (endsWith(x, "\n")) c(L, "") else L }
    L <- lines(code); d <- which(grepl("^\\s*#\\s*(if|ifdef|ifndef|elif|else|endif)\\b", L))
    if (!length(d)) return(list(code, nostr))
    on <- TRUE; st <- list(); state <- logical(length(d))
    for (j in seq_along(d)) {
      w <- sub("^\\s*#\\s*(\\w+).*$", "\\1", L[d[j]]); a <- sub("^\\s*#\\s*\\w+", "", L[d[j]])
      if (w %in% c("if", "ifdef", "ifndef")) { c1 <- cond(w, a); st[[length(st) + 1L]] <- c(on, c1); on <- on && c1 }
      else if (length(st) && w == "elif") { tp <- st[[length(st)]]; c1 <- !tp[2] && cond("if", a); st[[length(st)]] <- c(tp[1], tp[2] || c1); on <- tp[1] && c1 }
      else if (length(st) && w == "else") { tp <- st[[length(st)]]; on <- tp[1] && !tp[2]; st[[length(st)]] <- c(tp[1], TRUE) }
      else if (length(st) && w == "endif") { on <- st[[length(st)]][1]; st[[length(st)]] <- NULL }
      state[j] <- on
    }
    keep <- rep(TRUE, length(L)); keep[-d] <- !rep(c(FALSE, !state), diff(c(0L, d, length(L) + 1L)) - 1L)
    lapply(list(L, lines(nostr)), function(x) { x[!keep] <- gsub(".", " ", x[!keep]); paste(x, collapse = "\n") })
  }
  cf <- lapply(rel, function(r) {
    s <- rd(file.path(src, r))
    code <- nostr <- s
    m <- rx("//[^\\n]*|/\\*[\\s\\S]*?\\*/|\"(?:\\\\.|[^\"\\\\\\n])*\"|'(?:\\\\.|[^'\\\\\\n])*'", s)
    if (m[1] > 0) {
      t <- regmatches(s, list(m))[[1]]; com <- startsWith(t, "/"); n <- nchar(t)
      rc <- t; rc[com] <- gsub("[^\n]", " ", t[com])
      rn <- rc; rn[!com] <- paste0(substr(t[!com], 1, 1), strrep(" ", n[!com] - 2L), substr(t[!com], n[!com], n[!com]))
      regmatches(code, list(m)) <- list(rc); regmatches(nostr, list(m)) <- list(rn)
    }
    x <- pp(code, nostr); code <- x[[1]]; nostr <- x[[2]]
    nl <- as.integer(rx("\n", s)); list(rel = r, code = code, nostr = nostr, nl = nl[nl > 0], br = ev(nostr, "{", "}"), pr = ev(nostr, "(", ")"))
  })
  at <- function(i, p) sprintf("%s:%d", cf[[i]]$rel, findInterval(p - 1, cf[[i]]$nl) + 1L)
  split_top <- function(s) {
    ch <- strsplit(s, "")[[1]]; d <- cumsum((ch %in% c("<", "(")) - (ch %in% c(">", ")"))); cut <- which(ch == "," & d == 0)
    trimws(substring(s, c(1L, cut + 1L), c(cut - 1L, nchar(s))))
  }
  bare <- function(x) { x <- gsub("\\b(public|protected|private|virtual)\\b", "", x); x <- trimws(sub("<.*$", "", x)); sub("^.*::", "", x) }
  flat <- function(x) trimws(gsub("\\s+", " ", x))

  # classes: name, file, body range, C++ bases
  cl <- do.call(rbind, lapply(seq_along(cf), function(i) {
    m <- rx("(?<![\\w])(?<!enum )(?:class|struct)\\s+(\\w+)\\s*(?:final\\s*)?(?::([^{};()]*))?\\{", cf[[i]]$code)
    if (m[1] < 0) return(NULL)
    ob <- as.integer(m) + attr(m, "match.length") - 1L
    data.frame(name = caps(m, cf[[i]]$code, 1), bases = caps(m, cf[[i]]$code, 2), file = i, open = ob,
               close = vapply(ob, function(o) close_of(cf[[i]]$br, o), 1L), stringsAsFactors = FALSE)
  }))
  owner <- function(i, p) { k <- which(cl$file == i & cl$open < p & cl$close > p); if (length(k)) cl$name[k[which.max(cl$open[k])]] else NA_character_ }
  bases <- lapply(split(cl$bases, cl$name), function(b) { b <- b[nzchar(trimws(b))]; if (length(b)) unique(bare(split_top(b[1]))) else character() })
  anc_memo <- list()
  anc <- function(k) {                                   # ancestors, nearest first
    if (!is.null(anc_memo[[k]])) return(anc_memo[[k]])
    b <- bases[[k]]; r <- unique(c(b, unlist(lapply(b, function(x) if (x %in% names(bases)) anc(x)))))
    anc_memo[[k]] <<- r; r
  }
  isa <- function(k, b) k == b | b %in% anc(k)

  # DEFINE_DATA blocks and their DEFINE_VARIABLE lines
  vars <- list(); blk <- list(); bad <- character()
  for (i in seq_along(cf)) {
    code <- cf[[i]]$code
    m <- rx("\\bDEFINE_DATA(_WITH_PARENT)?\\s*\\(", code)
    if (m[1] < 0) next
    for (j in seq_along(m)) {
      p <- m[j]; ls <- max(c(0L, cf[[i]]$nl[cf[[i]]$nl < p]))
      if (grepl("^\\s*#", substr(code, ls + 1L, p))) next
      o <- p + attr(m, "match.length")[j] - 1L; cpos <- close_of(cf[[i]]$pr, o); inner <- substr(code, o + 1L, cpos - 1L)
      k <- owner(i, p); if (is.na(k)) next
      par <- if (nzchar(caps(m, code, 1)[j])) bare(split_top(sub("^\\s*([^,]*),[\\s\\S]*$", "\\1", inner, perl = TRUE))[1]) else ""
      fam <- regmatches(inner, regexec("DEFINE_SUBCLASS_FAMILY\\s*\\(([^)]*)\\)", inner))[[1]]
      blk[[k]] <- list(parent = par, family = if (length(fam)) bare(split_top(flat(fam[2]))) else character(), at = at(i, p))
      v <- rx("\\bDEFINE_VARIABLE\\s*\\(", inner)
      if (v[1] < 0) { vars[[k]] <- NULL; next }
      vo <- o + as.integer(v) + attr(v, "match.length") - 1L
      args <- vapply(vo, function(q) flat(substr(code, q + 1L, close_of(cf[[i]]$pr, q) - 1L)), "")
      a <- regmatches(args, regexec('^([^,]+?)\\s*,\\s*(?:"([^"]*)"|(\\w+)::getXMLNameStatic\\s*\\(\\s*(?:void\\s*)?\\))\\s*,\\s*(\\w+)\\s*,\\s*(.+)$', args))
      ok <- lengths(a) == 6L; if (!all(ok)) bad <- c(bad, at(i, o + as.integer(v)[!ok])); a <- a[ok]
      vars[[k]] <- data.frame(flags = vapply(a, `[`, "", 2), tag = vapply(a, function(x) if (nzchar(x[4])) paste0("@", x[4]) else x[3], ""),
                              var = vapply(a, `[`, "", 5), type = vapply(a, `[`, "", 6), source = at(i, o + as.integer(v)[ok]), stringsAsFactors = FALSE)
    }
  }
  has_blk <- function(k) !is.null(blk[[k]])
  blk_of <- function(k) { if (has_blk(k)) return(k); a <- anc(k); a <- a[vapply(a, has_blk, NA)]; if (length(a)) a[1] else NA_character_ }
  full_memo <- list()
  full <- function(k) {                                  # full data vector: root class first (first tag match wins)
    if (!is.null(full_memo[[k]])) return(full_memo[[k]])
    b <- blk[[k]]; own <- vars[[k]]; if (is.null(own)) own <- data.frame(flags = character(), tag = character(), var = character(), type = character(), source = character())
    r <- if (nzchar(b$parent) && !is.na(blk_of(b$parent))) rbind(full(blk_of(b$parent)), own) else own
    full_memo[[k]] <<- r; r
  }
  root_of <- function(k) { k <- blk_of(k); while (!is.na(k) && nzchar(blk[[k]]$parent)) k <- blk_of(blk[[k]]$parent); k }
  family <- function(k) { r <- root_of(k); if (is.na(r)) character() else blk[[r]]$family }

  # XML names (getXMLNameStatic), abstract classes, XMLParse overrides
  xn <- list()
  for (i in seq_along(cf)) {
    code <- cf[[i]]$code
    m <- rx("(?:\\b(\\w+)::)?getXMLNameStatic\\s*\\(\\s*(?:void\\s*)?\\)\\s*(?:const\\s*)?\\{", code)
    if (m[1] < 0) next
    k <- caps(m, code, 1)
    for (j in seq_along(m)) {
      if (!nzchar(k[j])) k[j] <- owner(i, m[j]); if (is.na(k[j])) next
      o <- m[j] + attr(m, "match.length")[j] - 1L; body <- substr(code, o, close_of(cf[[i]]$br, o))
      lit <- regmatches(body, regexpr('"[^"]*"', body))
      if (!length(lit)) {
        al <- regmatches(body, regexec("return\\s+(\\w+)::getXMLNameStatic", body))[[1]]
        if (length(al)) { xn[[k[j]]] <- list(alias = al[2]); next }
        fn <- regmatches(body, regexec("return\\s+(\\w+)\\s*\\(\\s*\\)\\s*;", body))[[1]]
        if (length(fn)) {                                    # name returned by a helper function in the same file
          q <- regexpr(sprintf("\\b%s\\s*\\(\\s*(?:void\\s*)?\\)\\s*\\{", fn[2]), code, perl = TRUE)
          if (q > 0) { q <- q + attr(q, "match.length") - 1L; lit <- regmatches(b2 <- substr(code, q, close_of(cf[[i]]$br, q)), regexpr('"[^"]*"', b2)) }
          if (length(lit)) { xn[[k[j]]] <- list(name = gsub('"', "", lit[1])); next }
        }
        id <- sub("^.*::", "", regmatches(body, regexec("return\\s+([\\w:]+)\\s*;", body, perl = TRUE))[[1]][2])
        if (is.na(id)) next
        lit <- regmatches(code, regexpr(sprintf("\\b(?:%s::)?%s\\s*(?:=|\\()\\s*\"[^\"]*\"", k[j], id), code, perl = TRUE))
        if (!length(lit)) for (cand in vapply(cf, `[[`, "", "code")) {
          lit <- regmatches(cand, regexpr(sprintf("\\b%s::%s\\s*(?:=|\\()\\s*\"[^\"]*\"", k[j], id), cand, perl = TRUE))
          if (length(lit)) break
        }
        lit <- sub('^[^"]*', "", lit)
      }
      if (length(lit)) xn[[k[j]]] <- list(name = gsub('"', "", lit[1]))
    }
  }
  own_name <- function(k) { x <- xn[[k]]; if (is.null(x)) NA_character_ else if (!is.null(x$alias)) own_name(x$alias) else x$name }
  xml_name <- function(k) { for (a in c(k, anc(k))) if (!is.na(n <- own_name(a))) return(n); NA_character_ }
  decl <- lapply(split(seq_len(nrow(cl)), cl$name), function(r) {
    r <- r[1]; b <- substr(cf[[cl$file[r]]]$nostr, cl$open[r] + 1L, cl$close[r] - 1L)
    while (grepl("\\{[^{}]*\\}", b)) b <- gsub("\\{[^{}]*\\}", ";", b)
    arg <- "\\([^;{}()]*(?:\\([^;{}()]*\\)[^;{}()]*)*\\)"
    pv <- rx(sprintf("(~?\\b\\w+)\\s*%s\\s*(?:const\\s*)?(?:override\\s*)?=\\s*0\\s*;", arg), b)
    dv <- rx(sprintf("(~?\\b\\w+)\\s*%s\\s*(?:const\\s*)?(?:override\\s*)?(?:final\\s*)?(?:;|=\\s*default)", arg), b)
    list(pure = if (pv[1] > 0) grep("^~", caps(pv, b, 1), value = TRUE, invert = TRUE) else character(), def = if (dv[1] > 0) caps(dv, b, 1) else character())
  })
  unres_memo <- list()
  unres <- function(k) {
    if (!is.null(unres_memo[[k]])) return(unres_memo[[k]])
    u <- unique(unlist(lapply(bases[[k]], function(b) if (b %in% names(decl)) unres(b))))
    u <- union(setdiff(u, decl[[k]]$def), decl[[k]]$pure); unres_memo[[k]] <<- u; u
  }
  concrete <- function(k) k %in% names(decl) && !length(unres(k))
  ov <- do.call(rbind, lapply(seq_along(cf), function(i) {
    m <- rx("\\bbool\\s+(\\w+)::XMLParse\\s*\\(", cf[[i]]$code)
    if (m[1] < 0) NULL else data.frame(class = caps(m, cf[[i]]$code, 1), source = at(i, as.integer(m)), stringsAsFactors = FALSE)
  }))
  ov_of <- function(k) { for (a in c(k, anc(k))) if (a %in% ov$class) return(a); NA_character_ }
  body_of <- function(cls, fn) {
    for (i in seq_along(cf)) {
      m <- regexpr(sprintf("\\b%s::%s\\s*\\((?:[^;{}()]|\\([^()]*\\))*\\)\\s*(?:const\\s*)?\\{", cls, fn), cf[[i]]$code, perl = TRUE)
      if (m > 0) { o <- regexpr("\\{", substring(cf[[i]]$code, m)) + m - 1L; return(list(i = i, p = m, text = substr(cf[[i]]$code, o, close_of(cf[[i]]$br, o)))) }
    }
    NULL
  }

  # constructor defaults: member -> expression, from the default constructor and the no-argument functions it calls
  ctor_memo <- list()
  ctor <- function(k) {
    if (!is.null(ctor_memo[[k]])) return(ctor_memo[[k]])
    res <- list(); seen <- character()
    take <- function(i, txt, p0) {
      st <- txt; while (grepl("\\{[^{}]*\\}", substring(st, 2))) st <- paste0("{", gsub("\\{[^{}]*\\}", ";", substring(st, 2)))
      for (s in trimws(strsplit(substring(st, 2, nchar(st) - 1L), ";", fixed = TRUE)[[1]])) {
        a <- regmatches(s, regexec("^(\\w+)\\s*=\\s*([^=][\\s\\S]*)$", s, perl = TRUE))[[1]]
        if (length(a) && is.null(res[[a[2]]])) res[[a[2]]] <<- list(e = flat(a[3]), at = at(i, p0))
        cn <- regmatches(s, regexec("^(\\w+)\\s*\\(\\s*\\)$", s))[[1]]
        if (length(cn) && !cn[2] %in% seen) { seen <<- c(seen, cn[2]); b <- body_of(k, cn[2]); if (!is.null(b)) take(b$i, b$text, b$p) }
      }
    }
    for (i in seq_along(cf)) {
      code <- cf[[i]]$code
      m <- rx(sprintf("\\b%s::%s\\s*\\(", k, k), code)
      if (m[1] < 0) next
      for (p in as.integer(m)) {
        po <- p + regexpr("\\(", substring(code, p)) - 1L; pc <- close_of(cf[[i]]$pr, po); args <- flat(substr(code, po + 1L, pc - 1L))
        if (nzchar(args) && args != "void") next
        q <- regexpr("\\{", substring(code, pc)) + pc - 1L; ini <- substr(code, pc + 1L, q - 1L)
        if (grepl("^\\s*:", ini)) for (s in split_top(sub("^\\s*:", "", ini))) {
          a <- regmatches(s, regexec("^(\\w+)\\s*[({]([\\s\\S]*)[)}]$", s, perl = TRUE))[[1]]
          if (length(a) && is.null(res[[a[2]]])) res[[a[2]]] <- list(e = flat(a[3]), at = at(i, p))
        }
        take(i, substr(code, q, close_of(cf[[i]]$br, q)), p)
      }
    }
    ctor_memo[[k]] <<- res; res
  }
  dflt <- function(k, v, type, kind) {
    if (!kind %in% c("SIMPLE", "ARRAY")) return(list(e = "", at = ""))
    for (a in c(k, anc(k))) if (!is.null(d <- ctor(a)[[v]])) {
      e <- if (grepl("^Value\\s*\\(\\s*\\)$", d$e)) "unset" else sub("^(?:Value|gcamstr|std::string)\\s*\\(\\s*(.*?)\\s*\\)$", "\\1", d$e, perl = TRUE)
      if (type %in% c("gcamstr", "std::string", "string") && nzchar(e) && !grepl('^"[^"\\\\]*"$', e)) e <- NA_character_   # text: a double-quoted literal (or gcamstr()); an identifier or expression: not known
      return(list(e = sub('^"(.*)"$', "\\1", e), at = d$at))
    }
    e <- if (grepl("^(objects::)?(PeriodVector|TechVintageVector|YearVector)<\\s*Value\\s*>$|^Value$", type)) "unset"
         else if (grepl("^(objects::)?PeriodVector<\\s*(double|int)\\s*>$", type)) "0"
         else if (type %in% c("gcamstr", "std::string", "string")) "" else NA_character_
    list(e = e, at = "")
  }

  # technology tags (TechnologyContainer::hasTechnologyType) and the vintage class each one creates
  creatable <- function(t) {                             # tag -> class for a member of declared type t (Factory order)
    fam <- family(t); fam <- fam[vapply(fam, concrete, NA)]
    nm <- vapply(fam, xml_name, ""); keep <- !is.na(nm) & !duplicated(nm); fam <- fam[keep]; nm <- nm[keep]
    ok <- vapply(fam, isa, NA, b = t); setNames(fam[ok], nm[ok])
  }
  ht <- body_of("TechnologyContainer", "hasTechnologyType")
  tech_tags <- vapply(unique(regmatches(ht$text, gregexpr("\\w+(?=::getXMLNameStatic)", ht$text, perl = TRUE))[[1]]), xml_name, "")
  vint <- creatable("ITechnology"); vint <- vint[names(vint) %in% tech_tags]
  tc <- function(v) sprintf("TechnologyContainer<%s>", v)
  ht_at <- at(ht$i, ht$p)

  # keyword maps: the SIMPLE map type XMLParseHelper::parseData specialises (attributes inserted, first value kept)
  xh <- match("util/base/source/xml_parse_helper.cpp", rel)
  kw_type <- gsub("\\s", "", regmatches(cf[[xh]]$code, regexec("parseData<Data<(std::map<[^>]*>)\\s*,\\s*SIMPLE>\\s*>\\s*\\(", cf[[xh]]$code))[[1]][2])
  if (is.na(kw_type)) stop("gcam_rules_build: keyword map parser not found in util/base/source/xml_parse_helper.cpp")
  # generated rows: every concrete class with a data vector, first tag match wins
  cols <- c("parent_class", "tag", "kind", "creates", "key_attribute", "value_type", "default", "deletable", "order_policy", "never_rebuild", "source", "var")
  row <- function(...) { x <- list(...); x <- x[cols]; names(x) <- cols; x[vapply(x, is.null, NA)] <- ""; as.data.frame(x, stringsAsFactors = FALSE) }
  gen <- list(); skipped <- character()
  for (k in sort(unique(cl$name))) {
    if (!concrete(k) || is.na(blk_of(k))) next
    fv <- full(blk_of(k)); taken <- character()
    at_cls <- startsWith(fv$tag, "@"); fv$tag[at_cls] <- vapply(substring(fv$tag[at_cls], 2), xml_name, "")
    for (r in seq_len(nrow(fv))) {
      fl <- trimws(strsplit(fv$flags[r], "|", fixed = TRUE)[[1]]); if ("NOT_PARSABLE" %in% fl) next
      ty <- fv$type[r]; add <- NULL
      if ("CONTAINER" %in% fl) {
        sh <- if (grepl("^std::vector<", ty)) "vec" else if (grepl("^std::map<", ty)) "map" else if (grepl("\\*\\s*$", ty)) "single" else "other"
        t <- bare(sub("\\*\\s*>?\\s*$", "", sub("^std::(vector|map)<([^,]*,)?\\s*", "", ty)))
        if (sh %in% c("other", "map") || is.na(blk_of(t))) { skipped <- c(skipped, paste(k, fv$tag[r], ty)); next }
        tg <- if (t == "ITechnologyContainer") c(setNames(tc(vint), names(vint)), `stub-technology` = "StubTechnologyContainer") else creatable(t)
        if (!length(tg)) next
        kind <- if (sh == "single") "SINGLE" else if (isa(t, "INamed")) "VEC_NAMED" else if (isa(t, "IYeared")) "VEC_YEAR" else "APPEND"
        key <- c(SINGLE = "", VEC_NAMED = "name", VEC_YEAR = "year", APPEND = "")[[kind]]
        ord <- c(SINGLE = "none", VEC_NAMED = "append", VEC_YEAR = "append", APPEND = "sequence")[[kind]]
        add <- do.call(rbind, lapply(names(tg), function(g) row(parent_class = k, tag = g, kind = kind, creates = tg[[g]], key_attribute = key,
                                                               value_type = ty, deletable = kind != "APPEND", order_policy = ord, source = fv$source[r], var = fv$var[r])))
      } else {
        kind <- if ("ARRAY" %in% fl) "ARRAY" else if (gsub("\\s", "", ty) %in% kw_type) "KEYWORD" else "SIMPLE"
        vt <- if (kind == "ARRAY") flat(sub("^.*<\\s*([^<>]*?)[\\s>]+$", "\\1", ty)) else ty
        d <- dflt(k, fv$var[r], ty, kind)
        add <- row(parent_class = k, tag = fv$tag[r], kind = kind, key_attribute = if (kind == "ARRAY") "year" else "", value_type = vt,
                   default = if (kind == "KEYWORD") "" else d$e, deletable = FALSE, order_policy = "none",
                   source = paste0(fv$source[r], if (nzchar(d$at)) paste0("; default ", d$at)), var = fv$var[r])
      }
      add <- add[!add$tag %in% taken, , drop = FALSE]; taken <- c(taken, add$tag); gen[[length(gen) + 1L]] <- add
    }
  }
  gen <- do.call(rbind, gen)

  # hand-written rows for the custom XMLParse handlers; pattern locates the source line
  hand <- utils::read.csv(text = '
parent_class,tag,kind,creates,key_attribute,value_type,default,deletable,order_policy,never_rebuild,file,pattern
,scenario,SINGLE,Scenario,,Scenario*,,FALSE,none,TRUE,util/base/source/xml_parse_helper.cpp,"XMLParseHelper::parseData(doc.first_node(), root)"
Scenario,modeltime,ONCE,Modeltime,,const Modeltime*,,FALSE,none,TRUE,containers/source/scenario.cpp,nodeName == Modeltime::getXMLNameStatic()
Scenario,calc-base-price,APPEND,CalcBasePrice,,IModelFeedbackCalc*,,FALSE,sequence,FALSE,containers/source/scenario.cpp,mModelFeedbacks.push_back(currFeedback)
Modeltime,start-year,SIMPLE,,,int,,FALSE,none,TRUE,util/base/source/model_time.cpp,"nodeName == ""start-year"""
Modeltime,inter-year,APPEND,,,int,,FALSE,keyed,TRUE,util/base/source/model_time.cpp,"nodeName == ""inter-year"""
Modeltime,end-year,SIMPLE,,,int,,FALSE,none,TRUE,util/base/source/model_time.cpp,"nodeName == ""end-year"""
Modeltime,final-calibration-year,SIMPLE,,,int,,FALSE,none,TRUE,util/base/source/model_time.cpp,"nodeName == ""final-calibration-year"""
Modeltime,carbon-model-start-year,SIMPLE,,,int,,FALSE,none,TRUE,util/base/source/model_time.cpp,"nodeName == ""carbon-model-start-year"""
RegionMiniCAM,PrimaryFuelCO2Coef,KEYMAP,,name,double,,FALSE,keyed,FALSE,containers/source/region_minicam.cpp,mPrimaryFuelCO2Coef[
Subsector,interpolation-rule,RULE,InterpolationRule,,std::vector<InterpolationRule*>,,FALSE,sequence,FALSE,sectors/source/subsector.cpp,"attrs[""apply-to""] == ""share-weight"" && attrs[""delete""] == ""1"""
TechnologyContainer,period,PERIOD,ITechnology,year,"std::map<int, ITechnology*>",,TRUE,keyed,FALSE,technologies/source/technology_container.cpp,auto dataIter = mVintages.find(year);
TechnologyContainer,interpolation-rule,RULE,InterpolationRule,,std::vector<InterpolationRule*>,,FALSE,sequence,FALSE,technologies/source/technology_container.cpp,"attrs[""apply-to""] == ""share-weight"" && attrs[""delete""] == ""1"""
StubTechnologyContainer,name,SIMPLE,,,gcamstr,,FALSE,none,FALSE,technologies/include/stub_technology_container.h,"(mName, ""name"")"
StubTechnologyContainer,period,STUB_QUEUE,ITechnology,year,,,FALSE,keyed,FALSE,technologies/source/stub_technology_container.cpp,mXMLAdjustments.push_back( XMLParseHelper::deepClone(aNode) );
StubTechnologyContainer,interpolation-rule,STUB_QUEUE,InterpolationRule,,,,FALSE,sequence,FALSE,technologies/source/stub_technology_container.cpp,mXMLAdjustments.push_back( XMLParseHelper::deepClone(aNode) );
StubTechnologyContainer,initial-available-year,STUB_QUEUE,,,int,,FALSE,none,FALSE,technologies/source/stub_technology_container.cpp,mXMLAdjustments.push_back( XMLParseHelper::deepClone(aNode) );
StubTechnologyContainer,final-available-year,STUB_QUEUE,,,int,,FALSE,none,FALSE,technologies/source/stub_technology_container.cpp,mXMLAdjustments.push_back( XMLParseHelper::deepClone(aNode) );
GlobalTechnologyDatabase,location-info,GTD_LOCATION,location-info,sector-name;subsector-name,,,FALSE,keyed,TRUE,technologies/source/global_technology_database.cpp,"if( nodeName == ""location-info"" )"
EnergyInput,efficiency,REPLACE,,,double,,FALSE,none,FALSE,functions/source/energy_input.cpp,mCoefficient = new Efficiency(
EnergyInput,coefficient,REPLACE,,,double,,FALSE,none,FALSE,functions/source/energy_input.cpp,mCoefficient = new Intensity(
EnergyInput,flag,OR,,,std::string,0,FALSE,none,FALSE,functions/source/energy_input.cpp,setFlagsByName( XMLParseHelper::getValue<string>( aNode ) );
MACControl,mac-reduction,KEYMAP,,tax,double,,FALSE,keyed,FALSE,emissions/source/mac_control.cpp,mMacCurve[taxVal] = reductionVal;
MACControl,no-zero-cost-reductions,SETTRUE,,,bool,false,FALSE,none,FALSE,emissions/source/mac_control.cpp,mNoZeroCostReductions = true;
MACControl,tech-change,ARRAY,,year,double,0,FALSE,none,FALSE,emissions/source/mac_control.cpp,"nodeName == ""tech-change"""
ReadInControl,future-emiss-factor,ARRAY,,year,double,0,FALSE,none,FALSE,emissions/source/readin_control.cpp,"nodeName == ""future-emiss-factor"""
FractionalSecondaryOutput,fraction-produced,KEYMAP,,price,double,,FALSE,keyed,FALSE,technologies/source/fractional_secondary_output.cpp,mCostCurve[price] = fraction;
ResidueBiomassOutput,fract-harvested,KEYMAP,,price,double,,FALSE,keyed,FALSE,technologies/source/residue_biomass_output.cpp,mCostCurve[price] = fraction;
LandUseHistory,allocation,KEYMAP,,year,double,,FALSE,keyed,FALSE,land_allocator/source/land_use_history.cpp,mHistoricalLand[ year ] = XMLParseHelper::getValue<double>( aNode );
CalcBasePrice,sector-map,KEYMAP,,from-name,gcamstr,,FALSE,keyed,FALSE,containers/source/calc_base_price.cpp,mSectorNameMap[ fromSector ] = toSector;
CalcBasePrice,ignore-region,APPEND,,,gcamstr,,FALSE,keyed,FALSE,containers/source/calc_base_price.cpp,mRegionIgnoreList.insert(currRegion);
InterpolationRule,interpolation-function,REPLACE,IInterpolationFunction,name,IInterpolationFunction*,,FALSE,none,FALSE,util/base/source/interpolation_rule.cpp,mInterpolationFunction = tempFn;
InterpolationRule,overwrite-policy,SIMPLE,,,std::string,ALWAYS,FALSE,none,FALSE,util/base/source/interpolation_rule.cpp,"nodeName == ""overwrite-policy"""
NationalAccount,savings-rate,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( SAVINGS_RATE )
NationalAccount,materials-capital-stock,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( MATERIALS_CAPITAL_STOCK )
NationalAccount,materials-capital-value,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( MATERIALS_CAPITAL_VALUE )
NationalAccount,depreciation-rate,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( DEPRECIATION_RATE )
NationalAccount,GDP,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( GDP )
NationalAccount,materials-labor-wages,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( MATERIALS_LABOR_WAGES )
NationalAccount,materials-labor-force-share,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( MATERIALS_LABOR_FORCE_SHARE )
NationalAccount,capital-net-export,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( CAPITAL_NET_EXPORT )
NationalAccount,total-factor-productivity,SIMPLE,,,double,,FALSE,none,FALSE,containers/source/national_account.cpp,enumToXMLName( TOTAL_FACTOR_PRODUCTIVITY )
SolutionInfoParamParser,solution-tolerance,APPEND,,,double,,FALSE,sequence,TRUE,solution/util/source/solution_info_param_parser.cpp,"nodeName == ""solution-tolerance"""
SolutionInfoParamParser,solution-floor,APPEND,,,double,,FALSE,sequence,TRUE,solution/util/source/solution_info_param_parser.cpp,"nodeName == ""solution-floor"""
SolutionInfoParamParser,bracket-interval,APPEND,,,double,,FALSE,sequence,TRUE,solution/util/source/solution_info_param_parser.cpp,"nodeName == ""bracket-interval"""
SolutionInfoParamParser,max-price-change,APPEND,,,double,,FALSE,sequence,TRUE,solution/util/source/solution_info_param_parser.cpp,"nodeName == ""max-price-change"""
SolutionInfoParamParser,delta-price,APPEND,,,double,,FALSE,sequence,TRUE,solution/util/source/solution_info_param_parser.cpp,"nodeName == ""delta-price"""
BisectAll,solution-info-filter,REPLACE,,,std::string,,FALSE,none,TRUE,solution/solvers/source/bisect_all.cpp,"nodeName == ""solution-info-filter"""
LogBroyden,solution-info-filter,REPLACE,,,std::string,,FALSE,none,TRUE,solution/solvers/source/logbroyden.cpp,"nodeName == ""solution-info-filter"""
LogBroyden,linear-price,SIMPLE,,,bool,,FALSE,none,TRUE,solution/solvers/source/logbroyden.cpp,"nodeName == ""linear-price"""
LogBroyden,log-price,SIMPLE,,,bool,,FALSE,none,TRUE,solution/solvers/source/logbroyden.cpp,"nodeName == ""log-price"""
Preconditioner,solution-info-filter,REPLACE,,,std::string,,FALSE,none,TRUE,solution/solvers/source/preconditioner.cpp,"nodeName == ""solution-info-filter"""
', colClasses = "character", na.strings = character())
  exclusive <- c("Modeltime", "CalcBasePrice", "StubTechnologyContainer")     # their handler reads every child itself
  not_input <- c("BatchRunner", "Configuration", "LoggerFactory", "LoggerFactoryWrapper", "PolicyTargetRunner")
  ln_of <- function(f, pat) {
    i <- match(f, rel); if (is.na(i)) return(paste0(f, ":?"))
    p <- regexpr(pat, cf[[i]]$code, fixed = TRUE); if (p < 0) paste0(f, ":?") else at(i, p)
  }
  hand$source <- unname(mapply(ln_of, hand$file, hand$pattern)); hand$var <- ""
  miss <- endsWith(hand$source, ":?")
  if (any(miss)) warning("gcam_rules_build: code of these hand-written rows not found, rows left out (tags refused): ",
                         paste(hand$parent_class[miss], hand$tag[miss], hand$file[miss], collapse = "; "), call. = FALSE)
  hand <- hand[!miss, ]
  # solvers (SolverFactory::hasSolver) that the Solver family can create
  hs <- body_of("SolverFactory", "hasSolver"); sv <- creatable("Solver")
  sv <- sv[names(sv) %in% vapply(unique(regmatches(hs$text, gregexpr("\\w+(?=::getXMLNameStatic)", hs$text, perl = TRUE))[[1]]), xml_name, "")]
  hand <- rbind(hand[, cols], if (length(sv)) do.call(rbind, lapply(names(sv), function(g) row(parent_class = "Scenario", tag = g, kind = "APPEND", creates = sv[[g]],
                key_attribute = "year", value_type = "Solver*", deletable = FALSE, order_policy = "keyed", never_rebuild = TRUE, source = ln_of("containers/source/scenario.cpp", "SolverFactory::hasSolver( nodeName )")))))
  # hand rows apply to the handler's class and every concrete subclass that inherits the handler
  conc <- sort(unique(cl$name)); conc <- conc[vapply(conc, concrete, NA)]
  hx <- do.call(rbind, lapply(split(hand, hand$parent_class), function(h) {
    k <- h$parent_class[1]; if (!nzchar(k) || k %in% exclusive || !k %in% ov$class) return(h)
    tg <- conc[vapply(conc, function(a) identical(ov_of(a), k), NA)]
    do.call(rbind, lapply(tg, function(a) transform(h, parent_class = a)))
  }))
  gen <- gen[!gen$parent_class %in% exclusive & !paste(gen$parent_class, gen$tag) %in% paste(hx$parent_class, hx$tag), ]
  R <- rbind(gen, hx)
  # GTD location-info: technology tags only (no stubs)
  R <- rbind(R, do.call(rbind, lapply(names(vint), function(g) row(parent_class = "location-info", tag = g, kind = "VEC_NAMED", creates = tc(vint[[g]]),
                key_attribute = "name", value_type = "std::vector<ITechnologyContainer*>", deletable = TRUE, order_policy = "keyed",
                source = ln_of("technologies/source/global_technology_database.cpp", "TechnologyContainer::hasTechnologyType( innerNodeName )")))))
  # TechnologyContainer<V>: one context per vintage class; the period row creates V
  tcr <- R[R$parent_class == "TechnologyContainer", ]
  R <- rbind(R[R$parent_class != "TechnologyContainer", ], do.call(rbind, lapply(unique(vint), function(v) {
    x <- tcr; x$parent_class <- tc(v); x$creates[x$tag == "period"] <- v; x$source[x$tag == "period"] <- paste0(x$source[x$tag == "period"], "; class ", ht_at); x
  })))
  # order-critical vectors
  sub_of <- function(b) conc[vapply(conc, isa, NA, b = b)]
  R$order_policy[R$var == "mGrade" & R$parent_class %in% sub_of("SubResource")] <- "critical"
  R$order_policy[R$var == "mInputs" & R$parent_class %in% sub_of("AgStorageTechnology")] <- "critical"
  # family contexts: fields of the family's classes, kept where every class that has the field parses it the same way;
  # the strictest order policy of those classes
  for (fr in c("ITechnology", "IInterpolationFunction")) {
    mem <- unique(if (fr == "ITechnology") vint else creatable(fr)); x <- R[R$parent_class %in% mem, ]
    sig <- paste(x$kind, x$creates, x$key_attribute, ifelse(nzchar(x$creates), "", x$value_type), sep = "\r")   # containers: the class created, not the pointer type
    ok <- tapply(sig, x$tag, function(s) length(unique(s)) == 1L)
    crit <- tapply(x$order_policy == "critical", x$tag, any)
    x <- x[x$tag %in% names(ok)[ok], ]; x <- x[!duplicated(x$tag), ]; x$order_policy[crit[x$tag]] <- "critical"
    if (nrow(x)) { dd <- tapply(R$default[R$parent_class %in% mem], R$tag[R$parent_class %in% mem], function(d) if (length(unique(d)) == 1L) d[1] else NA)
                   x$default <- unname(dd[x$tag]); x$parent_class <- fr; R <- rbind(R, x) }
  }
  # keep what the <scenario> root reaches
  seen <- ""; repeat { nx <- setdiff(R$creates[R$parent_class %in% seen & nzchar(R$creates)], seen); if (!length(nx)) break; seen <- c(seen, nx) }
  R <- R[R$parent_class %in% seen, ]
  # never-rebuild set
  nr <- c("Scenario", "World", "GlobalTechnologyDatabase", "location-info", "LandAllocator", "Modeltime", "SolutionInfoParamParser",
          sub_of("Region"), sub_of("IClimateModel"), sub_of("Solver"), sub_of("SolverComponent"))
  R$never_rebuild <- R$never_rebuild == "TRUE" | R$creates %in% nr
  R$deletable <- R$deletable %in% c("TRUE", TRUE)
  if (length(bad)) warning("gcam_rules_build: DEFINE_VARIABLE lines not read: ", paste(bad, collapse = ", "), call. = FALSE)
  skipped <- skipped[sub(" .*$", "", skipped) %in% seen]
  if (length(skipped)) warning("gcam_rules_build: container members of an unhandled type (their tags are refused): ", paste(skipped, collapse = "; "), call. = FALSE)
  und <- setdiff(ov$class, c(hand$parent_class, not_input))
  if (length(und)) warning("gcam_rules_build: XMLParse overrides without hand-written rows (their tags are refused): ", paste(und, collapse = ", "), call. = FALSE)
  if (any(d <- duplicated(R[c("parent_class", "tag")]))) stop("gcam_rules_build: duplicate rows: ", paste(unique(paste(R$parent_class, R$tag)[d]), collapse = ", "))
  R <- R[order(R$parent_class, R$tag, method = "radix"), setdiff(cols, "var")]; R$tree_hash <- tree_hash(R); rownames(R) <- NULL
  if (!is.null(out)) {                                   # LF line ends on every platform
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE); tmp <- paste0(out, ".part")
    con <- file(tmp, "wb"); tryCatch(utils::write.csv(R, con, row.names = FALSE, na = "NA"), finally = close(con)); move_into_place(tmp, out)
  }
  invisible(R)
}
