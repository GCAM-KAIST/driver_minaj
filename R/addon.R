# addon_minaj: the part of an override XML file that GCAM needs after its base file.

# Elements of an XML file, XML text or raw bytes, one row per element in document order: tag, depth (1 = top level),
# parent (row of the parent element, 0 at the top level), first and last (byte span of the element and its subtree),
# close (byte where its end tag starts, NA for <x/>), stag (start tag as written), text (leaf text as written, NA when
# the element has child elements, "" for <x/>), and one column "@<attribute>" per attribute name (value as written,
# NA where absent). Text and values are kept as raw bytes, as GCAM reads them (no entity translation). Comments and
# the XML declaration are skipped. Stops on anything else outside plain elements, double-quoted attributes and text:
# CDATA, DOCTYPE, other processing instructions, entities in attributes other than the five XML ones, repeated
# attributes, text beside child elements, comments inside a leaf, unbalanced or mismatched tags.
xml_tokens <- function(file_or_text) {
  x <- file_or_text; src <- "XML text"
  b <- if (is.raw(x)) x else if (is.character(x) && length(x) == 1 && !is.na(x) && !grepl("<", x, fixed = TRUE)) {
    src <- user_path(x); readBin(src, "raw", file.size(src))
  } else if (is.character(x)) charToRaw(paste(enc2utf8(x), collapse = "\n")) else stop("xml_tokens: give a file path, XML text or raw bytes", call. = FALSE)
  n <- length(b)
  bad <- function(at, what) stop(sprintf("%s, line %d (byte %d): %s; only elements, double-quoted attributes, text, comments and the XML declaration are read",
                                         src, sum(b[seq_len(at)] == as.raw(10L)) + 1L, at, what), call. = FALSE)
  show <- function(t) { t <- iconv(rawToChar(charToRaw(t)), "UTF-8", "UTF-8", sub = "byte"); if (nchar(t) > 80) paste0(substr(t, 1, 77), "...") else t }
  g <- function(re, v) grepl(re, v, perl = TRUE, useBytes = TRUE)
  nm <- "[A-Za-z_:][-A-Za-z0-9_:.]*"; ws <- "[ \t\r\n]"; val <- '"[^"<&]*(?:&(?:amp|lt|gt|quot|apos);[^"<&]*)*"'
  if (length(z <- grepRaw(as.raw(0L), b, fixed = TRUE))) bad(z[1], "NUL byte")
  s <- rawToChar(b); Encoding(s) <- "bytes"                                  # byte offsets throughout
  m <- gregexpr("(?s)<!--.*?-->|<[^>]*>", s, perl = TRUE, useBytes = TRUE)[[1]]
  if (m[1] < 0) stop(src, ": no elements", call. = FALSE)
  p <- as.integer(m); e <- p + attr(m, "match.length") - 1L
  tok <- substring(s, p, e)
  gap <- substring(s, c(1L, e + 1L), c(p - 1L, n))                          # gap[i] precedes tok[i]
  if (n >= 3 && identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) gap[1] <- substring(gap[1], 4L)
  c2 <- substr(tok, 2L, 2L)
  if (any(i <- c2 == "!")) {
    i <- which(i)[!g("(?s)^<!--.*-->$", tok[i])]
    if (length(i)) bad(p[i[1]], if (startsWith(tok[i[1]], "<![CDATA[")) "CDATA section" else if (startsWith(tok[i[1]], "<!DOCTYPE")) "DOCTYPE" else paste("markup", show(tok[i[1]])))
  }
  if (any(i <- c2 == "?")) {
    i <- which(i)[!(which(i) == 1L & g("(?s)^<\\?xml[ \t\r\n].*\\?>$", tok[i]) & !g("[^ \t\r\n]", gap[1]))]
    if (length(i)) bad(p[i[1]], paste("processing instruction", show(tok[i[1]])))
  }
  f <- which(c2 != "!" & c2 != "?")                                          # start, empty and end tags, document order
  if (!length(f)) stop(src, ": no elements", call. = FALSE)
  u <- unique(tok[f]); ui <- match(tok[f], u)                                # checks and names on distinct tag texts
  ue <- substr(u, 2L, 2L) == "/"
  ok <- logical(length(u)); ok[ue] <- g(paste0("^</", nm, ws, "*>$"), u[ue])
  ok[!ue] <- g(paste0("^<", nm, "(?:", ws, "+", nm, ws, "*=", ws, "*", val, ")*", ws, "*/?>$"), u[!ue])
  if (!all(ok)) {
    i <- f[which(!ok[ui])[1]]; t <- tok[i]
    bad(p[i], if (startsWith(t, "</")) paste("malformed end tag", show(t)) else paste(
      if (grepl("&", t, fixed = TRUE)) "entity other than &amp; &lt; &gt; &quot; &apos; in" else if (grepl("'", t, fixed = TRUE)) "single quotes in" else
        if (nchar(gsub('[^"]', "", t, useBytes = TRUE), "bytes") %% 2) "unterminated value or '>' inside a value in" else "malformed start tag", show(t)))
  }
  tyu <- ifelse(ue, -1L, ifelse(endsWith(u, "/>"), 0L, 1L)); ty <- tyu[ui]   # 1 start, 0 empty, -1 end
  tg <- substring(u, ifelse(ue, 3L, 2L), attr(regexpr(paste0("^</?", nm), u, perl = TRUE, useBytes = TRUE), "match.length"))[ui]
  d <- cumsum(ty)
  if (any(d < 0L)) { i <- which(d < 0L)[1]; bad(p[f[i]], paste0("</", tg[i], "> without a start tag")) }
  if (d[length(d)] != 0L) { i <- max(which(ty == 1L & d == d[length(d)])); bad(p[f[i]], paste0("<", tg[i], "> is never closed")) }
  dep <- d + (ty <= 0L)
  S <- which(ty == 1L); E <- which(ty == -1L)
  oS <- S[order(dep[S], method = "radix")]; oE <- E[order(dep[E], method = "radix")]   # i-th start and i-th end at each depth pair up
  if (any(i <- tg[oS] != tg[oE])) { j <- which(i)[which.min(oE[i])]; bad(p[f[oE[j]]], paste0("</", tg[oE[j]], "> closes <", tg[oS[j]], ">")) }
  mate <- integer(length(f)); mate[oS] <- oE
  el <- which(ty >= 0L); nel <- length(el)
  row <- integer(length(f)); row[el] <- seq_len(nel)
  kS <- dep[S] * 2^31 + p[f[S]]; oK <- order(kS)                             # parent = last open start one level up
  j <- findInterval((dep[el] - 1) * 2^31 + p[f[el]], kS[oK])
  parent <- integer(nel); parent[j > 0L] <- row[S[oK][j[j > 0L]]]
  lf <- el[ty[el] == 1L & mate[el] == el + 1L]
  if (any(i <- f[mate[lf]] != f[lf] + 1L)) bad(p[f[lf[which(i)[1]]]], paste0("comment inside leaf <", tg[lf[which(i)[1]]], ">"))
  text <- rep(NA_character_, nel); text[row[lf]] <- gap[f[lf] + 1L]; text[ty[el] == 0L] <- ""
  chk <- rep(TRUE, length(gap)); chk[f[lf] + 1L] <- FALSE; chk <- which(chk)
  ug <- unique(gap[chk])
  if (any(i <- g("[^ \t\r\n]", ug))) {
    i <- chk[which(gap[chk] %in% ug[i])[1]]; k <- regexpr("[^ \t\r\n]", gap[i], useBytes = TRUE)
    bad(c(1L, e + 1L)[i] + k - 1L, paste("text outside a leaf:", show(substring(gap[i], k))))
  }
  us <- which(!ue)                                                           # attributes of distinct start tags
  big <- paste(u[us], collapse = "")
  am <- gregexpr(paste0(ws, "(", nm, ")", ws, "*=", ws, "*\"([^\"]*)\""), big, perl = TRUE, useBytes = TRUE)[[1]]
  cols <- list()
  if (am[1] > 0) {
    cs <- attr(am, "capture.start"); cl <- attr(am, "capture.length")
    an <- substring(big, cs[, 1], cs[, 1] + cl[, 1] - 1L); av <- substring(big, cs[, 2], cs[, 2] + cl[, 2] - 1L)
    ar <- us[findInterval(as.integer(am), cumsum(c(1, nchar(u[us], "bytes")))[seq_along(us)])]
    ua <- unique(an); ai <- match(an, ua)
    if (any(i <- duplicated(as.numeric(ar) * length(ua) + ai))) { i <- which(i)[1]; bad(p[f[match(ar[i], ui)]], paste0("attribute ", an[i], " repeated in ", show(u[ar[i]]))) }
    for (k in seq_along(ua)) { v <- rep(NA_character_, length(u)); w <- ai == k; v[ar[w]] <- av[w]; cols[[paste0("@", ua[k])]] <- v[ui[el]] }
  }
  nz <- ty[el] == 1L
  last <- e[f[el]]; last[nz] <- e[f[mate[el[nz]]]]
  close <- rep(NA_integer_, nel); close[nz] <- p[f[mate[el[nz]]]]
  structure(c(list(tag = tg[el], depth = dep[el], parent = parent, first = p[f[el]], last = last, close = close, stag = tok[f[el]], text = text), cols),
            class = "data.frame", row.names = c(NA_integer_, -nel))
}

# The files GCAM 9.1 parses for configuration config (main.cpp:216-231), read as GCAM reads it: no entity translation,
# no trimming (xml_parse_helper.cpp:1055-1062); each element child of the root is a section, each element child of a
# section a value, whatever its tag (configuration.cpp:83-136): the last Files value of a name wins (:105), every
# ScenarioComponents value counts in document order, duplicates kept (:127-128), a Bools value is true for text matching
# ^[+]?0*1$ (lexical_cast<bool>, boost 1.81 converter_lexical_streams.hpp:706-736).
# Parse order (single_scenario_runner.cpp:112-147): xmlInputFileName, the components, then in batch mode (BatchMode true,
# scenario_runner_factory.cpp:123-126) the ComponentSet/FileSet/Value files of BatchFileName (read only in batch mode,
# batch_runner.cpp:79-88, 297-327, 393-430). Relative paths are read from the configuration's folder; a path starting
# with one / or \ from its drive; a path starting with two (UNC) is absolute, kept as written with / for \.
# xml: base file name (NULL: list only). Returns a list: path (resolved, parse order, xmlInputFileName first), text (as
# written, parallel to path), b (index of xml in path; NA when xml is NULL), years (model years of the first <modeltime>
# in xmlInputFileName, model_time.cpp:93-151, 203-264; NULL when GCAM would not get them, would leave periods out, or minaj
# cannot read the file), note (why years is NULL,
# else ""), batch (resolved batch files; character(0) outside batch mode). Stops when the configuration cannot be read as
# GCAM reads it, and (xml given) when xml is not loaded exactly once, is xmlInputFileName, or is in the batch file.
config_components <- function(config, xml = NULL) {
  cn <- basename(config); win <- .Platform$OS.type == "windows"
  x <- tryCatch(unclass(xml_tokens(config)), error = function(e) stop("minaj cannot read ", cn, " the way GCAM does: ", conditionMessage(e), call. = FALSE))
  an <- x[["@name"]]; if (is.null(an)) an <- rep(NA_character_, length(x$tag)); an[is.na(an)] <- ""   # attrs["name"]: "" when absent
  v <- which(x$parent %in% which(x$parent == which(x$depth == 1L)[1])); sn <- x$tag[x$parent[v]]   # values and their sections
  fi <- v[sn == "Files" & an[v] == "xmlInputFileName"]; fb <- v[sn == "Files" & an[v] == "BatchFileName"]; bm <- v[sn == "Bools" & an[v] == "BatchMode"]
  fi <- fi[length(fi)]; fb <- fb[length(fb)]; bm <- bm[length(bm)]; sc <- v[sn == "ScenarioComponents"]
  if (!length(fi)) stop(cn, " has no Files value xmlInputFileName; GCAM stops without it", call. = FALSE)
  if (length(bm) && (is.na(x$text[bm]) || !nzchar(x$text[bm])))
    stop(cn, ', Bools value "BatchMode": configuration value ', if (is.na(x$text[bm])) "with child elements" else "is empty", call. = FALSE)
  bmo <- length(bm) && grepl("^[+]?0*1$", x$text[bm])                      # lexical_cast<bool>
  pc <- c(fi, sc, if (bmo) fb); tx <- x$text[pc]; lb <- sprintf('%s, %s value "%s"', cn, x$tag[x$parent[pc]], an[pc]); n0 <- length(tx)
  cd <- normalizePath(dirname(config), "/", mustWork = FALSE); dr <- if (grepl("^[A-Za-z]:", cd)) substr(cd, 1L, 2L) else ""
  for (pass in 1:2) {
    if (pass == 2L) {                                                        # batch components, after the configured ones
      if (!bmo) break
      y <- tryCatch({ if (!length(fb)) stop("no Files value BatchFileName", call. = FALSE); unclass(xml_tokens(ab[n0])) }, error = function(e)
        if (!is.null(xml)) stop(cn, " runs in batch mode, and minaj cannot read its batch file the way GCAM does (", conditionMessage(e), "); the batch file might load ", xml, " again", call. = FALSE))
      if (is.null(y)) break
      ya <- y[["@name"]]; if (is.null(ya)) ya <- rep(NA_character_, length(y$tag)); ya[is.na(ya)] <- ""
      fs <- which(y$tag == "FileSet" & y$parent %in% which(y$tag == "ComponentSet" & y$parent == which(y$depth == 1L)[1]))
      bv <- which(y$tag == "Value" & y$parent %in% fs)
      tx <- c(tx, y$text[bv]); lb <- c(lb, sprintf('%s, FileSet value "%s"', basename(ab[n0]), ya[bv]))
    }
    if (anyNA(tx)) stop(lb[which(is.na(tx))[1]], ": configuration value with child elements", call. = FALSE)
    why <- ifelse(!nzchar(tx), "the path is empty", ifelse(tx != trimws(tx), "GCAM would open the path with the surrounding spaces",
             ifelse(grepl("&", tx, fixed = TRUE), "entities are not translated by GCAM", ifelse(grepl("^[A-Za-z]:([^/\\\\]|$)", tx), "a drive-relative path (it depends on that drive's current folder)", ""))))
    if (any(i <- nzchar(why))) { i <- which(i)[1]; stop(sprintf('%s: %s ("%s")', lb[i], why[i], tx[i]), call. = FALSE) }
    ab <- gsub("\\", "/", tx, fixed = TRUE); i <- !grepl("^//", ab)          # UNC kept as written (normalizePath would query the server)
    if (any(i)) ab[i] <- normalizePath(ifelse(grepl("^[A-Za-z]:/", ab[i]), ab[i], ifelse(grepl("^/", ab[i]), paste0(dr, ab[i]), file.path(cd, ab[i]))), "/", mustWork = FALSE)
  }
  np <- 1L + length(sc); path <- ab[seq_len(np)]; text <- tx[seq_len(np)]; batch <- ab[-seq_len(n0)]
  b <- NA_integer_
  if (!is.null(xml)) {
    h <- which(if (win) tolower(basename(path)) == tolower(xml) else basename(path) == xml)
    if (!length(h) && any(if (win) tolower(basename(batch)) == tolower(xml) else basename(batch) == xml))
      stop(cn, " loads ", xml, " only through its batch file ", ab[n0], "; addon_minaj needs the base file in ScenarioComponents, with the add-on directly after it", call. = FALSE)
    if (length(h) != 1L) stop(cn, " loads ", xml, " ", length(h), " times; it must load the base file exactly once", call. = FALSE)
    if (h == 1L) stop(cn, ": the base file is the configuration's xmlInputFileName; load ", sub("[.]xml$", "", xml), "_override.xml there instead", call. = FALSE)
    if (any(if (win) tolower(basename(batch)) == tolower(xml) else basename(batch) == xml)) stop(xml, " is also loaded by the batch file ", ab[n0], ", after the add-on", call. = FALSE)
    b <- h
  }
  years <- NULL; note <- ""; mf <- path[1]; cap <- nm_opt("addon_parse_cap_mb", 100) * 2^20
  if (!file.exists(mf)) note <- paste(mf, "does not exist") else {
    if (file.size(mf) > cap) stop(sprintf("%s is larger than option minaj.addon_parse_cap_mb (%g MB); minaj reads the model years from it", mf, cap / 2^20), call. = FALSE)
    m <- tryCatch(unclass(xml_tokens(mf)), error = function(e) paste0("minaj cannot read ", basename(mf), " the way GCAM does: ", conditionMessage(e)))
    mt <- if (is.character(m)) NA else which(m$parent == which(m$depth == 1L)[1] & m$tag == "modeltime")[1]   # read once (scenario.cpp:151-161)
    if (is.character(m)) note <- m else if (is.na(mt)) note <- paste0("no <modeltime> in ", basename(mf)) else {
      ch <- which(m$parent == mt); ta <- m[["@time-step"]]; ta <- if (is.null(ta)) rep(NA_character_, length(ch)) else ta[ch]
      z <- c(m$text[ch], ta); z[is.na(z)] <- ""; zn <- suppressWarnings(as.numeric(z))   # getValue<int>: T() = 0 when the cast fails
      z <- ifelse(grepl("^[-+]?[0-9]+$", z) & !is.na(zn) & zn >= -2^31 & zn <= 2^31 - 1, zn, 0)
      iv <- z[seq_along(ch)]; sv <- z[-seq_along(ch)]
      mp <- numeric(0); s0 <- e0 <- -1; fc <- 2021                           # model_time.cpp:65, 99-137: std::map year -> step
      for (j in seq_along(ch)) switch(m$tag[ch[j]],
                                      "start-year" = { s0 <- iv[j]; mp[as.character(iv[j])] <- sv[j] },
                                      "inter-year" = { mp[as.character(iv[j])] <- sv[j] },
                                      "end-year" = { e0 <- iv[j]; mp[as.character(iv[j])] <- -1 },
                                      "final-calibration-year" = { fc <- iv[j] })
      ky <- sort(as.numeric(names(mp))); st <- unname(mp[as.character(ky)]); n <- length(ky)
      note <- if (!n) "No year information has been parsed in modeltime" else if (s0 == -1) "No start year information has been parsed in modeltime" else    # model_time.cpp:209-236
        if (e0 == -1) "No end year information has been parsed in modeltime" else if (ky[1] != s0) sprintf("Parsed invalid year: %.0f which is before the start year: %.0f", ky[1], s0) else
          if (ky[n] != e0) sprintf("Parsed invalid year: %.0f which is after the end year: %.0f", ky[n], e0) else if (fc < s0 || fc > e0) sprintf("Parsed invalid final calibration year: %.0f", fc) else ""
      if (!nzchar(note) && n > 1L) {                                         # model_time.cpp:241-264; C++ % keeps the sign of the gap
        g <- diff(ky); s1 <- st[-n]; i <- which(s1 == 0 | g %% pmax(abs(s1), 1) != 0)[1]; ng <- which(s1 < 0)[1]
        note <- if (!is.na(i)) { if (s1[i] == 0) sprintf("GCAM stops: time step 0 from %.0f to %.0f (integer division by zero, model_time.cpp:247)", ky[i], ky[i + 1L]) else
          sprintf("Specified time step of %.0f does not evenly divide years from %.0f to %.0f", s1[i], ky[i], ky[i + 1L]) } else if (!is.na(ng))
          sprintf("time step of %.0f from %.0f to %.0f is negative; GCAM leaves out the years from %.0f up to %.0f (model_time.cpp:253-258)", s1[ng], ky[ng], ky[ng + 1L], ky[ng], ky[ng + 1L]) else ""
      }
      if (!nzchar(note)) years <- as.integer(c(unlist(lapply(seq_len(n - 1L), function(i) ky[i] + s1[i] * (seq_len(g[i] / s1[i]) - 1))), e0))
    }
  }
  list(path = path, text = text, b = b, years = years, note = note, batch = batch)
}

# Answers questions about the state GCAM builds from the files configuration config parses before base file xml (P, in
# parse order, a file listed twice applied twice). A question is "<kind>\t<path>" or "<kind>\t<path>\t<field>"; path:
# steps from <world> joined by "\n", a step being the C++ member (registry source, as addon_sim) "\r" the key GCAM matches
# (name as written; year as integer; sector-name "\r" subsector-name; "" for a single member). Kinds: touched (with a
# field: P's writes to that field of holder path; without: P reaches the container), creator, exists, rules, stub,
# keywords, keymap (field: tag), siblings (field: member). Returns a named list, one entry per question:
# list(state = "untouched", "touched" or "unknown", value, why). value: SIMPLE and REPLACE fields the last raw text or
# element, named by its file; ARRAY list(y, f, v) of the writes in order; SETTRUE TRUE; OR the flag texts; a container
# the extracts by file; creator step -> "<class>\t<file>"; exists TRUE or FALSE; rules the raw rules from the last clear
# on, named by file; keywords key -> value; keymap key -> raw element; siblings the keys in list order; stub "created"
# for childless creations, else the file that first queued nodes. why: the file and reason of an unknown answer. P
# files are read by blocks (children of <world>) found with a block index (other files whole, up to option
# minaj.addon_parse_cap_mb); indexes and extracts are kept as 'xmlscan' records by file md5 (without a store: none);
# a file that changes while it is read gives unknown answers and no record.
prefix_scan <- function(config, xml, questions) {
  cc <- config_components(config, xml); P <- cc$path[seq_len(cc$b - 1L)]; cn <- basename(config)
  rf <- system.file("extdata", "gcam_parse_rules.csv", package = "minaj")
  if (!nzchar(rf)) stop("minaj: extdata/gcam_parse_rules.csv is missing from the package")
  rules <- utils::read.csv(rf, colClasses = "character", na.strings = character(0))
  rk <- paste(rules$parent_class, rules$tag, sep = "\r"); mem <- sub(";.*", "", rules$source); kind <- rules$kind
  atr <- kind == "SIMPLE" & grepl("[.](h|hpp):", mem)                         # SIMPLE fields that attributes set too, before the children (xml_parse_helper.cpp:822-839)
  cap <- nm_opt("addon_parse_cap_mb", 100) * 2^20; gm <- mem[match("World\rglobal-technology-database", rk)]
  keep <- !is.null(.nm$store) && !is.null(.nm$state_dir)
  kh <- h64s(paste(fn_text(prefix_scan), fn_text(xml_tokens))); rmd <- if (keep) file_md5(rf) else unname(tools::md5sum(rf))
  # block index of file f, read in 64 MB chunks, each carrying the last 64 KB into the next: the lines that start with a
  # tag at most 8 spaces in and the checks of cut() in addon_write; a list (head, wtag, lv, a, z, tag, name, key; byte
  # offsets as doubles), why the file is not laid out as gcamdata writes it (a comment, CDATA section or processing
  # instruction after the <scenario> start tag, or the end of a comment there, which may hide lines; a tag line
  # xml_tokens would not read, or with name twice; ic set TRUE where this depends on the cap), or "unknown: <reason>" for
  # a file that sets a name or year with a child element (GCAM matches objects by it)
  idx <- function(f) {
    n <- file.size(f); con <- file(f, "rb"); on.exit(close(con)); no <- "not laid out as gcamdata writes it"
    o <- 0; last <- -1; nq <- 0; Z <- list()
    repeat {
      seek(con, o); b <- readBin(con, "raw", min(64 * 2^20, n - o)); m <- length(b); end <- o + m >= n
      p <- grepRaw("\n {0,8}<[/A-Za-z_:]", b, all = TRUE)
      if (!o) { s0 <- if (m >= 3L && identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) 3L else 0L
                if (m > s0 + 1L && b[s0 + 1L] == as.raw(60L) && grepl("[A-Za-z_:]", rawToChar(b[s0 + 2L]))) p <- c(s0, p) }
      if (!end) p <- p[p <= m - 65536]                                        # the rest is read again with the next chunk
      p <- p[o + p > last]                                                      # each line once, by absolute offset
      nq <- nq + length(p); if (nq > 1e5) return(no)
      np <- length(p); ls <- q <- gt <- dp <- numeric(np); tg <- character(np)
      for (h in seq_len(np)) {
        i <- p[h]; j <- i + 1L; while (b[j] == as.raw(32L)) j <- j + 1L
        if (!(j - i - 1L) %in% c(0L, 4L, 8L)) return(no)
        g <- grepRaw(">", b, offset = j, fixed = TRUE); if (!length(g)) return(no)
        t <- tryCatch(rawToChar(b[j:g]), error = function(e) NA_character_); if (is.na(t)) return(no)
        ls[h] <- o + i + 1; q[h] <- o + j; gt[h] <- o + g; dp[h] <- j - i - 1L; tg[h] <- t
      }
      s1 <- if (o || !np) 1L else gt[1] + 1L                                   # after <scenario> (also a comment closing there): read whole (xml_tokens skips comments)
      if (length(grepRaw("<!", b, offset = s1, fixed = TRUE)) || length(grepRaw("<?", b, offset = s1, fixed = TRUE)) || length(grepRaw("-->", b, offset = s1, fixed = TRUE))) return(no)
      nn <- c(grepRaw("<name", b, offset = s1, fixed = TRUE, all = TRUE), grepRaw("<year", b, offset = s1, fixed = TRUE, all = TRUE))
      if (any(b[nn + 5L] %in% as.raw(c(9L, 10L, 13L, 32L, 47L, 62L)))) return("unknown: it sets a name or year with a child element (GCAM matches the object by it later)")
      cl <- startsWith(tg, "</"); sc <- endsWith(tg, "/>")                     # tag lines as xml_tokens reads them, name written once (else read whole)
      if (!all(grepl("^</[A-Za-z_:][-A-Za-z0-9_:.]*[ \t\r\n]*>$", tg[cl], perl = TRUE, useBytes = TRUE)) ||
          !all(grepl("^<[A-Za-z_:][-A-Za-z0-9_:.]*(?:[ \t\r\n]+[A-Za-z_:][-A-Za-z0-9_:.]*[ \t\r\n]*=[ \t\r\n]*\"[^\"<&]*(?:&(?:amp|lt|gt|quot|apos);[^\"<&]*)*\")*[ \t\r\n]*/?>$", tg[!cl], perl = TRUE, useBytes = TRUE)) ||
          any(grepl("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\"(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=", tg, perl = TRUE, useBytes = TRUE))) return(no)
      nv <- ifelse(grepl("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=", tg, perl = TRUE, useBytes = TRUE),
                   sub("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=[ \t\r\n]*\"([^\"]*)\"(?s).*$", "\\1", tg, perl = TRUE, useBytes = TRUE), "")
      tg <- sub("(?s)^</?([-A-Za-z0-9_:.]+).*$", "\\1", tg, perl = TRUE, useBytes = TRUE)
      if (np) last <- o + p[np]
      Z[[length(Z) + 1L]] <- list(ls, q, gt, dp, tg, nv, cl, sc)
      if (end) break
      o <- o + m - 65536
    }
    V <- lapply(1:8, function(h) unlist(lapply(Z, `[[`, h))); ls <- V[[1]]; q <- V[[2]]; gt <- V[[3]]; dp <- V[[4]]; tg <- V[[5]]; nv <- V[[6]]; cl <- V[[7]]; sc <- V[[8]]
    rm(Z, V)
    m <- length(q); if (!m || dp[1] || cl[1] || sc[1] || tg[1] != "scenario") return(no)
    lv <- character(0); bi <- ei <- integer(0); nw <- 0L; wi <- NA; i <- 2L      # blocks: first and last line
    repeat {
      if (i > m) return(no)
      if (!dp[i]) { if (!cl[i] || tg[i] != "scenario" || i != m) return(no); break }
      if (dp[i] != 4L || cl[i]) return(no)
      if (tg[i] == "world") {
        nw <- nw + 1L; if (nw > 1L || sc[i]) return(no); wi <- i; i <- i + 1L
        repeat {
          if (i > m) return(no)
          if (dp[i] == 4L && cl[i] && tg[i] == "world") { i <- i + 1L; break }
          if (dp[i] != 8L || cl[i]) return(no)
          e <- if (sc[i]) i else if (i < m && dp[i + 1L] == 8L && cl[i + 1L] && tg[i + 1L] == tg[i]) i + 1L else return(no)
          lv <- c(lv, "W"); bi <- c(bi, i); ei <- c(ei, e); i <- e + 1L
        }
      } else {
        e <- if (sc[i]) i else if (i < m && dp[i + 1L] == 4L && cl[i + 1L] && tg[i + 1L] == tg[i]) i + 1L else return(no)
        lv <- c(lv, "S"); bi <- c(bi, i); ei <- c(ei, e); i <- e + 1L
      }
    }
    a <- ls[bi]; z <- gt[ei]; gs <- c(1, z + 1); ge <- c(a - 1, n); ng <- length(gs)
    if (sum(pmax(ge - gs + 1, 0)) > cap) { ic <<- TRUE; return(no) }
    u <- c(gs, q[1], if (nw) q[wi]); v <- c(ge, gt[1], if (nw) gt[wi])           # the skeleton (everything outside the blocks), then the start tags
    by <- lapply(seq_along(u), function(k) if (v[k] >= u[k]) { seek(con, u[k] - 1); readBin(con, "raw", v[k] - u[k] + 1) })
    x <- tryCatch(xml_tokens(do.call(c, by[seq_len(ng)])), error = function(e)   # xml_tokens' own refusals only; anything else (memory) stops
      if (grepl("(only elements, double-quoted attributes, text, comments and the XML declaration are read|: no elements)$", conditionMessage(e))) NULL else stop(e))
    if (is.null(x) || !identical(x$tag, c("scenario", if (nw) "world"))) return(no)
    ri <- match(paste(ifelse(lv == "W", "World", "Scenario"), tg[bi], sep = "\r"), rk)
    list(head = by[[ng + 1L]], wtag = if (nw) by[[ng + 2L]], lv = lv, a = a, z = z, tag = tg[bi], name = nv[bi],
         key = paste(lv, ifelse(is.na(ri), tg[bi], mem[ri]), ifelse(kind[ri] %in% "SINGLE", "", nv[bi]), sep = "\r"))
  }
  # extract of tokens x (text s) for path st: every <world> and every element matching a prefix of the path (start tag as
  # written, closed with a generated end tag), and the whole of every element matching the path, in document order,
  # inside <scenario>; raw(0) without <world>; "unknown: <reason>" where an element may match the path in a way minaj
  # cannot follow (keys matched with the classes the file's own tags create; a name or year set by a child element)
  ext <- function(x, s, st) {
    if (any(x$tag %in% c("name", "year"))) return("unknown: it sets a name or year with a child element (GCAM matches the object by it later)")
    N <- length(x$tag); n <- length(st); sm <- sub("\r.*", "", st); sk <- substring(st, nchar(sm) + 2L)
    for (a in c("@name", "@year", "@sector-name", "@subsector-name")) if (is.null(x[[a]])) x[[a]] <- rep(NA_character_, N)
    f <- x$parent + 1L; attr(f, "levels") <- as.character(seq_len(N + 1L)); class(f) <- "factor"; kd <- split(seq_len(N), f)
    yk <- suppressWarnings(ifelse(grepl("^[-+]?[0-9]{1,9}$", x[["@year"]]), as.character(as.integer(x[["@year"]])), NA))
    gk <- ifelse(is.na(x[["@sector-name"]]) | is.na(x[["@subsector-name"]]) | !nzchar(x[["@sector-name"]]) | !nzchar(x[["@subsector-name"]]), NA,
                 paste(x[["@sector-name"]], x[["@subsector-name"]], sep = "\r"))      # location-info without both names is skipped (global_technology_database.cpp:90-103)
    w <- unlist(kd[kd[[1]][1] + 1L], use.names = FALSE); w <- w[x$tag[w] == "world"]   # the first root element, whatever its tag (xml_parse_helper.cpp:955)
    if (!length(w)) return(raw(0))
    L <- list(w); C <- list(rep("World", length(w)))
    for (i in seq_len(n)) {
      p <- L[[i]]; h <- as.integer(unlist(kd[p + 1L], use.names = FALSE)); pc <- rep(C[[i]], lengths(kd[p + 1L]))
      ri <- match(paste(pc, x$tag[h], sep = "\r"), rk); k <- kind[ri]
      yr <- k %in% c("VEC_YEAR", "PERIOD") | (k %in% "STUB_QUEUE" & x$tag[h] == "period")
      ky <- ifelse(yr, yk[h], ifelse(k %in% "GTD_LOCATION", gk[h], ifelse(k %in% "SINGLE", "", x[["@name"]][h])))
      on <- !is.na(ri) & (mem[ri] == sm[i] | (k %in% "STUB_QUEUE" & x$tag[h] == "period" & kind[match(sm[i], mem)] %in% "PERIOD"))   # a stub's periods go to its technology's
      u <- which(on & yr & is.na(ky))
      if (length(u)) return(sprintf("unknown: <%s> with year \"%s\" (GCAM may read it otherwise)", x$tag[h[u[1]]], x[["@year"]][h[u[1]]]))
      u <- which(is.na(ri) & (x[["@name"]][h] %in% sk[i] | yk[h] %in% sk[i] | gk[h] %in% sk[i]))
      if (length(u)) return(sprintf("unknown: <%s> in %s is not in the registry", x$tag[h[u[1]]], pc[u[1]]))
      u <- which(!is.na(ri) & !on & ky %in% sk[i] & x$tag[h] %in% rules$tag[mem == sm[i]])
      if (length(u)) return(sprintf("unknown: <%s> in %s goes to another list than in the path", x$tag[h[u[1]]], pc[u[1]]))
      hit <- on & ky %in% sk[i]; L[[i + 1L]] <- h[hit]; C[[i + 1L]] <- rules$creates[ri[hit]]
    }
    ch <- unlist(L[seq_len(n)], use.names = FALSE); xx <- L[[n + 1L]]; ct <- !endsWith(x$stag[ch], "/>")
    pos <- c(x$first[ch], x$last[ch[ct]], x$first[xx])
    tx <- c(x$stag[ch], paste0("</", x$tag[ch[ct]], ">", recycle0 = TRUE), if (length(xx)) substring(s, x$first[xx], x$last[xx]))
    charToRaw(paste(c("<scenario>", tx[order(pos)], "</scenario>", ""), collapse = "\n"))
  }
  # answer to one question (kind k, steps st, field fl or NA) from the extracts xo of the P files in parse order (names fo)
  # and their tokens X (NULL for an absent or empty extract)
  ans <- function(k, st, fl, fo, xo, X) tryCatch({
    n <- length(st); sm <- sub("\r.*", "", st)
    u <- which(vapply(xo, is.character, NA))
    if (length(u)) stop("\001", fo[u[1]], ": ", sub("^unknown: ", "", xo[[u[1]]]), call. = FALSE)
    no <- list(state = "untouched", value = switch(k, exists = FALSE, siblings = character(0), NULL), why = "")
    sl <- c(FALSE, kind[match(sm, mem)] %in% "GTD_LOCATION")                 # levels 0 (<world>) to n; location-info flags are not read (global_technology_database.cpp:86-118)
    gl <- c(FALSE, sm %in% gm)                                                 # made with <world> (world.cpp:98)
    cr <- gl                                                                    # never created: nothing at or below exists (vectors and maps start empty, single members null)
    for (x in X) if (!is.null(x)) {
      lv <- x$depth - 2L; v <- lv >= 0L & lv <= n
      cr[unique(lv[v & (!(x[["@delete"]] %in% "1" | x[["@nocreate"]] %in% "1") | sl[pmin(pmax(lv, 0L), n) + 1L])]) + 1L] <- TRUE
    }
    if (!all(cr)) return(no)
    tg <- switch(k, rules = unique(rules$tag[kind == "RULE"]), keywords = unique(rules$tag[kind == "KEYWORD"]), keymap = fl, siblings = unique(rules$tag[mem == fl]),
                 touched = if (!is.na(fl)) { if (fl %in% c("coefficient", "efficiency")) c("coefficient", "efficiency") else fl }, NULL)
    ex <- logical(n + 1L); cl <- cf <- character(n + 1L)
    wv <- wf <- wn <- wk <- sv <- character(0); wy <- numeric(0); wl <- logical(0); wo <- integer(0); fk <- sk <- ""; seen <- FALSE
    for (j in seq_along(X)) {
      x <- X[[j]]; if (is.null(x)) next
      s <- rawToChar(xo[[j]]); Encoding(s) <- "bytes"; N <- length(x$tag); lv <- x$depth - 2L
      D <- x[["@delete"]] %in% "1"; NC <- x[["@nocreate"]] %in% "1"; ig <- lv >= 0L & lv <= n & sl[pmin(pmax(lv, 0L), n) + 1L]
      lab <- paste0("<", x$tag, ifelse(!is.na(x[["@name"]]), paste0(" name=\"", x[["@name"]], "\""), ifelse(!is.na(x[["@year"]]), paste0(" year=\"", x[["@year"]], "\""), "")), ">")
      ch <- which(lv >= 0L & lv <= n); xx <- which(lv == n)
      u <- ch[D[ch] & !ig[ch]]
      if (length(u)) stop("\001", fo[j], " writes ", lab[u[1]], " with delete=\"1\"", call. = FALSE)   # deletes the object and all below (xml_parse_helper.cpp:432-441, 533-542)
      hc <- if (is.null(tg)) rep(TRUE, length(xx)) else xx %in% x$parent[lv == n + 1L & x$tag %in% tg]
      if (k == "touched" && !is.na(fl) && !is.null(x[[paste0("@", fl)]])) hc <- hc | !is.na(x[[paste0("@", fl)]][xx])
      rc <- logical(N); u <- xx[hc]; while (length(u)) { rc[u] <- TRUE; u <- x$parent[u]; u <- u[u > 0L & !rc[u]] }   # elements on a chain that reaches the slot
      u <- ch[NC[ch] & rc[ch] & !ig[ch] & !gl[lv[ch] + 1L]]
      if (length(u)) stop("\001", fo[j], " writes ", lab[u[1]], " with nocreate=\"1\" on the way", call. = FALSE)
      if (k == "keywords" && length(u <- ch[!is.na(x[["@keyword"]][ch])])) stop("\001", fo[j], " writes a keyword attribute on ", lab[u[1]], " (it empties the keyword map)", call. = FALSE)   # xml_parse_helper.cpp:782-789
      u <- ch[x$tag[ch] == "stub-technology" & (lv[ch] < n | !k %in% c("stub", "exists", "creator"))]
      if (length(u)) stop("\001", fo[j], " writes ", lab[u[1]], if (lv[u[1]] < n) " on the way; below a stub-technology no question is answered" else "; at a stub-technology only the stub, exists and creator questions are answered", call. = FALSE)
      act <- logical(N); lc <- character(N)
      for (r in ch) {                                                           # GCAM's parse of the chain, document order
        i <- lv[r]
        if (i > 0L && !act[x$parent[r]]) next
        c0 <- "World"
        if (i > 0L) {                                                           # rows of the class that first created the parent (xml_parse_helper.cpp:443-446, 552-556)
          ri <- match(paste(cl[i], x$tag[r], sep = "\r"), rk)
          if (is.na(ri) || mem[ri] != sm[i]) stop("\001", fo[j], ": ", lab[r], " in ", cl[i], if (is.na(ri)) " is not in the registry" else " goes to another list than in the path", call. = FALSE)
          c0 <- rules$creates[ri]
          if (kind[ri] == "PERIOD") {                                           # a vintage: class of the enclosing technology tag (technology_container.cpp:145, 191)
            r2 <- match(paste(lc[x$parent[r]], "period", sep = "\r"), rk)
            if (is.na(r2)) stop("\001", fo[j], ": <period> in ", lc[x$parent[r]], " is not in the registry", call. = FALSE)
            c0 <- rules$creates[r2]
          }
        }
        lc[r] <- c0
        if (!ex[i + 1L]) {
          if (NC[r] && !ig[r]) next                                             # nocreate: skipped with its children (xml_parse_helper.cpp:403-410)
          ex[i + 1L] <- TRUE; cl[i + 1L] <- c0; cf[i + 1L] <- fo[j]
          if (!i) { ex[gl] <- TRUE; cl[gl] <- "GlobalTechnologyDatabase" }
        }
        act[r] <- TRUE
        if (cl[i + 1L] == "StubTechnologyContainer" && (i < n || !k %in% c("stub", "exists", "creator")))
          stop("\001", fo[j], ": ", lab[r], " was created as a stub-technology; ", if (i < n) "below a stub-technology no question is answered" else "at a stub-technology only the stub, exists and creator questions are answered", call. = FALSE)
        if (i < n) next
        C <- cl[n + 1L]; e <- findInterval(x$last[r], x$first); sb <- if (e > r) (r + 1L):e else integer(0)
        dc <- character(N); dc[r] <- C                                          # the subtree: rows of the classes its own tags create
        for (d in sort(unique(x$depth[sb]))) {
          w <- sb[x$depth[sb] == d]; pc <- dc[x$parent[w]]; r3 <- match(paste(pc, x$tag[w], sep = "\r"), rk); b3 <- which(is.na(r3) | !nzchar(pc))
          if (length(b3)) stop("\001", fo[j], ": ", lab[w[b3[1]]], if (nzchar(pc[b3[1]])) paste(" in", pc[b3[1]], "is not in the registry") else paste(" is inside the value", lab[x$parent[w[b3[1]]]]), call. = FALSE)   # also a SIMPLE write with child elements
          dc[w] <- rules$creates[r3]
        }
        kr <- sb[x$parent[sb] == r]; rr <- match(paste(C, x$tag[kr], sep = "\r"), rk)
        if (k == "touched" && is.na(fl)) wo <- c(wo, j)
        else if (k == "touched") {
          fr <- match(paste(C, fl, sep = "\r"), rk); fk <- kind[fr]
          if (is.na(fr) || !fk %in% c("SIMPLE", "ARRAY", "REPLACE", "SETTRUE", "OR")) stop("\001", cf[n + 1L], ": ", lab[r], " was created as ", C, ", which has no value field <", fl, ">", call. = FALSE)
          h <- kr[x$tag[kr] %in% tg]
          if (fk == "SIMPLE") {
            av <- x[[paste0("@", fl)]]
            if (atr[fr] && !is.null(av) && !is.na(av[r])) { wv <- c(wv, av[r]); wf <- c(wf, fo[j]) }
            wv <- c(wv, x$text[h]); wf <- c(wf, rep(fo[j], length(h)))      # the last write wins (xml_parse_helper.cpp:734-741)
          } else if (fk == "ARRAY") {
            h <- h[!is.na(x[["@year"]][h])]                                      # no year: sets nothing (xml_parse_helper.cpp:684-689)
            y <- suppressWarnings(ifelse(grepl("^[-+]?[0-9]{1,9}$", x[["@year"]][h]), as.numeric(x[["@year"]][h]), NA))
            if (anyNA(y)) stop("\001", fo[j], " writes <", fl, "> with a year GCAM may read otherwise", call. = FALSE)
            wy <- c(wy, y); wl <- c(wl, x[["@fillout"]][h] %in% "1"); wv <- c(wv, x$text[h])
          } else if (fk == "OR") {
            if (!is.na(x[["@type-flags"]][r]) || any(x$tag[kr] == "type-flags")) stop("\001", fo[j], " writes type-flags (it resets the input flags)", call. = FALSE)
            wv <- c(wv, x$text[h])                                              # minicam_input.cpp:87-95
          } else if (fk == "SETTRUE") {                                        # an attribute of the same name sets it too, true or false (mac_control.h:94)
            if (!is.null(x[[paste0("@", fl)]]) && !is.na(x[[paste0("@", fl)]][r])) stop("\001", fo[j], " writes <", fl, "> as an attribute of ", lab[r], call. = FALSE)
            if (length(h)) wv <- c(wv, "TRUE")
          } else { wv <- c(wv, if (length(h)) substring(s, x$first[h], x$last[h])); wf <- c(wf, rep(fo[j], length(h))) }   # REPLACE: energy_input.cpp:150-159
        } else if (k == "rules") {
          h <- kr[kind[rr] %in% "RULE"]                                         # a clear: apply-to exactly share-weight and delete exactly 1 (subsector.cpp:131)
          wv <- c(wv, if (length(h)) substring(s, x$first[h], x$last[h])); wf <- c(wf, rep(fo[j], length(h))); wl <- c(wl, x[["@apply-to"]][h] %in% "share-weight" & x[["@delete"]][h] %in% "1")
        } else if (k == "keywords") {
          for (t in kr[kind[rr] %in% "KEYWORD"]) for (a in grep("^@", names(x), value = TRUE)) if (!is.na(x[[a]][t]) && !substring(a, 2L) %in% names(wk)) wk[substring(a, 2L)] <- x[[a]][t]   # the first value stays (xml_parse_helper.cpp:744-750)
        } else if (k == "keymap") {
          fr <- match(paste(C, fl, sep = "\r"), rk)
          if (is.na(fr) || kind[fr] != "KEYMAP") stop("\001", cf[n + 1L], ": ", lab[r], " was created as ", C, ", which has no key map <", fl, ">", call. = FALSE)
          h <- kr[x$tag[kr] == fl]; ka <- rules$key_attribute[fr]; kv <- if (is.null(x[[paste0("@", ka)]])) rep(NA_character_, length(h)) else x[[paste0("@", ka)]][h]
          kn <- (function(a, k) {                                               # key as GCAM reads it, in the form of nkey() in addon_write
            if (a == "year") return(ifelse(grepl("^0*[1-9][0-9]{0,8}$", k), as.character(suppressWarnings(as.integer(k))), NA))
            if (!a %in% c("tax", "price")) return(ifelse(is.na(k), "", k))
            d <- nchar(gsub("[^0-9]", "", sub("[eE].*$", "", sub("^-?0*", "", k))))
            ok <- grepl("^-?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]{1,2})?$", k) & d <= 15L
            v <- sub("^-", "", k); e <- suppressWarnings(ifelse(grepl("[eE]", v), as.integer(sub("^.*[eE]", "", v)), 0L)); v <- sub("[eE].*$", "", v)
            ip <- sub("[.].*$", "", v); ds <- paste0(ip, ifelse(grepl("[.]", v), sub("^.*[.]", "", v), "")); e <- e + nchar(ip)
            z <- sub("^0+", "", ds); e <- e - (nchar(ds) - nchar(z)); z <- sub("0+$", "", z)
            ifelse(!ok, NA, ifelse(!nzchar(z), "0", paste0(ifelse(startsWith(k, "-"), "-", ""), "0.", z, "e", e)))
          })(ka, kv)
          if (anyNA(kn)) stop("\001", fo[j], " writes <", fl, "> with a ", ka, " GCAM may read otherwise", call. = FALSE)
          wv <- c(wv, if (length(h)) substring(s, x$first[h], x$last[h])); wn <- c(wn, kn)
        } else if (k == "siblings") {
          w <- which(!is.na(rr) & mem[rr] == fl); seen <- seen || length(w) > 0L
          for (q in w) {                                                         # GCAM's vector rules (xml_parse_helper.cpp:494-541)
            t <- kr[q]; sk <- kind[rr[q]]
            ky <- if (sk %in% c("VEC_YEAR", "PERIOD")) suppressWarnings(ifelse(grepl("^[-+]?[0-9]{1,9}$", x[["@year"]][t]), as.character(as.integer(x[["@year"]][t])), NA))
                  else if (sk == "GTD_LOCATION") { g2 <- c(x[["@sector-name"]][t], x[["@subsector-name"]][t]); if (anyNA(g2) || !all(nzchar(g2))) NA else paste(g2, collapse = "\r") }
                  else if (sk == "SINGLE") "" else x[["@name"]][t]
            if (is.na(ky)) stop("\001", fo[j], " writes ", lab[t], " without a key GCAM matches", call. = FALSE)
            if (sk == "GTD_LOCATION") { if (t %in% x$parent && !ky %in% sv) sv <- c(sv, ky)   # flags not read; an entry needs a technology (global_technology_database.cpp:86-118)
            } else if (D[t]) sv <- sv[sv != ky] else if (!NC[t] && !ky %in% sv) sv <- c(sv, ky)
          }
        } else if (k == "stub") {
          a <- grep("^@", names(x), value = TRUE); a <- a[vapply(a, function(z) !is.na(x[[z]][r]), NA)]
          wv <- c(wv, if (length(kr) || x$tag[r] != "stub-technology" || any(a != "@name")) "touched" else "created"); wf <- c(wf, fo[j])
        }
      }
    }
    switch(k,
           exists = if (ex[n + 1L]) list(state = "touched", value = TRUE, why = "") else no,
           creator = if (ex[n + 1L] && nzchar(cf[n + 1L])) { w <- which(nzchar(cf[-1])); list(state = "touched", value = setNames(paste(cl[-1][w], cf[-1][w], sep = "\t"), st[w]), why = "") } else no,
           touched = if (is.na(fl)) { if (length(wo)) list(state = "touched", value = setNames(xo[unique(wo)], fo[unique(wo)]), why = "") else no } else if (!length(wv)) no else
             list(state = "touched", value = switch(fk, SIMPLE = , REPLACE = setNames(wv[length(wv)], wf[length(wf)]), ARRAY = list(y = wy, f = wl, v = wv), SETTRUE = TRUE, OR = sort(unique(wv))), why = ""),
           rules = if (!length(wv)) no else { l <- max(c(1L, which(wl))); list(state = "touched", value = setNames(wv[l:length(wv)], wf[l:length(wf)]), why = "") },
           keywords = if (!length(wk)) no else list(state = "touched", value = wk, why = ""),
           keymap = if (!length(wv)) no else { u <- !duplicated(wn, fromLast = TRUE); list(state = "touched", value = setNames(wv[u], wn[u]), why = "") },
           siblings = if (!seen) no else list(state = "touched", value = if (sk == "PERIOD") sv[order(as.numeric(sv))] else if (sk == "GTD_LOCATION") sort(sv, method = "radix") else sv, why = ""),   # std::map order: bytes
           stub = if (any(wv == "touched")) list(state = "touched", value = wf[match("touched", wv)], why = "") else if (length(wv)) list(state = "untouched", value = "created", why = "") else no)
  }, error = function(e) { m <- conditionMessage(e); if (!startsWith(m, "\001")) stop(e); list(state = "unknown", value = NULL, why = substring(m, 2L)) })
  questions <- unique(questions); qs <- strsplit(questions, "\t", fixed = TRUE)
  ok <- vapply(qs, function(q) length(q) %in% 2:3 && nzchar(q[2]) && (q[1] == "touched" || (length(q) == 3L) == (q[1] %in% c("keymap", "siblings"))) &&
                 q[1] %in% c("touched", "creator", "exists", "rules", "stub", "keywords", "keymap", "siblings"), NA)
  qp <- vapply(qs, function(q) q[2], ""); s1 <- sub("\n.*", "", qp); m1 <- sub("\r.*", "", s1)
  r1 <- match(m1, ifelse(rules$parent_class == "World", mem, NA))            # the first step: a child of <world>
  ok <- ok & !is.na(r1) & vapply(seq_along(qs), function(j) {                 # every step a keyed member of the class before it, with its key's form; the field a row of its kind
    if (!ok[j]) return(FALSE)
    st <- strsplit(qp[j], "\n", fixed = TRUE)[[1]]; sm <- sub("\r.*", "", st); sy <- sub("^[^\r]*\r", "", st)
    if (!all(grepl("\r", st, fixed = TRUE))) return(FALSE)
    pc <- "World"
    for (i in seq_along(st)) {
      w <- which(mem == sm[i] & rules$parent_class %in% pc); sk <- unique(kind[w])
      if (length(sk) != 1L || !sk %in% c("SINGLE", "VEC_NAMED", "VEC_YEAR", "PERIOD", "GTD_LOCATION")) return(FALSE)
      if (!switch(sk, SINGLE = sy[i] == "", VEC_NAMED = !grepl("\r", sy[i], fixed = TRUE), GTD_LOCATION = grepl("^[^\r]+\r[^\r]+$", sy[i]), grepl("^(0|-?[1-9][0-9]{0,8})$", sy[i]))) return(FALSE)
      pc <- unique(rules$creates[w])
    }
    q <- qs[[j]]; if (length(q) < 3L) return(TRUE)
    w <- rules$parent_class %in% pc
    switch(q[1], touched = any(w & rules$tag == q[3] & kind %in% c("SIMPLE", "ARRAY", "REPLACE", "SETTRUE", "OR")), keymap = any(w & rules$tag == q[3] & kind == "KEYMAP"),
           siblings = any(w & mem == q[3] & kind %in% c("SINGLE", "VEC_NAMED", "VEC_YEAR", "PERIOD", "GTD_LOCATION")))
  }, NA)
  if (!all(ok & !is.na(r1))) stop("addon_minaj: internal error, no add-on written; a question about the earlier files was not understood (", gsub("[\t\r\n]", " | ", questions[!(ok & !is.na(r1))][1]), ")", call. = FALSE)
  bk <- paste("W", m1, ifelse(kind[r1] == "SINGLE", "", substring(s1, nchar(m1) + 2L)), sep = "\r")   # block key, as cut() in addon_write (no name for a single member)
  uf <- unique(P); fe <- file.exists(uf)
  fz <- file.info(uf, extra_cols = FALSE)                                     # size and time with the md5: a file that changes while read is not stored
  md5 <- vapply(seq_along(uf), function(i) if (!fe[i]) NA_character_ else if (keep) file_md5(uf[i]) else unname(tools::md5sum(uf[i])), "")
  IX <- lapply(seq_along(uf), function(i) if (fe[i] && keep) rec_get("xmlscan", h64(list("xmlscan", md5[i], "index", kh, rmd)))$answer)
  todo <- which(fe & vapply(IX, is.null, NA))
  if (length(todo)) {
    message(sprintf("minaj: indexing %d earlier %s of %s (%.1f GB), once per file version", length(todo), if (length(todo) == 1L) "file" else "files", cn, sum(file.size(uf[todo])) / 2^30))
    for (i in todo) {
      ic <- FALSE; v <- tryCatch(idx(uf[i]), error = function(e) e)
      if (inherits(v, "error") && grepl("cannot allocate", conditionMessage(v), fixed = TRUE))   # a failed allocation: the memory form
        stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s; try again later",
                     ps::ps_system_memory()$avail / 2^30, nm_opt("ram_floor_gb", 4), 0.2 + 15.6 * min(fz$size[i], cap) / 2^30, "the tag lines", basename(uf[i])), call. = FALSE)
      if (inherits(v, "error") && file.access(uf[i], 4L) == 0L) stop(v)       # unknown only when the file cannot be read; anything else stops
      if (!inherits(v, "error") && !identical(c(file.size(uf[i]), as.numeric(file.mtime(uf[i]))), c(fz$size[i], as.numeric(fz$mtime[i])))) v <- "unknown: the file changed while minaj read it"
      IX[i] <- list(if (inherits(v, "error")) paste0("unknown: minaj cannot read it (", conditionMessage(v), ")") else v)
      if (keep && !inherits(v, "error") && !identical(v, "unknown: the file changed while minaj read it") && !ic)   # "not laid out" by the skeleton's size depends on the cap
        save_rds(list(md5 = md5[i], block = "index", answer = v, date = Sys.time(), source = "xmlscan"), rec_path("xmlscan", h64(list("xmlscan", md5[i], "index", kh, rmd))))
    }
  }
  E <- rep(list(list()), length(uf)); rb <- 0; nf <- integer(0); t0 <- proc.time()[["elapsed"]]; nl <- as.raw(10L); fl <- nm_opt("ram_floor_gb", 4)
  for (i in seq_along(uf)) for (b in unique(bk)) {
    pp <- unique(qp[bk == b]); ix <- IX[[i]]
    if (!fe[i]) { for (p in pp) E[[i]][[p]] <- "unknown: the file does not exist (GCAM stops when it cannot open a component)"; next }
    if (is.character(ix) && startsWith(ix, "unknown: ")) { for (p in pp) E[[i]][[p]] <- ix; next }
    if (is.list(ix)) {
      j <- which(ix$lv == "W" & ix$key == b)
      if (!length(j)) { for (p in pp) E[[i]][[p]] <- if (is.null(ix$wtag)) raw(0) else c(charToRaw("<scenario>\n"), ix$wtag, charToRaw("\n</world>\n</scenario>\n")); next }   # no record: the index shows it
      sz <- sum(ix$z[j] - ix$a[j] + 1); bl <- paste0(ix$tag[j[1]], if (nzchar(ix$name[j[1]])) paste0(" ", ix$name[j[1]]))
    } else sz <- file.size(uf[i])
    rkey <- h64(list("xmlscan", md5[i], b, kh, rmd)); an <- if (keep) rec_get("xmlscan", rkey)$answer; if (is.null(an)) an <- list(); miss <- character(0); tk <- list()
    for (p in pp) {
      if (!is.null(an[[p]])) { E[[i]][[p]] <- an[[p]]; next }
      st <- strsplit(p, "\n", fixed = TRUE)[[1]]; h <- length(st) - 1L
      while (h > 0L && is.null(an[[paste(st[seq_len(h)], collapse = "\n")]])) h <- h - 1L
      if (!h) { miss <- c(miss, p); next }
      hp <- paste(st[seq_len(h)], collapse = "\n"); a <- an[[hp]]               # below a path already extracted: from its extract, tokenized once per call
      if (is.character(a) || !length(a)) { E[[i]][[p]] <- a; next }
      if (is.null(tk[[hp]])) {
        need <- 0.2 + 15.6 * length(a) / 2^30; free <- ps::ps_system_memory()$avail / 2^30
        if (free < fl + need) stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s; try again later",
                                           free, fl, need, if (is.list(ix)) paste("block", bl) else "the whole file", basename(uf[i])), call. = FALSE)
        s <- rawToChar(a); Encoding(s) <- "bytes"; tk[[hp]] <- list(unclass(xml_tokens(a)), s)
      }
      E[[i]][[p]] <- ext(tk[[hp]][[1]], tk[[hp]][[2]], st)
    }
    rm(tk)
    if (!length(miss)) next
    if (sz > cap) {                                                             # depends on the cap: not stored
      for (p in miss) E[[i]][[p]] <- sprintf("unknown: %s larger than option minaj.addon_parse_cap_mb (%g MB)", if (is.list(ix)) paste0("its block ", bl, " is") else "it is not laid out as gcamdata writes it and is", cap / 2^20)
      next
    }
    need <- 0.2 + 15.6 * sz / 2^30; free <- ps::ps_system_memory()$avail / 2^30
    if (free < fl + need) stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s; try again later",
                                       free, fl, need, if (is.list(ix)) paste("block", bl) else "the whole file", basename(uf[i])), call. = FALSE)
    d <- if (is.list(ix)) {
      con <- file(uf[i], "rb"); bs <- tryCatch(lapply(j, function(h) { seek(con, ix$a[h] - 1); c(readBin(con, "raw", ix$z[h] - ix$a[h] + 1), nl) }), finally = close(con))
      do.call(c, c(list(ix$head, nl, ix$wtag, nl), bs, list(charToRaw("</world>\n</scenario>\n"))))
    } else readBin(uf[i], "raw", sz)
    rb <- rb + sz; nf <- union(nf, i)
    if (!identical(c(file.size(uf[i]), as.numeric(file.mtime(uf[i]))), c(fz$size[i], as.numeric(fz$mtime[i])))) {   # not stored
      for (p in miss) E[[i]][[p]] <- "unknown: the file changed while minaj read it"
      next
    }
    y <- tryCatch(unclass(xml_tokens(d)), error = function(e) {               # xml_tokens' own refusals are answers; a failed allocation stops in the memory form
      m <- conditionMessage(e)
      if (grepl("(only elements, double-quoted attributes, text, comments and the XML declaration are read|: no elements)$", m))
        return(paste0("unknown: minaj cannot read ", if (is.list(ix)) paste0("its block ", bl) else "it", " the way GCAM does (", m, ")"))
      if (grepl("cannot allocate", m, fixed = TRUE))
        stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s; try again later",
                     ps::ps_system_memory()$avail / 2^30, fl, need, if (is.list(ix)) paste("block", bl) else "the whole file", basename(uf[i])), call. = FALSE)
      stop(e) })
    s <- if (is.character(y)) "" else { s <- rawToChar(d); Encoding(s) <- "bytes"; s }; rm(d)
    for (p in miss) { e <- if (is.character(y)) y else ext(y, s, strsplit(p, "\n", fixed = TRUE)[[1]]); E[[i]][[p]] <- e; if (is.character(e) || length(e) <= 20 * 2^20) an[[p]] <- e }
    rm(y, s); if (sz >= 16 * 2^20) invisible(gc())
    if (keep) save_rds(list(md5 = md5[i], block = b, answer = an, date = Sys.time(), source = "xmlscan"), rec_path("xmlscan", rkey))
  }
  if (rb > 0) message(sprintf("minaj: read %.1f MB from %d earlier %s of %s for %d question(s) in %.1f s", rb / 2^20, length(nf), if (length(nf) == 1L) "file" else "files", cn, length(questions),
                              proc.time()[["elapsed"]] - t0))
  fi <- match(P, uf); fo <- basename(P); R <- vector("list", length(qs))
  for (p in unique(qp)) {                                                     # each extract tokenized once, for all questions on its path
    xo <- lapply(seq_along(uf), function(i) E[[i]][[p]]); xr <- vapply(xo, function(r) is.raw(r) && length(r) > 0L, NA)
    if (any(xr)) {
      need <- 0.2 + 15.6 * sum(as.numeric(lengths(xo[xr]))) / 2^30; free <- ps::ps_system_memory()$avail / 2^30
      if (free < fl + need) stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading the extracts of %d earlier %s of %s; try again later",
                                         free, fl, need, sum(xr), if (sum(xr) == 1L) "file" else "files", cn), call. = FALSE)
    }
    X <- lapply(seq_along(xo), function(i) if (xr[i]) {
      x <- unclass(xml_tokens(xo[[i]]))
      for (a in c("@delete", "@nocreate", "@keyword", "@name", "@year", "@fillout", "@apply-to", "@type-flags", "@sector-name", "@subsector-name")) if (is.null(x[[a]])) x[[a]] <- rep(NA_character_, length(x$tag))
      x
    })
    for (j in which(qp == p)) R[j] <- list(ans(qs[[j]][1], strsplit(p, "\n", fixed = TRUE)[[1]], if (length(qs[[j]]) == 3L) qs[[j]][3] else NA_character_, fo, xo[fi], X[fi]))
    rm(X)
  }
  setNames(R, questions)
}

#' Write an add-on XML file: what an override changes, to load directly after the base file
#'
#' Writes the base XML file and <stem>_override.xml as make_minaj() does, compares the two the way GCAM parses them,
#' and writes <stem>_addon.xml with only what differs. A GCAM configuration that loads this exact base file and,
#' directly after it, a universal add-on builds the same model as one that loads <stem>_override.xml in place of the
#' base file, whatever else the configuration loads; a configuration-specific add-on (see below) does so only in its
#' configuration, while the files loaded before the base file keep their content. Both apart from the entry order in
#' some lists, which is reported. Each call that writes an add-on replaces <stem>_addon.xml in its folder; load one
#' add-on per base file, and pass overrides together to combine them.
#' Changes an add-on cannot carry on its own (for example, without config=, removals, values set only in the base
#' file, type changes, rule lists that are not extended, new regions, modeltime and solver changes) stop with each
#' place and reason (the first 2000 when there are more; a list longer than option warning.length allows is printed
#' as a message, and the error keeps the first place); use <stem>_override.xml for those. Places marked "config= can
#' decide it from the configuration's earlier files" depend on what the files loaded before the base file set; with
#' config= they are decided from those files, and an add-on that needs them is configuration-specific: valid directly
#' after the base file while the files loaded before it keep their content (status_minaj(config) checks this). A value with no default is
#' un-set by writing -0; with option minaj.addon_neg0 = FALSE the element holding it is rebuilt instead where possible,
#' else the change is refused. The base file's path and md5,
#' the number of defaults restored from the rules file (with the first 5) and the configuration line for the add-on
#' are printed. GCAM's parse rules come from inst/extdata/gcam_parse_rules.csv, built from the GCAM 9.1 C++ code; when
#' the tree has cvs/objects and its parse code differs, the call is refused. Only the blocks (regions and other
#' top-level parts) that differ are compared.
#' @param xml XML file name, e.g. "electricity_water.xml".
#' @param override Named list replacing CSV inputs or data objects, as in make_minaj().
#' @param config Path to a GCAM configuration that loads the base file exactly once, in its ScenarioComponents (not
#'   only through its batch file). Read as GCAM 9.1 reads it: its
#'   xmlInputFileName, then its ScenarioComponents values in order (and the batch file's in batch mode); paths with
#'   surrounding spaces, & or a drive-relative form (C:x.xml) are refused. Checks that the base file it loads is the current
#'   base result, names each listed file that does not exist, and refuses when another add-on to the same base file is
#'   loaded after it. Where a place depends on the files loaded before the base file, the first call that needs them
#'   reads each of them once in full to index it (minutes for a base file late in a full configuration); after that
#'   only the blocks needed are read (a file not laid out as gcamdata writes it: whole). Indexes and answers are kept
#'   in the store per file version (without a store, every call indexes again); a changed earlier file is indexed
#'   again. The check of a configuration-specific add-on reads the earlier files in full once per R session and
#'   refuses when one of them sets a name or year with a child element. When the base file's path is relative, prints
#'   the add-on's line relative to the configuration's folder. Relative paths in it are read from the configuration's
#'   folder, which must be the folder GCAM runs in (usually exe/).
#' @param dest Folder for the add-on (default: addon/ in the minaj XML folder). A relative path is taken from the
#'   working directory. Use one folder per override to keep add-ons for several overrides.
#' @param ask Ask before starting when the estimated time exceeds option minaj.ask_secs (default 120 s).
#' @return Invisibly, a data frame with one row: target, kind, status ("written", "unchanged" or "no change") and file
#'   (the add-on), or invisibly NULL when the run is declined.
#' @export
addon_minaj <- function(xml, override, config = NULL, dest = NULL, ask = interactive()) {
  if (!is.list(override) || !length(override) || is.null(names(override)) || any(!nzchar(names(override))))
    stop("override must be a named list of replacements, as in make_minaj()")
  if (anyDuplicated(names(override))) stop("override names ", names(override)[anyDuplicated(names(override))], " twice; give each input once")
  if (!is.null(dest) && !grepl("^([A-Za-z]:)?[/\\\\]", dest)) dest <- file.path(getwd(), dest)
  override <- lapply(override, function(o) if (is.character(o) && length(o) == 1) user_path(o) else o)   # paths from the working directory
  if (!is.null(config)) config <- user_path(config)
  rf <- system.file("extdata", "gcam_parse_rules.csv", package = "minaj")
  if (!nzchar(rf)) stop("minaj: extdata/gcam_parse_rules.csv is missing from the package")
  if (!is.null(.nm$tree) && dir.exists(od <- normalizePath(file.path(.nm$tree, "cvs", "objects"), "/", mustWork = FALSE))) {   # parse code against the registry's (gcam_rules_build's tree hash)
    rr <- utils::read.csv(rf, colClasses = "character", na.strings = character(0))
    th <- gcam_rules_build(od, out = rr); rh <- rr$tree_hash[1]; rm(rr)
    if (!identical(th, rh)) stop(sprintf(paste0("addon_minaj: the GCAM parse code in %s differs from the code %s was built from (tree hash %s here, %s there). ",
                                                "Rebuild it with minaj:::gcam_rules_build(\"%s\", out = \"%s\"); the next call uses it. Reinstalling minaj restores the packaged file; ",
                                                "to keep the rebuild, write it to inst/extdata/gcam_parse_rules.csv in the minaj source as well. One rules file serves every tree, ",
                                                "so trees with the earlier parse code are then refused until it is rebuilt for them"), od, rf, th, rh, od, rf), call. = FALSE)
  }
  restore <- enter(); on.exit(leave(restore))
  if (target_kind(xml) != "xml") stop(xml, " is not an XML file of this tree")
  pb <- plan_targets(xml, list()); po <- plan_targets(xml, override)        # one question for both: shared chunks once, chunks the override reaches twice
  ch <- unique(c(pb$chunks, po$chunks)); two <- intersect(intersect(pb$chunks, po$chunks), downstream_chunks(names(override)))
  secs <- sum(vapply(c(ch, two), est_secs, numeric(1)))
  if (length(ch) && ask && is.finite(secs) && secs > nm_opt("ask_secs", 120)) {
    ok <- utils::askYesNo(sprintf("minaj: %d chunk(s) to run in this R session, about %s (%s). Run now?", length(ch), fmt_secs(secs), paste(head(ch, 4), collapse = ", ")))
    if (!isTRUE(ok)) { message("minaj: not run"); return(invisible(NULL)) }
  }
  t0 <- proc.time()[["elapsed"]]
  stem <- sub("[.]xml$", "", xml); base_dir <- dest_dir()
  dest <- if (is.null(dest)) dest_dir(file.path(base_dir, "addon")) else dest_dir(dest)
  a_file <- file.path(dest, paste0(stem, "_addon.xml"))
  old <- if (file.exists(a_file)) file_md5(a_file) else NULL
  left <- function(m) {                                                      # an add-on of an earlier call at this path, named after the message
    if (is.null(old)) return(m)
    nt <- paste0(a_file, " was written by an earlier call and is still in place; keep it out of the configuration until a call for this override writes it.")
    if (nchar(m, "bytes") + nchar(nt, "bytes") > getOption("warning.length", 1000) - 20) { message("minaj: ", nt); m } else paste0(m, "\n", nt)
  }
  floor <- nm_opt("ram_floor_gb", 4)
  low <- function(m, need = 0) if ((free <- ps::ps_system_memory()$avail / 2^30) < floor + need)
    stop(left(sprintf("addon_minaj: %sonly %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb)%s; try again later", m, free, floor,
                      if (need > 0) sprintf(" plus about %.1f GB for comparing files of this size", need) else "")), call. = FALSE)
  conv <- function(h, f) {                                                   # TRUE when write_xml would run the Java converter for f
    xk <- h64(list("xml", h, writer_key())); r <- rec_get("xml", xk)
    is.null(r) || !any(vapply(c(f, xml_store_path(xk), r$source_file), function(z) file.exists(z) && identical(file_md5(z), r$md5), NA))
  }
  if (length(ch)) low(sprintf("%d chunk(s) to run, but ", length(ch)))
  noval <- function(ctx) { ctx$val <- list(); ctx$val_mb <- setNames(numeric(0), character(0)) }   # cached tables are not needed once the hash is known
  ctx_b <- new_ctx(); h_b <- value_hash(xml, ctx_b); noval(ctx_b)
  if (conv(h_b, file.path(base_dir, xml))) low(paste0(xml, " is to be converted to XML (Java), but "))
  b_file <- make_one(xml, ctx_b, base_dir)$file; b_md5 <- file_md5(b_file)
  rec_b <- rec_get("xml", h64(list("xml", h_b, writer_key())))
  rel <- FALSE
  if (!is.null(config)) {
    cc <- tryCatch(config_components(config, xml), error = function(e) stop(left(conditionMessage(e)), call. = FALSE))
    ms <- c(cc$path[-cc$b], cc$batch); ms <- unique(ms[!file.exists(ms)])
    for (f in ms) message("minaj: ", basename(config), " lists ", f, ", which does not exist; GCAM stops when it cannot open a component")
    hit <- cc$text[cc$b]; cfg_b <- cc$path[cc$b]
    rel <- !grepl("^([A-Za-z]:)?[/\\\\]", hit); cd <- normalizePath(dirname(config), "/")
    rw <- if (rel) paste0(" (a relative path, read from the configuration's folder ", cd, ", which minaj takes to be the folder GCAM runs in; GCAM reads relative paths from the folder it runs in)") else ""
    message("minaj: ", basename(config), " loads the base file from ", cfg_b, rw)
    if (!file.exists(cfg_b)) stop(left(paste0(cfg_b, " does not exist", rw)), call. = FALSE)
    if (!identical(file_md5(cfg_b), b_md5)) stop(left(paste0(cfg_b, " is not the current base result ", b_file, "; copy that file there (promote_minaj() copies to xml/) or load it in the configuration")), call. = FALSE)
    hd <- charToRaw(paste0("<!-- ", gsub("-{2,}", "-", paste0("minaj add-on to ", basename(b_file), " (md5 ", b_md5, ")"))))   # header of addon_write
    an <- normalizePath(a_file, "/", mustWork = FALSE); lf <- c(cc$path[-seq_len(cc$b)], cc$batch); lf <- unique(lf[file.exists(lf)])
    for (f in lf[if (.Platform$OS.type == "windows") tolower(lf) != tolower(an) else lf != an])
      if (length(grepRaw(hd, readBin(f, "raw", 512L), fixed = TRUE))) stop(left(paste0("another add-on to this base file is loaded after it (", f, "); combine the overrides into one call")), call. = FALSE)
  }
  ctx_o <- new_ctx(override); h_o <- value_hash(xml, ctx_o); noval(ctx_o)
  same <- identical(h_b, h_o) && !is.null(rec_b) && identical(b_md5, rec_b$md5)
  if (!same && conv(h_o, file.path(base_dir, paste0(stem, "_override.xml")))) low(paste0(stem, "_override.xml is to be converted to XML (Java), but "))
  o_file <- if (same) b_file else make_one(xml, ctx_o, base_dir, override = TRUE)$file
  key <- h64(list("addon", b_md5, h_o, writer_key(), file_md5(rf), isTRUE(nm_opt("addon_guard", TRUE)), isTRUE(nm_opt("addon_neg0", TRUE)), names(override),
                  h64s(paste(fn_text(addon_write), fn_text(addon_sim), fn_text(xml_tokens), fn_text(config_components), fn_text(prefix_scan)))))
  rec <- rec_get("addon", key)                                               # universal add-on, else one kept by the md5s of the earlier files
  if (!is.null(config) && !(!is.null(rec) && identical(old, rec$md5))) { key_cfg <- h64(list(key, "config", unname(vapply(cc$path[seq_len(cc$b - 1L)], file_md5, "")))); rec <- rec_get("addon", key_cfg) }
  if (!is.null(rec) && identical(old, rec$md5)) { status <- "unchanged"; r <- rec$result } else {
    low("", 0.15 + 15.6 * max(file.size(b_file), file.size(o_file)) / 2^30)                    # measured peak of addon_write on vanilla 9.1 files
    r <- tryCatch(addon_write(b_file, o_file, a_file, names(override), config), error = function(e) stop(left(conditionMessage(e)), call. = FALSE))
    status <- if (r$empty) "no change" else "written"
    rec <- list(xml = xml, md5 = file_md5(a_file), base_md5 = b_md5, xml_object = h_o, result = r, date = Sys.time(), source = "addon")
    if (!is.null(r$prefix_md5)) key_cfg <- h64(list(key, "config", unname(r$prefix_md5)))
    rec_put("addon", if (is.null(r$prefix_md5)) key else key_cfg, rec)
    if (!is.null(old) && !identical(old, rec$md5)) {
      mf <- tryCatch(utils::read.csv(file.path(dest, "minaj_manifest.csv"), colClasses = "character"), error = function(e) NULL)
      ml <- if (!is.null(mf)) mf[mf$file == basename(a_file) & mf$md5 == old, , drop = FALSE]
      message("minaj: replaced the earlier ", basename(a_file), " in ", dest, " (md5 ", old, if (length(ml) && nrow(ml)) paste0(", written ", ml$time[nrow(ml)], " for inputs ", ml$override[nrow(ml)]),
              if (is.null(r$prefix_md5)) "); every configuration that loads it now gets this override" else
                paste0("); it is valid only after the earlier files of ", basename(config), ", so check any other configuration that loads it with status_minaj(config)"),
              if (r$empty) " (the add-on is now empty)", ". Use dest = to keep add-ons for several overrides", if (!is.null(r$prefix_md5)) " or one add-on per configuration")
    }
    manifest_add(dest, data.frame(time = format(Sys.time()), file = basename(a_file), xml = xml, xml_object = h_o, md5 = rec$md5,
                                  tree_head = tree_head(), override = paste(names(override), collapse = " "), how = "addon"))
  }
  lbl <- c(values = "values", arrays = "array fields", new = "new elements", deletes = "deletes from the override",
           rules = "rule lists", keywords = "keyword sets", keymaps = "key-map entries", flags = "input flags",
           cbp = "calc-base-price blocks", wrappers = "wrappers", restores = "restored values", rebuilds = "rebuilt elements", removals = "base-only elements deleted")
  n <- r$n[r$n > 0]
  message(sprintf("minaj: %s %s in %s, %.1f KB%s | %s", basename(a_file), status, fmt_secs(proc.time()[["elapsed"]] - t0), r$size / 1024,
                  if (!same) sprintf(" (%.2f%% of %s)", 100 * r$size / file.size(o_file), basename(o_file)) else "",
                  if (length(n)) paste(sprintf("%s %d", lbl[names(n)], as.integer(n)), collapse = ", ") else "nothing differs"))
  if (length(r$defaults)) message("minaj: restored defaults (", length(r$defaults), "): ", paste(head(r$defaults, 5), collapse = "; "), if (length(r$defaults) > 5) "; ...")
  if (length(r$moves)) message("minaj: entry order differs from the override run in ", length(r$moves), " list(s) (this changes summation order and, where calibration values tie, ",
                               "which entry calibration takes share-weights from): ", paste(head(r$moves, 5), collapse = "; "), if (length(r$moves) > 5) "; ...")
  if (!same && status != "unchanged") {
    big <- file.size(o_file) > 50 * 2^20
    if (big || ps::ps_system_memory()$avail / 2^30 < floor) {
      message("minaj: table report skipped (", if (big) paste(basename(o_file), "is over 50 MB") else "free memory is under option minaj.ram_floor_gb", ")")
    } else {
      tabs <- tryCatch({
        tb <- obj_get(h_b)[[1]]$data_tables; to <- obj_get(h_o)[[1]]$data_tables
        unlist(lapply(seq_len(max(length(tb), length(to))), function(i) {
          s <- if (i > length(tb)) "table added" else if (i > length(to)) "table removed" else table_diff(tb[[i]]$data, to[[i]]$data)$summary
          if (!identical(s, "identical values")) paste0("  table ", i, " (", if (i <= length(to)) to[[i]]$header else tb[[i]]$header, "): ", s)
        }))
      }, error = function(e) NULL)
      if (length(tabs)) message("minaj: tables of ", xml, " changed by the override (paired by position):\n", paste(tabs, collapse = "\n"))
    }
  }
  ab <- normalizePath(a_file, "/"); ap <- ab
  if (rel) {                                                                 # relative to the configuration's folder, as the base file's line is
    up <- function(p) strsplit(if (.Platform$OS.type == "windows") tolower(p) else p, "/", fixed = TRUE)[[1]]
    a <- up(ab); f <- up(normalizePath(dirname(config), "/")); k <- 0L
    while (k < min(length(a), length(f)) && a[k + 1L] == f[k + 1L]) k <- k + 1L
    if (k) ap <- paste(c(rep("..", length(f) - k), strsplit(ab, "/", fixed = TRUE)[[1]][-seq_len(k)]), collapse = "/")
  }
  if (is.null(config)) {
    tf <- file.path(xml_dir(), xml)
    if (file.exists(tf) && !identical(normalizePath(tf, "/"), normalizePath(b_file, "/")) && !identical(file_md5(tf), b_md5))
      warning("minaj: ", tf, " differs from the base file of this add-on; a configuration that loads it needs that base file there (promote_minaj() copies it)",
              if (!same) paste0(" or ", basename(o_file)), call. = FALSE)
  }
  message(if (is.null(r$prefix_md5)) "minaj: universal add-on, valid directly after this exact base file in any configuration that loads it: " else
            sprintf("minaj: configuration-specific add-on, valid directly after this exact base file in %s while the %d %s (status_minaj(config) checks this): ", basename(config), length(r$prefix_md5),
                    if (length(r$prefix_md5) == 1L) "file loaded before it keeps its content" else "files loaded before it keep their content"), normalizePath(b_file, "/"), " (md5 ", b_md5,
          "). Add this line directly after the base file", if (!is.null(config)) paste0(" in ", basename(config)), ":\n  <Value name=\"", stem, "_addon\">", ap, "</Value>",
          if (rel && !identical(ap, ab)) paste0("\n  (relative to ", normalizePath(dirname(config), "/"), ", as the base file's line is; absolute path ", ab, ")"))
  invisible(data.frame(target = xml, kind = "xml", status = status, file = a_file))
}

# Writes to a_file the add-on for base file b_file and override file o_file (see addon_minaj): the elements of o_file
# that, parsed after b_file, give the state GCAM builds from o_file alone. override: input names for the header comment.
# Stops, writing nothing, when a difference needs more than the two files (each place and reason is listed; a list longer
# than option warning.length allows is printed as a message first and the error keeps the first place; after 2000
# places the remaining blocks are not compared).
# config: GCAM configuration (NULL: none); the base file it loads must be b_file. With config, places that depend on
# what the configuration's earlier files set are decided from prefix_scan's answers (each group of blocks is compared
# again until no question is open) and model years; an add-on that used such answers is checked by addon_sim after the
# earlier files, and its header comment adds "configuration-specific: <n> earlier files" ("1 earlier file").
# Returns, invisibly, a list: n (counts per rule), moves (lists whose entry order differs from the override run),
# defaults (each value written from the rules file's default, "<place> <field> = <value>"; an array field's year as
# <field>[<year>]), size (bytes), empty (TRUE when nothing differs) and prefix_md5 (md5 of each earlier file by path,
# in parse order, taken before they are read, when answers about them or the model years were used; else NULL). Stops,
# writing nothing, when the configuration's earlier files changed during the call.
addon_write <- function(b_file, o_file, a_file, override = character(0), config = NULL) {
  f <- system.file("extdata", "gcam_parse_rules.csv", package = "minaj")
  if (!nzchar(f)) stop("minaj: extdata/gcam_parse_rules.csv is missing from the package")
  yrs <- NULL; ynote <- ""; pm <- NULL
  if (!is.null(config)) {
    cc <- config_components(config, basename(b_file)); yrs <- cc$years; ynote <- cc$note; pp <- cc$path[seq_len(cc$b - 1L)]
    if (!identical(unname(tools::md5sum(cc$path[cc$b])), unname(tools::md5sum(b_file))))
      stop("addon_minaj: internal error, no add-on written; the base file ", basename(config), " loads (", cc$path[cc$b], ") differs from ", b_file, call. = FALSE)
    pm <- setNames(vapply(pp, function(p) if (!file.exists(p)) NA_character_ else if (!is.null(.nm$store) && !is.null(.nm$state_dir)) file_md5(p) else unname(tools::md5sum(p)), ""), pp)
  }
  rules <- utils::read.csv(f, colClasses = "character", na.strings = character(0))
  rk <- paste(rules$parent_class, rules$tag, sep = "\r"); kind <- rules$kind
  msrc <- sub(";.*", "", rules$source); mem <- paste(rules$parent_class, msrc)  # one C++ member per DEFINE_VARIABLE line
  atr <- kind == "SIMPLE" & grepl("\\.(h|hpp):", msrc)                        # SIMPLE fields of a data vector: attributes set them too (handler fields: elements only)
  guard <- isTRUE(nm_opt("addon_guard", TRUE)); cap <- nm_opt("addon_parse_cap_mb", 100) * 2^20
  lim <- max(300, getOption("warning.length", 1000) - 400)                   # longer errors are printed in full first, then stopped short
  neg0 <- isTRUE(nm_opt("addon_neg0", TRUE))
  rdv <- local({ dv <- rules$default; vt <- rules$value_type; ifelse(vt %in% c("double", "Value") & grepl("^-?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?$", dv), dv,   # a constructor default as GCAM reads it back, NA: none
                ifelse(vt %in% "int" & grepl("^-?[0-9]+$", dv), dv, ifelse(vt %in% "unsigned int" & grepl("^[0-9]+$", dv), dv,
                ifelse(vt %in% "bool" & dv %in% c("true", "false"), ifelse(dv == "true", "1", "0"),
                ifelse(vt %in% c("gcamstr", "std::string") & !dv %in% "NA" & !grepl("[[:space:]&<>\"']", dv), dv,  # "" too: gcamstr() and gcamstr("") are one flyweight value (gcamstr.h:161; xml_parse_helper.cpp:802); NA: not a literal
                ifelse(vt %in% "Value" & dv %in% "unset" & neg0, "-0", NA_character_)))))) })    # -0: the unset bit pattern (value.h:161, 287-297, 488-493)
  rdv[rules$parent_class %in% c("ITechnology", "IInterpolationFunction", "location-info") | startsWith(rules$parent_class, "TechnologyContainer<")] <- NA   # family contexts
  st <- new.env(parent = emptyenv())
  st$ref <- st$int <- st$moves <- st$defaults <- st$wt <- st$stx <- st$need <- character(0); st$wp <- st$sp <- numeric(0); st$nr <- 0L; st$cnt <- numeric(0); st$skip <- NULL
  st$ans <- st$okl <- list(); st$used <- FALSE
  st$n <- c(values = 0, arrays = 0, new = 0, deletes = 0, rules = 0, keywords = 0, keymaps = 0, flags = 0, cbp = 0, wrappers = 0, restores = 0, rebuilds = 0, removals = 0)
  plain <- function(v) vapply(ifelse(is.na(v), "", v), function(z) rawToChar(charToRaw(z)), "", USE.NAMES = FALSE)
  nmax <- 2000L                                                              # places listed in a refusal; the rest are counted per reason
  # later: TRUE or "noconfig" (config= can decide it), FALSE (refused in any case), or an unknown answer of prefix_scan
  nope <- function(path, why, later = TRUE) {
    why <- paste0(why, if (is.list(later)) paste0("; ", later$why, ", so the state before the base file is not known") else if (!isFALSE(later)) "; config= can decide it from the configuration's earlier files")
    st$nr <- st$nr + 1L; k <- st$cnt[why]; st$cnt[why] <- if (is.na(k)) 1 else k + 1
    if (length(st$ref) < nmax) st$ref[length(st$ref) + 1L] <- paste0(plain(path), ": ", why)
  }
  cnt <- function(k, by = 1) st$n[k] <- st$n[k] + by
  ind <- function(d) strrep("    ", d - 1L)
  # answer to a question about the earlier files (prefix_scan's form; path: steps from <world>): from st$ans; "noconfig"
  # without config; untouched below a slot that does not exist after them (NA step); else NULL, queued in st$need for
  # the group's next pass
  need <- function(kind, path, field = NULL) {
    if (anyNA(path)) return(list(state = "untouched", value = switch(kind, exists = FALSE, siblings = character(0), NULL), why = ""))
    if (is.null(config)) return("noconfig")
    if (!length(path)) return(list(state = "unknown", value = NULL, why = "minaj asks nothing about <world> itself"))
    q <- paste(c(kind, paste(path, collapse = "\n"), field), collapse = "\t"); a <- st$ans[[q]]
    if (is.null(a)) { st$need <- c(st$need, q); return(NULL) }
    st$used <- TRUE; a
  }
  wait <- function(...) any(vapply(list(...), is.null, NA))                 # an answer still open: the slot waits for the next pass
  # one parsed file part: tokens (as a list), text, children per row (position row + 1), last row of each subtree,
  # canonical element strings (tag, sorted attributes, text) and the attribute names of each row; sh: lines before the block
  prep <- function(b, what, sh = 0L) {
    x <- unclass(tryCatch(xml_tokens(b), error = function(e) {
      m <- conditionMessage(e); l <- as.integer(regmatches(m, regexpr("(?<=, line )[0-9]+(?= \\(byte [0-9]+\\))", m, perl = TRUE)))
      if (sh && length(l) && l > sh) m <- sub(", line [0-9]+ \\(byte [0-9]+\\)", sprintf(", line %d of the block", l - sh), m)
      stop(what, ": ", m, call. = FALSE) })); n <- length(x$tag)
    s <- rawToChar(b); Encoding(s) <- "bytes"
    an <- sort(grep("^@", names(x), value = TRUE))
    ap <- if (length(an)) do.call(paste0, lapply(an, function(a) ifelse(is.na(x[[a]]), "", paste0(" ", substring(a, 2), "=\"", x[[a]], "\"")))) else ""
    nm <- if (length(an)) do.call(paste, c(lapply(an, function(a) ifelse(is.na(x[[a]]), "", substring(a, 2))), sep = " ")) else rep("", n)
    f <- x$parent + 1L; attr(f, "levels") <- as.character(0:n); class(f) <- "factor"
    list(x = x, s = s, kids = split(seq_len(n), f), end = findInterval(x$last, x$first),
         ecan = paste0(x$tag, ap, "\t", ifelse(is.na(x$text), "\001", x$text)), anm = trimws(gsub(" +", " ", nm)))
  }
  av <- function(D, r, a) { v <- D$x[[paste0("@", a)]]; if (is.null(v)) rep(NA_character_, length(r)) else v[r] }
  avs <- function(D, i, a) vapply(a, function(z) av(D, i, z), "", USE.NAMES = FALSE)
  raw1 <- function(D, r) if (length(r)) substring(D$s, D$x$first[r], D$x$last[r]) else character(0)
  piece <- function(D, r) paste0(ind(D$x$depth[r]), raw1(D, r), recycle0 = TRUE)
  canon <- function(D, r) vapply(r, function(i) { j <- i:D$end[i]; paste(paste0(D$x$depth[j] - D$x$depth[i], ":", D$ecan[j]), collapse = "\n") }, "")
  bk2 <- function(D, r, ri) { k <- kind[ri]; y <- av(D, r, "year")             # keys GCAM reads as a number (year), or matches to an entry made without one (name="")
    ((k %in% c("VEC_YEAR", "PERIOD") | (k %in% "STUB_QUEUE" & D$x$tag[r] == "period")) & !is.na(y) & !grepl("^[1-9][0-9]*$", y)) | (k %in% "VEC_NAMED" & av(D, r, "name") %in% "") }
  bkw <- function(k) if (k %in% "VEC_NAMED") "empty name (GCAM matches it to an entry created without a name)" else "year not written as a whole number without leading zeros (GCAM reads it as a number)"
  idattr <- function(D, i, a) { v <- avs(D, i, a); w <- !is.na(v); if (any(w)) paste0(" ", a[w], "=\"", v[w], "\"", collapse = "") else "" }
  # every element of the subtrees r of D, copied under an element of class cls at path, must be one GCAM parses
  vet <- function(D, r, cls, path) {
    if (!length(r)) return(invisible())
    J <- unlist(lapply(r, function(i) i:D$end[i]), use.names = FALSE); dd <- D$x$depth[J]; up <- match(D$x$parent[J], J)
    nv <- av(D, J, "name"); yv <- av(D, J, "year")
    lb <- paste0(D$x$tag[J], ifelse(!is.na(nv), paste0("[", nv, "]"), ifelse(!is.na(yv), paste0("[", yv, "]"), "")))
    pc <- cr <- rep(NA_character_, length(J)); pl <- rep(path, length(J))     # class of the parent, class created (""; a value), path of the parent
    for (d in sort(unique(dd))) {
      w <- which(dd == d)
      if (d > min(dd)) { pc[w] <- cr[up[w]]; pl[w] <- paste0(pl[up[w]], "/", lb[up[w]]) } else pc[w] <- cls
      for (q in w[pc[w] %in% ""]) nope(paste0(pl[q], "/", lb[q]), "child element under a value (GCAM reads values as text)", FALSE)
      w <- w[!is.na(pc[w]) & nzchar(pc[w])]; if (!length(w)) next
      ri <- match(paste(pc[w], D$x$tag[J[w]], sep = "\r"), rk)
      for (q in w[is.na(ri)]) nope(paste0(pl[q], "/", lb[q]), if (pc[q] == "CalcBasePrice") "child that GCAM's calc-base-price reader ignores" else
                                     paste0("tag unknown to GCAM in ", pc[q], " (GCAM skips it as 'Unknown tag')"), FALSE)
      cr[w] <- rules$creates[ri]
      for (q in w[bk2(D, J[w], ri)]) nope(paste0(pl[q], "/", lb[q]), bkw(kind[ri[match(q, w)]]), FALSE)
    }
  }
  # slot of each child: S simple, A array, C container (member + key), K keyword, R rules, M key map, F input flags,
  # T set-true, P coefficient, X unkeyed list (also entries without their key attribute) or scenario-level element,
  # E period without a year
  skey <- function(D, r, ri, cls) {
    if (!length(r)) return(character(0))
    k <- kind[ri]; tg <- D$x$tag[r]; m <- mem[ri]
    if (cls == "Scenario") k[tg != "world"] <- "APPEND"
    w <- k == "STUB_QUEUE"; if (any(w)) k[w] <- ifelse(tg[w] == "period", "PERIOD", ifelse(tg[w] == "interpolation-rule", "RULE", "SIMPLE"))
    key <- character(length(r))
    for (u in unique(k)) {
      w <- k == u
      key[w] <- switch(u, SIMPLE = paste0("S\r", tg[w]), ARRAY = paste0("A\r", tg[w]),
                       VEC_NAMED = paste0("C\r", m[w], "\r", av(D, r[w], "name")),
                       VEC_YEAR = , PERIOD = paste0("C\r", m[w], "\r", av(D, r[w], "year")), SINGLE = paste0("C\r", m[w]),
                       GTD_LOCATION = paste0("C\r", m[w], "\r", av(D, r[w], "sector-name"), "\r", av(D, r[w], "subsector-name")),
                       KEYWORD = paste0("K\r", m[w]), RULE = paste0("R\r", m[w]), KEYMAP = paste0("M\r", m[w]), OR = paste0("F\r", m[w]),
                       SETTRUE = paste0("T\r", tg[w]), REPLACE = paste0("P\r", ifelse(tg[w] %in% c("efficiency", "coefficient"), "coef", tg[w])),
                       APPEND = , ONCE = paste0("X\r", m[w]), paste0("U\r", m[w]))
    }
    w <- (k == "VEC_NAMED" & is.na(av(D, r, "name"))) | (k == "VEC_YEAR" & is.na(av(D, r, "year")))   # never matched: a new entry each time
    key[w] <- paste0("X\r", m[w], "\r#")
    w <- k == "PERIOD" & is.na(av(D, r, "year")); key[w] <- paste0("E\r", tg[w])
    key
  }
  # the children of B occurrences bo and O occurrences oo of one slot (class cls, identity attributes idn): returns the
  # pieces to write inside its wrapper (O positions, text), SIMPLE values to write as wrapper attributes, and rb: a
  # request to rebuild the slot (place, reasons; NULL: none). rmode: B's content against the state after the earlier
  # files (no O element); mpath: steps from <world> of the slot, for questions (NA: nothing below exists after them)
  walk <- function(cls, bo, oo, idn, path, stub, rmode = FALSE, mpath = character(0)) {
    B <- st$B; O <- st$O
    bk <- as.integer(unlist(B$kids[bo + 1L], use.names = FALSE)); ok <- as.integer(unlist(O$kids[oo + 1L], use.names = FALSE))
    pos <- numeric(0); out <- character(0); wat <- ""; wj <- integer(0); rb <- NULL; gi <- 0L; j <- 0L
    d1 <- (if (length(bo)) B$x$depth[bo[1]] else O$x$depth[oo[1]]) + 1L      # depth of the children
    add <- function(p, t) {                                                   # p NA: a generated piece, at the start of the first O element (slot tag j)
      if (anyNA(p)) { p <- (if (length(oo)) O$x$first[oo[1]] else 0) + (gi + seq_along(t)) / 2^16; gi <<- gi + length(t) }
      pos <<- c(pos, p); out <<- c(out, t); wj <<- c(wj, rep(j, length(t)))
    }
    rq <- match(paste(cls, B$x$tag[bk], sep = "\r", recycle0 = TRUE), rk); ro <- match(paste(cls, O$x$tag[ok], sep = "\r", recycle0 = TRUE), rk)
    for (t in unique(c(B$x$tag[bk[is.na(rq)]], O$x$tag[ok[is.na(ro)]]))) nope(paste0(path, "/", t), paste0("tag unknown to GCAM in ", cls, " (GCAM skips it as 'Unknown tag')"), FALSE)
    bk <- bk[!is.na(rq)]; rq <- rq[!is.na(rq)]; ok <- ok[!is.na(ro)]; ro <- ro[!is.na(ro)]
    aev <- function(D, occ) {                                                 # SIMPLE values given as attributes (set before the children)
      e <- list(f = character(0), v = character(0), p = numeric(0))
      if (all(D$anm[occ] %in% c("", if (length(idn) == 2L) "sector-name subsector-name" else idn))) return(e)
      for (i in occ) {
        a <- setdiff(strsplit(D$anm[i], " ", fixed = TRUE)[[1]], c("", "fillout", "delete", "nocreate", idn,
                     if (identical(idn, "year") && cls %in% c("ITechnology", rules$creates[kind %in% "PERIOD"])) "name"))   # a period's own name attribute: GCAM reads its technology's name in its place (technology_container.cpp:154)
        for (f in a[atr[match(paste(cls, a, sep = "\r", recycle0 = TRUE), rk)] %in% TRUE]) { e$f <- c(e$f, f); e$v <- c(e$v, av(D, i, f)); e$p <- c(e$p, D$x$first[i]) }
      }
      e
    }
    eb <- aev(B, bo); eo <- aev(O, oo)
    kc <- function(D, r, ri) for (i in which(bk2(D, r, ri))) nope(paste0(path, "/", D$x$tag[r[i]], "[", av(D, r[i], if (kind[ri[i]] == "VEC_NAMED") "name" else "year"), "]"), bkw(kind[ri[i]]), FALSE)
    kc(B, bk, rq); kc(O, ok, ro)
    kb <- skey(B, bk, rq, cls); ko <- skey(O, ok, ro, cls)
    sl <- unique(c(if (length(eo$f)) paste0("S\r", eo$f), ko, if (length(eb$f)) paste0("S\r", eb$f), kb))
    if (!length(sl)) return(list(pos = pos, out = out, wat = wat, rb = rb))
    ib <- match(kb, sl); io <- match(ko, sl); lv <- as.character(seq_along(sl))
    fz <- function(i) { attr(i, "levels") <- lv; class(i) <- "factor"; i }
    gb <- split(bk, fz(ib)); go <- split(ok, fz(io))
    rs <- ro[match(seq_along(sl), io)]; w <- is.na(rs); rs[w] <- rq[match(which(w), ib)]             # registry row of each slot
    one <- which(lengths(gb) == 1L & lengths(go) == 1L & !sl %in% paste0("S\r", c(eb$f, eo$f), recycle0 = TRUE))
    same <- logical(length(sl))                                                # one element in each file, same bytes
    if (length(one)) same[one] <- raw1(B, unlist(gb[one], use.names = FALSE)) == raw1(O, unlist(go[one], use.names = FALSE))
    vm <- vk <- vt <- xr <- xs <- character(0); vpb <- vpo <- numeric(0); vre <- vcr <- logical(0)    # vector entries, for order checks (xs: lists where a stub-technology the earlier files created is deleted and created again)
    sv <- function(D, r, e, f) {                                               # last write of SIMPLE field f: value and row (NA: attribute)
      w <- e$f == f; p <- c(D$x$first[r], e$p[w])
      if (!length(p)) return(NULL)
      i <- which.max(p); list(v = c(D$x$text[r], e$v[w])[i], r = c(r, rep(NA_integer_, sum(w)))[i])
    }
    flg <- function(D, r, k) {
      f <- rep("M", length(r)); if (k == "GTD_LOCATION") return(f)
      f[av(D, r, "nocreate") %in% "1"] <- "N"; f[av(D, r, "delete") %in% "1"] <- "D"; f
    }
    done <- logical(length(sl))
    jt <- match("S\rtype-flags", sl); jf <- which(startsWith(sl, "F\r"))
    if (!is.na(jt) && length(jf)) {                                          # type-flags sets the input flags and <flag> adds one: compared in order
      tfs <- function(D, g, e) {
        r <- unlist(g[c(jt, jf)], use.names = FALSE); a <- e$f == "type-flags"
        p <- c(D$x$first[r], e$p[a]); v <- c(D$x$text[r], e$v[a]); t <- c(D$x$tag[r] == "type-flags", rep(TRUE, sum(a)))
        o <- order(p); v <- v[o]; t <- t[o]; l <- max(c(0L, which(t)))
        paste(c(if (l) v[l] else "\001", sort(unique(v[seq_along(v) > l & !t]))), collapse = "\r")
      }
      if (!identical(tfs(B, gb, eb), tfs(O, go, eo))) nope(paste0(path, "/type-flags"), "type-flags and input flags differ (their order decides the flags)", FALSE)
      done[c(jt, jf)] <- TRUE
    }
    sq <- vector("list", length(sl) + 1L); xm <- xd <- integer(0)               # counts, order moves and restored defaults before each slot; moves and defaults dropped
    for (j in seq_along(sl)) {
      sq[[j]] <- list(st$n, length(st$moves), length(st$defaults))
      if (done[j]) next
      s <- sl[j]; ty <- substr(s, 1L, 1L)
      if (ty == "X" && endsWith(s, "\r#")) {                                  # entries without their key: each a new entry, in file order
        um <- function(D, r) r[!(av(D, r, "delete") %in% "1" | av(D, r, "nocreate") %in% "1")]   # flagged ones create nothing
        br <- um(B, gb[[j]]); orr <- um(O, go[[j]]); nb <- length(br); no <- length(orr)
        vm <- c(vm, rep(mem[rs[j]], nb + no)); vk <- c(vk, paste0(s, seq_len(nb), recycle0 = TRUE), paste0(s, seq_len(no), recycle0 = TRUE)); vt <- c(vt, B$x$tag[br], O$x$tag[orr])
        vcr <- c(vcr, rep(rules$order_policy[rs[j]] == "critical", nb + no)); vre <- c(vre, logical(nb + no))
        vpb <- c(vpb, B$x$first[br], rep(NA, no)); vpo <- c(vpo, rep(NA, nb), O$x$first[orr])
      }
      br <- gb[[j]]; orr <- go[[j]]; ri <- rs[j]
      tg <- if (length(orr)) O$x$tag[orr[1]] else if (length(br)) B$x$tag[br[1]] else substring(s, 3L)
      lab <- paste0(path, "/", tg)
      if (s == "S\rname" && identical(idn, "year") && cls %in% c("ITechnology", rules$creates[kind %in% "PERIOD"])) {   # each later read of the period names the vintage after its technology first (technology_container.cpp:154)
        nope(lab, "<name> inside a technology's period (a later read of the period gives the vintage the technology's name again)", FALSE); next
      }
      if (s == "S\rname" && (startsWith(cls, "TechnologyContainer<") || cls == "StubTechnologyContainer")) {   # each period read takes the technology's name then (technology_container.cpp:154): one name for every read in both runs
        w0 <- "technology renamed (each period keeps the name the technology had when GCAM last read that period)"
        if (anyNA(B$x$text[br]) || anyNA(O$x$text[orr])) { nope(lab, "value element with child elements", FALSE); next }
        xb <- c(av(B, bo, "name"), B$x$text[br]); pb <- c(B$x$first[bo], B$x$first[br])[!is.na(xb)]; xb <- xb[!is.na(xb)]   # name attributes (set before the children) and <name> children
        xo <- c(av(O, oo, "name"), O$x$text[orr]); po <- c(O$x$first[oo], O$x$first[orr])[!is.na(xo)]; xo <- xo[!is.na(xo)]
        if (length(unique(c(xb, xo))) > 1L) { nope(lab, w0, FALSE); next }
        if (!length(xb) || !length(xo) || min(pb) > min(c(Inf, B$x$first[bk[B$x$tag[bk] == "period"]])) || min(po) > min(c(Inf, O$x$first[ok[O$x$tag[ok] == "period"]]))) {
          if ("name" %in% idn) v <- c(av(B, bo, "name"), av(O, oo, "name"))[1] else {   # a period read, or the end, under the name from before these files: matched by name, else the earlier files' ("" if none)
            a <- need("touched", mpath, "name"); if (wait(a)) next
            if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
            v <- if (a$state == "touched") unname(a$value) else ""
          }
          if (!identical(unique(c(xb, xo)), v)) { nope(lab, w0, FALSE); next }
        } else if (!is.null(config) && !"name" %in% idn && !anyNA(mpath) &&
                   length(setdiff(av(B, bk[B$x$tag[bk] == "period"], "year"), av(O, ok[O$x$tag[ok] == "period"], "year")))) {   # a year only the base file reads: the add-on
          a <- need("touched", mpath, "name"); h <- need("exists", mpath); if (wait(a, h)) next   # reads it again under this name, the override's run keeps the earlier files'
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }   # (one name for them; none: it does not exist after them)
          if (!is.list(h) || h$state == "unknown") { nope(lab, w0, h); next }
          if (isTRUE(h$value) && !identical(unique(c(xb, xo)), if (a$state == "touched") unname(a$value) else "")) { nope(lab, w0, FALSE); next }
        }
        next
      }
      if (same[j] && ty != "C") next
      if (ty == "S") {
        if (anyNA(B$x$text[br]) || anyNA(O$x$text[orr])) { nope(lab, "value element with child elements", FALSE); next }
        vb <- sv(B, br, eb, tg); vo <- sv(O, orr, eo, tg)
        if (is.null(vo)) {                                                     # set only in the base file: the earlier files' value, else the default (9a)
          w0 <- "value set only in the base file (un-set)"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("touched", mpath, tg); h <- need("creator", mpath)
          f2 <- if (tg == "type-flags" && !is.na(match(paste(cls, "flag", sep = "\r"), rk))) need("touched", mpath, "flag") else FALSE   # type-flags and <flag> set one int (minicam_input.h:205, minicam_input.cpp:85-96)
          if (wait(a, h, f2)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          if (h$state == "unknown") { nope(lab, w0, h); next }
          if (is.list(f2) && f2$state == "unknown") { nope(lab, w0, f2); next }
          if (is.list(f2) && (a$state == "touched" || f2$state == "touched")) {
            nope(lab, paste0(w0, "; an earlier file", if (a$state == "touched") paste0(" (", names(a$value)[1], ")"), " writes the input flags (type-flags or <flag>), which the add-on cannot restore"), FALSE); next
          }
          v <- if (h$state == "touched") h$value[mpath[length(mpath)]] else NA   # the holder's class: the earlier file's, else the class it has here
          i <- match(paste(if (is.na(v)) cls else sub("\t.*", "", v), tg, sep = "\r"), rk)
          v <- if (a$state == "touched") unname(a$value) else rdv[i]
          if (is.na(v)) {
            if (rules$parent_class[i] %in% c("ITechnology", "IInterpolationFunction", "location-info") || grepl("^TechnologyContainer<", rules$parent_class[i])) { nope(lab, paste0(w0, "; minaj restores no default inside ", rules$parent_class[i]), FALSE); next }
            rb <- c(rb, list(c(lab, paste0(w0, if (rules$value_type[i] %in% "Value" && rules$default[i] %in% "unset") "; minaj restores an unset value only with option minaj.addon_neg0 = TRUE" else "; minaj knows no default it can restore")))); next
          }
          if (a$state != "touched") st$defaults <- c(st$defaults, paste0(plain(path), " ", tg, " = ", v))
          v <- paste0("<", tg, ">", v, "</", tg, ">"); add(NA, paste0(ind(d1), v)); st$okl <- c(st$okl, list(c(path, cls, v))); cnt("restores"); next
        }
        if (!is.null(vb) && identical(vb$v, vo$v)) next
        if (stub && tg %in% c("output-scalar", "current-coef")) { nope(lab, "differs inside a stub-technology (storage shared between regions)", FALSE); next }
        if (is.na(vo$r)) wat <- paste0(wat, " ", tg, "=\"", vo$v, "\"") else add(O$x$first[vo$r], piece(O, vo$r))
        cnt("values")
      } else if (ty == "E") {
        nope(lab, "period without a year attribute (GCAM stops when it reads one)", FALSE)
      } else if (ty == "C") {
        k <- kind[ri]; nr <- rules$never_rebuild[ri] == "TRUE"; op <- rules$order_policy[ri]
        idc <- switch(k, VEC_NAMED = "name", VEC_YEAR = , PERIOD = , STUB_QUEUE = "year", GTD_LOCATION = c("sector-name", "subsector-name"), character(0))
        fb <- flg(B, br, k); fo <- flg(O, orr, k)
        dB <- max(c(0L, which(fb == "D"))); dO <- max(c(0L, which(fo == "D")))
        if (length(idc) && op %in% c("append", "critical")) {                 # position where each file (re)creates the entry
          made <- function(D, r, f, d) { w <- which(seq_along(r) > d & f == "M"); if (length(w)) D$x$first[r[w[1]]] else NA }
          vm <- c(vm, mem[ri]); vk <- c(vk, s); vt <- c(vt, tg); vcr <- c(vcr, op == "critical"); vre <- c(vre, FALSE)
          vpb <- c(vpb, made(B, br, fb, dB)); vpo <- c(vpo, made(O, orr, fo, dO))
        }
        if (same[j]) next
        ky0 <- if (length(orr)) avs(O, orr[1], idc) else avs(B, br[1], idc)  # the slot's key, its step and a delete sibling with B's tag
        if (length(idc)) lab <- paste0(lab, "[", paste(ky0, collapse = ", "), "]")
        mp <- if (cls == "Scenario") character(0) else c(mpath, paste0(msrc[ri], "\r", if (k %in% c("VEC_YEAR", "PERIOD", "STUB_QUEUE")) suppressWarnings(as.integer(ky0)) else paste(ky0, collapse = "\r")))
        dl <- if (length(br)) paste0("<", B$x$tag[br[1]], idattr(B, br[1], idc), " delete=\"1\"/>") else ""
        if (!length(orr)) {                                                    # only in the base file (9e)
          if (nr && !all(fb == "N")) { nope(lab, if (cls == "World" && tg == "region") "region only in the base file (addon_minaj never deletes a region: a delete would also remove what earlier files wrote there)"
                                                 else "only in the base file (removal), and addon_minaj never deletes this container (a delete would also remove what earlier files wrote there)", FALSE); next }
          w0 <- "only in the base file (removal)"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          if (any(B$x$tag[br] == "stub-technology" & fb == "M" & seq_along(br) > dB)) {   # deleted, then the earlier files' childless creation again
            a <- need("stub", mp); if (wait(a)) next
            if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
            if (a$state == "touched") { nope(lab, paste0(w0, "; an earlier file (", a$value, ") queued nodes on it, which the add-on cannot restore"), FALSE); next }
            v <- c(dl, if (identical(a$value, "created")) paste0("<stub-technology", idattr(B, br[1], idc), "/>")); if (identical(a$value, "created")) xs <- c(xs, mem[ri])
            add(NA, paste0(ind(d1), v)); st$okl <- c(st$okl, lapply(v, function(z) c(path, cls, z))); cnt("removals"); next
          }
          if (dB) {                                                            # deleted by B: nothing to undo where the earlier files have nothing
            a <- need("touched", mp); if (wait(a)) next
            if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
            if (a$state == "touched") { nope(lab, paste0(w0, "; an earlier file (", names(a$value)[1], ") has content there, which the add-on cannot restore without replaying earlier files (not supported yet)"), FALSE); next }
            if (any(fb[seq_along(fb) > dB] == "M")) { add(NA, paste0(ind(d1), dl)); st$okl <- c(st$okl, list(c(path, cls, dl))); cnt("removals") }
            next
          }
          a <- need(if (all(fb == "N")) "exists" else "touched", mp); h <- need("creator", mp); if (wait(a, h)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          if (h$state == "unknown") { nope(lab, w0, h); next }
          if (a$state == "untouched") {                                        # absent after the earlier files: B's nocreate elements did nothing, else B created it
            if (!all(fb == "N")) { add(NA, paste0(ind(d1), dl)); st$okl <- c(st$okl, list(c(path, cls, dl))); cnt("removals") }
            next
          }
          tgs <- unique(B$x$tag[br]); cc <- unique(rules$creates[match(paste(cls, tgs, sep = "\r"), rk)])   # present after them: B's content undone in a wrapper
          if (length(cc) > 1) {
            if (all(startsWith(cc, "TechnologyContainer<") | cc == "StubTechnologyContainer")) cc <- "StubTechnologyContainer"
            else if (fb[1] == "M") cc <- rules$creates[ri] else { nope(lab, paste0(w0, "; written with different types (", paste(tgs, collapse = ", "), ")"), FALSE); next }
          }
          v <- if (h$state == "touched") h$value[mp[length(mp)]] else NA; bc <- rules$creates[match(paste(cls, B$x$tag[br[match("M", fb)]], sep = "\r"), rk)]
          if (!is.na(v) && sub("\t.*", "", v) != (if (is.na(bc)) cc else bc)) {
            pc <- sub("\t.*", "", v); nope(lab, paste0(w0, "; an earlier file (", sub("^[^\t]*\t", "", v), ") created it as ", rules$tag[rules$parent_class == cls & rules$creates == pc][1], " (", pc, ")"), FALSE); next
          }
          res <- walk(cc, br, integer(0), idc, lab, cc == "StubTechnologyContainer", TRUE, mp)
          if (length(res$rb)) { for (z in res$rb) nope(z[1], paste0(paste(z[-1], collapse = "; "), "; ", lab, " is not rebuilt (an earlier file has content there)"), FALSE); next }
          if (length(res$out)) {                                               # a wrapper without an O element, at the end of the last O element
            gi <- gi + 1L; t1 <- B$x$tag[br[1]]
            add((if (length(oo)) O$x$last[oo[length(oo)]] - 1 else 0) + gi / 2^16, paste0(ind(d1), "<", t1, idattr(B, br[1], idc), " nocreate=\"1\">\n",
                                                                                       paste(res$out[order(res$pos)], collapse = "\n"), "\n", ind(d1), "</", t1, ">"))
            cnt("wrappers")
          }
          next
        }
        if (!length(br)) {
          if (cls == "World" && tg == "region") { nope(lab, "new region", FALSE); next }
          vet(O, orr, cls, path); add(O$x$first[orr], piece(O, orr)); cnt("new"); next
        }
        if (identical(paste(raw1(B, br), collapse = "\n"), paste(raw1(O, orr), collapse = "\n"))) next   # the same elements in both files: the same effect
        if (dO) {
          if (nr) { nope(lab, "deleted by the override, and addon_minaj never deletes or rebuilds this container", FALSE); next }
          ao <- orr[seq_along(orr) > dO]
          if (dB && identical(canon(B, br[seq_along(br) > dB]), canon(O, ao))) next
          d <- orr[dO]; vet(O, ao, cls, path)
          add(O$x$first[d], paste0(ind(O$x$depth[d]), "<", O$x$tag[d], idattr(O, d, idc), " delete=\"1\"/>"))
          if (length(ao)) add(O$x$first[ao], piece(O, ao))
          if (length(vre) && identical(vk[length(vk)], s)) vre[length(vre)] <- TRUE
          cnt("deletes"); next
        }
        if (dB) {                                                              # deleted in B only: the delete, then O's elements, where the earlier files have nothing (9g)
          w0 <- "deleted in the base file but not in the override"
          if (nr) { nope(lab, paste0(w0, ", and addon_minaj never deletes or rebuilds this container"), FALSE); next }
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("touched", mp); if (wait(a)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          if (a$state == "touched") { nope(lab, paste0(w0, "; an earlier file (", names(a$value)[1], ") has content there, which the add-on cannot restore without replaying earlier files (not supported yet)"), FALSE); next }
          v <- paste0("<", tg, idattr(O, orr[1], idc), " delete=\"1\"/>"); vet(O, orr, cls, path)
          add(O$x$first[orr[1]], paste0(ind(O$x$depth[orr[1]]), v)); add(O$x$first[orr], piece(O, orr)); st$okl <- c(st$okl, list(c(path, cls, v)))
          if (length(vre) && identical(vk[length(vk)], s)) vre[length(vre)] <- TRUE
          cnt("rebuilds"); next
        }
        if (B$x$tag[br[1]] != tg) {                                            # type change: deleted, then O's elements, where no earlier file made it (9f)
          w0 <- paste0("type change (", B$x$tag[br[1]], " to ", tg, ")")
          if (nr) { nope(lab, w0, FALSE); next }
          if (stub || rules$deletable[ri] != "TRUE") { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("touched", mp); h <- need("creator", mp); if (wait(a, h)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          if (h$state == "unknown") { nope(lab, w0, h); next }
          v <- if (h$state == "touched") h$value[mp[length(mp)]] else NA
          if (!is.na(v)) { pc <- sub("\t.*", "", v); nope(lab, paste0(w0, "; an earlier file (", sub("^[^\t]*\t", "", v), ") created it as ", rules$tag[rules$parent_class == cls & rules$creates == pc][1], " (", pc, ")"), FALSE); next }
          if (a$state == "touched") { nope(lab, paste0(w0, "; an earlier file (", names(a$value)[1], ") has content there"), FALSE); next }
          vet(O, orr, cls, path); add(O$x$first[orr[1]], paste0(ind(B$x$depth[br[1]]), dl)); add(O$x$first[orr], piece(O, orr)); st$okl <- c(st$okl, list(c(path, cls, dl)))
          if (length(vre) && identical(vk[length(vk)], s)) vre[length(vre)] <- TRUE
          cnt("rebuilds"); next
        }
        if (!(fb[1] == "M" && fo[1] == "M") && !(all(fb == "N") && all(fo == "N"))) {   # the first element creates it in both files, or none does; else it depends on the earlier files (9h)
          w0 <- if (identical(fb, fo)) "nocreate pattern: nocreate=\"1\" comes before the first element that creates it, so the result depends on earlier files" else "nocreate differs between the base file and the override"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("exists", mp); if (wait(a)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          if (!isTRUE(a$value)) {                                              # absent after them: each file's elements from its first creating one
            br <- br[seq_along(br) >= match("M", fb, nomatch = length(br) + 1L)]; orr <- orr[seq_along(orr) >= match("M", fo, nomatch = length(orr) + 1L)]
            fb <- flg(B, br, k); fo <- flg(O, orr, k)
            if (!length(br) && !length(orr)) next
            if (!length(br)) { vet(O, orr, cls, path); add(O$x$first[orr], piece(O, orr)); cnt("new"); next }
            if (!length(orr)) { add(NA, paste0(ind(d1), dl)); st$okl <- c(st$okl, list(c(path, cls, dl))); cnt("removals"); next }
            if (B$x$tag[br[1]] != O$x$tag[orr[1]] && !nr && rules$deletable[ri] == "TRUE") {   # the first creating elements differ in type: deleted, then O's elements (9f)
              v <- paste0("<", B$x$tag[br[1]], idattr(B, br[1], idc), " delete=\"1\"/>"); vet(O, orr, cls, path)
              add(O$x$first[orr[1]], paste0(ind(O$x$depth[orr[1]]), v)); add(O$x$first[orr], piece(O, orr)); st$okl <- c(st$okl, list(c(path, cls, v)))
              if (length(vre) && identical(vk[length(vk)], s)) vre[length(vre)] <- TRUE
              cnt("rebuilds"); next
            }
            mp <- c(mp, NA)                                                    # nothing below exists after them
          }
        }
        tgs <- unique(c(B$x$tag[br], O$x$tag[orr])); cc <- unique(rules$creates[match(paste(cls, tgs, sep = "\r"), rk)])
        if (length(cc) > 1) {                                                  # one name written with several tags (matched by name)
          if (all(startsWith(cc, "TechnologyContainer<") | cc == "StubTechnologyContainer")) cc <- "StubTechnologyContainer"   # technology and stub-technology: rows valid for any technology class
          else if (fb[1] == "M") cc <- rules$creates[ri]                       # created by the same first tag in both files: later tags parse onto its class
          else { nope(lab, paste0("written with different types (", paste(tgs, collapse = ", "), ")"), FALSE); next }
        }
        if (length(st$ans) && !stub && length(mp) && !anyNA(mp) && !(cls == "World" && length(unique(rules$creates[mem == mem[ri]])) == 1L)) {   # groups that read the earlier files: the class they created it with (not asked for a member of <world> with one class)
          h <- need("creator", mp)
          if (wait(h)) { walk(cc, br, orr, idc, lab, stub || cc == "StubTechnologyContainer", FALSE, mp); next }   # this pass is compared again: the questions below are asked now
          if (h$state == "unknown") { nope(lab, "an earlier file may have created it with another class", h); next }
          v <- if (h$state == "touched") h$value[mp[length(mp)]] else NA
          bc <- rules$creates[match(paste(cls, c(B$x$tag[br[fb == "M"]], O$x$tag[orr[fo == "M"]])[1], sep = "\r"), rk)]
          if (!is.na(v) && sub("\t.*", "", v) != (if (is.na(bc)) cc else bc)) {
            pc <- sub("\t.*", "", v); nope(lab, paste0("an earlier file (", sub("^[^\t]*\t", "", v), ") created it as ", rules$tag[rules$parent_class == cls & rules$creates == pc][1], " (", pc, ")"), FALSE); next
          }
        }
        ns <- list(st$n, length(st$moves), length(st$okl), length(st$defaults))   # before the child walk: a rebuild replaces what it counted
        res <- walk(cc, br, orr, idc, lab, stub || cc == "StubTechnologyContainer", FALSE, mp)
        if (length(res$rb)) {                                                  # requests to rebuild this slot: the delete, then O's elements
          w0 <- vapply(res$rb, function(z) paste(z[-1], collapse = "; "), ""); r1 <- vapply(res$rb, `[`, "", 1L)
          if (cc == "StubTechnologyContainer" && !stub) {                      # stub replace (9l)
            a <- need("stub", mp); if (wait(a)) next
            if (!is.list(a) || a$state == "unknown") { for (z in seq_along(w0)) nope(r1[z], w0[z], a); next }
            if (a$state == "touched" || any(c(fb, fo) == "N")) {
              for (z in seq_along(w0)) nope(r1[z], paste0(w0[z], "; inside a stub-technology, only changed values, new rows, prefix rule extensions, new keyword keys and the override's own deletes can be written; a stub replace is not allowed because ",
                                                          if (a$state == "touched") paste0("an earlier file (", a$value, ") queued nodes on it") else "the base file or the override writes it with nocreate"), FALSE)
              next
            }
            v <- paste0("<stub-technology", idattr(O, orr[1], idc), " delete=\"1\"/>"); if (identical(a$value, "created")) xs <- c(xs, mem[ri])
          } else if (!stub && !nr && rules$deletable[ri] == "TRUE") {          # holder rebuild (9f), where the earlier files have nothing
            a <- need("touched", mp); h <- need("creator", mp); if (wait(a, h)) next
            if (!is.list(a)) { for (z in seq_along(w0)) nope(r1[z], w0[z], a); next }
            if (a$state != "untouched" || h$state != "untouched") {            # refused here: the earlier files reach every ancestor too
              u <- if (a$state == "unknown") a else if (h$state == "unknown") h else FALSE
              for (z in seq_along(w0)) nope(r1[z], paste0(w0[z], "; ", lab, " is not rebuilt", if (isFALSE(u)) paste0(" (an earlier file (", if (a$state == "touched") names(a$value)[1] else sub("^[^\t]*\t", "", h$value[length(h$value)]), ") has content there)")), u)
              next
            }
            v <- dl
          } else { if (nr) for (z in seq_along(w0)) nope(r1[z], w0[z], FALSE) else rb <- c(rb, res$rb); next }   # never rebuilt: refused; else the requests climb
          st$n <- ns[[1]]; st$moves <- st$moves[seq_len(ns[[2]])]; st$okl <- st$okl[seq_len(ns[[3]])]; st$defaults <- st$defaults[seq_len(ns[[4]])]
          vet(O, orr, cls, path); add(O$x$first[orr[1]], paste0(ind(O$x$depth[orr[1]]), v)); add(O$x$first[orr], piece(O, orr)); st$okl <- c(st$okl, list(c(path, cls, v)))
          if (length(vre) && identical(vk[length(vk)], s)) vre[length(vre)] <- TRUE
          cnt("rebuilds"); next
        }
        if (cls == "Scenario" && tg == "world") {
          if (nzchar(res$wat)) nope(lab, "attributes of <world> differ", FALSE)
          st$wp <- c(st$wp, res$pos + st$off); st$wt <- c(st$wt, res$out); next
        }
        if (!length(res$out) && !nzchar(res$wat)) next
        oc <- findInterval(res$pos, O$x$first[orr]); us <- sort(unique(c(if (nzchar(res$wat)) 1L, oc)))
        for (u in us) {                                                        # one wrapper per override element, with its own tag
          o1 <- orr[u]; sp <- ind(O$x$depth[o1]); t1 <- O$x$tag[o1]; w <- oc == u
          stag <- paste0("<", t1, idattr(O, o1, idc), if (u == us[1]) res$wat, if (fo[u] == "N" || (guard && k != "GTD_LOCATION" && !stub)) " nocreate=\"1\"")
          add(O$x$first[o1], if (any(w)) paste0(sp, stag, ">\n", paste(res$out[w][order(res$pos[w])], collapse = "\n"), "\n", sp, "</", t1, ">") else paste0(sp, stag, "/>"))
          cnt("wrappers")
        }
      } else if (ty == "A") {
        if (identical(canon(B, br), canon(O, orr))) next
        if (stub && tg %in% c("output-scalar", "current-coef")) { nope(lab, "differs inside a stub-technology (storage shared between regions)", FALSE); next }
        ya <- av(B, br, "year"); fa <- av(B, br, "fillout") %in% "1"; yo <- paste(av(O, orr, "year"), av(O, orr, "fillout") %in% "1")
        if (!all(is.na(ya) | paste(ya, fa) %in% yo | (!fa & paste(ya, TRUE) %in% yo))) {   # no year: sets nothing; years only B sets: the earlier files' values, else the default (9b)
          w0 <- "years set only in the base file"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          if (is.null(config)) { nope(lab, w0); next }
          if (is.null(yrs)) { nope(lab, paste0(w0, "; the model years are not known (", ynote, ")"), FALSE); next }
          yb <- c(ya, av(O, orr, "year")); if (!all(is.na(yb) | grepl("^[-+]?[0-9]{1,9}$", yb))) { nope(lab, paste0(w0, "; a year GCAM may read otherwise"), FALSE); next }
          yo <- av(O, orr, "year"); fo <- av(O, orr, "fillout") %in% "1"         # model years each file sets: a model year, with fillout every later one (xml_parse_helper.cpp:696-706)
          yb <- unlist(lapply(seq_along(br), function(i) { y <- as.numeric(ya[i]); if (is.na(y) || !y %in% yrs) NULL else if (fa[i]) yrs[yrs >= y] else y }))
          yo <- unlist(lapply(seq_along(orr), function(i) { y <- as.numeric(yo[i]); if (is.na(y) || !y %in% yrs) NULL else if (fo[i]) yrs[yrs >= y] else y }))
          ry <- sort(unique(setdiff(yb, yo)))
          if (!length(ry)) st$used <- TRUE else {                              # no model year left to restore: the configuration's model years decided it
            a <- need("touched", mpath, tg); h <- need("creator", mpath); if (wait(a, h)) next
            if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
            if (h$state == "unknown") { nope(lab, w0, h); next }
            pw <- if (a$state == "touched") a$value else list(y = numeric(0), f = logical(0), v = character(0))
            v <- vapply(ry, function(y) { w <- which((pw$y == y | (pw$f & pw$y < y)) & pw$y %in% yrs); if (length(w)) pw$v[max(w)] else NA_character_ }, "")
            i <- if (h$state == "touched" && !is.na(h$value[mpath[length(mpath)]])) sub("\t.*", "", h$value[mpath[length(mpath)]]) else cls
            i <- match(paste(i, tg, sep = "\r"), rk); dy <- is.na(v); v[dy] <- rdv[i]
            if (anyNA(v)) {
              if (rules$parent_class[i] %in% c("ITechnology", "IInterpolationFunction", "location-info") || grepl("^TechnologyContainer<", rules$parent_class[i])) { nope(lab, paste0(w0, "; minaj restores no default inside ", rules$parent_class[i]), FALSE); next }
              rb <- c(rb, list(c(lab, paste0(w0, if (rules$value_type[i] %in% "Value" && rules$default[i] %in% "unset") "; minaj restores an unset value only with option minaj.addon_neg0 = TRUE" else "; minaj knows no default it can restore")))); next
            }
            st$defaults <- c(st$defaults, paste0(plain(path), " ", tg, "[", ry[dy], "] = ", v[dy], recycle0 = TRUE))
            v <- paste0("<", tg, " year=\"", ry, "\">", v, "</", tg, ">", recycle0 = TRUE)
            if (length(v) && length(orr)) { add(O$x$first[orr[1]] - 1 + (gi + seq_along(v)) / 2^16, paste0(ind(d1), v)); gi <- gi + length(v) } else if (length(v)) add(NA, paste0(ind(d1), v))
            st$okl <- c(st$okl, lapply(v, function(z) c(path, cls, z))); cnt("restores", length(v))
          }
        }
        if (length(orr)) { add(O$x$first[orr], piece(O, orr)); cnt("arrays") }
      } else if (ty == "K") {
        kw <- function(D, r) { v <- character(0); for (i in r) for (a in strsplit(D$anm[i], " ", fixed = TRUE)[[1]]) if (nzchar(a) && !a %in% names(v)) v[a] <- av(D, i, a); v }
        mb <- kw(B, br); mo <- kw(O, orr)
        if (!all(names(mb) %in% names(mo)) || any(mb != mo[names(mb)])) {     # a key B sets that O drops or changes: the holder rebuilt (9i)
          w0 <- "keyword dropped or changed (GCAM keeps the first value)"
          if (rmode) {                                                         # B's keys against the earlier files': nothing to undo where they set each key first
            a <- need("keywords", mpath); if (wait(a)) next
            if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
            if (all(names(mb) %in% names(a$value))) next
          }
          rb <- c(rb, list(c(lab, w0)))
          next
        }
        nw <- mo[!names(mo) %in% names(mb)]
        if (!length(nw)) next
        add(O$x$first[orr[1]], paste0(ind(O$x$depth[orr[1]]), "<keyword", paste0(" ", names(nw), "=\"", nw, "\"", collapse = ""), "/>"))
        cnt("keywords")
      } else if (ty == "R") {
        clr <- function(D, r) av(D, r, "apply-to") %in% "share-weight" & av(D, r, "delete") %in% "1"
        cb <- canon(B, br); co <- canon(O, orr)
        if (identical(cb, co)) next
        lb <- max(c(0L, which(clr(B, br)))); lo <- max(c(0L, which(clr(O, orr))))
        if (lo) {
          if (lb && identical(cb[lb:length(cb)], co[lo:length(co)])) next
          r <- orr[lo:length(orr)]
        } else if (!lb && length(cb) < length(co) && identical(cb, co[seq_along(cb)])) { r <- orr[seq_along(orr) > length(cb)]
        } else {                                                               # the earlier files' rules from their last clear, then O's, the first one clearing (9j)
          w0 <- if (lb) "the base file clears the rules and the override does not" else "rule list changed (not an extension of the base file's list)"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("rules", mpath); if (wait(a)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          x <- c(if (a$state == "touched") unname(a$value), raw1(O, orr)); np <- length(x) - length(orr)
          if (!length(x)) {                                                    # none left: a rule that sets nothing, over the first two model years (interpolation_rule.cpp:209-222, 225-237, 264-280, 315)
            if (length(yrs) < 2L) { nope(lab, paste0(w0, "; the model years are not known (", if (nzchar(ynote)) ynote else "fewer than two", ")"), FALSE); next }
            x <- sprintf("<%s apply-to=\"share-weight\" delete=\"1\" from-year=\"%d\" to-year=\"%d\"><from-value>1</from-value><to-value>1</to-value><interpolation-function name=\"linear\"/></%s>", tg, yrs[1], yrs[2], tg); np <- 1L
          } else {                                                             # the clear: apply-to exactly share-weight, delete exactly 1 (subsector.cpp:128-131, technology_container.cpp:229-235)
            s0 <- regmatches(x[1], regexpr("^<[^>]*>", x[1])); m <- gregexpr("[ \t\r\n]([A-Za-z_:][-A-Za-z0-9_:.]*)[ \t\r\n]*=[ \t\r\n]*\"([^\"]*)\"", s0, perl = TRUE)[[1]]
            an <- av0 <- character(0)
            if (m[1] > 0) { cs <- attr(m, "capture.start"); cl <- attr(m, "capture.length"); an <- substring(s0, cs[, 1], cs[, 1] + cl[, 1] - 1L); av0 <- substring(s0, cs[, 2], cs[, 2] + cl[, 2] - 1L) }
            u <- !an %in% c("apply-to", "delete")
            x[1] <- paste0("<", sub("^<([^ \t\r\n/>]+).*$", "\\1", s0), " apply-to=\"share-weight\" delete=\"1\"", paste0(" ", an[u], "=\"", av0[u], "\"", collapse = "", recycle0 = TRUE),
                           if (endsWith(s0, "/>")) "/>" else ">", substring(x[1], nchar(s0) + 1L))
          }
          st$okl <- c(st$okl, lapply(x[seq_len(max(np, 1L))], function(z) c(path, cls, z))); x <- paste0(ind(d1), x)
          if (np && length(orr)) { add(O$x$first[orr[1]] - 1 + (gi + seq_len(np)) / 2^16, x[seq_len(np)]); gi <- gi + np } else if (np) add(NA, x[seq_len(np)])
          if (length(orr)) { vet(O, orr, cls, path); add(O$x$first[orr], x[seq_along(x) > np]) }
          cnt("rules"); next
        }
        vet(O, r, cls, path); add(O$x$first[r], piece(O, r)); cnt("rules")
      } else if (ty == "M") {                                                 # keys compared as GCAM reads them: tax and price as double, year as unsigned int
        ka <- rules$key_attribute[ri]
        nk <- function(k) if (ka %in% c("tax", "price")) ifelse(grepl("^-?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]{1,2})?$", k) & nchar(gsub("^-?0*|[.]|[eE].*$", "", k)) <= 15L, sprintf("%.17g", suppressWarnings(as.numeric(k)) + 0), NA)
                            else if (ka == "year") ifelse(grepl("^[0-9]+$", k) & !grepl("^0+$", k) & nchar(sub("^0+", "", k)) <= 9L, sub("^0+", "", k), NA) else ifelse(is.na(k), "", k)
        kv <- function(D, r) { k <- av(D, r, ka); list(k = nk(k), v = vapply(seq_along(r), function(i) sub(paste0(" ", ka, "=\"", k[i], "\""), "", D$ecan[r[i]], fixed = TRUE, useBytes = TRUE), "")) }
        mb <- kv(B, br); mo <- kv(O, orr)
        if (anyNA(c(mb$k, mo$k))) { nope(lab, paste0(ka, " missing or not a plain ", if (ka == "year") "whole number above 0" else "decimal number of at most 15 digits", " (GCAM reads it as a number)"), FALSE); next }
        lb <- !duplicated(mb$k, fromLast = TRUE); lo <- !duplicated(mo$k, fromLast = TRUE)
        vb <- setNames(mb$v[lb], mb$k[lb]); vo <- setNames(mo$v[lo], mo$k[lo])
        if (!all(names(vb) %in% names(vo))) {                                  # keys only B sets: the earlier files' entries, assigned per key (9n; mac_control.cpp:136,
          if (tg == "PrimaryFuelCO2Coef") { nope(lab, "key removed (GCAM can only overwrite these keys)", FALSE); next }   # fractional_secondary_output.cpp:130, residue_biomass_output.cpp:364, land_use_history.cpp:75)
          w0 <- "key removed"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("keymap", mpath, tg); if (wait(a)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          pk <- if (a$state == "touched") a$value else character(0)
          if (length(pk) && ka %in% c("tax", "price")) names(pk) <- sprintf("%.17g", as.numeric(names(pk)) + 0)   # prefix_scan's exact decimal, as nk() gives it
          k0 <- setdiff(names(vb), names(vo))
          if (!all(k0 %in% names(pk))) { rb <- c(rb, list(c(lab, w0))); next }
          add(NA, paste0(ind(d1), pk[k0])); st$okl <- c(st$okl, lapply(unname(pk[k0]), function(z) c(path, cls, z))); cnt("restores", length(k0))
        }
        ch <- !names(vo) %in% names(vb) | vo != vb[names(vo)]
        if (any(ch)) { r <- orr[lo][ch]; add(O$x$first[r], piece(O, r)); cnt("keymaps", sum(ch)) }
      } else if (ty == "F") {
        xb <- B$x$text[br]; xo <- O$x$text[orr]
        if (all(xo %in% xb) && all(xb %in% xo)) next
        if (!all(xb %in% xo)) {                                                # flags only B sets: nothing to undo where the earlier files set them (9n; minicam_input.cpp:87-95)
          w0 <- "input flag set only in the base file"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("touched", mpath, tg); if (wait(a)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          if (!all(xb[!xb %in% xo] %in% (if (a$state == "touched") a$value))) { rb <- c(rb, list(c(lab, w0))); next }
        }
        r <- orr[!xo %in% xb & !duplicated(xo)]; add(O$x$first[r], piece(O, r)); cnt("flags", length(r))
      } else if (ty == "T") {
        if (length(br) && !length(orr)) {                                      # set only in B: nothing to undo where the earlier files set it (9n)
          w0 <- "set only in the base file"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("touched", mpath, tg); if (wait(a)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          if (a$state != "touched") rb <- c(rb, list(c(lab, w0)))
        } else if (!length(br)) { add(O$x$first[orr[1]], piece(O, orr[1])); cnt("values") }
      } else if (ty == "P") {
        lb <- if (length(br)) B$ecan[br[length(br)]]; lo <- if (length(orr)) O$ecan[orr[length(orr)]]
        if (identical(lb, lo)) next
        if (is.null(lo)) {                                                     # only in B: the earlier files' last coefficient or efficiency (9n; energy_input.cpp:150-159)
          w0 <- "coefficient set only in the base file"
          if (stub) { rb <- c(rb, list(c(lab, w0))); next }
          a <- need("touched", mpath, tg); if (wait(a)) next
          if (!is.list(a) || a$state == "unknown") { nope(lab, w0, a); next }
          if (a$state != "touched") { rb <- c(rb, list(c(lab, w0))); next }
          v <- unname(a$value); add(NA, paste0(ind(d1), v)); st$okl <- c(st$okl, list(c(path, cls, v))); cnt("restores"); next
        }
        r <- orr[length(orr)]; add(O$x$first[r], piece(O, r)); cnt("values")
      } else {
        cb <- canon(B, br); co <- canon(O, orr)
        if (identical(cb, co)) next
        if (ty == "U") nope(lab, paste0("registry kind ", kind[ri], " is not handled"), FALSE)
        else if (cls == "Scenario") {
          if (tg == "calc-base-price") {                                      # each block writes base prices; a later block with all its pairs rewrites them all
            pm <- function(D, r) lapply(r, function(i) { h <- D$kids[[i + 1L]]; h <- h[D$x$tag[h] == "sector-map"]; fr <- av(D, h, "from-name"); to <- av(D, h, "to-name")
                                                         fr[is.na(fr)] <- ""; to[is.na(to)] <- ""; u <- !duplicated(fr, fromLast = TRUE); paste(fr[u], to[u], sep = "\r") })
            pb <- pm(B, br); po <- pm(O, orr)
            if (all(vapply(pb, function(p) any(vapply(po, function(q) all(p %in% q), NA)), NA))) { vet(O, orr, cls, path); add(O$x$first[orr], piece(O, orr)); cnt("cbp") }
            else nope(lab, "calc-base-price changed: each block adds an object, so every block of the base file needs a block in the override holding all of its sector-map pairs (from-name, to-name)", FALSE)
          } else nope(lab, if (tg == "modeltime") "modeltime differs (GCAM reads modeltime only once)" else if (grepl("solver|solution", tg)) "solver settings differ" else "differs at scenario level", FALSE)
        } else if (any(c(av(B, br, "delete"), av(O, orr, "delete"), av(B, br, "nocreate"), av(O, orr, "nocreate")) %in% "1")) {
          nope(lab, "delete or nocreate on an entry without its key attribute (GCAM skips it)", FALSE)
        } else if (endsWith(s, "\r#") && rules$order_policy[ri] == "critical") {
          w0 <- "entry without its key attribute in an order-critical list (grades, food-storage inputs; GCAM always appends it)"
          if (cls == "ITechnology") { rb <- c(rb, list(c(lab, w0))) } else nope(lab, w0, FALSE)   # inside a stub: stub replace (9l)
          xr <- c(xr, mem[ri])
        } else if (length(cb) < length(co) && identical(cb, co[seq_along(cb)])) { r <- orr[seq_along(orr) > length(cb)]; vet(O, r, cls, path); add(O$x$first[r], piece(O, r)); cnt("new")
        } else nope(lab, "list without name or year keys changed (not an extension)", FALSE)
      }
    }
    sq[[length(sl) + 1L]] <- list(st$n, length(st$moves), length(st$defaults))
    for (m in unique(vm)) {                                                    # entry order: base order, then new and rebuilt entries in override order
      w <- which(vm == m); hb <- w[!is.na(vpb[w])]; ho <- w[!is.na(vpo[w])]
      kb2 <- vk[hb][order(vpb[hb])]; ko2 <- vk[ho][order(vpo[ho])]; re <- vk[w][vre[w]]
      kb2 <- kb2[kb2 %in% ko2 & !kb2 %in% re]; e <- c(kb2, ko2[!ko2 %in% kb2])
      if ((identical(e, ko2) && !m %in% xs) || (vcr[w[1]] && m %in% xr)) next   # xr: already refused for an entry without its key
      if (vcr[w[1]] || m %in% xs) {                                            # xs: the order after the earlier files, from their entries
        lab <- paste0(path, "/", vt[w[1]]); w0 <- paste0("entry order changes in an order-critical list (grades, food-storage inputs", if (cls == "ITechnology") "; inside a stub-technology the global technology decides the class", ")")
        if (vcr[w[1]] && cls != "ITechnology" && any(grepl("\r#[0-9]+$", vk[w]))) { nope(lab, w0, FALSE); next }
        if (vcr[w[1]] && (stub || cls == "ITechnology")) { rb <- c(rb, list(c(lab, w0))); next }   # inside a stub: stub replace (9l)
        fm <- msrc[match(m, mem)]; ka <- rules$key_attribute[match(m, mem)]  # suffix rebuild (9k): the order after the earlier files, B and the pieces so far (C0), and after them and O (T0)
        be <- bk[mem[rq] == m]; oe <- ok[mem[ro] == m]
        bs <- paste0(ifelse(av(B, be, "delete") %in% "1", "D", ifelse(av(B, be, "nocreate") %in% "1", "N", "M")), "\r", av(B, be, ka), recycle0 = TRUE)
        os <- paste0(ifelse(av(O, oe, "delete") %in% "1", "D", ifelse(av(O, oe, "nocreate") %in% "1", "N", "M")), "\r", av(O, oe, ka), recycle0 = TRUE)
        pj <- which(wj %in% which(mem[rs] == m)); pj <- pj[order(pos[pj])]
        s0 <- sub("(?s)^\\s*(<[^>]*>).*$", "\\1", out[pj], perl = TRUE)
        ps <- paste0(ifelse(grepl("\\sdelete=\"1\"", s0), "D", ifelse(grepl("\\snocreate=\"1\"", s0), "N", "M")), "\r", sub(paste0("(?s)^.*\\s", ka, "=\"([^\"]*)\".*$"), "\\1", s0, perl = TRUE), recycle0 = TRUE)
        a <- need("siblings", mpath, fm); if (wait(a)) next
        if (!is.list(a) || a$state == "unknown") {
          if (vcr[w[1]]) { nope(lab, w0, a); next }
          mv <- plain(unique(substring(ps[startsWith(ps, "D")], 3L)))          # the earlier files' order not known: the stub-technologies created again
          st$moves <- c(st$moves, paste0(plain(path), "/", vt[w[1]], " (", length(mv), " of ", length(unique(c(sub("^.*\r", "", ko2), mv))), ": ", paste(head(mv, 5), collapse = ", "), if (length(mv) > 5) ", ...", ")")); next
        }
        p0 <- if (a$state == "touched") a$value else character(0)
        T0 <- Reduce(function(v, z) { y <- substring(z, 3L); if (startsWith(z, "D")) v[v != y] else if (startsWith(z, "M") && !y %in% v) c(v, y) else v }, os, p0)   # GCAM's vector rules (xml_parse_helper.cpp:494-541)
        if (!vcr[w[1]]) {                                                      # not order-critical: the move reported
          C0 <- Reduce(function(v, z) { y <- substring(z, 3L); if (startsWith(z, "D")) v[v != y] else if (startsWith(z, "M") && !y %in% v) c(v, y) else v }, c(bs, ps), p0)
          if (identical(C0, T0)) next
          t1 <- T0[T0 %in% C0]; pe <- match(t1, C0[C0 %in% T0]); mv <- plain(c(t1[cummax(pe) > pe | rev(cummin(rev(pe))) < pe], setdiff(union(T0, C0), intersect(T0, C0))))
          st$moves <- c(st$moves, paste0(plain(path), "/", vt[w[1]], " (", length(mv), " of ", length(T0), ": ", paste(head(mv, 5), collapse = ", "), if (length(mv) > 5) ", ...", ")")); next
        }
        pk <- substring(ps, 3L); dp <- logical(length(pj)); kq <- NA            # dp: pieces dropped; kq: first difference, never later once pieces are dropped
        for (q in seq_len(length(pj) + 1L)) {                                  # drop the pieces of the keys from the first difference on, until no more are dropped
          C0 <- Reduce(function(v, z) { y <- substring(z, 3L); if (startsWith(z, "D")) v[v != y] else if (startsWith(z, "M") && !y %in% v) c(v, y) else v }, c(bs, ps[!dp]), p0)
          n0 <- max(length(C0), length(T0)); x <- c(C0, rep(NA, n0 - length(C0))); y <- c(T0, rep(NA, n0 - length(T0)))
          k2 <- which(is.na(x) != is.na(y) | (!is.na(x) & !is.na(y) & x != y))[1]
          if (!is.na(k2)) kq <- min(kq, k2, na.rm = TRUE)
          if (is.na(kq)) break
          d2 <- dp | pk %in% union(C0[seq_along(C0) >= kq], T0[seq_along(T0) >= kq])
          if (identical(d2, dp)) break
          dp <- d2
        }
        if (is.na(kq)) next
        K <- union(C0[seq_along(C0) >= kq], T0[seq_along(T0) >= kq])
        if (!all(pk[dp] %in% K)) stop("addon_minaj: internal error, no add-on written; the suffix rebuild of ", plain(lab), " dropped pieces it does not write again", call. = FALSE)
        if (any(dp)) {                                                       # the dropped slots' pieces, counts, order moves and restored defaults
          u <- pj[dp]; for (q in unique(wj[u][wj[u] > 0L])) { st$n <- st$n - (sq[[q + 1L]][[1]] - sq[[q]][[1]]); xm <- c(xm, sq[[q]][[2]] + seq_len(sq[[q + 1L]][[2]] - sq[[q]][[2]]))
                                                               xd <- c(xd, sq[[q]][[3]] + seq_len(sq[[q + 1L]][[3]] - sq[[q]][[3]])) }
          pos <- pos[-u]; out <- out[-u]; wj <- wj[-u]
        }
        if (kq <= length(p0)) { nope(lab, paste0(w0, "; the rebuild reaches ", c(p0[p0 %in% K], C0[kq])[1], ", which an earlier file made before the base file's entries"), FALSE); next }
        a <- lapply(K, function(z) need("touched", c(mpath, paste0(fm, "\r", z)))); if (do.call(wait, a)) next
        u <- which(!vapply(a, function(z) z$state == "untouched", NA))[1]
        if (!is.na(u)) { nope(lab, paste0(w0, "; the rebuild reaches ", K[u], ", which an earlier file touches"), if (a[[u]]$state == "unknown") a[[u]] else FALSE); next }
        ce <- C0[seq_along(C0) >= kq]; tb <- B$x$tag[be][match(ce, av(B, be, ka))]; tb[is.na(tb)] <- O$x$tag[oe][match(ce[is.na(tb)], av(O, oe, ka))]; tb[is.na(tb)] <- vt[w[1]]
        v <- paste0("<", tb, " ", ka, "=\"", ce, "\" delete=\"1\"/>", recycle0 = TRUE); e1 <- oe[av(O, oe, ka) %in% K]
        if (!identical(Reduce(function(v, z) { y <- substring(z, 3L); if (startsWith(z, "D")) v[v != y] else if (startsWith(z, "M") && !y %in% v) c(v, y) else v },
                              c(paste0("D\r", ce, recycle0 = TRUE), os[oe %in% e1]), C0[seq_len(kq - 1L)]), T0))
          stop("addon_minaj: internal error, no add-on written; the suffix rebuild of ", plain(lab), " does not give the override's order", call. = FALSE)
        vet(O, e1, cls, path); st$okl <- c(st$okl, lapply(v, function(z) c(path, cls, z))); x <- c(paste0(ind(d1), v, recycle0 = TRUE), piece(O, e1)); j <- 0L
        add((if (length(oo)) O$x$last[oo[length(oo)]] - 1 else 0) + (gi + seq_along(x)) / 2^16, x); gi <- gi + length(x); cnt("rebuilds", length(ce))
      } else { pe <- match(ko2, e); mv <- plain(sub("^.*\r", "", ko2[cummax(pe) > pe | rev(cummin(rev(pe))) < pe]))   # entries whose order against another entry differs
               st$moves <- c(st$moves, paste0(plain(path), "/", vt[w[1]], " (", length(mv), " of ", length(ko2), ": ", paste(head(mv, 5), collapse = ", "), if (length(mv) > 5) ", ...", ")")) }
    }
    if (length(xm)) st$moves <- st$moves[-unique(xm)]
    if (length(xd)) st$defaults <- st$defaults[-unique(xd)]
    list(pos = pos, out = out, wat = wat, rb = rb)
  }
  # leaf check: every leaf of the add-on equals a leaf of the override at the same path; names are plain ASCII
  chk <- function(A, O) {
    lab <- function(x) {                                                      # key attributes of the registry row (all four for unknown pairs)
      v <- rep(NA_character_, length(x$tag)); ka <- v; cl <- rep("", length(x$tag))
      for (d in seq_len(max(x$depth))) { w <- which(x$depth == d); ri <- match(paste(if (d == 1L) "" else cl[x$parent[w]], x$tag[w], sep = "\r"), rk)
                                         cl[w] <- ifelse(is.na(ri), "", rules$creates[ri]); ka[w] <- rules$key_attribute[ri] }
      for (a in c("name", "year", "sector-name", "subsector-name")) {
        z <- x[[paste0("@", a)]]; if (is.null(z)) next
        z[!is.na(ka) & !grepl(paste0("(^|;)", a, "(;|$)"), ka)] <- NA; v <- ifelse(is.na(v), z, ifelse(is.na(z), v, paste0(v, ", ", z)))
      }
      ifelse(is.na(v), x$tag, paste0(x$tag, "[", v, "]"))
    }
    pth <- function(x) { l <- lab(x); p <- l; for (d in seq_len(max(x$depth))[-1]) { w <- which(x$depth == d); p[w] <- paste0(p[x$parent[w]], "/", l[w], recycle0 = TRUE) }; p }
    ap <- pth(A$x); op <- pth(O$x)
    for (a in c("name", "year", "sector-name", "subsector-name")) {
      v <- A$x[[paste0("@", a)]]
      if (!is.null(v)) for (i in which(!is.na(v) & grepl("[^ -~]|[&<>\"']", v, useBytes = TRUE)))
        nope(plain(ap[i]), paste0(a, " holds & < > \" ' or a non-ASCII byte (GCAM compares names as raw bytes)"), FALSE)
    }
    ok2 <- character(0)                                                       # leaves addon_write generated (st$okl: holder path, holder class, element), in this form
    for (e in st$okl) {
      y <- prep(charToRaw(e[3]), "add-on"); x <- y$x; cg <- pg <- character(length(x$tag))
      for (i in seq_along(x$tag)) {
        h <- x$parent[i]; ri <- match(paste(if (h) cg[h] else e[2], x$tag[i], sep = "\r"), rk); cg[i] <- if (is.na(ri)) "" else rules$creates[ri]; v <- NA_character_
        for (a in c("name", "year", "sector-name", "subsector-name")) {
          z <- x[[paste0("@", a)]][i]; if (is.null(z) || is.na(z) || (!is.na(ri) && !grepl(paste0("(^|;)", a, "(;|$)"), rules$key_attribute[ri]))) next
          v <- if (is.na(v)) z else paste0(v, ", ", z)
        }
        pg[i] <- paste0(if (h) pg[h] else e[1], "/", x$tag[i], if (!is.na(v)) paste0("[", v, "]"))
      }
      ok2 <- c(ok2, paste0(pg, "\t", y$ecan)[!is.na(x$text)])
    }
    lf <- which(!is.na(A$x$text) & A$x$depth > 2L)
    full <- lf[nzchar(A$x$text[lf])]; emp <- lf[!nzchar(A$x$text[lf])]
    bad <- full[!paste0(ap[full], "\t", A$ecan[full], recycle0 = TRUE) %in% c(paste0(op, "\t", O$ecan), ok2)]
    if (length(emp)) {
      u <- unique(op); ix <- split(seq_along(op), factor(op, levels = u)); jx <- match(ap[emp], u)
      for (q in seq_along(emp)) {
        i <- emp[q]; rows <- if (is.na(jx[q])) integer(0) else ix[[jx[q]]]
        if (paste0(ap[i], "\t", A$ecan[i]) %in% ok2) next
        an <- setdiff(strsplit(A$anm[i], " ", fixed = TRUE)[[1]], c("", "nocreate"))
        if (!length(rows) || !all(vapply(an, function(a) any(av(O, rows, a) %in% av(A, i, a)), NA))) bad <- c(bad, i)
      }
    }
    st$int <- c(st$int, plain(ap[bad]))
  }
  # one group: the part of the base and override files holding the same blocks (lab names it in parse errors; sh: lines
  # before the block in each part). Compared again, from the state before it, while questions about the earlier files
  # are open (answered by prefix_scan between passes)
  grp <- function(bb, ob, off, lab = "", sh = c(0L, 0L)) {
    B <- prep(bb, paste0(basename(b_file), lab), sh[1]); O <- prep(ob, paste0(basename(o_file), lab), sh[2]); st$B <- B; st$O <- O; st$off <- off
    tb <- B$kids[[1]]; to <- O$kids[[1]]
    if (length(tb) != 1L || length(to) != 1L || B$x$tag[tb] != "scenario" || O$x$tag[to] != "scenario")
      stop("addon_minaj: ", basename(b_file), " and ", basename(o_file), " must each hold one <scenario> element", call. = FALSE)
    st$ans <- list(); snap <- mget(c("n", "moves", "defaults", "nr", "ref", "cnt", "int", "wt", "wp", "stx", "sp", "okl"), envir = st)
    for (pass in seq_len(max(B$x$depth, O$x$depth) + 2L)) {
      nw <- length(st$wt)
      r <- walk("Scenario", tb, to, character(0), "scenario", FALSE)
      if (length(st$need)) {
        a <- prefix_scan(config, basename(b_file), unique(st$need)); st$ans[names(a)] <- a; st$need <- character(0); list2env(snap, envir = st); next
      }
      if (nzchar(r$wat)) nope("scenario", "attributes of <scenario> differ", FALSE)
      for (z in r$rb) nope(z[1], paste(z[-1], collapse = "; "), FALSE)
      st$sp <- c(st$sp, r$pos + off); st$stx <- c(st$stx, r$out)
      gw <- st$wt[seq_along(st$wt) > nw]; gp <- st$wp[seq_along(st$wp) > nw]
      if (length(gw) || length(r$out))
        chk(prep(charToRaw(paste(c("<scenario>", if (length(gw)) c("    <world>", gw[order(gp)], "    </world>"), r$out[order(r$pos)], "</scenario>"), collapse = "\n")), "add-on"), O)
      return(invisible())
    }
    stop("addon_minaj: internal error, questions about earlier files did not settle", call. = FALSE)
  }
  # blocks of a file as gcamdata writes it: each child of <world> (indented 8 spaces) and each other child of <scenario>
  # (4 spaces), as byte spans with a key per slot and a hash, and the <scenario> and <world> start tags; NULL when the
  # layout differs (checked against the tokenizer)
  cut <- function(f) {
    b <- readBin(f, "raw", file.size(f)); n <- length(b)
    s0 <- if (n >= 3L && identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) 3L else 0L
    nl <- grepRaw("\n {0,8}<[/A-Za-z_:]", b, all = TRUE)
    if (n > s0 + 1L && b[s0 + 1L] == as.raw(60L) && grepl("[A-Za-z_:]", rawToChar(b[s0 + 2L]))) nl <- c(s0, nl)
    m <- length(nl); if (!m || m > 1e5) return(NULL)
    dp <- q <- gt <- integer(m); tg <- nv <- character(m); cl <- sc <- logical(m)
    for (i in seq_len(m)) {
      j <- nl[i] + 1L; while (b[j] == as.raw(32L)) j <- j + 1L
      g <- grepRaw(">", b, offset = j, fixed = TRUE); if (!length(g)) return(NULL)
      t <- tryCatch(rawToChar(b[j:g]), error = function(e) NA_character_); if (is.na(t)) return(NULL)
      dp[i] <- j - nl[i] - 1L; q[i] <- j; gt[i] <- g; cl[i] <- startsWith(t, "</"); sc[i] <- endsWith(t, "/>")
      tg[i] <- sub("(?s)^</?([-A-Za-z0-9_:.]+).*$", "\\1", t, perl = TRUE, useBytes = TRUE)
      nv[i] <- if (grepl("\\sname=\"", t, perl = TRUE, useBytes = TRUE)) sub("(?s)^.*?\\sname=\"([^\"]*)\".*$", "\\1", t, perl = TRUE, useBytes = TRUE) else ""
    }
    if (dp[1] || cl[1] || sc[1] || tg[1] != "scenario") return(NULL)
    lv <- bt <- bn <- character(0); a <- z <- integer(0); nw <- 0L; wq <- wg <- NA_integer_
    blk <- function(l, i, e) { lv <<- c(lv, l); a <<- c(a, nl[i] + 1L); z <<- c(z, e); bt <<- c(bt, tg[i]); bn <<- c(bn, nv[i]) }
    i <- 2L
    repeat {
      if (i > m) return(NULL)
      if (!dp[i]) { if (!cl[i] || tg[i] != "scenario" || i != m) return(NULL); break }
      if (dp[i] != 4L || cl[i]) return(NULL)
      if (tg[i] == "world" && !sc[i]) {
        nw <- nw + 1L; if (nw > 1L) return(NULL); wq <- q[i]; wg <- gt[i]; i <- i + 1L
        repeat {
          if (i > m) return(NULL)
          if (dp[i] == 4L && cl[i] && tg[i] == "world") { i <- i + 1L; break }
          if (dp[i] != 8L || cl[i]) return(NULL)
          if (sc[i]) { blk("W", i, gt[i]); i <- i + 1L }
          else if (i < m && dp[i + 1L] == 8L && cl[i + 1L] && tg[i + 1L] == tg[i]) { blk("W", i, gt[i + 1L]); i <- i + 2L }
          else return(NULL)
        }
      } else if (sc[i]) { blk("S", i, gt[i]); i <- i + 1L
      } else if (i < m && dp[i + 1L] == 4L && cl[i + 1L] && tg[i + 1L] == tg[i]) { blk("S", i, gt[i + 1L]); i <- i + 2L
      } else return(NULL)
    }
    gs <- c(1L, z + 1L); ge <- c(a - 1L, n)                                     # the skeleton: everything outside the blocks
    x <- tryCatch(xml_tokens(do.call(c, lapply(seq_along(gs), function(k) if (ge[k] >= gs[k]) b[gs[k]:ge[k]]))), error = function(e) NULL)
    if (is.null(x) || !identical(x$tag, c("scenario", if (nw) "world"))) return(NULL)
    ri <- match(paste(ifelse(lv == "W", "World", "Scenario"), bt, sep = "\r"), rk)
    list(f = f, head = b[q[1]:gt[1]], wtag = if (nw) b[wq:wg], lv = lv, a = a, z = z, tag = bt, name = bn,
         key = paste(lv, ifelse(is.na(ri), bt, mem[ri]), bn, sep = "\r"),
         h = vapply(seq_along(a), function(k) digest::digest(b[a[k]:z[k]], algo = "xxhash64", serialize = FALSE), ""))
  }
  # the part of a file holding blocks i, read by byte offset: <scenario> and <world> start tags, the blocks, end tags;
  # attribute sh: lines before the first block
  doc <- function(cf, i) {
    if (sum(cf$z[i] - cf$a[i] + 1) > cap) stop(sprintf("addon_minaj: block %s%s of %s is larger than option minaj.addon_parse_cap_mb (%g MB); use %s in place of the base file",
                                                       cf$tag[i[1]], if (nzchar(cf$name[i[1]])) paste0(" ", cf$name[i[1]]) else "", basename(cf$f), cap / 2^20, o_file), call. = FALSE)
    nl <- as.raw(10L); w <- i[cf$lv[i] == "W"]; s <- i[cf$lv[i] == "S"]
    con <- file(cf$f, "rb"); on.exit(close(con))
    rd <- function(k) { seek(con, cf$a[k] - 1); c(readBin(con, "raw", cf$z[k] - cf$a[k] + 1L), nl) }
    d <- do.call(c, c(list(cf$head, nl), if (!is.null(cf$wtag)) c(list(cf$wtag, nl), lapply(w, rd), list(charToRaw("</world>\n"))),
                      lapply(s, rd), list(charToRaw("</scenario>\n"))))
    attr(d, "sh") <- sum(cf$head == nl) + 1L + if (is.null(cf$wtag)) 0L else sum(cf$wtag == nl) + 1L + (length(s) > 0L)
    d
  }
  if (!identical(unname(tools::md5sum(b_file)), unname(tools::md5sum(o_file)))) {
    cb <- cut(b_file); co <- cut(o_file)
    if (is.null(cb) || is.null(co)) {
      bf <- basename(c(if (is.null(cb)) b_file, if (is.null(co)) o_file)); rm(cb, co)
      if (max(file.size(c(b_file, o_file))) > cap) stop(sprintf("addon_minaj: %s %s not laid out as gcamdata writes %s, and the larger file is over option minaj.addon_parse_cap_mb (%g MB); use %s in place of the base file",
                                                                paste(bf, collapse = " and "), if (length(bf) > 1L) "are" else "is", if (length(bf) > 1L) "them" else "it", cap / 2^20, o_file), call. = FALSE)
      grp(readBin(b_file, "raw", file.size(b_file)), readBin(o_file, "raw", file.size(o_file)), 0)
    } else {
      grp(doc(cb, integer(0)), doc(co, integer(0)), 0, " (<scenario> and <world> without their children; line numbers count from that part)")
      gi <- 0
      for (k in unique(c(co$key, cb$key))) {
        ib <- which(cb$key == k); io <- which(co$key == k)
        if (identical(cb$h[ib], co$h[io])) next                                # same bytes in both files: nothing to add
        x <- if (length(io)) list(co, io[1]) else list(cb, ib[1]); bl <- paste0(x[[1]]$tag[x[[2]]], if (nzchar(x[[1]]$name[x[[2]]])) paste0(" ", x[[1]]$name[x[[2]]]))
        if (length(st$ref) >= nmax) { st$skip <- bl; break }                  # enough places for a refusal: the rest is not compared
        db <- doc(cb, ib); dd <- doc(co, io); gi <- gi + 1
        grp(db, dd, gi * 2^32, paste0(" (block ", bl, "; line numbers count from the block)"), c(attr(db, "sh"), attr(dd, "sh"))); rm(db, dd)
      }
      rm(cb, co)
    }
    st$B <- st$O <- NULL; invisible(gc())
  }
  if (length(st$int)) {
    u <- unique(st$int)
    hd <- paste0("addon_minaj: internal error, no add-on written; ", length(u), " element(s) of the add-on not found in ", basename(o_file), "; use ", o_file, " in place of the base file")
    msg <- paste0(hd, ":\n", paste0("  - ", u, collapse = "\n"))
    if (nchar(msg, "bytes") > lim) { message(msg); msg <- paste0(hd, " (listed above; first: ", u[1], ")"); if (nchar(msg, "bytes") > lim) msg <- paste0(hd, " (listed above)") }
    stop(msg, call. = FALSE)
  }
  if (st$nr) {
    ref <- unique(st$ref); np <- if (st$nr > length(st$ref)) st$nr else length(ref)
    hd <- paste0("addon_minaj: no add-on written; use ", o_file, " in place of ", basename(b_file), ". ", if (!is.null(st$skip)) "At least ", np, " place(s) cannot be written as an add-on",
                 if (!is.null(st$skip)) paste0(" (blocks from ", st$skip, " on not compared)"))
    rs <- paste0("  reasons:", paste0("\n    ", names(st$cnt), " (", st$cnt, ")", collapse = ""))
    msg <- paste0(hd, ":\n", paste0("  - ", ref, collapse = "\n"), if (np > length(ref)) sprintf("\n  ... and %d more", np - length(ref)))
    if (nchar(msg, "bytes") > lim) {
      message(msg, "\n", rs)
      hs <- sub(o_file, basename(o_file), hd, fixed = TRUE)                    # the full path is in the list above
      msg <- paste0(hd, " (listed above):\n  - ", ref[1], "\n", rs)
      if (nchar(msg, "bytes") > lim) msg <- paste0(hd, " (listed above):\n  - ", ref[1])
      if (nchar(msg, "bytes") > lim) msg <- paste0(hs, " (listed above):\n  - ", ref[1], "\n", rs)
      if (nchar(msg, "bytes") > lim) msg <- paste0(hs, " (listed above):\n  - ", ref[1])
      if (nchar(hd, "bytes") + 15L > lim) hd <- hs
      if (nchar(msg, "bytes") > lim) msg <- paste0(hd, " (listed above)", if (nchar(hd, "bytes") + nchar(rs, "bytes") + 17L <= lim) paste0(":\n", rs))
    }
    stop(msg, call. = FALSE)
  }
  body <- c(if (length(st$wt)) c("    <world>", st$wt[order(st$wp)], "    </world>"), st$stx[order(st$sp)])
  note <- gsub("-{2,}", "-", paste0("minaj add-on to ", basename(b_file), " (md5 ", unname(tools::md5sum(b_file)), ")",
                                    if (length(override)) paste0("; override inputs: ", paste(override, collapse = ", ")),
                                    if (st$used) paste0("; configuration-specific: ", length(pp), if (length(pp) == 1L) " earlier file" else " earlier files")))
  txt <- paste(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", paste0("<!-- ", note, " -->"),
                 if (length(body)) c("<scenario>", body, "</scenario>") else "<scenario></scenario>", ""), collapse = "\n")
  size <- nchar(txt, "bytes"); ratio <- nm_opt("addon_max_ratio", 0.5)
  if (length(body) && size > ratio * file.size(o_file))
    stop(sprintf("addon_minaj: the add-on would be %.1f MB, more than %g x %s (%.1f MB); use that file in place of the base file, or set options(minaj.addon_max_ratio = Inf) to write the add-on anyway",
                 size / 2^20, ratio, basename(o_file), file.size(o_file) / 2^20), call. = FALSE)
  dir.create(dirname(a_file), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(a_file), fileext = ".part.xml"); kept <- FALSE; on.exit(if (!kept) unlink(tmp), add = TRUE)
  writeBin(charToRaw(txt), tmp); empty <- !length(body); rm(txt, body); st$wt <- st$stx <- NULL; invisible(gc())
  bad <- tryCatch({ xml2::read_xml(tmp); NULL }, error = function(e) conditionMessage(e)); invisible(gc())
  if (!is.null(bad)) stop("addon_minaj: internal error, no add-on written; the add-on is not well-formed XML (", bad, "); use ", o_file, " in place of the base file", call. = FALSE)
  # independent check (design step 12 iii): any difference, or an error, discards the add-on; an add-on that used answers
  # about the earlier files is checked after them; a stop in addon_minaj's form (memory, an earlier file the check does
  # not follow) is passed on (naming the add-on file)
  v <- tryCatch(addon_sim(b_file, tmp, o_file, if (st$used) config), error = function(e) {
    m <- sub(basename(tmp), basename(a_file), conditionMessage(e), fixed = TRUE)
    if (startsWith(m, "addon_minaj: ")) stop(m, call. = FALSE)
    m })
  if (!is.list(v) || !isTRUE(v$ok)) {
    hd <- "addon_minaj: internal error, no add-on written; "; ft <- paste0("\nUse ", o_file, " in place of the base file")
    if (is.list(v)) {
      m <- v$mismatches; tr <- nrow(m) > 0L && grepl("^\\([0-9]+ more differences not listed\\)$", m$target[nrow(m)])
      why <- paste0(m$target, ": ", m$column, " (", m$old, " | ", m$new, ")"); if (tr) why[nrow(m)] <- m$target[nrow(m)]
      hd <- paste0(hd, if (tr) "more than ", nrow(m) - tr, " place(s) where the base file followed by the add-on differs from ", basename(o_file))
      msg <- paste0(hd, ":\n", paste0("  - ", why, collapse = "\n"), ft)
    } else msg <- paste0(hd, "the check stopped: ", v, ft)
    if (nchar(msg, "bytes") > lim) {
      message(msg)
      msg <- if (is.list(v)) paste0(hd, " (listed above; first: ", why[1], ")", ft) else paste0(hd, "the check stopped (message above)", ft)
      if (nchar(msg, "bytes") > lim) msg <- paste0(if (is.list(v)) paste0(hd, " (listed above)") else paste0(hd, "the check stopped (message above)"), ft)
    }
    stop(msg, call. = FALSE)
  }
  if (st$used) {                                                             # the earlier files and their order as the add-on was built from them
    c2 <- config_components(config, basename(b_file)); p2 <- c2$path[seq_len(c2$b - 1L)]
    if (!identical(p2, pp)) stop("addon_minaj: ", config, " changed while the add-on was being written; run the call again", call. = FALSE)
    m2 <- vapply(p2, function(p) if (!file.exists(p)) NA_character_ else if (!is.null(.nm$store) && !is.null(.nm$state_dir)) file_md5(p) else unname(tools::md5sum(p)), "")
    if (!identical(unname(m2), unname(pm))) stop("addon_minaj: ", pp[which(m2 != pm | is.na(m2) != is.na(pm))[1]], " changed while the add-on was being written; run the call again", call. = FALSE)
  }
  move_into_place(tmp, a_file); kept <- TRUE
  pm <- if (st$used) pm
  invisible(list(n = st$n, moves = st$moves, defaults = st$defaults, size = size, empty = empty, prefix_md5 = pm))
}

# Independent check of an add-on, written apart from addon_write: GCAM's parse rules (registry and C++ facts) applied to
# base file b_file then add-on a_file, and to override file o_file alone, from an unknown earlier state. Compared on
# every block the add-on touches or where b_file and o_file differ: existence and creating tag of every object, fields
# (arrays for any set of model years), rule lists, keyword maps, key maps, input flags, set-true flags, lists without
# keys, stub-technology queues, the global technology database, and entry order in order-critical lists (grades,
# food-storage inputs; entries without their key included). Key-map keys are compared as GCAM reads them. An object earlier files may hold is compared both ways: absent before the base file, and
# present with unknown content; elements merging into such an object are read with the rules of their own tag. An
# element merging into an object another tag created passes only where its children and attributes have the same
# registry rows under both classes, apart from the class a period creates: each period is compared as its own object, a
# year already made keeping the class of the element that made it and a new year taking the class of the technology tag
# around it (a stub-technology takes the children of any technology tag; entry order in the periods it queues is read
# with the rules for any technology class). A technology's period (not a stub-technology's) is compared by the name it
# takes when read last: the technology's name then (a later technology tag reuses a subresource's one container, and its
# name attribute renames it), or a <name> child of that period; a period's own name attribute is not read.
# config: GCAM configuration loading b_file (NULL: none). With config, each group of blocks that differs above is
# compared again from the state the configuration's earlier files build: their blocks of that group, cut down to the
# objects that differed (start tags on the way, whole elements there), are parsed before both runs from nothing;
# arrays are compared for the configuration's model years (read here, checked against config_components), fields a
# new object outside a stub-technology queue never gets hold the default of the class that created it, the global
# technology database exists once <world> does, rules that set no model year are left out, and with option
# minaj.addon_neg0 TRUE (the default) a Value field written -0 is not set. It stops when free memory would fall under option
# minaj.ram_floor_gb: before the largest earlier-file block is read, before each earlier-file block, and before each
# group is read whole.
# Returns a list: ok (TRUE when nothing differs) and mismatches (data frame: target = place, column = what differs and
# for which earlier state, old = b_file then a_file, new = o_file; after 2000 differences, a last row counts the rest).
addon_sim <- function(b_file, a_file, o_file, config = NULL) {
  f <- system.file("extdata", "gcam_parse_rules.csv", package = "minaj")
  if (!nzchar(f)) stop("minaj: extdata/gcam_parse_rules.csv is missing from the package")
  rules <- utils::read.csv(f, colClasses = "character", na.strings = character(0))
  cap <- nm_opt("addon_parse_cap_mb", 100) * 2^20
  rk <- paste(rules$parent_class, rules$tag, sep = "\r"); mem <- sub(";.*", "", rules$source)   # member: its DEFINE_VARIABLE line or handler
  atr <- rules$kind == "SIMPLE" & grepl("[.](h|hpp):", mem)                # fields of a data vector: attributes set them too
  code <- c(SIMPLE = "S", ARRAY = "A", KEYWORD = "K", RULE = "R", REPLACE = "P", OR = "F", KEYMAP = "M", SETTRUE = "T", ONCE = "O", APPEND = "L",
            VEC_NAMED = "C", VEC_YEAR = "C", SINGLE = "C", PERIOD = "Y", GTD_LOCATION = "G", STUB_QUEUE = "Q")
  mm <- new.env(parent = emptyenv()); mm$n <- 0L; mm$x <- list()
  wl <- c(one = "", abs = " (if no earlier file creates it)", has = " (if an earlier file creates it)")
  neg0 <- isTRUE(nm_opt("addon_neg0", TRUE)); gm <- mem[match("World\rglobal-technology-database", rk)]; nz <- "^-(0+([.]0*)?|[.]0+)([eE][-+]?[0-9]+)?$"   # -0 as written
  s2 <- FALSE; yv <- NULL; pd <- list(); pn <- character(0)                  # stage 2: earlier files' parts and paths, model years
  sh <- function(x) {                                                         # display text of a value or canonical element text
    x <- ifelse(x == "\001", "(not set)", gsub("\001", "", gsub("(^|\n|=)[0-9]+:", "\\1", x, useBytes = TRUE), useBytes = TRUE))
    x <- gsub("[\r\n\t]+", " ", paste(x, collapse = "; "), useBytes = TRUE)
    if (nchar(x, "bytes") > 120) paste0(substr(x, 1, 117), "...") else if (nzchar(x)) x else "(none)"
  }
  # blocks of a file: each child of <world> (key: member and name) and the other children of <scenario>, with the start
  # tags of <scenario> and <world>, as byte spans and hashes; found from the lines that start with a tag when the file is
  # laid out as gcamdata writes it, else from every tag (files up to option minaj.addon_parse_cap_mb)
  wkey <- function(tg, nv) { r <- match(paste("World", tg, sep = "\r", recycle0 = TRUE), rk); kd <- rules$kind[r]
                             ifelse(kd %in% "VEC_NAMED", paste(mem[r], nv), ifelse(kd %in% "SINGLE", mem[r], paste(tg, nv))) }
  byline <- function(b) {
    n <- length(b); s0 <- if (n >= 3L && identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) 3L else 0L
    at <- grepRaw("\n {0,8}<[/A-Za-z_:]", b, all = TRUE) + 1L                # lines starting with a tag at most 8 spaces in
    if (n > s0 + 1L && b[s0 + 1L] == as.raw(60L) && grepl("[A-Za-z_:]", rawToChar(b[s0 + 2L]))) at <- c(s0 + 1L, at)
    m <- length(at); if (!m || m > 1e5) return(NULL)
    sp <- lt <- gt <- integer(m); tx <- character(m)
    for (i in seq_len(m)) {
      j <- at[i]; while (b[j] == as.raw(32L)) j <- j + 1L
      g <- grepRaw(">", b, offset = j, fixed = TRUE); if (!length(g)) return(NULL)
      t <- tryCatch(rawToChar(b[j:g]), error = function(e) NA_character_); if (is.na(t)) return(NULL)
      sp[i] <- j - at[i]; lt[i] <- j; gt[i] <- g; tx[i] <- t
    }
    en <- startsWith(tx, "</"); se <- endsWith(tx, "/>")
    if (length(grepRaw("<!", b, offset = gt[1] + 1L, fixed = TRUE)) || length(grepRaw("<?", b, offset = gt[1] + 1L, fixed = TRUE)) ||
        length(grepRaw("-->", b, offset = gt[1] + 1L, fixed = TRUE))) return(NULL)   # a comment, CDATA section or processing instruction after <scenario>, or the end of a comment there, may hide lines
    if (!all(grepl("^</[A-Za-z_:][-A-Za-z0-9_:.]*[ \t\r\n]*>$", tx[en], perl = TRUE, useBytes = TRUE)) ||   # tag lines as xml_tokens reads them, name written once
        !all(grepl("^<[A-Za-z_:][-A-Za-z0-9_:.]*(?:[ \t\r\n]+[A-Za-z_:][-A-Za-z0-9_:.]*[ \t\r\n]*=[ \t\r\n]*\"[^\"<&]*(?:&(?:amp|lt|gt|quot|apos);[^\"<&]*)*\")*[ \t\r\n]*/?>$", tx[!en], perl = TRUE, useBytes = TRUE)) ||
        any(grepl("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\"(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=", tx, perl = TRUE, useBytes = TRUE))) return(NULL)
    tg <- sub("(?s)^</?([^ \t\r\n/>]+).*$", "\\1", tx, perl = TRUE, useBytes = TRUE)
    nv <- ifelse(grepl("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=", tx, perl = TRUE, useBytes = TRUE),
                 sub("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=[ \t\r\n]*\"([^\"]*)\"(?s).*$", "\\1", tx, perl = TRUE, useBytes = TRUE), NA_character_)
    if (m < 2L || sp[1] || en[1] || se[1] || tg[1] != "scenario" || sp[m] || !en[m] || tg[m] != "scenario") return(NULL)
    pair <- function(i, d) i + 1L < m && sp[i + 1L] == d && en[i + 1L] && tg[i + 1L] == tg[i]   # the next tag line closes this one
    W <- K <- S <- list(); i <- 2L
    while (i < m) {
      if (sp[i] != 4L || en[i]) return(NULL)
      if (tg[i] == "world") {
        W[[length(W) + 1L]] <- c(lt[i], gt[i]); wi <- length(W); i <- i + 1L
        if (se[i - 1L]) next
        repeat {
          if (i >= m) return(NULL)
          if (sp[i] == 4L && en[i] && tg[i] == "world") { i <- i + 1L; break }
          if (sp[i] != 8L || en[i]) return(NULL)
          if (se[i]) { K[[length(K) + 1L]] <- c(wi, lt[i], gt[i], i); i <- i + 1L }
          else if (pair(i, 8L)) { K[[length(K) + 1L]] <- c(wi, lt[i], gt[i + 1L], i); i <- i + 2L }
          else return(NULL)
        }
      } else if (se[i]) { S[[length(S) + 1L]] <- c(lt[i], gt[i]); i <- i + 1L
      } else if (pair(i, 4L)) { S[[length(S) + 1L]] <- c(lt[i], gt[i + 1L]); i <- i + 2L
      } else return(NULL)
    }
    mx <- function(l, k) if (length(l)) do.call(rbind, l) else matrix(integer(0), 0L, k)
    wm <- mx(W, 2L); km <- mx(K, 4L); sm <- mx(S, 2L)
    ba <- sort(c(km[, 2], sm[, 1])); bz <- sort(c(km[, 3], sm[, 2])); gs <- c(1L, bz + 1L); ge <- c(ba - 1L, n)   # outside the blocks: tokenized as a check
    x <- tryCatch(xml_tokens(do.call(c, lapply(seq_along(gs), function(k) if (ge[k] >= gs[k]) b[gs[k]:ge[k]]))), error = function(e) NULL)
    if (is.null(x) || !identical(x$tag, c("scenario", rep("world", nrow(wm)))) || !identical(x$depth, c(1L, rep(2L, nrow(wm))))) return(NULL)
    list(rs = c(lt[1], gt[1]), w = data.frame(a = wm[, 1], z = wm[, 2]),
         k = data.frame(key = wkey(tg[km[, 4]], nv[km[, 4]]), w = km[, 1], a = km[, 2], z = km[, 3], t = tg[km[, 4]], n = nv[km[, 4]], stringsAsFactors = FALSE),
         s = data.frame(a = sm[, 1], z = sm[, 2]))
  }
  bytag <- function(b, f) {
    s <- rawToChar(b); Encoding(s) <- "bytes"
    m <- gregexpr("(?s)<!--.*?-->|<!\\[CDATA\\[.*?\\]\\]>|<\\?.*?\\?>|<[^>]*>", s, perl = TRUE, useBytes = TRUE)[[1]]   # comments, CDATA sections and processing instructions hide what they hold
    if (m[1] < 0) stop(basename(f), ": no elements", call. = FALSE)
    p <- as.integer(m); e <- p + attr(m, "match.length") - 1L; rm(m)
    c2 <- b[p + 1L]; el <- c2 != as.raw(33L) & c2 != as.raw(63L) & c2 != as.raw(47L)
    ty <- ifelse(c2 == as.raw(47L), -1L, ifelse(el & b[e - 1L] != as.raw(47L), 1L, 0L))
    d <- cumsum(ty); dp <- d + (ty == 0L)
    ix <- which(el & dp <= 3L); lv <- dp[ix]; st <- ty[ix] == 1L
    tok <- substring(s, p[ix], e[ix]); rm(s)
    w <- st | ty[ix] == 0L                                                      # start tags as xml_tokens reads them, name written once
    u <- !grepl("^<[A-Za-z_:][-A-Za-z0-9_:.]*(?:[ \t\r\n]+[A-Za-z_:][-A-Za-z0-9_:.]*[ \t\r\n]*=[ \t\r\n]*\"[^\"<&]*(?:&(?:amp|lt|gt|quot|apos);[^\"<&]*)*\")*[ \t\r\n]*/?>$", tok[w], perl = TRUE, useBytes = TRUE) |
      grepl("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\"(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=", tok[w], perl = TRUE, useBytes = TRUE)
    if (any(u)) stop(basename(f), ": a start tag xml_tokens does not read, or with name twice: ", tok[w][which(u)[1]], call. = FALSE)
    tg <- sub("^<([^ \t\r\n/>]+).*$", "\\1", tok, perl = TRUE, useBytes = TRUE)
    nv <- ifelse(grepl("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=", tok, perl = TRUE, useBytes = TRUE),
                 sub("^<[^ \t\r\n/>]+(?:[ \t\r\n]+[^ \t\r\n=]+[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\")*?[ \t\r\n]+name[ \t\r\n]*=[ \t\r\n]*\"([^\"]*)\"(?s).*$", "\\1", tok, perl = TRUE, useBytes = TRUE), NA_character_)
    if (sum(lv == 1L) != 1L || tg[lv == 1L] != "scenario" || d[length(d)] != 0L) stop(basename(f), " must hold one <scenario> element", call. = FALSE)
    z <- e[ix]
    for (k in 1:3) { w <- which(st & lv == k); if (length(w)) { q <- which(ty == -1L & d == k - 1L); z[w] <- e[q[findInterval(ix[w], q) + 1L]] } }
    if (anyNA(z)) stop(basename(f), ": unbalanced tags", call. = FALSE)
    i2 <- which(lv == 2L); i3 <- which(lv == 3L); pw <- i2[findInterval(ix[i3], ix[i2])]
    wd <- i2[tg[i2] == "world"]; kk <- i3[tg[pw] == "world"]
    list(rs = c(p[ix[lv == 1L]], e[ix[lv == 1L]]), w = data.frame(a = p[ix[wd]], z = e[ix[wd]]),
         k = data.frame(key = wkey(tg[kk], nv[kk]), w = match(pw[tg[pw] == "world"], wd), a = p[ix[kk]], z = z[kk], t = tg[kk], n = nv[kk], stringsAsFactors = FALSE),
         s = data.frame(a = p[ix[setdiff(i2, wd)]], z = z[setdiff(i2, wd)]))
  }
  top <- function(f) {                                                        # spans, block hashes, start tags and other children of <scenario>
    sz <- file.size(f)
    if (sz >= 2^31 - 1) stop(basename(f), " is 2 GB or larger, more than R reads as one string", call. = FALSE)
    b <- readBin(f, "raw", sz); T <- byline(b)
    if (is.null(T)) {
      if (sz > cap) stop(sprintf("%s is not laid out as gcamdata writes it and is larger than option minaj.addon_parse_cap_mb (%g MB)", basename(f), cap / 2^20), call. = FALSE)
      T <- bytag(b, f)
    }
    T$h <- vapply(seq_len(nrow(T$k)), function(i) digest::digest(b[T$k$a[i]:T$k$z[i]], algo = "xxhash64", serialize = FALSE), "")
    bs <- function(a, z) lapply(seq_along(a), function(i) b[a[i]:z[i]])
    T$rb <- b[T$rs[1]:T$rs[2]]; T$wb <- bs(T$w$a, T$w$z); T$sb <- bs(T$s$a, T$s$z); T$f <- f; T
  }
  op <- function(x) { n <- length(x); if (x[n - 1L] == as.raw(47L)) c(x[seq_len(n - 2L)], as.raw(62L)) else x }   # <x .../> to <x ...>
  doc <- function(T, k) {                                                     # k: one world child key (its blocks read by byte offset), or NULL for the rest
    nl <- as.raw(10L); ln <- function(x) lapply(x, function(y) c(y, nl))
    j <- if (is.null(k)) integer(0) else which(T$k$key == k)
    if (sum(T$k$z[j] - T$k$a[j] + 1) > cap) stop(sprintf("%s: block %s%s is larger than option minaj.addon_parse_cap_mb (%g MB)", basename(T$f), T$k$t[j[1]],
                                                         if (!is.na(T$k$n[j[1]])) paste0(" ", T$k$n[j[1]]) else "", cap / 2^20), call. = FALSE)
    if (is.null(k) && sum(lengths(T$sb)) > cap) stop(sprintf("%s: the children of <scenario> other than <world> are larger than option minaj.addon_parse_cap_mb (%g MB)", basename(T$f), cap / 2^20), call. = FALSE)
    kb <- list()
    if (length(j)) { con <- file(T$f, "rb"); on.exit(close(con)); kb <- setNames(lapply(j, function(i) { seek(con, T$k$a[i] - 1); readBin(con, "raw", T$k$z[i] - T$k$a[i] + 1L) }), j) }
    ws <- lapply(seq_len(nrow(T$w)), function(i) c(list(op(T$wb[[i]]), nl), ln(kb[as.character(j[T$k$w[j] == i])]), list(charToRaw("</world>\n"))))
    d <- do.call(c, c(list(op(T$rb), nl), unlist(ws, recursive = FALSE), if (is.null(k)) ln(T$sb), list(charToRaw("</scenario>\n"))))
    if (length(j)) { w1 <- T$k$w[j[1]]                                       # lines before the first block
                     attr(d, "sh") <- sum(vapply(c(list(op(T$rb), nl), unlist(ws[seq_len(w1 - 1L)], recursive = FALSE), list(op(T$wb[[w1]]), nl)), function(x) sum(x == nl), 0L)) }
    d
  }
  # model years of the first <modeltime> under the root of file f (read with xml2, apart from config_components), as
  # model_time.cpp:93-151 and 203-264 build them: getValue<int> gives 0 for text it cannot cast, a missing time-step
  # reads 0, std::map year -> step with later writes winning, final-calibration-year defaults to 2021 (:65); NULL where
  # GCAM stops (initMembers exit(1), or a time step of 0: integer division by zero at :247)
  mtime <- function(f) {
    m <- xml2::xml_find_first(xml2::read_xml(f), "/*/modeltime"); if (inherits(m, "xml_missing")) return(NULL)
    e <- xml2::xml_children(m); tg <- xml2::xml_name(e); z <- c(xml2::xml_text(e), xml2::xml_attr(e, "time-step"))
    z <- vapply(z, function(v) if (!is.na(v) && grepl("^[-+]?[0-9]+$", v) && abs(as.numeric(v) + 0.5) <= 2^31) as.numeric(v) else 0, 0, USE.NAMES = FALSE)
    iv <- z[seq_along(e)]; sv <- z[-seq_along(e)]; ky <- st <- numeric(0); s0 <- e0 <- -1; fc <- 2021
    for (i in seq_along(e)) {
      if (tg[i] == "final-calibration-year") fc <- iv[i]
      if (!tg[i] %in% c("start-year", "inter-year", "end-year")) next
      v <- if (tg[i] == "end-year") -1 else sv[i]; h <- match(iv[i], ky); if (is.na(h)) { ky <- c(ky, iv[i]); st <- c(st, v) } else st[h] <- v
      if (tg[i] == "start-year") s0 <- iv[i]; if (tg[i] == "end-year") e0 <- iv[i]
    }
    o <- order(ky); ky <- ky[o]; st <- st[o]; n <- length(ky)
    if (!n || s0 == -1 || e0 == -1 || ky[1] != s0 || ky[n] != e0 || fc < s0 || fc > e0) return(NULL)
    y <- numeric(0)
    for (i in seq_len(n - 1L)) { g <- ky[i + 1L] - ky[i]; if (st[i] == 0 || g %% st[i] != 0) return(NULL); q <- trunc(g / st[i]); if (q > 0) y <- c(y, ky[i] + st[i] * (seq_len(q) - 1)) }
    as.integer(c(y, e0))
  }
  # block index of earlier file f, as top() gives it: files up to 50 MB read whole as top() reads them; larger ones read
  # in 64 MB chunks, each carrying its last 64 KB into the next, keeping the lines that start with a tag at most 8 spaces
  # in, which byline() reads as they stand (byte offsets as doubles; a comment, CDATA section or processing instruction
  # after <scenario>, the end of a comment there, or another indent: read whole, or stopped over the cap as top() stops);
  # kept for the R session in .nm$sim_index by file md5. Stops for a file that sets a name or year with a child element
  # outside comments
  loc <- function(f) {
    h <- if (!is.null(.nm$store) && !is.null(.nm$state_dir)) file_md5(f) else unname(tools::md5sum(f))
    T <- .nm$sim_index[[h]]
    if (is.null(T)) {
      n <- file.size(f)
      ms <- paste0("addon_minaj: no add-on written; ", basename(f), " (an earlier file of ", basename(config), ") sets a name or year with a child element (GCAM matches the object by it later), ",
                   "which the check after the earlier files does not follow; use ", o_file, " in place of the base file")
      if (n > 50 * 2^20) { con <- file(f, "rb"); T <- tryCatch({
        o <- 0; done <- 0; nt <- 0; s1 <- FALSE; TX <- LA <- GA <- list()
        repeat {
          seek(con, o); b <- readBin(con, "raw", min(2^26, n - o)); m <- length(b); end <- o + m >= n
          at <- grepRaw("\n {0,8}<[/A-Za-z_:]", b, all = TRUE) + 1L
          if (!o) { s0 <- if (m >= 3L && identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) 3L else 0L
                    if (m > s0 + 1L && b[s0 + 1L] == as.raw(60L) && grepl("[A-Za-z_:]", rawToChar(b[s0 + 2L]))) { at <- c(s0 + 1L, at); s1 <- TRUE } }
          if (!end) at <- at[at <= m - 65535]                                 # newline before the carried 64 KB
          at <- at[o + at > done]; nt <- nt + length(at); if (nt > 1e5) stop("too many lines")
          t1 <- character(length(at)); l1 <- g1 <- numeric(length(at))
          for (u in seq_along(at)) { i <- at[u]; j <- i; while (b[j] == as.raw(32L)) j <- j + 1L; if (!(j - i) %in% c(0L, 4L, 8L)) stop("not laid out")
                                     g <- grepRaw(">", b, offset = j, fixed = TRUE); if (!length(g)) stop("no >")
                                     t1[u] <- rawToChar(b[i:g]); l1[u] <- o + j; g1[u] <- o + g }
          TX[[length(TX) + 1L]] <- t1; LA[[length(LA) + 1L]] <- l1; GA[[length(GA) + 1L]] <- g1
          w1 <- if (o || !length(GA[[1]])) 1L else GA[[1]][1] + 1L             # after <scenario>: may hide lines
          if (length(grepRaw("<!", b, offset = w1, fixed = TRUE)) || length(grepRaw("<?", b, offset = w1, fixed = TRUE)) || length(grepRaw("-->", b, offset = w1, fixed = TRUE))) stop("not laid out")
          nn <- c(grepRaw("<name", b, offset = w1, fixed = TRUE, all = TRUE), grepRaw("<year", b, offset = w1, fixed = TRUE, all = TRUE))
          if (any(b[nn + 5L] %in% as.raw(c(9L, 10L, 13L, 32L, 47L, 62L)))) stop("name")
          if (length(at)) done <- o + max(at)
          if (end) break
          o <- o + m - 65536
        }
        tx <- unlist(TX); la <- unlist(LA); ga <- unlist(GA); rm(TX, LA, GA)
        nb <- nchar(tx, "bytes"); p0 <- cumsum(c(if (s1) 0 else 1, nb[-length(nb)] + 1)); i1 <- p0 + regexpr("[^ ]", tx, useBytes = TRUE); i2 <- p0 + nb
        Z <- byline(charToRaw(paste0(if (s1) "" else "\n", paste(tx, collapse = "\n"))))   # the tag lines alone, laid out as in the file
        if (is.null(Z)) stop("not laid out")
        Z <- list(rs = c(la[match(Z$rs[1], i1)], ga[match(Z$rs[2], i2)]), w = data.frame(a = la[match(Z$w$a, i1)], z = ga[match(Z$w$z, i2)]),
                  k = data.frame(key = Z$k$key, w = Z$k$w, a = la[match(Z$k$a, i1)], z = ga[match(Z$k$z, i2)], t = Z$k$t, n = Z$k$n, stringsAsFactors = FALSE),
                  s = data.frame(a = la[match(Z$s$a, i1)], z = ga[match(Z$s$z, i2)]))
        ba <- sort(c(Z$k$a, Z$s$a)); bz <- sort(c(Z$k$z, Z$s$z)); gs <- c(1, bz + 1); ge <- c(ba - 1, n); ng <- length(gs); nw <- nrow(Z$w)   # outside the blocks: tokenized as a check
        if (sum(pmax(ge - gs + 1, 0)) > cap) stop("skeleton")
        u <- c(gs, Z$rs[1], Z$w$a, Z$s$a); v <- c(ge, Z$rs[2], Z$w$z, Z$s$z)
        by <- lapply(seq_along(u), function(i) if (v[i] >= u[i]) { seek(con, u[i] - 1); readBin(con, "raw", v[i] - u[i] + 1) })
        x <- xml_tokens(do.call(c, by[seq_len(ng)]))
        if (!identical(x$tag, c("scenario", rep("world", nw))) || !identical(x$depth, c(1L, rep(2L, nw)))) stop("skeleton")
        c(Z, list(rb = by[[ng + 1L]], wb = by[ng + 1L + seq_len(nw)], sb = by[ng + 1L + nw + seq_len(nrow(Z$s))]))
      }, error = function(e) if (grepl("cannot allocate", conditionMessage(e), fixed = TRUE)) stop(e) else conditionMessage(e), finally = close(con)) }
      if (identical(T, "name")) stop(ms, call. = FALSE)
      if (is.character(T)) {                                                   # what byline() refuses too: over the cap, top() stops
        if (T %in% c("not laid out", "too many lines", "no >") && n > cap) stop(sprintf("%s is not laid out as gcamdata writes it and is larger than option minaj.addon_parse_cap_mb (%g MB)", basename(f), cap / 2^20), call. = FALSE)
        T <- NULL
      }
      if (is.null(T)) {                                                        # read whole, as top()
        if (n >= 2^31 - 1) stop(basename(f), " is 2 GB or larger, more than R reads as one string", call. = FALSE)
        b <- readBin(f, "raw", n); T <- byline(b)
        if (is.null(T)) {
          if (n > cap) stop(sprintf("%s is not laid out as gcamdata writes it and is larger than option minaj.addon_parse_cap_mb (%g MB)", basename(f), cap / 2^20), call. = FALSE)
          s <- rawToChar(b); Encoding(s) <- "bytes"                            # name and year tags outside comments, CDATA sections and processing instructions
          nn <- gregexpr("(?s)<!--.*?-->|<!\\[CDATA\\[.*?\\]\\]>|<\\?.*?\\?>|<(?:name|year)[ \t\r\n/>]", s, perl = TRUE, useBytes = TRUE)[[1]]; rm(s)
          if (any(b[nn[nn > 0] + 1L] %in% charToRaw("ny"))) stop(ms, call. = FALSE)
          T <- bytag(b, f)
        } else {                                                               # laid out: nothing after <scenario> is hidden
          nn <- c(grepRaw("<name", b, offset = T$rs[2] + 1L, fixed = TRUE, all = TRUE), grepRaw("<year", b, offset = T$rs[2] + 1L, fixed = TRUE, all = TRUE))
          if (any(b[nn + 5L] %in% as.raw(c(9L, 10L, 13L, 32L, 47L, 62L)))) stop(ms, call. = FALSE)
        }
        T$rb <- b[T$rs[1]:T$rs[2]]; T$wb <- lapply(seq_len(nrow(T$w)), function(i) b[T$w$a[i]:T$w$z[i]]); T$sb <- lapply(seq_len(nrow(T$s)), function(i) b[T$s$a[i]:T$s$z[i]]); rm(b)
      }
      T <- T[c("rs", "w", "k", "s", "rb", "wb", "sb")]; if (is.null(.nm$sim_index)) .nm$sim_index <- list(); .nm$sim_index[[h]] <- T
    }
    T$f <- f; T
  }
  # the part of an earlier file's block document (tokens x, text s) GCAM parses on the way to the objects at paths A
  # (steps from <scenario>, as mp in grp): start tags on the way (closed after what is kept inside), the whole of every
  # element at a path, and whole every entry keyed so that GCAM may read the key otherwise
  prune <- function(x, s, A) {
    N <- length(x$tag); for (a in c("@name", "@year", "@sector-name", "@subsector-name")) if (is.null(x[[a]])) x[[a]] <- rep(NA_character_, N)
    ri <- rep(NA_integer_, N); cx <- rep(NA_character_, N)
    for (d in seq_len(max(x$depth))) { w <- which(x$depth == d); ri[w] <- match(paste(if (d == 1L) "" else cx[x$parent[w]], x$tag[w], sep = "\r"), rk); v <- rules$creates[ri[w]]; v[!nzchar(v)] <- NA; cx[w] <- v }
    K <- unname(code[rules$kind[ri]]); K[is.na(K)] <- "?"; w <- K == "Q"; K[w] <- ifelse(x$tag[w] == "period", "Y", ifelse(x$tag[w] == "interpolation-rule", "R", "S"))
    ka <- rules$key_attribute[ri]; kv <- rep(NA_character_, N)
    for (a in unique(ka[K %in% c("C", "Y") & !is.na(ka) & nzchar(ka)])) { u <- which(K %in% c("C", "Y") & ka %in% a); v <- x[[paste0("@", a)]]; if (!is.null(v)) kv[u] <- v[u] }
    K[K == "C" & !is.na(ka) & nzchar(ka) & is.na(kv)] <- "L"
    yk <- K == "Y" | (K == "C" & ka %in% "year")
    kn <- ifelse(!is.na(kv) & yk & grepl("^[-+]?[0-9]{1,9}$", kv), as.character(suppressWarnings(as.integer(kv))), kv)
    st0 <- ifelse(K %in% c("C", "Y", "G"), paste0(K, "|", ifelse(K == "C", paste0(mem[ri], "|", ifelse(is.na(kn), "", kn)), ifelse(K == "Y", kn, paste0(x[["@sector-name"]], "|", x[["@subsector-name"]])))), NA)
    wi <- !is.na(kv) & ((yk & !grepl("^[1-9][0-9]*$", kv)) | (K == "C" & ka %in% "name" & !nzchar(kv)))
    mq <- rep(NA_character_, N); kp <- fu <- logical(N)
    for (d in seq_len(max(x$depth))) {
      w <- which(x$depth == d); if (d > 1L) w <- w[kp[x$parent[w]] & !fu[x$parent[w]]]
      if (!length(w)) break
      mq[w] <- if (d == 1L) "" else ifelse(is.na(st0[w]), NA, paste0(mq[x$parent[w]], "\n", st0[w]))
      fu[w] <- (!is.na(mq[w]) & mq[w] %in% A) | (d > 1L & wi[w])
      kp[w] <- fu[w] | (!is.na(mq[w]) & vapply(mq[w], function(p) any(startsWith(A, paste0(p, "\n"))), NA))
    }
    cs <- which(kp & !fu); fs <- which(fu); ct <- !endsWith(x$stag[cs], "/>")
    pos <- c(x$first[cs], x$last[cs[ct]], x$first[fs])
    tx <- c(x$stag[cs], paste0("</", x$tag[cs[ct]], ">", recycle0 = TRUE), if (length(fs)) substring(s, x$first[fs], x$last[fs]))
    charToRaw(paste(c(tx[order(pos)], ""), collapse = "\n"))
  }
  # write sequences (year, fillout, value) of one array field: TRUE when they give the same values for every set of model
  # years (a write sets its year if that is a model year, and with fillout every later model year); in stage 2, for the
  # configuration's model years
  arr <- function(x, y) {
    if (anyNA(c(x$y, y$y))) return(identical(x, y))
    val <- function(z, t, inm) { w <- which((z$y == t | (z$f & z$y < t)) & inm(z$y)); if (length(w)) z$v[max(w)] else "\001" }
    if (!is.null(yv)) return(all(vapply(yv, function(t) identical(val(x, t, function(q) q %in% yv), val(y, t, function(q) q %in% yv)), NA)))
    red <- function(z) { k <- vapply(seq_along(z$y), function(i) !any(z$y[-seq_len(i)] == z$y[i] & (z$f[-seq_len(i)] | !z$f[i])), NA); lapply(z, `[`, k) }
    x <- red(x); y <- red(y)
    if (identical(x, y)) return(TRUE)
    ys <- sort(unique(c(x$y, y$y))); if (length(ys) > 10L) return(FALSE)
    for (t in c(ys, ys + 0.5)) {
      lo <- ys[ys < t]
      for (s in seq_len(2^length(lo)) - 1L) {
        inm <- function(q) q == t | q %in% lo[bitwAnd(s, as.integer(2^(seq_along(lo) - 1L))) > 0L]
        if (!identical(val(x, t, inm), val(y, t, inm))) return(FALSE)
      }
    }
    TRUE
  }
  # one group of blocks: base then add-on (run 1) against the override (run 2); in stage 2 after the earlier files' parts pd
  grp <- function(k) {
    D <- c(pd, list(doc(TB, k), doc(TA, k), doc(TO, k))); np <- length(pd)      # files 1..np earlier, then base, add-on, override
    pl <- if (is.null(k)) "<scenario> and <world> without their children" else {
      T <- Find(function(T) k %in% T$k$key, list(TO, TB, TA)); i <- match(k, T$k$key); paste0("block ", T$k$t[i], if (!is.na(T$k$n[i])) paste0(" ", T$k$n[i])) }
    X <- Map(function(r, f) unclass(tryCatch(xml_tokens(r), error = function(e) {
      m <- conditionMessage(e); s0 <- attr(r, "sh"); l <- as.integer(regmatches(m, regexpr("(?<=, line )[0-9]+(?= \\(byte [0-9]+\\))", m, perl = TRUE)))
      if (!is.null(s0) && length(l) && l > s0) m <- sub(", line [0-9]+ \\(byte [0-9]+\\)", sprintf(", line %d of the block", l - s0), m) else pl <- paste0(pl, "; line numbers count from that part")
      stop(basename(f), " (", pl, "): ", m, call. = FALSE) })), D, c(pn, b_file, a_file, o_file))
    S <- lapply(D, function(r) { s <- rawToChar(r); Encoding(s) <- "bytes"; s }); rm(D)
    n <- vapply(X, function(x) length(x$tag), 1L); off <- c(0L, cumsum(n))[seq_along(n)]; N <- sum(n); ix <- seq_len(N)
    cl <- function(v) unlist(lapply(X, `[[`, v), use.names = FALSE)
    tag <- cl("tag"); dep <- cl("depth"); fi <- cl("first"); la <- cl("last"); tx <- cl("text"); fl <- rep(seq_along(n), n)
    # a difference at a place, with the steps from <scenario> of the object the calling obj() or kids() compares
    bad <- function(path, what, wn, x, y) { e <- parent.frame(); g <- c(e$l1, e$l2, e$p1, e$p2)[1]
                                            mm$n <- mm$n + 1L; if (mm$n <= 2000L) mm$x[[mm$n]] <- c(path, what, sh(x), sh(y), wn, if (is.null(g)) "" else mp[g]) }
    pa <- unlist(Map(function(x, o) ifelse(x$parent > 0L, x$parent + o, 0L), X, off), use.names = FALSE)
    ed <- unlist(Map(function(x, o) findInterval(x$last, x$first) + o, X, off), use.names = FALSE)   # last row of each subtree
    an <- sort(unique(sub("^@", "", unlist(lapply(X, function(x) grep("^@", names(x), value = TRUE))))))
    at <- setNames(lapply(an, function(a) unlist(lapply(X, function(x) { v <- x[[paste0("@", a)]]; if (is.null(v)) rep(NA_character_, length(x$tag)) else v }), use.names = FALSE)), an)
    rm(X)
    av <- function(a, g) { v <- at[[a]]; if (is.null(v)) rep(NA_character_, length(g)) else v[g] }
    ri <- rep(NA_integer_, N); cx <- rep(NA_character_, N)                   # registry row, and class the element's own tag creates
    for (d in seq_len(max(dep))) {
      w <- which(dep == d); ri[w] <- match(paste(if (d == 1L) "" else cx[pa[w]], tag[w], sep = "\r"), rk)
      v <- rules$creates[ri[w]]; v[!nzchar(v)] <- NA; cx[w] <- v
    }
    K <- unname(code[rules$kind[ri]]); K[is.na(K)] <- "?"
    w <- K == "Q"; K[w] <- ifelse(tag[w] == "period", "Y", ifelse(tag[w] == "interpolation-rule", "R", "S"))
    K[K == "S" & tag == "type-flags"] <- "F"
    ka <- rules$key_attribute[ri]; mb <- mem[ri]; kv <- rep(NA_character_, N)
    w <- which(K %in% c("C", "Y") & nzchar(ka)); for (a in unique(ka[w])) { u <- w[ka[w] == a]; kv[u] <- av(a, u) }
    K[K == "C" & nzchar(ka) & is.na(kv)] <- "L"                              # vector entry without its key: always appended
    kq <- !is.na(kv) & ((K == "Y" | (K == "C" & ka %in% "year")) & !grepl("^[1-9][0-9]*$", kv) | (K == "C" & ka %in% "name" & !nzchar(kv)))   # keys GCAM may read otherwise
    sk <- paste0(K, "|", ifelse(K %in% c("S", "A", "K", "M", "T", "?"), tag, ifelse(K == "P", ifelse(tag %in% c("efficiency", "coefficient"), "coef", tag),
                 ifelse(K %in% c("O", "L"), mb, ifelse(K == "C", paste0(mb, "|", ifelse(is.na(kv), "", kv)), ifelse(K == "Y", kv,
                 ifelse(K == "G", paste0(av("sector-name", ix), "|", av("subsector-name", ix)), "")))))))
    FL <- ifelse(av("delete", ix) %in% "1", "D", ifelse(av("nocreate", ix) %in% "1", "N", "M")); FL[K == "G"] <- "M"
    if (s2) FL[K == "C" & mb %in% gm & FL == "N"] <- "M"                       # the global technology database exists with <world> (world.cpp:95-99)
    mp <- rep("", N)                                                           # steps from <scenario>, for stage 2 only
    if (!is.null(config)) {
      yk <- K == "Y" | (K == "C" & ka %in% "year"); kn <- ifelse(!is.na(kv) & yk & grepl("^[-+]?[0-9]{1,9}$", kv), as.character(suppressWarnings(as.integer(kv))), kv)
      st0 <- ifelse(K %in% c("C", "Y", "G"), paste0(K, "|", ifelse(K == "C", paste0(mb, "|", ifelse(is.na(kn), "", kn)), ifelse(K == "Y", kn, paste0(av("sector-name", ix), "|", av("subsector-name", ix))))), NA)
      for (d in seq_len(max(dep))[-1]) { w <- which(dep == d); mp[w] <- ifelse(is.na(st0[w]), mp[pa[w]], paste0(mp[pa[w]], "\n", st0[w])) }
    }
    fz <- function(i, n) { attr(i, "levels") <- as.character(seq_len(n)); class(i) <- "factor"; i }
    KD <- split(ix, fz(pa + 1L, N + 1L))                                       # children of row g at g + 1
    rowc <- function(e, drop = character(0)) {
      a <- setdiff(an, drop); a <- a[vapply(a, function(z) any(!is.na(at[[z]][e])), NA)]
      paste0(tag[e], if (length(a)) do.call(paste0, lapply(a, function(z) { v <- at[[z]][e]; ifelse(is.na(v), "", paste0(" ", z, "=\"", v, "\"")) })) else "",
             "\t", ifelse(is.na(tx[e]), "\001", tx[e]), recycle0 = TRUE)
    }
    can <- function(e, drop = character(0)) {                                # canonical subtree text: tags, sorted attributes, leaf text
      if (!length(e)) return(character(0))
      j <- lapply(e, function(i) i:ed[i]); n <- lengths(j); J <- unlist(j, use.names = FALSE); r <- rowc(J)
      if (length(drop)) r[cumsum(c(1L, n[-length(n)]))] <- rowc(e, drop)
      vapply(split(paste0(dep[J] - rep(dep[e], n), ":", r), fz(rep(seq_along(e), n), length(e))), paste, "", collapse = "\n", USE.NAMES = FALSE)
    }
    vl <- function(e) { v <- tx[e]; w <- is.na(v); if (any(w)) v[w] <- can(e[w]); v }
    pnm <- rep(NA_character_, N); z <- which(sk == "S|name" & pa > 0L); z <- z[K[pa[z]] %in% "Y"]; if (length(z)) pnm[pa[z]] <- vl(z)   # each period's last <name> child
    cw <- which(K %in% c("C", "Y", "G") | dep == 1L)                          # SIMPLE fields given as attributes of an object element
    awg <- integer(0); awk <- awv <- character(0); kwa <- integer(0)
    for (a in setdiff(an, c("fillout", "delete", "nocreate"))) {
      w <- cw[!is.na(at[[a]][cw])]; if (a == "name") w <- w[K[w] != "Y"]; if (!length(w)) next   # a period's own name attribute: GCAM reads its container's name in its place (technology_container.cpp:154)
      r <- match(paste(cx[w], a, sep = "\r"), rk)
      u <- w[atr[r] %in% TRUE]; awg <- c(awg, u); awk <- c(awk, rep(if (a == "type-flags") "F|" else paste0("S|", a), length(u))); awv <- c(awv, at[[a]][u])
      u <- w[rules$kind[r] %in% "SETTRUE"]; awg <- c(awg, u); awk <- c(awk, rep(paste0("T|", a), length(u))); awv <- c(awv, at[[a]][u])   # a set-true flag also read from an attribute (true or false, mac_control.h:94)
      kwa <- c(kwa, w[rules$kind[r] %in% "KEYWORD"])
    }
    AW <- split(seq_along(awg), fz(awg, N))
    raw <- function(l) { v <- character(length(l)); for (f in unique(fl[l])) { w <- fl[l] == f; v[w] <- substring(S[[f]], fi[l[w]], la[l[w]]) }; v }
    made <- function(g, pm) if (K[g] != "Y") tag[g] else if (pm == "replay") "class of the global technology's first period" else paste0(cx[g], if (pm == "?") " or the earlier technology's period class")
    fg <- function(e, a) {                                                     # type-flags resets, flag adds
      g <- c(e, awg[a]); r <- c(tag[e] == "type-flags", rep(TRUE, length(a))); v <- c(vl(e), awv[a]); o <- order(g); r <- r[o]; v <- v[o]
      l <- max(c(0L, which(r))); paste(c(if (l) v[l] else "\001", sort(unique(v[seq_along(v) > l & !r]))), collapse = " + ")
    }
    kw <- function(e) { v <- character(0); for (i in e) for (a in an) { x <- at[[a]][i]; if (!is.na(x) && !a %in% names(v)) v[a] <- x }; v[order(names(v))] }
    nkey <- function(a, k) {                                                   # key as GCAM reads it (tax, price: double; year: unsigned int); NA if it may read otherwise
      if (a == "year") return(ifelse(grepl("^0*[1-9][0-9]{0,8}$", k), as.character(suppressWarnings(as.integer(k))), NA))
      if (!a %in% c("tax", "price")) return(ifelse(is.na(k), "", k))
      d <- nchar(gsub("[^0-9]", "", sub("[eE].*$", "", sub("^-?0*", "", k))))
      ok <- grepl("^-?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]{1,2})?$", k) & d <= 15L   # at most 15 digits: distinct decimals are distinct doubles
      x <- sub("^-", "", k); e <- suppressWarnings(ifelse(grepl("[eE]", x), as.integer(sub("^.*[eE]", "", x)), 0L)); x <- sub("[eE].*$", "", x)
      ip <- sub("[.].*$", "", x); ds <- paste0(ip, ifelse(grepl("[.]", x), sub("^.*[.]", "", x), "")); e <- e + nchar(ip)   # exact decimal: 0.ds x 10^e
      z <- sub("^0+", "", ds); e <- e - (nchar(ds) - nchar(z)); z <- sub("0+$", "", z)
      ifelse(!ok, NA, ifelse(!nzchar(z), "0", paste0(ifelse(startsWith(k, "-"), "-", ""), "0.", z, "e", e)))
    }
    km <- function(e) { if (!length(e)) return(character(0)); a <- ka[e[1]]; k <- nkey(a, av(a, e)); u <- !duplicated(k, fromLast = TRUE); x <- setNames(can(e[u], a), k[u]); x[order(names(x))] }
    ru <- function(e) { c0 <- av("apply-to", e) %in% "share-weight" & av("delete", e) %in% "1"; l <- max(c(0L, which(c0))); e <- e[seq_along(e) >= max(1L, l)]
                        if (s2 && length(yv) > 1L) e <- e[!vapply(e, function(g) {     # sets no model year: from and to adjacent model years, both values, a function other than fixed (interpolation_rule.cpp:111-136, 205-320)
                          d <- KD[[g + 1L]]; f <- d[tag[d] == "interpolation-function" & av("name", d) %in% c("linear", "s-curve", "fixed")]; y1 <- av("from-year", g); y2 <- av("to-year", g)
                          i <- if (grepl("^[-+]?[0-9]{1,9}$", y1)) match(as.integer(y1), yv) else NA; if (identical(y2, "9999") && length(d)) y2 <- as.character(yv[length(yv)])   # 9999: the end year (:112-115)
                          length(f) > 0L && !av("name", f[length(f)]) %in% "fixed" && any(tag[d] == "from-value") && any(tag[d] == "to-value") && !is.na(i) && i < length(yv) &&
                            grepl("^[-+]?[0-9]{1,9}$", y2) && as.integer(y2) == yv[i + 1L] }, NA)]
                        list(c = l > 0L, r = can(e, c("apply-to", "delete", "nocreate", "fillout"))) }
    cbp <- function(e) {                                                       # (from-name, to-name) maps; a block whose pairs a later block holds is rewritten by it
      pr <- lapply(e, function(i) { d <- KD[[i + 1L]]; d <- d[tag[d] == "sector-map"]; fr <- av("from-name", d); to <- av("to-name", d); fr[is.na(fr)] <- ""; to[is.na(to)] <- ""
                                    u <- !duplicated(fr, fromLast = TRUE); sort(paste0(fr[u], "->", to[u])) })
      k <- vapply(seq_along(e), function(i) !any(vapply(seq_along(e), function(j) j > i && all(pr[[i]] %in% pr[[j]]), NA)), NA)
      vapply(pr[k], paste, "", collapse = ", ")
    }
    seen <- new.env(hash = TRUE, parent = emptyenv())
    # one object: element lists l1 (run 1) and l2 (run 2); w "abs" when it cannot exist before them, "unk" otherwise;
    # pm: how new periods get their class (stub replay, known technology, unknown); obj and kids are memoised
    obj <- function(path, l1, l2, w, pm) {
      key <- paste(l1[1], length(l1), l2[1], length(l2), w, pm); if (!is.null(seen[[key]])) return(invisible()); seen[[key]] <- TRUE
      if (s2 && mb[c(l1, l2)[1]] %in% gm && !"D" %in% FL[c(l1, l2)]) return(kids(path, l1, l2, TRUE, "abs", "", "one"))   # created empty with <world> in both runs (world.cpp:95-99)
      cs <- function(l) {
        f <- FL[l]; d <- max(c(0L, which(f == "D")))
        mk <- function(from) { j <- which(seq_along(l) > from & f == "M")[1]; if (is.na(j)) list(st = "absent") else list(st = "created", g = l[j], from = j) }
        if (d) { x <- mk(d); list(abs = x, has = x) } else list(abs = mk(0L), has = list(st = "merged into the earlier one", from = 1L))
      }
      c1 <- cs(l1); c2 <- cs(l2); cl <- cx[c(l1, l2)]; tc <- any(startsWith(cl, "TechnologyContainer<") | cl == "StubTechnologyContainer")
      ws <- if (w == "abs") "abs" else if (identical(c1$abs, c1$has) && identical(c2$abs, c2$has)) "one" else c("abs", "has")
      sub <- length(ws) == 2L && !tc && c1$abs$st == "created" && c2$abs$st == "created" && c1$abs$from == 1L && c2$abs$from == 1L   # has case covers abs case
      for (wn in ws) {
        a <- c1[[if (wn == "one") "abs" else wn]]; b <- c2[[if (wn == "one") "abs" else wn]]
        if (a$st != b$st) { bad(path, "existence", wn, a$st, b$st); next }
        if (a$st == "absent") next
        p1 <- l1[seq_along(l1) >= a$from]; p2 <- l2[seq_along(l2) >= b$from]
        if (a$st == "created") {
          m1 <- made(a$g, pm); m2 <- made(b$g, pm)
          if (m1 != m2) { bad(path, "created as", wn, m1, m2); next }
          cc <- cx[c(p1, p2)]; ty1 <- cc == cx[a$g] | cc == "ITechnology" | cx[a$g] == "ITechnology" | (cx[a$g] == "StubTechnologyContainer" & startsWith(cc, "TechnologyContainer<")); ok <- all(ty1)   # a stub queues the children of any later tag
          if (!ok) {                                                           # other tags merge into the creating class (xml_parse_helper.cpp:443-446, 545-548): their children and attributes must read the same there
            sg <- function(cl, t) { r <- match(paste(cl, t, sep = "\r", recycle0 = TRUE), rk)   # row signature: kind, member, class created (left out for a period: obj() compares each period's class), key, order
              paste(rules$kind[r], mem[r], ifelse(rules$kind[r] %in% "PERIOD", "", rules$creates[r]), rules$key_attribute[r], rules$order_policy[r], recycle0 = TRUE) }
            o <- c(p1, p2)[!ty1]; ch <- unlist(KD[o + 1L], use.names = FALSE)
            oa <- unlist(lapply(o, function(g) paste(cx[g], setdiff(an[vapply(an, function(z) !is.na(at[[z]][g]), NA)], c("name", "fillout", "delete", "nocreate")), sep = "\r", recycle0 = TRUE)))
            ok <- identical(sg(cx[pa[ch]], tag[ch]), sg(cx[a$g], tag[ch])) && identical(sg(sub("\r.*", "", oa), sub("^.*\r", "", oa)), sg(cx[a$g], sub("^.*\r", "", oa)))
          }
          if (!ok) { bad(path, "written with tags of different types", wn, unique(tag[p1]), unique(tag[p2])); next }
          if (sub) next
          sb <- cx[a$g] == "StubTechnologyContainer"                         # its children replay onto the global technology
          kids(path, p1, p2, !sb, if (sb) "unk" else "abs", if (sb || pm == "replay") "replay" else "", wn)
        } else kids(path, p1, p2, FALSE, "unk", if (pm == "replay") "replay" else if (tc) "?" else "", if (sub) "one" else wn)
      }
    }
    # the children of object elements p1 and p2 (empty: the object held nothing before them)
    kids <- function(path, p1, p2, empty, w, pm, wn) {
      key <- paste("k", p1[1], length(p1), p2[1], length(p2), empty, w, pm, wn); if (!is.null(seen[[key]])) return(invisible()); seen[[key]] <- TRUE
      k1 <- as.integer(unlist(KD[p1 + 1L], use.names = FALSE)); k2 <- as.integer(unlist(KD[p2 + 1L], use.names = FALSE))
      a1 <- as.integer(unlist(AW[p1], use.names = FALSE)); a2 <- as.integer(unlist(AW[p2], use.names = FALSE))
      if (any(c(p1, p2) %in% kwa)) bad(path, "keyword map given as an attribute", wn, "", "")
      for (z in c(k1, k2)[kq[c(k1, k2)]]) bad(paste0(path, "/", tag[z], "[", kv[z], "]"), if (nzchar(kv[z])) "year key not a whole number without leading zeros (GCAM reads it as a number)"
                                              else "empty key attribute (GCAM matches it to an entry made without one)", wn, kv[z], "")
      ky <- unique(c(sk[k1], sk[k2], awk[a1], awk[a2])); if (!length(ky)) return(invisible())
      nk <- length(ky); ty <- substr(ky, 1L, 1L); c1 <- match(sk[k1], ky); c2 <- match(sk[k2], ky); b1 <- match(awk[a1], ky); b2 <- match(awk[a2], ky)
      lb <- function(j, z) paste0(path, "/", if (is.na(z)) sub("^.\\|", "", ky[j]) else tag[z], if (is.na(z)) "" else if (ty[j] %in% c("C", "Y") && !is.na(kv[z])) paste0("[", kv[z], "]")
                                  else if (ty[j] == "G") paste0("[", av("sector-name", z), ", ", av("subsector-name", z), "]") else "")
      lw <- function(k, cc, a, ca) {                                          # last write of each SIMPLE slot (attributes before children)
        r <- rep("\001", nk); g <- integer(nk)
        j <- which(ty[ca] == "S"); j <- j[!duplicated(ca[j], fromLast = TRUE)]; r[ca[j]] <- awv[a[j]]; g[ca[j]] <- awg[a[j]]
        i <- which(ty[cc] == "S"); i <- i[!duplicated(cc[i], fromLast = TRUE)]; i <- i[k[i] > g[cc[i]]]; r[cc[i]] <- vl(k[i]); r
      }
      v1 <- lw(k1, c1, a1, b1); v2 <- lw(k2, c2, a2, b2); dv <- rep(NA_character_, nk)
      if (s2 && empty && pm != "replay") {                                    # a new object outside a stub-technology queue: a field nothing writes holds the
        dv <- vapply(seq_len(nk), function(j) {                               # default of the class its first element creates, as GCAM reads it back (NA: not known)
          r <- match(paste(unique(stats::na.omit(cx[c(p1[1], p2[1])])), if (ty[j] == "F") "type-flags" else sub("^.\\|", "", ky[j]), sep = "\r", recycle0 = TRUE), rk)
          if (!ty[j] %in% c("S", "A", "F")) r <- NA_integer_
          d <- rules$default[r]; vt <- rules$value_type[r]
          v <- ifelse(vt %in% "Value" & d %in% "unset", "\001", ifelse(vt %in% "bool" & d %in% c("true", "false"), ifelse(d %in% "true", "1", "0"),
               ifelse(vt %in% c("double", "Value", "int", "unsigned int") & grepl("^-?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?$", d), d,
               ifelse(vt %in% c("gcamstr", "std::string") & !is.na(d) & !d %in% "NA" & !grepl("[[:space:]&<>\"']", d), d, NA_character_))))
          if (length(unique(v)) == 1L) v[1] else NA_character_ }, "")
        ws <- ty == "S" & dv %in% "\001"; if (neg0) { v1[ws & grepl(nz, v1)] <- "\001"; v2[ws & grepl(nz, v2)] <- "\001" }   # -0: not set (value.h:161)
        ws <- ty == "S" & !is.na(dv); v1[ws & v1 == "\001"] <- dv[ws & v1 == "\001"]; v2[ws & v2 == "\001"] <- dv[ws & v2 == "\001"]
      }
      vn <- pm != "replay" && all(K[c(p1, p2)] == "Y") && any(startsWith(cx[pa[c(p1, p2)]], "TechnologyContainer<"))   # a technology's period: its name is compared with the technology's below
      for (j in which(ty == "S" & v1 != v2 & !(vn & ky == "S|name"))) bad(lb(j, c(k2[c2 %in% j], k1[c1 %in% j])[1]), "value", wn, v1[j], v2[j])
      if (pm != "replay" && any(startsWith(cx[c(p1, p2)], "TechnologyContainer<")) && any(ty == "Y")) {
        # vintage names in a technology container not made by a stub-technology (a subresource's one container is reused by any later technology
        # tag, xml_parse_helper.cpp:443-446): each period read takes the container's name there (technology_container.cpp:154), the last name
        # attribute (set before the element's children, xml_parse_helper.cpp:824-839) or <name> child before it ("" in a new container, "\001" as
        # before these files), and a <name> child of the period read last is set after it; compared for each year there in the end in both runs
        # ("(not read)": no element of that year in this run, the name from before)
        js <- which(ty == "Y")
        x <- lapply(list(list(k1, c1, a1), list(k2, c2, a2)), function(r) {
          u <- r[[3]][awk[r[[3]]] == "S|name"]; z <- r[[1]][sk[r[[1]]] == "S|name"]; t <- c(awg[u], z); v <- c(if (empty) "" else "\001", c(awv[u], vl(z))[order(t)]); t <- sort(t)
          o <- rep(if (empty) NA_character_ else "(not read)", length(js)); w <- r[[2]] %in% js; if (!any(w)) return(o)
          e <- r[[1]][w]; g <- match(r[[2]][w], js); i <- seq_along(e)          # period elements in file order, the year of each, and per year
          li <- ld <- lm <- integer(length(js)); li[g] <- i; h <- FL[e] == "D"; ld[g[h]] <- i[h]; h <- FL[e] == "M"; lm[g[h]] <- i[h]   # the last element, delete, create
          h <- li > 0L & !((ld > 0L | empty) & lm <= ld); o[li > 0L] <- NA_character_   # no period of that year in the end: NA
          l <- e[li[h]]; o[h] <- ifelse(is.na(pnm[l]), v[findInterval(l, t) + 1L], pnm[l]); o })
        for (i in which(!is.na(x[[1]]) & !is.na(x[[2]]) & x[[1]] != x[[2]])) bad(paste0(lb(js[i], c(k2[c2 %in% js[i]], k1[c1 %in% js[i]])[1]), "/name"), "value", wn, x[[1]][i], x[[2]][i])
      }
      n1 <- ty[c1] != "S"; n2 <- ty[c2] != "S"; k1 <- k1[n1]; c1 <- c1[n1]; k2 <- k2[n2]; c2 <- c2[n2]
      g1 <- split(k1, fz(c1, nk)); g2 <- split(k2, fz(c2, nk)); if (any(ty %in% c("F", "T"))) { h1 <- split(a1, fz(b1, nk)); h2 <- split(a2, fz(b2, nk)) }
      sm <- function(r, cc) vapply(split(r, fz(cc, nk)), paste, "", collapse = "\001", USE.NAMES = FALSE)   # slots with the same bytes in both runs
      eq <- ty == "S" | (tabulate(c1, nk) == tabulate(c2, nk) & tabulate(c1[fl[k1] == np + 2L], nk) == 0L & tabulate(c(b1, b2), nk) == 0L &
                         (length(unique(cx[c(p1, p2)])) == 1L | !ty %in% c("C", "Y", "G")))
      w0 <- eq & ty != "S"; if (any(w0)) eq[w0] <- (sm(raw(k1), c1) == sm(raw(k2), c2))[w0]
      for (j in which(!eq)) {
        e1 <- g1[[j]]; e2 <- g2[[j]]; t <- ty[j]; z <- c(e2, e1)[1]; lab <- lb(j, z)
        if (t %in% c("C", "Y", "G")) { obj(lab, e1, e2, w, pm); next }
        if (t == "?") {
          if (any(fl[e1] == np + 2L)) bad(lab, "tag unknown to GCAM (skipped) in the add-on", wn, "", "")
          if (!identical(can(e1), can(e2))) bad(lab, "tag unknown to GCAM (skipped)", wn, can(e1), can(e2))
          next
        }
        if (t == "L" && any(FL[c(e1, e2)] != "M")) { bad(lab, "delete or nocreate on an entry without a key", wn, tag[e1], tag[e2]); next }
        if (t == "M" && anyNA(nkey(ka[z], av(ka[z], c(e1, e2))))) { bad(lab, "key-map key not a plain number (GCAM reads it as one)", wn, "", ""); next }
        x <- switch(t, F = lapply(list(fg(e1, h1[[j]]), fg(e2, h2[[j]])), function(v) if (!is.na(dv[j]) && startsWith(v, "\001")) paste0(dv[j], substring(v, 2L)) else v),
                    A = { y <- function(e) { e <- e[!is.na(av("year", e))]; v <- vl(e); if (neg0 && dv[j] %in% "\001") v[grepl(nz, v)] <- "\001"   # no year: sets nothing
                                             z <- list(y = suppressWarnings(as.numeric(av("year", e))), f = av("fillout", e) %in% "1", v = v)
                                             if (!is.na(dv[j]) && length(yv)) list(y = c(yv[1], z$y), f = c(TRUE, z$f), v = c(dv[j], z$v)) else z }   # nothing written: the default
                          if (arr(y(e1), y(e2))) NULL else list(paste0(av("year", e1), ifelse(av("fillout", e1) %in% "1", "+", ""), "=", vl(e1), recycle0 = TRUE), paste0(av("year", e2), ifelse(av("fillout", e2) %in% "1", "+", ""), "=", vl(e2), recycle0 = TRUE)) },
                    K = list(kw(e1), kw(e2)), M = list(km(e1), km(e2)),
                    R = { r1 <- ru(e1); r2 <- ru(e2); if (identical(r1$r, r2$r) && (empty || r1$c == r2$c)) NULL
                          else if (r1$c == r2$c && length(r1$r) == length(r2$r)) list(r1$r, r2$r)   # same count: the rules themselves
                          else list(c(if (r1$c) "cleared", sprintf("%d rule(s)", length(r1$r))), c(if (r2$c) "cleared", sprintf("%d rule(s)", length(r2$r)))) },
                    P = list(if (length(e1)) can(e1[length(e1)]) else "\001", if (length(e2)) can(e2[length(e2)]) else "\001"),
                    T = lapply(list(list(e1, h1[[j]]), list(e2, h2[[j]])), function(z) {   # the last write: an attribute before its element's children (false: not set in a new object;
                      g <- awg[z[[2]]]                                                     # other attribute text quoted: GCAM stops on it, and it never equals an element's write)
                      v <- c(rep("true", length(z[[1]])), ifelse(grepl("^[+]?0*1$", awv[z[[2]]]), "true", ifelse(grepl("^[+-]?0+$", awv[z[[2]]]), if (empty) "\001" else "false", paste0("\"", awv[z[[2]]], "\""))))
                      if (length(v)) v[which.max(c(fl[z[[1]]], fl[g]) * 2^32 + c(fi[z[[1]]], fi[g]))] else "\001" }),
                    O = list(if (length(e1)) can(e1[1]) else "\001", if (length(e2)) can(e2[1]) else "\001"),
                    L = if (!is.na(z) && tag[z] == "calc-base-price") list(cbp(e1), cbp(e2)) else list(can(e1), can(e2)))
        if (is.null(x) || identical(x[[1]], x[[2]])) next
        if (t %in% c("K", "M")) { u <- union(names(x[[1]]), names(x[[2]])); u <- u[is.na(x[[1]][u]) | is.na(x[[2]][u]) | x[[1]][u] != x[[2]][u]]
                                  x <- lapply(x, function(v) paste0(u, "=", ifelse(is.na(v[u]), "(none)", v[u]))) }
        if (t %in% c("L", "O", "P") && length(x[[1]]) + length(x[[2]]) > 2L) x <- lapply(x, function(v) sprintf("%d element(s)", length(v)))
        bad(lab, c(F = "input flags", A = "values by year", K = "keywords", M = "key map", R = "rule list", P = "replaced element",
                   T = "set-true flag", O = "element read once", L = "list without keys")[[t]], if (t == "R" && wn == "one" && identical(r1$r, r2$r)) "has" else wn, x[[1]], x[[2]])
      }
      cr <- which(startsWith(ky, "C|") | startsWith(ky, "L|"))               # entry order in order-critical lists (entries without a key: each one created)
      if (length(cr)) {
        z <- vapply(cr, function(j) c(g2[[j]], g1[[j]])[1], 1L); rz <- ri[z]
        if (any(cx[pa[c(p1, p2)]] %in% "StubTechnologyContainer")) { r <- match(paste("ITechnology", tag[z], sep = "\r"), rk); rz[!is.na(r)] <- r[!is.na(r)] }   # periods a stub queues: ITechnology rows
        q <- rules$order_policy[rz] %in% "critical"
        for (m in unique(mb[z[q]])) {
          js <- cr[q][mb[z[q]] == m]
          ev <- function(g) {
            ps <- integer(0); lk <- character(0); c0 <- character(length(js))
            for (i in seq_along(js)) {
              e <- g[[js[i]]]
              if (ty[js[i]] == "L") { e <- e[FL[e] == "M"]; ps <- c(ps, e); lk <- c(lk, rep(NA_character_, length(e))); next }
              f <- FL[e]; d <- max(c(0L, which(f == "D"))); u <- which(seq_along(e) > d & f == "M")[1]
              c0[i] <- if (is.na(u)) { if (d) "X" else "N" } else { ps <- c(ps, e[u]); lk <- c(lk, kv[e[u]]); if (d) "R" else "C" }
            }
            lk <- lk[order(ps)]; w <- is.na(lk); lk[w] <- paste0("#", seq_len(sum(w)), " (no key)"); list(c = c0, o = lk)
          }
          x1 <- ev(g1); x2 <- ev(g2)
          if (!identical(x1$o, x2$o)) bad(paste0(path, "/", tag[z[q]][1]), "entry order", wn, x1$o, x2$o)
          else if (!empty && !identical(x1$c, x2$c)) bad(paste0(path, "/", tag[z[q]][1]), "entry order (entries created, kept or rebuilt)", if (wn == "one") "has" else wn, x1$c, x2$c)
        }
      }
    }
    rt <- KD[[1]]
    if (s2) kids("scenario", rt[fl[rt] != np + 3L], rt[fl[rt] <= np | fl[rt] == np + 3L], TRUE, "abs", "", "one")   # earlier files in both runs, nothing before them
    else kids("scenario", rt[fl[rt] != 3L], rt[fl[rt] == 3L], FALSE, "unk", "", "one")
  }
  TB <- top(b_file); TA <- top(a_file); TO <- top(o_file)
  ks <- unique(c(TB$k$key, TO$k$key, TA$k$key))
  sel <- ks[ks %in% TA$k$key | vapply(ks, function(k) !identical(TB$h[TB$k$key == k], TO$h[TO$k$key == k]), NA)]
  kk <- character(0); ga <- list()                                           # groups that differ, and the objects where ("": all of it)
  for (k in c(list(NULL), as.list(sel))) {
    n0 <- mm$n; grp(k)
    if (mm$n > n0) { kk <- c(kk, if (is.null(k)) NA else k); ga[[length(kk)]] <- if (mm$n <= 2000L) unique(vapply(mm$x[(n0 + 1L):mm$n], `[`, "", 6L)) else "" }
  }
  if (mm$n && !is.null(config)) {                                            # stage 2: those groups again, after the earlier files
    cc <- config_components(config, basename(b_file)); P <- cc$path[seq_len(cc$b - 1L)]; P <- P[file.exists(P)]   # GCAM stops at a missing file
    if (!is.null(cc$years)) {
      yv <- mtime(cc$path[1])
      if (!identical(yv, cc$years)) stop(sprintf("the model years read from %s here (%s) differ from config_components' (%s)", basename(cc$path[1]),
                                                 if (is.null(yv)) "none" else paste(yv, collapse = ", "), paste(cc$years, collapse = ", ")), call. = FALSE)
    }
    rf <- nm_opt("ram_floor_gb", 4)
    TP <- lapply(P, function(f) tryCatch(loc(f), error = function(e) {       # a failed allocation: the memory form
      if (!grepl("cannot allocate", conditionMessage(e), fixed = TRUE)) stop(e)
      stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s; try again later",
                   ps::ps_system_memory()$avail / 2^30, rf, 0.2 + 15.6 * min(file.size(f), cap) / 2^30, "the whole file", basename(f)), call. = FALSE) }))
    bs <- matrix(vapply(kk, function(k) vapply(TP, function(T) if (is.na(k)) sum(as.numeric(lengths(T$sb))) else sum(T$k$z[T$k$key %in% k] - T$k$a[T$k$key %in% k] + 1), 0),
                        numeric(length(TP))), length(TP))                    # bytes of each group's blocks in each earlier file
    if (length(bs)) {                                                        # memory: the largest block read alone, then each group read whole
      h <- which.max(bs) - 1L; i <- h %% length(TP) + 1L; j <- match(kk[h %/% length(TP) + 1L], TP[[i]]$k$key)
      need <- 0.2 + 15.6 * max(bs) / 2^30; free <- ps::ps_system_memory()$avail / 2^30
      if (free < rf + need) stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s; try again later",
                                         free, rf, need, if (is.na(j)) "the parts outside <world>" else paste0("block ", TP[[i]]$k$t[j], if (!is.na(TP[[i]]$k$n[j])) paste0(" ", TP[[i]]$k$n[j])), basename(P[i])), call. = FALSE)
    }
    mm$n <- 0L; mm$x <- list(); s2 <- TRUE
    for (q in seq_along(kk)) {
      k <- if (is.na(kk[q])) NULL else kk[q]; pd <- list(); pn <- character(0)
      for (i in seq_along(TP)) {
        if (!is.null(k) && !k %in% TP[[i]]$k$key) next
        j <- which(TP[[i]]$k$key %in% k); sz <- if (is.null(k)) sum(as.numeric(lengths(TP[[i]]$sb))) else sum(TP[[i]]$k$z[j] - TP[[i]]$k$a[j] + 1)   # this block, with the parts held so far
        bl <- if (is.null(k)) "the parts outside <world>" else paste0("block ", TP[[i]]$k$t[j[1]], if (!is.na(TP[[i]]$k$n[j[1]])) paste0(" ", TP[[i]]$k$n[j[1]]))
        need <- 0.2 + 15.6 * (sz + sum(as.numeric(lengths(pd)))) / 2^30; free <- ps::ps_system_memory()$avail / 2^30
        if (free < rf + need) stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s; try again later",
                                           free, rf, need, bl, basename(P[i])), call. = FALSE)
        r <- doc(TP[[i]], k); x <- tryCatch(unclass(xml_tokens(r)), error = function(e) stop(if (grepl("cannot allocate", conditionMessage(e), fixed = TRUE))   # a failed allocation: the memory form
          sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s; try again later",
                  ps::ps_system_memory()$avail / 2^30, rf, need, bl, basename(P[i])) else paste0(basename(P[i]), ": ", conditionMessage(e)), call. = FALSE))
        s <- rawToChar(r); Encoding(s) <- "bytes"; rm(r)
        pd[[length(pd) + 1L]] <- prune(x, s, ga[[q]]); pn <- c(pn, P[i]); rm(x, s); if (sz >= 16 * 2^20) invisible(gc())
      }
      need <- 0.2 + 15.6 * (sum(as.numeric(lengths(pd))) + sum(vapply(list(TB, TA, TO), function(T) if (is.null(k)) sum(as.numeric(lengths(T$sb)))
                                                                      else sum(T$k$z[T$k$key %in% k] - T$k$a[T$k$key %in% k] + 1), 0))) / 2^30; free <- ps::ps_system_memory()$avail / 2^30
      if (free < rf + need) { if (!is.null(k)) { T <- Find(function(T) k %in% T$k$key, list(TO, TB, TA)); j <- match(k, T$k$key) }
        stop(sprintf("addon_minaj: only %.1f GB of memory is free, under the floor of %g GB (option minaj.ram_floor_gb) plus about %.1f GB for reading %s of %s, %s and %s with the parts of %d earlier %s; try again later",
                     free, rf, need, if (is.null(k)) "the parts outside <world>" else paste0("block ", T$k$t[j], if (!is.na(T$k$n[j])) paste0(" ", T$k$n[j])),
                     basename(b_file), basename(a_file), basename(o_file), length(pd), if (length(pd) == 1L) "file" else "files"), call. = FALSE) }
      grp(k)
    }
  }
  x <- matrix(vapply(unlist(mm$x[seq_len(min(mm$n, 2000L))]), function(z) rawToChar(charToRaw(z)), "", USE.NAMES = FALSE), ncol = 6, byrow = TRUE)
  g <- paste(x[, 1], x[, 2], x[, 3], x[, 4], sep = "\r"); u <- !duplicated(g)  # one row for a difference found for several earlier states
  w <- vapply(g[u], function(h) { v <- unique(x[g == h, 5]); if (length(v) == 1L) v else "one" }, "", USE.NAMES = FALSE)
  mi <- data.frame(target = x[u, 1], column = paste0(x[u, 2], if (s2) paste0(" (for the earlier files of ", basename(config), ")") else wl[w], recycle0 = TRUE),
                   old = x[u, 3], new = x[u, 4], stringsAsFactors = FALSE)
  if (mm$n > 2000L) mi[nrow(mi) + 1L, ] <- list(sprintf("(%d more differences not listed)", mm$n - 2000L), "", "", "")
  list(ok = mm$n == 0L, mismatches = mi)
}
