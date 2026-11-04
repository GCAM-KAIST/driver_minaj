# Usage (from the package root): Rscript tests/addon/mutation.R [vanilla 9.1 gcamdata xml folder] [config=<path>] [file.xml ...] [seed]
#   Random edits to real blocks of vanilla XML files (value change, row drop, logit swap, rule change, gas drop, grade
#   insert and others). For each edit addon_write must give the expected verdict, a refusal must name the place and
#   say whether config= can decide it, and a written add-on must agree with a naive full-file simulator (below: GCAM's
#   parse rules applied to whole files, no blocks, no pruning): base then add-on against the override, from no earlier
#   file, from earlier files that create every object, and (files under 20 MB) after the base file itself. Wrong
#   add-ons made on purpose (negative controls) must differ under the naive simulator and under addon_sim. An internal
#   error fails, and so does a refusal for a nocreate pattern in a *_MAC.xml file (each slot there starts with the
#   element that creates it). MINAJ_HOME and TMPDIR as in unit.R. Files over 50 MB are read only while at least 4 GB of
#   memory is free, with fewer edits; files over 10 MB get one edit of each kind.
#   Given a folder of unit.R's files (b<k>.xml, a<k>.xml, o<k>.xml and sb<k>.xml, sa<k>.xml, so<k>.xml), it runs the
#   naive simulator on each: written add-ons must agree, and it must agree with addon_sim's verdict on each check. A
#   triple with a configuration c<k>.xml beside it is run once each way after that configuration's earlier files, from
#   nothing, for its model years (both read here with xml2).
#   config=<path>: one base file per run, the one the configuration loads with that name, with the configuration's real
#   earlier files; addon_write and addon_sim get the configuration. One edit each: value change, row drop, logit swap,
#   rule change, keyword change, gas drop, grade insert, value un-set. Expected: written, or refused with a reason from
#   the earlier files; never an internal error. Each written add-on is simulated after the earlier files' blocks of the
#   parts the edit changes (read here by byte offsets, not through prefix_scan), from nothing, for the configuration's
#   model years; its names must be in the base, the override or an earlier file (searched by bytes). Wrong add-ons as
#   above, plus the add-on written as if no earlier file touched the base file's objects. 'xmlscan' records go to a
#   temporary store, kept for the run. Needs 4 GB free; where an edit's earlier-file blocks need more (about 31 times
#   their size, above 4 GB free), the naive simulator checks after them are skipped and counted in the last line, and the
#   other checks run; minaj's own memory stop is counted there too. For each edit whose blocks are read, R's memory peak
#   per GB of blocks is printed (ag_storage.xml: 23 to 30 times). Runs on vanilla configuration_ref.xml, one base each:
#   electricity_water.xml 3.4 to 5.1 min (peak 1.0 GB), en_supply.xml 1.3 min, all_energy_emissions_MAC.xml 6.4 min
#   (67 earlier files, 3.0 GB; peak 2.8 GB); stopped at 8 min after 2 edits of gas_trade.xml (76 earlier files, 3.2 GB;
#   peak 6.3 GB) and 3 of ag_storage.xml (44 earlier files).
args <- commandArgs(trailingOnly = TRUE)
if (nzchar(Sys.getenv("MINAJ_HOME"))) suppressMessages(pkgload::load_all(Sys.getenv("MINAJ_HOME"), quiet = TRUE)) else library(minaj)
ns <- asNamespace("minaj")
seed <- suppressWarnings(as.integer(args[grepl("^[0-9]+$", args)][1])); if (is.na(seed)) seed <- 1L
args <- args[!grepl("^[0-9]+$", args)]
config <- sub("^config=", "", args[startsWith(args, "config=")]); args <- args[!startsWith(args, "config=")]
config <- if (length(config)) normalizePath(config[length(config)], "/") else NULL
xml_dir <- if (length(args)) args[1] else if (nzchar(Sys.getenv("MINAJ_TEST_TREE"))) file.path(Sys.getenv("MINAJ_TEST_TREE"), "input", "gcamdata", "xml") else
  stop("usage: Rscript tests/addon/mutation.R <vanilla 9.1 gcamdata xml folder> ..., or set MINAJ_TEST_TREE", call. = FALSE)
tri <- length(args) == 1L && length(list.files(xml_dir, "^s?b[0-9]+\\.xml$")) > 0L
files <- if (length(args) > 1) args[-1] else c("electricity.xml", "electricity_water.xml", "en_supply.xml", "gas_trade.xml", "land_input_3_IRR.xml", "resources.xml",
                                               "bio_trade.xml", "ag_storage.xml", "socioeconomics_macro.xml", "all_energy_emissions_MAC.xml", "all_fgas_emissions_MAC.xml",
                                               "all_fgas_emissions.xml", "all_energy_emissions.xml")
out_dir <- file.path(tempdir(), "mutation"); dir.create(out_dir, showWarnings = FALSE)
if (!is.null(config)) {
  if (length(files) != 1L) stop("config= takes one base file per run")
  if (ps::ps_system_memory()$avail / 2^30 < 4) { cat("SKIP config mode (less than 4 GB free)\n"); quit(status = 0) }
  nm <- get(".nm", ns); nm$store <- nm$state_dir <- tempfile(tmpdir = out_dir); nm$findex <- NULL; dir.create(nm$store, showWarnings = FALSE)
}
n_pass <- 0L; n_fail <- 0L; n_skip <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
  cat(if (isTRUE(ok)) "PASS" else "FAIL", label, "\n")
}
mem_ok <- function() ps::ps_system_memory()$avail / 2^30 >= 4
lowmem <- "addon_minaj: only [0-9.]+ GB of memory is free"                  # minaj's own memory stop: counted as skipped, not failed
options(width = 200, warning.length = 8170)                                   # refusals keep every place in the condition message

local({
  xml_tokens <- get("xml_tokens", ns); addon_write <- get("addon_write", ns); addon_sim <- get("addon_sim", ns)
  rules <- utils::read.csv(system.file("extdata", "gcam_parse_rules.csv", package = "minaj"), colClasses = "character", na.strings = character(0))
  rk <- paste(rules$parent_class, rules$tag, sep = "\r"); kind <- rules$kind
  src <- sub(";.*", "", rules$source); mc <- paste0("m", match(src, unique(src)))   # one code per C++ member
  crit <- rules$order_policy == "critical"
  atr <- kind == "SIMPLE" & grepl("[.](h|hpp):", src)                         # data-vector fields: attributes set them too
  nkey <- function(a, k) {                                                     # key-map key as GCAM reads it ("?" + text when it may read it otherwise)
    v <- suppressWarnings(as.numeric(k)); bad <- is.na(v) | !grepl("^-?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]{1,2})?$", k)
    if (a == "year") return(ifelse(bad | v < 1 | v != round(v) | grepl("[.eE]", k), paste0("?", k), as.character(v)))
    if (a %in% c("tax", "price")) return(ifelse(bad, paste0("?", k), sprintf("%.15g", v + 0)))
    ifelse(is.na(k), "", k)
  }
  gcs <- function(x, g) { o <- order(g, method = "radix"); xs <- x[o]; cs <- cumsum(xs); st <- !duplicated(g[o]); r <- numeric(length(x)); r[o] <- cs - (cs - xs)[st][cumsum(st)]; r }

  # ---- naive simulator -------------------------------------------------------------------------------------------
  # State of GCAM's objects after parsing token tables xs in order, as a named character vector ("place\tfield" =
  # value). pre: every object exists before the first file (empty; class of the first element naming it, or cls0).
  # years: model-year sets for array fields. cbu: sectors for calc-base-price (every from-name, and "*" for any other
  # sector). Stub-technology children are applied in file order (replay on an empty copy). A period read in a technology container
  # (not a stub-technology's) takes the container's name then (technology_container.cpp:154); a period's own name attribute is not read.
  state <- function(xs, pre = FALSE, years, cls0 = NULL, cbu = "*") {
    n <- vapply(xs, function(x) length(x$tag), 1L); off <- c(0L, cumsum(n))[seq_along(n)]; N <- sum(n)
    cl <- function(v) unlist(lapply(xs, `[[`, v), use.names = FALSE)
    tag <- cl("tag"); dep <- cl("depth"); tx <- cl("text")
    par <- unlist(Map(function(x, o) ifelse(x$parent > 0L, x$parent + o, 0L), xs, off), use.names = FALSE)
    ed <- unlist(Map(function(x, o) findInterval(x$last, x$first) + o, xs, off), use.names = FALSE)
    an <- sort(unique(sub("^@", "", grep("^@", unlist(lapply(xs, names)), value = TRUE))))
    at <- setNames(lapply(an, function(a) unlist(lapply(xs, function(x) { v <- x[[paste0("@", a)]]; if (is.null(v)) rep(NA_character_, length(x$tag)) else v }), use.names = FALSE)), an)
    A <- function(a, i) { v <- at[[a]]; if (is.null(v)) rep(NA_character_, length(i)) else v[i] }
    rowc <- function(e, drop = character(0)) {
      s <- tag[e]; for (a in setdiff(an, drop)) { v <- at[[a]][e]; w <- !is.na(v); s[w] <- paste0(s[w], " ", a, "=\"", v[w], "\"") }
      paste0(s, "\t", ifelse(is.na(tx[e]), "", tx[e]))
    }
    can <- function(e, drop = character(0)) {
      if (!length(e)) return(character(0))
      j <- lapply(e, function(i) i:ed[i]); k <- lengths(j); J <- unlist(j, use.names = FALSE); r <- rowc(J)
      if (length(drop)) r[cumsum(c(1L, k[-length(k)]))] <- rowc(e, drop)
      g <- rep(seq_along(e), k); attr(g, "levels") <- as.character(seq_along(e)); class(g) <- "factor"
      vapply(split(paste0(dep[J] - rep(dep[e], k), ":", r), g), paste, "", collapse = "\n", USE.NAMES = FALSE)
    }
    live <- logical(N); ins <- integer(N); ecls <- character(N)              # ecls: class the element's own tag creates
    I <- list(cls = "Scenario", par = 0L, kp = "", dp = "scenario", fin = TRUE, made = TRUE, mk = 0L, mem = "", key = "", crit = FALSE)
    gone <- list(par = integer(0), kp = character(0)); W <- list()
    put <- function(k, i, f, r, v, y = NA_character_, fo = NA) W[[length(W) + 1L]] <<- data.frame(k = k, i = i, f = f, r = r, v = v, y = y, fo = fo, stringsAsFactors = FALSE)
    t1 <- which(dep == 1L & tag == "scenario"); live[t1] <- TRUE; ins[t1] <- 1L
    if (!is.null(at$name)) { u <- t1[!is.na(at$name[t1])]; if (length(u)) put("S", 1L, "name", u, at$name[u]) }
    unk <- 0L
    for (d in seq_len(max(dep))[-1]) {
      w <- which(dep == d); w <- w[live[par[w]]]
      if (!length(w)) next
      pi <- ins[par[w]]; r <- match(paste(I$cls[pi], tag[w], sep = "\r"), rk)
      unk <- unk + sum(is.na(r)); w <- w[!is.na(r)]; pi <- pi[!is.na(r)]; r <- r[!is.na(r)]   # unknown tags: skipped, as GCAM does (and counted)
      k <- kind[r]; q <- k == "STUB_QUEUE"
      k[q] <- ifelse(tag[w[q]] == "period", "PERIOD", ifelse(tag[w[q]] == "interpolation-rule", "RULE", "SIMPLE"))
      k[k == "SIMPLE" & tag[w] == "type-flags"] <- "TF"
      ct <- k %in% c("VEC_NAMED", "VEC_YEAR", "SINGLE", "PERIOD", "GTD_LOCATION")
      if (any(ct)) {                                                         # objects: delete, nocreate, merge or create
        cw <- w[ct]; cr <- r[ct]; cp <- pi[ct]; ka <- rules$key_attribute[cr]; kv <- rep("", length(cw))
        for (a in setdiff(unique(ka), "")) {
          u <- ka == a; sn <- A("sector-name", cw[u]); ss <- A("subsector-name", cw[u])
          kv[u] <- if (grepl(";", a)) ifelse(is.na(sn) & is.na(ss), NA, paste(sn, ss, sep = ", ")) else A(a, cw[u])
        }
        u <- which(nzchar(ka) & is.na(kv))                                   # entry without its key: a new entry each time, numbered in order
        if (length(u)) kv[u] <- paste0("#", stats::ave(seq_along(u), paste(cp[u], mc[cr[u]]), FUN = seq_along))
        cre <- rules$creates[cr]; ecls[cw] <- cre; cre[q[ct]] <- "ITechnology"
        pt <- which(k[ct] == "PERIOD" & !q[ct] & startsWith(ecls[par[cw]], "TechnologyContainer<"))   # a new period's class comes from its parent element's tag
        if (length(pt)) { rr <- match(paste(ecls[par[cw[pt]]], tag[cw[pt]], sep = "\r"), rk); cre[pt] <- ifelse(is.na(rr), cre[pt], rules$creates[rr]) }
        sk <- paste(cp, mc[cr], kv, sep = "\r"); sid <- match(sk, unique(sk))
        fl <- ifelse(A("delete", cw) %in% "1", "D", ifelse(A("nocreate", cw) %in% "1", "N", "M")); fl[k[ct] == "GTD_LOCATION"] <- "M"
        isD <- fl == "D"; isM <- fl == "M"
        nD <- gcs(as.numeric(isD), sid) - isD; totD <- tabulate(sid[isD], max(sid))[sid]
        gk <- paste(sid, nD); gid <- match(gk, unique(gk))
        lv <- isM | (fl == "N" & (gcs(as.numeric(isM), gid) - isM > 0 | (pre & nD == 0 & !startsWith(kv, "#"))))
        kp0 <- paste0(I$kp[cp], "/", mc[cr], "[", kv, "]")
        if (pre) { u <- which(totD > 0 & !duplicated(sid) & tabulate(sid[isM & nD == totD], max(sid))[sid] == 0)   # deleted, not created again
                   gone$par <- c(gone$par, cp[u]); gone$kp <- c(gone$kp, kp0[u]) }
        lw <- which(lv)
        if (length(lw)) {
          g <- gid[lw]; ug <- unique(g); f1 <- lw[match(ug, g)]
          fm <- match(ug, gid[isM]); mk <- ifelse(is.na(fm), f1, which(isM)[fm])
          cls <- cre[mk]
          if (pre && length(cls0)) { u <- nD[f1] == 0 & kp0[f1] %in% names(cls0); cls[u] <- cls0[kp0[f1][u]] }
          ni <- length(I$cls) + seq_along(ug)
          I$cls <- c(I$cls, cls); I$par <- c(I$par, cp[f1]); I$kp <- c(I$kp, kp0[f1]); I$mem <- c(I$mem, mc[cr[f1]]); I$key <- c(I$key, kv[f1])
          I$dp <- c(I$dp, paste0(I$dp[cp[f1]], "/", tag[cw[f1]], ifelse(nzchar(kv[f1]), paste0("[", kv[f1], "]"), "")))
          I$fin <- c(I$fin, nD[f1] == totD[f1]); I$made <- c(I$made, !is.na(fm) & !(pre & nD[f1] == 0)); I$mk <- c(I$mk, ifelse(is.na(fm), 0L, cw[mk]))
          I$crit <- c(I$crit, crit[cr[f1]])
          ins[cw[lw]] <- ni[match(g, ug)]; live[cw[lw]] <- TRUE
          lc <- cw[lw]; li <- ins[lc]; lk <- paste0(";", ka[lw], ";")      # SIMPLE fields given as attributes
          for (a in setdiff(an, c("delete", "nocreate", "fillout"))) {
            v <- at[[a]][lc]; u <- which(!is.na(v) & !grepl(paste0(";", a, ";"), lk, fixed = TRUE) & !(a == "name" & k[ct][lw] == "PERIOD")); if (!length(u)) next   # a period's own name attribute: its container's name is read in its place
            u <- u[atr[match(paste(I$cls[li[u]], a, sep = "\r"), rk)] %in% TRUE]
            if (length(u)) put(if (a == "type-flags") "F0" else "S", li[u], a, lc[u], v[u])
          }
        }
      }
      for (z in unique(k[!ct])) {                                            # fields of the parent object
        u <- which(!ct & k == z); e <- w[u]; i <- pi[u]
        switch(z,
          SIMPLE = put("S", i, tag[e], e, ifelse(is.na(tx[e]), can(e), tx[e])),
          TF = put("F0", i, "flags", e, tx[e]),
          ARRAY = put("A", i, tag[e], e, ifelse(is.na(tx[e]), can(e), tx[e]), A("year", e), A("fillout", e) %in% "1"),
          RULE = put(ifelse(A("apply-to", e) %in% "share-weight" & A("delete", e) %in% "1", "R0", "R"), i, mc[r[u]], e, can(e, "delete")),
          KEYWORD = for (a in an) { v <- at[[a]][e]; h <- !is.na(v); if (any(h)) put("K", i[h], a, e[h], v[h]) },
          KEYMAP = { ka <- rules$key_attribute[r[u]]; for (a in unique(ka)) { h <- ka == a; put("M", i[h], paste0(mc[r[u][h]], "[", nkey(a, A(a, e[h])), "]"), e[h], can(e[h], a)) } },
          OR = put("F", i, "flags", e, tx[e]),
          REPLACE = put("P", i, ifelse(tag[e] %in% c("efficiency", "coefficient"), "coefficient", tag[e]), e, can(e)),
          SETTRUE = put("T", i, tag[e], e, "true"),
          ONCE = put("O", i, mc[r[u]], e, can(e)),
          APPEND = put(ifelse(tag[e] == "calc-base-price", "C", "L"), i, mc[r[u]], e, can(e)),
          stop("naive simulator: registry kind ", z))
      }
    }
    fin <- I$fin; for (j in seq_along(fin)[-1]) fin[j] <- fin[j] && fin[I$par[j]]
    out <- list(c(`scenario\tunknown tags skipped` = as.character(unk)))
    em <- function(i, f, v) out[[length(out) + 1L]] <<- setNames(as.character(v), paste(I$kp[i], f, sep = "\t"))
    h <- which(fin & I$made); if (length(h)) em(h, "exists as", I$cls[h])
    if (pre && length(gone$kp)) { h <- fin[gone$par]; if (any(h)) out[[length(out) + 1L]] <- setNames(rep("deleted", sum(h)), paste(gone$kp[h], "exists as", sep = "\t")) }
    W <- do.call(rbind, W)
    w <- which(live & tag == "period"); i <- I$par[ins[w]]                   # vintage names: each period read in a technology container (not a stub-technology's)
    w <- w[startsWith(I$cls[i], "TechnologyContainer<")]
    if (length(w)) {                                                        # takes the container's last name write before it (technology_container.cpp:154); none: its key (named
      i <- I$par[ins[w]]; s <- which(W$k == "S" & W$f == "name" & W$i %in% i); s <- s[order(W$i[s], W$r[s])]; j <- findInterval(i * (N + 1) + w, W$i[s] * (N + 1) + W$r[s])
      h <- j > 0L; h[h] <- W$i[s[j[h]]] == i[h]; v <- ifelse(nzchar(I$key[i]) & !startsWith(I$key[i], "#"), I$key[i], ifelse(I$made[i], "", "(before the first file)"))   # entries), "" in a
      v[h] <- W$v[s[j[h]]]                                                  # new one, a marker in one from before (pre)
      W <- rbind(W, data.frame(k = "S", i = ins[w], f = "name", r = w, v = v, y = NA_character_, fo = NA, stringsAsFactors = FALSE))   # a <name> child of the period comes later
    }
    if (!is.null(W)) {
      W <- W[fin[W$i], ]; W <- W[order(W$r), ]
      last <- function(x, key) x[!duplicated(key, fromLast = TRUE), ]
      first <- function(x, key) x[!duplicated(key), ]
      for (z in c("S", "P", "M")) { x <- W[W$k == z, ]; x <- last(x, paste(x$i, x$f)); if (nrow(x)) em(x$i, paste(c(S = "value", P = "replaced", M = "key map")[[z]], x$f), x$v) }
      for (z in c("T", "O", "K")) { x <- W[W$k == z, ]; x <- first(x, paste(x$i, x$f)); if (nrow(x)) em(x$i, paste(c(T = "set-true", O = "read once", K = "keyword")[[z]], x$f), x$v) }
      x <- W[W$k %in% c("F", "F0"), ]                                        # type-flags resets, flags add
      if (nrow(x)) for (g in split(seq_len(nrow(x)), x$i)) { y <- x[g, ]; l <- max(c(0L, which(y$k == "F0")))
        em(y$i[1], "flags", paste(c(if (l) y$v[l] else "(none)", sort(unique(y$v[seq_len(nrow(y)) > l]))), collapse = " + ")) }
      x <- W[W$k %in% c("R", "R0"), ]                                        # rules after the last clear (the clearing rule kept)
      if (nrow(x)) for (g in split(seq_len(nrow(x)), paste(x$i, x$f))) { y <- x[g, ]; l <- max(c(1L, which(y$k == "R0")))
        em(y$i[1], paste("rules", y$f[1]), paste(y$v[seq_len(nrow(y)) >= l], collapse = "\n|\n")) }
      x <- W[W$k == "L", ]
      if (nrow(x)) for (g in split(seq_len(nrow(x)), paste(x$i, x$f))) em(x$i[g[1]], paste("list", x$f[g[1]]), paste(x$v[g], collapse = "\n|\n"))
      # calc-base-price (calc_base_price.cpp): each block, in order, sets the base price of every sector with currency
      # output that it does not map, and of every from-name whose to-name bucket holds such a sector (the bucket's
      # aggregate); ignore-region only gates a log line. Every set S of sectors with currency output is tried; a value is
      # the set of sectors it aggregates (one sector: its own price).
      x <- W[W$k == "C", ]
      if (nrow(x)) for (g in split(seq_len(nrow(x)), paste(x$i, x$f))) {
        e <- x$r[g]; nu <- length(cbu)
        if (nu > 18L) { em(x$i[g[1]], paste("calc-base-price blocks", x$f[g[1]]), paste(can(e), collapse = "\n|\n")); next }   # too many sectors: the blocks as written
        S <- 0:(2L^nu - 1L); val <- matrix(-1, length(S), nu)
        for (b in e) {
          ch <- if (ed[b] > b) seq.int(b + 1L, ed[b]) else integer(0); ch <- ch[par[ch] == b & tag[ch] == "sector-map"]
          fr <- A("from-name", ch); to <- A("to-name", ch); fr[is.na(fr)] <- ""; to[is.na(to)] <- ""; u <- !duplicated(fr, fromLast = TRUE); fr <- fr[u]; to <- to[u]
          for (i in seq_len(nu)) {
            if (cbu[i] %in% fr) { m <- as.integer(sum(2^(which(cbu %in% fr[to == to[fr == cbu[i]]]) - 1))); v <- bitwAnd(S, m); h <- v > 0L }
            else { v <- rep(2^(i - 1), length(S)); h <- bitwAnd(S, as.integer(2^(i - 1))) > 0L }
            val[h, i] <- v[h]
          }
        }
        for (i in seq_len(nu)) em(x$i[g[1]], paste0("calc-base-price ", cbu[i], " (", x$f[g[1]], ")"), digest::digest(val[, i]))
      }
      x <- W[W$k == "A", ]                                                   # arrays: a write sets its year if a model year, with fillout every later one
      if (nrow(x)) for (s in seq_along(years)) {
        Y <- years[[s]]; y <- x[x$y %in% Y, ]; a <- y[!y$fo, ]; a$t <- a$y; b <- y[y$fo, ]
        if (nrow(b)) { j <- rep(seq_len(nrow(b)), each = length(Y)); b <- b[j, ]; b$t <- rep(Y, length.out = nrow(b)); b <- b[as.numeric(b$t) >= as.numeric(b$y), ] } else b$t <- character(0)
        z <- rbind(a, b); z <- z[order(z$r), ]; z <- last(z, paste(z$i, z$f, z$t))
        if (nrow(z)) em(z$i, paste0("value ", z$f, " in ", z$t, " (model years ", s, ")"), z$v)
      }
    }
    h <- which(fin & I$crit)                                                # entry order of order-critical lists
    if (length(h)) for (g in split(h, paste(I$par[h], I$mem[h]))) { o <- g[order(I$mk[g] > 0, I$mk[g], I$kp[g])]; em(I$par[g[1]], paste("order", I$mem[g[1]]), paste(I$key[o], collapse = "; ")) }
    s <- unlist(out)
    if (anyDuplicated(names(s))) stop("naive simulator: duplicated state entries: ", paste(head(names(s)[duplicated(names(s))], 3), collapse = "; "))
    attr(s, "cls") <- setNames(I$cls, I$kp)[!duplicated(I$kp)]; attr(s, "dsp") <- setNames(I$dp, I$kp)[fin]
    s
  }
  differ <- function(s1, s2) {                                              # place, field, run 1, run 2
    k <- union(names(s1), names(s2)); d <- k[is.na(s1[k]) | is.na(s2[k]) | s1[k] != s2[k]]
    p <- sub("\t.*", "", d); dp <- c(attr(s2, "dsp"), attr(s1, "dsp")); pl <- dp[p]; pl[is.na(pl)] <- p[is.na(pl)]
    data.frame(place = unname(pl), field = sub("^[^\t]*\t", "", d), run1 = unname(s1[d]), run2 = unname(s2[d]), stringsAsFactors = FALSE)
  }
  vy <- as.character(c(1975, 1990, 2005, seq(2010, 2100, 5)))
  yrs <- function(...) { y <- unique(unlist(lapply(list(...), `[[`, "@year"))); list(vy, sort(y[!is.na(y)]), as.character(seq(1975, 2100, 25))) }
  cbs <- function(...) { u <- unique(unlist(lapply(list(...), function(x) { v <- x[["@from-name"]]; if (is.null(v)) NULL else v[x$tag == "sector-map"] }))); c(sort(u[!is.na(u) & nzchar(u)]), "*") }
  naive <- function(xb, xa, xo, twice, Y = yrs(xb, xa, xo)) {               # differences of base + add-on against the override, per earlier state
    if (is.null(xb$tag)) {
      # xb: a list of earlier files' tables, then the base file's: one run each way after them, from nothing, plus what GCAM
      # does that state() leaves out: a field a new object never gets holds its constructor default (not inside a
      # stub-technology), -0 un-sets a Value field, and a rule over two adjacent model years with both values and a
      # function other than fixed sets nothing
      xp <- xb[-length(xb)]; x <- c(xb[length(xb)], list(xa, xo)); U <- do.call(cbs, c(xp, x))
      s <- list(state(c(xp, x[1:2]), FALSE, Y, cbu = U), state(c(xp, x[3]), FALSE, Y, cbu = U))
      for (j in 1:2) {
        z <- s[[j]]; a <- attributes(z); h <- sub("^[^\t]*\t", "", names(z)); w <- which(startsWith(h, "rules "))
        for (q in w) { r <- sub('^(0:interpolation-rule[^\t\n]*) apply-to="[^"]*"', "\\1", strsplit(z[q], "\n|\n", fixed = TRUE)[[1]])   # apply-to only selects the clear (subsector.cpp:128-131)
                       y1 <- sub('(?s)^0:interpolation-rule[^\t\n]* from-year="([0-9]+)".*$', "\\1", r, perl = TRUE); y2 <- sub('(?s)^0:interpolation-rule[^\t\n]* to-year="([0-9]+)".*$', "\\1", r, perl = TRUE)
                       i1 <- match(y1, Y[[1]]); nop <- grepl("\n1:from-value\t", r, fixed = TRUE) & grepl("\n1:to-value\t", r, fixed = TRUE) &
                         grepl('\n1:interpolation-function name="(linear|s-curve)"', r) & !is.na(i1) & (i1 + 1L) %in% match(y2, Y[[1]])
                       z[q] <- paste(r[!nop], collapse = "\n|\n") }
        r <- match(paste(a$cls[sub("\t.*", "", names(z))], sub("^value ([^ ]+).*$", "\\1", h), sep = "\r"), rk)
        k0 <- (startsWith(h, "rules ") & !nzchar(z)) | (startsWith(h, "value ") & rules$value_type[r] %in% "Value" & rules$default[r] %in% "unset" & grepl("^-(0+([.]0*)?|[.]0+)([eE][-+]?[0-9]+)?$", z))
        z <- z[!k0]; attr(z, "cls") <- a$cls; attr(z, "dsp") <- a$dsp; s[[j]] <- z
      }
      k2 <- union(names(s[[1]]), names(s[[2]])); h <- sub("^[^\t]*\t", "", k2); pl <- sub("\t.*", "", k2); cl <- c(attr(s[[1]], "cls"), attr(s[[2]], "cls")); ds <- c(attr(s[[1]], "dsp"), attr(s[[2]], "dsp"))
      r <- match(paste(cl[pl], ifelse(h == "flags", "type-flags", sub("^value ([^ ]+).*$", "\\1", h)), sep = "\r"), rk); vt <- rules$value_type[r]; d0 <- rules$default[r]
      dv <- ifelse(vt %in% "bool" & d0 %in% c("true", "false"), ifelse(d0 %in% "true", "1", "0"), ifelse(vt %in% c("double", "Value", "int", "unsigned int") & grepl("^-?[0-9]+([.][0-9]+)?([eE][-+]?[0-9]+)?$", d0), d0,
                   ifelse(vt %in% c("gcamstr", "std::string") & !is.na(d0) & !d0 %in% "NA" & !grepl("[[:space:]&<>\"']", d0), d0, NA)))
      dv[!(startsWith(h, "value ") | h == "flags") | grepl("/stub-technology[", ds[pl], fixed = TRUE)] <- NA
      for (j in 1:2) { z <- s[[j]]; a <- attributes(z); u <- match(names(z), k2); fl <- which(h[u] == "flags" & !is.na(dv[u]) & startsWith(z, "(none)"))
                       z[fl] <- paste0(dv[u[fl]], substring(z[fl], 7L)); m0 <- !k2 %in% names(z) & !is.na(dv)
                       z <- c(z, setNames(dv[m0], k2[m0])); attr(z, "cls") <- a$cls; attr(z, "dsp") <- a$dsp; s[[j]] <- z }
      return(list(none = differ(s[[1]], s[[2]])))
    }
    U <- cbs(xb, xa, xo)
    s2 <- state(list(xo), FALSE, Y, cbu = U); d <- list(none = differ(state(list(xb, xa), FALSE, Y, cbu = U), s2))
    s2 <- state(list(xo), TRUE, Y, cbu = U); d$created <- differ(state(list(xb, xa), TRUE, Y, attr(s2, "cls"), cbu = U), s2)
    if (twice) d$base <- differ(state(list(xb, xb, xa), FALSE, Y, cbu = U), state(list(xb, xo), FALSE, Y, cbu = U))
    d
  }
  shown <- function(d) for (k in names(d)[vapply(d, nrow, 1L) > 0]) { cat("  naive, earlier state", k, ":\n"); print(utils::head(d[[k]], 5), row.names = FALSE) }

  if (tri) {                                                                 # unit.R's triples
    addon_sim <- get("addon_sim", ns)
    k <- sort(unique(as.integer(sub("^s?b([0-9]+)\\.xml$", "\\1", list.files(xml_dir, "^s?b[0-9]+\\.xml$")))))
    for (i in k) for (p in c("", "s")) {
      f <- file.path(xml_dir, paste0(p, c("b", "a", "o"), i, ".xml")); if (!all(file.exists(f))) next
      fc <- file.path(xml_dir, paste0("c", i, ".xml")); fp <- character(0); Y <- NULL
      if (!nzchar(p) && file.exists(fc)) {                                     # written with a configuration: its earlier files (read here with xml2) before both runs
        cx <- xml2::read_xml(fc); fi <- xml2::xml_text(xml2::xml_find_all(cx, "/*/Files/*[@name='xmlInputFileName']"))
        v <- file.path(dirname(fc), c(fi[length(fi)], xml2::xml_text(xml2::xml_find_all(cx, "/*/ScenarioComponents/*")))); fp <- v[seq_len(match(basename(f[1]), basename(v)) - 1L)]
        if (!all(file.exists(fp))) { cat("SKIP unit file", basename(f[2]), "(an earlier file of", basename(fc), "is missing)\n"); next }
        mt <- xml2::xml_find_first(xml2::read_xml(fp[1]), "/*/modeltime")       # the configuration's model years: start, inter and end years with their time steps
        if (!inherits(mt, "xml_missing")) {
          e <- xml2::xml_children(mt); w <- xml2::xml_name(e) %in% c("start-year", "inter-year", "end-year")
          yr <- as.integer(xml2::xml_text(e[w])); ts <- as.integer(xml2::xml_attr(e[w], "time-step")); o <- order(yr); yr <- yr[o]; ts <- ts[o]
          Y <- list(as.character(c(unlist(lapply(seq_len(length(yr) - 1L), function(h) seq(yr[h], yr[h + 1L] - ts[h], by = ts[h]))), yr[length(yr)])))
        }
      }
      x <- tryCatch(lapply(c(fp, f), function(z) unclass(xml_tokens(z))), error = function(e) conditionMessage(e))
      if (is.character(x)) { cat("SKIP unit file", basename(f[1]), "(not readable whole:", substr(x, 1, 120), ")
"); next }   # e.g. a block the add-on never reads
      if (length(fp)) { xp <- x[seq_along(fp)]; x <- x[-seq_along(fp)]; d <- naive(c(xp, x[1]), x[[2]], x[[3]], FALSE, if (is.null(Y)) do.call(yrs, c(xp, x)) else Y) }   # after the earlier files, from nothing
      else d <- naive(x[[1]], x[[2]], x[[3]], TRUE)
      nd <- sum(vapply(d, nrow, 1L))
      if (!nzchar(p)) check(sprintf("unit add-on %d (%s): naive simulator agrees%s", i, basename(f[2]), if (length(fp)) sprintf(" (after the %d earlier %s of %s)", length(fp), if (length(fp) == 1L) "file" else "files", basename(fc)) else ""), nd == 0L)
      else { v <- isTRUE(addon_sim(f[1], f[2], f[3])$ok)
             check(sprintf("unit check %d (%s): addon_sim %s, naive simulator %s", i, basename(f[2]), if (v) "passes" else "differs", if (nd) "differs" else "agrees"), v == (nd == 0L)) }
      if (nd) shown(d)
    }
    return(invisible())
  }

  # ---- edits -------------------------------------------------------------------------------------------------------
  # each: expected verdict ("written", "refused" or "either"); for refusals, whether the message must say config= can
  # decide it ("step2"; with config=, a reason from the earlier files), must not mention config= ("never") or either ("");
  # and a function returning the bytes to replace (a..z, z = a - 1 inserts before a) and the new text, or NULL without a
  # candidate
  pick <- function(v, n = 1L) if (length(v) <= n) v else v[sort(sample.int(length(v), n))]
  num <- "^-?[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?$"
  x1 <- function(v) format(as.numeric(v) * 1.1, digits = 10)
  sline <- function(F, i) F$x$first[i] + nchar(F$x$stag[i], "bytes") - 1L    # last byte of the start tag
  ins1 <- function(F, i, t) list(a = sline(F, i) + 1L, z = sline(F, i), new = paste0("\n", strrep(" ", 4L * F$x$depth[i]), t))   # a new first child line
  kav <- function(F, i) vapply(i, function(j) { v <- F$x[[paste0("@", F$x$ka[j])]]; if (is.null(v)) NA_character_ else v[j] }, "")   # key-map key of each element
  newkey <- function(k, sib) { n <- if (grepl("^-?[0-9]+$", k)) as.character(as.integer(k) + 1L) else if (grepl(num, k)) format(as.numeric(k) + 0.5, digits = 10) else paste0(k, " 2")
                               if (n %in% sib) NULL else n }
  edits <- list(
    "value x1.1 (1)" = list("written", "", function(F) { i <- pick(F$nl); if (length(i)) list(a = F$ta[i], z = F$tz[i], new = x1(F$x$text[i])) }),
    "value x1.1 (25)" = list("written", "", function(F) { i <- pick(F$nl, 25L); if (length(i)) list(a = F$ta[i], z = F$tz[i], new = x1(F$x$text[i])) }),
    "value x1.1 (one tag, up to 2000)" = list("either", "", function(F) { t <- pick(unique(F$x$tag[F$nl])); i <- pick(F$nl[F$x$tag[F$nl] == t], 2000L); if (length(i)) list(a = F$ta[i], z = F$tz[i], new = x1(F$x$text[i])) }),
    "row drop" = list("refused", "step2", function(F) { i <- pick(F$lv[F$x$depth[F$lv] >= 4L & !F$x$tag[F$lv] %in% c("PrimaryFuelCO2Coef", "sector-map", "ignore-region", "type-flags")]); if (length(i)) list(a = F$ls[i], z = F$le[i], new = "") }),
    "row add (new year)" = list("written", "", function(F) {
      i <- pick(F$nl[!is.na(F$x[["@year"]][F$nl]) & F$ok[F$nl]]); if (!length(i)) return(NULL)
      y <- F$x[["@year"]][i]; l <- substring(F$s, F$ls[i], F$le[i])
      l <- sub(paste0('year="', y, '"'), paste0('year="', as.integer(y) + 3L, '"'), l, fixed = TRUE); l <- sub(paste0(">", F$x$text[i], "<"), paste0(">", x1(F$x$text[i]), "<"), l, fixed = TRUE)
      list(a = F$le[i] + 1L, z = F$le[i], new = l) }),
    "container drop" = list("refused", "step2", function(F) { i <- pick(F$co[F$x$depth[F$co] >= 5L]); if (length(i)) list(a = F$ls[i], z = F$le[i], new = "") }),
    "container add (copy, new name)" = list("either", "", function(F) {
      i <- pick(F$co[F$x$depth[F$co] >= 4L & !is.na(F$x[["@name"]][F$co])]); if (!length(i)) return(NULL)
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0(' name="', F$x[["@name"]][i], '"'), paste0(' name="', F$x[["@name"]][i], ' 2"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)) }),
    "container rename" = list("refused", "step2", function(F) {
      i <- pick(F$co[F$x$depth[F$co] >= 4L & !is.na(F$x[["@name"]][F$co]) & !F$x$kd[F$co] %in% "SINGLE"]); if (!length(i)) return(NULL)   # a single object's name is a field
      list(a = F$x$first[i], z = sline(F, i), new = sub(' name="', ' name="2 ', F$x$stag[i], fixed = TRUE)) }),
    "technology rename (a subresource's one technology)" = list("refused", "never", function(F) {   # each period read takes its name (technology_container.cpp:154)
      i <- pick(F$co[F$x$kd[F$co] %in% "SINGLE" & startsWith(F$x$cls[F$co], "TechnologyContainer<") %in% TRUE & !is.na(F$x[["@name"]][F$co])]); if (!length(i)) return(NULL)
      list(a = F$x$first[i], z = sline(F, i), new = sub(' name="', ' name="2 ', F$x$stag[i], fixed = TRUE)) }),
    "logit swap" = list("refused", "step2", function(F) {
      i <- pick(which(F$x$tag %in% c("relative-cost-logit", "absolute-cost-logit") & !is.na(F$x$close))); if (!length(i)) return(NULL)
      t <- F$x$tag[i]; u <- if (t == "relative-cost-logit") "absolute-cost-logit" else "relative-cost-logit"
      list(a = c(F$x$first[i] + 1L, F$x$close[i] + 2L), z = c(F$x$first[i], F$x$close[i] + 1L) + nchar(t), new = c(u, u)) }),
    "rule to-year change" = list("refused", "step2", function(F) {
      i <- pick(which(F$x$tag == "interpolation-rule" & !is.na(F$x[["@to-year"]]))); if (!length(i)) return(NULL)
      y <- F$x[["@to-year"]][i]; list(a = F$x$first[i], z = sline(F, i), new = sub(paste0('to-year="', y, '"'), paste0('to-year="', as.integer(y) - 5L, '"'), F$x$stag[i], fixed = TRUE)) }),
    "rule append" = list("written", "", function(F) {
      i <- pick(F$lr); if (!length(i)) return(NULL); y <- F$x[["@to-year"]][i]
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0('to-year="', y, '"'), paste0('to-year="', as.integer(y) - 5L, '"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)) }),
    "rule clear added (the override's own)" = list("written", "", function(F) {
      i <- pick(F$lr[F$x[["@apply-to"]][F$lr] %in% "share-weight"]); if (!length(i)) return(NULL); y <- F$x[["@to-year"]][i]
      l <- sub(paste0('to-year="', y, '"'), paste0('to-year="', as.integer(y) - 5L, '"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)
      list(a = F$le[i] + 1L, z = F$le[i], new = sub("<interpolation-rule ", '<interpolation-rule delete="1" ', l, fixed = TRUE)) }),
    "rule list emptied" = list("refused", "step2", function(F) {
      i <- pick(F$lr); if (!length(i)) return(NULL); s <- which(F$x$parent == F$x$parent[i] & F$x$tag == "interpolation-rule")
      if (!all(F$ok[s])) return(NULL); list(a = F$ls[s], z = F$le[s], new = rep("", length(s))) }),
    "gas drop" = list("refused", "step2", function(F) { i <- pick(which(F$x$tag %in% c("CO2", "Non-CO2") & F$ok)); if (length(i)) list(a = F$ls[i], z = F$le[i], new = "") }),
    "gas add (copy, new name)" = list("written", "", function(F) {
      i <- pick(which(F$x$tag == "Non-CO2" & F$ok)); if (!length(i)) return(NULL)
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0(' name="', F$x[["@name"]][i], '"'), paste0(' name="', F$x[["@name"]][i], '_2"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)) }),
    "grade insert (between grades)" = list("refused", "step2", function(F) {
      g <- which(F$x$tag == "grade" & F$ok); i <- pick(g[duplicated(F$x$parent[g], fromLast = TRUE)]); if (!length(i)) return(NULL)   # a grade with a later sibling grade
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0(' name="', F$x[["@name"]][i], '"'), paste0(' name="', F$x[["@name"]][i], ' b"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE), grade = i) }),
    "grade append (after the last grade)" = list("written", "", function(F) {
      g <- which(F$x$tag == "grade" & F$ok); g <- g[!duplicated(F$x$parent[g], fromLast = TRUE)]; i <- pick(g); if (!length(i)) return(NULL)
      l <- sub(paste0(' name="', F$x[["@name"]][i], '"'), paste0(' name="', F$x[["@name"]][i], ' b"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)
      l <- sub("<extractioncost>([^<]*)</extractioncost>", "<extractioncost>\\1\\1</extractioncost>", l)            # larger cost: digits repeated
      list(a = F$le[i] + 1L, z = F$le[i], new = l) }),
    "keyword key added" = list("written", "", function(F) {
      i <- pick(which(F$x$tag == "keyword")); if (!length(i)) return(NULL)
      k <- setdiff(F$kwk, strsplit(F$x$anm[i], " +")[[1]]); if (!length(k)) return(NULL)
      list(a = F$x$first[i], z = sline(F, i), new = sub("/>$", paste0(" ", k[1], '="x"/>'), F$x$stag[i])) }),
    "keyword value changed" = list("refused", "step2", function(F) {
      i <- pick(which(F$x$tag == "keyword")); if (!length(i)) return(NULL)
      list(a = F$x$first[i], z = sline(F, i), new = sub('="([^"]*)"', '="\\1 2"', F$x$stag[i])) }),
    "nocreate added" = list("either", "step2", function(F) {
      i <- pick(F$co[F$x$depth[F$co] >= 4L & is.na(F$x[["@nocreate"]][F$co]) & is.na(F$x[["@delete"]][F$co])]); if (!length(i)) return(NULL)
      list(a = F$x$first[i], z = sline(F, i), new = sub(">$", ' nocreate="1">', F$x$stag[i])) }),
    "delete and rebuild (the override's own)" = list("either", "", function(F) {
      i <- pick(F$co[F$x$depth[F$co] >= 4L & !is.na(F$x[["@name"]][F$co]) & is.na(F$x[["@delete"]][F$co])]); if (!length(i)) return(NULL)
      list(a = F$ls[i], z = F$ls[i] - 1L, new = paste0(substring(F$s, F$ls[i], F$x$first[i] - 1L), "<", F$x$tag[i], ' name="', F$x[["@name"]][i], '" delete="1"/>\n')) }),
    "fillout added" = list("written", "", function(F) {
      i <- pick(F$nl[!is.na(F$x[["@year"]][F$nl]) & is.na(F$x[["@fillout"]][F$nl]) & F$x$kd[F$nl] %in% "ARRAY"]); if (!length(i)) return(NULL)
      list(a = F$x$first[i], z = sline(F, i), new = sub(">$", ' fillout="1">', F$x$stag[i])) }),
    "base delete removed in the override" = list("refused", "step2", function(F) {
      i <- pick(which(F$x[["@delete"]] %in% "1" & F$ok)); if (length(i)) list(a = F$ls[i], z = F$le[i], new = "") }),
    "key-map value changed" = list("written", "", function(F) { i <- pick(F$nl[F$x$kd[F$nl] %in% "KEYMAP"]); if (length(i)) list(a = F$ta[i], z = F$tz[i], new = x1(F$x$text[i])) }),
    "key-map key added" = list("written", "", function(F) {
      i <- pick(F$lv[F$x$kd[F$lv] %in% "KEYMAP" & F$x$tag[F$lv] != "sector-map"]); if (!length(i)) return(NULL); ka <- F$x$ka[i]; k <- kav(F, i); if (is.na(k)) return(NULL)
      sib <- which(F$x$parent == F$x$parent[i] & F$x$tag == F$x$tag[i]); nk <- newkey(k, kav(F, sib)); if (is.null(nk)) return(NULL)
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0(ka, '="', k, '"'), paste0(ka, '="', nk, '"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)) }),
    "key-map key removed" = list("refused", "step2", function(F) {
      c0 <- F$lv[F$x$kd[F$lv] %in% "KEYMAP" & !F$x$tag[F$lv] %in% c("sector-map", "PrimaryFuelCO2Coef")]
      ky <- paste(F$x$parent[c0], F$x$tag[c0], kav(F, c0))
      i <- pick(c0[!duplicated(ky) & !duplicated(ky, fromLast = TRUE)]); if (length(i)) list(a = F$ls[i], z = F$le[i], new = "") }),
    "input flag added" = list("written", "", function(F) {
      i <- pick(which(F$x$cls == "EnergyInput" & is.na(F$x$text) & F$ok & !endsWith(F$x$stag, "/>"))); if (length(i)) ins1(F, i, "<flag>BackupEnergy</flag>") }),
    "set-true added" = list("written", "", function(F) {
      i <- pick(which(F$x$cls == "MACControl" & is.na(F$x$text) & F$ok & !endsWith(F$x$stag, "/>"))); if (length(i)) ins1(F, i, "<no-zero-cost-reductions/>") }),
    "coefficient to efficiency" = list("written", "", function(F) {
      i <- pick(F$nl[F$x$tag[F$nl] == "coefficient" & F$x$kd[F$nl] %in% "REPLACE"]); if (!length(i)) return(NULL)
      list(a = F$x$first[i], z = F$x$last[i], new = paste0("<efficiency>", format(1 / as.numeric(F$x$text[i]), digits = 10), "</efficiency>")) }),
    "attribute value changed (a field set on a container)" = list("written", "", function(F) {
      c0 <- F$co[F$x$kd[F$co] %in% c("SINGLE", "VEC_NAMED", "VEC_YEAR", "PERIOD")]
      a <- lapply(c0, function(i) { n <- setdiff(strsplit(F$x$anm[i], " +")[[1]], c("", "delete", "nocreate", "fillout", strsplit(F$x$ka[i], ";")[[1]]))
                                    n[kind[match(paste(F$x$cls[i], n, sep = "\r"), rk)] %in% "SIMPLE" & !(n == "name" & startsWith(F$x$cls[i], "TechnologyContainer<"))] })   # not a technology's name: a rename, refused
      j <- pick(which(lengths(a) > 0L)); if (!length(j)) return(NULL); i <- c0[j]; n <- a[[j]][1]; v <- F$x[[paste0("@", n)]][i]
      list(a = F$x$first[i], z = sline(F, i), new = sub(paste0(n, '="', v, '"'), paste0(n, '="', if (grepl(num, v)) x1(v) else paste0(v, "2"), '"'), F$x$stag[i], fixed = TRUE)) }),
    "stub keyword key added" = list("written", "", function(F) {
      i <- pick(which(F$x$tag == "keyword" & F$stub)); if (!length(i)) return(NULL)
      k <- setdiff(F$kwk, strsplit(F$x$anm[i], " +")[[1]]); if (!length(k)) return(NULL)
      list(a = F$x$first[i], z = sline(F, i), new = sub("/>$", paste0(" ", k[1], '="x"/>'), F$x$stag[i])) }),
    "stub period added" = list("written", "", function(F) {
      p <- which(F$x$tag == "period" & F$x$tag[pmax(F$x$parent, 1L)] == "stub-technology" & F$ok & !is.na(F$x[["@year"]]))
      p <- p[!duplicated(F$x$parent[p], fromLast = TRUE)]; i <- pick(p); if (!length(i)) return(NULL); y <- F$x[["@year"]][i]
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0('year="', y, '"'), paste0('year="', as.integer(y) + 1L, '"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)) }),
    "stub rule append" = list("written", "", function(F) {
      i <- pick(F$lr[F$stub[F$lr]]); if (!length(i)) return(NULL); y <- F$x[["@to-year"]][i]
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0('to-year="', y, '"'), paste0('to-year="', as.integer(y) - 5L, '"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)) }),
    "stub period delete and rebuild (the override's own)" = list("written", "", function(F) {
      i <- pick(which(F$x$tag == "period" & F$x$tag[pmax(F$x$parent, 1L)] == "stub-technology" & F$ok & !is.na(F$x[["@year"]]) & is.na(F$x[["@delete"]]))); if (!length(i)) return(NULL)
      list(a = F$ls[i], z = F$ls[i] - 1L, new = paste0(substring(F$s, F$ls[i], F$x$first[i] - 1L), '<period year="', F$x[["@year"]][i], '" delete="1"/>\n')) }),
    "calc-base-price pair added" = list("written", "", function(F) {
      i <- pick(which(F$x$tag == "sector-map" & F$ok)); if (!length(i)) return(NULL); fr <- F$x[["@from-name"]][i]
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0('from-name="', fr, '"'), paste0('from-name="', fr, '_2"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)) }),
    "calc-base-price to-name changed" = list("refused", "never", function(F) {
      i <- pick(which(F$x$tag == "sector-map" & !is.na(F$x[["@to-name"]]))); if (!length(i)) return(NULL); to <- F$x[["@to-name"]][i]
      list(a = F$x$first[i], z = sline(F, i), new = sub(paste0('to-name="', to, '"'), paste0('to-name="', to, '_2"'), F$x$stag[i], fixed = TRUE)) }),
    "new region (copy)" = list("refused", "never", function(F) {
      if (nchar(F$s, "bytes") > 20 * 2^20) return(NULL); i <- pick(which(F$x$tag == "region" & F$x$depth == 3L & F$ok)); if (!length(i)) return(NULL)
      list(a = F$le[i] + 1L, z = F$le[i], new = sub(paste0(' name="', F$x[["@name"]][i], '"'), paste0(' name="', F$x[["@name"]][i], ' 2"'), substring(F$s, F$ls[i], F$le[i]), fixed = TRUE)) }),
    "region removed" = list("refused", "never", function(F) {
      i <- pick(which(F$x$tag == "region" & F$x$depth == 3L & F$ok & is.na(F$x[["@nocreate"]]))); if (length(i)) list(a = F$ls[i], z = F$le[i], new = "") }),
    "value un-set" = list("refused", "step2", function(F) {                 # a Value field with no default (unset), dropped where no year is set (listed last: the edits above draw the same candidates as in step 1)
      r <- match(paste(F$x$cls[pmax(F$x$parent[F$lv], 1L)], F$x$tag[F$lv], sep = "\r"), rk)
      i <- pick(F$lv[rules$value_type[r] %in% "Value" & rules$default[r] %in% "unset" & F$x$depth[F$lv] >= 4L & !F$stub[F$lv] & is.na(F$x[["@year"]][F$lv])]); if (length(i)) list(a = F$ls[i], z = F$le[i], new = "") })
  )
  big <- c("value x1.1 (1)", "value x1.1 (25)", "row drop", "container drop", "container add (copy, new name)", "gas drop", "gas add (copy, new name)", "nocreate added",
           "delete and rebuild (the override's own)", "key-map value changed", "region removed")
  ce <- c("value x1.1 (1)", "row drop", "logit swap", "rule to-year change", "keyword value changed", "gas drop", "grade insert (between grades)", "value un-set")   # config mode
  # path of each element (tags with name, year or sector-name/subsector-name), for matching base and add-on elements
  paths <- function(x) { v <- function(a) { z <- x[[a]]; if (is.null(z)) rep(NA_character_, length(x$tag)) else z }
                         l <- paste0(x$tag, "[", ifelse(is.na(v("@name")), ifelse(is.na(v("@year")), "", v("@year")), v("@name")), "]"); p <- l
                         for (d in seq_len(max(x$depth))[-1]) { w <- which(x$depth == d); p[w] <- paste0(p[x$parent[w]], "/", l[w]) }; p }

  res <- data.frame(file = character(), edit = character(), expected = character(), verdict = character(), detail = character(), KB = numeric(), pct = numeric(), cpu_s = numeric(), stringsAsFactors = FALSE)
  set.seed(seed)
  for (fn in files) {
    if (is.null(config)) f <- file.path(xml_dir, fn) else {                 # the base file the configuration loads, its earlier files and model years
      cc <- get("config_components", ns)(config, fn); f <- cc$path[cc$b]; pf <- cc$path[seq_len(cc$b - 1L)]; Yc <- if (length(cc$years)) list(as.character(cc$years))
      pix <- list(); t_run <- proc.time()[["elapsed"]]
    }
    if (!file.exists(f)) { cat("SKIP", fn, "(not found)\n"); next }
    mb <- file.size(f) / 2^20
    if (mb > 50 && !mem_ok()) { cat("SKIP", fn, "(less than 4 GB free)\n"); next }
    b <- readBin(f, "raw", file.size(f)); s <- rawToChar(b); Encoding(s) <- "bytes"; rm(b)
    x <- unclass(xml_tokens(f)); n <- nchar(s, "bytes"); an <- grep("^@", names(x), value = TRUE)
    xb <- x[c("tag", "depth", "parent", "first", "last", "close", "stag", "text", an)]
    for (a in paste0("@", c("name", "year", "fillout", "nocreate", "delete", "apply-to", "from-year", "to-year", "from-name", "to-name"))) if (is.null(x[[a]])) x[[a]] <- rep(NA_character_, length(x$tag))
    nlp <- as.integer(gregexpr("\n", s, perl = TRUE, useBytes = TRUE)[[1]])
    j <- findInterval(x$first - 1L, nlp); ls <- ifelse(j > 0L, nlp[pmax(j, 1L)] + 1L, 1L)
    le <- nlp[findInterval(x$last, nlp) + 1L]; le[is.na(le)] <- n
    ok <- !grepl("[^ \t]", substring(s, ls, x$first - 1L)) & !grepl("[^ \t\r\n]", substring(s, x$last + 1L, le))   # alone on its lines
    x$anm <- do.call(paste, c(lapply(an, function(a) ifelse(is.na(x[[a]]), "", substring(a, 2))), sep = " "))
    cl <- rep("", length(x$tag)); kd <- ka <- rep(NA_character_, length(x$tag)); sb <- uk <- logical(length(x$tag))   # registry class, kind and key; inside a stub-technology; inside an entry without its key
    for (d in seq_len(max(x$depth))) { w <- which(x$depth == d); ri <- match(paste(if (d == 1L) "" else cl[x$parent[w]], x$tag[w], sep = "\r"), rk)
                                       cl[w] <- ifelse(is.na(ri), "", rules$creates[ri]); kd[w] <- kind[ri]; ka[w] <- rules$key_attribute[ri]
                                       if (d > 1L) sb[w] <- sb[x$parent[w]] | x$tag[x$parent[w]] == "stub-technology"
                                       uk[w] <- (if (d > 1L) uk[x$parent[w]] else FALSE) | (kd[w] %in% c("VEC_NAMED", "VEC_YEAR") & is.na(ifelse(ka[w] %in% "year", x[["@year"]][w], x[["@name"]][w]))) }
    x$cls <- cl; x$kd <- kd; x$ka <- ka
    kwk <- unique(unlist(strsplit(x$anm[x$tag == "keyword"], " ", fixed = TRUE))); kwk <- kwk[nzchar(kwk)]
    lf <- which(!is.na(x$text) & ok)
    nlv <- lf[grepl(num, x$text[lf]) & x$text[lf] != "0" & !x$tag[lf] %in% c("initial-available-year", "final-available-year") & x$tag[x$parent[lf]] != "interpolation-rule" & !uk[lf]]   # not inside an entry without its key
    rl <- which(x$tag == "interpolation-rule" & ok & !is.na(x[["@to-year"]]) & !is.na(x[["@from-year"]]))
    rl <- rl[vapply(rl, function(i) { sib <- which(x$parent == x$parent[i] & x$tag == "interpolation-rule"); i == max(sib) }, NA) & as.integer(x[["@to-year"]][rl]) - 5L > as.integer(x[["@from-year"]][rl])]
    F <- list(x = x, s = s, ls = ls, le = le, ok = ok, nl = nlv, lv = lf, co = which(is.na(x$text) & ok), lr = rl, kwk = kwk, stub = sb,
              ta = x$first + nchar(x$stag, "bytes"), tz = x$close - 1L)
    nc <- any(x[["@nocreate"]] %in% "1") && !grepl("_MAC[.]xml$", fn)       # files written with nocreate: a written edit may also be refused (nocreate pattern); not *_MAC.xml
    pb <- if (mb < 20) paths(xb) else NULL
    first_ok <- 0L
    for (ek in if (!is.null(config)) ce else if (mb > 50) big else names(edits)) for (ri in if (!is.null(config) || mb > 10) 1L else 1:2) {
      e <- edits[[ek]][[3]](F); if (is.null(e)) { if (ri == 1L) cat("SKIP", fn, ek, "(no candidate)\n"); next }
      o <- order(e$a); kp <- substring(s, c(1L, e$z[o] + 1L), c(e$a[o] - 1L, n))
      so <- paste(c(rbind(kp[-length(kp)], e$new[o]), kp[length(kp)]), collapse = "")
      tag <- paste0(sub("\\.xml$", "", fn), "_", substr(gsub("[^a-z0-9]+", "_", ek), 1, 30), "_", ri)
      fo <- file.path(out_dir, paste0(tag, ".xml")); fa <- file.path(out_dir, paste0(tag, "_addon.xml")); writeBin(charToRaw(so), fo); rm(so)
      op <- options(minaj.addon_max_ratio = Inf); t0 <- proc.time()
      r <- tryCatch(addon_write(f, fo, fa, config = config), error = function(e) conditionMessage(e)); cpu <- (proc.time() - t0)[[1]]; options(op)
      if (is.character(r) && grepl(lowmem, r)) { cat(sprintf("SKIP %s %s #%d: %s\n", fn, ek, ri, substr(gsub("\n", " ", r), 1, 200))); n_skip <<- n_skip + 1L; unlink(c(fo, fa)); next }
      ex <- edits[[ek]][[1]]; cf <- edits[[ek]][[2]]; if (ex == "written" && nc) ex <- "either"; if (!is.null(config) && cf == "step2") ex <- "either"
      noblk <- FALSE; gb <- NA_real_                                        # noblk: the earlier files' blocks not read (memory), naive checks after them skipped
      if (!is.null(config) && (!is.character(r) || (!is.null(e$grade) && mb < 20))) {   # the earlier files' blocks of the parts the edit changes, read here by byte offsets
        so <- readBin(fo, "raw", file.size(fo)); so <- rawToChar(so); Encoding(so) <- "bytes"; xo <- unclass(xml_tokens(fo)); if (is.null(xo[["@name"]])) xo[["@name"]] <- rep(NA_character_, length(xo$tag))
        w <- which(x$depth == 3L); kb <- paste(x$tag[w], x[["@name"]][w]); tb <- substring(s, x$first[w], x$last[w])
        w <- which(xo$depth == 3L); ko <- paste(xo$tag[w], xo[["@name"]][w]); to <- substring(so, xo$first[w], xo$last[w]); rm(so)
        K <- unique(c(kb[!paste(kb, tb) %in% paste(ko, to)], ko[!paste(ko, to) %in% paste(kb, tb)])); rm(tb, to); xp <- list()
        for (p in pf[file.exists(pf)]) if (is.null(pix[[p]])) {            # spans of the elements under <world>: tag lines indented 8 spaces, in 64 MB chunks carrying 64 KB
          con <- file(p, "rb"); o <- 0; cr <- raw(0); a <- numeric(0); k <- character(0); we <- NA_real_; hw <- FALSE
          repeat {
            b <- readBin(con, "raw", 2^26); if (!length(b)) break
            bb <- c(cr, b); b0 <- o - length(cr); o <- o + length(b); hw <- hw || length(grepRaw("<world", bb, fixed = TRUE)) > 0L
            for (i in grepRaw("\n        <[A-Za-z_]", bb, all = TRUE)) {
              if ((b0 + i + 9) %in% a) next
              t <- rawToChar(bb[(i + 9L):min(length(bb), i + 520L)]); if (!grepl(">", t, fixed = TRUE)) next
              st <- sub(">.*$", ">", t); nv <- regmatches(st, regexpr(' name="[^"]*"', st))
              a <- c(a, b0 + i + 9); k <- c(k, paste(sub("^<([^ \t\r\n/>]+).*$", "\\1", st), if (length(nv)) substr(nv, 8L, nchar(nv) - 1L) else NA))
            }
            wz <- grepRaw("\n    </world>", bb); if (length(wz)) we <- b0 + wz
            cr <- bb[max(1L, length(bb) - 65535L):length(bb)]
          }
          close(con)
          pix[[p]] <- if (!is.na(we)) data.frame(k = k, a = a, e = c(a[-1] - 10, we - 1)) else if (!hw) data.frame(k = character(0), a = numeric(0), e = numeric(0)) else {
            z <- unclass(xml_tokens(p)); w <- which(z$depth == 3L)                # not laid out as gcamdata writes it: whole-file tokens
            data.frame(k = paste(z$tag[w], if (is.null(z[["@name"]])) NA else z[["@name"]][w]), a = z$first[w], e = z$last[w])
          }
        }
        gb <- sum(vapply(pf[file.exists(pf)], function(p) { j <- pix[[p]]$k %in% K; sum(pix[[p]]$e[j] - pix[[p]]$a[j] + 1) }, 0)) / 2^30
        if (0.2 + 31 * gb > ps::ps_system_memory()$avail / 2^30 - 4) {         # two simulator runs over the blocks' tokens
          cat(sprintf("SKIP %s %s: naive simulator after the earlier files (their blocks of the changed parts, %.2f GB, need about %.1f GB more than 4 GB free)\n", fn, ek, gb, 0.2 + 31 * gb))
          noblk <- TRUE; n_skip <<- n_skip + 1L
        }
        m0 <- sum(gc(reset = TRUE)[, 2]) / 1024
        if (!noblk) for (p in pf[file.exists(pf)]) {
          j <- which(pix[[p]]$k %in% K); if (!length(j)) next
          con <- file(p, "rb"); bl <- lapply(j, function(i) { seek(con, pix[[p]]$a[i] - 1); c(readBin(con, "raw", pix[[p]]$e[i] - pix[[p]]$a[i] + 1), as.raw(10L)) }); close(con)
          xp[[length(xp) + 1L]] <- unclass(xml_tokens(c(charToRaw("<scenario><world>\n"), unlist(bl), charToRaw("</world></scenario>")))); rm(bl)
        }
      }
      if (is.character(r)) {
        int <- grepl("internal error", r, fixed = TRUE)
        why <- if (int) r else sub("^.*?\n  - ", "", r); why <- substr(gsub("\n", " ", why), 1, if (int) 2000 else 220)
        okr <- !int && ex != "written" && grepl("scenario/", r, fixed = TRUE) && (cf != "step2" || if (is.null(config)) grepl("config= can decide it from the configuration's earlier files", r, fixed = TRUE) else
          grepl("earlier file|state before the base file is not known|minaj[.]addon_neg0|no default it can restore|model years are not known", r)) &&   # with config: a reason from the earlier files
          (cf != "never" || !grepl("config=", r, fixed = TRUE)) && (is.null(config) || !grepl("config= can decide it", r, fixed = TRUE)) && !(grepl("_MAC[.]xml$", fn) && grepl("nocreate pattern", r, fixed = TRUE))
        res[nrow(res) + 1L, ] <- list(fn, ek, ex, if (int) "INTERNAL ERROR" else "refused", why, NA, NA, round(cpu, 1))
        check(sprintf("%s | %s #%d: expected %s%s, %s", fn, ek, ri, ex, if (nzchar(cf)) paste0(" (", cf, ")") else "", if (int) why else paste("refused:", substr(why, 1, 160))), okr)
        if (!is.null(e$grade) && !int && mb < 20) {                          # negative control: the inserted grade appended (out of order)
          g <- e$grade; an2 <- integer(0); q <- F$x$parent[g]; while (q > 0L) { an2 <- c(q, an2); q <- F$x$parent[q] }
          an2 <- an2[F$x$depth[an2] >= 2L]
          op2 <- vapply(an2, function(i) if (F$x$tag[i] == "world") "<world>" else if (grepl('nocreate="1"', F$x$stag[i], fixed = TRUE)) F$x$stag[i] else sub(">$", ' nocreate="1">', F$x$stag[i]), "")
          wa <- c("<scenario>", op2, sub(paste0(' name="', F$x[["@name"]][g], '"'), paste0(' name="', F$x[["@name"]][g], ' b"'), substring(s, F$x$first[g], F$x$last[g]), fixed = TRUE),
                  paste0("</", rev(F$x$tag[an2]), ">"), "</scenario>")
          writeBin(charToRaw(paste(wa, collapse = "\n")), fa)
          if (!noblk) { dn <- if (is.null(config)) naive(xb, unclass(xml_tokens(fa)), unclass(xml_tokens(fo)), FALSE) else naive(c(xp, list(xb)), unclass(xml_tokens(fa)), xo, FALSE, if (is.null(Yc)) yrs(xb, xo) else Yc)
                     check(sprintf("%s | negative control: inserted grade appended instead: naive simulator differs (%d places)", fn, sum(vapply(dn, nrow, 1L))), any(vapply(dn, nrow, 1L) > 0L)) }
          v <- tryCatch(addon_sim(f, fa, fo, config), error = function(e) conditionMessage(e))
          if (is.character(v) && grepl(lowmem, v)) { cat(sprintf("SKIP %s | negative control: inserted grade appended instead: addon_sim: %s\n", fn, substr(v, 1, 200))); n_skip <<- n_skip + 1L } else check(sprintf("%s | negative control: inserted grade appended instead: addon_sim finds the entry order%s", fn, if (is.character(v)) paste(":", substr(v, 1, 200)) else ""),
                is.list(v) && !isTRUE(v$ok) && any(startsWith(v$mismatches$column, "entry order") & endsWith(v$mismatches$target, "/grade")))
        }
      } else {
        xa <- unclass(xml_tokens(fa)); xo <- unclass(xml_tokens(fo))
        Y <- yrs(xb, xa, xo); U <- cbs(xb, xa, xo); Yn <- if (is.null(config) || is.null(Yc)) Y else Yc
        d <- if (is.null(config) || is.null(r$prefix_md5)) naive(xb, xa, xo, mb < 20) else list()   # with config: after the earlier files' blocks as well
        if (!is.null(config) && !noblk) d[[basename(config)]] <- naive(c(xp, list(xb)), xa, xo, FALSE, Yn)$none
        nd <- vapply(d, nrow, 1L)
        blind <- !r$empty && mb < 50 && nrow(differ(state(list(xb), TRUE, Y, cbu = U), state(list(xo), TRUE, Y, cbu = U))) == 0L &&
          nrow(differ(state(list(xb), FALSE, Y, cbu = U), state(list(xo), FALSE, Y, cbu = U))) == 0L
        det <- paste0(paste(names(r$n)[r$n > 0], r$n[r$n > 0], collapse = " "), if (r$empty) "empty add-on", if (blind) "; naive simulator sees no change", if (length(r$moves)) paste0("; order moves ", length(r$moves)),
                      if (noblk) "; naive simulator after the earlier files skipped for memory")
        res[nrow(res) + 1L, ] <- list(fn, ek, ex, if (any(nd > 0)) "NAIVE DIFFERS" else "written", det, round(r$size / 1024, 1), round(100 * r$size / file.size(fo), 2), round(cpu, 1))
        if (length(nd)) check(sprintf("%s | %s #%d: written (%s), naive simulator agrees (%s)", fn, ek, ri, det, paste(names(nd), collapse = ", ")), all(nd == 0L))
        else cat(sprintf("SKIP %s | %s #%d: written (%s)\n", fn, ek, ri, det))
        shown(d)
        sa <- readBin(fa, "raw", file.size(fa)); sa <- rawToChar(sa); Encoding(sa) <- "bytes"
        neg <- function(what, txt) {                                         # a wrong add-on made on purpose must differ, under both simulators
          fn2 <- file.path(out_dir, paste0(tag, "_neg.xml")); writeBin(charToRaw(txt), fn2)
          dn <- if (is.null(config)) naive(xb, unclass(xml_tokens(fn2)), xo, mb < 20) else if (!noblk) list(naive(c(xp, list(xb)), unclass(xml_tokens(fn2)), xo, FALSE, Yn)$none)
          v <- tryCatch(addon_sim(f, fn2, fo, config), error = function(e) conditionMessage(e)); unlink(fn2)
          if (!is.null(dn)) check(sprintf("%s | negative control: %s add-on with %s: naive simulator differs (%d places)", fn, ek, what, sum(vapply(dn, nrow, 1L))), any(vapply(dn, nrow, 1L) > 0L))
          if (is.character(v) && grepl(lowmem, v)) { cat(sprintf("SKIP %s | negative control: %s add-on with %s: addon_sim: %s\n", fn, ek, what, substr(v, 1, 200))); n_skip <<- n_skip + 1L } else check(sprintf("%s | negative control: %s add-on with %s: addon_sim differs (%s)", fn, ek, what, if (is.list(v)) paste(nrow(v$mismatches), "places") else substr(v, 1, 200)),
                is.list(v) && !isTRUE(v$ok))
        }
        if (!r$empty && first_ok < 2L && any(w <- !is.na(xa$text) & nzchar(xa$text))) {
          first_ok <- first_ok + 1L; i <- which(w)[1]; p <- xa$first[i] + nchar(xa$stag[i], "bytes")
          neg("one value changed", paste0(substring(sa, 1L, p - 1L), "9", substring(sa, p, nchar(sa, "bytes"))))
        }
        cr <- which(xa[["@delete"]] %in% "1" & xa[["@apply-to"]] %in% "share-weight" & xa$tag == "interpolation-rule")
        if (length(cr)) neg("the clearing rule appended instead", paste0(substring(sa, 1L, xa$first[cr[1]] - 1L), sub(' delete="1"', "", xa$stag[cr[1]], fixed = TRUE), substring(sa, xa$first[cr[1]] + nchar(xa$stag[cr[1]], "bytes"), nchar(sa, "bytes"))))
        if (!is.null(pb) && !is.null(xa[["@nocreate"]]) && !is.null(xb[["@nocreate"]])) {                   # the override's own nocreate dropped where the base only writes with nocreate
          pa <- paths(xa); bn <- xb[["@nocreate"]] %in% "1"
          nb <- tapply(bn, pb, all); mm <- tapply(!bn, pb, any)
          pp <- ifelse(xa$parent > 0L, pa[pmax(xa$parent, 1L)], "")       # parent's path (names may hold "/")
          cand <- which(xa[["@nocreate"]] %in% "1" & pa %in% names(nb)[nb] & (xa$depth <= 2L | pp %in% names(mm)[mm]))
          if (!is.null(config) && length(cand)) {                            # with config: where the earlier files already have the element, dropping nocreate changes nothing
            pe <- if (!noblk) sub("^[^/]*/[^/]*/", "", unique(unlist(lapply(xp, paths))))   # their elements, by path below <world>
            n0 <- length(cand); cand <- if (noblk) integer(0) else cand[!sub("^[^/]*/[^/]*/", "", pa[cand]) %in% pe]
            if (!length(cand)) { cat(sprintf("SKIP %s | negative control: %s add-on with the override's nocreate dropped: %s\n", fn, ek,
                                             if (noblk) "the earlier files' blocks not read (memory)" else sprintf("all %d elements are in the earlier files' blocks", n0))) }   # not counted: no memory stop of its own
          }
          if (length(cand)) { i <- cand[1]; neg("the override's nocreate dropped", paste0(substring(sa, 1L, xa$first[i] - 1L), sub(' nocreate="1"', "", xa$stag[i], fixed = TRUE), substring(sa, xa$first[i] + nchar(xa$stag[i], "bytes"), nchar(sa, "bytes")))) }
        }
        if (!is.null(config)) {
          na <- unique(xa[["@name"]]); na <- na[!is.na(na) & !na %in% c(xb[["@name"]], xo[["@name"]])]   # names not in the base file or the override: searched in the earlier files' bytes
          for (p in pf[file.exists(pf)]) {
            con <- file(p, "rb"); cr <- raw(0)
            while (length(na) && length(b <- readBin(con, "raw", 2^26))) { bb <- c(cr, b); na <- na[!vapply(na, function(v) length(grepRaw(paste0(' name="', v, '"'), bb, fixed = TRUE)) > 0L, NA)]; cr <- bb[max(1L, length(bb) - 65535L):length(bb)] }
            close(con)
          }
          check(sprintf("%s | %s #%d: every name in the add-on is in the base file, the override or an earlier file%s", fn, ek, ri, if (length(na)) paste(":", paste(head(na, 3), collapse = ", ")) else ""), !length(na))
          if (!is.null(r$prefix_md5)) {                                      # written as if only xmlInputFileName came before the base file
            tc <- tempfile(fileext = ".xml"); fn2 <- file.path(out_dir, paste0(tag, "_neg.xml"))
            writeLines(c("<Configuration>", "  <Files>", sprintf('    <Value name="xmlInputFileName">%s</Value>', pf[1]), "  </Files>", "  <ScenarioComponents>",
                         sprintf('    <Value name="%s">%s</Value>', sub("[.]xml$", "", fn), f), "  </ScenarioComponents>", "</Configuration>"), tc)
            r2 <- tryCatch(suppressMessages(addon_write(f, fo, fn2, config = tc)), error = function(e) conditionMessage(e)); unlink(tc)
            ie <- is.character(r2) && grepl("internal error", r2, fixed = TRUE)
            if (ie) check(sprintf("%s | %s #%d: add-on with every earlier-file answer untouched: %s", fn, ek, ri, substr(gsub("\n", " ", r2), 1, 300)), FALSE)
            s2 <- if (is.list(r2) && file.exists(fn2)) { z <- readBin(fn2, "raw", file.size(fn2)); z <- rawToChar(z); Encoding(z) <- "bytes"; z }
            lm <- is.character(r2) && grepl(lowmem, r2); if (lm) n_skip <<- n_skip + 1L
            if (ie) NULL else if (is.null(s2) || identical(sub("(?s)^.*?-->", "", s2, perl = TRUE), sub("(?s)^.*?-->", "", sa, perl = TRUE)))
              cat("SKIP", fn, ek, "negative control with every earlier-file answer untouched", if (lm) "(memory)" else if (is.null(s2)) "(refused)" else "(the same add-on)", "\n")
            else neg("every earlier-file answer replaced by untouched", s2)
          }
        }
      }
      if (!is.na(gb) && !noblk && gb > 0) { pk <- sum(gc()[, 6]) / 1024 - m0
                                           cat(sprintf("%s %s: earlier-file blocks %.3f GB, R memory peak %.2f GB above the edit's start (%.0f times the blocks)\n", fn, ek, gb, pk, pk / gb)) }
      unlink(c(fo, fa)); invisible(gc())
    }
    rm(x, F, xb, s, pb); invisible(gc())
    if (!is.null(config)) cat(sprintf("\nconfig mode: %s after %d earlier %s of %s (%.2f GB): %.1f min, peak %.1f GB\n", fn, length(pf), if (length(pf) == 1L) "file" else "files", basename(config), sum(file.size(pf[file.exists(pf)])) / 2^30,
                                      (proc.time()[["elapsed"]] - t_run) / 60, ps::ps_memory_info()[["peak_wset"]] / 2^30))
  }
  cat("\n"); print(res[, c("file", "edit", "expected", "verdict", "KB", "pct", "cpu_s")], row.names = FALSE)
  un <- res[res$expected == "refused" & res$verdict == "written", ]
  if (nrow(un)) { cat("\nWritten where a refusal was expected (not a failure; the naive simulator agreed):\n"); for (i in seq_len(nrow(un))) cat(sprintf("  %s | %s: %s\n", un$file[i], un$edit[i], un$detail[i])) }
  cat(sprintf("\nverdicts: %s\n", paste(names(table(res$verdict)), table(res$verdict), collapse = ", ")))
})

cat(sprintf("\n%d passed, %d failed%s\n", n_pass, n_fail, if (n_skip) sprintf("; %d SKIP for memory (naive simulator after the earlier files, or minaj's own memory stop)", n_skip) else ""))
if (n_fail) quit(status = 1)
