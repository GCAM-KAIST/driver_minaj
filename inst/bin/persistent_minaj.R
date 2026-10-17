# Start the persistent minaj worker: Rscript persistent_minaj.R <tree> [store]
# Runs until `pm stop`. Uses the installed minaj, or the source folder in MINAJ_HOME when set.
# Without [store], the default store applies (option minaj.store, else
# file.path(tools::R_user_dir("minaj", "cache"), "minaj_store")).
a <- commandArgs(trailingOnly = TRUE)
if (!length(a)) stop("usage: Rscript persistent_minaj.R <tree> [store]", call. = FALSE)
home <- Sys.getenv("MINAJ_HOME")
if (nzchar(home)) suppressMessages(pkgload::load_all(home, quiet = TRUE)) else library(minaj)
if (length(a) >= 2) persistent_minaj(a[1], store = a[2]) else persistent_minaj(a[1])
