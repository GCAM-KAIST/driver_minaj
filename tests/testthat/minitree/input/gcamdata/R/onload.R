.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("gcamdata.use_java"))) options(gcamdata.use_java = TRUE)
}
