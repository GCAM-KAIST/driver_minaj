module_mini_L100.base <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "mini/A100.base",
             OPTIONAL_FILE = "mini/A100.extra"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.base"))
  } else if(command == driver.MAKE) {
    all_data <- list(...)[[1]]
    A100.base <- get_data(all_data, "mini/A100.base")
    A100.extra <- get_data(all_data, "mini/A100.extra")
    if(!is.null(A100.extra)) A100.base <- rbind(A100.base, A100.extra)

    yrs <- as.character(MODEL_YEARS)
    L100.base <- tibble::tibble(region = rep(A100.base$region, length(yrs)),
                                sector = rep(A100.base$sector, length(yrs)),
                                year = rep(MODEL_YEARS, each = nrow(A100.base)),
                                value = unlist(A100.base[yrs], use.names = FALSE) * mini.SCALE)
    L100.base <- add_title(L100.base, "Output by region, sector and year")
    L100.base <- add_units(L100.base, "EJ")
    L100.base <- add_comments(L100.base, "Year columns of A100.base, scaled")
    L100.base <- add_precursors(L100.base, "mini/A100.base", "mini/A100.extra")

    return_data(L100.base)
  }
}
