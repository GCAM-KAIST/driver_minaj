module_mini_L201.other <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "mini/A201.other"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.Subsector", "L201.SubsectorShrwtFllt"))
  } else if(command == driver.MAKE) {
    all_data <- list(...)[[1]]
    A201.other <- get_data(all_data, "mini/A201.other")

    L201.Subsector <- tibble::tibble(region = A201.other$region, supplysector = A201.other$supplysector, subsector = A201.other$subsector)
    L201.SubsectorShrwtFllt <- tibble::tibble(L201.Subsector, year.fillout = MODEL_BASE_YEARS, share.weight = A201.other$share.weight)

    L201.Subsector <- add_precursors(add_comments(add_units(add_title(L201.Subsector, "Subsectors"), "none"), "From A201.other"), "mini/A201.other")
    L201.Subsector <- add_flags(L201.Subsector, FLAG_NO_OUTPUT)
    L201.SubsectorShrwtFllt <- add_precursors(add_comments(add_units(add_title(L201.SubsectorShrwtFllt, "Subsector share-weights"), "unitless"), "From A201.other"), "mini/A201.other")

    return_data(L201.Subsector, L201.SubsectorShrwtFllt)
  }
}
