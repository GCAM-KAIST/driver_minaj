module_mini_L200.coef <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L100.base",
             FILE = "mini/A200.coef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L200.Subsector", "L200.SubsectorShrwtFllt", "L200.StubTechCoef"))
  } else if(command == driver.MAKE) {
    all_data <- list(...)[[1]]
    L100.base <- get_data(all_data, "L100.base")
    A200.coef <- get_data(all_data, "mini/A200.coef")

    base <- L100.base[L100.base$year == MODEL_BASE_YEARS, ]
    tech <- merge(base, A200.coef, by = "sector")
    tech <- tech[order(tech$region, tech$sector, tech$subsector), ]
    L200.Subsector <- tibble::tibble(region = tech$region, supplysector = tech$sector, subsector = tech$subsector)
    L200.SubsectorShrwtFllt <- tibble::tibble(L200.Subsector, year.fillout = MODEL_BASE_YEARS,
                                              share.weight = mini_round(tech$value / ave(tech$value, tech$region, FUN = sum)))
    fy <- rep(MODEL_FUTURE_YEARS, each = nrow(tech))
    tf <- tech[rep(seq_len(nrow(tech)), times = length(MODEL_FUTURE_YEARS)), ]
    L200.StubTechCoef <- tibble::tibble(region = tf$region, supplysector = tf$sector, subsector = tf$subsector,
                                        stub.technology = tf$technology, year = fy,
                                        minicam.energy.input = tf$minicam.energy.input,
                                        coefficient = mini_round(tf$coefficient * (1 - (fy - MODEL_BASE_YEARS) / 100)),
                                        market.name = tf$region)

    L200.Subsector <- add_precursors(add_comments(add_units(add_title(L200.Subsector, "Subsectors"), "none"), "From A200.coef"), "L100.base", "mini/A200.coef")
    L200.SubsectorShrwtFllt <- add_precursors(add_comments(add_units(add_title(L200.SubsectorShrwtFllt, "Subsector share-weights"), "unitless"), "Base-year shares of L100.base"), "L100.base", "mini/A200.coef")
    L200.StubTechCoef <- add_precursors(add_comments(add_units(add_title(L200.StubTechCoef, "Input coefficients"), "EJ/EJ"), "A200.coef, falling 1% per year"), "L100.base", "mini/A200.coef")

    return_data(L200.Subsector, L200.SubsectorShrwtFllt, L200.StubTechCoef)
  }
}
