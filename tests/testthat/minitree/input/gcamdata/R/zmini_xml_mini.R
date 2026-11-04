module_mini_mini_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L200.Subsector", "L200.SubsectorShrwtFllt", "L200.StubTechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "mini.xml"))
  } else if(command == driver.MAKE) {
    all_data <- list(...)[[1]]
    mini.xml <- create_xml("mini.xml")
    mini.xml <- add_xml_data(mini.xml, get_data(all_data, "L200.Subsector"), "Subsector")
    mini.xml <- add_xml_data(mini.xml, get_data(all_data, "L200.SubsectorShrwtFllt"), "SubsectorShrwtFllt")
    mini.xml <- add_xml_data(mini.xml, get_data(all_data, "L200.StubTechCoef"), "StubTechCoef")
    mini.xml <- add_precursors(mini.xml, "L200.Subsector", "L200.SubsectorShrwtFllt", "L200.StubTechCoef")
    return_data(mini.xml)
  }
}

module_mini_other_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Subsector", "L201.SubsectorShrwtFllt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "other.xml"))
  } else if(command == driver.MAKE) {
    all_data <- list(...)[[1]]
    other.xml <- create_xml("other.xml")
    other.xml <- add_xml_data(other.xml, get_data(all_data, "L201.Subsector"), "Subsector")
    other.xml <- add_xml_data(other.xml, get_data(all_data, "L201.SubsectorShrwtFllt"), "SubsectorShrwtFllt")
    other.xml <- add_precursors(other.xml, "L201.Subsector", "L201.SubsectorShrwtFllt")
    return_data(other.xml)
  }
}
