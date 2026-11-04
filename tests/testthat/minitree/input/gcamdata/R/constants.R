# Constants, named as in gcamdata.

XML_DIR                <- "xml/"
OUTPUTS_DIR            <- "outputs/"
COMMENT_CHAR           <- "#"
data.SEPARATOR         <- "; "

FLAG_INPUT_DATA        <- "FLAG_INPUT_DATA"
FLAG_NO_OUTPUT         <- "FLAG_NO_OUTPUT"
FLAG_XML               <- "FLAG_XML"

MODEL_BASE_YEARS       <- 2015
MODEL_FUTURE_YEARS     <- c(2020, 2025)
MODEL_YEARS            <- c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)
YEAR_RECODE            <- c("start-year" = 2015, "final-calibration-year" = 2015, "end-year" = 2025)

driver.MAKE            <- "MAKE"
driver.DECLARE_OUTPUTS <- "DECLARE_OUTPUTS"
driver.DECLARE_INPUTS  <- "DECLARE_INPUTS"

mini.SCALE             <- 2       # used by module_mini_L100.base only
