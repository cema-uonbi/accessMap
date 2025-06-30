#' @importFrom grDevices rainbow
#' @importFrom stats complete.cases
NULL

# Suppress R CMD check NOTEs for data.table and dplyr variables
utils::globalVariables(c(
  # data.table variables
  ".N", 
  "..admin_cols",
  
  # dplyr/ggplot2 variables (column names)
  "percentage", 
  "geometry", 
  "pop", 
  "population", 
  "total_pop"
))