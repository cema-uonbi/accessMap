# Suppress R CMD check notes for data.table syntax
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".SD", ":=", ".", "let",
    "travel_time", "cat_travel_time", "ken_pop_count", "percent",
    "Travel time", "Percentage of the population",
    "x", "y", "lng", "lat"
  ))
}

.onAttach <- function(libname, pkgname) {
  if (!isNamespaceLoaded("sf")) {
    requireNamespace("sf", quietly = TRUE)
  }
}
.datatable.aware <- TRUE
