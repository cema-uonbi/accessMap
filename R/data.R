#' Kenya Administrative Level 0 Boundaries (Country)
#'
#' @description
#' Country boundary for Kenya from GADM (Global Administrative Areas).
#' Used as default study area boundary in accessibility analysis.
#'
#' @format An sf object with country-level administrative boundary:
#' \describe{
#'   \item{NAME_0}{Country name}
#'   \item{geometry}{Polygon geometry}
#' }
#'
#' @source \url{https://gadm.org/download_country.html}
#' @examples
#' \dontrun{
#' # Use in accessibility analysis
#' result <- accessibility(facility_coords, adm0 = accessMap::adm0)
#' 
#' # Plot boundary
#' plot(accessMap::adm0)
#' }
"adm0"

#' Kenya Administrative Level 1 Boundaries (Counties)
#'
#' @description  
#' County boundaries for Kenya from GADM. Used for administrative
#' boundary overlays and spatial aggregation in accessibility analysis.
#'
#' @format An sf object with 47 county boundaries:
#' \describe{
#'   \item{NAME_0}{Country name}
#'   \item{NAME_1}{County name}
#'   \item{geometry}{Polygon geometry}
#' }
#'
#' @source \url{https://gadm.org/download_country.html}
#' @examples
#' \dontrun{
#' # Use in accessibility analysis
#' result <- accessibility(facility_coords, adm1 = accessMap::adm1)
#' 
#' # Generate county-level summaries
#' county_summary <- generate_admin_summary(access_data, accessMap::adm1)
#' }
"adm1"

#' Kenya Administrative Level 2 Boundaries (Sub-Counties)
#'
#' @description  
#' Sub-county boundaries for Kenya from GADM. Used for administrative
#' boundary overlays and spatial aggregation in accessibility analysis.
#'
#' @format An sf object with 300 sub-county boundaries:
#' \describe{
#'   \item{NAME_0}{Country name}
#'   \item{NAME_1}{County name}
#'   \item{NAME_2}{sub-county name}
#'   \item{geometry}{Polygon geometry}
#' }
#'
#' @source \url{https://gadm.org/download_country.html}
#' @examples
#' \dontrun{
#' # Use in accessibility analysis
#' result <- accessibility(facility_coords, adm1 = accessMap::adm1)
#' 
#' # Generate county-level summaries
#' county_summary <- generate_admin_summary(access_data, accessMap::adm1)
#' }
"adm2"

#' Kenya Administrative Level 3 Boundaries (wards)
#'
#' @description  
#' Ward boundaries for Kenya from GADM. Used for administrative
#' boundary overlays and spatial aggregation in accessibility analysis.
#'
#' @format An sf object with 1442 ward boundaries:
#' \describe{
#'   \item{NAME_0}{Country name}
#'   \item{NAME_1}{County name}
#'   \item{NAME_2}{sub-county name}
#'   \item{NAME_3}{ward name}
#'   \item{geometry}{Polygon geometry}
#' }
#'
#' @source \url{https://gadm.org/download_country.html}
#' @examples
#' \dontrun{
#' # Use in accessibility analysis
#' result <- accessibility(facility_coords, adm1 = accessMap::adm1)
#' 
#' # Generate county-level summaries
#' county_summary <- generate_admin_summary(access_data, accessMap::adm1)
#' }
"adm3"

#' Kenya Population Raster (WorldPop 2020)
#'
#' @description
#' High-resolution population count raster for Kenya from WorldPop.
#' Constrained to administrative boundaries and adjusted to UN estimates.
#'
#' @format A RasterLayer object:
#' \describe{
#'   \item{values}{Population count per pixel}
#'   \item{resolution}{Approximately 100m x 100m}
#'   \item{extent}{Kenya country boundary}
#' }
#'
#' @source \url{https://hub.worldpop.org/geodata/summary?id=49694}
#' @examples
#' \dontrun{
#' # Use in accessibility analysis
#' result <- accessibility(facility_coords, populationRaster = accessMap::population_raster)
#' 
#' # Plot population distribution
#' plot(accessMap::population_raster)
#' }
"population_raster"

#' Kenya Motorized Transport Transition Matrix
#'
#' @description
#' Transition matrix for calculating travel costs using motorized transport.
#' Based on friction surfaces from the Malaria Atlas Project incorporating
#' road networks, elevation, and land cover constraints.
#'
#' @format A TransitionLayer object for use with gdistance package
#'
#' @source \url{https://malariaatlas.org/project-resources/accessibility-to-healthcare/}
#' @examples
#' \dontrun{
#' # Use in accessibility analysis
#' result <- accessibility(facility_coords, transitionMatrix = accessMap::transition_matrix)
#' 
#' # Compute custom travel costs
#' travel_costs <- compute_travel_cost(points, accessMap::transition_matrix, 
#'                                    accessMap::population_raster)
#' }
"transition_matrix"