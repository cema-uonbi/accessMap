#' Compute Travel Cost Surface
#'
#' @description
#' Computes accumulated travel cost from points using a transition matrix.
#'
#' @param points Matrix of facility coordinates (lng, lat) - use validate_points() first
#' @param transitionMatrix TransitionLayer object for cost calculation
#' @param populationRaster Population raster.
#' @param progress Logical, show progress messages (default: TRUE)
#'
#' @return List with:
#' \describe{
#'   \item{access}{RasterLayer with travel times to nearest points location}
#'   \item{pop}{Population raster resampled to match accessibility surface}
#' }
#'
#' @examples
#' \dontrun{
#' # Load default data
#' data <- load_default_data()
#'
#' # Prepare coordinates
#' facility_coords <- matrix(c(36.8, -1.3), ncol = 2)
#' points <- validate_points(facility_coords)
#'
#' # Compute accessibility
#' results <- compute_travel_cost(points, data$transitionMatrix, data$populationRaster)
#'
#' # Plot results
#' plot(results$access, main = "Travel time (minutes)")
#' }
#'
#' @export
compute_travel_cost <- function(points,
                                transitionMatrix = NULL,
                                populationRaster = NULL,
                                progress = TRUE) {
  if (progress) {
    cat(crayon::blue$bold("Computing travel costs...\n"))
  }
  access <- gdistance::accCost(transitionMatrix, points)
  raster::crs(access) <- raster::crs(populationRaster)
  if (progress) {
    cat(crayon::green("[DONE] Travel time calculation completed\n"))
    cat(crayon::blue$bold("Resampling population raster...\n"))
  }
  pop_resampled <- raster::raster(terra::resample(
    terra::rast(populationRaster),
    terra::rast(access),
    method = "sum",
    threads = TRUE
  ))
  list(access = access, pop = pop_resampled)
}

#' Process accessibility data to data.table
#'
#' @description
#' Converts accessibility and population rasters to a data.table with
#' coordinates, population counts, travel times, and travel time categories.
#'
#' @param access_raster RasterLayer with travel times
#' @param populationRaster Population raster resampled to match accessibility surface (from compute_travel_cost())
#' @param categories Travel time categories (from create_categories())
#'
#' @return data.table with columns: x, y, pop, travel_time, cat_travel_time
#'
#' @examples
#' \dontrun{
#'  # After computing travel costs
#' data <- load_default_data()
#' points <- validate_points(facility_coords)
#' results <- compute_travel_cost(points, data$transitionMatrix, data$populationRaster)
#' # Process to data.table
#' cats <- default_categories()
#' dt <- process_access_data(results$access, results$pop, cats)
#' # Examine results
#' head(dt)
#' dt[, .N, by = cat_travel_time]
#'
#' }
#'
#' @export
process_access_data <- function(access_raster, populationRaster, categories) {
  temp <- raster::stack(populationRaster, access_raster)
  temp_dt <- data.table::as.data.table(
    terra::rast(temp),
    xy = TRUE
  )[, data.table::setnames(.SD, c("x", "y", "pop", "travel_time"))
  ][is.finite(travel_time)
  ][, cat_travel_time := categorize_travel_time(travel_time, categories)][]

  return(temp_dt)
}
