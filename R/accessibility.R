#' Calculate Spatial Accessibility to Facilities
#'
#' @description
#' Calculates and visualizes spatial accessibility to facilities (e.g., health centers,
#' veterinary clinics) using cost-distance analysis based on travel time. The function
#' performs accessibility modeling by computing accumulated travel costs from facility
#' locations to population centers using motorized friction surfaces, categorizes
#' populations by travel time bands, and generates visualizations
#' including maps and summary statistics.
#'
#' The methodology follows established accessibility modeling frameworks as described in
#' Weiss et al. (2018, 2020), Pfeffer et al. (2018), and van Etten (2017), which emphasize
#' the importance of incorporating realistic travel impedances and population distribution
#' for healthcare accessibility analysis. The default friction surface uses motorized
#' transport assumptions from the Malaria Atlas Project.
#'
#' @param points Matrix or data.frame with facility coordinates. Must contain longitude
#'   and latitude columns (automatically detected or specify lng_col/lat_col).
#' @param transitionMatrix Optional TransitionLayer object for travel cost calculation.
#'   If NULL, uses default Kenya transition matrix based on motorized friction surface
#'   from the Malaria Atlas Project (https://malariaatlas.org/project-resources/accessibility-to-healthcare/).
#'   **Important**: If providing your own transition matrix, ensure it is properly cropped
#'   and masked using your study area boundary to avoid computational errors and ensure
#'   accurate results within your study area.
#' @param populationRaster Optional population raster (SpatRaster or RasterLayer).
#'   If NULL, uses default Kenya WorldPop population data (https://hub.worldpop.org/geodata/summary?id=49694).
#' @param adm0 Optional country/study area boundary (sf object). If NULL, uses default
#'   Kenya administrative boundaries (level 0) from GADM (https://gadm.org/download_country.html).
#' @param adm1 Optional administrative subdivision boundaries (sf object). If NULL,
#'   uses default Kenya administrative boundaries (level 1) from GADM.
#' @param categories Travel time categories created by create_categories() or default_categories().
#'   If NULL, uses default 5-category classification: "<=30", "31-60", "61-90", "91-120", ">120" minutes.
#' @param lng_col Character name of longitude column in points (default: "lng"). Auto-detected if NULL.
#' @param lat_col Character name of latitude column in points (default: "lat"). Auto-detected if NULL.
#' @param label Character string for legend label describing the facilities (default: "facilities").
#' @param show_admin Logical, whether to show administrative boundaries on map (default: TRUE).
#' @param show_scale Logical, whether to show scale bar and north arrow (default: TRUE).
#' @param path Character string specifying output directory for saved files (default: "." for current directory).
#' @param save_opts List containing plot saving parameters:
#'   \itemize{
#'     \item dpi: Resolution for saved plots (default: 1000)
#'     \item width: Base width multiplier (default: 1)
#'     \item height: Base height multiplier (default: 1)
#'     \item units: Units for dimensions (default: "in")
#'   }
#' @param name Character string for output file prefix. If NULL, uses generic names.
#' @param save_files Logical, whether to save outputs to disk (default: TRUE).
#' @param progress Logical, show progress messages (default: TRUE).
#' @param generate_admin_summary Logical, whether to generate administrative unit summaries (default: FALSE).
#' @param ... Additional arguments passed to individual functions:
#'   \itemize{
#'     \item For \code{generate_admin_summary()}: admin_name_cols, admin_code_cols
#'     \item For \code{create_map()}: point_color, point_shape, point_size, 
#'           point_colors, point_shapes, show_point_legend
#'     \item For \code{create_bar_plot()}: show_labels, color_labels
#'     \item For \code{combine_plots()}: map_width, bar_position
#'     \item For \code{save_outputs()}: formats
#'   }
#'
#' @return A list containing:
#' \describe{
#'   \item{national_summary}{data.table with population counts and percentages by
#'     travel time category}
#'   \item{admin_summary}{data.table with population counts and percentages by
#'     travel time category for each administrative unit (only if generate_admin_summary = TRUE)}
#'   \item{map_plot}{ggplot2 object showing spatial accessibility map}
#'   \item{bar_plot}{ggplot2 object showing population distribution by travel time}
#'   \item{combined_plot}{ggplot2 object combining map and bar chart}
#'   \item{access_raster}{RasterLayer containing travel time values (in minutes) to nearest facility}
#'   \item{processed_data}{data.table containing gridded accessibility data with coordinates,
#'     population counts, travel times, and travel time categories for further analysis}
#'   \item{categories}{Travel time categories used in the analysis}
#'   \item{points}{Validated facility coordinates matrix}
#' }
#'
#' @details
#' The accessibility analysis follows a standardized workflow:
#'
#' **1. Cost-Distance Calculation**: Uses the gdistance package to compute accumulated
#' travel costs from each facility point to all population cells, accounting for
#' terrain, road networks, and other mobility constraints encoded in the transition matrix.
#' If more than one coordinate is supplied in points, the function calculates the minimum
#' least-cost distance from any origin point. The function uses Dijkstra's algorithm 
#' (as implemented in the igraph package). See \link[gdistance]{accCost} for more details.
#'
#' **2. Population Allocation**: Resamples population data to match the resolution of
#' the accessibility surface using sum-based aggregation to preserve population counts.
#'
#' **3. Travel Time Categorization**: Classifies population into accessibility bands
#' based on provided categories or default 5-category system:
#' \itemize{
#'   \item <=30 minutes: High accessibility
#'   \item 31-60 minutes: Moderate accessibility
#'   \item 61-90 minutes: Low accessibility
#'   \item 91-120 minutes: Very low accessibility
#'   \item >120 minutes: Extremely low accessibility
#' }
#'
#' **4. Visualization**: Creates maps and summary charts showing
#' spatial distribution of accessibility and population coverage statistics.
#'
#' **Data Requirements and Recommendations:**
#'
#' - **Friction Surface**: The default transition matrix is derived from motorized
#'   friction surfaces provided by the Malaria Atlas Project, which incorporates
#'   road networks, elevation, land cover, and other mobility constraints optimized
#'   for motorized transport accessibility analysis.
#'
#' - **Transition Matrix**: Should incorporate realistic travel speeds for different
#'   land cover types, elevation constraints, and road networks. When using custom
#'   transition matrices, ensure proper spatial extent alignment with your study area
#'   by cropping and masking with the study area boundary to prevent edge effects and
#'   computational inefficiencies.
#'
#' - **Population Data**: High-resolution gridded population data (e.g., WorldPop,
#'   LandScan) provides more accurate accessibility estimates than administrative
#'   unit averages.
#'
#' - **Administrative Boundaries**: Used for cartographic visualization and should
#'   match the coordinate reference system of other input data.
#'
#' @section File Outputs:
#' When save_files = TRUE, the function automatically saves the following files to the specified path:
#' \itemize{
#'   \item {name}_combined_plot.png: Integrated map and summary chart
#'   \item {name}_map_plot.png: Accessibility map only
#'   \item {name}_bar_plot.png: Summary bar chart only
#'   \item {name}_national_summary.csv: Summary statistics table
#'   \item {name}_admin_summary.csv: Administrative summary statistics (if generate_admin_summary = TRUE)
#' }
#'
#' @references
#' Pfeffer, D. A., Lucas, T. C., May, D., Harris, J., Rozier, J., Twohig, K. A.,
#' ... & Gething, P. W. (2018). malariaAtlas: an R interface to global malariometric
#' data hosted by the Malaria Atlas Project. \emph{Malaria Journal}, 17(1), 352.
#'
#' van Etten, J. (2017). R Package gdistance: Distances and Routes on Geographical
#' Grids. \emph{Journal of Statistical Software}, 76(13), 1-21.
#'
#' Weiss, D. J., Nelson, A., Gibson, H. S., Temperley, W., Peedell, S., Lieber, A.,
#' ... & Gething, P. W. (2018). A global map of travel time to cities to assess
#' inequalities in accessibility in 2015. \emph{Nature}, 553(7688), 333-336.
#'
#' Weiss, D. J., Nelson, A., Vargas-Ruiz, C. A., Gligoric, K., Bavadekar, S.,
#' Gabrilovich, E., ... & Gething, P. W. (2020). Global maps of travel time to
#' healthcare facilities. \emph{Nature Medicine}, 26(12), 1835-1838.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default Kenya data
#' facility_coords <- matrix(c(36.8, -1.3, 37.0, -1.5), ncol = 2, byrow = TRUE)
#' colnames(facility_coords) <- c("lng", "lat")
#'
#' result <- accessibility(
#'   points = facility_coords,
#'   label = "Health Facilities",
#'   name = "kenya_health_access",
#'   save_files = FALSE 
#' )
#'
#' # View results
#' print(result$national_summary)
#' plot(result$combined_plot)
#'
#' # Custom categories
#' custom_cats <- create_categories(
#'   breaks = c(0, 15, 30, 60, Inf),
#'   labels = c("Immediate", "Quick", "Moderate", "Slow"),
#'   palette = "viridis"
#' )
#'
#' result <- accessibility(
#'   points = facility_coords,
#'   categories = custom_cats,
#'   name = "custom_access"
#' )
#'
#' # Using your own data
#' result <- accessibility(
#'   points = my_facilities,
#'   transitionMatrix = my_transition,  # Ensure cropped/masked with study area
#'   populationRaster = my_population,
#'   adm0 = my_country_boundary,
#'   adm1 = my_admin_boundaries,
#'   label = "Veterinary Clinics",
#'   path = "./outputs",
#'   save_opts = list(dpi = 300, width = 1.5, height = 1.5, units = "in"),
#'   name = "veterinary_access",
#'   save_files = FALSE
#' )
#'
#' # High-resolution outputs without admin boundaries
#' result <- accessibility(
#'   points = facility_coords,
#'   show_admin = FALSE,
#'   save_opts = list(dpi = 2000, width = 2, height = 2, units = "in"),
#'   name = "high_res_analysis",
#'   save_files = FALSE
#' )
#'
#' # Workflow using individual functions for more control
#' # Step 1: Load and validate data
#' kenya_data <- load_default_data()
#' pts <- validate_points(facility_coords)
#' cats <- default_categories()
#'
#' # Step 2: Compute accessibility
#' travel_results <- compute_travel_cost(pts, kenya_data$transitionMatrix, 
#'                                      kenya_data$populationRaster)
#'
#' # Step 3: Process data
#' access_dt <- process_access_data(travel_results$access, travel_results$pop, cats)
#'
#' # Step 4: Generate summaries and visualizations
#' summary_stats <- generate_summary(access_dt, cats)
#' map_plot <- create_map(access_dt, pts, kenya_data$adm0, kenya_data$adm1, cats)
#' bar_plot <- create_bar_plot(summary_stats, cats)
#' combined_plot <- combine_plots(map_plot, bar_plot)
#' 
#' # With custom styling
#' result <- accessibility(
#'   points = facility_coords,
#'   point_color = "blue",           # passed to create_map()
#'   show_labels = FALSE,            # passed to create_bar_plot()
#'   map_width = 0.8                 # passed to combine_plots()
#' )
#' 
#' result <- accessibility(
#'   points = facility_coords,
#'   generate_admin_summary = TRUE,
#'   admin_name_cols = "NAME_1"  # County names
#' )
#' }
#' @export
accessibility <- function(points,
                          transitionMatrix = NULL,
                          populationRaster = NULL,
                          adm0 = NULL,
                          adm1 = NULL,
                          categories = NULL,
                          lng_col = "lng",
                          lat_col = "lat", 
                          label = "facilities",
                          show_admin = TRUE,
                          show_scale = TRUE,
                          path = ".",
                          save_opts = list(dpi = 1000, width = 1, height = 1, units = "in"),
                          name = NULL,
                          save_files = F,
                          progress = TRUE,
                          generate_admin_summary = FALSE,
                          ...
                          ) {
  
  if (progress) {
    cat(crayon::blue$bold("Starting accessibility analysis...\n"))
  }
  points <- validate_points(points, lng_col, lat_col)
  if (is.null(transitionMatrix) || is.null(populationRaster) || is.null(adm0) || is.null(adm1)) {
    if (progress) cat(crayon::blue("Loading default Kenya datasets...\n"))
    if (is.null(transitionMatrix)) transitionMatrix <- accessMap::transition_matrix
    if (is.null(populationRaster)) populationRaster <- accessMap::population_raster
    if (is.null(adm0)) adm0 <- if (show_admin) accessMap::adm0 else NULL
    if (is.null(adm1)) adm1 <- if (show_admin) accessMap::adm1 else NULL
  }
  if (is.null(categories)) {
    categories <- default_categories()
  }
  travel_results <- compute_travel_cost(points, transitionMatrix, populationRaster, progress)
  processed_data <- process_access_data(travel_results$access, travel_results$pop, categories)
  national_summary <- generate_summary(processed_data, categories, progress)
  admin_summary <- NULL
  if (generate_admin_summary & !is.null(adm1)) {
    admin_summary <- .call_args(
      generate_admin_summary,
      base_args = list(
        data = processed_data,
        admin_sf = adm1,
        progress = progress
      ),
      valid_extra_args = c('admin_name_cols', 'admin_code_cols'),
      ...
    )
  }
  if (progress) {
    cat(crayon::blue$bold("Creating visualizations...\n"))
  }
  map_plot <- .call_args(
    create_map,
    base_args = list(
      data = processed_data,
      points = points,
      adm0 = adm0,
      adm1 = adm1,
      categories = categories,
      label = label,
      show_admin = show_admin,
      show_scale = show_scale
    ),
    valid_extra_args = c(
      "point_color",
      "point_shape",
      "point_size",
      "point_colors",
      "point_shapes",
      "show_point_legend",
      "legend_position"
    ),
    ...
  )
  bar_plot <- .call_args(
    create_bar_plot,
    base_args = list(national_summary, categories),
    valid_extra_args = c("show_labels", "color_labels", "title", "label_position"),
    ...
  )
  if (progress) {
    cat(crayon::blue$bold("Combining visualizations...\n"))
  }
  combined_plot <- combine_plots(map_plot, bar_plot)
  if (save_files) {
    plots <- list(map_plot = map_plot,
                  bar_plot = bar_plot,
                  combined_plot = combined_plot)
    
    .call_args(
      save_outputs,
      base_args = list(plots, national_summary, path, name, save_opts),
      valid_extra_args = c("formats"),
      ...
    )
    if (!is.null(admin_summary)) {
      admin_file <- file.path(path, paste0(name %||% "accessibility", "_admin_summary.csv"))
      data.table::fwrite(admin_summary, file = admin_file)
    }
  }
  if (progress) {
    cat(crayon::green$bold("Accessibility analysis completed successfully!\n"))
  }
  result <- list(
    national_summary = national_summary,
    map_plot = map_plot,
    bar_plot = bar_plot,
    combined_plot = combined_plot,
    access_raster = travel_results$access,
    processed_data = processed_data,
    categories = categories,
    points = points
  )
  if (!is.null(admin_summary)) {
    result$admin_summary <- admin_summary
  }
  return(result)
}

#' Helper function to call functions with filtered arguments
#' 
#' @description
#' Clean helper that handles the rlang magic internally. This keeps the main
#' function readable while providing the benefits of rlang.
#'
#' @param fun Function to call
#' @param base_args Named list of base arguments
#' @param valid_extra_args Character vector of valid extra argument names
#' @param ... Additional arguments from the user
#' @return Result of function call
#' @keywords internal
#' @noRd
.call_args <- function(fun, base_args, valid_extra_args, ...) {
  extra_args <- rlang::enquos(...)
  filtered_extra <- extra_args[intersect(names(extra_args), valid_extra_args)]
  all_args <- c(base_args, filtered_extra)
  call_obj <- rlang::call2(.fn = substitute(fun), !!!all_args)
  rlang::eval_tidy(call_obj)
}
