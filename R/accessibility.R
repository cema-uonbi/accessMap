# Suppress R CMD check notes for data.table syntax
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".SD", ":=", ".", "let",
    "travel_time", "cat_travel_time", "ken_pop_count", "percent",
    "Travel time", "Percentage of the population",
    "x", "y", "lng", "lat"
  ))
}

.datatable.aware <- TRUE

#' Calculate Spatial Accessibility to Facilities
#'
#' @description
#' Calculates and visualizes spatial accessibility to facilities (e.g., health centers,
#' veterinary clinics) using the cost-distance analysis based on travel time. The function
#' performs accessibility modeling by computing accumulated travel costs from facility
#' locations to population centers using motorized friction surfaces, categorizes
#' populations by travel time bands, and generates comprehensive visualizations
#' including maps and summary statistics.
#'
#' The methodology follows established accessibility modeling frameworks as described in
#' Weiss et al. (2018, 2020), Pfeffer et al. (2018), and van Etten (2017), which emphasize
#' the importance of incorporating realistic travel impedances and population distribution
#' for healthcare accessibility analysis. The default friction surface uses motorized
#' transport assumptions from the Malaria Atlas Project.
#'
#' @param points Matrix or data frame with facility coordinates. Must contain longitude
#'   and latitude columns named (lng, lat) representing facility locations.
#' @param transitionMatrix Optional TransitionLayer object for travel cost calculation.
#'   If NULL, uses default Kenya transition matrix based on motorized friction surface
#'   from the Malaria Atlas Project (https://malariaatlas.org/project-resources/accessibility-to-healthcare/).
#'   **Important**: If providing your own transition matrix, ensure it is properly cropped
#'   and masked using your adm0 shapefile to avoid computational errors and ensure
#'   accurate results within your study area.
#' @param populationRaster Optional population raster (SpatRaster or RasterLayer).
#'   If NULL, uses default Kenya WorldPop population data (https://hub.worldpop.org/geodata/summary?id=49694).
#' @param adm0 Optional country/study area boundary (sf object). If NULL, uses default
#'   Kenya administrative boundaries (level 0) from gadm (https://gadm.org/download_country.html).
#' @param adm1 Optional administrative subdivision boundaries (sf object). If NULL,
#'   uses default Kenya administrative boundaries (level 1) from gadm (https://gadm.org/download_country.html).
#' @param label Character string for legend label describing the facilities
#'   (default: "Facilities").
#' @param path Character string specifying output directory for saved files
#'   (default: "." for current directory).
#' @param saving_list List containing plot saving parameters:
#'   \itemize{
#'     \item dpi: Resolution for saved plots (default: 1000)
#'     \item width: Base width multiplier (default: 1)
#'     \item height: Base height multiplier (default: 1)
#'     \item units: Units for dimensions (default: "in")
#'   }
#' @param name Character string for output file prefix. If NULL, uses generic names.
#' @details
#' The accessibility analysis follows a standardized workflow:
#'
#' 1. **Cost-Distance Calculation**: Uses the gdistance package to compute accumulated
#'    travel costs from each facility point to all population cells, accounting for
#'    terrain, road networks, and other mobility constraints encoded in the transition matrix. Note that
#'    If more than one coordinate is supplied in points, the function calculates the minimum
#'    least-cost distance from any origin point.The function uses Dijkstra's algorithm (as implemented in the igraph package).
#'    see \link[gdistance]{accCost} for more details.
#'
#' 2. **Population Allocation**: Resamples population data to match the resolution of
#'    the accessibility surface using sum-based aggregation to preserve population counts.
#'
#' 3. **Travel Time Categorization**: Classifies population into five accessibility bands:
#'    - <=30 minutes: High accessibility
#'    - 31-60 minutes: Moderate accessibility
#'    - 61-90 minutes: Low accessibility
#'    - 91-120 minutes: Very low accessibility
#'    - \>120 minutes: Extremely low accessibility
#'
#' 4. **Visualization**: Creates publication-ready maps and summary charts showing
#'    spatial distribution of accessibility and population coverage statistics.
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
#'   by cropping and masking with the adm0 boundary to prevent edge effects and
#'   computational inefficiencies.
#'
#' - **Population Data**: High-resolution gridded population data (e.g., WorldPop,
#'   LandScan) provides more accurate accessibility estimates than administrative
#'   unit averages.
#'
#' - **Administrative Boundaries**: Used for cartographic visualization and should
#'   match the coordinate reference system of other input data.
#'
#' @return A list containing:
#' \describe{
#'   \item{national_summary}{Data.table with population counts and percentages by
#'     travel time category}
#'   \item{map_plot}{ggplot2 object showing spatial accessibility map}
#'   \item{bar_plot}{ggplot2 object showing population distribution by travel time}
#'   \item{combined_plot}{ggplot2 object combining map and bar chart}
#'   \item{access}{RasterLayer containing travel time values (in minutes) to nearest facility}
#'   \item{temp_df}{Data.table containing gridded accessibility data with coordinates,
#'     population counts, travel times, and travel time categories for further analysis}
#' }
#'
#' @section File Outputs:
#' The function automatically saves the following files to the specified path:
#' \itemize{
#'   \item {name}_combined_plot.png: Integrated map and summary chart
#'   \item {name}_map_plot.png: Accessibility map only
#'   \item {name}_bar_plot.png: Summary bar chart only
#'   \item {name}_national_summary.csv: Summary statistics table
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
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with default Kenya data
#' facility_coords <- matrix(c(36.8, -1.3, 37.0, -1.5), ncol = 2, byrow = TRUE)
#' colnames(facility_coords) <- c("lng", "lat")
#'
#' result <- accessibility(
#'   points = facility_coords,
#'   label = "Health Facilities",
#'   name = "kenya_health_access"
#' )
#'
#' # View results
#' print(result$national_summary)
#' plot(result$combined_plot)
#'
#' # Custom analysis with your own data
#' result <- accessibility(
#'   points = my_facilities,
#'   transitionMatrix = my_transition,  # Ensure cropped/masked with adm0
#'   populationRaster = my_population,
#'   adm0 = my_country_boundary,
#'   adm1 = my_admin_boundaries,
#'   label = "Veterinary Clinics",
#'   path = "./outputs",
#'   saving_list = list(dpi = 300, width = 1.5, height = 1.5),
#'   name = "veterinary_access"
#' )
#'
#' # High-resolution outputs
#' result <- accessibility(
#'   points = facility_coords,
#'   saving_list = list(dpi = 2000, width = 2, height = 2),
#'   name = "high_res_analysis"
#' )
#' }
#'
accessibility <- \(
  points,
  transitionMatrix = NULL,
  populationRaster = NULL,
  label = "facilities",
  adm0 = NULL,
  adm1 = NULL,
  path = ".",
  saving_list = list(dpi = 1e3, width = 1, height = 1, units = "in"),
  name = NULL
) {
  if (!inherits(points, "matrix")) {
    points <- as.matrix(points)
  }
  colnames(points) <- c("lng", "lat")
  cat(crayon::blue$bold("Starting accessibility analysis...\n"))
  if (is.null(populationRaster)) {
    populationRaster <- raster::raster(system.file("extdata", "ken_ppp_2020_UNadj_constrained.tif", package = "accessMap"))
  }
  if (is.null(transitionMatrix)) {
    transitionMatrix <- readRDS(system.file("extdata", "kenya_transition_matrix.RDS", package = "accessMap"))
  }
  if (is.null(adm0)) {
    adm0 <- sf::st_read(system.file("extdata", "gadm41_KEN_0.shp", package = "accessMap"), quiet = T)
  }
  if (is.null(adm1)) {
    adm1 <- sf::st_read(system.file("extdata", "gadm41_KEN_1.shp", package = "accessMap"), quiet = T)
  }
  cat(crayon::blue$bold("Computing travel costs...\n"))
  access <- gdistance::accCost(transitionMatrix, points)
  raster::crs(access) <- raster::crs(populationRaster)
  cat(crayon::green("[DONE] Travel time calculation completed\n"))
  cat(crayon::blue$bold("Resampling population raster file...\n"))
  pop <- raster::raster(terra::resample(
    terra::rast(populationRaster),
    terra::rast(access),
    method = "sum",
    threads = T
  ))
  temp <- raster::stack(pop, access)
  temp_df <- data.table::as.data.table(terra::rast(temp), xy = T)[, data.table::setnames(.SD, c("x", "y", "ken_pop_count", "travel_time"))
                                                                  ] [is.finite(travel_time)
                                                                     ][, cat_travel_time := data.table::fcase(
      travel_time <= 30,  "<=30",
      travel_time > 30 & travel_time <= 60, "31-60",
      travel_time > 60 & travel_time <= 90, "61-90",
      travel_time > 90 & travel_time <= 120, "91-120",
      travel_time > 120, ">120",
      default = NA_character_
    )]
  cat(crayon::blue$bold("Generating summary statistics...\n"))
  national_summary <- data.table::copy(
    temp_df[, .(
      pop = sum(ken_pop_count, na.rm = T)),
      by = cat_travel_time])[, percent := pop/sum(pop, na.rm = T)*1e2
                             ][, data.table::setnames(.SD, c("Travel time", "Population", "Percentage of the population"))
                               ][, let(
      `Travel time` = factor(`Travel time`, levels = c(
        ">120",
        "91-120",
        "61-90",
        "31-60",
        "<=30"
      ))
    )][]
  cat(crayon::blue$bold("Building accessibility map...\n"))
  map_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = adm0, fill = "white", alpha = 1, show.legend = NA, color = NA) +
    ggplot2::geom_raster(
      data = temp_df,
      ggplot2::aes(
        x = x,
        y = y,
        fill = factor(cat_travel_time)
      ),
      interpolate = FALSE,
      show.legend = NA
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        ">120" = "#08306b",
        "91-120" = "#2171b5",
        "61-90" = "#6baed6",
        "31-60" = "#c6dbef",
        "<=30" = "#f7fbff"
      ),
      guide = "none",
      na.value = "#969696"
    ) +
    ggplot2::geom_sf(data = adm1, fill = NA, lwd = 0.1, colour = "black") +
    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.25,
      pad_x = ggplot2::unit(0.26, "in"),
      pad_y = ggplot2::unit(0.4, "in"),
      height = ggplot2::unit(0.15, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "bl",
      which_north = "true",
      pad_x = ggplot2::unit(0.57, "in"),
      pad_y = ggplot2::unit(0.6, "in"),
      height = ggplot2::unit(0.9, "cm"),
      width = ggplot2::unit(0.81, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(text_size = 10)
    ) +
    ggplot2::geom_point(
      data = data.table::as.data.table(points),
      ggplot2::aes(x = lng, y = lat, color = "value"),
      size = 4.5,
      shape = "+"
    ) +
    ggplot2::scale_colour_manual(
      name = " ",
      values = "red",
      labels = label,
      guide = ggplot2::guide_legend()
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key = ggplot2::element_rect(color = "black", fill = NA),
      legend.text = ggplot2::element_text(color = "black", size = 12)
    )
  cat(crayon::blue$bold("Creating summary chart...\n"))
  bar_plot <- ggplot2::ggplot(national_summary,
                              ggplot2::aes(
                                x = `Travel time`,
                                y = (`Percentage of the population` + 24)/3,
                                fill = `Travel time`
                              )) +
    ggplot2::geom_col(show.legend = F) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        ">120" = "#08306b",
        "91-120" = "#2171b5",
        "61-90" = "#6baed6",
        "31-60" = "#c6dbef",
        "<=30" = "#f7fbff"
      ),
      na.value = "#d9d9d9"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(`Percentage of the population`, 1), "%"), y = 15),
      size = 4.5,
      position = ggplot2::position_dodge(width = 0.9),
      hjust = 0,
      color = "black"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(color = "black", size = 12),
      plot.margin = ggplot2::margin(t = 5,
                                    r = 0,
                                    b = 0,
                                    l = 10)
    )
  cat(crayon::blue$bold("Combining plots...\n"))
  combined_plot <- cowplot::ggdraw() +
    cowplot::draw_plot(map_plot,x = 0,y = 0,height = 1,width = 0.75) +
    cowplot::draw_plot(
      bar_plot,
      x = 0.6,
      y = 0.3,
      hjust = 0,
      vjust = 0,
      width = 0.28,
      height = 0.42
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(), # optional - remove gridlines
      panel.grid.minor = ggplot2::element_blank(), # optional - remove gridlines
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      legend.background = ggplot2::element_rect(fill = 'transparent'),
      legend.box.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      legend.key = ggplot2::element_rect(fill = "transparent"), # remove grey background color of the key
      legend.margin =  ggplot2::margin(0,0,0,0,unit = "pt")
    )
  cat(crayon::blue$bold("Saving outputs...\n"))
  ggplot2::ggsave(
    plot = combined_plot,
    filename = file.path(path, paste0(name, '_combined_plot.png')),
    dpi = saving_list$dpi,
    width = 2 * saving_list$width * 5,
    height = saving_list$height * 5,
    units = "in",
    bg = NULL
  )
  ggplot2::ggsave(
    plot = map_plot,
    filename = file.path(path, paste0(name, '_map_plot.png')),
    dpi = saving_list$dpi,
    width = saving_list$width * 5,
    height = saving_list$height * 5,
    units = "in",
    bg = NULL
  )
  ggplot2::ggsave(
    plot = bar_plot,
    filename = file.path(path, paste0(name, '_bar_plot.png')),
    dpi = saving_list$dpi,
    width = saving_list$width * 5,
    height = saving_list$height * 5,
    units = "in",
    bg = NULL
  )
  if (!is.null(name)) {
    data.table::fwrite(national_summary, file = file.path(path, paste0(name, '_national_summary.csv')))
  } else {
    data.table::fwrite(national_summary, file = file.path(path, 'national_summary.csv'))
  }
  cat(crayon::blue$bold("Files saved to:", path, "\n"))
  return(
    list(
      national_summary = national_summary,
      map_plot = map_plot,
      bar_plot = bar_plot,
      combined_plot = combined_plot,
      access = access,
      temp_df = temp_df
    )
  )
}
