#' Create Accessibility Map
#'
#' @description
#' Creates a spatial map showing travel time accessibility to facilities.
#'
#' @param data data.table with spatial accessibility data
#' @param points Matrix or data.frame with facility coordinates (must have lng, lat columns)
#' @param adm0 Country/study area boundary (sf object, optional)
#' @param adm1 Administrative subdivision boundaries (sf object, optional)
#' @param categories Travel time categories with colors
#' @param label Character label for facilities in legend
#' @param show_admin Logical, whether to show administrative boundaries
#' @param show_scale Logical, whether to show scale bar and north arrow
#' @param point_color Character color for points, or column name in points data for color mapping
#' @param point_shape Numeric shape code, character shape, or column name in points data for shape mapping
#' @param point_size Numeric size for points, or column name in points data for size mapping
#' @param point_colors Named vector of colors for categorical color mapping (when point_color is column name)
#' @param point_shapes Named vector of shapes for categorical shape mapping (when point_shape is column name)
#' @param show_point_legend Logical, whether to show legend for points (default: TRUE)
#'
#' @return ggplot2 object
#'
#' @details
#' **Point Styling Options:**
#'
#' **Fixed styling**: Provide single values for color, shape, size
#'
#' **Variable styling**: Provide column names from points data to map aesthetics
#'
#' **Color mapping**: When point_color is a column name, use point_colors to specify colors
#'
#' **Shape mapping**: When point_shape is a column name, use point_shapes to specify shapes
#'
#' **Available shapes**: Numbers 0-25, or characters "+", "x", "*", etc.
#'
#' @examples
#' \dontrun{
#' # Basic map with default red "+" points
#' facility_coords <- points
#' map1 <- create_map(access_dt, facility_coords, adm0, adm1, cats)
#'
#' # Custom point color and shape
#' map2 <- create_map(
#'   access_dt, facility_coords, adm0, adm1, cats,
#'   point_color = "blue",
#'   point_shape = 17,  # Triangle
#'   point_size = 6
#' )
#'
#' # Color points by facility type
#' facilities_df <- data.frame(
#'   lng = c(36.8, 37.0),
#'   lat = c(-1.3, -1.5),
#'   type = c("Hospital", "Clinic")
#'   )
#'
#' map3 <- create_map(
#'   access_dt, points = facilities_df, adm0, adm1, cats,
#'   point_color = "type",
#'   point_colors = c("Hospital" = "red", "Clinic" = "blue"),
#'   label = "Health Facilities"
#' )
#'
#' # Shape points by ownership and size by capacity
#' facilities_df <- data.frame(
#'   lng = c(36.8, 37.0, 36.9),
#'   lat = c(-1.3, -1.5, -1.4),
#'   ownership = c("Public", "Private", "Public"),
#'   capacity = c(100, 50, 75)
#' )
#'
#' map4 <- create_map(
#'   access_dt, facilities_df, adm0, adm1, cats,
#'   point_color = "darkgreen",
#'   point_shape = "ownership",
#'   point_size = "capacity",
#'   point_shapes = c("Public" = 16, "Private" = 17)
#' )
#'
#' # Multiple aesthetics mapped
#' map5 <- create_map(
#'  access_dt,
#'  facilities_df,
#'  adm0,
#'  adm1,
#'  cats,
#'  point_color = "ownership",
#'  point_shape = "ownership",
#'  point_size = "capacity",
#'  point_colors = c("Public" = "red", "Private" = "blue"),
#'  point_shapes = c("Public" = 16, "Private" = 17)
#')
#' }
#'
#' @export
create_map <- function(data,
                       points,
                       adm0 = NULL,
                       adm1 = NULL,
                       categories,
                       label = "facilities",
                       show_admin = TRUE,
                       show_scale = TRUE,
                       point_color = "red",
                       point_shape = "+",
                       point_size = 4.5,
                       point_colors = NULL,
                       point_shapes = NULL,
                       show_point_legend = TRUE) {
  if (inherits(points, "matrix")) {
    points <- data.table::as.data.table(points)
  } else {
    points <- data.table::as.data.table(points)
  }

  if (!all(c("lng", "lat") %in% names(points))) {
    stop("Points data must contain 'lng' and 'lat' columns")
  }

  p <- ggplot2::ggplot()
  if (!is.null(adm0)) {
    map_crs <- sf::st_crs(adm0)
  } else if (!is.null(adm1)) {
    map_crs <- sf::st_crs(adm1)
  } else {
    # No admin boundaries - assume WGS84 from coordinate range
    map_crs <- sf::st_crs(4326)
  }
  p <- p + ggplot2::coord_sf(crs = map_crs, expand = FALSE)
  p <- p + ggplot2::geom_raster(
    data = data,
    ggplot2::aes(x = x, y = y, fill = cat_travel_time),
    interpolate = FALSE,
    show.legend = FALSE
  ) +
    ggplot2::scale_fill_manual(
      values = categories$colors,
      guide = "none",
      na.value = "#969696"
    )
  if (show_admin && !is.null(adm1)) {
    suppressMessages({
      p <- p + ggplot2::geom_sf(
        data = adm1,
        fill = NA,
        linewidth = 0.1,
        color = "black"
      )
    })
  }
  if (show_scale) {
    p <- p +
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
      )
  }
  p <- .add_facility_points(
    p,
    points,
    point_color,
    point_shape,
    point_size,
    point_colors,
    point_shapes,
    label,
    show_point_legend
  )
  p <- p +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key = ggplot2::element_rect(color = "black", fill = NA),
      legend.text = ggplot2::element_text(color = "black", size = 12),
      legend.box = "horizontal"
    )
  if (.is_column_name(point_color, points) &&
      .is_column_name(point_shape, points) &&
      point_color == point_shape) {
    p <- p + ggplot2::guides(
      color = ggplot2::guide_legend(
        title = label,
        override.aes = list(
          size = 4,
          # Ensure the legend uses the actual values from the scales
          shape = if (!is.null(point_shapes)) unname(point_shapes) else c(16, 17),
          color = if (!is.null(point_colors)) unname(point_colors) else c("red", "blue")
        )
      ),
      shape = "none"
    )
  }
  return(p)
}

#' Add facility points to map with flexible styling
#' @param p ggplot object
#' @param points data.table with point coordinates and attributes
#' @param point_color Color specification (fixed or column name)
#' @param point_shape Shape specification (fixed or column name)
#' @param point_size Size specification (fixed or column name)
#' @param point_colors Named vector for color mapping
#' @param point_shapes Named vector for shape mapping
#' @param label Legend label
#' @param show_legend Whether to show point legend
#' @noRd
.add_facility_points <- function(p,
                                 points,
                                 point_color,
                                 point_shape,
                                 point_size,
                                 point_colors,
                                 point_shapes,
                                 label,
                                 show_legend) {
  color_mapped <- .is_column_name(point_color, points)
  shape_mapped <- .is_column_name(point_shape, points)
  size_mapped <- .is_column_name(point_size, points)

  if (color_mapped || shape_mapped || size_mapped) {
    aes_mapping <- ggplot2::aes(x = lng, y = lat)
    if (color_mapped) {
      aes_mapping$colour <- rlang::sym(point_color)
    }
    if (shape_mapped) {
      aes_mapping$shape <- rlang::sym(point_shape)
    }
    if (size_mapped) {
      aes_mapping$size <- rlang::sym(point_size)
    }
    layer_args <- list(data = points, mapping = aes_mapping)
    if (!color_mapped)
      layer_args$color <- point_color
    if (!shape_mapped)
      layer_args$shape <- point_shape
    if (!size_mapped)
      layer_args$size <- point_size
    p <- p + do.call(ggplot2::geom_point, layer_args)
    if (color_mapped) {
      if (!is.null(point_colors)) {
        p <- p + ggplot2::scale_color_manual(
          name = label,
          values = point_colors,
          guide = if (show_legend)
            ggplot2::guide_legend()
          else
            "none"
        )
      } else {
        p <- p + ggplot2::scale_color_discrete(name = label,
                                               guide = if (show_legend)
                                                 ggplot2::guide_legend()
                                               else
                                                 "none")
      }
    }
    if (shape_mapped) {
      shape_name <- if (!color_mapped)
        label
      else
        ggplot2::waiver()
      shape_guide <- if (show_legend &&
                         !color_mapped)
        ggplot2::guide_legend()
      else
        "none"

      if (!is.null(point_shapes)) {
        p <- p + ggplot2::scale_shape_manual(name = shape_name,
                                             values = point_shapes,
                                             guide = shape_guide)
      } else {
        p <- p + ggplot2::scale_shape_discrete(name = shape_name, guide = shape_guide)
      }
    }
    if (size_mapped) {
      size_name <- if (!color_mapped && !shape_mapped)
        label
      else
        "Size"
      size_guide <- if (show_legend &&
                        !color_mapped && !shape_mapped)
        ggplot2::guide_legend()
      else
        "none"

      p <- p + ggplot2::scale_size_continuous(name = size_name, guide = size_guide)
    }

  } else {
    p <- p + ggplot2::geom_point(
      data = points,
      ggplot2::aes(x = lng, y = lat, color = "facilities"),
      size = point_size,
      shape = point_shape
    ) +
      ggplot2::scale_color_manual(
        name = " ",
        values = c("facilities" = point_color),
        labels = label,
        guide = if (show_legend)
          ggplot2::guide_legend()
        else
          "none"
      )
  }

  return(p)
}

#' Check if parameter refers to a column name
#' @param param Parameter value
#' @param dt data.table to check against
#' @noRd
.is_column_name <- function(param, dt) {
  is.character(param) && length(param) == 1 && param %in% names(dt)
}

#' Create Summary Bar Chart
#'
#' @description
#' Creates a horizontal bar chart showing population distribution by travel time category.
#'
#' @param summary_dt Summary statistics data.table
#' @param categories Travel time categories with colors
#' @param show_labels Logical, whether to show percentage labels on bars
#' @param color_labels Color for the bar plot labels
#' @return ggplot2 object
#'
#' @examples
#' \dontrun{
#' # Create bar chart
#' bar_chart <- create_bar_plot(summary_stats, cats)
#' print(bar_chart)
#'
#' # Without percentage labels
#' bar_chart <- create_bar_plot(summary_stats, cats, show_labels = FALSE)
#' }
#'
#' @export
create_bar_plot <- function(summary_dt,
                            categories,
                            show_labels = TRUE,
                            color_labels = NULL) {
  if (is.null(color_labels)) {
    color_labels <- "black"
  } else {
    color_labels <- color_labels
  }
  p <- ggplot2::ggplot(summary_dt,
                       ggplot2::aes(
                         x = travel_time,
                         y = (percentage + 24) / 3,
                         fill = travel_time
                       )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = categories$colors, na.value = "#d9d9d9") +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(color = "black", size = 12),
      plot.margin = ggplot2::margin(
        t = 5,
        r = 0,
        b = 0,
        l = 10
      )
    )
  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(percentage, 1), "%"), y = 15),
      size = 4.5,
      hjust = 0,
      color = color_labels
    )
  }
  return(p)
}

#' Combine Map and Bar Chart
#'
#' @description
#' Creates a combined visualization with accessibility map and summary bar chart.
#'
#' @param map_plot ggplot2 map object (from create_map())
#' @param bar_plot ggplot2 bar chart object (from create_bar_plot())
#' @param map_width Proportion of width for map (default: 0.75)
#' @param bar_position List with x, y, width, height for bar chart placement
#'
#' @return ggplot2 object with combined plots
#'
#' @examples
#' \dontrun{
#' # Create individual plots
#' map <- create_map(access_dt, points, adm0, adm1, cats)
#' bar <- create_bar_plot(summary_stats, cats)
#'
#' # Combine plots
#' combined <- combine_plots(map, bar)
#' print(combined)
#'
#' # Custom positioning
#' combined <- combine_plots(
#'   map, bar,
#'   bar_position = list(x = 0.7, y = 0.4, width = 0.25, height = 0.35)
#' )
#' }
#'
#' @export
combine_plots <- function(map_plot,
                          bar_plot,
                          map_width = 0.75,
                          bar_position = list(
                            x = 0.6,
                            y = 0.3,
                            width = 0.28,
                            height = 0.42
                          )) {
  combined <- cowplot::ggdraw() +
    cowplot::draw_plot(
      map_plot,
      x = 0,
      y = 0,
      height = 1,
      width = map_width
    ) +
    cowplot::draw_plot(
      bar_plot,
      x = bar_position$x,
      y = bar_position$y,
      width = bar_position$width,
      height = bar_position$height
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      legend.background = ggplot2::element_rect(fill = 'transparent'),
      legend.box.background = ggplot2::element_rect(fill = 'transparent', color = NA)
    )
  return(combined)
}
