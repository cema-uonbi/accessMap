#' Create Default Travel Time Categories
#'
#' @description
#' Creates default travel time categories used in accessibility analysis.
#' Returns the standard 5-category classification.
#'
#' @return List with breaks, labels, and colors for default categories
#'
#' @examples
#' cats <- default_categories()
#' print(cats$labels)
#' print(cats$colors)
#'
#' @export
default_categories <- function() {
  list(
    breaks = c(0, 30, 60, 90, 120, Inf),
    labels = c("<=30", "31-60", "61-90", "91-120", ">120"),
    colors = c(
      "<=30" = "#f7fbff",
      "31-60" = "#c6dbef",
      "61-90" = "#6baed6",
      "91-120" = "#2171b5",
      ">120" = "#08306b"
    )
  )
}

#' Create Custom Travel Time Categories
#'
#' @description
#' Creates standardized category specification for travel time analysis.
#' Validates inputs and generates labels/colors if not provided.
#'
#' @param breaks Numeric vector of break points (must start with 0, end with Inf)
#' @param labels Character vector of category labels (optional - will auto-generate if NULL)
#' @param colors Named character vector of colors for each category
#' @param palette Character name of color palette. Supported options:
#'   \describe{
#'     \item{Viridis palettes}{"viridis", "plasma", "inferno", "magma", "cividis"}
#'     \item{Base R palettes}{"heat.colors", "terrain.colors", "topo.colors", "cm.colors", "rainbow"}
#'     \item{ColorBrewer palettes}{Any valid ColorBrewer palette name (e.g., "GnBu", "RdYlBu", "Set1", "Dark2").
#'     See RColorBrewer::brewer.pal.info for all available options.}
#'   }
#' @param direction Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param label_format Character specifying label format: "range" (default), "threshold", or "ordinal"
#'
#' @return List with validated breaks, labels, and colors
#'
#' @examples
#' # Auto-generate labels (range format)
#' cats <- create_categories(breaks = c(0, 30, 60, 120, Inf))
#' # Labels: "0-30", "30-60", "60-120", ">120"
#'
#' # Custom labels
#' cats <- create_categories(
#'   breaks = c(0, 30, 60, 120, Inf),
#'   labels = c("Close", "Moderate", "Far", "Very far")
#' )
#'
#' # Different label formats
#' cats <- create_categories(
#'   breaks = c(0, 30, 60, 120, Inf),
#'   label_format = "threshold"
#' )
#' # Labels: "<30", "30-60", "60-120", ">120"
#'
#' cats <- create_categories(
#'   breaks = c(0, 30, 60, 120, Inf),
#'   label_format = "ordinal"
#' )
#' # Labels: "Category 1", "Category 2", "Category 3", "Category 4"
#'
#' # Reverse palette direction
#' cats <- create_categories(
#'   breaks = c(0, 30, 60, 120, Inf),
#'   palette = "viridis",
#'   direction = -1
#' )
#'
#' @export
create_categories <- function(breaks, labels = NULL, colors = NULL, palette = NULL,
                              direction = 1, label_format = "range") {
  if (length(breaks) < 2) {
    stop("breaks must have at least 2 elements")
  }

  if (breaks[1] != 0) {
    warning("First break should typically be 0")
  }

  if (!is.infinite(breaks[length(breaks)])) {
    warning("Last break should typically be Inf")
  }

  # Validate direction parameter
  if (!direction %in% c(1, -1)) {
    stop("direction must be 1 or -1")
  }

  if (is.null(labels)) {
    n_categories <- length(breaks) - 1
    labels <- switch(label_format,
                     "range" = {
                       sapply(1:n_categories, function(i) {
                         if (i == n_categories && is.infinite(breaks[i + 1])) {
                           paste0(">", breaks[i])
                         } else {
                           paste0(breaks[i], "-", breaks[i + 1])
                         }
                       })
                     },

                     "threshold" = {
                       sapply(1:n_categories, function(i) {
                         if (i == 1) {
                           paste0("<", breaks[i + 1])
                         } else if (i == n_categories && is.infinite(breaks[i + 1])) {
                           paste0(">=", breaks[i])
                         } else {
                           paste0(breaks[i], "-", breaks[i + 1])
                         }
                       })
                     },

                     "ordinal" = {
                       paste("Category", 1:n_categories)
                     },

                     # Default to range if unknown format
                     {
                       warning("Unknown label_format. Using 'range' format.")
                       sapply(1:n_categories, function(i) {
                         if (i == n_categories && is.infinite(breaks[i + 1])) {
                           paste0(">", breaks[i])
                         } else {
                           paste0(breaks[i], "-", breaks[i + 1])
                         }
                       })
                     }
    )
  }

  # Validate labels length
  if (length(breaks) != length(labels) + 1) {
    stop("breaks must be one element longer than labels")
  }

  # Handle colors and palette
  if (!is.null(colors) && !is.null(palette)) {
    stop("Provide either 'colors' or 'palette', but not both")
  }

  if (!is.null(palette)) {
    colors <- switch(palette,
                     # Viridis palettes
                     "viridis" = viridis::viridis(length(labels), direction = direction),
                     "plasma" = viridis::plasma(length(labels), direction = direction),
                     "inferno" = viridis::inferno(length(labels), direction = direction),
                     "magma" = viridis::magma(length(labels), direction = direction),
                     "cividis" = viridis::cividis(length(labels), direction = direction),

                     # Base R color functions
                     "heat.colors" = {
                       cols <- heat.colors(length(labels))
                       if (direction == -1) rev(cols) else cols
                     },
                     "terrain.colors" = {
                       cols <- terrain.colors(length(labels))
                       if (direction == -1) rev(cols) else cols
                     },
                     "topo.colors" = {
                       cols <- topo.colors(length(labels))
                       if (direction == -1) rev(cols) else cols
                     },
                     "cm.colors" = {
                       cols <- cm.colors(length(labels))
                       if (direction == -1) rev(cols) else cols
                     },
                     "rainbow" = {
                       cols <- rainbow(length(labels))
                       if (direction == -1) rev(cols) else cols
                     },

                     # Default: Try as ColorBrewer palette
                     {
                       # Check if it's a valid ColorBrewer palette
                       if (requireNamespace("RColorBrewer", quietly = TRUE)) {
                         # Get all available ColorBrewer palettes
                         all_palettes <- rownames(RColorBrewer::brewer.pal.info)

                         if (palette %in% all_palettes) {
                           # Get max colors for this palette
                           max_colors <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
                           n_colors <- min(length(labels), max_colors)

                           # Handle case where we need more colors than available
                           if (length(labels) > max_colors) {
                             warning("Palette '", palette, "' only supports ", max_colors,
                                     " colors, but ", length(labels), " are needed. ",
                                     "Using interpolation to generate additional colors.")
                             # Use colorRampPalette for interpolation
                             base_colors <- RColorBrewer::brewer.pal(max_colors, palette)
                             cols <- colorRampPalette(base_colors)(length(labels))
                           } else {
                             # Use the palette directly
                             cols <- RColorBrewer::brewer.pal(max(3, n_colors), palette)[1:length(labels)]
                           }

                           if (direction == -1) rev(cols) else cols
                         } else {
                           warning("Unknown palette '", palette, "'. Available ColorBrewer palettes: ",
                                   paste(all_palettes, collapse = ", "), ". Using rainbow colors.")
                           cols <- rainbow(length(labels))
                           if (direction == -1) rev(cols) else cols
                         }
                       } else {
                         warning("RColorBrewer package not available. Using rainbow colors.")
                         cols <- rainbow(length(labels))
                         if (direction == -1) rev(cols) else cols
                       }
                     }
    )
    names(colors) <- labels
  }

  if (is.null(colors)) {
    n_cats <- length(labels)
    default_colors <- c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b",
                        "#253494", "#081d58", "#2c7fb8", "#41b6c4", "#c7e9b4")
    colors <- rep(default_colors, length.out = n_cats)
    if (direction == -1) colors <- rev(colors)
    names(colors) <- labels
  }

  if (is.null(names(colors))) {
    names(colors) <- labels
  }

  structure(
    list(
      breaks = breaks,
      labels = labels,
      colors = colors,
      label_format = if(is.null(labels)) label_format else "custom",
      direction = direction
    ),
    class = "travel_categories"
  )
}

#' Categorize Travel Times
#'
#' @description
#' Assigns travel time values to predefined categories using cut().
#'
#' @param travel_time Numeric vector of travel times (in minutes)
#' @param categories List with breaks and labels (from create_categories())
#'
#' @return Factor vector with travel time categories
#'
#' @examples
#' # Create categories
#' cats <- create_categories(
#'   breaks = c(0, 30, 60, Inf),
#'   labels = c("Close", "Medium", "Far")
#' )
#'
#' # Categorize some travel times
#' times <- c(15, 45, 75, 25)
#' categorized <- categorize_travel_time(times, cats)
#' print(categorized)
#'
#' @export
categorize_travel_time <- function(travel_time, categories) {
  cut(travel_time,
      breaks = categories$breaks,
      labels = categories$labels,
      include.lowest = TRUE,
      right = FALSE)
}
