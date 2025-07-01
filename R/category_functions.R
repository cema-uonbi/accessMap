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
#' @param palette Character name of color palette ("viridis", "plasma", "blues", etc.)
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
                     "viridis" = viridis::viridis(length(labels), direction = direction),
                     "plasma" = viridis::plasma(length(labels), direction = direction),
                     "inferno" = viridis::inferno(length(labels), direction = direction),
                     "magma" = viridis::magma(length(labels), direction = direction),
                     "cividis" = viridis::cividis(length(labels), direction = direction),
                     "blues" = {
                       cols <- RColorBrewer::brewer.pal(min(length(labels), 9), "Blues")
                       if (direction == -1) rev(cols) else cols
                     },
                     "reds" = {
                       cols <- RColorBrewer::brewer.pal(min(length(labels), 9), "Reds")
                       if (direction == -1) rev(cols) else cols
                     },
                     "spectral" = {
                       cols <- RColorBrewer::brewer.pal(min(length(labels), 11), "Spectral")
                       if (direction == -1) rev(cols) else cols
                     },
                     "greens" = {
                       cols <- RColorBrewer::brewer.pal(min(length(labels), 9), "Greens")
                       if (direction == -1) rev(cols) else cols
                     },
                     "oranges" = {
                       cols <- RColorBrewer::brewer.pal(min(length(labels), 9), "Oranges")
                       if (direction == -1) rev(cols) else cols
                     },
                     "purples" = {
                       cols <- RColorBrewer::brewer.pal(min(length(labels), 9), "Purples")
                       if (direction == -1) rev(cols) else cols
                     },
                     {
                       cols <- rainbow(length(labels))
                       if (direction == -1) rev(cols) else cols
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
