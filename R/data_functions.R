#' Load Default Kenya Spatial Data
#'
#' @description
#' Loads default spatial datasets included with accessMap package.
#' Includes population data, transition matrix, and administrative boundaries.
#'
#' @return List containing:
#' \describe{
#'   \item{populationRaster}{WorldPop 2020 population raster for Kenya (https://hub.worldpop.org/geodata/summary?id=49694)}
#'   \item{transitionMatrix}{Motorized transport transition matrix (https://malariaatlas.org/project-resources/accessibility-to-healthcare/)}
#'   \item{adm0}{Kenya country boundary (sf object : https://gadm.org/download_country.html)}
#'   \item{adm1}{Kenya county boundaries (sf object : https://gadm.org/download_country.html)}
#' }
#'
#' @examples
#' # Load all default data
#' kenya_data <- load_default_data()
#'
#' # Use specific components
#' facility_coords <- matrix(c(36.8, -1.3, 37.0, -1.5), ncol = 2, byrow = TRUE)
#' result <- accessibility(
#'   points = facility_coords,
#'   transitionMatrix = kenya_data$transitionMatrix,
#'   populationRaster = kenya_data$populationRaster
#' )
#'
#' @export
load_default_data <- function() {
  list(
    populationRaster = accessMap::population_raster,
    transitionMatrix = accessMap::transition_matrix,
    adm0 = accessMap::adm0,
    adm1 = accessMap::adm1
  )
}

#' Get Kenya Population Raster
#' @description Returns the WorldPop 2020 population raster for Kenya
#' @return RasterLayer object
#' @export
get_population_raster <- function() {
  accessMap::population_raster
}

#' Get Kenya Transition Matrix
#' @description Returns the motorized transport transition matrix for Kenya
#' @return Transition matrix object
#' @export
get_transition_matrix <- function() {
  accessMap::transition_matrix
}

#' Get Kenya Administrative Boundaries
#' @description Returns Kenya administrative boundary data
#' @param level Administrative level (0 = country, 1 = county, 2 = subcounty, 3 = ward)
#' @return sf object with administrative boundaries
#' @examples
#' # Get country boundary
#' country <- get_admin_boundaries(0)
#'
#' # Get county boundaries
#' counties <- get_admin_boundaries(1)
#' @export
get_admin_boundaries <- function(level = 1) {
  switch(as.character(level),
         "0" = accessMap::adm0,
         "1" = accessMap::adm1,
         "2" = accessMap::adm2,
         "3" = accessMap::adm3,
         stop("Level must be 0, 1, 2, or 3")
  )
}

#' Validate Point Coordinates
#'
#' @description
#' Validates and standardizes facility coordinate input for accessibility analysis.
#' Handles various input formats and automatically detects coordinate columns.
#'
#' @param points Matrix, data.frame, or sf object with facility coordinates
#' @param lng_col Character name of longitude column (default: "lng").
#'   If NULL, auto-detects from common longitude column names.
#' @param lat_col Character name of latitude column (default: "lat").
#'   If NULL, auto-detects from common latitude column names.
#'
#' @return Matrix with standardized lng/lat columns
#'
#' @details
#' The function handles multiple input formats:
#'
#' **For sf objects**: Extracts coordinates using st_coordinates()
#'
#' **For data.frames**:
#' - If lng_col and lat_col are specified, uses those columns
#' - If lng_col/lat_col are NULL, auto-detects coordinate columns from common names
#' - If data.frame has exactly 2 columns, assumes first = longitude, second = latitude
#'
#' **For matrices**: Assumes first column = longitude, second column = latitude
#'
#' **Auto-detection searches for these column name patterns:**
#' - Longitude: "lng", "lon", "long", "longitude", "x", "X"
#' - Latitude: "lat", "latitude", "y", "Y"
#'
#' @examples
#' # From matrix (assumes lng, lat order)
#' coords_matrix <- matrix(c(36.8, -1.3, 37.0, -1.5), ncol = 2, byrow = TRUE)
#' points <- validate_points(coords_matrix)
#'
#' # From data.frame with standard names
#' coords_df <- data.frame(lng = c(36.8, 37.0), lat = c(-1.3, -1.5))
#' points <- validate_points(coords_df)
#'
#' # From data.frame with custom names
#' coords_df <- data.frame(longitude = c(36.8, 37.0), latitude = c(-1.3, -1.5))
#' points <- validate_points(coords_df, lng_col = "longitude", lat_col = "latitude")
#'
#' # Auto-detection
#' coords_df <- data.frame(lon = c(36.8, 37.0), lat = c(-1.3, -1.5))
#' points <- validate_points(coords_df, lng_col = NULL, lat_col = NULL)
#'
#' # Two-column data.frame (assumes lng, lat order)
#' coords_df <- data.frame(c(36.8, 37.0), c(-1.3, -1.5))
#' points <- validate_points(coords_df)
#'
#' @export
validate_points <- function(points, lng_col = NULL, lat_col = NULL) {
  if (inherits(points, "sf")) {
    coords <- sf::st_coordinates(points)
    points <- matrix(coords[, 1:2], ncol = 2)
    colnames(points) <- c("lng", "lat")
  } else if (inherits(points, "data.frame")) {
    if (ncol(points) == 2) {
      if (is.null(lng_col) && is.null(lat_col)) {
        points <- as.matrix(points[, 1:2])
        colnames(points) <- c("lng", "lat")
      } else if (!is.null(lng_col) && !is.null(lat_col)) {
        if (!all(c(lng_col, lat_col) %in% names(points))) {
          stop("Columns '", lng_col, "' and '", lat_col, "' not found in data")
        }
        points <- as.matrix(points[, c(lng_col, lat_col)])
        colnames(points) <- c("lng", "lat")
      } else {
        points <- .auto_detect_coords(points, lng_col, lat_col)
      }
    } else if (ncol(points) > 2) {
      if (!is.null(lng_col) && !is.null(lat_col)) {
        if (!all(c(lng_col, lat_col) %in% names(points))) {
          stop("Columns '", lng_col, "' and '", lat_col, "' not found in data")
        }
        points <- as.matrix(points[, c(lng_col, lat_col)])
        colnames(points) <- c("lng", "lat")
      } else {
        points <- .auto_detect_coords(points, lng_col, lat_col)
      }
    } else {
      stop("Data.frame must have at least 2 columns for coordinates")
    }
  } else {
    if (!inherits(points, "matrix")) {
      points <- as.matrix(points)
    }

    if (ncol(points) != 2) {
      stop("Points must have exactly 2 columns (longitude, latitude)")
    }
    colnames(points) <- c("lng", "lat")
  }
  if (any(abs(points[, "lng"]) > 180, na.rm = TRUE)) {
    stop("Longitude values must be between -180 and 180")
  }
  if (any(abs(points[, "lat"]) > 90, na.rm = TRUE)) {
    stop("Latitude values must be between -90 and 90")
  }
  if (any(is.na(points))) {
    warning("Missing coordinate values detected")
  }
  return(points)
}

#' Auto-detect coordinate columns in data.frame
#' @param df Data.frame with coordinates
#' @param lng_col Specified longitude column (can be NULL)
#' @param lat_col Specified latitude column (can be NULL)
#' @noRd
.auto_detect_coords <- function(df, lng_col = NULL, lat_col = NULL) {
  lng_patterns <- c("lng", "lon", "long", "longitude", "x", "X", "eastings", "easting")
  lat_patterns <- c("lat", "latitude", "y", "Y", "northings", "northing")
  if(inherits(df, "data.table")) {
    df <- as.data.frame(df)
  }
  col_names <- names(df)

  if (is.null(lng_col)) {
    lng_matches <- col_names[tolower(col_names) %in% tolower(lng_patterns)]
    if (length(lng_matches) == 0) {
      stop(
        "Could not auto-detect longitude column. Please specify 'lng_col'.\n",
        .format_column_list(col_names, "Available columns")
      )
    } else if (length(lng_matches) > 1) {
      lng_col <- lng_matches[1]
      warning(
        "Multiple longitude columns found: ",
        paste(lng_matches, collapse = ", "),
        ". Using: ",
        lng_col
      )
    } else {
      lng_col <- lng_matches[1]
    }
  }

  if (is.null(lat_col)) {
    lat_matches <- col_names[tolower(col_names) %in% tolower(lat_patterns)]
    if (length(lat_matches) == 0) {
      stop(
        "Could not auto-detect latitude column. Please specify 'lat_col'.\n",
        .format_column_list(col_names, "Available columns")
      )
    } else if (length(lat_matches) > 1) {
      lat_col <- lat_matches[1]
      warning(
        "Multiple latitude columns found: ",
        paste(lat_matches, collapse = ", "),
        ". Using: ",
        lat_col
      )
    } else {
      lat_col <- lat_matches[1]
    }
  }

  if (!lng_col %in% col_names) {
    stop("Longitude column '", lng_col, "' not found in data")
  }
  if (!lat_col %in% col_names) {
    stop("Latitude column '", lat_col, "' not found in data")
  }
  points <- as.matrix(df[, c(lng_col, lat_col)])
  colnames(points) <- c("lng", "lat")

  return(points)
}


#' Format column list for user-friendly display
#' @param columns Character vector of column names
#' @param label Label for the column list (e.g., "Available columns")
#' @param max_show Maximum number of columns to show before truncating
#' @param inline Logical, whether to format for inline display (warnings)
#' @noRd
.format_column_list <- function(columns, label = "Columns", max_show = 10, inline = FALSE) {

  n_cols <- length(columns)

  if (n_cols == 0) {
    return(paste0(label, ": (none)"))
  }

  if (n_cols <= max_show) {
    # Show all columns
    if (inline) {
      return(paste(columns, collapse = ", "))
    } else {
      return(paste0(label, ": ", paste(columns, collapse = ", ")))
    }
  } else {
    # Truncate and show expandable message
    shown_cols <- columns[1:max_show]
    hidden_count <- n_cols - max_show

    if (inline) {
      return(paste0(paste(shown_cols, collapse = ", "),
                    " ... and ", hidden_count, " more"))
    } else {
      msg <- paste0(
        label, " (", n_cols, " total): ",
        paste(shown_cols, collapse = ", "),
        "... and ", hidden_count, " more columns."
      )
      return(msg)
    }
  }
}

