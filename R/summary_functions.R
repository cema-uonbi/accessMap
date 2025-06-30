#' Generate Summary Statistics
#'
#' @description
#' Calculates population counts and percentages by travel time category.
#'
#' @param data data.table with accessibility data (from process_access_data())
#' @param categories Travel time categories for factor level ordering
#' @param progress Logical, show progress messages (default: TRUE)
#'
#' @return data.table with columns: travel_time, population, percentage
#'
#' @examples
#' \dontrun{
#' # After processing accessibility data
#' summary_stats <- generate_summary(access_dt, cats)
#' print(summary_stats)
#'
#' # Calculate total population within 30 minutes
#' close_pop <- summary_stats[travel_time == "<=30", population]
#'
#' }
#'
#' @export
generate_summary <- function(data, categories, progress = TRUE) {
  if (!inherits(data, "data.table")) {
    data <- data.table::data.table(data)
  }
  if (progress) {
    cat(crayon::blue$bold("Generating summary statistics...\n"))
  }
  summary_dt <- data.table::copy(
    data[, .(pop = sum(pop, na.rm = TRUE)),
            by = cat_travel_time]
  )[, percent := pop / sum(pop, na.rm = TRUE) * 100
  ][, data.table::setnames(.SD,
                           c("travel_time", "population", "percentage"))
  ][, travel_time := factor(travel_time,
                            levels = rev(categories$labels))][]

  return(summary_dt)
}

#' Generate Summary Statistics by Administrative Units
#'
#' @description
#' Calculates accessibility summary statistics aggregated by administrative units
#' (e.g., counties, subcounties, districts). Returns population counts and percentages
#' by travel time category for each administrative unit.
#'
#' @param data data.table with accessibility data (from process_access_data())
#' @param admin_sf sf object with administrative boundaries
#' @param admin_name_cols Character vector of column names containing admin unit names
#'   (default: "NAME_1"). Can specify multiple levels like c("county", "subcounty").
#' @param admin_code_cols Character vector of column names containing admin unit codes
#'   (optional). Should correspond to admin_name_cols if provided.
#' @param progress Logical, show progress messages (default: TRUE)
#'
#' @return data.table with columns: admin_name, admin_code (if provided),
#'   travel_time, population, percentage, total_pop
#'
#' @details
#' The function performs spatial overlay of accessibility grid points with
#' administrative boundaries to assign each population cell to an admin unit.
#' It then aggregates by both admin unit and travel time category.
#'
#' **Output Columns:**
#' - **Admin columns**: Named according to admin_name_cols and admin_code_cols parameters
#'   (e.g., NAME_1, COUNTY_NAME, etc.) - identifies the administrative unit
#' - **cat_travel_time**: Factor with travel time categories (e.g., "<=30", "31-60", ">120")
#'   - represents travel time ranges in minutes to nearest facility
#' - **total_pop**: Total population within the administrative unit across all travel times
#'   - same value repeated for all travel time categories within each admin unit
#' - **population**: Population count within this admin unit for this specific travel time category
#'   - sum of population in grid cells that fall within the travel time range
#' - **percentage**: Percentage of admin unit's total population in this travel time category
#'   - calculated as (population / total_pop) * 100
#'
#' **Note**: For large data sets, this function may take several minutes due to
#' spatial overlay operations. Consider using smaller sample areas for testing.
#'
#' @examples
#' \dontrun{
#' # Basic usage with county boundaries
#' data <- load_default_data()
#' pts <- validate_points(facility_coords)
#' travel_results <- compute_travel_cost(pts, data$transitionMatrix, data$populationRaster)
#' cats <- default_categories()
#' access_dt <- process_access_data(travel_results$access, travel_results$pop, cats)
#' # Generate county-level summary
#' county_summary <- generate_admin_summary(
#'   data = access_dt,
#'   admin_sf = data$adm1,
#'   categories = cats$labels,
#'   admin_name_col = "NAME_1"
#' )
#' # View results
#' print(county_summary)
#' }
#'
#' @export
generate_admin_summary <- function(data, admin_sf,
                                   admin_name_cols = "NAME_1",
                                   admin_code_cols = NULL,
                                   progress = TRUE) {

  if (!inherits(data, "data.table")) {
    data <- data.table::as.data.table(data)
  }
  if (!inherits(admin_sf, "sf")) {
    stop("admin_sf must be an sf object")
  }
  if (length(setdiff(admin_name_cols, names(admin_sf))) > 0) {
    temp <- setdiff(admin_name_cols, names(admin_sf))
    if (length(temp) > 1) {
      stop("Columns ", paste(temp, collapse = ", "), " not found in admin_sf")
    } else {
      stop("Column ", temp, " not found in admin_sf")
    }
  }
  if (!is.null(admin_code_cols) &&
      length(setdiff(admin_code_cols, names(admin_sf))) > 0) {
    temp <- setdiff(admin_code_cols, names(admin_sf))
    if (length(temp) > 1) {
      stop("Columns ", paste(temp, collapse = ", "), " not found in admin_sf")
    } else {
      stop("Column ", temp, " not found in admin_sf")
    }
  }
  required_cols <- c("x", "y", "pop", "travel_time", "cat_travel_time")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  if (progress) {
    cat(crayon::blue$bold("Generating admin-level summary statistics...\n"))
    cat(crayon::yellow("Note: Spatial overlay may take several minutes for large datasets\n"))
  }

  access_sf <- sf::st_as_sf(data, coords = c("x", "y"), crs = sf::st_crs(admin_sf))

  if (progress) {
    cat(crayon::blue(paste("Processing", nrow(access_sf), "points against",
                           nrow(admin_sf), "admin units...\n")))
  }

  admin_cols <- c(admin_name_cols)
  if (!is.null(admin_code_cols)) {
    admin_cols <- c(admin_cols, admin_code_cols)
  }

  admin_subset <- admin_sf[, admin_cols]
  overlay_result <- sf::st_join(access_sf, admin_subset, join = sf::st_within)
  overlay_dt <- data.table::as.data.table(overlay_result)
  overlay_dt[, geometry := NULL]

  if (length(admin_name_cols) == 1) {
    points_outside <- sum(is.na(overlay_dt[[admin_name_cols[1]]]))
  } else {
    points_outside <- sum(!complete.cases(overlay_dt[, admin_name_cols, with = FALSE]))
  }

  if (points_outside > 0 && progress) {
    cat(crayon::yellow("Warning:", points_outside,
                       "points fall outside admin boundaries and will be excluded\n"))
  }

  if (length(admin_name_cols) == 1) {
    overlay_dt <- overlay_dt[!is.na(get(admin_name_cols[1]))]
  } else {
    overlay_dt <- overlay_dt[complete.cases(overlay_dt[, admin_name_cols, with = FALSE])]
  }

  if (nrow(overlay_dt) == 0) {
    stop("No accessibility points overlap with admin boundaries. Check CRS alignment.")
  }

  if (progress) {
    cat(crayon::blue("Aggregating statistics by admin unit and travel time...\n"))
  }
  categories <- unique(data$cat_travel_time)
  group_cols <- c(admin_cols, "cat_travel_time")
  admin_summary <- overlay_dt[, .(population = sum(pop, na.rm = TRUE)), by = group_cols]

  admin_totals <- overlay_dt[, .(total_pop = sum(pop, na.rm = TRUE)), by = admin_cols]

  admin_units <- unique(admin_totals[, ..admin_cols])

  complete_combos <- admin_units[, {
    result <- lapply(admin_cols, function(col) rep(get(col), each = length(categories)))
    names(result) <- admin_cols
    result$cat_travel_time <- rep(categories, .N)
    result
  }]

  complete_combos <- merge(complete_combos, admin_totals, by = admin_cols, all.x = TRUE)

  admin_summary <- merge(complete_combos, admin_summary,
                         by = c(admin_cols, "cat_travel_time"), all.x = TRUE)

  admin_summary[is.na(population), population := 0]
  admin_summary[, percentage := population / total_pop * 100]
  admin_summary[is.na(percentage), percentage := 0]
  admin_summary[, cat_travel_time := factor(cat_travel_time, levels = categories)][]

  if (progress) {
    cat(crayon::green("[DONE] Admin-level summary completed\n"))
  }
  return(admin_summary)
}
