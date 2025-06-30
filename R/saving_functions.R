#' Save Accessibility Outputs
#'
#' @description
#' Saves plots and summary data to specified directory with consistent naming.
#'
#' @param plots List with map_plot, bar_plot, combined_plot
#' @param summary_dt Summary statistics data.table
#' @param path Output directory path
#' @param name File prefix for outputs
#' @param save_opoints List with dpi, width, height, units for saving
#' @param formats Character vector of plot formats to save ("png", "pdf", "svg")
#'
#' @return Invisible list of saved file paths
#'
#' @examples
#' \dontrun{
#' # Save all outputs
#' plots <- list(
#'   map_plot = map,
#'   bar_plot = bar,
#'   combined_plot = combined
#' )
#'
#' save_outputs(plots, summary_stats, path = "./results",
#'              name = "health_access")
#'
#' # High resolution outputs
#' save_outputs(plots, summary_stats,
#'              save_opoints = list(dpi = 2000, width = 2, height = 2, units = "in"))
#'
#' # Multiple formats
#' save_outputs(plots, summary_stats, formats = c("png", "pdf"))
#' }
#'
#' @export
save_outputs <- function(plots,
                         summary_dt,
                         path = ".",
                         name = NULL,
                         save_opoints = list(
                           dpi = 1000,
                           width = 1,
                           height = 1,
                           units = "in"
                         ),
                         formats = "png") {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  prefix <- if (is.null(name))
    ""
  else
    paste0(name, "_")
  
  saved_files <- list()
  plot_names <- c("combined", "map", "bar")
  plot_objects <- list(plots$combined_plot, plots$map_plot, plots$bar_plot)
  for (fmt in formats) {
    purrr::walk2(plot_objects, plot_names, ~ {
      filename <- file.path(path, paste0(prefix, .y, "_plot.", fmt))
      
      ggplot2::ggsave(
        plot = .x,
        filename = filename,
        dpi = save_opoints$dpi,
        width = save_opoints$width,
        height = save_opoints$height,
        units = save_opoints$units,
        bg = NULL
      )
      saved_files[[paste0(.y, "_", fmt)]] <<- filename
    })
  }
  csv_file <- file.path(path, paste0(prefix, "summary.csv"))
  data.table::fwrite(summary_dt, file = csv_file)
  saved_files$summary <- csv_file
  cat(crayon::green("Files saved to:", path, "\n"))
  invisible(saved_files)
}
