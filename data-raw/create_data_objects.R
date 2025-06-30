# data-raw/process_data.R
# NOTE: This script reads from inst/extdata which will be removed
# Before running, either:
# 1. Move raw files to a local directory and update paths below
# 2. Download files from original sources
# This script is only run during development, not by end users

adm0 <- sf::st_read(system.file("extdata", "gadm41_KEN_0.shp", package = "accessMap"), 
                    quiet = TRUE)

adm1 <- sf::st_read(system.file("extdata", "gadm41_KEN_1.shp", package = "accessMap"), 
                    quiet = TRUE)
adm2 <- sf::st_read(system.file("extdata", "gadm41_KEN_2.shp", package = "accessMap"), 
                    quiet = TRUE)
ad3 <- sf::st_read(system.file("extdata", "gadm41_KEN_3.shp", package = "accessMap"), 
                    quiet = TRUE)

population_raster <- raster::raster(
  system.file("extdata", "ken_ppp_2020_UNadj_constrained.tif", package = "accessMap")
)

transition_matrix <- readRDS(
  system.file("extdata", "kenya_transition_matrix.RDS", package = "accessMap")
)

# Save as package data
usethis::use_data(adm0, compress = "xz", overwrite = TRUE)
usethis::use_data(adm1, compress = "xz", overwrite = TRUE) 
usethis::use_data(adm2, compress = "xz", overwrite = TRUE) 
usethis::use_data(adm3, compress = "xz", overwrite = TRUE) 
usethis::use_data(population_raster, compress = "xz", overwrite = TRUE)
usethis::use_data(transition_matrix, compress = "xz", overwrite = TRUE)