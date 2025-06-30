# accessMap

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

An R package for calculating and visualizing spatial accessibility to facilities using cost-distance analysis. The package performs accessibility modeling by computing accumulated travel costs from facility locations to population centers and generates comprehensive visualizations including maps and summary statistics.

## Features

- **Modular design** with individual functions for each analysis step
- **Cost-distance analysis** using realistic travel impedances  
- **Motorized friction surfaces** from the Malaria Atlas Project
- **Flexible point styling** with color, shape, and size mapping
- **Administrative-level summaries** (counties, subcounties, districts)
- **Custom travel time categories** with automatic label generation
- **Publication-ready visualizations** with maps and summary charts
- **Default Kenya datasets** accessible as `accessMap::adm0`, `accessMap::adm1`, etc.
- **Multiple output formats** (PNG, PDF, SVG maps, CSV summaries)

## Installation

You can install the development version of accessMap from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("cema-uonbi/accessMap")
```

Or using devtools:

``` r
# install.packages("devtools")
devtools::install_github("cema-uonbi/accessMap")
```

## Quick Start

### Basic Example with Default Kenya Data

``` r
library(accessMap)

# Define facility coordinates (longitude, latitude)
health_facilities <- matrix(c(
  36.8219, -1.2921,  # Nairobi
  39.6682, -4.0435,  # Mombasa  
  35.2834, 0.5143    # Eldoret
), ncol = 2, byrow = TRUE)
colnames(health_facilities) <- c("lng", "lat")

# Run complete accessibility analysis
result <- accessibility(
  points = health_facilities,
  label = "Health Facilities",
  name = "kenya_health_access"
)

# View summary statistics
print(result$national_summary)

# Display combined map and chart
plot(result$combined_plot)
```

### Using Built-in Data Objects

``` r
# Access Kenya data directly
plot(accessMap::adm1)  # County boundaries
plot(accessMap::population_raster)  # Population distribution

# Use in analysis
result <- accessibility(
  points = health_facilities,
  adm0 = accessMap::adm0,
  adm1 = accessMap::adm1,
  populationRaster = accessMap::population_raster,
  transitionMatrix = accessMap::transition_matrix
)
```

### Custom Travel Time Categories

``` r
# Create custom categories
custom_cats <- create_categories(
  breaks = c(0, 15, 30, 60, Inf),
  labels = c("Immediate", "Quick", "Moderate", "Slow"),
  palette = "viridis"
)

# Auto-generate labels from breaks
auto_cats <- create_categories(
  breaks = c(0, 20, 45, 90, Inf)  # Labels: "0-20", "20-45", "45-90", ">90"
)

result <- accessibility(
  points = facilities,
  categories = custom_cats,
  name = "custom_categories"
)
```

## Advanced Features

### Individual Function Workflow

For full control, use individual functions:

``` r
# Step 1: Load and validate data
kenya_data <- load_default_data()
points <- validate_points(health_facilities)
cats <- default_categories()

# Step 2: Compute accessibility surface
travel_results <- compute_travel_cost(
  points, 
  kenya_data$transitionMatrix, 
  kenya_data$populationRaster
)

# Step 3: Process data
access_data <- process_access_data(
  travel_results$access, 
  travel_results$pop, 
  cats
)

# Step 4: Generate summaries
summary_stats <- generate_summary(access_data, cats)

# Step 5: Create custom visualizations
map_plot <- create_map(
  access_data, points, kenya_data$adm0, kenya_data$adm1, cats,
  point_color = "blue", point_size = 6
)

bar_plot <- create_bar_plot(summary_stats, cats, color_labels = "red")
combined_plot <- combine_plots(map_plot, bar_plot)
```

### Administrative-Level Analysis

``` r
# County-level summaries
county_summary <- generate_admin_summary(
  data = access_data,
  admin_sf = accessMap::adm1,
  admin_name_cols = "NAME_1"
)

# Multi-level analysis
subcounty_summary <- generate_admin_summary(
  data = access_data,
  admin_sf = accessMap::adm2,
  admin_name_cols = c("NAME_1", "NAME_2")
)
```

### Flexible Point Styling

``` r
# Create facility data with attributes
facilities_df <- data.frame(
  lng = c(36.8, 37.0, 36.9),
  lat = c(-1.3, -1.5, -1.4),
  type = c("Hospital", "Clinic", "Hospital"),
  ownership = c("Public", "Private", "Public"),
  capacity = c(100, 50, 75)
)

# Color and shape by ownership, size by capacity
map <- create_map(
  access_data, facilities_df, accessMap::adm0, accessMap::adm1, cats,
  point_color = "ownership",
  point_shape = "ownership", 
  point_size = "capacity",
  point_colors = c("Public" = "red", "Private" = "blue"),
  point_shapes = c("Public" = 16, "Private" = 17),
  label = "Health Facilities"
)
```

### Custom Analysis with Your Own Data

``` r
# Validate your coordinates (auto-detects column names)
my_points <- validate_points(my_facility_data)  # Works with "longitude"/"latitude" too

# Use your own datasets
result <- accessibility(
  points = my_points,
  transitionMatrix = my_friction_surface,
  populationRaster = my_population_data,
  adm0 = my_country_boundary,
  adm1 = my_admin_boundaries,
  categories = my_custom_categories,
  label = "Veterinary Clinics",
  path = "./outputs",
  name = "custom_analysis"
)
```

## Creating categories

``` r
# Create categories with any ColorBrewer palette
cats <- create_categories(
  breaks = c(0, 30, 60, 90, 120, Inf),
  labels = c("Excellent", "Good", "Fair", "Poor", "Very Poor"),
  palette = "RdYlBu",  # Any valid ColorBrewer palette
  direction = -1        # Reverse colors
)
```

## Output Files

The `accessibility()` function automatically generates:

- `{name}_combined_plot.png` - Integrated map and summary chart
- `{name}_map_plot.png` - Accessibility map only  
- `{name}_bar_plot.png` - Summary bar chart only
- `{name}_summary.csv` - Population statistics by travel time

Customize outputs:
``` r
accessibility(
  points = facilities,
  save_opts = list(
    dpi = 2000,      # High resolution
    width = 2,       # Larger size  
    height = 2,
    units = "in"
  ),
  path = "./publication_maps",
  name = "high_res_analysis"
)
```

## Travel Time Categories

### Default Classification
The analysis uses a standard 5-category system:

- **≤30 minutes**: 
- **31-60 minutes**: 
- **61-90 minutes**: 
- **91-120 minutes**: 
- **>120 minutes**: 

### Custom Categories
Create your own categories for specific use cases:

``` r
# Emergency services (shorter time bands)
emergency_cats <- create_categories(
  breaks = c(0, 5, 10, 15, 30, Inf),
  labels = c("Critical", "Urgent", "Priority", "Standard", "Remote")
)

# Rural health (longer acceptable times)
rural_cats <- create_categories(
  breaks = c(0, 60, 120, 180, Inf),
  labels = c("Accessible", "Moderate", "Difficult", "Very Difficult")
)
```

## Data Requirements

### Default Data (Kenya)
The package includes ready-to-use datasets accessible as:

- `accessMap::population_raster` - WorldPop 2020 gridded population
- `accessMap::transition_matrix` - Motorized travel friction (Malaria Atlas Project)
- `accessMap::adm0` - Country boundary from GADM
- `accessMap::adm1` - County boundaries from GADM
- `accessMap::adm2` - Sub-county boundaries from GADM
- `accessMap::adm3` - Ward boundaries from GADM

### Custom Data
For other countries/regions, provide:

- **Facility coordinates**: Matrix/data.frame with lng/lat (auto-detected)
- **Population raster**: High-resolution gridded population data
- **Transition matrix**: Cost-distance friction surface  
- **Administrative boundaries**: Country and subdivision boundaries (sf objects)

> **Important**: When using custom transition matrices, ensure they are properly cropped and masked with your study area boundaries to avoid computational errors.

## Use Cases

- **Healthcare accessibility** - Hospital and clinic catchment analysis
- **Veterinary services** - Animal health facility coverage  
- **Education access** - School accessibility mapping
- **Emergency services** - Response time analysis
- **Infrastructure planning** - Facility placement optimization
- **Policy evaluation** - Before/after accessibility comparisons

## Methodology

The accessibility analysis follows established frameworks from:

- **Weiss et al. (2018, 2020)** - Global travel time mapping methodologies
- **Pfeffer et al. (2018)** - Malaria Atlas Project data access  
- **van Etten (2017)** - Spatial distance calculations in R

The methodology uses:
1. **Cost-distance calculation** via gdistance package (Dijkstra's algorithm)
2. **Population-weighted metrics** using high-resolution gridded data
3. **Realistic travel impedances** from motorized friction surfaces
4. **Travel time categorization** with flexible band definitions

## Function Reference

### Core Functions

- `accessibility()` - Main wrapper for complete analysis
- `compute_travel_cost()` - Calculate travel time surfaces
- `process_access_data()` - Convert rasters to analysis-ready data

### Data Functions  

- `load_default_data()` - Load Kenya datasets
- `validate_points()` - Validate and standardize coordinates
- `default_categories()` - Standard travel time categories
- `create_categories()` - Custom travel time categories

### Analysis Functions

- `generate_summary()` - National-level population summaries
- `generate_admin_summary()` - Administrative unit summaries

### Visualization Functions

- `create_map()` - Accessibility maps with flexible styling
- `create_bar_plot()` - Population distribution charts
- `combine_plots()` - Integrated map and chart layouts
- `save_outputs()` - Save plots and data with consistent naming

### Utility Functions

- `categorize_travel_time()` - Assign travel times to categories

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## References

- Pfeffer, D. A., et al. (2018). malariaAtlas: an R interface to global malariometric data hosted by the Malaria Atlas Project. *Malaria Journal*, 17(1), 352.

- van Etten, J. (2017). R Package gdistance: Distances and Routes on Geographical Grids. *Journal of Statistical Software*, 76(13), 1-21.

- Weiss, D. J., et al. (2018). A global map of travel time to cities to assess inequalities in accessibility in 2015. *Nature*, 553(7688), 333-336.

- Weiss, D. J., et al. (2020). Global maps of travel time to healthcare facilities. *Nature Medicine*, 26(12), 1835-1838.
