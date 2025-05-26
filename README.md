# accessibility

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

An R package for calculating and visualizing spatial accessibility to facilities using cost-distance analysis. The package performs accessibility modeling by computing accumulated travel costs from facility locations to population centers and generates comprehensive visualizations including maps and summary statistics.

## Features

- **Cost-distance analysis** using realistic travel impedances
- **Motorized friction surfaces** from the Malaria Atlas Project
- **Publication-ready visualizations** with maps and summary charts
- **Default Kenya datasets** included (population, boundaries, friction surface)
- **Flexible input options** for custom data and study areas
- **Multiple output formats** (PNG maps, CSV summaries)

## Installation

You can install the development version of accessibility from [GitHub](https://github.com/) with:

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
library(accessibility)

# Define facility coordinates (longitude, latitude)
health_facilities <- matrix(c(
  36.8219, -1.2921,  # Nairobi
  39.6682, -4.0435,  # Mombasa  
  35.2834, 0.5143    # Eldoret
), ncol = 2, byrow = TRUE)

# Run accessibility analysis
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

### Custom Analysis with Your Own Data

``` r
# Use your own datasets
result <- accessibility(
  points = my_facility_coordinates,
  transitionMatrix = my_friction_surface,
  populationRaster = my_population_data,
  adm0 = my_country_boundary,
  adm1 = my_admin_boundaries,
  label = "Veterinary Clinics",
  path = "./outputs",
  name = "custom_analysis"
)
```

### High-Resolution Outputs

``` r
# Generate high-quality maps for publication
result <- accessibility(
  points = facilities,
  saving_list = list(
    dpi = 2000,      # High resolution
    width = 2,       # Larger size
    height = 2
  ),
  name = "publication_maps"
)
```

## Output Files

The function automatically generates:

- `{name}_combined_plot.png` - Integrated map and summary chart
- `{name}_map_plot.png` - Accessibility map only  
- `{name}_bar_plot.png` - Summary bar chart only
- `{name}_national_summary.csv` - Population statistics by travel time

## Travel Time Categories

The analysis classifies population accessibility into five bands:

- **≤30 minutes**: High accessibility
- **31-60 minutes**: Moderate accessibility  
- **61-90 minutes**: Low accessibility
- **91-120 minutes**: Very low accessibility
- **>120 minutes**: Extremely low accessibility

## Data Requirements

### Default Data (Kenya)
The package includes ready-to-use datasets for Kenya:
- **Population**: WorldPop gridded population data
- **Friction surface**: Motorized travel friction from Malaria Atlas Project
- **Administrative boundaries**: Country and county boundaries

### Custom Data
For other countries/regions, provide:
- **Facility coordinates**: Matrix or data frame with longitude/latitude

- **Population raster**: High-resolution gridded population

- **Transition matrix**: Cost-distance friction surface

- **Administrative boundaries**: Country and subdivision boundaries

> **Important**: When using custom transition matrices, ensure they are properly cropped and masked with your study area boundaries to avoid computational errors.

## Methodology

The accessibility analysis is based on established frameworks from:

- **Weiss et al. (2018, 2020)** - Global travel time mapping methodologies
- **Pfeffer et al. (2018)** - Malaria Atlas Project data access
- **van Etten (2017)** - Spatial distance calculations in R

The default friction surface uses motorized transport assumptions optimized for healthcare accessibility analysis.

## Use Cases

- **Healthcare accessibility** - Hospital and clinic catchment analysis
- **Veterinary services** - Animal health facility coverage
- **Education access** - School accessibility mapping  
- **Emergency services** - Response time analysis
- **Infrastructure planning** - Facility placement optimization

## Advanced Features

### Access Raw Outputs
``` r
result <- accessibility(points = facilities, name = "analysis")

# Travel time raster (minutes to nearest facility)
travel_times <- result$access

# Gridded data with coordinates and population
detailed_data <- result$temp_df

# Customize further analysis
library(ggplot2)
ggplot(result$temp_df, aes(x = x, y = y, fill = travel_time)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Minutes")
```

### Custom Saving Options
``` r
accessibility(
  points = facilities,
  saving_list = list(
    dpi = 1500,
    width = 3,
    height = 2,
    units = "in"
  ),
  path = "~/Desktop/maps"
)
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## References

- Pfeffer, D. A., et al. (2018). malariaAtlas: an R interface to global malariometric data hosted by the Malaria Atlas Project. *Malaria Journal*, 17(1), 352.

- van Etten, J. (2017). R Package gdistance: Distances and Routes on Geographical Grids. *Journal of Statistical Software*, 76(13), 1-21.

- Weiss, D. J., et al. (2018). A global map of travel time to cities to assess inequalities in accessibility in 2015. *Nature*, 553(7688), 333-336.

- Weiss, D. J., et al. (2020). Global maps of travel time to healthcare facilities. *Nature Medicine*, 26(12), 1835-1838.
