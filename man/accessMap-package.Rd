% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessMap-package.R
\docType{package}
\name{accessMap-package}
\alias{accessMap}
\alias{accessMap-package}
\title{accessMap: Spatial Accessibility Analysis}
\description{
The accessMap package provides tools for calculating and visualizing spatial
accessibility to facilities such as health centers, schools, or services.
It uses cost-distance analysis based on travel time, incorporates population
distribution, and generates publication-ready maps and summary statistics.
}
\section{Main Functions}{

\itemize{
\item \code{\link{accessibility}}: Main wrapper function for complete accessibility analysis
\item \code{\link{compute_travel_cost}}: Calculate travel time surfaces
\item \code{\link{create_map}}: Generate accessibility maps
\item \code{\link{generate_admin_summary}}: Administrative-level summaries
\item \code{\link{validate_points}}: Validate and standardize coordinates
\item \code{\link{create_categories}}: Create custom travel time categories
}
}

\section{Data Objects}{

The package includes default datasets for Kenya (accessible as data objects):
\itemize{
\item Administrative boundaries from GADM
\item WorldPop 2020 population data
\item Motorized transport friction surface from Malaria Atlas Project
}
Use \code{data(package = "accessMap")} to see available datasets.
}

\section{Getting Started}{

\preformatted{
# Basic accessibility analysis
facility_coords <- matrix(c(36.8, -1.3, 37.0, -1.5), ncol = 2, byrow = TRUE)
colnames(facility_coords) <- c("lng", "lat")

result <- accessibility(
  points = facility_coords,
  label = "Health Facilities"
)

# View results
print(result$national_summary)
plot(result$combined_plot)
}
}

\author{
\strong{Maintainer}: Brian Njuguna \email{brian.njuguna@cema.africa}

}
\keyword{internal}
