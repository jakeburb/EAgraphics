# Plot Size Spectrum Anomalies

Calculates and visualizes size spectrum anomalies from RV surveys
independently for 2 regions: the nGSL and sGSL. Uses a binned color
scale with explicit '\<-3' and '\>3' labels. Aligns x-axes perfectly
across a shared timeline, ensuring 1984 and edge years are fully
rendered without clipping.

## Usage

``` r
plot_size_spectrum_anomalies(
  data,
  lang = "en",
  year_range = NULL,
  x_breaks = NULL,
  standardize = TRUE,
  log_transform = FALSE,
  base_size = 14
)
```

## Arguments

- data:

  A data frame containing 'year', 'EAR', 'variable', and 'value' (raw
  data).

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- year_range:

  Optional numeric vector `c(start, end)` to set a fixed timeline.
  Defaults to the full range found in the data (e.g., 1984-2025).

- x_breaks:

  Optional numeric vector for x-axis tick marks.

- standardize:

  Logical. If `TRUE` (default), calculates Z-scores.

- log_transform:

  Logical. If `TRUE` (default), log-transforms values before
  calculation.

- base_size:

  Numeric. Base font size. Defaults to 14.

## Value

A `patchwork` object with two stacked panels (A) and (B).

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Basic plot with binned scale and automatic year detection
plot_size_spectrum_anomalies(size.spectrum.data.gsl)

# 2. French version with a specific year range and 5-year increments
plot_size_spectrum_anomalies(size.spectrum.data.gsl,
                              lang = "fr",
                              year_range = c(1984, 2024),
                              x_breaks = seq(1984, 2024, 5))
} # }
```
