# Plot Hypercapnic Area Time Series

Visualizes the percentage of bottom area \> 100m depth with hypercapnic
conditions for the Estuary and Gulf regions.

## Usage

``` r
plot_hypercapnic_area_timeseries(
  data,
  lang = "en",
  year_range = NULL,
  x_breaks = NULL,
  base_size = 14
)
```

## Arguments

- data:

  A data frame containing 'year', 'percentage', and 'region'.

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- year_range:

  Optional numeric vector `c(start, end)` for the x-axis.

- x_breaks:

  Optional numeric vector for x-axis tick marks.

- base_size:

  Numeric. Base font size. Defaults to 14.

## Value

A `patchwork` object with two side-by-side panels.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Standard hypercapnia plot
plot_hypercapnic_area_timeseries(hypercapnia_data)

# 2. French version with custom year range
plot_hypercapnic_area_timeseries(hypercapnia_data,
                                 lang = "fr",
                                 year_range = c(1990, 2025))
} # }
```
