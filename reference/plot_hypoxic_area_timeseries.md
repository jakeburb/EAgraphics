# Plot Hypoxic Area Time Series

Visualizes the percentage of bottom area \> 100m depth with oxygen
levels below numeric thresholds (defaults to 20 and 30) for the Estuary
and Gulf regions.

## Usage

``` r
plot_hypoxic_area_timeseries(
  data,
  thresholds = c(20, 30),
  lang = "en",
  year_range = NULL,
  x_breaks = NULL,
  base_size = 14
)
```

## Arguments

- data:

  A data frame containing 'year', 'percentage', 'region', and
  'threshold'.

- thresholds:

  Numeric vector of thresholds to plot. Defaults to `c(20, 30)`.

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- year_range:

  Optional numeric vector `c(start, end)` for the x-axis.

- x_breaks:

  Optional numeric vector for x-axis tick marks.

- base_size:

  Numeric. Base font size. Defaults to 14.

  1\. Standard plot with default 20
  plot_hypoxic_area_timeseries(hypoxia_data)

  \# 2. French version with custom thresholds and specific year range
  plot_hypoxic_area_timeseries(hypoxia_data, thresholds = c(15, 25, 35),
  lang = "fr", year_range = c(2000, 2025), x_breaks = seq(2000, 2025,
  5))
