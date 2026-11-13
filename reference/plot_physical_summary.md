# Plot Physical Summary with 1991-2020 Climatology

Creates a two-panel dashboard where anomalies and summary statistics are
calculated based on the 1991-2020 climatological mean. Includes
dual-axis time series and an anomaly scorecard.

## Usage

``` r
plot_physical_summary(
  data,
  lang = "en",
  year_range = c(1969, 2025),
  clim_range = c(1991, 2020),
  base_size = 11
)
```

## Arguments

- data:

  A long-format data frame with columns: 'year', 'variable', 'value'.

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- year_range:

  Numeric vector for plot x-axis. Defaults to `c(1969, 2025)`.

- clim_range:

  Numeric vector for baseline mean. Defaults to `c(1991, 2020)`.

- base_size:

  Numeric. Base font size. Defaults to `11`.
