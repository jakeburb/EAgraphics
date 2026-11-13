# Plot Generic Anomaly with Standardized Scorecard With Composite Computed Internally

Creates a publication-quality anomaly plot with stacked bars and a
scorecard. The function automatically calculates a composite annual sum
and a standardized scorecard value. Standardized scores (Z-scores) can
be calculated relative to a specific baseline year range (e.g., a
climate normal) or the entire time series.

## Usage

``` r
plot_anomaly_comp(
  data,
  value_col,
  var_col = NULL,
  unit = NULL,
  show_composite = TRUE,
  show_scorecard = TRUE,
  year_range = NULL,
  baseline_range = NULL,
  x_breaks_interval = 1,
  colors = NULL,
  lang = "en",
  y_label = NULL,
  y_breaks = ggplot2::waiver(),
  base_size = 11
)
```

## Arguments

- data:

  Data frame containing `year` and the anomaly values.

- value_col:

  Unquoted name of the anomaly column for the bars.

- var_col:

  Optional unquoted name of the grouping column for stacked bars.

- unit:

  Optional string for units (e.g., "m"). Appends to legend items only.

- show_composite:

  Logical. Should the composite line be drawn?

- show_scorecard:

  Logical. Should the heatmap scorecard be drawn?

- year_range:

  Numeric vector `c(start, end)` for plot display limits.

- baseline_range:

  Numeric vector `c(start, end)` for Z-score baseline. If `NULL`,
  defaults to the entire time series in the data.

- x_breaks_interval:

  Numeric. Interval for year labels. Defaults to 1.

- colors:

  Optional character vector of colors. Defaults to "YlGnBu".

- lang:

  Language: `"en"` (default) or `"fr"`.

- y_label:

  Optional string for the y-axis title. Defaults to "Anomalies".

- y_breaks:

  Optional numeric vector for y-axis tick marks.

- base_size:

  Numeric. Base font size. Defaults to 11.

## Value

A combined ggplot/cowplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Dissolved Oxygen anomalies at different depths
# Baseline defaults to the entire range of 'do_data'
plot_anomaly_comp(data = do_data,
                  value_col = anomaly,
                  var_col = depth,
                  unit = "m",
                  y_label = "Dissolved Oxygen Anomaly (ml/L)")

# Example 2: SST anomaly with a specific 1991-2020 baseline for the scorecard
plot_anomaly_comp(data = sst_data,
                  value_col = sst_anom,
                  baseline_range = c(1991, 2020),
                  show_composite = TRUE,
                  colors = "red")
} # }
```
