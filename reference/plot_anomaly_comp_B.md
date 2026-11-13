# Plot Generic Anomaly with Standardized Scorecard With Composite Computed Internally-Blank Scorecard With Colour Legend for Ecoystem Summary

Creates a publication-quality anomaly plot with stacked bars and a
scorecard. The function automatically calculates a composite annual sum
and a standardized scorecard value. Standardized scores (Z-scores) can
be calculated relative to a specific baseline year range (e.g., a
climate normal) or the entire time series. The scorecard is left blank
and a scale bar for the anomaly is added to match the aesthetic of the
RV anomaly plots.

## Usage

``` r
plot_anomaly_comp_B(
  data,
  value_col,
  var_col = NULL,
  unit = NULL,
  show_composite = TRUE,
  show_scorecard = TRUE,
  year_range = NULL,
  baseline_range = NULL,
  x_breaks_interval = 3,
  colors = NULL,
  lang = "en",
  y_label = NULL,
  y_breaks = ggplot2::waiver(),
  base_size = 14
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

  Numeric. Interval for year labels. Defaults to 3.

- colors:

  Optional character vector of colors. Defaults to "YlGnBu".

- lang:

  Language: `"en"` (default) or `"fr"`.

- y_label:

  Optional string for the y-axis title. Defaults to "Anomalies".

- y_breaks:

  Optional numeric vector for y-axis tick marks.

- base_size:

  Numeric. Base font size. Defaults to 14.

## Value

A combined ggplot/cowplot object.
