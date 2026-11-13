# Plot Fish Abundance Trends

Visualizes trends in fish abundance (e.g., biomass or density) over
time. Designed to handle long-form data (gslea) by allowing users to
pick out a specific variable (e.g., a species name) from the data. The
Y-axis label automatically updates to include the variable name.

## Usage

``` r
plot_abundance_trends(
  data,
  var = NULL,
  year_range = NULL,
  ylim = c(0, NA),
  year_col = year,
  value_col = value,
  group_col = variable,
  lang = "en",
  show_legend = TRUE,
  col_palette = NULL,
  base_size = 14,
  line_width = 1.2,
  custom_theme = ggplot2::theme_bw()
)
```

## Arguments

- data:

  A data frame containing abundance data (e.g., gslea long-form).

- var:

  Character. The specific variable or species to pick out from the
  `group_col` (e.g., "abft.n"). If `NULL`, all variables in the data are
  plotted.

- year_range:

  Numeric vector `c(start, end)` to filter the timeline.

- ylim:

  Numeric vector `c(min, max)` for the Y-axis. Defaults to `c(0, NA)`.

- year_col:

  Unquoted name of the year column. Defaults to `year`.

- value_col:

  Unquoted name of the value column. Defaults to `value`.

- group_col:

  Unquoted name of the grouping column. Defaults to `variable`.

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- show_legend:

  Logical. Whether to display the legend. Defaults to `TRUE`.

- col_palette:

  Optional character vector of colors.

- base_size:

  Numeric. Base font size for the plot. Defaults to 14.

- line_width:

  Numeric. Thickness of the trend lines. Defaults to 1.2.

- custom_theme:

  A ggplot2 theme. Defaults to
  [`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Plot specific species from gslea data (Dynamic Y-axis label)
plot_abundance_trends(gslea_data, var = "abft.n")

# 2. French version, specific for Atlantic Bluefin Tuna
plot_abundance_trends(gslea_data, lang = "fr", show_legend = FALSE)

} # }
```
