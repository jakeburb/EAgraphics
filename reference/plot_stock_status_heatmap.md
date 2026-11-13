# Plot Stock Status Heatmap Relative to LRP for Gulf of St. Lawrence Ecosystem Report

A highly flexible heatmap visualizing stock status with RdYlGn palette.
Includes full bilingual support for species and management areas.

## Usage

``` r
plot_stock_status_heatmap(
  data = NULL,
  year_col = Year,
  stock_col = Stock,
  status_col = status.LRP,
  lang = "en",
  year_range = NULL,
  show_title = TRUE,
  base_size = 12
)
```

## Arguments

- data:

  A data frame containing stock status data. Defaults to
  `EA.extra.data$stock.status`.

- year_col:

  Unquoted name of column for year. Defaults to `Year`.

- stock_col:

  Unquoted name of column for stock names. Defaults to `Stock`.

- status_col:

  Unquoted name of column for status values. Defaults to `status.LRP`.

- lang:

  Character string for language: `"en"` (default) or `"fr"`.

- year_range:

  Optional numeric vector `c(start, end)`.

- show_title:

  Logical. Defaults to `TRUE`.

- base_size:

  Numeric. Base font size. Defaults to 12.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Simple plot using package data
plot_stock_status_heatmap(lang = "en", year_range = c(1994, 2025))

# 2. French version with title removed for a report
plot_stock_status_heatmap(lang = "fr", show_title = FALSE)

# 3. Using a custom external dataset
plot_stock_status_heatmap(my_data, year_col = Annee, stock_col = Espece, status_col = Ratio)
} # }
```
