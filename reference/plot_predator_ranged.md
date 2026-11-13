# Plot Ranged Predator Abundance

Visualizes the relative abundance trends of key predators using a
"ranged" (Min-Max) normalization: \\(Value - Min) / (Max - Min)\\. This
allows comparison of species with different absolute population sizes.
To support accessibility for visually impaired users, the function maps
both color and distinct geometric shapes to each species. The legend
displays the original min/max values for context and remains stable
across languages.

## Usage

``` r
plot_predator_ranged(
  data = gslea::EA.data,
  year_col = year,
  var_col = variable,
  val_col = value,
  year_range = NULL,
  lang = "en",
  show_legend = TRUE,
  legend_position = c(0.05, 0.95),
  y_label = NULL,
  base_size = 14,
  palette = NULL,
  species_metadata = NULL
)
```

## Arguments

- data:

  A data frame. Defaults to `gslea::EA.data`.

- year_col:

  Unquoted year column. Defaults to `year`.

- var_col:

  Unquoted variable column. Defaults to `variable`.

- val_col:

  Unquoted value column. Defaults to `value`.

- year_range:

  Numeric vector `c(start, end)` to filter the timeline.

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- show_legend:

  Logical. Defaults to `TRUE`.

- legend_position:

  Numeric vector `c(x, y)` for internal placement. Defaults to
  `c(0.05, 0.95)`.

- y_label:

  Optional string to override the default Y-axis label.

- base_size:

  Numeric. Base font size for the plot. Defaults to `14`.

- palette:

  Optional color vector. Must match the number of predators.

- species_metadata:

  Optional mapping data frame with columns: 'variable', 'en', 'fr'.

## Value

A `ggplot` object with combined color and shape scales.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Basic English plot with accessibility shapes
plot_predator_ranged(data = my_data, lang = "en")

# 2. French version with a custom year range and internal legend
plot_predator_ranged(data = my_data,
                     lang = "fr",
                     year_range = c(1990, 2024),
                     legend_position = c(0.1, 0.9))
} # }
```
