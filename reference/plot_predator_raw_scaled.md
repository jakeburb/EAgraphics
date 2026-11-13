# Plot Predator Abundance (Raw Scaled)

Visualizes abundance trends using raw values with specific scaling
factors (Harp Seals / 100, Grey Seals / 10) as requested by peer review.
Includes regional tags (NWA/GSL) and distinct shapes for accessibility.

## Usage

``` r
plot_predator_raw_scaled(
  data = gslea::EA.data,
  year_col = year,
  var_col = variable,
  val_col = value,
  year_range = NULL,
  lang = "en",
  show_legend = TRUE,
  legend_position = c(0.05, 0.95),
  base_size = 14
)
```

## Arguments

- data:

  A data frame. Defaults to `gslea::EA.data`.

- year_col:

  Unquoted year column.

- var_col:

  Unquoted variable column.

- val_col:

  Unquoted value column.

- lang:

  Language: `"en"` (default) or `"fr"`.

- show_legend:

  Logical. Defaults to `TRUE`.

- legend_position:

  Position of the legend. Defaults to `c(0.05, 0.95)`.

- base_size:

  Numeric. Defaults to `14`.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_predator_raw_scaled(my_data, lang = "en")
} # }
```
