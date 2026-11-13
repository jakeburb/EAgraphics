# Plot Stacked Landings or Biomass by Group

Visualizes fisheries landings or biomass trends over time using a
stacked area plot. Supports bilingual labeling (via an internal
dictionary and `rosettafish`), custom color palettes, and adjustable
text scaling. Automatically detects the legend title (Group vs Guild)
and stacking order based on the content of the data.

## Usage

``` r
plot_landings_stacked(
  data,
  year_col = year,
  group_col = grp,
  value_col = landings,
  lang = "en",
  year_range = NULL,
  group_order = NULL,
  show_title = FALSE,
  col_palette = NULL,
  base_size = 14,
  xlab = NULL,
  ylab = NULL,
  legend_title = NULL,
  x_breaks = NULL,
  y_breaks = ggplot2::waiver(),
  custom_theme = ggplot2::theme_bw()
)
```

## Arguments

- data:

  A data frame containing landings or biomass.

- year_col:

  Unquoted name of the year column. Defaults to `year`.

- group_col:

  Unquoted name of the grouping column (e.g., species group). Defaults
  to `grp`.

- value_col:

  Unquoted name of the landings or biomass value column. Defaults to
  `landings`.

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- year_range:

  Numeric vector `c(start, end)` to filter the timeline.

- group_order:

  Character vector defining the visual order from **top to bottom**. If
  `NULL`, the function auto-detects based on species names in the data.

- show_title:

  Logical. Defaults to `FALSE`.

- col_palette:

  Optional named character vector of colors.

- base_size:

  Numeric. Base font size for the plot. Defaults to 14.

- xlab, ylab:

  Optional strings to override default axis labels.

- legend_title:

  Optional string to override the automatic legend title detection.

- x_breaks:

  Optional numeric vector for x-axis tick marks (e.g.
  `seq(1990, 2025, 5)`).

- y_breaks:

  Optional numeric vector for y-axis tick marks. Defaults to
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  (automatic).

- custom_theme:

  A ggplot2 theme. Defaults to
  [`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load your data
landings_data <- read.csv("landings_by_group.csv")

# 1. Basic plot with default order (Top: Pelagics -> Middle: Crustaceans -> Bottom: Groundfish)
plot_landings_stacked(landings_data)

# 2. French version, custom year range, and larger text scaling
plot_landings_stacked(landings_data,
                       lang = "fr",
                       year_range = c(1990, 2023),
                       base_size = 16)

# 3. Custom stack order: Pelagics on top, Crustaceans on bottom
plot_landings_stacked(landings_data,
                       group_order = c("pelagics", "groundfish", "crustaceans"))

# 4. Custom colour palette with rosettafish fallback
my_pal <- c("Crustaceans" = "#8b0000",
             "Groundfish"  = "#4682b4",
             "Pelagics"    = "#2e8b57")

plot_landings_stacked(landings_data, col_palette = my_pal)
} # }
```
