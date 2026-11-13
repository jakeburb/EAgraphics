# Plot GSLEA Planktonic Metrics by EAR

Visualizes planktonic variables (phytoplankton and zooplankton) from the
gslea package. This function handles complex units (e.g., 10³ ind m⁻²,
mg chla m⁻²) and provides bilingual labels. It can visualize with points
connected by lines (default) and/or a smoothed GAM trend.

## Usage

``` r
plot_gslea_plankton(
  data,
  var,
  EARs = 0,
  groups = NULL,
  year_range = c(1990, 2023),
  lang = "en",
  fit_smooth = FALSE,
  show_line = TRUE,
  method = "gam",
  formula = y ~ s(x, bs = "cs", k = 15),
  col_palette = NULL,
  ear_names = NULL,
  xlab = NULL,
  ylab = NULL,
  base_size = 14,
  facet_scales = "free_y",
  custom_theme = ggplot2::theme_bw()
)
```

## Arguments

- data:

  Long-format data frame containing `year`, `EAR`, `variable`, and
  `value`.

- var:

  Planktonic variable to plot (unquoted, e.g.,
  `calanus.finmarchicus.annual`).

- EARs:

  Vector of EAR identifiers to include. Defaults to `0` (Entire Gulf).

- groups:

  Optional named list to aggregate EARs (e.g.,
  `list("Northern" = 1:4)`).

- year_range:

  Numeric vector `c(start, end)`. Defaults to `c(1990, 2023)`.

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- fit_smooth:

  Logical. If `TRUE`, adds a GAM smoother to the plot.

- show_line:

  Logical. If `TRUE`, connects annual points with a line. Defaults to
  `TRUE`.

- method:

  Smoothing method to use. Defaults to `"gam"`.

- formula:

  Smoothing formula. Defaults to `y ~ s(x, bs = "cs", k = 15)`.

- col_palette:

  Optional character vector of colors for the regions/groups.

- ear_names:

  Optional named vector to override EAR numbers with custom names.

- xlab, ylab:

  Optional strings to override default axis labels.

- base_size:

  Base font size for the ggplot2 theme. Defaults to `14`.

- facet_scales:

  Character. Control facet scales: `"free_y"` (default) or `"fixed"`.

- custom_theme:

  A ggplot2 theme object. Defaults to
  [`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Simple plot for a specific EAR
plot_gslea_plankton(EA.data, dw2_t.annual, EARs = 5)

# 2. Compare Calanus abundance between regions with custom colors
region_list <- list("Northern GSL" = 1:4, "Southern GSL" = c(5, 6, 50))

plot_gslea_plankton(data = EA.data,
                    var = calanus.finmarchicus.annual,
                    groups = region_list,
                    year_range = c(2005, 2023),
                    lang = "en",
                    col_palette = c("Northern GSL" = "darkgreen",
                                    "Southern GSL" = "orange"))

# 3. Plot phytoplankton bloom start date in French
plot_gslea_plankton(EA.data, start, EARs = 1:8, lang = "fr")
} # }
```
