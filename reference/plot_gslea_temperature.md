# Plot Temperature data from gslea by EAR

Visualizes temperature data from the gslea package using long-format
data. Provides billingual plots. Can visualize with points connnected by
line or with a smoothed GAM, plots line by default.

## Usage

``` r
plot_gslea_temperature(
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
  base_size = 16,
  facet_scales = "free_y",
  custom_theme = ggplot2::theme_bw()
)
```

## Arguments

- data:

  Long-format data frame containing `year`, `EAR`, `variable`, and
  `value`.

- var:

  Temperature variable to plot (unquoted, e.g., `sst.month10`).

- EARs:

  Vector of EAR identifiers. Defaults to `0`.

- groups:

  Optional named list to aggregate EARs.

- year_range:

  Numeric vector `c(start, end)`. Defaults to `c(1990, 2023)`.

- lang:

  Language for labels: `"en"` or `"fr"`.

- fit_smooth:

  Logical. If `TRUE`, fits a GAM smoother.

- show_line:

  Logical. If `TRUE`, connects annual points with a line. Defaults to
  `TRUE`.

- method:

  Smoothing method. Defaults to `"gam"`.

- formula:

  Smoothing formula. Defaults to `y ~ s(x, bs = "cs", k = 15)`.

- col_palette:

  Optional character vector of colors for the regions.

- ear_names:

  Optional named vector to override EAR numbers with names.

- xlab, ylab:

  Optional custom axis labels.

- base_size:

  Base font size for the plot. Defaults to `14`.

- facet_scales:

  Character. Control facet scales: `"free_y"` (default) or `"fixed"`.

- custom_theme:

  A ggplot2 theme. Defaults to
  [`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare surface warming in broad regions with custom colors and fixed scales
my_regions <- list("Northern GSL" = 1:4, "Southern GSL" = c(5, 6, 50))

plot_gslea_temperature(EA.data, sst.month10,
                       groups = my_regions,
                       year_range = c(2000, 2023),
                       facet_scales = "fixed",
                       col_palette = c("Northern GSL" = "royalblue",
                                       "Southern GSL" = "firebrick"))
} # }
```
