# Plot Trophic Guild Condition Anomalies

Calculates and visualizes condition Z-score anomalies for habitat guilds
(Pelagic, Benthopelagic, Benthic, Demersal) and a combined "All" group.
Stacks EAR 100 (nGSL) and 200 (sGSL) with aligned axes and binned color
scale.

## Usage

``` r
plot_condition_guild_anomalies(
  data,
  lang = "en",
  year_range = NULL,
  x_breaks = NULL,
  base_size = 14
)
```

## Arguments

- data:

  A data frame containing 'year', 'EAR', 'HabitatGuild', and 'value'.

- lang:

  Language for labels: `"en"` (default) or `"fr"`.

- year_range:

  Optional numeric vector `c(start, end)` to set a fixed timeline.

- x_breaks:

  Optional numeric vector for x-axis ticks.

- base_size:

  Numeric. Base font size. Defaults to 14.

## Value

A `patchwork` object with two stacked panels (A and B).

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Standard English plot with automatic year range
plot_condition_guild_anomalies(CONsGSL, lang = "en")

# 2. French version with specific timeline and 5-year breaks
plot_condition_guild_anomalies(CONsGSL,
                               lang = "fr",
                               year_range = c(1984, 2024),
                               x_breaks = seq(1984, 2024, 5))
} # }
```
