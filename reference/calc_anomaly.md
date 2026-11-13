# Calculate Anomalies from a Baseline Mean

Calculates the anomaly of a variable relative to a user-specified
baseline period. Returns a focused data frame containing only the year,
the original value, the baseline mean used, and the resulting anomaly.

## Usage

``` r
calc_anomaly(data, baseline_range, year_range = NULL, quiet = FALSE)
```

## Arguments

- data:

  A data frame containing at least `year` and `value` columns.

- baseline_range:

  Numeric vector `c(start, end)` defining the period used to calculate
  the mean. This argument is required.

- year_range:

  Optional numeric vector `c(start, end)` to filter the returned data.
  If `NULL`, returns all years in the input data.

- quiet:

  Logical. If `FALSE` (default), prints the baseline mean to the console
  via a message.

## Value

A focused data frame (tibble) with columns: `year`, `value`,
`baseline_mean`, and `anomaly`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Using the native R pipe (|>)
sst_anom <- EA.data |>
  dplyr::filter(variable == "sst.month10", EAR == 0) |>
  calc_anomaly(baseline_range = c(1991, 2020))

# Example 2: Using base R subset() and assignment
physical_data <- subset(EA.data, variable == "t.deep" & EAR == 1)
deep_t_anom <- calc_anomaly(data = physical_data,
                            baseline_range = c(1995, 2015))

# View the clean result
head(deep_t_anom)
} # }
```
