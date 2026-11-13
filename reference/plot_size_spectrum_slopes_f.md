# Compute and plot size spectrum slopes over time

Computes the slope of log(abundance) vs size (not log-transformed) for
each year, then plots the slope over time with a GAM smooth and
confidence interval. The slope represents the size spectrum exponent.

## Usage

``` r
plot_size_spectrum_slopes_f(years, EAR, min_points = 5, gam_k = 5, ...)
```

## Arguments

- years:

  year vector

- EAR:

  single EAR value (must be length 1)

- min_points:

  minimum number of data points required to fit slope (default = 5)

- gam_k:

  basis dimension for GAM smooth (default = 5)

- ...:

  additional arguments passed to ggplot theme

## Value

A list containing: plot (ggplot object), slopes (data.frame with slope
estimates)

## Examples

``` r
result <- plot_size_spectrum_slopes_f(years=1970:2025, EAR=200)
#> Error in plot_size_spectrum_slopes_f(years = 1970:2025, EAR = 200): object 'size.spectrum.data.gsl' not found
result <- plot_size_spectrum_slopes_f(years=1970:2025, EAR=100)
#> Error in plot_size_spectrum_slopes_f(years = 1970:2025, EAR = 100): object 'size.spectrum.data.gsl' not found
print(result$plot)
#> Error: object 'result' not found
head(result$slopes)
#> Error: object 'result' not found
```
