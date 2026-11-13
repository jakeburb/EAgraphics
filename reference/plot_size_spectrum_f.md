# Plot size spectrum for a single EAR

Plots fish size distribution across years for a single Ecosystem
Approach Region (EAR). Uses a log scale for abundance and fits a GAM
smooth to the data.

## Usage

``` r
plot_size_spectrum_f(years, EAR, ...)
```

## Arguments

- years:

  year vector

- EAR:

  single EAR value (must be length 1)

- ...:

  additional arguments passed to ggplot theme or geom_smooth

## Value

A ggplot object

## Examples

``` r
plot_size_spectrum_f(years=1970:2025, EAR=100)
#> Error in plot_size_spectrum_f(years = 1970:2025, EAR = 100): object 'size.spectrum.data.gsl' not found
plot_size_spectrum_f(years=1970:2025, EAR=200)
#> Error in plot_size_spectrum_f(years = 1970:2025, EAR = 200): object 'size.spectrum.data.gsl' not found
```
