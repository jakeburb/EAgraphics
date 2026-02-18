README
================
Jacob Burbank
2026-02-05

## Overview

The EAgraphics package provides a suite of user-friendly functions
designed to visualize data and indicators frequently utilized in
Ecosystem Approaches (EA) to Fisheries Management.

Initially developed as a visualization companion to the Gulf of
St. Lawrence Ecosystem Approaches (gslea) R package
(<https://github.com/duplisea/gslea>), EAgraphics is built to be
flexible. While it integrates seamlessly with gslea data structures, it
is equally capable of visualizing any EA-relevant datasets that follow
standard long-format conventions.

### Key Capabilities

-Publication-Quality Figures: Standardized plotting for physical
oceanography (temperature, ice dynamics), biological metrics
(phytoplankton phenology, zooplankton abundance), and fisheries data.

-Integrated Trend Analysis: Simplified implementation of Generalized
Additive Models (GAMs) and annual trend lines to distinguish
inter-annual variability from long-term ecosystem shifts.

-Bilingual Reporting: Full support for English and French outputs. Using
internal dictionaries and the rosettafish package
(<https://github.com/pbs-assess/rosettafish>), EAgraphics automatically
translates species names, region identifiers, and complex scientific
units. Translations will need to be added to internal dictionaries when
new terms or areas are used, therefore contact the package maintainer to
add these as necessary (<Jacob.Burbank@dfo-mpo.gc.ca>)

-Anomaly Visualization: Flexible functions for the computation and
visualization of anomalies based on user-defined baseline periods,
essential for identifying significant environmental departures.

-Fisheries Status Monitoring: Specialized heatmap visualizations
designed to show stock status relative to Limit Reference Points (LRP),
ensuring consistent stock ordering and clear threshold demarcations
across languages.

## Why use EAgraphics?

Ecosystem Approach data is often multi-faceted and spatially complex.
This package reduces the “coding overhead” required to produce
consistent figures across multiple indicators. Whether you are comparing
sea surface temperature across different Ecosystem Assessment Regions
(EARs) or visualizing the status of multiple fish stocks simultaneously,
EAgraphics ensures the output is standardized, reproducible, and ready
for technical reports or peer-reviewed publications. Also in line with
billinguial requirements in DFO Res Docs, this package provides the
ability to translated plots to provide both english and french versions
of all figures.

## Installation

You can install the package directly from github and explore the
functions:

``` r
install.packages("devtools")
devtools::install_github("your-username/EAgraphics")
library(EAgraphics)
```

## How To Use

The following will walk through the use and functionality of each of the
functions provided in the package which include:

plot_gslea_temperature(data, var, EARs, groups, …) Visualizes
temperature data (SST, bottom temperature, or depth-specific layers)
from the gslea dataset or any ecosystem dataset. It supports bilingual
labeling (lang = “en” or “fr”) and can aggregate multiple Ecoregions
(EARs) into custom groups (e.g., “Northern GSL”). You can choose to show
annual trends with simple lines or apply a smoother (fit_smooth = TRUE)
to help identify long-term climatic tendencies; while it defaults to a
GAM, you can specify alternative smoothing methods and formulas to fit
your specific statistical needs. For report-ready graphics, the text
size is fully customizable via base_size, ensuring your plots stay
legible.

plot_gslea_ice(data, var, EARs, groups, …) Visualizes ice metrics (e.g.,
duration, maximum coverage, first/last ice) and seasonal phenology
(e.g., start of warming or cooling at specific thresholds) from the
gslea dataset or any ecosystem dataset. Like its temperature
counterpart, it features full bilingual support (lang = “en” or “fr”)
and dynamic aggregation of Ecoregions. The function automatically
detects the variable type to apply the correct units, whether it’s Days,
Day of Year, or Week of Year. You can visualize trends with simple
annual lines or utilize a customizable smoother to identify long-term
patterns, while adjusting the base_size to ensure all labels remain
perfectly legible for publication.

## Best Practices of Use

## Development Plan

## Citation for Package:

Burbank, J., Duplisea, D.E. 2026. EAgraphics: An R Package for
Visualizing Ecosystem Approaches to Fisheries Management. R package
version 0.1 <https://github.com/jakeburb/EAgraphics>
