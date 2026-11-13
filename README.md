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

## How To Use

View the functions and their capabilities in the package website: https://jakeburb.github.io/EAgraphics/index.html
The functions are organized in 5 main categories: Environmental Indicators, Biological Indicators, Fisheries Indicators, Anomaly Analysis, Other Functions


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
ability to rapidly and easily produce all figures in both english and french.

## Installation

You can install the package directly from github and explore the
functions:

``` r
install.packages("devtools")
devtools::install_github("jakeburb/EAgraphics")
library(EAgraphics)
```

## Citation for Package:

Burbank, J., Sutton, J.T., Duplisea, D.E. 2026. EAgraphics: An R Package
for Visualizing Ecosystem Approaches to Fisheries Management. R package
version 0.1 <https://github.com/jakeburb/EAgraphics>
