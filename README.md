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

    install.packages("devtools")
    devtools::install_github("jakeburb/EAgraphics")
    library(EAgraphics)

## How To Use

The following will walk through the use and functionality of each of the
functions provided in the package which include:

<u>plot\_gslea\_temperature(data, var, EARs, groups, …)</u>

Visualizes temperature data (SST, bottom temperature, or depth-specific
layers) from the gslea dataset or any ecosystem dataset. It supports
bilingual labeling (lang = “en” or “fr”) and can aggregate multiple
Ecoregions (EARs) into custom groups (e.g., “Northern GSL”). You can
choose to show annual trends with simple lines or apply a smoother
(fit\_smooth = TRUE) to help identify long-term climatic tendencies;
while it defaults to a GAM, you can specify alternative smoothing
methods and formulas to fit your specific statistical needs. For
report-ready graphics, the text size is fully customizable via
base\_size, ensuring your plots stay legible.

    plot_gslea_temperature(data, var, EARs, groups)

<u>plot\_gslea\_ice(data, var, EARs, groups, …)</u>

Visualizes ice metrics (e.g., duration, maximum coverage, first/last
ice) and seasonal phenology (e.g., start of warming or cooling at
specific thresholds) from the gslea dataset or any ecosystem dataset.
Like its temperature counterpart, it features full bilingual support
(lang = “en” or “fr”) and dynamic aggregation of Ecoregions. The
function automatically detects the variable type to apply the correct
units, whether it’s Days, Day of Year, or Week of Year. You can
visualize trends with simple annual lines or utilize a customizable
smoother to identify long-term patterns, while adjusting the base\_size
to ensure all labels remain perfectly legible for publication.

    plot_gslea_ice(data, var, EARs, groups, ...)

<u>plot\_gslea\_plankton(data, var, EARs, groups, …)</u>

Visualizes lower trophic level metrics including phytoplankton bloom
dynamics (start and magnitude) and zooplankton abundance or biomass
(e.g., Calanus species, total dry weight) from the gslea dataset or
other ecosystem datasets. This function is specifically designed to
handle complex biological units—such as
10<sup>3</sup> ind m<sup>−2</sup> or
mg chla m<sup>−2</sup>—automatically based on the variable selected. It
offers full bilingual support (lang = “en” or “fr”) and allows for the
aggregation of Ecoregions into ecological groups. Users can display raw
annual variability with connected lines or apply a customizable smoother
to highlight multi-decadal shifts in the GSL food web, all while
maintaining control over font sizes and color palettes for
publication-quality output.

    plot_gslea_plankton(data, var, EARs, groups, ...)

<u>calc\_anomaly(data, baseline\_range, year\_range, quiet)</u>

A “pipe-friendly” utility used to transform raw observations into
climatological anomalies. By providing a required baseline\_range (e.g.,
c(1991, 2020)), the function calculates a long-term reference mean and
computes the departure for every year in your dataset. It returns a
focused, high-density data frame containing only the essentials: the
year, the original value, the reference mean used, and the final
anomaly. It includes built-in safety checks to ensure your baseline
period actually contains data and, unless set to quiet = TRUE, will
report the calculated mean to your console to ensure your “math is
mathing” before you head to the plotting stage.

<u>plot\_anomaly(data, value\_col, composite\_data, …)</u>

The flagship visualization tool for oceanographers to show environmental
trends and standardized anomaly “scorecards.” This function generates a
two-part plot: a top bar chart showing annual departures from a baseline
and a bottom heatmap scorecard that bins values into a standardized
color scale (from deep blue for negatives to dark red for positives). It
features clamping logic to handle extreme outliers without breaking the
color scale, supports stacked bars for composite indices, and includes
bilingual support (lang = “en” or “fr”). The X-axis is highly
customizable via x\_breaks\_interval, making it easy to create legible
plots for long time series.

    # calculate the anomalies and plots them
    calc_anomaly(data, baseline_range, year_range, quiet)
    plot_anomaly(data, value_col, composite_data, ...)

## Best Practices of Use

The purpose of the package is to define publication quality graphics and
tables that can be used in reports and primary publications. This
package has developed primarily to make graphics from gslea but we have
tried to adopt function arguments that can accept any kind of long-form
data.

The package is designed to facilitate an ecosystem approach to marine
management and therefore any kinds of novel approaches to visualising
that are welcome. The package is not, however, designed to make whole
new analyses and if one has a new analytical approach to working with
the data, we would encourage the development of a new package that calls
this and others as dependencies. In this way, a modularised robust
approach is maintained over a suite of packages where a fatal error (or
lack of updating) in one does not necessarily have to kill the
functionality of the suite. We can include in this package graphical
aspects for the ecosystem approach from your new analytical method.

## Development Plan

This package can easily be expanded to include multiple kinds of
graphics and tables. Pull requests are welcome as long as they do not
break existing functionality.

To include:

- Visually appealing tables of data and pertinent summaries

## Citation for Package:

Burbank, J., Duplisea, D.E. 2026. EAgraphics: An R Package for
Visualizing Ecosystem Approaches to Fisheries Management. R package
version 0.1 <https://github.com/jakeburb/EAgraphics>
