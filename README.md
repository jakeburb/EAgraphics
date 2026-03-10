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
devtools::install_github("jakeburb/EAgraphics")
library(EAgraphics)
```

## How To Use

The following will walk through the use and functionality of each of the
functions provided in the package which include:

<u>plot_gslea_temperature(data, var, EARs, groups, …)</u>

Visualizes temperature data (SST, bottom temperature, or depth-specific
layers) from the gslea dataset or any ecosystem dataset. It supports
bilingual labeling (lang = “en” or “fr”) and can aggregate multiple
Ecoregions (EARs) into custom groups (e.g., “Northern GSL”). You can
choose to show annual trends with simple lines or apply a smoother
(fit_smooth = TRUE) to help identify long-term climatic tendencies;
while it defaults to a GAM, you can specify alternative smoothing
methods and formulas to fit your specific statistical needs. For
report-ready graphics, the text size is fully customizable via
base_size, ensuring your plots stay legible.

<u>plot_gslea_ice(data, var, EARs, groups, …)</u>

Visualizes ice metrics (e.g., duration, maximum coverage, first/last
ice) and seasonal phenology (e.g., start of warming or cooling at
specific thresholds) from the gslea dataset or any ecosystem dataset.
Like its temperature counterpart, it features full bilingual support
(lang = “en” or “fr”) and dynamic aggregation of Ecoregions. The
function automatically detects the variable type to apply the correct
units, whether it’s Days, Day of Year, or Week of Year. You can
visualize trends with simple annual lines or utilize a customizable
smoother to identify long-term patterns, while adjusting the base_size
to ensure all labels remain perfectly legible for publication.

<u>plot_gslea_plankton(data, var, EARs, groups, …)</u>

Visualizes lower trophic level metrics including phytoplankton bloom
dynamics (start and magnitude) and zooplankton abundance or biomass
(e.g., Calanus species, total dry weight) from the gslea dataset or
other ecosystem datasets. This function is specifically designed to
handle complex biological units—such as $10^3\text{ ind m}^{-2}$ or
$\text{mg chla m}^{-2}$—automatically based on the variable selected. It
offers full bilingual support (lang = “en” or “fr”) and allows for the
aggregation of Ecoregions into ecological groups. Users can display raw
annual variability with connected lines or apply a customizable smoother
to highlight multi-decadal shifts in the GSL food web, all while
maintaining control over font sizes and color palettes for
publication-quality output.

<u>plot_predator_ranged(data, year_col, var_col, val_col, …)</u>

A specialized visualization tool designed to compare population trends
across species with vastly different absolute abundances (e.g.,
comparing millions of seals to thousands of tuna). This function applies
a Min-Max normalization—or ‘ranging’—to the data, rescaling all values
to a common $0$ to $1$ scale. This allows users to identify synchronous
shifts, lags, or divergent trajectories in predator groups that would
otherwise be impossible to view on a single Y-axis.The function features
a highly informative, ‘smart’ legend that automatically calculates and
displays the original minimum and maximum values for each species,
providing essential context to the normalized lines. It is built with a
default focus on key regional predators (Harp Seals, Grey Seals,
Northern Gannets, and Atlantic Bluefin Tuna) but is fully adaptable to
external datasets through tidy evaluation. Like the other tools in the
package, it supports bilingual labeling (lang = “en” or “fr”),
customizable year ranges, and flexible legend positioning for
publication-ready outputs.

<u>plot_landings_stacked(data, year_col, group_col, value_col, …)</u>

The primary tool for visualizing the evolution of fisheries composition
and ecosystem components over time. While originally developed for
landings, this function is equally robust for generating stacked biomass
graphs, such as those used for pelagic fish in the GSL Ecosystem
Summaries Report. By using high-impact stacked areas, it effectively
communicates both the total volume of the resource and the shifting
dominance of specific species or groups—making it easy to visualize
transitions like the regime shift from groundfish to invertebrates. A
standout feature of this function is its integrated translation and
ordering logic: it automatically handles the mapping of common fisheries
categories between English and French and ensures that legend items
appear in a biologically or economically logical order rather than a
simple alphabetical one. Users can easily customize color palettes, set
specific Y-axis intervals (breaks) for better readability, and filter by
year range.

<u>plot_stock_status_heatmap(data, status_col, year_range, …)</u>

A useful summary tool for ecosystem-level reporting, designed to
visualize the health of multiple fish stocks simultaneously. This
function generates a Stock Status Heatmap that categorizes stocks into
three standard management zones: Healthy (Green), Cautious (Yellow), and
Critical (Red). By aligning diverse species—from invertebrates to
groundfish—along a shared timeline, it allows scientists and managers to
identify broad ecosystem trends, such as periods of widespread decline
or recovery across different functional groups.

The function is built with rigorous categorization logic, automatically
mapping various assessment terminologies into a unified color-coded
system. It features bilingual support for species names and status
levels, ensures chronological consistency across the X-axis, and
provides a clean, “grid-style” layout that remains legible even when
displaying a large number of stocks. It is an efficient visualization
for Ecosystem Summaries Reports to provide a “snapshot” of the current
state of fisheries relative to their precautionary approach (PA)
reference points.”

<u>plot_abundance_trends(data, var, year_range, …)</u>

A flexible tool for visualizing temporal trends in absolute fish
abundance, such as biomass or density. While other functions in the
package focus on relative shifts or compositions, this function is
designed to handle long-form datasets (like gslea) where users need to
extract and plot specific variables or species from a large data frame.

A key feature of this function is its dynamic labeling: when a single
species is selected, the Y-axis label automatically updates to include
the species name (e.g., ‘Atlantic Bluefin Tuna Abundance Index’). For
broader comparisons, it can also plot multiple variables simultaneously
using a high-contrast color palette. It includes an internal dictionary
for common species names, supports the rosettafish package for automated
French translations, and ensures that the Y-axis starts at zero by
default to maintain a clear and honest representation of the data. It is
highly customizable, allowing users to adjust line thickness, color
palettes, and theme settings for publication-ready figures.

<u>calc_anomaly(data, baseline_range, year_range, quiet)</u>

A “pipe-friendly” utility used to transform raw observations into
climatological anomalies. By providing a required baseline_range (e.g.,
c(1991, 2020)), the function calculates a long-term reference mean and
computes the departure for every year in your dataset. It returns a
focused, high-density data frame containing only the essentials: the
year, the original value, the reference mean used, and the final
anomaly. It includes built-in safety checks to ensure your baseline
period actually contains data and, unless set to quiet = TRUE, will
report the calculated mean to your console to ensure your “math is
mathing” before you head to the plotting stage.

<u>plot_anomaly(data, value_col, composite_data, …)</u>

The flagship visualization tool for oceanographers to show environmental
trends and standardized anomaly “scorecards.” This function generates a
two-part plot: a top bar chart showing annual departures from a baseline
and a bottom heatmap scorecard that bins values into a standardized
color scale (from deep blue for negatives to dark red for positives). It
features clamping logic to handle extreme outliers without breaking the
color scale, supports stacked bars for composite indices, and includes
bilingual support (lang = “en” or “fr”). The X-axis is highly
customizable via x_breaks_interval, making it easy to create legible
plots for long time series.

<u>plot_anomaly_comp(data, value_col, var_col, …)</u>

An advanced variation of the anomaly tool designed for multi-layered
datasets, such as temperature or oxygen levels across multiple depth
strata. This function goes beyond simple plotting by internally
calculating a composite annual sum and a standardized scorecard value
($Annual Sum / SD$).A key feature is the ability to define a baseline
reference period (e.g., 2014-2020) for calculating the mean and standard
deviation, allowing for consistent anomaly comparisons across different
environmental variables regardless of the total time-series length.The
resulting visualization features a stacked bar chart where individual
components are clearly distinguished, overlaid with a composite trend
line. The bottom scorecard provides a rigorous, standardized heatmap of
the total annual departure. A key strength of this function is its
‘smart’ legend logic: it can automatically append units (e.g., ‘m’ for
depth) and numerically sort group levels to ensure logical ordering
(e.g., 50m, 200m, 500m) regardless of the input order. Like its
counterpart, it includes bilingual support and robust outlier clamping
for publication-quality output.

### Choosing the Right Anomaly Function

| Feature | `plot_anomaly()` | `plot_anomaly_comp()` |
|:---|:---|:---|
| **Primary Use** | Pre-calculated anomalies | Raw component anomalies |
| **Composite Logic** | User-supplied `composite_data` | **Calculated internally** (Sum) |
| **Scorecard Value** | User-supplied | **Standardized Score** ($Z-score$) |
| **Best For** | Single time-series trends | Depth strata, areas, or species groups |

\##Examples

## Best Practices of Use

## Development Plan

## Citation for Package:

Burbank, J., Duplisea, D.E. 2026. EAgraphics: An R Package for
Visualizing Ecosystem Approaches to Fisheries Management. R package
version 0.1 <https://github.com/jakeburb/EAgraphics>
