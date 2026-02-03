# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Plot Temperature data from gslea by Ecosystem Approach Region (EAR)
#'
#' @description
#' Visualizes temperature data from the gslea package. Supports filtering by
#' one or more EAR with numeric codes (0-7, 50, 10/1), automatic unit detection,
#' and bilingual labeling via \code{rosettafish}.
#'
#' @param data A data frame containing gslea ecosystem data (e.g., \code{EA.data}).
#' Must contain \code{year} and \code{ear} columns.
#' @param var The temperature variable to plot. Can be unquoted (e.g., \code{sst.month10}).
#' @param ears Vector of EAR identifiers (e.g., \code{0, 1, 2, "10/1"}). Defaults to \code{0}.
#' @param years Numeric vector of length 2: \code{c(start, end)}. Defaults to \code{c(1990, 2023)}.
#' @param lang Language for labels: \code{"en"}, \code{"fr"}, or \code{"both"}.
#' @param fit_smooth Logical. If \code{TRUE}, fits a smoother (GAM by default).
#' @param method The smoothing method (e.g., \code{"gam"}, \code{"lm"}).
#' @param formula The smoothing formula. Defaults to \code{y ~ s(x, bs = "cs")}.
#' @param col_palette A character vector of colors for the EARs.
#' @param ear_names Optional named character vector to override EAR numbers with
#' names (e.g., \code{c("0" = "Entire Gulf")}).
#' @param xlab Optional custom x-axis label.
#' @param ylab Optional custom y-axis label.
#' @param custom_theme A \code{ggplot2} theme object. Defaults to \code{theme_bw()}.
#'
#' @details
#' This function streamlines the visualization of temporal ecosystem trends across
#' various spatial scales in the Gulf of St. Lawrence. It automatically handles
#' common data preparation tasks:
#' \itemize{
#'   \item \strong{Subsetting:} Filters the dataset by year range and region codes.
#'   \item \strong{Labeling:} Uses a lookup table and regex to identify temperature
#'   variables, appends units (°C), and translates labels using \code{rosettafish}.
#'   \item \strong{Smoothing:} If \code{fit_smooth} is \code{TRUE} and a region has
#'   more than 5 data points, a trend line is added. By default, this uses a
#'   Generalized Additive Model (GAM) with a cubic regression spline to
#'   capture non-linear ecosystem shifts.
#'   \item \strong{Faceting:} If multiple EARs are selected, the function
#'   utilizes \code{facet_wrap} to create a side-by-side comparison with
#'   independent y-axes, allowing for clear visualization of regional differences.
#' }
#'
#' @return A \code{ggplot} object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_bw theme element_blank facet_wrap scale_color_manual
#' @importFrom dplyr filter select mutate
#' @importFrom rosettafish en2fr
#' @importFrom rlang enquo as_label !!
#' @importFrom mgcv s
#'
#' @examples
#' \dontrun{
#' # Plotting Magdalen Shallows (EAR 5)  for the 2000 - 2020
#' plot_gslea_temp(EA.data, sst.month10, ears = 5, years = c(2000, 2020))
#'
#' # Comparison with custom names and French labels
#' plot_gslea_temp(EA.data, bottom.temp,
#'                ears = c(1, 2, 3),
#'                lang = "fr",
#'                ear_names = c("1" = "North Western", "2" = "North Eastern", "3" = "Centre"))
#' }
#' @export
plot_gslea_temp <- function(data,
                            var,
                            ears = 0,
                            years = c(1990, 2023),
                            lang = "en",
                            fit_smooth = TRUE,
                            method = "gam",
                            formula = y ~ s(x, bs = "cs"),
                            col_palette = NULL,
                            ear_names = NULL,
                            xlab = NULL,
                            ylab = NULL,
                            custom_theme = ggplot2::theme_bw()) {

  # 1. Tidy Eval
  target_var <- rlang::enquo(var)
  var_name_raw <- rlang::as_label(target_var)

  # 2. Pretty Label Lookup
  lookup <- c(
    "sst.month11" = "November Sea Surface Temperature",
    "sst.month10" = "October Sea Surface Temperature",
    "sst.month9" = "September Sea Surface Temperature",
    "sst.month8" = "August Sea Surface Temperature",
    "sst.month7" = "July Sea Surface Temperature",
    "sst.month6" = "June Sea Surface Temperature",
    "sst.month5"  = "May Sea Surface Temperature",
    "t.deep" = "Bottom Temperature",
    "t.shallow" = "Bottom Temperature",
    "t.150" = "Temperature at 150m",
    "t.200" = "Temperature at 200m",
    "t.250" = "Temperature at 250m",
    "t.300" = "Temperature at 300m",
    "tmax200.400" = "Maximum Temperature between 200 and 400m"
  )

  base_label <- if (var_name_raw %in% names(lookup)) lookup[var_name_raw] else var_name_raw

  # 3. Data Processing
  plot_df <- data %>%
    dplyr::filter(year >= years[1], year <= years[2], ear %in% ears) %>%
    dplyr::mutate(ear_str = as.character(ear))

  # Apply custom names if provided, otherwise default to "EAR #"
  if (!is.null(ear_names)) {
    plot_df <- plot_df %>%
      dplyr::mutate(ear_label = ifelse(ear_str %in% names(ear_names),
                                       ear_names[ear_str],
                                       paste("EAR", ear_str)))
  } else {
    plot_df <- plot_df %>%
      dplyr::mutate(ear_label = paste("EAR", ear_str))
  }

  # Ensure factors for plotting order
  plot_df <- plot_df %>%
    dplyr::mutate(ear_label = factor(ear_label)) %>%
    dplyr::select(year, ear_label, value = !!target_var) %>%
    dplyr::filter(!is.na(value))

  # 4. Multilingual Labeling
  translated_label <- rosettafish::en2fr(base_label, lang = lang, translate = (lang != "en"))
  if (grepl("sst|temp", var_name_raw, ignore.case = TRUE)) {
    translated_label <- paste0(translated_label, " (°C)")
  }

  final_xlab <- if(is.null(xlab)) rosettafish::en2fr("Year", lang = lang, translate = (lang != "en")) else xlab
  final_ylab <- if(is.null(ylab)) translated_label else ylab

  # 5. Build Plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = value, color = ear_label)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::labs(x = final_xlab, y = final_ylab, color = "Region") +
    custom_theme +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  if (!is.null(col_palette)) {
    p <- p + ggplot2::scale_color_manual(values = col_palette)
  }

  # 6. Trend Fitting
  if (fit_smooth && nrow(plot_df) > 5) {
    p <- p + ggplot2::geom_smooth(
      method = method,
      formula = formula,
      color = "black",
      se = TRUE,
      fill = "grey80",
      alpha = 0.4
    )
  }

  # 7. Faceting
  if (length(unique(plot_df$ear_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ear_label, scales = "free_y") +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}



#' Plot GSLEA Ice and Seasonal Phenology by EAR
#'
#' @description
#' Visualizes ice-related and seasonal warming/cooling metrics from the gslea
#' package. This function specifically handles phenological variables such as
#' ice duration, ice timing, and temperature thresholds.
#'
#' @param data A data frame containing gslea ecosystem data (e.g., \code{EA.data}).
#' Must contain \code{year} and \code{ear} columns.
#' @param var The ice/seasonal variable to plot (unquoted, e.g., \code{ice.duration}, \code{start.10}).
#' @param ears Vector of EAR identifiers (0-7, 50, 10/1). Defaults to \code{0}.
#' @param years Numeric vector of length 2: \code{c(start, end)}. Defaults to \code{c(1990, 2023)}.
#' @param lang Language for labels: \code{"en"}, \code{"fr"}, or \code{"both"}.
#' @param fit_smooth Logical. If \code{TRUE}, fits a smoother (GAM by default).
#' @param method The smoothing method (e.g., \code{"gam"}, \code{"lm"}).
#' @param formula The smoothing formula. Defaults to \code{y ~ s(x, bs = "cs")}.
#' @param col_palette A character vector of colors for the EARs.
#' @param ear_names Optional named character vector to override EAR numbers with names.
#' @param xlab Optional custom x-axis label.
#' @param ylab Optional custom y-axis label.
#' @param custom_theme A \code{ggplot2} theme object. Defaults to \code{theme_bw()}.
#'
#' @details
#' This function streamlines the plotting of seasonal timing metrics of physical variables (e.g. ice and wamring/cooling). It
#' automatically detects and appends the correct units based on the variable name:
#' \itemize{
#'   \item \strong{Week of Year:} Applied to variables containing "start" or "decrease"
#'   (e.g., \code{start.10}, \code{decrease.12}).
#'   \item \strong{Day of Year (DOY):} Applied to \code{first.ice} and \code{last.ice}.
#'   \item \strong{Days:} Applied to variables containing "duration".
#' }
#' If multiple regions are selected, the function uses \code{facet_wrap} with
#' independent y-scales to accommodate different regional timing baselines.
#'
#' @return A \code{ggplot} object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs theme_bw theme element_blank facet_wrap scale_color_manual
#' @importFrom dplyr filter select mutate
#' @importFrom rosettafish en2fr
#' @importFrom rlang enquo as_label !!
#' @importFrom mgcv s
#'
#' @examples
#' \dontrun{
#' # Plotting Ice Duration for the whole Gulf
#' plot_gslea_ice(EA.data, ice.duration, ears = 0)
#'
#' # Comparing warming onset (Week of Year) across regions
#' plot_gslea_ice(EA.data, start.10, ears = c(1, 2, 3), years = c(2000, 2023))
#' }
#' @export
plot_gslea_ice <- function(data,
                           var,
                           ears = 0,
                           years = c(1990, 2023),
                           lang = "en",
                           fit_smooth = TRUE,
                           method = "gam",
                           formula = y ~ s(x, bs = "cs"),
                           col_palette = NULL,
                           ear_names = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           custom_theme = ggplot2::theme_bw()) {

  # 1. Tidy Eval
  target_var <- rlang::enquo(var)
  var_name_raw <- rlang::as_label(target_var)

  # 2. Ice/Seasonal Lookup Table
  lookup <- c(
    "first.ice"    = "Date of First Ice",
    "last.ice"     = "Date of Last Ice",
    "ice.duration" = "Ice Duration",
    "ice.max"      = "Date of Maximum Ice Coverage",
    "start.10"     = "Start of Warming (10°C)",
    "start.12"     = "Start of Warming (12°C)",
    "decrease.10"  = "Start of Cooling (10°C)",
    "decrease.12"  = "Start of Cooling (12°C)"
  )

  base_label <- if (var_name_raw %in% names(lookup)) lookup[var_name_raw] else var_name_raw

  # 3. Unit Detection (Specific to Ice/Phenology)
  unit <- ""
  if (grepl("start|decrease", var_name_raw, ignore.case = TRUE)) {
    unit <- " (Week of Year)"
  } else if (grepl("duration", var_name_raw, ignore.case = TRUE)) {
    unit <- " (Days)"
  } else if (grepl("first|last", var_name_raw, ignore.case = TRUE)) {
    unit <- " (Day of Year)"
  } else if (grepl("max", var_name_raw, ignore.case = TRUE)) {
    unit <- " (Day of Year)"
  }

  # 4. Data Processing
  plot_df <- data %>%
    dplyr::filter(year >= years[1], year <= years[2], ear %in% ears) %>%
    dplyr::mutate(ear_str = as.character(ear))

  # Regional Naming Logic
  if (!is.null(ear_names)) {
    plot_df <- plot_df %>%
      dplyr::mutate(ear_label = ifelse(ear_str %in% names(ear_names),
                                       ear_names[ear_str],
                                       paste("EAR", ear_str)))
  } else {
    plot_df <- plot_df %>%
      dplyr::mutate(ear_label = paste("EAR", ear_str))
  }

  plot_df <- plot_df %>%
    dplyr::mutate(ear_label = factor(ear_label)) %>%
    dplyr::select(year, ear_label, value = !!target_var) %>%
    dplyr::filter(!is.na(value))

  # 5. Multilingual Labeling
  translated_label <- rosettafish::en2fr(base_label, lang = lang, translate = (lang != "en"))
  final_ylab <- if(is.null(ylab)) paste0(translated_label, unit) else ylab
  final_xlab <- if(is.null(xlab)) rosettafish::en2fr("Year", lang = lang, translate = (lang != "en")) else xlab

  # 6. Build Plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = value, color = ear_label)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::labs(x = final_xlab, y = final_ylab, color = "Region") +
    custom_theme +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  if (!is.null(col_palette)) {
    p <- p + ggplot2::scale_color_manual(values = col_palette)
  }

  # 7. Trend Fitting
  if (fit_smooth && nrow(plot_df) > 5) {
    p <- p + ggplot2::geom_smooth(
      method = method,
      formula = formula,
      color = "black",
      se = TRUE,
      fill = "grey80",
      alpha = 0.4
    )
  }

  # 8. Faceting
  if (length(unique(plot_df$ear_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ear_label, scales = "free_y") +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}
