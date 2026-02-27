# Functions to make graphs and tables for the Gulf of St. Lawrence physical
# and biological environment. Using data from gslea which is a dependency
# of this package.

#' Plot Temperature data from gslea by EAR
#'
#' @description
#' Visualizes temperature data from the gslea package using long-format data. Provides billingual plots.
#' Can visualize with points connnected by line or with a smoothed GAM, plots line by default.
#'
#'
#' @param data Long-format data frame containing \code{year}, \code{EAR}, \code{variable}, and \code{value}.
#' @param var Temperature variable to plot (unquoted, e.g., \code{sst.month10}).
#' @param EARs Vector of EAR identifiers. Defaults to \code{0}.
#' @param groups Optional named list to aggregate EARs.
#' @param year_range Numeric vector \code{c(start, end)}. Defaults to \code{c(1990, 2023)}.
#' @param lang Language for labels: \code{"en"} or \code{"fr"}.
#' @param fit_smooth Logical. If \code{TRUE}, fits a GAM smoother.
#' @param show_line Logical. If \code{TRUE}, connects annual points with a line. Defaults to \code{TRUE}.
#' @param method Smoothing method. Defaults to \code{"gam"}.
#' @param formula Smoothing formula. Defaults to \code{y ~ s(x, bs = "cs", k = 15)}.
#' @param col_palette Optional character vector of colors for the regions.
#' @param ear_names Optional named vector to override EAR numbers with names.
#' @param xlab,ylab Optional custom axis labels.
#' @param base_size Base font size for the plot. Defaults to \code{14}.
#' @param facet_scales Character. Control facet scales: \code{"free_y"} (default) or \code{"fixed"}.
#' @param custom_theme A ggplot2 theme. Defaults to \code{theme_bw()}.
#'
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' # Compare surface warming in broad regions with custom colors and fixed scales
#' my_regions <- list("Northern GSL" = 1:4, "Southern GSL" = c(5, 6, 50))
#'
#' plot_gslea_temperature(EA.data, sst.month10,
#'                        groups = my_regions,
#'                        year_range = c(2000, 2023),
#'                        facet_scales = "fixed",
#'                        col_palette = c("Northern GSL" = "royalblue",
#'                                        "Southern GSL" = "firebrick"))
#' }
#' @export
plot_gslea_temperature <- function(data, var, EARs = 0, groups = NULL, year_range = c(1990, 2023),
                                   lang = "en", fit_smooth = FALSE, show_line = TRUE,
                                   method = "gam", formula = y ~ s(x, bs = "cs", k = 15),
                                   col_palette = NULL, ear_names = NULL,
                                   xlab = NULL, ylab = NULL, base_size = 16,
                                   facet_scales = "free_y",
                                   custom_theme = ggplot2::theme_bw()) {

  target_var <- rlang::enquo(var)
  var_name_raw <- rlang::as_label(target_var)

  # Full Temperature Dictionary
  lookup <- list(
    en = c("sst"="Annual Sea Surface Temperature", "sst.anomaly"="Anomaly in Annual Sea Surface Temperature",
           "sst.month5"="May Sea Surface Temperature", "sst.month6"="June Sea Surface Temperature",
           "sst.month7"="July Sea Surface Temperature", "sst.month8"="August Sea Surface Temperature",
           "sst.month9"="September Sea Surface Temperature", "sst.month10"="October Sea Surface Temperature",
           "sst.month11"="November Sea Surface Temperature", "t.deep"="Bottom Temperature",
           "t.shallow"="Bottom Temperature", "t.150"="Temperature at 150m", "t.200"="Temperature at 200m",
           "t.250"="Temperature at 250m", "t.300"="Temperature at 300m",
           "tmax200.400"="Maximum Temperature (200-400m)",
           "Northern"="Northern GSL", "Southern"="Southern GSL"),
    fr = c("sst"="Température de surface de la mer", "sst.anomaly"="Anomalie de la température annuelle de la surface de la mer",
           "sst.month5"="Température de surface de la mer en mai", "sst.month6"="Température de surface de la mer en juin",
           "sst.month7"="Température de surface de la mer en juillet", "sst.month8"="Température de surface de la mer en août",
           "sst.month9"="Température de surface de la mer en septembre", "sst.month10"="Température de surface de la mer en octobre",
           "sst.month11"="Température de surface de la mer en novembre", "t.deep"="Température du fond",
           "t.shallow"="Température du fond", "t.150"="Température à 150m", "t.200"="Température à 200m",
           "t.250"="Température à 250m", "t.300"="Température à 300m",
           "tmax200.400"="Température maximale (200-400m)",
           "Northern"="Nord du GSL", "Southern"="Sud du GSL")
  )

  base_label <- if(var_name_raw %in% names(lookup[[lang]])) lookup[[lang]][var_name_raw] else var_name_raw

  all_ears_char <- if(!is.null(groups)) as.character(unlist(groups)) else as.character(EARs)

  plot_df <- data |>
    dplyr::mutate(EAR_tmp = as.character(EAR)) |>
    dplyr::filter(year >= year_range[1], year <= year_range[2], EAR_tmp %in% all_ears_char, variable == var_name_raw)

  if (!is.null(groups)) {
    group_map <- stack(groups) |>
      dplyr::mutate(EAR_tmp = as.character(values)) |>
      dplyr::rename(ear_label = ind)

    plot_df <- plot_df |>
      dplyr::inner_join(group_map, by = "EAR_tmp") |>
      dplyr::group_by(year, ear_label) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(ear_str = as.character(EAR),
                    ear_label = if(!is.null(ear_names) && ear_str %in% names(ear_names)) ear_names[ear_str] else paste("EAR", ear_str))
  }

  safe_translate <- function(x, language) {
    if(language != "fr") return(x)
    if(x %in% names(lookup$fr)) return(lookup$fr[x])
    res <- try(rosettafish::en2fr(x), silent = TRUE)
    if(inherits(res, "try-error")) return(x) else return(res)
  }

  plot_df <- plot_df |>
    dplyr::mutate(ear_label = purrr::map_chr(as.character(ear_label), \(x) safe_translate(x, lang))) |>
    dplyr::mutate(ear_label = factor(ear_label)) |>
    dplyr::filter(!is.na(value))

  final_xlab <- if(!is.null(xlab)) xlab else if(lang == "fr") "Année" else "Year"
  final_ylab <- if(is.null(ylab)) paste0(base_label, " (°C)") else ylab

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = value, color = ear_label))
  # Add line before points so points sit on top
  if (show_line) {
    p <- p + ggplot2::geom_line(alpha = 0.5, linewidth = 0.8)
  }

  p <- p + ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::labs(x = final_xlab, y = final_ylab, color = "Region") +
    custom_theme +
    ggplot2::theme(text = ggplot2::element_text(size = base_size),
                   axis.title = ggplot2::element_text(face = "bold"))

  if (!is.null(col_palette)) {
    p <- p + ggplot2::scale_color_manual(values = col_palette)
  } else if (length(unique(plot_df$ear_label)) == 1) {
    p <- p + ggplot2::scale_color_manual(values = "black")
  }

  if (fit_smooth && nrow(plot_df) > 5) {
    p <- p + ggplot2::geom_smooth(method = method, formula = formula, color = "black", se = TRUE, fill = "grey80", alpha = 0.4)
  }

  if (length(unique(plot_df$ear_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ear_label, scales = facet_scales) +
      ggplot2::theme(legend.position = "none")
  }
  return(p)
}

#' Plot GSLEA Ice and Seasonal Phenology by EAR
#'
#' @description
#' Visualizes ice and seasonal timing metrics from the gslea package.
#' Supports individual EARs or custom aggregated groups with controllable scales.
#' Can visualize with points connected by line or with a smoothed GAM.
#'
#' @param data Long-format data frame containing \code{year}, \code{EAR}, \code{variable}, and \code{value}.
#' @param var Ice/seasonal variable to plot (unquoted, e.g., \code{ice.duration}).
#' @param EARs Vector of EAR identifiers. Defaults to \code{0}.
#' @param groups Optional named list to aggregate EARs.
#' @param year_range Numeric vector \code{c(start, end)}. Defaults to \code{c(1990, 2023)}.
#' @param lang Language for labels: \code{"en"} or \code{"fr"}.
#' @param fit_smooth Logical. If \code{TRUE}, adds a GAM smoother.
#' @param show_line Logical. If \code{TRUE}, connects annual points with a line. Defaults to \code{TRUE}.
#' @param method Smoothing method. Defaults to \code{"gam"}.
#' @param formula Smoothing formula. Defaults to \code{y ~ s(x, bs = "cs", k = 15)}.
#' @param col_palette Optional character vector of colors for the regions.
#' @param ear_names Optional named vector to override EAR numbers with names.
#' @param xlab,ylab Optional custom axis labels.
#' @param base_size Base font size for the plot. Defaults to \code{14}.
#' @param facet_scales Character. Control facet scales: \code{"free_y"} (default) or \code{"fixed"}.
#' @param custom_theme A ggplot2 theme. Defaults to \code{theme_bw()}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Compare ice duration between Northern and Southern groups with lines and points
#' plot_gslea_ice(EA.data, ice.duration,
#'                year_range = c(2000, 2020),
#'                groups = list("Northern" = 1:4, "Southern" = 5:6),
#'                lang = "fr",
#'                show_line = TRUE)
#' }
#' @export
plot_gslea_ice <- function(data, var, EARs = 0, groups = NULL, year_range = c(1990, 2023),
                           lang = "en", fit_smooth = FALSE, show_line = TRUE,
                           method = "gam", formula = y ~ s(x, bs = "cs", k = 15),
                           col_palette = NULL, ear_names = NULL,
                           xlab = NULL, ylab = NULL, base_size = 14,
                           facet_scales = "free_y",
                           custom_theme = ggplot2::theme_bw()) {

  target_var <- rlang::enquo(var)
  var_name_raw <- rlang::as_label(target_var)

  # 1. Dictionary
  lookup <- list(
    en = c("first.ice"="Date of First Ice", "last.ice"="Date of Last Ice", "ice.duration"="Ice Duration",
           "ice.max"="Maximum Ice Coverage", "start.10"="Start of Warming (10°C)", "start.12"="Start of Warming (12°C)",
           "decrease.10"="Start of Cooling (10°C)", "decrease.12"="Start of Cooling (12°C)",
           "Northern"="Northern GSL", "Southern"="Southern GSL"),
    fr = c("first.ice"="Date de première glace", "last.ice"="Date de dernière glace", "ice.duration"="Durée de la glace",
           "ice.max"="Couverture de glace maximale", "start.10"="Début du réchauffement (10°C)", "start.12"="Début du réchauffement (12°C)",
           "decrease.10"="Début du refroidissement (10°C)", "decrease.12"="Début du refroidissement (12°C)",
           "Northern"="Nord du GSL", "Southern"="Sud du GSL")
  )

  base_label <- if(var_name_raw %in% names(lookup[[lang]])) lookup[[lang]][var_name_raw] else var_name_raw

  # 2. Units
  unit <- if (lang == "fr") {
    if(grepl("start|decrease", var_name_raw)) " (Semaine de l'année)" else
      if(grepl("duration", var_name_raw)) " (Jours)" else
        if(grepl("first|last", var_name_raw)) " (Jour de l'année)" else ""
  } else {
    if(grepl("start|decrease", var_name_raw)) " (Week of Year)" else
      if(grepl("duration", var_name_raw)) " (Days)" else
        if(grepl("first|last", var_name_raw)) " (Day of Year)" else ""
  }

  # 3. Filtering & Grouping
  all_ears_char <- if(!is.null(groups)) as.character(unlist(groups)) else as.character(EARs)

  plot_df <- data |>
    dplyr::mutate(EAR_tmp = as.character(EAR)) |>
    dplyr::filter(year >= year_range[1], year <= year_range[2], EAR_tmp %in% all_ears_char, variable == var_name_raw)

  if (!is.null(groups)) {
    group_map <- stack(groups) |>
      dplyr::mutate(EAR_tmp = as.character(values)) |>
      dplyr::rename(ear_label = ind)

    plot_df <- plot_df |>
      dplyr::inner_join(group_map, by = "EAR_tmp") |>
      dplyr::group_by(year, ear_label) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(ear_str = as.character(EAR),
                    ear_label = if(!is.null(ear_names) && ear_str %in% names(ear_names)) ear_names[ear_str] else paste("EAR", ear_str))
  }

  # 4. Translation
  safe_translate <- function(x, language) {
    if(language != "fr") return(x)
    if(x %in% names(lookup$fr)) return(lookup$fr[x])
    res <- try(rosettafish::en2fr(x), silent = TRUE)
    if(inherits(res, "try-error")) return(x) else return(res)
  }

  plot_df <- plot_df |>
    dplyr::mutate(ear_label = purrr::map_chr(as.character(ear_label), \(x) safe_translate(x, lang))) |>
    dplyr::mutate(ear_label = factor(ear_label)) |>
    dplyr::filter(!is.na(value))

  # 5. Build Plot
  final_xlab <- if(!is.null(xlab)) xlab else if(lang == "fr") "Année" else "Year"
  final_ylab <- if(is.null(ylab)) paste0(base_label, unit) else ylab

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = value, color = ear_label))

  # Add connecting lines
  if (show_line) {
    p <- p + ggplot2::geom_line(alpha = 0.5, linewidth = 0.8)
  }

  p <- p +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::labs(x = final_xlab, y = final_ylab, color = "Region") +
    custom_theme +
    ggplot2::theme(text = ggplot2::element_text(size = base_size),
                   axis.title = ggplot2::element_text(face = "bold"))

  if (!is.null(col_palette)) {
    p <- p + ggplot2::scale_color_manual(values = col_palette)
  } else if (length(unique(plot_df$ear_label)) == 1) {
    p <- p + ggplot2::scale_color_manual(values = "black")
  }

  if (fit_smooth && nrow(plot_df) > 5) {
    p <- p + ggplot2::geom_smooth(method = method, formula = formula, color = "black",
                                  se = TRUE, fill = "grey80", alpha = 0.4)
  }

  if (length(unique(plot_df$ear_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ear_label, scales = facet_scales) +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}

#' Plot GSLEA Planktonic Metrics by EAR
#'
#' @description
#' Visualizes planktonic variables (phytoplankton and zooplankton) from the gslea
#' package. This function handles complex units (e.g., 10³ ind m⁻², mg chla m⁻²)
#' and provides bilingual labels. It can visualize with points connected by lines
#' (default) and/or a smoothed GAM trend.
#'
#' @param data Long-format data frame containing \code{year}, \code{EAR}, \code{variable}, and \code{value}.
#' @param var Planktonic variable to plot (unquoted, e.g., \code{calanus.finmarchicus.annual}).
#' @param EARs Vector of EAR identifiers to include. Defaults to \code{0} (Entire Gulf).
#' @param groups Optional named list to aggregate EARs (e.g., \code{list("Northern" = 1:4)}).
#' @param year_range Numeric vector \code{c(start, end)}. Defaults to \code{c(1990, 2023)}.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param fit_smooth Logical. If \code{TRUE}, adds a GAM smoother to the plot.
#' @param show_line Logical. If \code{TRUE}, connects annual points with a line. Defaults to \code{TRUE}.
#' @param method Smoothing method to use. Defaults to \code{"gam"}.
#' @param formula Smoothing formula. Defaults to \code{y ~ s(x, bs = "cs", k = 15)}.
#' @param col_palette Optional character vector of colors for the regions/groups.
#' @param ear_names Optional named vector to override EAR numbers with custom names.
#' @param xlab,ylab Optional strings to override default axis labels.
#' @param base_size Base font size for the ggplot2 theme. Defaults to \code{14}.
#' @param facet_scales Character. Control facet scales: \code{"free_y"} (default) or \code{"fixed"}.
#' @param custom_theme A ggplot2 theme object. Defaults to \code{theme_bw()}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # 1. Simple plot for a specific EAR
#' plot_gslea_plankton(EA.data, dw2_t.annual, EARs = 5)
#'
#' # 2. Compare Calanus abundance between regions with custom colors
#' region_list <- list("Northern GSL" = 1:4, "Southern GSL" = c(5, 6, 50))
#'
#' plot_gslea_plankton(data = EA.data,
#'                     var = calanus.finmarchicus.annual,
#'                     groups = region_list,
#'                     year_range = c(2005, 2023),
#'                     lang = "en",
#'                     col_palette = c("Northern GSL" = "darkgreen",
#'                                     "Southern GSL" = "orange"))
#'
#' # 3. Plot phytoplankton bloom start date in French
#' plot_gslea_plankton(EA.data, start, EARs = 1:8, lang = "fr")
#' }
#' @export
plot_gslea_plankton <- function(data, var, EARs = 0, groups = NULL, year_range = c(1990, 2023),
                                lang = "en", fit_smooth = FALSE, show_line = TRUE,
                                method = "gam", formula = y ~ s(x, bs = "cs", k = 15),
                                col_palette = NULL, ear_names = NULL,
                                xlab = NULL, ylab = NULL, base_size = 14,
                                facet_scales = "free_y",
                                custom_theme = ggplot2::theme_bw()) {

  target_var <- rlang::enquo(var)
  var_name_raw <- rlang::as_label(target_var)

  # 1. Dictionary
  lookup <- list(
    en = c("calanus.finmarchicus.annual"="Abundance of Calanus finmarchicus (Annual)",
           "calanus.finmarchicus.early_summer"="Abundance of Calanus finmarchicus (Early Summer)",
           "calanus.finmarchicus.fall"="Abundance of Calanus finmarchicus (Fall)",
           "calanus.hyperboreus.annual"="Abundance of Calanus hyperboreus (Annual)",
           "calanus.hyperboreus.early_summer"="Abundance of Calanus hyperboreus (Early Summer)",
           "calanus.hyperboreus.fall"="Abundance of Calanus hyperboreus (Fall)",
           "chl0_100.annual"="Chlorophyll-a weight (0-100m, Annual)",
           "chl0_100.early_summer"="Chlorophyll-a weight (Early Summer)",
           "chl0_100.fall"="Chlorophyll-a weight (Fall)",
           "chl0_100.late_summer"="Chlorophyll-a weight (Late Summer)",
           "dw2_t.annual"="Total Dry Weight of Zooplankton (Annual)",
           "dw2_t.early_summer"="Total Dry Weight (Early Summer)",
           "dw2_t.fall"="Total Dry Weight (Fall)",
           "cold.annual"="Cold/Arctic Species Abundance (Annual)",
           "warm.annual"="Warm-water Species Abundance (Annual)",
           "largecal.annual"="Abundance of Large Calanus (Annual)",
           "smallcal.annual"="Abundance of Small Calanus (Annual)",
           "pseudocalanus.annual"="Abundance of Pseudocalanus (Annual)",
           "non.copepods.annual"="Abundance of Non-copepod Zooplankton (Annual)",
           "start"="Phytoplankton Bloom Start",
           "magnitude"="Phytoplankton Bloom Magnitude"),
    fr = c("calanus.finmarchicus.annual"="Abondance de Calanus finmarchicus (Annuel)",
           "calanus.finmarchicus.early_summer"="Abondance de Calanus finmarchicus (Début d'été)",
           "calanus.finmarchicus.fall"="Abondance de Calanus finmarchicus (Automne)",
           "calanus.hyperboreus.annual"="Abondance de Calanus hyperboreus (Annuel)",
           "calanus.hyperboreus.early_summer"="Abondance de Calanus hyperboreus (Début d'été)",
           "calanus.hyperboreus.fall"="Abondance de Calanus hyperboreus (Automne)",
           "chl0_100.annual"="Poids de chlorophylle-a (0-100m, Annuel)",
           "chl0_100.early_summer"="Poids de chlorophylle-a (Début d'été)",
           "chl0_100.fall"="Poids de chlorophylle-a (Automne)",
           "chl0_100.late_summer"="Poids de chlorophylle-a (Fin d'été)",
           "dw2_t.annual"="Poids sec total du zooplancton (Annuel)",
           "dw2_t.early_summer"="Poids sec total (Début d'été)",
           "dw2_t.fall"="Poids sec total (Automne)",
           "cold.annual"="Abondance des espèces froides/arctiques (Annuel)",
           "warm.annual"="Abondance des espèces d'eau chaude (Annuel)",
           "largecal.annual"="Abondance des grands Calanus (Annuel)",
           "smallcal.annual"="Abondance des petits Calanus (Annuel)",
           "pseudocalanus.annual"="Abondance de Pseudocalanus (Annuel)",
           "non.copepods.annual"="Abondance de zooplancton non-copépode (Annuel)",
           "start"="Début de la floraison phytoplanctonique",
           "magnitude"="Magnitude de la floraison phytoplanctonique")
  )

  base_label <- if(var_name_raw %in% names(lookup[[lang]])) lookup[[lang]][var_name_raw] else var_name_raw

  # 2. Refined Unit Logic
  unit <- if(grepl("dw2_t", var_name_raw)) " (g m⁻²)" else
    if(grepl("chl0_100|magnitude", var_name_raw)) " (mg chla m⁻²)" else
      if(var_name_raw == "start") { if(lang == "fr") " (Jour de l'année)" else " (Day of Year)" } else
        if(grepl("^ci\\.|^civ\\.", var_name_raw)) "" else " (10³ ind m⁻²)"

  # 3. Filtering & Aggregating
  all_ears_char <- if(!is.null(groups)) as.character(unlist(groups)) else as.character(EARs)

  plot_df <- data |>
    dplyr::mutate(EAR_tmp = as.character(EAR)) |>
    dplyr::filter(year >= year_range[1], year <= year_range[2], EAR_tmp %in% all_ears_char, variable == var_name_raw)

  if (!is.null(groups)) {
    group_map <- stack(groups) |>
      dplyr::mutate(EAR_tmp = as.character(values)) |>
      dplyr::rename(ear_label = ind)

    plot_df <- plot_df |>
      dplyr::inner_join(group_map, by = "EAR_tmp") |>
      dplyr::group_by(year, ear_label) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(ear_str = as.character(EAR),
                    ear_label = if(!is.null(ear_names) && ear_str %in% names(ear_names)) ear_names[ear_str] else paste("EAR", ear_str))
  }

  # 4. Translation & Factors
  safe_translate <- function(x, language) {
    if(language != "fr") return(x)
    if(x %in% names(lookup$fr)) return(lookup$fr[x])
    res <- try(rosettafish::en2fr(x), silent = TRUE)
    if(inherits(res, "try-error")) return(x) else return(res)
  }

  plot_df <- plot_df |>
    dplyr::mutate(ear_label = purrr::map_chr(as.character(ear_label), \(x) safe_translate(x, lang))) |>
    dplyr::mutate(ear_label = factor(ear_label)) |>
    dplyr::filter(!is.na(value))

  # 5. Build Plot
  final_xlab <- if(!is.null(xlab)) xlab else if(lang == "fr") "Année" else "Year"
  final_ylab <- if(is.null(ylab)) paste0(base_label, unit) else ylab

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = value, color = ear_label))

  # NEW: Add connecting lines before points
  if (show_line) {
    p <- p + ggplot2::geom_line(alpha = 0.5, linewidth = 0.8)
  }

  p <- p +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::labs(x = final_xlab, y = final_ylab, color = "Region") +
    custom_theme +
    ggplot2::theme(text = ggplot2::element_text(size = base_size),
                   axis.title = ggplot2::element_text(face = "bold"))

  if (!is.null(col_palette)) {
    p <- p + ggplot2::scale_color_manual(values = col_palette)
  } else if (length(unique(plot_df$ear_label)) == 1) {
    p <- p + ggplot2::scale_color_manual(values = "black")
  }

  if (fit_smooth && nrow(plot_df) > 5) {
    p <- p + ggplot2::geom_smooth(method = method, formula = formula, color = "black",
                                  se = TRUE, fill = "grey80", alpha = 0.4)
  }

  if (length(unique(plot_df$ear_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ear_label, scales = facet_scales) +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}


#' Calculate Anomalies from a Baseline Mean
#'
#' @description
#' Calculates the anomaly of a variable relative to a user-specified baseline
#' period. Returns a focused data frame containing only the year, the original
#' value, the baseline mean used, and the resulting anomaly.
#'
#' @param data A data frame containing at least \code{year} and \code{value} columns.
#' @param baseline_range Numeric vector \code{c(start, end)} defining the period
#'   used to calculate the mean. This argument is required.
#' @param year_range Optional numeric vector \code{c(start, end)} to filter the
#'   returned data. If \code{NULL}, returns all years in the input data.
#' @param quiet Logical. If \code{FALSE} (default), prints the baseline mean
#'   to the console via a message.
#'
#' @return A focused data frame (tibble) with columns: \code{year}, \code{value},
#'   \code{baseline_mean}, and \code{anomaly}.
#'
#' @examples
#' \dontrun{
#' # Example 1: Using the native R pipe (|>)
#' sst_anom <- EA.data |>
#'   dplyr::filter(variable == "sst.month10", EAR == 0) |>
#'   calc_anomaly(baseline_range = c(1991, 2020))
#'
#' # Example 2: Using base R subset() and assignment
#' physical_data <- subset(EA.data, variable == "t.deep" & EAR == 1)
#' deep_t_anom <- calc_anomaly(data = physical_data,
#'                             baseline_range = c(1995, 2015))
#'
#' # View the clean result
#' head(deep_t_anom)
#' }
#' @export
calc_anomaly <- function(data,
                         baseline_range,
                         year_range = NULL,
                         quiet = FALSE) {

  # 1. Input Validation
  if (missing(baseline_range)) {
    stop("Argument 'baseline_range' is missing. You must specify c(start_year, end_year).")
  }

  if (!all(c("year", "value") %in% names(data))) {
    stop("The data frame must contain 'year' and 'value' columns.")
  }

  # 2. Calculate the baseline mean (climatology)
  climatology_mean <- data |>
    dplyr::filter(year >= baseline_range[1], year <= baseline_range[2]) |>
    dplyr::summarise(m = mean(value, na.rm = TRUE)) |>
    dplyr::pull(m)

  # 3. Handle cases where the baseline period has no data
  if (is.na(climatology_mean)) {
    stop("The calculated baseline mean is NA. Ensure the 'baseline_range' overlaps with your data.")
  }

  if (!quiet) {
    message(paste("Calculated baseline mean:", round(climatology_mean, 3)))
  }

  # 4. Filter for specific output years if requested
  out_df <- data
  if (!is.null(year_range)) {
    out_df <- out_df |>
      dplyr::filter(year >= year_range[1], year <= year_range[2])
  }

  # 5. Construct the focused result
  result <- out_df |>
    dplyr::transmute(
      year = year,
      value = value,
      baseline_mean = climatology_mean,
      anomaly = value - climatology_mean
    ) |>
    dplyr::filter(!is.na(value))

  return(result)
}

#' Plot Generic Anomaly with Standardized Scorecard
#'
#' @description
#' Creates a publication-quality anomaly plot with a standardized color scorecard.
#' Handles extreme values via clamping and allows for customizable X-axis labeling intervals.
#'
#' @param data Data frame containing \code{year} and the anomaly values.
#' @param value_col Unquoted name of the anomaly column for the bars.
#' @param var_col Optional unquoted name of the grouping column for stacked bars.
#' @param composite_data Optional data frame for the line and scorecard.
#' @param comp_value_col Unquoted name of the column for the line/scorecard.
#' @param show_composite Logical. Should the composite line be drawn?
#' @param show_scorecard Logical. Should the heatmap scorecard be drawn?
#' @param year_range Numeric vector \code{c(start, end)}.
#' @param x_breaks_interval Numeric. Interval for year labels (e.g., 2 for every other year). Defaults to 1.
#' @param colors Optional named vector for bar colors.
#' @param lang Language: \code{"en"} (default) or \code{"fr"}.
#' @param y_label Optional string for the y-axis title.
#' @param y_breaks Optional numeric vector for y-axis tick marks.
#' @param base_size Numeric. Base font size for scaling text. Defaults to 11.
#'
#' @return A combined plot object (cowplot).
#'
#' @examples
#' \dontrun{
#' # Example 1: Standard SST Anomaly for EAR 5 (Southern Gulf)
#' sst_anom <- EA.data |>
#'   dplyr::filter(variable == "sst.month10", EAR == 5) |>
#'   calc_anomaly(baseline_range = c(1991, 2020))
#'
#' plot_anomaly(sst_anom, value_col = anomaly, comp_value_col = anomaly,
#'              y_label = "SST Anomaly (°C)")
#'
#'
#' # Example 2: Simple bar-only look (e.g., for biological indices)
#' calanus_anom <- EA.data |>
#'   dplyr::filter(variable == "calanus.finmarchicus.annual", EAR == 5) |>
#'   calc_anomaly(baseline_range = c(1999, 2015))
#'
#' plot_anomaly(data = calanus_anom,
#'              value_col = anomaly,
#'              show_scorecard = FALSE,
#'              show_composite = FALSE,
#'              y_label = "Standardized Anomaly")
#' }
#' @export
plot_anomaly <- function(data,
                         value_col,
                         var_col = NULL,
                         composite_data = NULL,
                         comp_value_col = NULL,
                         show_composite = TRUE,
                         show_scorecard = TRUE,
                         year_range = NULL,
                         x_breaks_interval = 1,
                         colors = NULL,
                         lang = "en",
                         y_label = NULL,
                         y_breaks = ggplot2::waiver(),
                         base_size = 11) {

  # 1. Setup & Translation
  val_enquo <- rlang::enquo(value_col)
  var_enquo <- rlang::enquo(var_col)
  comp_val_enquo <- rlang::enquo(comp_value_col)

  terms <- list(
    en = c(anom = "Anomalies", yr = "Year", depth = "Depth", area = "Area"),
    fr = c(anom = "Anomalies", yr = "Année", depth = "Profondeur", area = "Zone")
  )

  translate_term <- function(x) {
    if(is.null(x)) return(NULL)
    low_x <- tolower(x)
    if(low_x %in% names(terms[[lang]])) return(terms[[lang]][[low_x]])
    return(x)
  }

  if (is.null(composite_data)) composite_data <- data

  all_years <- sort(unique(c(data$year, composite_data$year)))
  x_lims <- if (!is.null(year_range)) year_range else range(all_years, na.rm = TRUE)

  label_breaks <- seq(x_lims[1], x_lims[2], by = x_breaks_interval)
  grid_breaks <- seq(x_lims[1] - 0.5, x_lims[2] + 0.5, by = 1)

  data <- data |> dplyr::filter(year >= x_lims[1], year <= x_lims[2])
  composite_data <- composite_data |> dplyr::filter(year >= x_lims[1], year <= x_lims[2])
  final_y_label <- if(!is.null(y_label)) y_label else terms[[lang]][["anom"]]

  # 2. Top Plot
  p_top <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.7)

  if (!rlang::quo_is_null(var_enquo)) {
    p_top <- p_top +
      ggplot2::geom_bar(data = data,
                        ggplot2::aes(x = year, y = !!val_enquo, fill = as.factor(!!var_enquo)),
                        color = "black", stat = "identity", width = 0.8) +
      ggplot2::labs(fill = translate_term(rlang::as_label(var_enquo)))
  } else {
    p_top <- p_top +
      ggplot2::geom_bar(data = data,
                        ggplot2::aes(x = year, y = !!val_enquo),
                        fill = "grey70", color = "black", stat = "identity", width = 0.8)
  }

  if (show_composite && !rlang::quo_is_null(comp_val_enquo)) {
    p_top <- p_top +
      ggplot2::geom_line(data = composite_data,
                         ggplot2::aes(x = year, y = !!comp_val_enquo), linewidth = 1) +
      ggplot2::geom_point(data = composite_data,
                          ggplot2::aes(x = year, y = !!comp_val_enquo), size = 3)
  }

  p_top <- p_top +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::coord_cartesian(xlim = c(x_lims[1]-0.6, x_lims[2]+0.6), clip = "off") +
    ggplot2::scale_x_continuous(expand = c(0,0),
                                breaks = label_breaks,
                                minor_breaks = grid_breaks) +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::labs(y = final_y_label) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_line(color = "grey92"),
      plot.margin = ggplot2::margin(t = 5, r = 10, b = 0, l = 5)
    )

  if (!is.null(colors)) p_top <- p_top + ggplot2::scale_fill_manual(values = colors)

  # 3. Bottom Plot (Scorecard)
  if (show_scorecard && !rlang::quo_is_null(comp_val_enquo)) {

    score_palette <- c(grDevices::colorRampPalette(c("black","blue", "white"))(10),
                       grDevices::colorRampPalette(c("white", "red", "#5D0000"))(7))

    Breaks <- c(-5, -4.5, -4, -3.5, -3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3)
    Labels <- c("-5", "-4.5", "-4", "-3.5", "-3", "-2.5", "-2", "-1.5", "-1", "-0.5", "0", "0.5", "1", "1.5", "2", "2.5")

    score_df <- composite_data |>
      dplyr::mutate(
        original_val = !!comp_val_enquo,
        clamped_val = pmin(pmax(original_val, -5), 2.99),
        score_bin = cut(clamped_val, breaks = Breaks, labels = Labels, include.lowest = TRUE)
      )

    p_bot <- ggplot2::ggplot(score_df, ggplot2::aes(x = year, y = 1, fill = score_bin)) +
      ggplot2::geom_tile(color = "black", linewidth = 0.25) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", original_val)),
                         color = ifelse(abs(score_df$original_val) >= 2, "white", "black"),
                         size = base_size * 0.25) +
      ggplot2::scale_fill_manual(values = score_palette, drop = FALSE, na.value = "grey80") +
      ggplot2::coord_cartesian(xlim = c(x_lims[1]-0.6, x_lims[2]+0.6), clip = "off") +
      ggplot2::scale_x_continuous(expand = c(0,0), breaks = label_breaks) +
      ggplot2::labs(x = terms[[lang]][["yr"]]) + # Added X Label here
      ggplot2::theme_void(base_size = base_size) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = base_size * 0.8),
        axis.title.x = ggplot2::element_text(size = base_size, margin = ggplot2::margin(t = 10)), # Visible X label
        legend.position = "none",
        plot.margin = ggplot2::margin(t = -1, r = 10, b = 5, l = 5)
      )

    final_plot <- cowplot::plot_grid(p_top, p_bot, ncol = 1, rel_heights = c(10, 2.2), align = "v", axis = "lr")
  } else {
    # If no scorecard, restore the X axis to the top plot
    final_plot <- p_top + ggplot2::theme(
      axis.title.x = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      axis.ticks.x = ggplot2::element_line()
    ) + ggplot2::labs(x = terms[[lang]][["yr"]])
  }

  return(final_plot)
}

#' Plot Generic Anomaly with Standardized Scorecard With Composite Computed Internally
#'
#' @description
#' Creates a publication-quality anomaly plot with stacked bars and a scorecard.
#' The function automatically calculates a composite annual sum and a
#' standardized scorecard value (Annual Sum / SD of all Annual Sums).
#' The grouping legend (e.g., depth) can be customized with units.
#'
#' @param data Data frame containing \code{year} and the anomaly values.
#' @param value_col Unquoted name of the anomaly column for the bars.
#' @param var_col Optional unquoted name of the grouping column for stacked bars.
#' @param unit Optional string for units (e.g., "m"). Appends to legend items only.
#' @param show_composite Logical. Should the composite line be drawn?
#' @param show_scorecard Logical. Should the heatmap scorecard be drawn?
#' @param year_range Numeric vector \code{c(start, end)}.
#' @param x_breaks_interval Numeric. Interval for year labels. Defaults to 1.
#' @param colors Optional character vector of colors. Defaults to "YlGnBu".
#' @param lang Language: \code{"en"} (default) or \code{"fr"}.
#' @param y_label Optional string for the y-axis title. Defaults to "Anomalies".
#' @param y_breaks Optional numeric vector for y-axis tick marks.
#' @param base_size Numeric. Base font size. Defaults to 11.
#'
#' @return A combined ggplot/cowplot object.
#'
#' @examples
#' \dontrun{
#' # Example 1: Dissolved Oxygen anomalies at different depths
#' # The legend will show "200 m", "500 m", etc.
#' plot_anomaly_comp(data = do_data,
#'                   value_col = anomaly,
#'                   var_col = depth,
#'                   unit = "m",
#'                   y_label = "Dissolved Oxygen Anomaly (ml/L)")
#'
#' # Example 2: Simple temperature anomaly with scorecard but no stacks
#' plot_anomaly_comp(data = sst_data,
#'                   value_col = sst_anom,
#'                   show_composite = TRUE,
#'                   colors = "red")
#' }
#' @export
plot_anomaly_comp <- function(data,
                              value_col,
                              var_col = NULL,
                              unit = NULL,
                              show_composite = TRUE,
                              show_scorecard = TRUE,
                              year_range = NULL,
                              x_breaks_interval = 1,
                              colors = NULL,
                              lang = "en",
                              y_label = NULL,
                              y_breaks = ggplot2::waiver(),
                              base_size = 11) {

  # 1. Setup & Aggregation
  val_enquo <- rlang::enquo(value_col)
  var_enquo <- rlang::enquo(var_col)

  terms <- list(
    en = c(anom = "Anomalies", yr = "Year"),
    fr = c(anom = "Anomalies", yr = "Année")
  )

  # Internal Calculation: Composite Sum and Standardized Score
  composite_data <- data |>
    dplyr::group_by(year) |>
    dplyr::summarise(annual_sum = sum(!!val_enquo, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(std_score = annual_sum / stats::sd(annual_sum, na.rm = TRUE))

  all_years <- sort(unique(data$year))
  x_lims <- if (!is.null(year_range)) year_range else range(all_years, na.rm = TRUE)

  data <- data |> dplyr::filter(year >= x_lims[1], year <= x_lims[2])
  composite_data <- composite_data |> dplyr::filter(year >= x_lims[1], year <= x_lims[2])

  # 2. Labeling Logic
  final_y_label <- if(!is.null(y_label)) y_label else terms[[lang]][["anom"]]

  if (!rlang::quo_is_null(var_enquo)) {
    var_nm <- rlang::as_label(var_enquo)
    data[[var_nm]] <- as.character(data[[var_nm]])

    if (!is.null(unit)) {
      data[[var_nm]] <- ifelse(
        grepl(unit, data[[var_nm]], fixed = TRUE),
        data[[var_nm]],
        paste(data[[var_nm]], unit)
      )
    }

    # Sort legend items numerically to handle depths correctly
    unique_vals <- unique(data[[var_nm]])
    numeric_sort <- unique_vals[order(as.numeric(gsub("[^0-9.]", "", unique_vals)))]
    data[[var_nm]] <- factor(data[[var_nm]], levels = numeric_sort)
  }

  # 3. Top Plot Construction
  label_breaks <- seq(x_lims[1], x_lims[2], by = x_breaks_interval)
  grid_breaks <- seq(x_lims[1] - 0.5, x_lims[2] + 0.5, by = 1)

  p_top <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.7)

  if (!rlang::quo_is_null(var_enquo)) {
    p_top <- p_top +
      ggplot2::geom_bar(data = data,
                        ggplot2::aes(x = year, y = !!val_enquo, fill = !!var_enquo),
                        color = "black", stat = "identity", width = 0.8) +
      ggplot2::labs(fill = stringr::str_to_title(rlang::as_label(var_enquo)))

    if (is.null(colors)) {
      p_top <- p_top + ggplot2::scale_fill_brewer(palette = "YlGnBu")
    } else {
      p_top <- p_top + ggplot2::scale_fill_manual(values = colors)
    }
  } else {
    p_top <- p_top +
      ggplot2::geom_bar(data = data,
                        ggplot2::aes(x = year, y = !!val_enquo),
                        fill = "grey70", color = "black", stat = "identity", width = 0.8)
  }

  if (show_composite) {
    p_top <- p_top +
      ggplot2::geom_line(data = composite_data,
                         ggplot2::aes(x = year, y = annual_sum), linewidth = 0.8) +
      ggplot2::geom_point(data = composite_data,
                          ggplot2::aes(x = year, y = annual_sum), size = 2.5)
  }

  p_top <- p_top +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::coord_cartesian(xlim = c(x_lims[1]-0.6, x_lims[2]+0.6), clip = "off") +
    ggplot2::scale_x_continuous(expand = c(0,0), breaks = label_breaks, minor_breaks = grid_breaks) +
    ggplot2::scale_y_continuous(breaks = y_breaks) +
    ggplot2::labs(y = final_y_label) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_line(color = "grey92"),
      plot.margin = ggplot2::margin(t = 5, r = 10, b = 0, l = 5)
    )

  # 4. Scorecard Construction
  if (show_scorecard) {
    score_palette <- c(grDevices::colorRampPalette(c("black","blue", "white"))(10),
                       grDevices::colorRampPalette(c("white", "red", "#5D0000"))(7))

    Breaks <- c(-5, -4.5, -4, -3.5, -3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3)
    Labels <- c("-5", "-4.5", "-4", "-3.5", "-3", "-2.5", "-2", "-1.5", "-1", "-0.5", "0", "0.5", "1", "1.5", "2", "2.5")

    score_df <- composite_data |>
      dplyr::mutate(
        clamped_val = pmin(pmax(std_score, -5), 2.99),
        score_bin = cut(clamped_val, breaks = Breaks, labels = Labels, include.lowest = TRUE)
      )

    p_bot <- ggplot2::ggplot(score_df, ggplot2::aes(x = year, y = 1, fill = score_bin)) +
      ggplot2::geom_tile(color = "black", linewidth = 0.25) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", std_score)),
                         color = ifelse(abs(score_df$std_score) >= 2, "white", "black"),
                         size = base_size * 0.22) +
      ggplot2::scale_fill_manual(values = score_palette, drop = FALSE, na.value = "grey80") +
      ggplot2::coord_cartesian(xlim = c(x_lims[1]-0.6, x_lims[2]+0.6), clip = "off") +
      ggplot2::scale_x_continuous(expand = c(0,0), breaks = label_breaks) +
      ggplot2::labs(x = terms[[lang]][["yr"]]) +
      ggplot2::theme_void(base_size = base_size) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = base_size * 0.8),
        axis.title.x = ggplot2::element_text(size = base_size, margin = ggplot2::margin(t = 10)),
        legend.position = "none",
        plot.margin = ggplot2::margin(t = -1, r = 10, b = 5, l = 5)
      )

    final_plot <- cowplot::plot_grid(p_top, p_bot, ncol = 1, rel_heights = c(10, 2.2), align = "v", axis = "lr")
  } else {
    final_plot <- p_top + ggplot2::theme(
      axis.title.x = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      axis.ticks.x = ggplot2::element_line()
    ) + ggplot2::labs(x = terms[[lang]][["yr"]])
  }

  return(final_plot)
}


#' Plot Stock Status Heatmap Relative to LRP for Gulf of St. Lawrence Ecosystem Report
#'
#' @description
#' A highly flexible heatmap visualizing stock status with RdYlGn palette.
#' Includes full bilingual support for species and management areas.
#'
#' @param data A data frame containing stock status data. Defaults to \code{EA.extra.data$stock.status}.
#' @param year_col Unquoted name of column for year. Defaults to \code{Year}.
#' @param stock_col Unquoted name of column for stock names. Defaults to \code{Stock}.
#' @param status_col Unquoted name of column for status values. Defaults to \code{status.LRP}.
#' @param lang Character string for language: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Optional numeric vector \code{c(start, end)}.
#' @param show_title Logical. Defaults to \code{TRUE}.
#' @param base_size Numeric. Base font size. Defaults to 12.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # 1. Simple plot using package data
#' plot_stock_status_heatmap(lang = "en", year_range = c(1994, 2025))
#'
#' # 2. French version with title removed for a report
#' plot_stock_status_heatmap(lang = "fr", show_title = FALSE)
#'
#' # 3. Using a custom external dataset
#' plot_stock_status_heatmap(my_data, year_col = Annee, stock_col = Espece, status_col = Ratio)
#' }
#'
#' @export
plot_stock_status_heatmap <- function(data = NULL,
                                      year_col = Year,
                                      stock_col = Stock,
                                      status_col = status.LRP,
                                      lang = "en",
                                      year_range = NULL,
                                      show_title = TRUE,
                                      base_size = 12) {

  # 1. Data Setup
  if (is.null(data)) {
    if (!exists("EA.extra.data")) stop("EA.extra.data not found.")
    data <- EA.extra.data$stock.status
  }

  year_enquo   <- rlang::enquo(year_col)
  stock_enquo  <- rlang::enquo(stock_col)
  status_enquo <- rlang::enquo(status_col)

  df <- data |>
    dplyr::rename(yr = !!year_enquo, stk = !!stock_enquo, val = !!status_enquo)

  # 2. DEFINITIVE ORDERING LIST
  target_order <- c(
    "Northern shrimp-Anticosti",
    "Northern shrimp-Esquiman",
    "Northern shrimp-Estuary",
    "Northern shrimp-Sept-Iles",
    "Snow crab - sGSL",
    "Atlantic halibut - 4RST",
    "Greenland halibut - 4RST",
    "Sebastes fasciatus - Unit1",
    "Sebastes mentella - Unit1",
    "Witch flounder - 4RST",
    "Atlantic cod - 3Pn4RS",
    "American plaice - 4T",
    "Atlantic cod - 4TVn",
    "White hake - 4T",
    "Winter flounder - 4T",
    "Yellowtail flounder - 4T",
    "Atlantic mackerel",
    "Atlantic herring - 4RSw Fall",
    "Atlantic herring - 4RSw Spring",
    "Atlantic herring - 4TVn Fall",
    "Atlantic herring - 4TVn Spring"
  )

  # 3. Filtering & Forced Sorting
  if (!is.null(year_range)) {
    df <- df |> dplyr::filter(yr >= year_range[1], yr <= year_range[2])
  }

  df <- df |>
    dplyr::filter(stk %in% target_order) |>
    dplyr::mutate(stk = factor(stk, levels = rev(target_order)))

  df <- df |>
    dplyr::group_by(yr, stk) |>
    dplyr::summarize(val = mean(val, na.rm = TRUE), .groups = "drop")

  # 4. Translation Logic with Unicode Escapes (Safe for Accents)
  stock_dict <- c(
    "Northern shrimp-Anticosti"    = "Crevette nordique - Anticosti",
    "Northern shrimp-Esquiman"     = "Crevette nordique - Esquiman",
    "Northern shrimp-Estuary"      = "Crevette nordique - Estuaire",
    "Northern shrimp-Sept-Iles"    = "Crevette nordique - Sept-Îles",
    "Snow crab - sGSL"             = "Crabe des neiges - sGSL",
    "Atlantic halibut - 4RST"      = "Flétan de l'Atlantique - 4RST",
    "Greenland halibut - 4RST"     = "Flétan du Groenland - 4RST",
    "Sebastes fasciatus - Unit1"   = "Sebastes fasciatus - Unité 1",
    "Sebastes mentella - Unit1"    = "Sebastes mentella - Unité 1",
    "Witch flounder - 4RST"        = "Plie grise - 4RST",
    "Atlantic cod - 3Pn4RS"        = "Morue de l'Atlantique - 3Pn4RS",
    "American plaice - 4T"         = "Plie canadienne - 4T",
    "Atlantic cod - 4TVn"          = "Morue de l'Atlantique - 4TVn",
    "White hake - 4T"              = "Merluche blanche - 4T",
    "Winter flounder - 4T"         = "Plie rouge - 4T",
    "Yellowtail flounder - 4T"     = "Limande à queue jaune - 4T",
    "Atlantic mackerel"            = "Maquereau bleu",
    "Atlantic herring - 4RSw Fall" = "Hareng de l'Atlantique - 4RSw automne",
    "Atlantic herring - 4RSw Spring" = "Hareng de l'Atlantique - 4RSw printemps",
    "Atlantic herring - 4TVn Fall" = "Hareng de l'Atlantique - 4TVn automne",
    "Atlantic herring - 4TVn Spring" = "Hareng de l'Atlantique - 4TVn printemps"
  )

  terms <- list(
    en = c(title = "Stock Status Relative to Limit Reference Point (LRP)",
           xlab = "Year", ylab = "Stock", leg = "Status/LRP"),
    fr = c(title = "État des stocks par rapport au point de référence limite (PRL)",
           xlab = "Année", ylab = "Stock", leg = "État/PRL")
  )

  if (lang == "fr") {
    current_levels <- levels(df$stk)
    new_levels <- dplyr::recode(current_levels, !!!stock_dict)
    levels(df$stk) <- new_levels
  }
  # 5. Build Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = yr, y = stk, fill = val)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_distiller(
      palette = "RdYlGn", direction = 1, limits = c(0, 2), na.value = "grey80",
      name = terms[[lang]][["leg"]], oob = scales::squish,
      breaks = c(0, 0.5, 1, 1.5, 2),
      labels = if(lang == "fr") c("0", "0.5", "1.0 (PRL)", "1.5", "2.0+") else
        c("0", "0.5", "1.0 (LRP)", "1.5", "2.0+")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1955, 2035, by = 5), expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = terms[[lang]][["xlab"]], y = terms[[lang]][["ylab"]]) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "right",
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
      axis.ticks = ggplot2::element_line(color = "black")
    )

  if (show_title) {
    p <- p + ggplot2::labs(title = terms[[lang]][["title"]]) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
  }

  return(p)
}


#' Plot Stacked Landings by Group
#'
#' @description
#' Visualizes fisheries landings over time using a stacked area plot.
#' Supports bilingual labeling (via an internal dictionary and \code{rosettafish}),
#' custom color palettes, and adjustable text scaling.
#' By default, groups are stacked as: Crustaceans (Top), Groundfish (Middle), Pelagics (Bottom).
#'
#' @param data A data frame containing landings.
#' @param year_col Unquoted name of the year column. Defaults to \code{year}.
#' @param group_col Unquoted name of the grouping column (e.g., species group). Defaults to \code{grp}.
#' @param value_col Unquoted name of the landings value column. Defaults to \code{landings}.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Numeric vector \code{c(start, end)} to filter the timeline.
#' @param group_order Character vector defining the visual order from \strong{top to bottom}.
#'   Defaults to \code{c("pelagics", "crustaceans", "groundfish")}.
#' @param show_title Logical. Defaults to \code{FALSE}.
#' @param col_palette Optional named character vector of colors.
#' @param base_size Numeric. Base font size for the plot. Defaults to 14.
#' @param xlab,ylab Optional strings to override default axis labels.
#' @param y_breaks Optional numeric vector for y-axis tick marks.
#'        Defaults to \code{ggplot2::waiver()} (automatic).
#' @param custom_theme A ggplot2 theme. Defaults to \code{theme_bw()}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Load your data
#' landings_data <- read.csv("landings_by_group.csv")
#'
#' # 1. Basic plot with default order (Top: Pelagics -> Middle: Crustaceans  -> Bottom: Groundfish)
#' plot_landings_stacked(landings_data)
#'
#' # 2. French version, custom year range, and larger text scaling
#' plot_landings_stacked(landings_data,
#'                       lang = "fr",
#'                       year_range = c(1990, 2023),
#'                       base_size = 16)
#'
#' # 3. Custom stack order: Pelagics on top, Crustaceans on bottom
#' plot_landings_stacked(landings_data,
#'                       group_order = c("pelagics", "groundfish", "crustaceans"))
#'
#' # 4. Custom colour palette with rosettafish fallback
#' my_pal <- c("Crustaceans" = "#8b0000",
#'             "Groundfish"  = "#4682b4",
#'             "Pelagics"    = "#2e8b57")
#'
#' plot_landings_stacked(landings_data, col_palette = my_pal)
#' }
#' @export
plot_landings_stacked <- function(data,
                                  year_col = year,
                                  group_col = grp,
                                  value_col = landings,
                                  lang = "en",
                                  year_range = NULL,
                                  group_order = c( "pelagics", "crustaceans", "groundfish"),
                                  show_title = FALSE,
                                  col_palette = NULL,
                                  base_size = 14,
                                  xlab = NULL,
                                  ylab = NULL,
                                  y_breaks = ggplot2::waiver(),
                                  custom_theme = ggplot2::theme_bw()) {

  # 1. Setup
  yr_enquo  <- rlang::enquo(year_col)
  grp_enquo <- rlang::enquo(group_col)
  val_enquo <- rlang::enquo(value_col)

  # 2. Local Dictionary
  terms <- list(
    en = c(title = "Fisheries Landings by Group", xlab = "Year", ylab = "Landings (t)", leg = "Group",
           crustaceans = "Crustaceans", groundfish = "Groundfish", pelagics = "Pelagics"),
    fr = c(title = "Débarquements de pêche par groupe", xlab = "Année", ylab = "Débarquements (t)", leg = "Groupe",
           crustaceans = "Crustacés", groundfish = "Poissons de fond", pelagics = "Pélagiques")
  )

  get_term <- function(x, dictionary, language) {
    if (x %in% names(dictionary)) return(dictionary[[x]])
    if (language == "fr" && requireNamespace("rosettafish", quietly = TRUE)) {
      return(rosettafish::en2fr(x))
    }
    return(x)
  }

  # 3. Data Prep
  df <- data |>
    dplyr::rename(yr = !!yr_enquo, grp = !!grp_enquo, val = !!val_enquo) |>
    dplyr::filter(!is.na(val))

  if (!is.null(year_range)) {
    df <- df |> dplyr::filter(yr >= year_range[1], yr <= year_range[2])
  }

  # 4. Critical Factor Ordering
  # Level 1 will be the first item in group_order (default: crustaceans)
  df$grp <- factor(as.character(df$grp), levels = group_order)

  # Translate levels (preserving the integer order)
  current_levs <- levels(df$grp)
  translated_levs <- sapply(current_levs, get_term, dictionary = terms[[lang]], language = lang)
  levels(df$grp) <- unname(translated_levs)

  # 5. Colors
  if (is.null(col_palette)) {
    base_pal <- c(
      "Crustaceans" = "#D55E00", "Crustacés" = "#D55E00",
      "Groundfish"  = "#0072B2", "Poissons de fond" = "#0072B2",
      "Pelagics"    = "#009E73", "Pélagiques" = "#009E73"
    )
    if (all(levels(df$grp) %in% names(base_pal))) {
      col_palette <- base_pal
    } else {
      col_palette <- scales::hue_pal()(length(levels(df$grp)))
      names(col_palette) <- levels(df$grp)
    }
  }

  # 6. Build Plot
  final_xlab <- if(!is.null(xlab)) xlab else get_term("xlab", terms[[lang]], lang)
  final_ylab <- if(!is.null(ylab)) ylab else get_term("ylab", terms[[lang]], lang)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = yr, y = val, fill = grp)) +
    # USE REVERSE STACK:
    # This puts Level 1 (Crustaceans) at the TOP of the plot stack.
    ggplot2::geom_area(alpha = 0.8, color = "white", linewidth = 0.3,
                       position = ggplot2::position_stack(reverse = FALSE)) +
    ggplot2::scale_fill_manual(values = col_palette) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.15)),breaks = y_breaks) +
    # LEGEND: Default (no reverse)
    # This puts Level 1 (Crustaceans) at the TOP of the legend list.
    ggplot2::labs(x = final_xlab, y = final_ylab, fill = get_term("leg", terms[[lang]], lang)) +
    custom_theme +
    ggplot2::theme(text = ggplot2::element_text(size = base_size),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.position = "right")

  if (show_title) {
    p <- p + ggplot2::labs(title = get_term("title", terms[[lang]], lang)) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
  }

  return(p)
}


#' Plot Fish Abundance Trends
#'
#' @description
#' Visualizes trends in fish abundance (e.g., biomass or density) over time.
#' Designed to handle long-form data (gslea) by allowing users to pick out
#' a specific variable (e.g., a species name) from the data. The Y-axis label
#' automatically updates to include the variable name.
#'
#' @param data A data frame containing abundance data (e.g., gslea long-form).
#' @param var Character. The specific variable or species to pick out
#'   from the \code{group_col} (e.g., "abft.n"). If \code{NULL},
#'   all variables in the data are plotted.
#' @param year_range Numeric vector \code{c(start, end)} to filter the timeline.
#' @param ylim Numeric vector \code{c(min, max)} for the Y-axis. Defaults to \code{c(0, NA)}.
#' @param year_col Unquoted name of the year column. Defaults to \code{year}.
#' @param value_col Unquoted name of the value column. Defaults to \code{value}.
#' @param group_col Unquoted name of the grouping column. Defaults to \code{variable}.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param show_legend Logical. Whether to display the legend. Defaults to \code{TRUE}.
#' @param col_palette Optional character vector of colors.
#' @param base_size Numeric. Base font size for the plot. Defaults to 14.
#' @param line_width Numeric. Thickness of the trend lines. Defaults to 1.2.
#' @param custom_theme A ggplot2 theme. Defaults to \code{theme_bw()}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # 1. Plot specific species from gslea data (Dynamic Y-axis label)
#' plot_abundance_trends(gslea_data, var = "abft.n")
#'
#' # 2. French version, specific for Atlantic Bluefin Tuna
#' plot_abundance_trends(gslea_data, lang = "fr", show_legend = FALSE)
#'
#' }
#' @export
plot_abundance_trends <- function(data,
                                  var = NULL,
                                  year_range = NULL,
                                  ylim = c(0, NA),
                                  year_col = year,
                                  value_col = value,
                                  group_col = variable,
                                  lang = "en",
                                  show_legend = TRUE,
                                  col_palette = NULL,
                                  base_size = 14,
                                  line_width = 1.2,
                                  custom_theme = ggplot2::theme_bw()) {

  # 1. Setup & Mapping
  yr_enquo  <- rlang::enquo(year_col)
  val_enquo <- rlang::enquo(value_col)
  grp_enquo <- rlang::enquo(group_col)

  # 2. Internal Dictionary
  terms <- list(
    en = c(xlab = "Year", ylab_suffix = "Abundance Index", leg = "Variable",
           "abft.n" = "Atlantic Bluefin Tuna"),
    fr = c(xlab = "Année", ylab_suffix = "Indice d'abondance", leg = "Variable",
           "abft.n" = "Thon rouge de l'Atlantique")
  )

  get_term <- function(x, dictionary, language) {
    clean_x <- tolower(trimws(as.character(x)))
    if (clean_x %in% names(dictionary)) return(dictionary[[clean_x]])

    if (language == "fr" && requireNamespace("rosettafish", quietly = TRUE)) {
      return(rosettafish::en2fr(x))
    }
    return(x)
  }

  # 3. Data Processing & Filtering
  df <- data |>
    dplyr::rename(yr = !!yr_enquo, val = !!val_enquo, grp = !!grp_enquo)

  # Filter for the var if provided
  if (!is.null(var)) {
    df <- df |> dplyr::filter(tolower(grp) == tolower(var))

    if (nrow(df) == 0) {
      col_nm <- rlang::as_label(grp_enquo)
      stop(paste0("Variable '", var, "' not found in column '", col_nm, "'. ",
                  "Available options: ", paste(unique(data[[col_nm]]), collapse = ", ")))
    }
  }

  # Filter for Year Range
  if (!is.null(year_range)) {
    df <- df |> dplyr::filter(yr >= year_range[1], yr <= year_range[2])
  }

  df <- df |> dplyr::filter(!is.na(val))

  # 4. Prepare Labels
  unique_groups <- unique(df$grp)

  if (length(unique_groups) == 1) {
    var_label <- get_term(unique_groups[1], terms[[lang]], lang)
    dynamic_ylab <- paste(var_label, terms[[lang]][["ylab_suffix"]], sep = " ")
  } else {
    dynamic_ylab <- terms[[lang]][["ylab_suffix"]]
  }

  # 5. Build Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = yr, y = val, color = grp, group = grp)) +
    ggplot2::geom_line(linewidth = line_width) +
    # High-visibility points
    ggplot2::geom_point(size = line_width * 2.5) +
    ggplot2::scale_x_continuous(expand = c(0.02, 0.02)) +
    # Force Y to start at 0 by default, allow override
    ggplot2::scale_y_continuous(labels = scales::comma,
                                limits = ylim,
                                expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(
      x = get_term("xlab", terms[[lang]], lang),
      y = dynamic_ylab,
      color = get_term("leg", terms[[lang]], lang)
    ) +
    custom_theme +
    ggplot2::theme(
      text = ggplot2::element_text(size = base_size),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.minor = ggplot2::element_blank()
    )

  # 6. Colors & Legend Toggle
  if (!is.null(col_palette)) {
    p <- p + ggplot2::scale_color_manual(values = col_palette)
  } else {
    p <- p + ggplot2::scale_color_viridis_d(option = "mako", end = 0.8)
  }

  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}
