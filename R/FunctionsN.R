# Functions to make graphs and tables for the Gulf of St. Lawrence physical
# and biological environment. Using data from gslea which is a dependency
# of this package.

#' Plot Temperature data from gslea by EAR
#'
#' @description
#' Visualizes temperature data from the gslea package using long-format data.
#' Restored with full bilingual dictionary and native R pipes.
#'
#' @param data Long-format data frame containing \code{year}, \code{EAR}, \code{variable}, and \code{value}.
#' @param var Temperature variable to plot (unquoted, e.g., \code{sst.month10}).
#' @param EARs Vector of EAR identifiers. Defaults to \code{0}.
#' @param groups Optional named list to aggregate EARs.
#' @param year_range Numeric vector \code{c(start, end)}. Defaults to \code{c(1990, 2023)}.
#' @param lang Language for labels: \code{"en"} or \code{"fr"}.
#' @param fit_smooth Logical. If \code{TRUE}, fits a GAM smoother.
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
                                   lang = "en", fit_smooth = TRUE, method = "gam",
                                   formula = y ~ s(x, bs = "cs", k = 15),
                                   col_palette = NULL, ear_names = NULL,
                                   xlab = NULL, ylab = NULL, base_size = 14,
                                   facet_scales = "free_y",
                                   custom_theme = ggplot2::theme_bw()) {

  target_var <- rlang::enquo(var)
  var_name_raw <- rlang::as_label(target_var)

  # Full Restored Dictionary
  lookup <- list(
    en = c("sst.month5"="May Sea Surface Temperature", "sst.month6"="June Sea Surface Temperature",
           "sst.month7"="July Sea Surface Temperature", "sst.month8"="August Sea Surface Temperature",
           "sst.month9"="September Sea Surface Temperature", "sst.month10"="October Sea Surface Temperature",
           "sst.month11"="November Sea Surface Temperature", "t.deep"="Bottom Temperature",
           "t.shallow"="Bottom Temperature", "t.150"="Temperature at 150m", "t.200"="Temperature at 200m",
           "t.250"="Temperature at 250m", "t.300"="Temperature at 300m",
           "tmax200.400"="Maximum Temperature (200-400m)",
           "Northern"="Northern GSL", "Southern"="Southern GSL"),
    fr = c("sst.month5"="Température de surface de la mer en mai", "sst.month6"="Température de surface de la mer en juin",
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

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = value, color = ear_label)) +
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
#'
#' @param data Long-format data frame containing \code{year}, \code{EAR}, \code{variable}, and \code{value}.
#' @param var Ice/seasonal variable to plot (unquoted, e.g., \code{ice.duration}).
#' @param EARs Vector of EAR identifiers. Defaults to \code{0}.
#' @param groups Optional named list to aggregate EARs.
#' @param year_range Numeric vector \code{c(start, end)}. Defaults to \code{c(1990, 2023)}.
#' @param lang Language for labels: \code{"en"} or \code{"fr"}.
#' @param fit_smooth Logical. If \code{TRUE}, adds a GAM smoother.
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
#' # Compare ice duration between Northern and Southern groups
#' plot_gslea_ice(EA.data, ice.duration,
#'                year_range = c(2000, 2020),
#'                groups = list("Northern" = 1:4, "Southern" = 5:6),
#'                lang = "fr",
#'                facet_scales = "fixed",
#'                col_palette = c("black", "blue"))
#' }
#' @export
plot_gslea_ice <- function(data, var, EARs = 0, groups = NULL, year_range = c(1990, 2023),
                           lang = "en", fit_smooth = TRUE, method = "gam",
                           formula = y ~ s(x, bs = "cs", k = 15),
                           col_palette = NULL, ear_names = NULL,
                           xlab = NULL, ylab = NULL, base_size = 14,
                           facet_scales = "free_y",
                           custom_theme = ggplot2::theme_bw()) {

  target_var <- rlang::enquo(var)
  var_name_raw <- rlang::as_label(target_var)

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

  # Bilingual Unit Logic
  unit <- if (lang == "fr") {
    if(grepl("start|decrease", var_name_raw)) " (Semaine de l'année)" else
      if(grepl("duration", var_name_raw)) " (Jours)" else
        if(grepl("first|last", var_name_raw)) " (Jour de l'année)" else ""
  } else {
    if(grepl("start|decrease", var_name_raw)) " (Week of Year)" else
      if(grepl("duration", var_name_raw)) " (Days)" else
        if(grepl("first|last", var_name_raw)) " (Day of Year)" else ""
  }

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
  final_ylab <- if(is.null(ylab)) paste0(base_label, unit) else ylab

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = value, color = ear_label)) +
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
    p <- p + ggplot2::geom_smooth(method = method, formula = formula, color = "black", se = TRUE, fill = "grey80", alpha = 0.4)
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
#' and provides bilingual labels for species and seasonal subsets.
#'
#' @param data Long-format data frame containing \code{year}, \code{EAR}, \code{variable}, and \code{value}.
#' @param var Planktonic variable to plot (unquoted, e.g., \code{calanus.finmarchicus.annual}).
#' @param EARs Vector of EAR identifiers to include. Defaults to \code{0} (Entire Gulf).
#' @param groups Optional named list to aggregate EARs (e.g., \code{list("Northern" = 1:4)}).
#' @param year_range Numeric vector \code{c(start, end)}. Defaults to \code{c(1990, 2023)}.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param fit_smooth Logical. If \code{TRUE}, adds a GAM smoother to the plot.
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
                               lang = "en", fit_smooth = TRUE, method = "gam",
                               formula = y ~ s(x, bs = "cs", k = 15),
                               col_palette = NULL, ear_names = NULL,
                               xlab = NULL, ylab = NULL, base_size = 14,
                               facet_scales = "free_y",
                               custom_theme = ggplot2::theme_bw()) {

  target_var <- rlang::enquo(var)
  var_name_raw <- rlang::as_label(target_var)

  # Comprehensive Dictionary from gslea variables
  lookup <- list(
    en = c("calanus.finmarchicus.annual"="Abundance of Calanus finmarchicus (Annual)",
           "calanus.finmarchicus.early_summer"="Abundance of C. finmarchicus (Early Summer)",
           "calanus.finmarchicus.fall"="Abundance of C. finmarchicus (Fall)",
           "calanus.hyperboreus.annual"="Abundance of Calanus hyperboreus (Annual)",
           "calanus.hyperboreus.early_summer"="Abundance of C. hyperboreus (Early Summer)",
           "calanus.hyperboreus.fall"="Abundance of C. hyperboreus (Fall)",
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
           "calanus.finmarchicus.early_summer"="Abondance de C. finmarchicus (Début d'été)",
           "calanus.finmarchicus.fall"="Abondance de C. finmarchicus (Automne)",
           "calanus.hyperboreus.annual"="Abondance de Calanus hyperboreus (Annuel)",
           "calanus.hyperboreus.early_summer"="Abondance de C. hyperboreus (Début d'été)",
           "calanus.hyperboreus.fall"="Abondance de C. hyperboreus (Automne)",
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

  # Refined Unit Logic based on Table
  unit <- if(grepl("dw2_t", var_name_raw)) " (g m⁻²)" else
    if(grepl("chl0_100|magnitude", var_name_raw)) " (mg chla m⁻²)" else
      if(var_name_raw == "start") { if(lang == "fr") " (Jour de l'année)" else " (Day of Year)" } else
        if(grepl("^ci\\.|^civ\\.", var_name_raw)) "" else " (10³ ind m⁻²)"

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
  final_ylab <- if(is.null(ylab)) paste0(base_label, unit) else ylab

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = value, color = ear_label)) +
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
    p <- p + ggplot2::geom_smooth(method = method, formula = formula, color = "black", se = TRUE, fill = "grey80", alpha = 0.4)
  }

  if (length(unique(plot_df$ear_label)) > 1) {
    p <- p + ggplot2::facet_wrap(~ear_label, scales = facet_scales) +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}
