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

