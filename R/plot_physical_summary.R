#' Plot Physical Summary with 1991-2020 Climatology
#'
#' @description
#' Creates a two-panel dashboard where anomalies and summary statistics are
#' calculated based on the 1991-2020 climatological mean. Includes dual-axis
#' time series and an anomaly scorecard.
#'
#' @param data A long-format data frame with columns: 'year', 'variable', 'value'.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Numeric vector for plot x-axis. Defaults to \code{c(1969, 2025)}.
#' @param clim_range Numeric vector for baseline mean. Defaults to \code{c(1991, 2020)}.
#' @param base_size Numeric. Base font size. Defaults to \code{11}.
#'
#' @export
plot_physical_summary <- function(data,
                                  lang = "en",
                                  year_range = c(1969, 2025),
                                  clim_range = c(1991, 2020),
                                  base_size = 11) {

  # 1. Setup Variable Mapping
  vars_in_order <- c("SST_combined", "T300m", "IceSeasonalMaxVolume", "CILTmin")
  ice_var <- "IceSeasonalMaxVolume"
  ice_scaling <- 11

  terms <- list(
    en = list(y_temp = "Temperature (°C)", y_ice = "Ice (km³)", year = "Year",
              labs = c("SST_combined" = "SST", "T300m" = "300 m",
                       "IceSeasonalMaxVolume" = "Ice", "CILTmin" = "CIL")),
    fr = list(y_temp = "Température (°C)", y_ice = "Glace (km³)", year = "Année",
              labs = c("SST_combined" = "SST", "T300m" = "300 m",
                       "IceSeasonalMaxVolume" = "Glace", "CILTmin" = "CLF"))
  )

  # 2. Pre-process and Calculate Climatology (1991-2020)
  # Create the display_var grouping first so we can calculate means for the combined SST series
  df_base <- data |>
    dplyr::mutate(
      display_var = dplyr::case_when(
        variable %in% c("SST", "SSTproxy") ~ "SST_combined",
        TRUE ~ variable
      )
    )

  # Calculate Mean and SD based ONLY on the climatology range
  clim_stats <- df_base |>
    dplyr::filter(year >= clim_range[1], year <= clim_range[2]) |>
    dplyr::group_by(display_var) |>
    dplyr::summarize(
      clim_mean = mean(value, na.rm = TRUE),
      clim_sd = sd(value, na.rm = TRUE),
      .groups = "drop"
    )

  # 3. Prepare Plotting Data
  processed_data <- df_base |>
    dplyr::filter(year >= year_range[1], year <= year_range[2]) |>
    # Apply SSTproxy cutoff
    dplyr::filter(!(variable == "SSTproxy" & year > 1982)) |>
    dplyr::left_join(clim_stats, by = "display_var") |>
    dplyr::mutate(
      lt = ifelse(variable == "SSTproxy", "dashed", "solid"),
      anomaly = (value - clim_mean) / clim_sd,
      stat_label = paste0(round(clim_mean, 2), " ± ", round(clim_sd, 2)),
      display_var = factor(display_var, levels = rev(vars_in_order))
    )

  # 4. Time Series Plot (Top)
  p1 <- ggplot2::ggplot(processed_data,
                        ggplot2::aes(x = year,
                                     y = ifelse(display_var == ice_var, value / ice_scaling, value),
                                     color = display_var)) +
    # The actual data lines
    ggplot2::geom_line(ggplot2::aes(linetype = lt), linewidth = 0.8) +
    # Climatological Mean Lines (Horizontal)
    # Note: Ice mean must be scaled to match the primary Y-axis
    ggplot2::geom_hline(ggplot2::aes(yintercept = ifelse(display_var == ice_var, clim_mean / ice_scaling, clim_mean),
                                     color = display_var),
                        linetype = "dotted", alpha = 0.6) +
    ggplot2::scale_color_manual(
      values = c("SST_combined" = "#D55E00", "T300m" = "#009E73",
                 "IceSeasonalMaxVolume" = "#4B0082", "CILTmin" = "#0072B2"),
      labels = terms[[lang]]$labs
    ) +
    ggplot2::scale_linetype_identity() +
    ggplot2::scale_y_continuous(
      name = terms[[lang]]$y_temp,
      limits = c(-1.5, 12),
      sec.axis = ggplot2::sec_axis(~ . * ice_scaling, name = terms[[lang]]$y_ice)
    ) +
    ggplot2::scale_x_continuous(limits = year_range, expand = c(0,0)) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(b = 0, unit = "pt")
    )

  # 5. Scorecard Plot (Bottom)
  p2 <- ggplot2::ggplot(processed_data, ggplot2::aes(x = year, y = display_var, fill = anomaly)) +
    ggplot2::geom_tile(color = "black", linewidth = 0.1) +
    ggplot2::scale_fill_stepsn(
      colors = c("#0000FF", "#7069FF", "#FFFFFF", "#FF7B5C", "#FF0000"),
      breaks = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5),
      limits = c(-3, 3), oob = scales::squish
    ) +
    # Annotated with 1991-2020 Stats
    ggplot2::geom_text(data = dplyr::distinct(processed_data, display_var, stat_label),
                       ggplot2::aes(x = year_range[2] + 1, y = display_var, label = stat_label),
                       hjust = 0, size = base_size * 0.28, inherit.aes = FALSE) +
    ggplot2::scale_x_continuous(limits = c(year_range[1], year_range[2] + 10),
                                breaks = seq(year_range[1], year_range[2], 5),
                                expand = c(0,0)) +
    ggplot2::scale_y_discrete(labels = terms[[lang]]$labs) +
    ggplot2::labs(x = terms[[lang]]$year, y = NULL) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 0, r = 10, b = 5, l = 5, unit = "pt")
    )

  # 6. Assemble
  patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(3, 1))
}
