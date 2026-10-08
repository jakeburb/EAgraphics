#' Plot Generic Anomaly with Standardized Scorecard With Composite Computed Internally
#'
#' @description
#' Creates a publication-quality anomaly plot with stacked bars and a scorecard.
#' The function automatically calculates a composite annual sum and a
#' standardized scorecard value. Standardized scores (Z-scores) can be calculated
#' relative to a specific baseline year range (e.g., a climate normal) or the
#' entire time series.
#'
#' @param data Data frame containing \code{year} and the anomaly values.
#' @param value_col Unquoted name of the anomaly column for the bars.
#' @param var_col Optional unquoted name of the grouping column for stacked bars.
#' @param unit Optional string for units (e.g., "m"). Appends to legend items only.
#' @param show_composite Logical. Should the composite line be drawn?
#' @param show_scorecard Logical. Should the heatmap scorecard be drawn?
#' @param year_range Numeric vector \code{c(start, end)} for plot display limits.
#' @param baseline_range Numeric vector \code{c(start, end)} for Z-score baseline.
#'        If \code{NULL}, defaults to the entire time series in the data.
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
#' # Baseline defaults to the entire range of 'do_data'
#' plot_anomaly_comp(data = do_data,
#'                   value_col = anomaly,
#'                   var_col = depth,
#'                   unit = "m",
#'                   y_label = "Dissolved Oxygen Anomaly (ml/L)")
#'
#' # Example 2: SST anomaly with a specific 1991-2020 baseline for the scorecard
#' plot_anomaly_comp(data = sst_data,
#'                   value_col = sst_anom,
#'                   baseline_range = c(1991, 2020),
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
                              baseline_range = NULL,
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

  # Internal Calculation: Composite Sum
  composite_data <- data |>
    dplyr::group_by(year) |>
    dplyr::summarise(annual_sum = sum(!!val_enquo, na.rm = TRUE), .groups = "drop")

  # Calculate Baseline Statistics for Standardized Score
  if (!is.null(baseline_range)) {
    baseline_df <- composite_data |>
      dplyr::filter(year >= baseline_range[1], year <= baseline_range[2])

    b_mean <- mean(baseline_df$annual_sum, na.rm = TRUE)
    b_sd   <- stats::sd(baseline_df$annual_sum, na.rm = TRUE)
  } else {
    b_mean <- mean(composite_data$annual_sum, na.rm = TRUE)
    b_sd   <- stats::sd(composite_data$annual_sum, na.rm = TRUE)
  }

  # Apply Standardization (Z-score)
  composite_data <- composite_data |>
    dplyr::mutate(std_score = (annual_sum - b_mean) / b_sd)

  # Filter Display Range
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
    col_low  <- "#0000FF"
    col_mid  <- "white"
    col_high <- "#FF0000"

    # Define the 16 bins based on your Breaks: seq(-5, 3, by = 0.5)
    # Bins 1-9:   Negative ramp (-5.0 to -0.5)
    # Bins 10-11: Neutral zone (-0.5 to 0.5) -> MUST BE WHITE
    # Bins 12-16: Positive ramp (0.5 to 3.0)

    score_palette <- c(
      # 9 shades from Blue to "almost" White
      grDevices::colorRampPalette(c(col_low, col_mid))(10)[1:9],
      # 2 bins of pure White for the -0.5 to 0.5 range
      rep(col_mid, 2),
      # 5 shades from "almost" White to Red
      grDevices::colorRampPalette(c(col_mid, col_high))(6)[2:6]
    )

    #Application
    Breaks <- seq(-5, 3, by = 0.5)
    Labels <- as.character(Breaks[-length(Breaks)])

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
