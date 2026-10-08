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
