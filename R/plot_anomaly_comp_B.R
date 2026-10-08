#' Plot Generic Anomaly with Standardized Scorecard With Composite Computed Internally-Blank Scorecard With Colour Legend for Ecoystem Summary
#'
#' @description
#' Creates a publication-quality anomaly plot with stacked bars and a scorecard.
#' The function automatically calculates a composite annual sum and a
#' standardized scorecard value. Standardized scores (Z-scores) can be calculated
#' relative to a specific baseline year range (e.g., a climate normal) or the
#' entire time series. The scorecard is left blank and a scale bar for the anomaly
#' is added to match the aesthetic of the RV anomaly plots.
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
#' @param x_breaks_interval Numeric. Interval for year labels. Defaults to 3.
#' @param colors Optional character vector of colors. Defaults to "YlGnBu".
#' @param lang Language: \code{"en"} (default) or \code{"fr"}.
#' @param y_label Optional string for the y-axis title. Defaults to "Anomalies".
#' @param y_breaks Optional numeric vector for y-axis tick marks.
#' @param base_size Numeric. Base font size. Defaults to 14.
#'
#' @return A combined ggplot/cowplot object.
#'
#' @export
plot_anomaly_comp_B <- function(data,
                                value_col,
                                var_col = NULL,
                                unit = NULL,
                                show_composite = TRUE,
                                show_scorecard = TRUE,
                                year_range = NULL,
                                baseline_range = NULL,
                                x_breaks_interval = 3,
                                colors = NULL,
                                lang = "en",
                                y_label = NULL,
                                y_breaks = ggplot2::waiver(),
                                base_size = 14) {

  # 1. Setup & Aggregation
  val_enquo <- rlang::enquo(value_col)
  var_enquo <- rlang::enquo(var_col)

  terms <- list(
    en = c(anom = "Anomaly", yr = "Year", y_title = "Anomalies"),
    fr = c(anom = "Anomalie", yr = "Année", y_title = "Anomalies")
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
  final_y_label <- if(!is.null(y_label)) y_label else terms[[lang]][["y_title"]]

  if (!rlang::quo_is_null(var_enquo)) {
    var_nm <- rlang::as_label(var_enquo)

    # Define translation dictionary
    region_dict <- c(
      "Magdalen Shallows"   = "Plateau madelinot",
      "Estuary & NW Gulf"   = "Estuaire et N.-O. du golfe",
      "NE Gulf"             = "N.-E. du golfe",
      "Center Gulf & Cabot" = "Centre du golfe et Cabot"
    )

    # 1. Capture the exact baseline order (from factor levels or original appearance)
    if (is.factor(data[[var_nm]])) {
      orig_levels <- levels(data[[var_nm]])
    } else {
      # If numeric depths exist, sort numerically; otherwise preserve original unique order
      raw_unique <- unique(data[[var_nm]])
      suppressWarnings({
        has_nums <- any(!is.na(as.numeric(gsub("[^0-9.]", "", raw_unique))))
      })

      if (has_nums) {
        orig_levels <- raw_unique[order(as.numeric(gsub("[^0-9.]", "", raw_unique)))]
      } else {
        orig_levels <- raw_unique
      }
    }

    #  Append units if supplied
    data[[var_nm]] <- as.character(data[[var_nm]])
    if (!is.null(unit)) {
      data[[var_nm]] <- ifelse(
        grepl(unit, data[[var_nm]], fixed = TRUE),
        data[[var_nm]],
        paste(data[[var_nm]], unit)
      )
      orig_levels <- paste(orig_levels, unit)
    }

    # Translate data AND preserve factor level ordering if French
    if (lang == "fr") {
      # Translate actual row values
      data[[var_nm]] <- ifelse(data[[var_nm]] %in% names(region_dict),
                               region_dict[data[[var_nm]]],
                               data[[var_nm]])

      # Translate the level sequence to match
      final_levels <- ifelse(orig_levels %in% names(region_dict),
                             region_dict[orig_levels],
                             orig_levels)
    } else {
      final_levels <- orig_levels
    }

    # Lock in factor levels in the identical sequence
    data[[var_nm]] <- factor(data[[var_nm]], levels = final_levels)
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

    # Render scorecard using the standardized scientific color bar
    p_bot <- ggplot2::ggplot(composite_data, ggplot2::aes(x = year, y = 1, fill = std_score)) +
      ggplot2::geom_tile(color = "black", linewidth = 0.25) +

      # Added from user snippet: strictly bounded scale
      ggplot2::scale_fill_steps2(
        low = "#0000FF",
        mid = "white",
        high = "#FF0000",
        midpoint = 0,
        breaks = c(-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5),
        labels = c("<-3.5", "-2.5", "-1.5", "-0.5", "0.5", "1.5", "2.5", ">3.5"),
        limits = c(-3.5, 3.5),
        oob = scales::squish,
        na.value = "transparent",
        guide = ggplot2::guide_colorsteps(
          barwidth = 20,
          barheight = 1,
          show.limits = FALSE,
          title.position = "top",
          title.hjust = 0.5,
          # Outline and ticks to prevent 'white-out' in the center of the bar
          frame.colour = "black",
          frame.linewidth = 0.5,
          ticks.colour = "black",
          ticks.linewidth = 0.5
        )
      ) +
      ggplot2::coord_cartesian(xlim = c(x_lims[1]-0.6, x_lims[2]+0.6), clip = "off") +
      ggplot2::scale_x_continuous(expand = c(0,0), breaks = label_breaks) +
      # Assign the translated "Anomaly" term to the fill legend
      ggplot2::labs(x = terms[[lang]][["yr"]], fill = terms[[lang]][["anom"]]) +
      ggplot2::theme_void(base_size = base_size) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = base_size * 0.8),
        axis.title.x = ggplot2::element_text(size = base_size, margin = ggplot2::margin(t = 10)),
        legend.position = "bottom", # Moved legend to bottom
        legend.title = ggplot2::element_text(size = base_size),
        legend.text = ggplot2::element_text(size = base_size * 0.8),
        plot.margin = ggplot2::margin(t = -1, r = 10, b = 5, l = 5)
      )

    # Increased the bottom panel rel_height from 2.2 to 3.5 to accommodate the new legend
    final_plot <- cowplot::plot_grid(p_top, p_bot, ncol = 1, rel_heights = c(10, 3.5), align = "v", axis = "lr")

  } else {
    final_plot <- p_top + ggplot2::theme(
      axis.title.x = ggplot2::element_text(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      axis.ticks.x = ggplot2::element_line()
    ) + ggplot2::labs(x = terms[[lang]][["yr"]])
  }

  return(final_plot)
}
