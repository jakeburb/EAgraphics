#' Plot Hypoxic Area Time Series
#'
#' @description
#' Visualizes the percentage of bottom area > 100m depth with oxygen levels below
#' numeric thresholds (defaults to 20 and 30) for the Estuary and Gulf regions.
#'
#' @param data A data frame containing 'year', 'percentage', 'region', and 'threshold'.
#' @param thresholds Numeric vector of thresholds to plot. Defaults to \code{c(20, 30)}.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Optional numeric vector \code{c(start, end)} for the x-axis.
#' @param x_breaks Optional numeric vector for x-axis tick marks.
#' @param base_size Numeric. Base font size. Defaults to 14.
#'
#'1. Standard plot with default 20% and 30% thresholds
#' plot_hypoxic_area_timeseries(hypoxia_data)
#'
#' # 2. French version with custom thresholds and specific year range
#' plot_hypoxic_area_timeseries(hypoxia_data,
#'                              thresholds = c(15, 25, 35),
#'                              lang = "fr",
#'                              year_range = c(2000, 2025),
#'                              x_breaks = seq(2000, 2025, 5))
#' @export
plot_hypoxic_area_timeseries <- function(data,
                                         thresholds = c(20, 30),
                                         lang = "en",
                                         year_range = NULL,
                                         x_breaks = NULL,
                                         base_size = 14) {

  # 1. Translation Dictionary
  terms <- list(
    en = c(xlab = "Year",
           ylab = "Percentage of bottom area > 100 m",
           estuary = "Estuary",
           gulf = "Gulf"),
    fr = c(xlab = "Année",
           ylab = "Pourcentage de la zone de fond > 100 m",
           estuary = "Estuaire",
           gulf = "Golfe")
  )

  # 2. Data Preparation & Filtering
  # Convert numeric thresholds to factors with '%' labels for the legend
  df <- data |>
    dplyr::filter(threshold %in% thresholds) |>
    dplyr::mutate(
      threshold_f = factor(threshold,
                           levels = thresholds,
                           labels = paste0(thresholds, "%"))
    )

  global_min <- if(!is.null(year_range)) year_range[1] else min(df$year, na.rm = TRUE)
  global_max <- if(!is.null(year_range)) year_range[2] else max(df$year, na.rm = TRUE)

  # 3. Dynamic Styling Logic (Mapped to the factor labels)
  # Match colors/shapes to reference: 20% = Black/Circle, 30% = Blue/Triangle
  custom_cols   <- c("20%" = "black", "30%" = "#0072B2")
  custom_shapes <- c("20%" = 16, "30%" = 17)

  # 4. Panel Builder Logic
  make_panel <- function(region_name, title_key, show_y_title = TRUE) {
    sub_data <- df |> dplyr::filter(region == region_name)

    ggplot2::ggplot(sub_data, ggplot2::aes(x = year, y = percentage,
                                           group = threshold_f,
                                           shape = threshold_f,
                                           color = threshold_f)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 3) +

      # Manual scales specifically for 20% and 30%
      ggplot2::scale_color_manual(values = custom_cols) +
      ggplot2::scale_shape_manual(values = custom_shapes) +

      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      ggplot2::scale_x_continuous(
        limits = c(global_min, global_max),
        breaks = x_breaks %||% seq(global_min, global_max, 4)
      ) +
      ggplot2::labs(title = terms[[lang]][[title_key]],
                    x = terms[[lang]][["xlab"]],
                    y = if(show_y_title) terms[[lang]][["ylab"]] else NULL,
                    shape = NULL, color = NULL) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = c(0.12, 0.88),
        legend.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = base_size * 0.9)
      )
  }

  # 5. Generate and Assemble
  p_estuary <- make_panel("Estuary", "estuary", show_y_title = TRUE)
  p_gulf    <- make_panel("Gulf", "gulf", show_y_title = TRUE) # Matches reference style

  # Place side-by-side using patchwork
  p_estuary + p_gulf
}
