#' Plot Hypercapnic Area Time Series
#'
#' @description
#' Visualizes the percentage of bottom area > 100m depth with hypercapnic conditions
#' for the Estuary and Gulf regions.
#'
#' @param data A data frame containing 'year', 'percentage', and 'region'.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Optional numeric vector \code{c(start, end)} for the x-axis.
#' @param x_breaks Optional numeric vector for x-axis tick marks.
#' @param base_size Numeric. Base font size. Defaults to 14.
#'
#' @return A \code{patchwork} object with two side-by-side panels.
#'
#' @examples
#' \dontrun{
#' # 1. Standard hypercapnia plot
#' plot_hypercapnic_area_timeseries(hypercapnia_data)
#'
#' # 2. French version with custom year range
#' plot_hypercapnic_area_timeseries(hypercapnia_data,
#'                                  lang = "fr",
#'                                  year_range = c(1990, 2025))
#' }
#' @export
plot_hypercapnic_area_timeseries <- function(data,
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

  global_min <- if(!is.null(year_range)) year_range[1] else min(data$year, na.rm = TRUE)
  global_max <- if(!is.null(year_range)) year_range[2] else max(data$year, na.rm = TRUE)

  # 2. Panel Builder Logic
  make_panel <- function(region_name, title_key, show_y_title = TRUE) {
    sub_data <- data |> dplyr::filter(region == region_name)

    ggplot2::ggplot(sub_data, ggplot2::aes(x = year, y = percentage)) +
      # Using a single line/point style consistent with your time series theme
      ggplot2::geom_line(linewidth = 0.8, color = "black") +
      ggplot2::geom_point(size = 3, shape = 16, color = "black") +

      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      ggplot2::scale_x_continuous(
        limits = c(global_min, global_max),
        breaks = x_breaks %||% seq(global_min, global_max, 4)
      ) +
      ggplot2::labs(title = terms[[lang]][[title_key]],
                    x = terms[[lang]][["xlab"]],
                    y = if(show_y_title) terms[[lang]][["ylab"]] else NULL) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  # 3. Generate and Assemble
  p_estuary <- make_panel("Estuary", "estuary", show_y_title = TRUE)
  p_gulf    <- make_panel("Gulf", "gulf", show_y_title = TRUE) # Maintains consistency

  # Side-by-side assembly
  p_estuary + p_gulf
}
