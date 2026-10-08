#' Plot Size Spectrum Anomalies
#'
#' @description
#' Calculates and visualizes size spectrum anomalies from RV surveys independently for 2 regions: the nGSL and sGSL.
#' Uses a binned color scale with explicit '<-3' and '>3' labels. Aligns x-axes perfectly across a shared timeline, ensuring 1984
#' and edge years are fully rendered without clipping.
#'
#' @param data A data frame containing 'year', 'EAR', 'variable', and 'value' (raw data).
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Optional numeric vector \code{c(start, end)} to set a fixed timeline.
#'        Defaults to the full range found in the data (e.g., 1984-2025).
#' @param x_breaks Optional numeric vector for x-axis tick marks.
#' @param standardize Logical. If \code{TRUE} (default), calculates Z-scores.
#' @param log_transform Logical. If \code{TRUE} (default), log-transforms values before calculation.
#' @param base_size Numeric. Base font size. Defaults to 14.
#'
#' @return A \code{patchwork} object with two stacked panels (A) and (B).
#'
#' @examples
#' \dontrun{
#' # 1. Basic plot with binned scale and automatic year detection
#' plot_size_spectrum_anomalies(size.spectrum.data.gsl)
#'
#' # 2. French version with a specific year range and 5-year increments
#' plot_size_spectrum_anomalies(size.spectrum.data.gsl,
#'                               lang = "fr",
#'                               year_range = c(1984, 2024),
#'                               x_breaks = seq(1984, 2024, 5))
#' }
#' @export
plot_size_spectrum_anomalies <- function(data,
                                         lang = "en",
                                         year_range = NULL,
                                         x_breaks = NULL,
                                         standardize = TRUE,
                                         log_transform = FALSE,
                                         base_size = 14) {

  # 1. Dictionary
  terms <- list(
    en = c(xlab = "Year", ylab = "Length class (cm)", leg = "Anomaly"),
    fr = c(xlab = "Année", ylab = "Classe de longueur (cm)", leg = "Anomalie")
  )

  # 2. Independent Anomaly Calculation
  df <- data |>
    dplyr::mutate(working_val = if(log_transform) log1p(value) else value) |>
    dplyr::group_by(EAR, variable) |>
    dplyr::mutate(
      mean_val = mean(working_val, na.rm = TRUE),
      sd_val   = sd(working_val, na.rm = TRUE),
      anomaly  = if(standardize) {
        dplyr::if_else(sd_val > 0, (working_val - mean_val) / sd_val, 0)
      } else {
        working_val - mean_val
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      nums = stringr::str_extract_all(variable, "\\d+"),
      low  = purrr::map_chr(nums, ~ .x[1]),
      high = purrr::map_chr(nums, ~ .x[2]),
      size_grp = paste0("[", low, "-", high, "["),
      size_grp = stats::reorder(size_grp, as.numeric(low))
    )

  # 3. Timeline Alignment Logic
  data_min <- min(df$year, na.rm = TRUE)
  data_max <- max(df$year, na.rm = TRUE)

  if (!is.null(year_range)) {
    global_min <- year_range[1]
    global_max <- year_range[2]
  } else {
    global_min <- data_min
    global_max <- data_max
  }

  # 4. Panel Builder Helper
  make_panel <- function(sub_data, show_x = TRUE) {
    ggplot2::ggplot(sub_data, ggplot2::aes(x = year, y = size_grp, fill = anomaly)) +
      ggplot2::geom_tile(color = "black", linewidth = 0.2, na.rm = TRUE) +
      # Binned Scale strictly following scientific reporting standards
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
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0, add = 0.6),
                                  limits = c(global_min - 0.5, global_max + 0.5),
                                  breaks = x_breaks %||% seq(global_min, global_max, 5)) +
      ggplot2::labs(x = if(show_x) terms[[lang]][["xlab"]] else NULL,
                    y = terms[[lang]][["ylab"]],
                    fill = terms[[lang]][["leg"]]) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = if(!show_x) ggplot2::element_blank() else ggplot2::element_text(angle = 90, vjust = 0.5),
        axis.ticks.x = if(!show_x) ggplot2::element_blank() else ggplot2::element_line()
      )
  }

  # 5. Assemble
  p1 <- make_panel(dplyr::filter(df, EAR == 100), show_x = FALSE)
  p2 <- make_panel(dplyr::filter(df, EAR == 200), show_x = TRUE)

  (p1 / p2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(tag_levels = 'A') &
    ggplot2::theme(legend.position = "bottom")
}
