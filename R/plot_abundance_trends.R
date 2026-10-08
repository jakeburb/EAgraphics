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
