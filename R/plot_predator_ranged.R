#' Plot Ranged Predator Abundance
#'
#' @description
#' Visualizes the relative abundance trends of key predators using a "ranged" (Min-Max)
#' normalization: \eqn{(Value - Min) / (Max - Min)}. This allows comparison of species
#' with different absolute population sizes. To support accessibility for visually
#' impaired users, the function maps both color and distinct geometric shapes to
#' each species. The legend displays the original min/max values for context and
#' remains stable across languages.
#'
#' @param data A data frame. Defaults to \code{gslea::EA.data}.
#' @param year_col Unquoted year column. Defaults to \code{year}.
#' @param var_col Unquoted variable column. Defaults to \code{variable}.
#' @param val_col Unquoted value column. Defaults to \code{value}.
#' @param year_range Numeric vector \code{c(start, end)} to filter the timeline.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param show_legend Logical. Defaults to \code{TRUE}.
#' @param legend_position Numeric vector \code{c(x, y)} for internal placement. Defaults to \code{c(0.05, 0.95)}.
#' @param y_label Optional string to override the default Y-axis label.
#' @param base_size Numeric. Base font size for the plot. Defaults to \code{14}.
#' @param palette Optional color vector. Must match the number of predators.
#' @param species_metadata Optional mapping data frame with columns: 'variable', 'en', 'fr'.
#'
#' @return A \code{ggplot} object with combined color and shape scales.
#'
#' @examples
#' \dontrun{
#' # 1. Basic English plot with accessibility shapes
#' plot_predator_ranged(data = my_data, lang = "en")
#'
#' # 2. French version with a custom year range and internal legend
#' plot_predator_ranged(data = my_data,
#'                      lang = "fr",
#'                      year_range = c(1990, 2024),
#'                      legend_position = c(0.1, 0.9))
#' }
#' @export
plot_predator_ranged <- function(data = gslea::EA.data,
                                 year_col = year,
                                 var_col = variable,
                                 val_col = value,
                                 year_range = NULL,
                                 lang = "en",
                                 show_legend = TRUE,
                                 legend_position = c(0.05, 0.95),
                                 y_label = NULL,
                                 base_size = 14,
                                 palette = NULL,
                                 species_metadata = NULL) {

  # 1. Setup Enquo
  yr_enquo  <- rlang::enquo(year_col)
  var_enquo <- rlang::enquo(var_col)
  val_enquo <- rlang::enquo(val_col)

  # Default Metadata if none provided
  if (is.null(species_metadata)) {
    pred_meta <- data.frame(
      variable = c("harpseal.totalabundance.nwatl.2024", "greyseal.totalabundance.acw.2021", "gannet.n", "abft.n"),
      en = c("Harp Seal", "Grey Seal", "Northern Gannet", "Atlantic Bluefin Tuna"),
      fr = c("Phoque du Groenland", "Phoque gris", "Fou de Bassan", "Thon rouge de l'Atlantique"),
      stringsAsFactors = FALSE
    )
  } else {
    pred_meta <- species_metadata
  }

  terms <- list(
    en = c(xlab = "Year", ylab = "Ranged Abundance", min = "Min", max = "Max"),
    fr = c(xlab = "Année", ylab = "Abondance indexée", min = "Min", max = "Max")
  )

  # 2. Data Preparation
  df <- data |>
    dplyr::rename(yr = !!yr_enquo, var = !!var_enquo, val = !!val_enquo) |>
    dplyr::mutate(yr = as.numeric(as.character(yr))) |>
    dplyr::filter(var %in% pred_meta$variable, !is.na(val))

  if (!is.null(year_range)) {
    df <- df |> dplyr::filter(yr >= year_range[1], yr <= year_range[2])
  }

  # 3. Ranging & Labeling
  df_ranged <- df |>
    dplyr::group_by(var) |>
    dplyr::mutate(
      min_val = min(val, na.rm = TRUE),
      max_val = max(val, na.rm = TRUE),
      ranged_val = (val - min_val) / (max_val - min_val)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(pred_meta, by = c("var" = "variable")) |>
    dplyr::mutate(
      species_name = if(lang == "fr") fr else en,
      legend_label = paste0(species_name, " (", terms[[lang]][["min"]], ": ",
                            scales::comma(round(min_val)), ", ",
                            terms[[lang]][["max"]], ": ", scales::comma(round(max_val)), ")")
    )

  # 4. Legend Order & Aesthetics Stability
  ordered_labels <- df_ranged |>
    dplyr::select(var, legend_label) |>
    dplyr::distinct() |>
    dplyr::slice(match(pred_meta$variable, var)) |>
    dplyr::pull(legend_label)

  df_ranged$legend_label <- factor(df_ranged$legend_label, levels = ordered_labels)

  # Colors
  if (is.null(palette)) {
    palette <- if(length(ordered_labels) <= 4) {
      c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
    } else {
      scales::hue_pal()(length(ordered_labels))
    }
  }
  names(palette) <- ordered_labels

  # Accessible Shapes: 16=Circle, 17=Triangle, 15=Square, 18=Diamond
  shapes <- c(16, 17, 15, 18)
  # Handle case where there might be more than 4 predators
  if(length(ordered_labels) > 4) shapes <- c(shapes, seq(1, length(ordered_labels)-4))
  shapes <- shapes[1:length(ordered_labels)]
  names(shapes) <- ordered_labels

  # 5. Build Plot
  final_ylab <- if(!is.null(y_label)) y_label else terms[[lang]][["ylab"]]
  leg_just <- if(is.numeric(legend_position)) c(0, 1) else "center"

  p <- ggplot2::ggplot(df_ranged, ggplot2::aes(x = yr, y = ranged_val,
                                               color = legend_label,
                                               shape = legend_label)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::labs(x = terms[[lang]][["xlab"]], y = final_ylab, color = NULL, shape = NULL) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = if(show_legend) legend_position else "none",
      legend.justification = leg_just,
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.7), color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
    )

  return(p)
}
