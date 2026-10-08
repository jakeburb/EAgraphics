#' Plot Predator Abundance (Raw Scaled)
#'
#' @description
#' Visualizes abundance trends using raw values with specific scaling factors
#' (Harp Seals / 100, Grey Seals / 10) as requested by peer review.
#' Includes regional tags (NWA/GSL) and distinct shapes for accessibility.
#'
#' @param data A data frame. Defaults to \code{gslea::EA.data}.
#' @param year_col Unquoted year column.
#' @param var_col Unquoted variable column.
#' @param val_col Unquoted value column.
#' @param lang Language: \code{"en"} (default) or \code{"fr"}.
#' @param show_legend Logical. Defaults to \code{TRUE}.
#' @param legend_position Position of the legend. Defaults to \code{c(0.05, 0.95)}.
#' @param base_size Numeric. Defaults to \code{14}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' plot_predator_raw_scaled(my_data, lang = "en")
#' }
#' @export
plot_predator_raw_scaled <- function(data = gslea::EA.data,
                                     year_col = year,
                                     var_col = variable,
                                     val_col = value,
                                     year_range = NULL,
                                     lang = "en",
                                     show_legend = TRUE,
                                     legend_position = c(0.05, 0.95),
                                     base_size = 14) {

  # 1. Setup
  yr_enquo  <- rlang::enquo(year_col)
  var_enquo <- rlang::enquo(var_col)
  val_enquo <- rlang::enquo(val_col)

  # Updated Metadata with Peer Review requirements (NWA/GSL tags)
  pred_meta <- data.frame(
    variable = c("harpseal.totalabundance.nwatl.2024", "greyseal.totalabundance.acw.2021", "gannet.n", "abft.n"),
    en = c("Harp Seal (NWA)", "Grey Seal (NWA)", "Northern Gannet (GSL)", "Atlantic Bluefin Tuna (GSL)"),
    fr = c("Phoque du Groenland (ANO)", "Phoque gris (ANO)", "Fou de Bassan (GSL)", "Thon rouge de l'Atlantique (GSL)"),
    scale_factor = c(100, 10, 1, 1), # The division factors
    stringsAsFactors = FALSE
  )

  terms <- list(
    en = c(xlab = "Year", ylab = "Abundance", unit100 = "/ 100", unit10 = "/ 10"),
    fr = c(xlab = "Année", ylab = "Abondance", unit100 = "/ 100", unit10 = "/ 10")
  )

  # 2. Data Prep & Transformation
  df_plot <- data |>
    dplyr::rename(yr = !!yr_enquo, var = !!var_enquo, val = !!val_enquo) |>
    dplyr::mutate(yr = as.numeric(as.character(yr))) |>
    dplyr::filter(var %in% pred_meta$variable, !is.na(val)) |>
    dplyr::left_join(pred_meta, by = c("var" = "variable")) |>
    # Apply the Reviewer's scaling math
    dplyr::mutate(display_val = val / scale_factor)

  if (!is.null(year_range)) {
    df_plot <- df_plot |> dplyr::filter(yr >= year_range[1], yr <= year_range[2])
  }

  # 3. Dynamic Legend Labels (Adding the "divided by" indicator)
  df_plot <- df_plot |>
    dplyr::mutate(
      species_name = if(lang == "fr") fr else en,
      legend_label = dplyr::case_when(
        scale_factor == 100 ~ paste(species_name, terms[[lang]][["unit100"]]),
        scale_factor == 10 ~ paste(species_name, terms[[lang]][["unit10"]]),
        TRUE ~ species_name
      )
    )

  # 4. Aesthetics Setup
  ordered_vars <- pred_meta$variable
  ordered_labels <- df_plot |>
    dplyr::select(var, legend_label) |>
    dplyr::distinct() |>
    dplyr::slice(match(ordered_vars, var)) |>
    dplyr::pull(legend_label)

  df_plot$legend_label <- factor(df_plot$legend_label, levels = ordered_labels)

  palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
  names(palette) <- ordered_labels
  shapes <- c(16, 17, 15, 18)
  names(shapes) <- ordered_labels

  # 5. Build Plot
  leg_just <- if(is.numeric(legend_position)) c(0, 1) else "center"

  ggplot2::ggplot(df_plot, ggplot2::aes(x = yr, y = display_val, color = legend_label, shape = legend_label)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::labs(x = terms[[lang]][["xlab"]], y = terms[[lang]][["ylab"]], color = NULL, shape = NULL) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = if(show_legend) legend_position else "none",
      legend.justification = leg_just,
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.7), color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1)
    )
}
