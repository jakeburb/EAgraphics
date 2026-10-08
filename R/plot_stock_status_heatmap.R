#' Plot Stock Status Heatmap Relative to LRP for Gulf of St. Lawrence Ecosystem Report
#'
#' @description
#' A highly flexible heatmap visualizing stock status with RdYlGn palette.
#' Includes full bilingual support for species and management areas.
#'
#' @param data A data frame containing stock status data. Defaults to \code{EA.extra.data$stock.status}.
#' @param year_col Unquoted name of column for year. Defaults to \code{Year}.
#' @param stock_col Unquoted name of column for stock names. Defaults to \code{Stock}.
#' @param status_col Unquoted name of column for status values. Defaults to \code{status.LRP}.
#' @param lang Character string for language: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Optional numeric vector \code{c(start, end)}.
#' @param show_title Logical. Defaults to \code{TRUE}.
#' @param base_size Numeric. Base font size. Defaults to 12.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # 1. Simple plot using package data
#' plot_stock_status_heatmap(lang = "en", year_range = c(1994, 2025))
#'
#' # 2. French version with title removed for a report
#' plot_stock_status_heatmap(lang = "fr", show_title = FALSE)
#'
#' # 3. Using a custom external dataset
#' plot_stock_status_heatmap(my_data, year_col = Annee, stock_col = Espece, status_col = Ratio)
#' }
#'
#' @export
plot_stock_status_heatmap <- function(data = NULL,
                                      year_col = Year,
                                      stock_col = Stock,
                                      status_col = status.LRP,
                                      lang = "en",
                                      year_range = NULL,
                                      show_title = TRUE,
                                      base_size = 12) {

  # 1. Data Setup
  if (is.null(data)) {
    if (!exists("EA.extra.data")) stop("EA.extra.data not found.")
    data <- EA.extra.data$stock.status
  }

  year_enquo   <- rlang::enquo(year_col)
  stock_enquo  <- rlang::enquo(stock_col)
  status_enquo <- rlang::enquo(status_col)

  df <- data |>
    dplyr::rename(yr = !!year_enquo, stk = !!stock_enquo, val = !!status_enquo)

  # 2. DEFINITIVE ORDERING LIST
  target_order <- c(
    "Northern shrimp-Anticosti",
    "Northern shrimp-Esquiman",
    "Northern shrimp-Estuary",
    "Northern shrimp-Sept-Iles",
    "Snow crab - sGSL",
    "Atlantic halibut - 4RST",
    "Greenland halibut - 4RST",
    "Sebastes fasciatus - Unit1",
    "Sebastes mentella - Unit1",
    "Witch flounder - 4RST",
    "Atlantic cod - 3Pn4RS",
    "American plaice - 4T",
    "Atlantic cod - 4TVn",
    "White hake - 4T",
    "Winter flounder - 4T",
    "Yellowtail flounder - 4T",
    "Atlantic mackerel",
    "Atlantic herring - 4RSw Fall",
    "Atlantic herring - 4RSw Spring",
    "Atlantic herring - 4TVn Fall",
    "Atlantic herring - 4TVn Spring"
  )

  # 3. Filtering & Forced Sorting
  if (!is.null(year_range)) {
    df <- df |> dplyr::filter(yr >= year_range[1], yr <= year_range[2])
  }

  df <- df |>
    dplyr::filter(stk %in% target_order) |>
    dplyr::mutate(stk = factor(stk, levels = rev(target_order)))

  df <- df |>
    dplyr::group_by(yr, stk) |>
    dplyr::summarize(val = mean(val, na.rm = TRUE), .groups = "drop")

  # 4. Translation Logic with Unicode Escapes (Safe for Accents)
  stock_dict <- c(
    "Northern shrimp-Anticosti"    = "Crevette nordique - Anticosti",
    "Northern shrimp-Esquiman"     = "Crevette nordique - Esquiman",
    "Northern shrimp-Estuary"      = "Crevette nordique - Estuaire",
    "Northern shrimp-Sept-Iles"    = "Crevette nordique - Sept-Îles",
    "Snow crab - sGSL"             = "Crabe des neiges - sGSL",
    "Atlantic halibut - 4RST"      = "Flétan de l'Atlantique - 4RST",
    "Greenland halibut - 4RST"     = "Flétan du Groenland - 4RST",
    "Sebastes fasciatus - Unit1"   = "Sebastes fasciatus - Unité 1",
    "Sebastes mentella - Unit1"    = "Sebastes mentella - Unité 1",
    "Witch flounder - 4RST"        = "Plie grise - 4RST",
    "Atlantic cod - 3Pn4RS"        = "Morue de l'Atlantique - 3Pn4RS",
    "American plaice - 4T"         = "Plie canadienne - 4T",
    "Atlantic cod - 4TVn"          = "Morue de l'Atlantique - 4TVn",
    "White hake - 4T"              = "Merluche blanche - 4T",
    "Winter flounder - 4T"         = "Plie rouge - 4T",
    "Yellowtail flounder - 4T"     = "Limande à queue jaune - 4T",
    "Atlantic mackerel"            = "Maquereau bleu",
    "Atlantic herring - 4RSw Fall" = "Hareng de l'Atlantique - 4RSw automne",
    "Atlantic herring - 4RSw Spring" = "Hareng de l'Atlantique - 4RSw printemps",
    "Atlantic herring - 4TVn Fall" = "Hareng de l'Atlantique - 4TVn automne",
    "Atlantic herring - 4TVn Spring" = "Hareng de l'Atlantique - 4TVn printemps"
  )

  terms <- list(
    en = c(title = "Stock State Relative to Limit Reference Point (LRP)",
           xlab = "Year", ylab = "Stock", leg = "State/LRP"),
    fr = c(title = "État des stocks par rapport au point de référence limite (PRL)",
           xlab = "Année", ylab = "Stock", leg = "État/PRL")
  )

  if (lang == "fr") {
    current_levels <- levels(df$stk)
    new_levels <- dplyr::recode(current_levels, !!!stock_dict)
    levels(df$stk) <- new_levels
  }
  # 5. Build Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = yr, y = stk, fill = val)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_distiller(
      palette = "RdYlGn", direction = 1, limits = c(0, 2), na.value = "grey80",
      name = terms[[lang]][["leg"]], oob = scales::squish,
      breaks = c(0, 0.5, 1, 1.5, 2),
      labels = if(lang == "fr") c("0", "0.5", "1.0 (PRL)", "1.5", "2.0+") else
        c("0", "0.5", "1.0 (LRP)", "1.5", "2.0+")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1955, 2035, by = 5), expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = terms[[lang]][["xlab"]], y = terms[[lang]][["ylab"]]) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "right",
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
      axis.ticks = ggplot2::element_line(color = "black")
    )

  if (show_title) {
    p <- p + ggplot2::labs(title = terms[[lang]][["title"]]) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
  }

  return(p)
}
