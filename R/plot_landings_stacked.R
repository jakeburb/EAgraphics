#' Plot Stacked Landings or Biomass by Group
#'
#' @description
#' Visualizes fisheries landings or biomass trends over time using a stacked area plot.
#' Supports bilingual labeling (via an internal dictionary and \code{rosettafish}),
#' custom color palettes, and adjustable text scaling.
#' Automatically detects the legend title (Group vs Guild) and stacking order
#' based on the content of the data.
#'
#' @param data A data frame containing landings or biomass.
#' @param year_col Unquoted name of the year column. Defaults to \code{year}.
#' @param group_col Unquoted name of the grouping column (e.g., species group). Defaults to \code{grp}.
#' @param value_col Unquoted name of the landings or biomass value column. Defaults to \code{landings}.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Numeric vector \code{c(start, end)} to filter the timeline.
#' @param group_order Character vector defining the visual order from \strong{top to bottom}.
#'    If \code{NULL}, the function auto-detects based on species names in the data.
#' @param show_title Logical. Defaults to \code{FALSE}.
#' @param col_palette Optional named character vector of colors.
#' @param base_size Numeric. Base font size for the plot. Defaults to 14.
#' @param xlab,ylab Optional strings to override default axis labels.
#' @param legend_title Optional string to override the automatic legend title detection.
#' @param x_breaks Optional numeric vector for x-axis tick marks (e.g. \code{seq(1990, 2025, 5)}).
#' @param y_breaks Optional numeric vector for y-axis tick marks.
#'        Defaults to \code{ggplot2::waiver()} (automatic).
#' @param custom_theme A ggplot2 theme. Defaults to \code{theme_bw()}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Load your data
#' landings_data <- read.csv("landings_by_group.csv")
#'
#' # 1. Basic plot with default order (Top: Pelagics -> Middle: Crustaceans -> Bottom: Groundfish)
#' plot_landings_stacked(landings_data)
#'
#' # 2. French version, custom year range, and larger text scaling
#' plot_landings_stacked(landings_data,
#'                        lang = "fr",
#'                        year_range = c(1990, 2023),
#'                        base_size = 16)
#'
#' # 3. Custom stack order: Pelagics on top, Crustaceans on bottom
#' plot_landings_stacked(landings_data,
#'                        group_order = c("pelagics", "groundfish", "crustaceans"))
#'
#' # 4. Custom colour palette with rosettafish fallback
#' my_pal <- c("Crustaceans" = "#8b0000",
#'              "Groundfish"  = "#4682b4",
#'              "Pelagics"    = "#2e8b57")
#'
#' plot_landings_stacked(landings_data, col_palette = my_pal)
#' }
#' @export
plot_landings_stacked <- function(data,
                                  year_col = year,
                                  group_col = grp,
                                  value_col = landings,
                                  lang = "en",
                                  year_range = NULL,
                                  group_order = NULL,
                                  show_title = FALSE,
                                  col_palette = NULL,
                                  base_size = 14,
                                  xlab = NULL,
                                  ylab = NULL,
                                  legend_title = NULL,
                                  x_breaks = NULL,
                                  y_breaks = ggplot2::waiver(),
                                  custom_theme = ggplot2::theme_bw()) {

  # 1. Setup
  yr_enquo  <- rlang::enquo(year_col)
  grp_enquo <- rlang::enquo(group_col)
  val_enquo <- rlang::enquo(value_col)

  # 2. Local Dictionary
  terms <- list(
    en = c(title = "Fisheries Landings by Group", xlab = "Year", ylab = "Landings (t)",
           leg_group = "Group", leg_guild = "Guild",
           crustaceans = "Crustaceans", groundfish = "Groundfish", pelagics = "Pelagics",
           "Atlantic herring" = "Atlantic herring",
           "Atlantic mackerel" = "Atlantic mackerel",
           "Northern shrimp" = "Northern shrimp",
           "piscivorous fishes and invertebrates" = "Piscivorous fishes and invertebrates",
           "limivorous and detritivorous fishes and invertebrates" = "Limivorous and detritivorous fishes and invertebrates",
           "benthivorous fishes" = "Benthivorous fishes",
           "planktivorous fishes" = "Planktivorous fishes",
           "mesopelagic micronecton" = "Mesopelagic micronekton",
           "demersal micronecton" = "Demersal micronekton",
           "filter-feeding invertebrates" = "Filter-feeding invertebrates",
           "benthivorous invertebrates" = "Benthivorous invertebrates"),
    fr = c(title = "Débarquements de pêche par groupe", xlab = "Année", ylab = "Débarquements (t)",
           leg_group = "Groupe", leg_guild = "Guilde",
           crustaceans = "Crustacés", groundfish = "Poissons de fond", pelagics = "Pélagiques",
           "Atlantic herring" = "Hareng de l'Atlantique",
           "Atlantic mackerel" = "Maquereau de l'Atlantique",
           "Northern shrimp" = "Crevette nordique",
           "piscivorous fishes and invertebrates" = "Poissons piscivores et invertébrés",
           "limivorous and detritivorous fishes and invertebrates" = "Poissons limivores et détritivores et invertébrés",
           "benthivorous fishes" = "Poissons benthivores",
           "planktivorous fishes" = "Poissons planctonivores",
           "mesopelagic micronecton" = "Micronecton mésopélagique",
           "demersal micronecton" = "Micronecton démersal",
           "filter-feeding invertebrates" = "Invertébrés filtreurs",
           "benthivorous invertebrates" = "Invertébrés benthivores")
  )

  get_term <- function(x, dictionary, language) {
    if (x %in% names(dictionary)) return(dictionary[[x]])
    if (language == "fr" && requireNamespace("rosettafish", quietly = TRUE)) {
      return(rosettafish::en2fr(x))
    }
    return(x)
  }

  # 3. Smart Default Detection
  unique_in_data <- unique(as.character(data[[rlang::as_label(grp_enquo)]]))
  guild_keys <- names(terms$en)[12:19]
  is_guild_data <- any(unique_in_data %in% guild_keys)

  if (is_guild_data) {
    auto_order <- guild_keys
  } else if (any(unique_in_data %in% c("Northern shrimp", "Atlantic herring", "Atlantic mackerel"))) {
    auto_order <- c("Northern shrimp","Atlantic herring", "Atlantic mackerel")
  } else {
    auto_order <- c("pelagics", "crustaceans", "groundfish")
  }

  if (is.null(group_order)) {
    group_order <- auto_order
  }

  if (is.null(legend_title)) {
    legend_key <- if (is_guild_data) "leg_guild" else "leg_group"
    final_leg_title <- terms[[lang]][[legend_key]]
  } else {
    final_leg_title <- legend_title
  }

  # 4. Data Prep
  df <- data |>
    dplyr::rename(yr = !!yr_enquo, grp = !!grp_enquo, val = !!val_enquo) |>
    dplyr::filter(!is.na(val))

  if (!is.null(year_range)) {
    df <- df |> dplyr::filter(yr >= year_range[1], yr <= year_range[2])
  }

  # 5. Factor Ordering & Translation
  df$grp <- factor(as.character(df$grp), levels = group_order)
  current_levs <- levels(df$grp)
  translated_levs <- sapply(current_levs, get_term, dictionary = terms[[lang]], language = lang)
  levels(df$grp) <- unname(translated_levs)

  # 6. Colors
  if (is.null(col_palette)) {
    base_pal <- c(
      "Crustaceans" = "#D55E00", "Crustacés" = "#D55E00",
      "Groundfish"  = "#0072B2", "Poissons de fond" = "#0072B2",
      "Pelagics"    = "#009E73", "Pélagiques" = "#009E73",
      "Atlantic herring" = "#66c2a5", "Hareng de l'Atlantique" = "#66c2a5",
      "Atlantic mackerel" = "#8da0cb", "Maquereau de l'Atlantique" = "#8da0cb",
      "Northern shrimp" = "#e78ac3", "Crevette nordique" = "#e78ac3"
    )
    col_palette <- base_pal[names(base_pal) %in% levels(df$grp)]

    if (length(col_palette) < length(levels(df$grp))) {
      col_palette <- scales::hue_pal()(length(levels(df$grp)))
      names(col_palette) <- levels(df$grp)
    }
  }

  # 7. Axis Logic
  final_xlab <- if(!is.null(xlab)) xlab else get_term("xlab", terms[[lang]], lang)
  final_ylab <- if(!is.null(ylab)) ylab else get_term("ylab", terms[[lang]], lang)

  if (is.null(x_breaks)) {
    x_breaks <- ggplot2::waiver()
  }

  # 8. Build Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = yr, y = val, fill = grp)) +
    ggplot2::geom_area(alpha = 0.8, color = "white", linewidth = 0.3,
                       position = ggplot2::position_stack(reverse = FALSE)) +
    ggplot2::scale_fill_manual(values = col_palette) +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = x_breaks) +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                expand = ggplot2::expansion(mult = c(0, 0.15)),
                                breaks = y_breaks) +
    ggplot2::labs(x = final_xlab, y = final_ylab, fill = final_leg_title) +
    custom_theme +
    ggplot2::theme(text = ggplot2::element_text(size = base_size),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.position = "right")

  if (show_title) {
    p <- p + ggplot2::labs(title = get_term("title", terms[[lang]], lang)) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
  }

  return(p)
}

