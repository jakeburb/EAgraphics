#' Plot Trophic Guild Condition Anomalies
#'
#' @description
#' Calculates and visualizes condition Z-score anomalies for habitat guilds
#' (Pelagic, Benthopelagic, Benthic, Demersal) and a combined "All" group.
#' Stacks EAR 100 (nGSL) and 200 (sGSL) with aligned axes and binned color scale.
#'
#' @param data A data frame containing 'year', 'EAR', 'HabitatGuild', and 'value'.
#' @param lang Language for labels: \code{"en"} (default) or \code{"fr"}.
#' @param year_range Optional numeric vector \code{c(start, end)} to set a fixed timeline.
#' @param x_breaks Optional numeric vector for x-axis ticks.
#' @param base_size Numeric. Base font size. Defaults to 14.
#'
#' @return A \code{patchwork} object with two stacked panels (A and B).
#'
#' @examples
#' \dontrun{
#' # 1. Standard English plot with automatic year range
#' plot_condition_guild_anomalies(CONsGSL, lang = "en")
#'
#' # 2. French version with specific timeline and 5-year breaks
#' plot_condition_guild_anomalies(CONsGSL,
#'                                lang = "fr",
#'                                year_range = c(1984, 2024),
#'                                x_breaks = seq(1984, 2024, 5))
#' }
#' @export
plot_condition_guild_anomalies <- function(data,
                                           lang = "en",
                                           year_range = NULL,
                                           x_breaks = NULL,
                                           base_size = 14) {

  # 1. Dictionaries for labels and Guild names
  terms <- list(
    en = c(xlab = "Year", ylab = "Habitat Guild", leg = "Anomaly",
           Pelagic = "Pelagic", Benthopelagic = "Benthopelagic",
           Benthic = "Benthic", Demersal = "Demersal", All = "All"),
    fr = c(xlab = "Année", ylab = "Guilde d'habitat", leg = "Anomalie",
           Pelagic = "Pélagique", Benthopelagic = "Benthopélagique",
           Benthic = "Benthique", Demersal = "Démersal", All = "Tous")
  )

  # 2. Data Preparation & "All" Group Creation
  guilds_of_interest <- c("Pelagic", "Benthopelagic", "Benthic", "Demersal")

  df_guilds <- data |>
    dplyr::filter(HabitatGuild %in% guilds_of_interest)

  df_all <- data |>
    dplyr::filter(HabitatGuild %in% guilds_of_interest) |>
    dplyr::mutate(HabitatGuild = "All")

  # 3. Calculate Z-Scores & Translate
  df_final <- dplyr::bind_rows(df_guilds, df_all) |>
    dplyr::group_by(EAR, HabitatGuild, year) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(EAR, HabitatGuild) |>
    dplyr::mutate(
      mean_val = mean(value, na.rm = TRUE),
      sd_val   = sd(value, na.rm = TRUE),
      anomaly  = (value - mean_val) / sd_val,
      # Map to translated names from dictionary
      GuildLabel = terms[[lang]][HabitatGuild],
      # Order: Pelagic (top) -> Benthopelagic -> Benthic -> Demersal -> All (bottom)
      GuildLabel = factor(GuildLabel, levels = rev(c(
        terms[[lang]][["Pelagic"]],
        terms[[lang]][["Benthopelagic"]],
        terms[[lang]][["Benthic"]],
        terms[[lang]][["Demersal"]],
        terms[[lang]][["All"]]
      )))
    ) |>
    dplyr::ungroup()

  # 4. Timeline Alignment
  global_min <- if(!is.null(year_range)) year_range[1] else min(df_final$year, na.rm = TRUE)
  global_max <- if(!is.null(year_range)) year_range[2] else max(df_final$year, na.rm = TRUE)

  # 5. Panel Builder
  make_panel <- function(sub_data, show_x = TRUE) {
    ggplot2::ggplot(sub_data, ggplot2::aes(x = year, y = GuildLabel, fill = anomaly)) +
      ggplot2::geom_tile(color = "black", linewidth = 0.2, na.rm = TRUE) +
      ggplot2::scale_fill_steps2(
        low = "#0000FF", mid = "white", high = "#FF0000",
        midpoint = 0,
        breaks = c(-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5),
        labels = c("<-3.5", "-2.5", "-1.5", "-0.5", "0.5", "1.5", "2.5", ">3.5"),
        limits = c(-3.5, 3.5),
        oob = scales::squish,
        guide = ggplot2::guide_colorsteps(
          barwidth = 20,
          barheight = 1,
          show.limits = FALSE,
          title.position = "top",
          title.hjust = 0.5,
          # Outline settings for legend consistency
          frame.colour = "black",
          frame.linewidth = 0.5,
          ticks.colour = "black",
          ticks.linewidth = 0.5
        )
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.6),
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

  # 6. Assemble with patchwork
  p1 <- make_panel(dplyr::filter(df_final, EAR == 100), show_x = FALSE)
  p2 <- make_panel(dplyr::filter(df_final, EAR == 200), show_x = TRUE)

  (p1 / p2) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(tag_levels = 'A') &
    ggplot2::theme(legend.position = "bottom")
}
