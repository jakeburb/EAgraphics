#' Plot size spectrum for a single EAR
#'
#' @param years year vector
#' @param EAR single EAR value (must be length 1)
#' @param ... additional arguments passed to ggplot theme or geom_smooth
#' @description Plots fish size distribution across years for a single Ecosystem Approach Region (EAR).
#'              Uses a log scale for abundance and fits a GAM smooth to the data.
#' @return A ggplot object
#' @export
#' @examples
#' plot_size_spectrum_f(years=1970:2025, EAR=100)
#' plot_size_spectrum_f(years=1970:2025, EAR=200)
plot_size_spectrum_f = function(years, EAR, ...) {

  EA.data=size.spectrum.data.gsl # remove this line for the package, this is just a temporary hack to get temporary data

  # Error check: EAR must be length 1
  if(length(EAR) != 1) {
    stop("EAR must be a single value. You provided ", length(EAR), " values: ", paste(EAR, collapse=", "))
  }

  # Query the data using EA.query.f
  size.spectrum = EA.query.f(
    years = years,
    variables = unique(EA.data$variable[grep("size.class", EA.data$variable)]),
    EARs = EAR
  )

  # Check if data was returned
  if(is.null(size.spectrum) || nrow(size.spectrum) == 0) {
    stop("No size spectrum data found for EAR ", EAR, " in years ", min(years), "-", max(years))
  }

  # Extract size range from variable name
  size.spectrum$size_range = gsub("size.class.abund.", "", size.spectrum$variable)

  # Extract lower and upper bounds from size_range
  size.spectrum$lower = as.numeric(gsub("from(\\d+)to\\d+", "\\1", size.spectrum$size_range))
  size.spectrum$upper = as.numeric(gsub("from\\d+to(\\d+)", "\\1", size.spectrum$size_range))

  # Calculate midpoint
  size.spectrum$midpoint = (size.spectrum$lower + size.spectrum$upper) / 2

  # Create value_positive (handle any negative or zero values)
  size.spectrum$value_positive = ifelse(size.spectrum$value <= 0, 1e-6, size.spectrum$value)

  # Create the plot
  p = ggplot(size.spectrum, aes(x = midpoint, y = value_positive)) +
    geom_point(alpha = 0.6, size = 2, color = "#2c7bb6") +
    geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cs", k = 5),
      se = TRUE,
      color = "#d7191c",
      linewidth = 1,
      fill = "#fdae61",
      alpha = 0.3
    ) +
    facet_wrap(~year, ncol = 4) +
    scale_y_log10(
      labels = scales::label_number(accuracy = 0.1),
      breaks = scales::breaks_log(n = 6)
    ) +
    annotation_logticks(sides = "l", linewidth = 0.3, alpha = 0.5) +
    labs(
      x = "Size Class Midpoint (cm)",
      y = "Abundance (log scale)",
      title = paste("Fish Size Distribution Across Years - EAR", EAR),
      subtitle = "Gulf of St. Lawrence, EAR 100= Northern Gulf; EAR 200= Southern Gulf"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#e0e0e0", color = "gray50"),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray70", linewidth = 0.5),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "gray30", hjust = 0)
    )

  return(p)
}

