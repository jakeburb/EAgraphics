#' Compute and plot size spectrum slopes over time
#'
#' @param years year vector
#' @param EAR single EAR value (must be length 1)
#' @param min_points minimum number of data points required to fit slope (default = 5)
#' @param gam_k basis dimension for GAM smooth (default = 5)
#' @param ... additional arguments passed to ggplot theme
#' @description Computes the slope of log(abundance) vs size (not log-transformed) for each year,
#'              then plots the slope over time with a GAM smooth and confidence interval.
#'              The slope represents the size spectrum exponent.
#' @return A list containing: plot (ggplot object), slopes (data.frame with slope estimates)
#' @export
#' @examples
#' result <- plot_size_spectrum_slopes_f(years=1970:2025, EAR=200)
#' result <- plot_size_spectrum_slopes_f(years=1970:2025, EAR=100)
#' print(result$plot)
#' head(result$slopes)
plot_size_spectrum_slopes_f = function(years, EAR, min_points = 5, gam_k = 5, ...) {

  EA.data=size.spectrum.data.gsl # remove this line for the package, this is just a temporary hack to get temporary data

  # Load required library
  require(mgcv)
  require(ggplot2)

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

  # Convert to regular data.frame and ensure year is numeric
  size.spectrum = as.data.frame(size.spectrum)
  size.spectrum$year = as.numeric(as.character(size.spectrum$year))
  size.spectrum$EAR = as.numeric(as.character(size.spectrum$EAR))
  size.spectrum$value = as.numeric(as.character(size.spectrum$value))

  # Extract size range from variable name
  size.spectrum$size_range = gsub("size.class.abund.", "", size.spectrum$variable)

  # Extract lower and upper bounds from size_range
  size.spectrum$lower = as.numeric(gsub("from(\\d+)to\\d+", "\\1", size.spectrum$size_range))
  size.spectrum$upper = as.numeric(gsub("from\\d+to(\\d+)", "\\1", size.spectrum$size_range))

  # Calculate midpoint
  size.spectrum$midpoint = (size.spectrum$lower + size.spectrum$upper) / 2

  # Remove zero or negative values
  size.spectrum = size.spectrum[size.spectrum$value > 0 & !is.na(size.spectrum$value), ]

  # Calculate log-transformed abundance only (NOT log size)
  size.spectrum$log_abundance = log(size.spectrum$value)  # natural log

  # Compute slope for each year
  slopes = numeric()
  slope_ses = numeric()
  r_squareds = numeric()
  n_points_vec = numeric()
  year_vec = numeric()

  for(yr in sort(unique(size.spectrum$year))) {
    year_data = size.spectrum[size.spectrum$year == yr, ]

    # Only fit if we have enough points
    if(nrow(year_data) >= min_points) {
      # Fit linear model: log(abundance) ~ midpoint (NOT log-transformed)
      lm_fit = lm(log_abundance ~ midpoint, data = year_data)

      # Extract slope and standard error
      year_vec = c(year_vec, yr)
      slopes = c(slopes, coef(lm_fit)[2])
      slope_ses = c(slope_ses, summary(lm_fit)$coefficients[2, 2])
      r_squareds = c(r_squareds, summary(lm_fit)$r.squared)
      n_points_vec = c(n_points_vec, nrow(year_data))
    }
  }

  # Create slope data frame
  slope_data = data.frame(
    year = year_vec,
    slope = slopes,
    slope_se = slope_ses,
    r_squared = r_squareds,
    n_points = n_points_vec,
    stringsAsFactors = FALSE
  )

  # Check if we have slope data
  if(nrow(slope_data) == 0) {
    stop("Unable to compute slopes - insufficient data points in each year")
  }

  # Calculate y-axis limits based on data + error bars
  y_limits = range(c(slope_data$slope - 1.96 * slope_data$slope_se,
                     slope_data$slope + 1.96 * slope_data$slope_se))

  # Add 5% padding
  y_range = diff(y_limits)
  y_limits = c(y_limits[1] - 0.05 * y_range, y_limits[2] + 0.05 * y_range)

  # Fit GAM or linear model to slope over time
  method_used = "Linear"

  if(nrow(slope_data) > gam_k + 2) {
    # Adjust k if necessary
    k_use = min(gam_k, nrow(slope_data) - 2)

    tryCatch({
      gam_fit = gam(slope ~ s(year, bs = "cs", k = k_use), data = slope_data)

      # Predict with confidence intervals
      pred_years = seq(min(slope_data$year), max(slope_data$year), length.out = 100)
      gam_pred = predict(gam_fit, newdata = data.frame(year = pred_years), se.fit = TRUE)

      gam_data = data.frame(
        year = pred_years,
        fit = gam_pred$fit,
        lower = gam_pred$fit - 1.96 * gam_pred$se.fit,
        upper = gam_pred$fit + 1.96 * gam_pred$se.fit,
        stringsAsFactors = FALSE
      )

      method_used = "GAM"
    }, error = function(e) {
      # If GAM fails, fall back to linear model
      warning("GAM fitting failed, using linear model instead: ", e$message)
      lm_fit = lm(slope ~ year, data = slope_data)
      pred_years = seq(min(slope_data$year), max(slope_data$year), length.out = 100)
      lm_pred = predict(lm_fit, newdata = data.frame(year = pred_years), se.fit = TRUE)

      gam_data <<- data.frame(
        year = pred_years,
        fit = lm_pred$fit,
        lower = lm_pred$fit - 1.96 * lm_pred$se.fit,
        upper = lm_pred$fit + 1.96 * lm_pred$se.fit,
        stringsAsFactors = FALSE
      )
    })
  } else {
    # Not enough points for GAM, use linear model
    lm_fit = lm(slope ~ year, data = slope_data)
    pred_years = seq(min(slope_data$year), max(slope_data$year), length.out = 100)
    lm_pred = predict(lm_fit, newdata = data.frame(year = pred_years), se.fit = TRUE)

    gam_data = data.frame(
      year = pred_years,
      fit = lm_pred$fit,
      lower = lm_pred$fit - 1.96 * lm_pred$se.fit,
      upper = lm_pred$fit + 1.96 * lm_pred$se.fit,
      stringsAsFactors = FALSE
    )
  }

  # Create the plot
  p = ggplot(slope_data, aes(x = year, y = slope)) +
    geom_ribbon(data = gam_data, aes(x = year, y = NULL, ymin = lower, ymax = upper),
                fill = "#fdae61", alpha = 0.3) +
    geom_line(data = gam_data, aes(x = year, y = fit),
              color = "#d7191c", linewidth = 1.2) +
    geom_point(aes(size = r_squared), color = "#2c7bb6", alpha = 0.7) +
    geom_errorbar(aes(ymin = slope - 1.96 * slope_se, ymax = slope + 1.96 * slope_se),
                  width = 0.5, color = "#2c7bb6", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    scale_size_continuous(name = expression(R^2), range = c(2, 5)) +
    coord_cartesian(ylim = y_limits) +
    labs(
      x = "Year",
      y = "Size Spectrum Slope",
      title = paste("Size Spectrum Slope Over Time - EAR", EAR),
      subtitle = paste0("Slope from ln(abundance) ~ length; Smooth: ", method_used)
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray70", linewidth = 0.5),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(color = "gray30", hjust = 0, size = 10),
      legend.position = "right"
    )

  # Return both the plot and the slope data
  return(list(
    plot = p,
    slopes = slope_data,
    gam_predictions = gam_data,
    method = method_used
  ))
}
