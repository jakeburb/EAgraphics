#' Calculate Anomalies from a Baseline Mean
#'
#' @description
#' Calculates the anomaly of a variable relative to a user-specified baseline
#' period. Returns a focused data frame containing only the year, the original
#' value, the baseline mean used, and the resulting anomaly.
#'
#' @param data A data frame containing at least \code{year} and \code{value} columns.
#' @param baseline_range Numeric vector \code{c(start, end)} defining the period
#'   used to calculate the mean. This argument is required.
#' @param year_range Optional numeric vector \code{c(start, end)} to filter the
#'   returned data. If \code{NULL}, returns all years in the input data.
#' @param quiet Logical. If \code{FALSE} (default), prints the baseline mean
#'   to the console via a message.
#'
#' @return A focused data frame (tibble) with columns: \code{year}, \code{value},
#'   \code{baseline_mean}, and \code{anomaly}.
#'
#' @examples
#' \dontrun{
#' # Example 1: Using the native R pipe (|>)
#' sst_anom <- EA.data |>
#'   dplyr::filter(variable == "sst.month10", EAR == 0) |>
#'   calc_anomaly(baseline_range = c(1991, 2020))
#'
#' # Example 2: Using base R subset() and assignment
#' physical_data <- subset(EA.data, variable == "t.deep" & EAR == 1)
#' deep_t_anom <- calc_anomaly(data = physical_data,
#'                             baseline_range = c(1995, 2015))
#'
#' # View the clean result
#' head(deep_t_anom)
#' }
#' @export
calc_anomaly <- function(data,
                         baseline_range,
                         year_range = NULL,
                         quiet = FALSE) {

  # 1. Input Validation
  if (missing(baseline_range)) {
    stop("Argument 'baseline_range' is missing. You must specify c(start_year, end_year).")
  }

  if (!all(c("year", "value") %in% names(data))) {
    stop("The data frame must contain 'year' and 'value' columns.")
  }

  # 2. Calculate the baseline mean (climatology)
  climatology_mean <- data |>
    dplyr::filter(year >= baseline_range[1], year <= baseline_range[2]) |>
    dplyr::summarise(m = mean(value, na.rm = TRUE)) |>
    dplyr::pull(m)

  # 3. Handle cases where the baseline period has no data
  if (is.na(climatology_mean)) {
    stop("The calculated baseline mean is NA. Ensure the 'baseline_range' overlaps with your data.")
  }

  if (!quiet) {
    message(paste("Calculated baseline mean:", round(climatology_mean, 3)))
  }

  # 4. Filter for specific output years if requested
  out_df <- data
  if (!is.null(year_range)) {
    out_df <- out_df |>
      dplyr::filter(year >= year_range[1], year <= year_range[2])
  }

  # 5. Construct the focused result
  result <- out_df |>
    dplyr::transmute(
      year = year,
      value = value,
      baseline_mean = climatology_mean,
      anomaly = value - climatology_mean
    ) |>
    dplyr::filter(!is.na(value))

  return(result)
}
