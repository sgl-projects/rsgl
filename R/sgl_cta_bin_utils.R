bin_limits <- function(input_data, bin_num) {
  min_value <- min(input_data, na.rm = TRUE)
  max_value <- max(input_data, na.rm = TRUE)
  rng <- max_value - min_value
  padding_factor <- .001
  if (rng == 0) {
    padding <- max_value * padding_factor
  } else {
    padding <- rng * padding_factor
  }
  lowest_bin_value <- min_value - padding
  highest_bin_value <- max_value + padding
  bin_limits <- seq(
    lowest_bin_value, highest_bin_value,
    length.out = bin_num + 1
  )
  bin_limits
}

bin_indices <- function(input_data, bin_limits) {
  sapply(
    input_data,
    function(input_value) sum(input_value >= bin_limits)
  )
}

bin_centers <- function(bin_limits) {
  lowers <- bin_limits[-length(bin_limits)]
  lead_limits <- dplyr::lead(bin_limits)
  uppers <- lead_limits[-length(lead_limits)]
  (lowers + uppers) / 2
}

bin_values <- function(input_data, bin_num, scale) {
  if (all(is.na(input_data))) {
    return(input_data)
  }
  scaled_input_data <- apply_scale(scale, input_data)
  bin_limits_vec <- bin_limits(scaled_input_data, bin_num)
  bin_centers_vec <- bin_centers(bin_limits_vec)
  bin_indices_vec <- bin_indices(scaled_input_data, bin_limits_vec)
  bin_values_vec <- bin_centers_vec[bin_indices_vec]
  vals_in_orig_scale <- apply_scale_inverse(scale, bin_values_vec)
  vals_in_orig_scale
}
