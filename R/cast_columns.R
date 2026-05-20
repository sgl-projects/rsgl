aes_has_dt_and_ts <- function(aes, layers, dfs) {
  dt_found <- 0
  ts_found <- 0
  found_check <- function(layer, df) {
    if (aes %in% names(layer$aes_mappings)) {
      if (is_date_mapping(layer, df, aes)) {
        dt_found <<- 1
      } else if (is_timestamp_mapping(layer, df, aes)) {
        ts_found <<- 1
      }
    }
  }
  Map(found_check, layers, dfs)
  if (dt_found == 1 && ts_found == 1) {
    TRUE
  } else {
    FALSE
  }
}

aes_with_dt_and_ts <- function(rgs, dfs) {
  all_aes <- all_aesthetics(rgs)
  purrr::keep(
    all_aes,
    \(aes) aes_has_dt_and_ts(aes, rgs$layers, dfs)
  )
}

cast_for_aes <- function(aes, layers, dfs) {
  cast_df <- function(layer, df) {
    aes_mappings <- layer$aes_mappings
    if (!(aes %in% names(aes_mappings))) {
      df
    } else {
      col_name <- aes_mappings[[aes]]$column
      df |>
        dplyr::mutate(!!col_name := lubridate::as_datetime(.data[[col_name]]))
    }
  }

  purrr::map2(layers, dfs, cast_df)
}

cast_columns <- function(rgs, dfs) {
  aes_to_cast <- aes_with_dt_and_ts(rgs, dfs)
  for (aes in aes_to_cast) {
    dfs <- cast_for_aes(aes, rgs$layers, dfs)
  }
  dfs
}
