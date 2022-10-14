rectangularize_series_column <- function(series_data, column_name, missing_value = NA) {
  N_series <- max(as.integer(series_data$.serie))
  N_time <- max(series_data$.time)

  rect <- array(missing_value, c(N_series, N_time))

  for(o in 1:nrow(series_data)) {
    s <- as.integer(series_data$.serie[o])
    t <- series_data$.time[o]
    if(!is.na(series_data[[column_name]][o])) {
      rect[s, t] = series_data[[column_name]][o];
    }
  }

  rect
}
