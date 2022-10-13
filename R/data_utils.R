rectangularize_series_column <- function(serie_data, column_name, missing_value = NA) {
  N_series <- max(as.integer(serie_data$.serie))
  N_time <- max(serie_data$.time)

  rect <- array(missing_value, c(N_series, N_time))

  for(o in 1:nrow(serie_data)) {
    s <- as.integer(serie_data$.serie[o])
    t <- serie_data$.time[o]
    if(!is.na(serie_data[[column_name]][o])) {
      rect[s, t] = serie_data[[column_name]][o];
    }
  }

  rect
}
