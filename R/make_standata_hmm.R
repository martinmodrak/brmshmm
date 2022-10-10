make_data_hmm <- function(brmshmmdata) {
  d <- validate_brmshmmdata(brmshmmdata)

  formula_processed <- d$formula
  serie_data <- d$serie_data
  hidden_state_data <- d$hidden_state_data
  observed_state_data <- d$observed_state_data

  N_states_hidden <- length(unique(c(rate_data$.from, rate_data$.to)))


  N_series <- max(as.integer(serie_data$.serie))

  N_time <- max(serie_data$.time)

  all_vars_needed <- all.vars(as.formula(formula_processed))

  serie_data_vars <- intersect(all_vars_needed, names(serie_data))
  if(length(serie_data_vars) > 0) {
    serie_data_distinct <- serie_data %>% select(one_of(serie_data_vars)) %>%
      dplyr::distinct() %>%
      mutate(.predictor_set = 1:n())

    N_predictor_sets <- nrow(serie_data_distinct)

    serie_data_raw <- serie_data
    serie_data <- serie_data %>% dplyr::left_join(serie_data_distinct, by = serie_data_vars)
    if(any(is.na(serie_data$.predictor_set)) || nrow(serie_data_raw) != nrow(serie_data)) {
      stop("Failed join")
    }
  } else {
    N_predictor_sets <- 1
    serie_data <- serie_data %>% mutate(.predictor_set = 1)
    serie_data_distinct <- data.frame(.predictor_set = 1)
  }

  predictor_data <- predictor_data_transitions(trans_model)

  brmsdata_all <- crossing(serie_data_distinct, predictor_data)

  #TODO avoid repetitions (would only apply if some rates are identical)
  # brmsdata_distinct <- brmsdata_all %>% select(one_of(all_vars_needed)) %>%
  #   dplyr::distinct() %>%
  #   mutate(.predictor_id = 1:n())
  #
  # brmsdata <- brmsdata_all %>% left_join(
  brmsdata <- brmsdata_all %>%
    arrange(.predictor_set, .transition_id) %>%
    mutate(.brms_id = 1:n(), .dummy = seq(0, 1, length.out = n()))

  obs_states <- as.integer(serie_data$.observed)
  obs_states[is.na(serie_data$.observed)] <- 0

  predictors <- array(NA_integer_, c(N_predictor_sets, N_rates))
  for(i in 1:nrow(brmsdata)) {
    predictors[brmsdata$.predictor_set[i], brmsdata$.transition_id[i]] <- brmsdata$.brms_id[i]
  }

  predictor_sets_rect <- array(0, c(N_series, N_time))
  serie_max_time <- array(0, N_series)

  for(o in 1:nrow(serie_data)) {
    s <- as.integer(serie_data$.serie[o])
    t <- serie_data$.time[o]
    serie_max_time[s] = max(serie_max_time[s], t);
    predictor_sets_rect[s, t] = serie_data$.predictor_set[o];
  }

  for(s in 1:N_series) {
    if(serie_max_time[s] <= 1) {
      stop("At least two time points are needed for each serie")
    }
    for(t in 1:serie_max_time[s]) {
      if(predictor_sets_rect[s, t] == 0) {
        stop(paste0("Serie ", s, ", is missing predictors for time ", t, ".\n",
                    "Predictors are needed for all times, observations are optional"))
      }
    }
  }

  init_standata <- make_standata_initial_states(d$init_model)
  transitions_standata <- make_standata_transitions(d$trans_model)
  observations_standata <- make_standata_observations(d$obs_model, d$serie_data)

  base_standata <- loo::nlist(
    N_states_hidden,
    N_predictors <- n_predictors_per_timepoint(d$trans_model),

    N_series,
    N_time,
    N_predictor_sets,

    serie_max_time,
    predictor_sets_rect,
    predictors#,
    #optimize_possible = d$optimize_possible
  )

  standata <- c(base_standata, init_standata, transitions_standata, observations_standata)
  if(length(unique(names(standata))) != length(standata)) {
    stop("Duplicate names")
  }

  loo::nlist(standata, brmsdata)
}

make_standata_hmm <- function(brmshmmdata) {
  d <- validate_brmshmmdata(brmshmmdata)

  data <- make_data_hmm(d)
  c(brms::make_standata(d$formula, data = data$brmsdata), data$standata)
}
