ordered_binary_observations <- function(column) {
  structure(loo::nlist(column), class = "brmshmm_ordered_binary_observations")
}

make_standata_observations.brmshmm_ordered_binary_observations <- function(obs_model, series_data) {
  obs_rect <- rectangularize_series_column(seires_data, obs_model$column)
  list(
    is_observed = is.na(obs_rect),
    observations = tidyr::replace_na(as.integer(obs_rect), 0)
  )
}


stanvars_observations.brmshmm_ordered_binary_observations <- function(obs_model, standata) {
  brms::stanvar(standata$is_observed, scode = "array[N_series, N_time] int<lower=0,upper=1> is_observed;", block = "data") +
    brms::stanvar(standata$observations, scode = "array[N_series, N_time] int<lower=0,upper=1> observations;", block = "data") +
    brms::stanvar(scode = "ordered[N_states_hidden] logit_emit_prob;", block = "parameters") +
    brms::stanvar(scode = "vector<lower=0, upper=1>[N_states_hidden] emit_prob = inv_logit(logit_emit_prob)", block = "tparameters");
}

observations_prob_stancode.brmshmm_ordered_binary_observations <- function(
    obs_model, assignment_target, time_expr, serie_expr, hidden_state_expr) {
  paste0(assignment_target, " = bernoulli_lpmf(observations[", serie_expr, ", ", time_expr, "] | emit_prob[", hidden_state_expr,");");
}


is_observed_stancode.brmshmm_ordered_binary_observations <- function(obs_model, assignment_target, time_expr, series_expr) {
  paste0("
    ", assignment_target, " = is_observed[", series_expr, ", ", time_expr,"];")
}

validate_observations.brmshmm_ordered_binary_observations <- function(obs_model, series_data) {
  stopifnot(is.logical(series_data[[obs_model$column]]))
  obs_model
}