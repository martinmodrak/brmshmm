
make_standata_observations <- function(obs_model, series_data) {
  UseMethod("make_standata_observation")
}

stanvars_observations <- function(obs_model, standata) {
  UseMethod("stanvars_observations")
}

observations_prob_stancode <- function(
    obs_model, assignment_target, time_expr, serie_expr, hidden_state_expr) {
  UseMethod("observations_prob_stancode")
}

observations_prob_vector_stancode <- function(
    obs_model, assignment_target, time_expr, serie_expr) {
  UseMethod("observations_prob_vector_stancode")
}


is_observed_stancode <- function(obs_model, assignment_target, time_expr, series_expr) {
  UseMethod("is_observed_stancode")
}

validate_observations <- function(obs_model, serie_data) {
  UseMethod("validate_observations")
}

observations_prob_vector_stancode.default <- function(
    obs_model, assignment_target, time_expr, serie_expr) {
  paste0("
    {
      vector[N_states_hidden] obs_probs_tmp;
      for(int hidden_state_i in 1:N_states_hidden) {
        ", observations_prob_stancode(obs_model,
                                      assignment_target = "obs_probs_tmp[hidden_state_i]",
                                      time_expr = time_expr,
                                      serie_expr = serie_expr,
                                      hidden_state_expr = "hidden_state_i"),"
      }
      ", assignment_target, " = obs_probs_tmp;
    }")
}




