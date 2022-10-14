validate_initial_states <- function(init_model, states_data, series_data) {
  UseMethod("validate_initial_states")
}

make_standata_initial_states <- function(init_model) {
  UseMethod("make_standata_initial_states")
}

stanvars_initial_states <- function(init_model, standata) {
  UseMethod("stanvars_initial_states")
}

initial_states_probability_stancode <- function(init_model, assignment_target, series_expr) {
  UseMethod("initial_states_probability_stancode")
}

validate_initial_states.default <- function(init_model, states_data, series_data) {
  #No validation by default
  init_model
}


known_initial_states <- function(initial_states) {
  structure(loo::nlist(
    initial_states), class = "known_initial_states")
}

stanvars_initial_states.known_initial_states <- function(init_model, standata) {
  brms::stanvar(x = standata$initial_states, name = "initial_states", scode = "array[N_series]  int<lower=1, upper=N_states_hidden> initial_states;", block = "data")
}

initial_states_probability_stancode.known_initial_states <- function(init_model, assignment_target, series_expr) {
  paste0("
    {
      vector[N_states_hidden] init_probs_tmp = rep_vector(0, N_states_hidden);
      init_probs_tmp[initial_states[s]] = 1;
      ", assignment_target, " = init_probs_tmp;
    }")
}


validate_initial_states.known_initial_states <- function(init_model, states_data, series_data) {
  #TODO make the binding between initial states and series explicit
  if(length(init_model$initial_states) != length(unique(series_data$.serie))) {
    stop("Incorrect number of initial states")
  }

  init_model$initial_states <- validate_id(init_model$initial_states, "init_model$initial_states",
                                           force_no_na = TRUE, reference_levels = states_data$id,
                                           reference_levels_for_message = "states_data$id")

  init_model
}


make_standata_initial_states.known_initial_states <- function(init_model) {
  list(
    initial_states = as.integer(init_model$initial_states)
  )
}
