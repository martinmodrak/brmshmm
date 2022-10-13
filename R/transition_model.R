# Compute a single transition matrix
# The first matrix index is 'from', second is 'to'
make_standata_transitions <- function(trans_model, hmmdata) {
  UseMethod("make_standata_transitions")
}

stanvars_transitions <- function(trans_model, standata) {
  UseMethod("stanvars_transitions")
}

family_transitions <- function(trans_model){
  UseMethod("family_transitions")
}

compute_transition_matrix_stancode <- function(trans_model, assignment_target) {
  UseMethod("compute_transition_matrix_stancode")
}

n_predictors_per_timepoint <- function(trans_model) {
  UseMethod("n_predictors_per_timepoint")
}

predictor_data_transitions <- function(trans_model) {
  UseMethod("predictor_data_transitions")
}

validate_transitions <- function(trans_model, hidden_state_data) {
  UseMethod("validate_transitions")
}

predictor_data_transitions.default <- function(trans_model) {
  NULL
}

n_predictors_per_timepoint.default <- function(trans_model) {
  predictor_data <- predictor_data_transitions(trans_model)
  if(is.null(predictor_data)) {
    0
  } else {
    nrow(predictor_data_transitions)
  }
}


