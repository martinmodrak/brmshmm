#' @export
make_standata_transitions <- function(trans_model, hmmdata) {
  UseMethod("make_standata_transitions")
}

#' @export
stanvars_transitions <- function(trans_model, standata) {
  UseMethod("stanvars_transitions")
}

#' @export
family_transitions <- function(trans_model){
  UseMethod("family_transitions")
}

#' @export
compute_transition_matrix_stancode <- function(trans_model, assignment_target) {
  UseMethod("compute_transition_matrix_stancode")
}

#' @export
n_predictors_per_timepoint <- function(trans_model) {
  UseMethod("n_predictors_per_timepoint")
}

#' @export
predictor_data_transitions <- function(trans_model) {
  UseMethod("predictor_data_transitions")
}

#' @export
validate_transitions <- function(trans_model, hidden_state_data) {
  UseMethod("validate_transitions")
}

#' @export
compute_transition_matrix <- function(trans_model, states_data, predictor_values) {
  UseMethod("compute_transition_matrix")
}

#' @export
predictor_data_transitions.default <- function(trans_model) {
  NULL
}

#' @export
n_predictors_per_timepoint.default <- function(trans_model) {
  predictor_data <- predictor_data_transitions(trans_model)
  if(is.null(predictor_data)) {
    0
  } else {
    nrow(predictor_data)
  }
}


#' Compute a single transition matrix
#' The first matrix index is 'from', second is 'to'
compute_transition_matrix.default <- function(trans_model, predictor_values) {
  stop("Transition matrix computation in R not supported for ", class(trans_model))
}
