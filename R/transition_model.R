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

validate_transitions <- function(trans_model) {
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

categorical_transitions <- function(hidden_state_data, transitions_data) {
  structure(loo::nlist(hidden_state_data,transitions_data),
            "brmshmm_categorical_transitions")
}

family_transitions.brmshmm_categorical_transitions <- function(trans_model) {
  custom_family(
    "hmm_categorical", dpars = c("mu"),
    links = link,
    type = "real",
    loop = FALSE)
}

stanvars_transitions.brmshmm_categorical_transitions <- function(transmodel, standata) {
  brms::stanvar(scode = categorical_transitions_functions_code, block = "functions")
}

categorical_transitions_functions_code <- r"(
  real hmm_categorical_lpdf(vector y, real mu) {
    return 0;
  }
)"

