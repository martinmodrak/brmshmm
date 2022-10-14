
#' @export
categorical_transitions <- function(nonref_trans_data, states_data,
                                    reference_transitions = default_reference_transitions(states_data)) {
  raw <- structure(loo::nlist(reference_transitions, nonref_trans_data), class = "brmshmm_categorical_transitions")
  validate_transitions.brmshmm_categorical_transitions(raw, states_data)
}

#' @export
default_reference_transitions <- function(states_data) {
  data.frame(.from = states_data$id, .to = states_data$id)
}

#' @export
validate_transitions.brmshmm_categorical_transitions <- function(trans_model, states_data) {
  t <- trans_model
  if(length(setdiff(c(".from", ".to"), names(t$nonref_trans_data)) > 0)) {
    stop("Missing required columns in nonref_trans_data")
  }
  if(length(setdiff(c(".from", ".to"), names(t$reference_transitions)) > 0)) {
    stop("Missing required columns in reference_transitions")
  }

  if(is.null(t$nonref_trans_data[[".transition_id"]])) {
    auto_ids <- paste0(t$nonref_trans_data$.from, "__", t$nonref_trans_data$.to)
    t$nonref_trans_data <- t$nonref_trans_data %>% mutate(.transition_id = factor(auto_ids, levels = auto_ids))
  } else {
    t$nonref_trans_data$.transition_id <- validate_id(t$nonref_trans_data$.transition_id, "categorical_transitions::nonref_trans_data$.transition_id", force_no_gaps = TRUE, force_unique = TRUE)
  }



  #TODO: test for duplicate (from, to)
  if(nrow(t$reference_transitions) != nrow(states_data)) {
    stop("There has to be the same number of reference transitions as states")
  }
  t$reference_transitions$.from <- validate_id(t$reference_transitions$.from, "reference_transitions$.from",
                                   reference_levels = levels(states_data$id),
                                   force_unique = TRUE,
                                   force_no_gaps = TRUE,
                                   reference_levels_for_message = "states_data$id")

  t$reference_transitions$.to <- validate_id(t$reference_transitions$.to, "reference_transitions$.to",
                                 reference_levels = levels(states_data$id),
                                 reference_levels_for_message = "states_data$id")

  t$reference_transitions <- dplyr::arrange(t$reference_transitions, as.integer(.from))

  t$nonref_trans_data$.from <- validate_id(t$nonref_trans_data$.from, "nonref_trans_data$.from",
                                   reference_levels = levels(states_data$id),
                                   reference_levels_for_message = "states_data$id")

  t$nonref_trans_data$.to <- validate_id(t$nonref_trans_data$.to, "nonref_trans_data$.to",
                                 reference_levels = levels(states_data$id),
                                 reference_levels_for_message = "states_data$id")


  t
}

#' @export
make_standata_transitions.brmshmm_categorical_transitions <- function(trans_model, states_data) {
  t <- trans_model
  list(
    N_nonref_trans = nrow(t$nonref_trans_data),
    trans_from = as.integer(t$nonref_trans_data$.from),
    trans_to = as.integer(t$nonref_trans_data$.to),
    reference_trans = as.integer(t$reference_transitions$.to)
  )
}


categorical_transitions_stanvars_data <- function(standata) {
  brms::stanvar(x = standata$N_nonref_trans, name = "N_nonref_trans", scode = "  int<lower=1> N_nonref_trans;", block = "data") +
  brms::stanvar(x = standata$trans_from, name = "trans_from", scode = "  array[N_nonref_trans] int<lower=1, upper=N_states_hidden> trans_from;", block = "data") +
  brms::stanvar(x = standata$trans_to, name = "trans_to", scode = "  array[N_nonref_trans] int<lower=1, upper=N_states_hidden> trans_to;", block = "data") +
  brms::stanvar(x = standata$reference_trans, name = "reference_trans", scode = "  array[N_states_hidden] int<lower=1, upper=N_states_hidden> reference_trans;", block = "data")
}


#' @export
family_transitions.brmshmm_categorical_transitions <- function(trans_model) {
  brms::custom_family(
    "hmm_categorical", dpars = c("mu"),
    links = "identity",
    type = "real",
    loop = FALSE)
}

#' @export
stanvars_transitions.brmshmm_categorical_transitions <- function(transmodel, standata) {
  brms::stanvar(scode = categorical_transitions_functions_code, block = "functions") +
    categorical_transitions_stanvars_data(standata) +
    brms::stanvar(scode = categorical_transitions_tdata_def, block = "tdata", position = "start") +
    brms::stanvar(scode = categorical_transitions_tdata_code, block = "tdata", position = "end")
}

#' @export
predictor_data_transitions.brmshmm_categorical_transitions <- function(trans_model) {
  trans_model$nonref_trans_data
}

#' @export
compute_transition_matrix_stancode.brmshmm_categorical_transitions <- function(trans_model, assignment_target) {
  paste0("
    {
      // The first matrix index is 'from', second is 'to'
      matrix[N_states_hidden, N_states_hidden] tmatrix_tmp = rep_matrix(0, N_states_hidden, N_states_hidden);
      for(s in 1:N_states_hidden) {
        vector[N_nonref_trans_state[s] + 1] trans_mu;
        array[N_nonref_trans_state[s] + 1] int trans_idx;
        trans_mu[1] = 0;
        trans_idx[1] = reference_trans[s];
        for(s2idx in 1:N_nonref_trans_state[s]) {
          trans_mu[s2idx + 1] = predictor_values[trans_start[s] + s2idx - 1];
          trans_idx[s2idx + 1] = trans_to[trans_start[s] + s2idx - 1];
        }
        tmatrix_tmp[s, trans_idx] = to_row_vector(softmax(trans_mu));
      }
      ", assignment_target, " = tmatrix_tmp;
    }")

}

categorical_transitions_functions_code <- r"(
  // This is just a dummy function to appease brms, the real action is in the model block
  real hmm_categorical_lpdf(vector y, vector mu) {
    return 0;
  }
)"

categorical_transitions_tdata_def <- "

  array[N_states_hidden] int<lower=1, upper=N_nonref_trans> trans_start;
  array[N_states_hidden] int<lower=0, upper=N_states_hidden> N_nonref_trans_state = rep_array(0, N_states_hidden);
"

categorical_transitions_tdata_code <- "

  //TODO: check that from is in order
  trans_start[1] = 1;
  {
    int prev_trans_from = 1;
    for(i in 1:N_nonref_trans) {
      if(trans_from[i] != prev_trans_from) {
        trans_start[trans_from[i]] = i;
        prev_trans_from = trans_from[i];
      }
      N_nonref_trans_state[trans_from[i]] = N_nonref_trans_state[trans_from[i]] + 1;
    }
  }
"

#' @export
compute_transition_matrix.brmshmm_categorical_transitions <- function(trans_model, states_data, predictor_values) {
  t <- validate_transitions.brmshmm_categorical_transitions(trans_model, states_data)
  if(length(predictor_values) != nrow(t$nonref_trans_data)) {
    stop("There must be the same number of predictors as non-reference transitions")
  }
  N_states <- nrow(states_data)
  predictor_matrix <- matrix(-Inf, nrow = N_states, ncol = N_states)
  for(i in 1:N_states) {
    predictor_matrix[t$reference_transitions$.from[i], t$reference_transitions$.to[i]] <- 0
  }
  for(i in 1:nrow(t$nonref_trans_data)) {
    predictor_matrix[t$nonref_trans_data$.from[i], t$nonref_trans_data$.to[i]] <-
      predictor_values[as.integer(t$nonref_trans_data$.transition_id[i])]
  }
  trans_matrix <- matrix(NA_real_, nrow = N_states, ncol = N_states,
                         dimnames = list(from = states_data$id, to = states_data$id))
  for(i in 1:N_states) {
    trans_matrix[i, ] <- brms:::softmax(predictor_matrix[i,])
  }
  trans_matrix
}
