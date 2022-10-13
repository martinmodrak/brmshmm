categorical_transitions <- function(trans_data, hidden_state_data) {
  raw <- structure(list(data = trans_data), class = "brmshmm_categorical_transitions")
  validate_transitions.brmshmm_categorical_transitions(raw, hidden_state_data)
}

validate_transitions.brmshmm_categorical_transitions <- function(trans_model, hidden_state_data) {
  t <- trans_model
  if(length(setdiff(c(".from", ".to", ".reference"), names(t$data)) > 0)) {
    stop("Missing required columns")
  }
  if(!is.logical(t$data$.reference)) {
    stop(".reference must be a logical column")
  }
  if(is.null(t$data$.transition_id)) {
    t$data <- t$data %>% mutate(.transition_id = factor(1:n()))
  } else {
    t$data$.transition_id <- validate_id(t$data$.transition_id, "categorical_transitions::data$.transition_id", force_no_gaps = TRUE)
  }


  #TODO: test for duplicate (from, to)
  #TODO: test for states with no transitions

  t$data$.from <- validate_id(t$data$.from, "data$.from",
                                   reference_levels = levels(hidden_state_data$id),
                                   reference_levels_for_message = "hidden_state_data$id")

  t$data$.to <- validate_id(t$data$.to, "data$.to",
                                 reference_levels = levels(hidden_state_data$id),
                                 reference_levels_for_message = "hidden_state_data$id")

  n_ref_transitions <-
    dplyr::summarise(
      dplyr::group_by(t$data, .from),
      n_ref = sum(.reference)
    )

  bad_refs <- dplyr::filter(n_ref_transitions, n_ref != 1)
  if(nrow(bad_refs) > 0) {
    print(bad_refs)
    stop("Some states do not have a single unique reference transition")
  }

  t
}

make_standata_transitions.brmshmm_categorical_transitions <- function(trans_model, hmmdata) {
  list(
    N_trans = nrow(trans_model$data),
    trans_from = trans_model$data$.from,
    trans_to = trans_model$data$.to,
    ref =
  )
}


categorical_transitions_stanvars_data <- function(data) {
  brms::stanvar(x = standata$N_trans, name = "N_trans", scode = "  int<lower=1> N_trans;", block = "data") +
  brms::stanvar(x = standata$trans_from, name = "trans_from", scode = "  int<lower=1, upper=N_states_hidden> trans_from[N_trans];", block = "data") +
  brms::stanvar(x = standata$trans_to, name = "trans_to", scode = "  int<lower=1, upper=N_states_hidden> trans_to[N_trans];", block = "data")
}


family_transitions.brmshmm_categorical_transitions <- function(trans_model) {
  custom_family(
    "hmm_categorical", dpars = c("mu"),
    links = link,
    type = "real",
    loop = FALSE)
}

stanvars_transitions.brmshmm_categorical_transitions <- function(transmodel, standata) {
  brms::stanvar(scode = categorical_transitions_functions_code, block = "functions") +
    brms::stanvar(scode = categorical_transitions_tdata_def, block = "tdata", position = "start") +
    brms::stanvar(scode = categorical_transitions_tdata_code, block = "tdata", position = "end")
}

predictor_data_transitions.brmshmm_categorical_transitions <- function(transmodel) {
  transmodel$data
}

compute_transition_matrix_stancode.categorical_transitions <- function(trans_model, assignment_target) {
  paste0("
    {
      // The first matrix index is 'from', second is 'to'
      matrix[N_states_hidden, N_states_hidden] tmatrix_tmp = rep_matrix(0, N_states_hidden, N_states_hidden);
      for(s in 1:N_states_hidden) {
        row_vector[N_nonref_trans[s]] + 1] trans_mu;
        array[[N_nonref_trans[s]] + 1] int trans_idx;
        trans_mu[1] = 0;
        trans_idx[1] = reference_trans[s];
        for(s2idx in 1:N_nonref_trans[s]) {
          trans_mu[s2idx + 1] = predictor_values[trans_start[s] + s2idx - 1];
          trans_idx[s2idx + 1] = to[trans_start[s] + s2idx - 1];
        }
        tmatrix_tmp[s, trans_idx] = softmax(trans_mu);
      }
      ", assignment_target, " = tmatrix_tmp;
    }")

}

categorical_transitions_functions_code <- r"(
  real hmm_categorical_lpdf(vector y, real mu) {
    return 0;
  }
)"

categorical_transitions_tdata_def <- "

  array[N_states_hidden] int<lower=1, upper=N_trans> trans_start;
  array[N_states_hidden] int<lower=1, upper=N_states_hidden> N_nonref_trans = rep_array(0, N_states_hidden);
"

categorical_transitions_tdata_code <- "

  //TODO: check that from is in order
  trans_start[1] = 1;
  {
    int prev_trans_from = 1;
    for(i in 1:N_trans) {
      if(trans_from[i] != prev_trans_from) {
        trans_start[trans_from[i]] = i;
        prev_trans_from = trans_from[i];
      }
      N_nonref_trans[trans_from[i]] = N_nonref_trans[trans_from[i]] + 1;
    }
  }
"