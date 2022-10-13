validate_transitions.rate_transitions <- function(trans_model, hidden_state_data) {
  t <- trans_model
  if(is.null(t$rate_data$.rate_id)) {
    t$rate_data <- t$rate_data %>% mutate(.rate_id = factor(1:n()))
  } else {
    t$rate_data$.rate_id <- validate_id(t$rate_data$.rate_id, "rate_data$.rate_id", force_no_gaps = TRUE)
  }

  if(!is.null(t$rate_data$.transition_id)) {
    stop(".transition_id cannot be provided")
  }

  t$rate_data$.transition_id <- t$rate_data$.rate_id

  #TODO: test for duplicate (rate_from, rate_to)
  #TODO: test for states with no rates

  t$rate_data$.from <- validate_id(t$rate_data$.from, "rate_data$.from",
                                   reference_levels = levels(hidden_state_data$id),
                                   reference_levels_for_message = "hidden_state_data$id")

  t$rate_data$.to <- validate_id(t$rate_data$.to, "rate_data$.to",
                                 reference_levels = levels(hidden_state_data$id),
                                 reference_levels_for_message = "hidden_state_data$id")

  t
}


rate_transitions_functions_code <- r"(
  // Compute a single transition matrix
  // The first matrix index is 'from', second is 'to'
  matrix compute_transition_matrix(
    int N_states, int N_rates, int[] rates_from, int[] rates_to, vector rates
  ) {
    matrix[N_states, N_states] rate_matrix = rep_matrix(0, N_states, N_states);
    vector[N_states] outgoing_rate_sum = rep_vector(0, N_states);
    for(r in 1:N_rates) {
      rate_matrix[rates_from[r], rates_to[r]] = rates[r];
      outgoing_rate_sum[rates_from[r]] += rates[r];
    }
    for(s in 1:N_states) {
      rate_matrix[s,s] = -outgoing_rate_sum[s];
    }
    return matrix_exp(rate_matrix);
  }
)"

rate_transitions_stanvars_data <- function(data) {
  brms::stanvar(x = standata$N_rates, name = "N_rates", scode = "  int<lower=1> N_rates;", block = "data") +
  brms::stanvar(x = standata$rates_from, name = "rates_from", scode = "  int<lower=1, upper=N_states_hidden> rates_from[N_rates];", block = "data") +
  brms::stanvar(x = standata$rates_to, name = "rates_to", scode = "  int<lower=1, upper=N_states_hidden> rates_to[N_rates];", block = "data")
}

compute_transition_matrix_stancode.rate_transitions <-  function(trans_model, assignment_target) {
  paste0("
    ", assignment_target, " = compute_transition_matrix(N_states_hidden,  N_predictors, rates_from, rates_to, predictor_values);")

}

make_standata_transitions.rate_transitions <- function(trans_model){
  rate_data <- trans_model$rate_data

  loo::nlist(
        rates_from = as.integer(rate_data$.from),
    rates_to = as.integer(rate_data$.to),
  )
}


