make_brms_formula_hmm <- function(formula) {
  if(!is.null(brms:::lhs(formula))) {
    stop("Formula needs to be one-sided")
  }

  brms::brmsformula(update.formula(formula, .dummy ~ .))
}

make_stanvars_hmm <- function(brmshmmdata) {
    d <- validate_brmshmmdata(brmshmmdata)
    prep <- prepare_data_hmm(d)

    all_stanvars <-
      stanvars_base_hmm_api(d, prep$standata) +
      stanvars_initial_states(d$init_model, prep$standata) +
      stanvars_transitions(d$trans_model, prep$standata) +
      stanvars_observations(d$obs_model, prep$standata) +
      stanvars_base_hmm_code(d, prep$standata)

    all_stanvars
}

make_stancode_hmm <- function(brmshmmdata) {
  d <- validate_brmshmmdata(brmshmmdata)

  all_stanvars <- make_stanvars_hmm(d)

  brms::make_stancode(d$formula, family = family_transitions(d$trans_model),
                      data = data$brmsdata,
                      stanvars = all_stanvars, prior = d$prior)
}

# posterior_epred_rate_hmm <- function(prep) {
#   brms:::posterior_epred_gaussian(prep)
# }


stanvars_base_hmm_api <- function(brmshmmdata, standata) {
  base_hmm_stanvars_data(standata) +
    brms::stanvar(scode = base_hmm_tdata_code, block = "tdata", position = "end") +
    brms::stanvar(scode = hmm_log_lik_definitions, block = "likelihood", position = "start")
}

stanvars_base_hmm_code <- function(brmshmmdata, standata) {
  log_lik_code <- hmm_log_lik_code(
    brmshmmdata$init_model, brmshmmdata$trans_model, brmshmmdata$obs_model)

  brms::stanvar(scode = log_lik_code, block = "likelihood", position = "end")
}


base_hmm_stanvars_data  <- function(standata) {
  brms::stanvar(x = standata$N_states_hidden, name = "N_states_hidden", scode = "  // HMM data\n  int<lower=1> N_states_hidden;", block = "data") +

  brms::stanvar(x = standata$N_series, name = "N_series", scode = "  // Observations\n  int<lower=1> N_series;", block = "data") +
  brms::stanvar(x = standata$N_time, name = "N_time", scode = "  int<lower=1> N_time;", block = "data") +
  brms::stanvar(x = standata$N_predictors, name = "N_predictors", scode = "  int<lower=1> N_predictors;", block = "data") +
  brms::stanvar(x = standata$N_predictor_sets, name = "N_predictor_sets", scode = "  int<lower=1> N_predictor_sets;", block = "data") +

  brms::stanvar(x = standata$serie_max_time, name = "serie_max_time", scode = "array[N_series] int<lower=1, upper=N_time> serie_max_time;", block = "data") +

  brms::stanvar(x = standata$predictor_sets_rect, name = "predictor_sets_rect", scode = "array[N_series, N_time] int<lower=0, upper=N_predictor_sets> predictor_sets_rect;", block = "data") +
  brms::stanvar(x = standata$predictors, name = "predictors", scode = "array[N_predictor_sets, N_predictors]  int<lower=1, upper=N> predictors;", block = "data")
  #TODO once possible is reintroduced
  #brms::stanvar(x = standata$optimize_possible, name = "optimize_possible", scode = "  int<lower=0, upper=2> optimize_possible;", block = "data")
}

base_hmm_tdata_code <- r"(
  //Check data validity
  for(p in 1:N_series) {
    for(t in 1:(serie_max_time[p] - 1)) {
      if(predictor_sets_rect[p, t] == 0) {
        reject("serie ", p, " has missing predictors for time ", t);
      }
    }
  }
)"

hmm_log_lik_definitions <- "
  array[N_predictor_sets] matrix[N_states_hidden, N_states_hidden] transition_matrices_t;
"

hmm_log_lik_code <- function(init_model, trans_model, obs_model) {
paste0(
"

  //Compute all transition matrices
  for(ps in 1:N_predictor_sets) {
    vector[N_predictors] predictor_values = mu[predictors[ps]];
    matrix[N_states_hidden, N_states_hidden] tm;",
    compute_transition_matrix_stancode(trans_model, "tm"),
"
    transition_matrices_t[ps] =
      transpose(tm);
  }



  for(s in 1:N_series) {
    matrix[N_states_hidden, serie_max_time[s]] alpha = rep_matrix(0, N_states_hidden, serie_max_time[s]);
    vector[serie_max_time[s]] alpha_log_norms;
    vector[N_states_hidden] initial_probs;
    int is_observed_1;
    ",  initial_states_probability_stancode(init_model, "initial_probs", series_expr = "s"), "
    ",  is_observed_stancode(obs_model, "is_observed_1", time_expr = "1", series_expr = "s"), "
    if(is_observed_1) {
      vector[N_states_hidden] initial_obs_probs;
      ",  observations_prob_vector_stancode(obs_model, "initial_obs_probs", time_expr = "1", serie_expr = "s"), "
      alpha[, 1] = initial_probs .* initial_obs_probs;
    } else {
      alpha[, 1] = initial_probs;
    }

    // Normalize the first time point
    {
      real norm = max(alpha[, 1]);
      alpha[, 1] /= norm;
      alpha_log_norms[1] = log(norm);
    }
    for(t in 2:serie_max_time[s]) {
      real col_norm;
      int ps = predictor_sets_rect[s, t - 1];
      int is_observed_current;
      ",  is_observed_stancode(obs_model, "is_observed_current", time_expr = "t", series_expr = "s"), "
      if(is_observed_current) {
        //Oberved something.
        vector[N_states_hidden] obs_probs;
        vector[N_states_hidden] transition_probs;

        ",  observations_prob_vector_stancode(obs_model, "obs_probs", time_expr = "t", serie_expr = "s"), "
        transition_probs =  transition_matrices_t[ps] * alpha[, t - 1];
        alpha[, t] = obs_probs .* transition_probs;

        //TODO return optimize_possible
        /*} else {
          //Optimization: only update states that are possible
          int obs_prev = obs_states_rect[s,t - 1];
          int N_possible_prev = obs_prev == 0 ? N_states_hidden : N_possible_states[obs_prev];
          if(optimize_possible == 1 || N_possible_prev == N_states_hidden) {
            for(p_state_id in 1:N_possible) {
               int p_state = possible_states[obs, p_state_id];
               alpha[p_state, t] = observation_probs[p_state, obs] * dot_product(alpha[, t-1], transition_matrices[ps, , p_state]);
            }
          } else {
            for(p_state_id in 1:N_possible) {
               int p_state = possible_states[obs, p_state_id];
               vector[N_possible_prev] transition_probs;
               for(p_state_id_prev in 1:N_possible_prev) {
                 int p_state_prev = possible_states[obs_prev, p_state_id_prev];
                 transition_probs[p_state_id_prev] = alpha[ p_state_prev, t - 1] * transition_matrices[ps, p_state_prev , p_state];
               }
               alpha[p_state, t] = observation_probs[p_state, obs] * sum(transition_probs);
            }
          }
        }*/
      } else {
        //No observation
        vector[N_states_hidden] transition_probs = (transition_matrices_t[ps] * alpha[, t - 1]);
        alpha[, t] = transition_probs;
      }

      // Renormalize
      col_norm = max(alpha[, t]);
      alpha[, t] /= col_norm;
      alpha_log_norms[t] = log(col_norm) + alpha_log_norms[t - 1];
    }
    target += log(sum(alpha[,serie_max_time[s]])) + alpha_log_norms[serie_max_time[s]];
  }
"
)
}

