noisy_state_2_state_observations <- function(sensitivity_low_bound = 0.5) {
  structure(loo::nlist(sensitivity_low_bound), class = "noisy_state_2_state")
}

validate_observations.noisy_state_2_state <- function(obs_model, serie_data) {
   if(is.null(d$observed_state_data)) {

    d$hidden_state_data$corresponding_obs <-
      validate_id(d$hidden_state_data$corresponding_obs, "corresponding_obs (without observed data)", force_no_gaps = TRUE)


    d$observed_state_data <- data.frame(id = levels(d$hidden_state_data$corresponding_obs), is_noisy = FALSE)
  } else {
    d$hidden_state_data$corresponding_obs <-
      validate_id(d$hidden_state_data$corresponding_obs, "corresponding_obs", force_no_gaps = TRUE,
                  reference_levels = levels(d$observed_state_data$id), reference_levels_for_message = "observed_state_data$id")
  }

  d$observed_state_data$id <- validate_id(d$observed_state_data$id, force_unique = TRUE, force_no_gaps = TRUE)
  d$observed_state_data <- d$observed_state_data %>% arrange(as.integer(id))

  other_obs_cols <- names(d$observed_state_data)[grepl("^other_obs_", names(d$observed_state_data))]
  for(oc in other_obs_cols) {
    d$observed_state_data[[oc]] <- validate_id(d$observed_state_data[[oc]], oc,
                                               force_no_na = FALSE,
                                               reference_levels = levels(d$observed_state_data$id),
                                               reference_levels_for_message = "observed_state_data$id")
  }

  d$initial_states <- validate_id(d$initial_states, "initial_states", force_unique = FALSE,
                                  force_no_gaps = FALSE, reference_levels = levels(d$hidden_state_data$id),
                                  reference_levels_for_message = "hidden_state_data$id")


  d$serie_data$.observed <- validate_id(d$serie_data$.observed, "serie_data$.observed",
                                        force_no_na = FALSE,
                                        reference_levels = levels(d$observed_state_data$id),
                                        reference_levels_for_message = "observed_state_data$id")


}

noisy_state_2_state_functions_code <- r"(
  matrix compute_observation_matrix(
    int N_states_hidden, int N_states_observed, int[] corresponding_observation,
    int[] is_state_noisy, int[] noisy_state_id,
    vector sensitivity, int N_other_observations, int[,] noisy_states_other_obs,
    vector[] other_observations_probs
  ) {
    matrix[N_states_hidden, N_states_observed] observation_probs = rep_matrix(0, N_states_hidden, N_states_observed);
    for(s_hidden in 1:N_states_hidden) {
      int corresponding_obs = corresponding_observation[s_hidden];
      if(is_state_noisy[corresponding_obs]) {
        int noisy_id = noisy_state_id[corresponding_obs];
        observation_probs[s_hidden, corresponding_obs] = sensitivity[noisy_id];
        for(other_index in 1:N_other_observations) {
          int s_observed = noisy_states_other_obs[noisy_id, other_index];
          observation_probs[s_hidden, s_observed] =
            (1 - sensitivity[noisy_id]) * other_observations_probs[noisy_id, other_index];
        }
      } else {
        observation_probs[s_hidden, corresponding_obs] = 1;
      }
    }

    return observation_probs;
  }
)"

noisy_state_2_state_stanvars_data <- function(standata) {
  brms::stanvar(x = standata$N_states_observed, name = "N_states_observed", scode = "  int<lower=1> N_states_observed;", block = "data") +
  brms::stanvar(x = standata$corresponding_observation, name = "corresponding_observation", scode = "  int<lower=1, upper=N_states_observed> corresponding_observation[N_states_hidden];", block = "data") +

  brms::stanvar(x = standata$N_noisy_states, name = "N_noisy_states", scode = "  int<lower=0> N_noisy_states;", block = "data") +
  brms::stanvar(x = standata$noisy_states, name = "noisy_states", scode = "  int<lower=1, upper=N_states_observed> noisy_states[N_noisy_states];", block = "data") +
  brms::stanvar(x = standata$N_other_observations, name = "N_other_observations", scode = "  int<lower=1> N_other_observations;", block = "data") +
  brms::stanvar(x = standata$noisy_states_other_obs, name = "noisy_states_other_obs", scode = "  int<lower=1, upper=N_states_observed> noisy_states_other_obs[N_noisy_states, N_other_observations];", block = "data") +

  brms::stanvar(x = standata$sensitivity_low_bound, name = "sensitivity_low_bound", scode = "  real<lower=0, upper=1> sensitivity_low_bound;", block = "data") +

  brms::stanvar(x = standata$obs_states, name = "obs_states_rect", scode = "      //0 for unobserved states
\n  int<lower=0, upper=N_states_observed> obs_states_rect[N_series, N_time];", block = "data")
}

noisy_state_2_state_tdata_code <- r"(
  int<lower=0,upper=1> is_state_noisy[N_states_observed] = rep_array(0, N_states_observed);
  int<lower=0,upper=N_noisy_states> noisy_state_id[N_states_observed] = rep_array(0, N_states_observed);
  int<lower=1,upper=N_states_hidden> N_possible_states[N_states_observed] = rep_array(0, N_states_observed);
  int<lower=0,upper=N_states_hidden> possible_states[N_states_observed, N_states_hidden] = rep_array(0, N_states_observed, N_states_hidden);

  for(s_index in 1:N_noisy_states) {
    is_state_noisy[noisy_states[s_index]] = 1;
    noisy_state_id[noisy_states[s_index]] = s_index;
  }

  //Compute possible hidden states for each observation
  //This let\'s us optimize some computation for the case where not all hidden states are possible
  {
    int is_state_possible[N_states_observed, N_states_hidden] = rep_array(0, N_states_observed, N_states_hidden);
    for(s_hidden in 1:N_states_hidden) {
      int corresponding_obs = corresponding_observation[s_hidden];
      is_state_possible[corresponding_obs, s_hidden] = 1;
      if(is_state_noisy[corresponding_obs]) {
        int noisy_id = noisy_state_id[corresponding_obs];
        for(other_index in 1:N_other_observations) {
          int s_observed = noisy_states_other_obs[noisy_id, other_index];
          is_state_possible[s_observed, s_hidden] = 1;
        }
      }
    }
    for(s_observed in 1:N_states_observed) {
      int next_possible_state = 1;
      N_possible_states[s_observed] = sum(is_state_possible[s_observed, ]);
      for(s_hidden in 1:N_states_hidden) {
        if(is_state_possible[s_observed, s_hidden]) {
          possible_states[s_observed, next_possible_state] = s_hidden;
          next_possible_state += 1;
        }
      }
    }
  }
)"

noisy_state_2_state_tdata_code <- r"(
  vector<lower=sensitivity_low_bound, upper=1>[N_noisy_states] sensitivity;
  simplex[N_other_observations] other_observations_probs[N_noisy_states];
)"

noise_state_2_state_loglik_definitions <- "
  matrix[N_states_hidden, N_states_observed] observation_probs = compute_observation_matrix(
   N_states_hidden, N_states_observed, corresponding_observation,
    is_state_noisy, noisy_state_id, sensitivity, N_other_observations, noisy_states_other_obs,
    other_observations_probs);
"


noisy_state_2_state_genquant_code <- '
    matrix[N_states_hidden, N_states_observed] observation_probs = compute_observation_matrix(
     N_states_hidden, N_states_observed, corresponding_observation,
      is_state_noisy, noisy_state_id, sensitivity, N_other_observations, noisy_states_other_obs,
      other_observations_probs);
'

observations_prob_stancode.noisy_state_2_state <- function(
    obs_model, assignment_target, time_expr, serie_expr, hidden_state_expr) {
  paste0(assignment_target, " = observation_probs[", hidden_state_expr,", obs_states_rect[", serie_expr, ", ", time_expr, "]];")
}

observations_prob_vector_stancode.noisy_state_2_state <- function(
    obs_model, assignment_target, time_expr, serie_expr) {
    paste0(assignment_target, " = observation_probs[, obs_states_rect[", serie_expr, ", ", time_expr, "]];")

}

is_observed_stancode.noisy_state_2_state <- function(obs_model, assignment_target, time_expr, series_expr) {
  paste0("
    ", assignment_target, " = (obs_states_rect[", series_expr, ", ", time_expr,"] != 0);")
}

make_standata_observations.noisy_state_2_state <- function(obs_model, series_data) {
  N_states_observed <- nrow(observed_state_data)


  N_noisy_states <- sum(observed_state_data$is_noisy)
  if(N_noisy_states > 0) {
    noisy_states <- observed_state_data %>% filter(is_noisy) %>% pull(id)

    N_other_observations <- observed_state_data %>% select(starts_with("other_obs_")) %>% length()
    if(N_other_observations < 1) {
      stop("Noisy states require other_obs_XX columns")
    }

    noisy_states_other_obs <- observed_state_data %>%
      filter(is_noisy) %>%
      select(starts_with("other_obs_")) %>%
      mutate_all(as.integer) %>%
      as.matrix()
  } else {
    noisy_states = numeric(0)
    N_other_observations = 1
    noisy_states_other_obs = array(0, c(0, 1))
  }

  obs_states_rect <- as.integer(rectangularize_series_column(serie_data, ".observed", missing_value = 0))

  loo::nlist(
    N_states_observed,
    corresponding_observation = as.integer(hidden_state_data$corresponding_obs),

    N_noisy_states,
    noisy_states = as.integer(noisy_states),
    N_other_observations,
    noisy_states_other_obs,

    sensitivity_low_bound = d$sensitivity_low_bound,
    obs_states_rect,
  )
}
