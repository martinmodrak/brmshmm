#'
#' @return Array with dimensions time, serie, sample
posterior_epred_rect <- function(fit, nsamples = NULL, newdata = NULL, method = posterior_epred_rect_simulate,
                                 cl = NULL, cores = parallel::detectCores()) {
  validate_brmshmmfit(fit)

  brms:::contains_samples(fit$brmsfit)

  if(is.null(newdata)) {
    pred_rawdata <- fit$data
  } else {
    pred_rawdata <- newdata
  }

  data_hmm <- prepare_data_hmm(pred_rawdata)

  epred_mu <- brms::posterior_epred(fit$brmsfit, newdata = data_hmm$brmsdata, nsamples = nsamples)
  if(any(is.na(epred_mu))) {
    stop("NAs in epred_mu")
  }
  transition_matrices <- compute_all_transition_matrices(data_hmm, epred_mu, cl = cl, cores = cores)

  if(is.null(nsamples)) {
    nsamples <- dim(epred_mu)[1]
  }

  max_times <- pred_rawdata$series_data %>% group_by(.serie) %>% summarise(max_time = max(.time)) %>%
    arrange(.serie) %>% pull(max_time)

  method(
    nsamples = nsamples,
    initial_states = pred_rawdata$initial_states,
    max_times = max_times,
    transition_matrices = transition_matrices,
    predictor_sets_rect = data_hmm$standata$predictor_sets_rect)
}

#' @return Array with dimensions time, serie, sample
posterior_epred_rect_simulate <- function(nsamples, max_times, initial_states, transition_matrices, predictor_sets_rect) {

  if(length(max_times) != length(initial_states)) {
    stop("Incompatible lengths")
  }
  N_series <- length(initial_states)
  result_rect <- array(NA_integer_, c(max(max_times), N_series, nsamples))
  for(s in 1:N_series) {

    result_rect[1, s, ] <- as.integer(initial_states[s])


    for(i in 1:nsamples) {
      state <- initial_states[s]
      for(t in 2:max_times[s]) {
        ps <- predictor_sets_rect[s, t]
        if(ps == 0) {
          stop(paste0("Element with no predictor at serie ",s,", time ", t))
        }
        #cat(state, ":", ps, ",", i, ":", transition_matrices[state, , ps, i], "\n")
        transition_probs <- transition_matrices[state, , ps, i]
        state <- sample.int(length(transition_probs), size = 1, prob = transition_probs)
        if(is.na(state)) {
          stop("NA state")
        }
        result_rect[t, s, i] <- state
      }
    }
  }

  result_rect

}

#' @return array with dimensions n_states, n_time, n_samples, n_series
posterior_epred_state_prob <- function(nsamples, max_times, initial_states, transition_matrices, predictor_sets_rect) {
  if(length(max_times) != length(initial_states)) {
    stop("Incompatible lengths")
  }
  n_series <- length(initial_states)
  n_time = max(max_times)
  n_states <- dim(transition_matrices)[1]
  n_predictor_sets <- dim(transition_matrices)[3]
  if(nsamples != dim(transition_matrices)[4]) {
    stop("nsamples doesn't match")
  }

  res_internal <- posterior_epred_state_prob_internal(n_samples = nsamples,
                                    max_times = max_times,
                                    initial_states = initial_states,
                                    n_states = n_states,
                                    n_time = n_time,
                                    n_predictor_sets = n_predictor_sets,
                                    transition_matrices = transition_matrices,
                                    predictor_sets_rect = predictor_sets_rect)

  dim(res_internal) <- c(n_states, n_time, nsamples, n_series);

  res_internal

}

posterior_rect_to_long <- function(fit, posterior_rect, newdata = NULL) {
  validate_brmshmmfit(fit)
  brms:::contains_samples(fit$brmsfit)

  nsamples <- dim(posterior_rect)[3]
  if(is.null(newdata)) {
    pred_rawdata <- fit$data
  } else {
    pred_rawdata <- newdata
  }

  result <- array(NA_integer_, c(nsamples, nrow(pred_rawdata$series_data)))

  for(o in 1:nrow(pred_rawdata$series_data)) {
    result_for_row <- posterior_rect[pred_rawdata$series_data$.time[o], as.integer(pred_rawdata$series_data$.serie[o]), ]
    if(any(is.na(result_for_row))) {
      stop(paste0("NA for row ", o))
    }
    result[, o] <- result_for_row
  }

  if(any(is.na(result))) {
    stop("NA in translation from rect")
  }
  result

}

posterior_epred.brmshmmfit <- function(fit, nsamples = NULL) {
  validate_brmshmmfit(fit)
  brms:::contains_samples(fit$brmsfit)

  pred_rawdata <- fit$data

  result_rect <- posterior_epred_rect(fit, nsamples = nsamples)

  posterior_rect_to_long(fit, result_rect)
}

hidden_to_corresponding_observed <- function(data_processed, x) {
  if(!is.integer(x) || any(x < 1 | x > data_processed$standata$N_states_hidden)) {
    stop("Invalid state data")
  }
  res_corresponding <- data_processed$standata$corresponding_observation[x]
  dim(res_corresponding) <- dim(x)

  res_corresponding

}

posterior_epred_to_predicted <- function(fit, epred) {
  validate_brmshmmfit(fit)

  n_dim <- length(dim(epred))
  if(!(n_dim %in% c(2,3))) {
    stop("Only works for two or three dimensions of epred")
  }

  nsamples = dim(epred)[1]

  # TODO: match to the same samples as epred has.
  observation_probs_samples <- rstan::extract(fit$brmsfit$fit, "observation_probs")$observation_probs

  N_hidden_states = dim(observation_probs_samples)[2]
  N_observed_states = dim(observation_probs_samples)[3]

  result <- array(NA_integer_, dim(epred))
  for(i in 1:nsamples) {
    observation_probs <- observation_probs_samples[i, ,]
    # Array with one less dimension than epred
    one_sample <- array(NA_integer_, dim(epred)[2:n_dim])

    for(h in 1:N_hidden_states) {
      # Equivalent to using , just with the correct number of missing arguments
      if(n_dim == 2) {
        indices <- !(is.na(epred[i,])) & epred[i,] == h
      } else {
        indices <- !(is.na(epred[i,,])) & epred[i,,] == h
      }
      one_sample[indices] <- sample.int(N_observed_states, size = sum(indices), replace = TRUE,
                                        prob = observation_probs_samples[i, h, ])
    }
    if(n_dim == 2) {
      result[i,] <- one_sample
    } else {
      result[i,,] <- one_sample
    }
  }

  if(!identical(is.na(epred), is.na(result))) {
    stop("Added NAs when computing observed")
  }

  result

}

posterior_predict.brmshmmfit <- function(fit, nsamples = NULL) {
  validate_brmshmmfit(fit)
  epred <- posterior_epred.brmshmmfit(fit, nsamples = nsamples)

  if(any(is.na(epred))) {
    stop("NA in epred")
  }

  posterior_epred_to_predicted(fit, epred)
}

posterior_long_to_df <- function(data, prediction) {
  validate_brmshmmdata(data)
  nsamples <- dim(prediction)[1]

  samples_df <- as.data.frame(t(prediction))
  names(samples_df) <- paste0("__S", 1:nsamples)

  data$series_data %>% cbind(samples_df) %>%
    pivot_longer(matches("^__S[0-9]*$"), names_prefix = "__S", names_to = ".sample", values_to = ".predicted") %>%
    mutate(.sample = as.integer(.sample))

}

#'
#' @return An array of dimensions `N_states_hidden` x `N_states_hidden` x `N_predictor_sets` x `N_samples`
compute_all_transition_matrices <- function(data_hmm, epred_mu, cl = NULL, cores = parallel::detectCores(), cache = "auto") {
  res <- NULL
  if(cache == "auto") {
    all_params <- list(data_hmm, epred_mu)

    cache_dir <- here::here("local_temp_data", "hmm", "epred_cache")
    if(!dir.exists(cache_dir)) {
      dir.create(cache_dir)
    }
    cache_file <- paste0(cache_dir,"/transition_matrices_", digest::digest(all_params), ".rds")
    if(file.exists(cache_file)) {
      res <- readRDS(cache_file)
    }
  }

  if(is.null(res)) {
    start_time = proc.time()
    nsamples <- dim(epred_mu)[1]
    epred_mu_exp <- exp(epred_mu)
    standata <- data_hmm$standata

    compute_transition_matrices_ps <- function(ps, epred_mu_exp, standata) {
      res <- array(NA_real_, c(standata$N_states_hidden, standata$N_states_hidden, nsamples))
      for(i in 1:nsamples) {
        rate_matrix <- matrix(0, standata$N_states_hidden, standata$N_states_hidden)

        rate_values <- epred_mu_exp[i, standata$rate_predictors[ps,]]
        for(r in 1:standata$N_rates) {
          rate_matrix[standata$rates_from[r], standata$rates_to[r]] = rate_values[r]
          rate_matrix[standata$rates_from[r],standata$rates_from[r]] =
            rate_matrix[standata$rates_from[r],standata$rates_from[r]] - rate_values[r]
        }

        res[,, i] <-  expm::expm(rate_matrix)
      }
      res
    }

    res <- array(NA_real_, c(standata$N_states_hidden, standata$N_states_hidden, standata$N_predictor_sets, nsamples))
    if(cores == 1) {
      for(ps in 1:standata$N_predictor_sets) {
        res[,,ps,] <- compute_transition_matrices_ps(ps, epred_mu_exp, standata)
      }
    } else {
      #TODO tryCatch + stopCluster
      if(is.null(cl)) {
        cl <- parallel::makePSOCKcluster(min(cores, standata$N_predictor_sets))
        do_stop_cluster <- TRUE
      } else {
        do_stop_cluster <- FALSE
      }
      res_list <- parallel::parLapply(cl, 1:standata$N_predictor_sets, fun = compute_transition_matrices_ps,
                                        epred_mu_exp = epred_mu_exp, standata = standata)

      if(do_stop_cluster) {
        parallel::stopCluster(cl)
      }
      for(ps in 1:standata$N_predictor_sets) {
        res[,,ps,] <- res_list[[ps]]
      }

    }

    time_taken <- proc.time() - start_time
    message(paste0("Transition matrices computed in ", time_taken["elapsed"], "s."))

    if(cache == "auto") {
      saveRDS(res, cache_file)
    }
  }


  res
}
