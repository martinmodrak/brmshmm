#' @param  optimize_possible Let's the algorithm optimize for models where some
#' hidden states are impossible, given an observation. The optimization
#' some overhead and so can be turned of. Takes values 0 - no such optimalization),
#'  1 - avoid redundant rows in matrix to multiply and 2 - also avoid some multiplications.
brmshmmdata <- function(formula,
                        series_data,
                        states_data,
                        init_model,
                        trans_model,
                        obs_model,
                        prior = NULL
                        #optimize_possible = 2
                        ) {

  validate_brmshmmdata(
    structure(loo::nlist(
      formula = make_brms_formula_hmm(formula),
      series_data,
      states_data,
      init_model,
      trans_model,
      obs_model,
      prior
      #optimize_possible
      ),
      class = "brmshmmdata"
      ))
}

validate_id <- function(id, name_for_message,
                        force_unique = FALSE, force_no_gaps = FALSE, force_no_na = TRUE,
                        reference_levels = NULL, reference_levels_for_message = NULL) {
  if(force_no_na && any(is.na(id))) {
    stop(paste0(name_for_message, " contains NA values"))
  }
  if(!is.factor(id)) {
    if(is.double(id)) {
      if(!identical(round(id), id)) {
        stop(paste0(name_for_message, " includes non-integer numbers"))
      }
      id <- as.integer(id)
    }
    if(is.integer(id) || is.character(id)) {
      if(is.null(reference_levels)) {
        id <- factor(id, levels = sort(unique(id)))
      } else {
        na_orig <- is.na(id)
        id <- factor(id, levels = reference_levels)
        if(any(is.na(id) & !na_orig)) {
          stop(paste0("Some components of ", name_for_message, " do not fit into reference levels (", reference_levels_for_message, " )."))
        }
      }
    } else {
      stop(paste0(name_for_message, " must be a factor, integer or character"))
    }
  } else if(!is.null(reference_levels) && !all(levels(id) == reference_levels)) {
    stop(paste0("Levels of ", name_for_message, " are different than reference levels (", reference_levels_for_message, " )."))
  }

  if(force_unique && !identical(sort(unique(as.integer(id))), 1:length(id))) {
    stop(paste0(name_for_message, " has to be unique and without gaps"))
  } else if(force_no_gaps && !identical(sort(unique(as.integer(id))), 1:max(as.integer(id)))) {
    stop(paste0(name_for_message, " has to be without gaps"))
  }

  id
}

validate_brmshmmdata <- function(d) {
  if(!inherits(d, "brmshmmdata")) {
    stop("Must be of class brmshmmdata")
  }

  d$series_data <- dplyr::ungroup(d$series_data)
  d$states_data <- dplyr::ungroup(d$states_data)

  d$states_data$id <- validate_id(d$states_data$id, "states_data$id", force_unique = TRUE, force_no_gaps = TRUE)
  d$states_data <- d$states_data %>% arrange(as.integer(id))


  d$init_model <- validate_initial_states(d$init_model, d$states_data, d$series_data)
  d$trans_model <- validate_transitions(d$trans_model, d$states_data)
  d$obs_model <- validate_observations(d$obs_model, d$series_data)


  d$series_data$.serie <- validate_id(d$series_data$.serie, "series_data$.serie",
                                     force_no_gaps = TRUE)

  d
}
