brmhmm <- function(brmshmmdata, ...) {
  d <- validate_brmshmmdata(brmshmmdata)

  prepdata <- prepare_data_hmm(d)

  bfit <- brms::brm(
    formula = brms::brmsformula(d$formula, family = family_transitions(d$trans_model)),
    data = prepdata$brmsdata,
    prior = d$prior,
    stanvars =  make_stanvars_hmm(d),
    ...
  )

  structure(list(
    brmsfit = bfit,
    data = brmshmmdata,
    data_processed = prepdata
  ), class = "brmshmmfit")
}


validate_brmshmmfit <- function(fit) {
  if(!inherits(fit, "brmshmmfit")) {
    stop("Not of class brmshmmfit")
  }
  fit
}

#' @export
summary.brmshmmfit <- function(fit) {
  validate_brmshmmfit(fit)
  structure(list(
    brmssummary = summary(fit$brmsfit)#,
    #hmmsummary = summary(fit$brmsfit$fit, pars =  c("sensitivity", "other_observations_probs"))
    ),
    class = "summary.brmshmmfit"
  )
}

#' @export
print.summary.brmshmmfit <- function(s) {
  print(s$brmssummary)
  #print(s$hmmsummary$summary)
}
