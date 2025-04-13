#' @title MP_i
#'
#' @description Multi-period objects that hold results for unit-time average treatment effects
#'
#' @param id which unit the unit-time treatment effects are for
#' @param group which group (defined by period first treated) an unit-time average treatment effect is for
#' @param t which time period a group-time average treatment effect is for
#' @param att the estimate for unit \code{id} and time
#'  period \code{t}
#' @param c always NULL
#' @param V_analytical Analytical estimator for the asymptotic variance-covariance matrix for unit-time average treatment effects
#' @param se standard errors for unit-time average treatment effects. If bootstrap is set to TRUE, this provides bootstrap-based se.
#' @param lci lower confidence interval for att
#' @param uci upper confidence interval for att
#' @param inffunc the influence function for estimating group-time average treatment effects
#' @param n the number of unique cross-sectional units (unique values of idname)
#' @param aggite an aggregate treatment effects object
#' @param alp the significance level, default is 0.05
#' @param ipwqual the maximum propensity score
#' @param attcalc similar to att but does not remove estimates when there are propensity score problems
#' @param baseline the baseline of the unit
#' @param count takes a count of the number of units in the component estimate
#' @param DIDparams a [`DIDparams_i`] object.  A way to optionally return the parameters
#'  of the call to [att_it()].
#'
#' @return MP object
#' @export
#'
#' @examples
#' # Helper function for [att_it()]. See documentation of that function for an example.
#'
#'
MP_i <- function(id, group, t, att, V_analytical, se, c, lci, uci, inffunc, n=NULL, aggite=NULL, alp = 0.05, ipwqual=NULL, attcalc=NULL, baseline=NULL, count=NULL, DIDparams=NULL) {
  out <- list(id=id,group=group, t=t, att=att, V_analytical=V_analytical, se=se, c=c, lci=lci, uci=uci,
  inffunc=inffunc, n=n, aggite=aggite, alp = alp, ipwqual=ipwqual,attcalc=attcalc, count=count, baseline=baseline,
  DIDparams=DIDparams, call=DIDparams$call)
  class(out) <- "MP_i"
  out
}

