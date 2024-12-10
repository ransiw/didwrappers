#' @title MP_i
#'
#' @description Multi-period objects that hold results for unit-time average treatment effects
#'
#' @param id which unit the unit-time treatment effects are for
#' @param group which group (defined by period first treated) an unit-time average treatment effect is for
#' @param t which time period a group-time average treatment effect is for
#' @param att the group-average treatment effect for group \code{group} and time
#'  period \code{t}
#' @param c simultaneous critical value if one is obtaining simultaneous confidence
#'  bands. Otherwise it reports the critical value based on pointwise normal
#'  approximation.
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
MP_i <- function(id, group, t, att, V_analytical, se, c, lci, uci, inffunc, n=NULL, aggite=NULL, alp = 0.05, ipwqual=NULL, attcalc=NULL, count=NULL, DIDparams=NULL) {
  out <- list(id=id,group=group, t=t, att=att, V_analytical=V_analytical, se=se, c=c, lci=lci, uci=uci,
  inffunc=inffunc, n=n, aggite=aggite, alp = alp, ipwqual=ipwqual,attcalc=attcalc, count=count,
  DIDparams=DIDparams, call=DIDparams$call)
  class(out) <- "MP_i"
  out
}

#' @title summarizes an MP_i object
#'
#' @description prints a summary of a \code{MP_i} object
#'
#' @param object an \code{MP_i} object
#' @param ... extra arguments
#'
# @export
summary.MP_i <- function(object, ...) {
  print("Please use the attit_table() function for the unit-level ATT[i,t] effects")
}

#' @title print.MP_i
#'
#' @description prints value of a \code{MP_i} object
#'
#' @param x a \code{MP_i} object
#' @param ... extra arguments
#'
# @export
print.MP_i <- function(x,...) {
  summary.MP_i(x,...)
}
