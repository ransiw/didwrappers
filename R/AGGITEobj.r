#' @title Aggregate Treatment Effect Parameters Object
#'
#' @description Objects of this class hold results on aggregated
#'  unit-time average treatment effects
#'
#' @inheritParams aggite2
#' @inheritParams compute.aggite2
#' @param overall.att The estimated overall ATT
#' @param overall.se Standard error for overall ATT
#' @param overall.lci The lower confidence interval of the overall ATT
#' @param overall.uci The upper confidence interval of the overall ATT
#' @param egt Holds the length of exposure (for dynamic effects), the
#'  group (for selective treatment timing), the unit (for selective treatment),
#'  cohort (for cohort level effects), or the time period (for calendar
#'  time effects)
#' @param egt2 a second aggregation type to hold a secondary object, NULL if no such object
#' @param att.egt The ATT specific to egt
#' @param se.egt The standard error specific to egt from the bootstrap
#' @param lci.egt The lower confidence interval from the bootstrap
#' @param uci.egt The upper confidence interval from the bootstrap
#' @param crit.val.egt A critical value for computing uniform confidence
#'  bands for dynamic effects, selective treatment timing, or time period
#'  effects.
#' @param inf.function The bootstrap draws specific to the egt
#' @param DIDparams A DIDparams_i object
#'
#' @return an AGGITEobj
#' @export
#'
#' @examples
#' # A helper function for [aggite()]. See examples in documentation in that function.
#'
#'
AGGITEobj <- function(overall.att = NULL,
                     overall.se = NULL,
                     overall.lci = NULL,
                     overall.uci = NULL,
                     type = "simple",
                     type2 = NULL,
                     egt = NULL,
                     egt2 = NULL,
                     att.egt = NULL,
                     se.egt = NULL,
                     lci.egt = NULL,
                     uci.egt = NULL,
                     crit.val.egt = NULL,
                     inf.function = NULL,
                     min_e = NULL,
                     max_e = NULL,
                     balance_e = NULL,
                     min_agg = NULL,
                     call=NULL,
                     DIDparams=NULL) {

  out <- list(overall.att = overall.att,
              overall.se = overall.se,
              overall.lci = overall.lci,
              overall.uci = overall.uci,
              type = type,
              type2 = type2,
              egt = egt,
              egt2 = egt2,
              att.egt = att.egt,
              se.egt = se.egt,
              lci.egt = lci.egt,
              uci.egt = uci.egt,
              crit.val.egt = crit.val.egt,
              inf.function = inf.function,
              min_e = min_e,
              max_e = max_e,
              balance_e = balance_e,
              min_agg = min_agg,
              call = call,
              DIDparams = DIDparams)

  class(out)  <- "AGGITEobj"
  out
}

#' @title Summary Aggregate Treatment Effect Parameter Objects
#'
#' @description A function to summarize aggregated treatment effect parameters.
#'
#' @param object an \code{AGGITEobj} object
#' @param ... other arguments
#'
# @export
summary.AGGITEobj <- function(object, ...) {
  print(paste0("Overall effects is", object$overall.att))
  print(paste0("The confidence region corresponding to the specified level of significance is", object$overall.lci,"-", object$overall.uci))
  print("Please use the function aggite_table() for intermediate results")
}

#' @title print.AGGITEobj
#'
#' @description prints value of a \code{AGGITEobj} object
#'
#' @param x a \code{AGGITEobj} object
#' @param ... extra arguments
#'
# @export
print.AGGITEobj <- function(x,...) {
  summary.AGGITEobj(x,...)
}
