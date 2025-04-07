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
#'  group, the unit,
#'  cohort, custom aggregator, or the time period (for calendar
#'  time effects)
#' @param egt2 a second aggregation type to hold a secondary object, NULL if no such object
#' @param att.egt The ATT specific to egt
#' @param se.egt The standard error specific to egt from the bootstrap
#' @param lci.egt The lower confidence interval from the bootstrap
#' @param uci.egt The upper confidence interval from the bootstrap
#' @param crit.val.egt Always NULL because simultaneous option is turned off.
#' @param inf.function The bootstrap draws specific to the egt. Name is carried over from the did package.
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

