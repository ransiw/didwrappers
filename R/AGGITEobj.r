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

  # call
  cat("\n")
  cat("Call:\n")
  print(object$call)
  cat("\n")

  # overall estimates
  alp <- object$DIDparams$alp
  pointwise_cval <- stats::qnorm(1-alp/2)
  overall_cband_upper <- object$overall.att + pointwise_cval*object$overall.se
  overall_cband_lower <- object$overall.att - pointwise_cval*object$overall.se
  out1 <- cbind.data.frame(object$overall.att, object$overall.se, overall_cband_lower, overall_cband_upper)
  out1 <- round(out1, 4)
  overall_sig <- (overall_cband_upper < 0) | (overall_cband_lower > 0)
  overall_sig[is.na(overall_sig)] <- FALSE
  overall_sig_text <- ifelse(overall_sig, "*", "")
  out1 <- cbind.data.frame(out1, overall_sig_text)
  cat("\n")
  #cat("Overall ATT:  \n")
  if (object$type=="dynamic") cat("Overall summary of ATT\'s based on event-study/dynamic aggregation:  \n")
  if (object$type=="group") cat("Overall summary of ATT\'s based on group aggregation:  \n")
  if (object$type=="unit") cat("Overall summary of ATT\'s based on unit aggregation:  \n")
  if (object$type %in% object$DIDparams$cohortnames) cat("Overall summary of ATT\'s based on other aggregation:  \n")
  if (object$type=="calendar") cat("Overall summary of ATT\'s based on calendar time aggregation:  \n")
  colnames(out1) <- c("ATT","   Std. Error", paste0("    [ ",100*(1-object$DIDparams$alp),"% "), "Conf. Int.]","")
  print(out1, row.names=FALSE)
  cat("\n\n")

  # handle cases depending on type
  if (object$type %in% c("group","dynamic","calendar")) {

    # header
    if (object$type=="dynamic") { c1name <- "Event time"; cat("Dynamic Effects:") }
    if (object$type=="group") { c1name <- "Group"; cat("Group Effects:") }
    if (object$type=="unit") { c1name <- "Unit"; cat("Unit Effects:") }
    if (object$type %in% object$DIDparams$cohortnames) { c1name <- "Other"; cat("Other Effects:") }
    if (object$type=="calendar") { c1name <- "Time"; cat("Time Effects:") }

    cat("\n")
    cband_text1a <- paste0(100*(1-object$DIDparams$alp),"% ")
    cband_text1b <- ifelse(object$DIDparams$bstrap,
                           ifelse(object$DIDparams$cband, "Simult. ", "Pointwise "),
                           "Pointwise ")
    cband_text1 <- paste0("[", cband_text1a, cband_text1b)

    cband_lower <- object$att.egt - object$crit.val.egt*object$se.egt
    cband_upper <- object$att.egt + object$crit.val.egt*object$se.egt

    sig <- (cband_upper < 0) | (cband_lower > 0)
    sig[is.na(sig)] <- FALSE
    sig_text <- ifelse(sig, "*", "")

    out2 <- cbind.data.frame(object$egt, object$att.egt, object$se.egt, cband_lower, cband_upper)
    out2 <- round(out2, 4)
    out2 <- cbind.data.frame(out2, sig_text)

    colnames(out2) <- c(c1name, "Estimate","Std. Error", cband_text1, "Conf. Band]", "")
    print(out2, row.names=FALSE, justify = "centre")
  }
  cat("---\n")
  cat("Signif. codes: `*' confidence band does not cover 0")
  cat("\n\n")

  # set control group text
  control_group <- object$DIDparams$control_group
  control_group_text <- NULL
  if (control_group == "nevertreated") {
    control_group_text <- "Never Treated"
  } else if (control_group == "notyettreated") {
    control_group_text <- "Not Yet Treated"
  }

  if (!is.null(control_group)) {
    cat("Control Group:  ")
    cat(control_group_text)
    cat(",  ")
  }

  # anticipation periods
  cat("Anticipation Periods:  ")
  cat(object$DIDparams$anticipation)
  cat("\n")

  # estimation method text
  est_method <- object$DIDparams$est_method
  if ( methods::is(est_method,"character") ) {
    est_method_text <- est_method
    if (est_method == "dr") {
      est_method_text <- "Doubly Robust"
    } else if (est_method == "ipw") {
      est_method_text <- "Inverse Probability Weighting"
    } else if (est_method == "reg") {
      est_method_text <- "Outcome Regression"
    }

    cat("Estimation Method:  ")
    cat(est_method_text)
    cat("\n")
  }
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
