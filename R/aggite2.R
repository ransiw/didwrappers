#' @title Pair Aggregate Unit-Time Average Treatment Effects
#'
#' @description A function to take unit-time average treatment effects
#'  and aggregate them into sub-effects where there is more than one aggregating type.
#'  Possible aggregations include pairs of any of the following:
#'  "group" or a custom aggregation name with "dynamic"
#'
#' @param MP an MP_i object (i.e., the results of the [att_it()] method)
#' @param type Which type of aggregated treatment effect parameter to compute.
#'   One option is "simple" (this just computes a weighted average of all
#'   unit-time average treatment effects).
#'   Other options are "dynamic" (this computes average effects across
#'   different lengths of exposure to the treatment and is similar to an
#'   "event study"; here the overall effect averages the effect of the
#'   treatment across all positive lengths of exposure); "group" (this
#'   is the default option and
#'   computes average treatment effects across different groups; here
#'   the overall effect averages the effect across different groups); and
#'   "calendar" (this computes average treatment effects across different
#'   time periods; here the overall effect averages the effect across each
#'   time period).
#' @param type2 the second type of aggregator
#' @param balance_e If set (and if one computes dynamic effects), it balances
#'  the sample with respect to event time.  For example, if `balance.e=2`,
#'  `aggite` will drop groups that are not exposed to treatment for
#'  at least three periods. (the initial period when `e=0` as well as the
#'  next two periods when `e=1` and the `e=2`).  This ensures that
#'  the composition of groups does not change when event time changes.
#' @param min_e For event studies, this is the smallest event time to compute
#'  dynamic effects for.  By default, `min_e = -Inf` so that effects at
#'  all lengths of exposure are computed.
#' @param max_e For event studies, this is the largest event time to compute
#'  dynamic effects for.  By default, `max_e = Inf` so that effects at
#'  all lengths of exposure are computed.
#' @param min_agg the minimum number of unit-time effects required for aggregation. Default is 2.
#' @param na.rm Logical value if we are to remove missing Values from analyses. Defaults is FALSE.
#' @param bstrap This is always TRUE. Turning off makes no difference in the second-step.
#' @param biters The number of bootstrap iterations to use.  The default is the value set in the MP_i object
#' @param maxbackp The number of pre-period observations sampled for baseline variance. Default is 5.
#' @param ignorebackp Logical for whether the pre-period should be sampled for baseline variance. Default is FALSE.
#' @param cband Does not apply. All confidence intervals are bootstrapped confidence intervals in the second-step.
#' @param alp Significance level, default is value set in the MP_i object.
#' @param clustervars Does not apply.
#'
#' @return An [`AGGITEobj`] object that holds the results from the aggregation.
#'
#' @export
#'
#' @examples
#' # first run the att_it() function
#' simdata = sim_data(posttreat=6)
#' attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
#'
#' # aggregate all post-treatment effects of the group and dynamic level
#' agtobject = aggite2(attobject,type="group",type2="dynamic")
#'
#'
aggite2 <- function(MP,
                   type = "group",
                   type2 = "dynamic",
                   balance_e = NULL,
                   min_e = -Inf,
                   max_e = Inf,
                   min_agg = 2,
                   na.rm = FALSE,
                   bstrap = NULL,
                   biters = NULL,
                   maxbackp = 5,
                   ignorebackp = FALSE,
                   cband = NULL,
                   alp = NULL,
                   clustervars = NULL
) {

  call <- match.call()

  compute.aggite2(MP = MP,
                 type = type,
                 type2 = type2,
                 balance_e = balance_e,
                 min_e = min_e,
                 max_e = max_e,
                 min_agg = min_agg,
                 na.rm = na.rm,
                 bstrap = bstrap,
                 biters = biters,
                 maxbackp = maxbackp,
                 ignorebackp = ignorebackp,
                 cband = cband,
                 alp = alp,
                 clustervars = clustervars,
                 call = call)
}
