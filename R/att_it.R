#' Unit-Time Average Treatment Effects
#'
#' @description
#' Computes the 2x2 DID estimate at the unit-time level and allows for unit-level covariate matching in a staggered treatment setting.
#' The original package by Brantly Callaway and Pedro Santa'Anna (`did`) only allows for group-time ATTs
#' where units treated in the same period is in a single group. Unlike the related `did` package, there is no provision for repeated cross-sections.
#'
#'
#' @param yname the name of the outcome variable in the `data`
#' @param tname the name of the column containing the time periods in `data`. Must be numeric
#' @param idname the individual (cross-sectional unit) id name in `data`. Cannot be missing and must be numeric.
#' @param gname the name of the variable in `data` that
#'  contains the first period when a particular observation is treated.
#'  This should be a positive number for all observations in treated groups.
#'  It defines which "treatment-group" a unit belongs to. A zero (0) value is considered to be a unit that is never treated.
#'  A unit treated after the last period in `tname` is also considered never treated.
#' @param cohortnames the names of additional aggregation variables in `data`.
#' @param xformla a formula for the covariates to include in the
#'  model.  It should be of the form `~ X1 + X2`. An intercept is automatically included. Default
#'  is NULL which is equivalent to `xformla=~1`.
#' @param data the name of the data.frame that contains the data.
#' @param panel if panel is TRUE, the data is coerced to a balanced panel on the. Default is TRUE.
#' @param control_group which units to use the control group.
#'  The default is "nevertreated" which sets the control group
#'  to be the group of units that never participate in the
#'  treatment.  This group does not change across groups or
#'  time periods.  The other option is to set
#'  `group="notyettreated"`.  In this case, the control group
#'  is set to the group of units that have not yet participated
#'  in the treatment in that time period.  This includes all
#'  never treated units, but it includes additional units that
#'  eventually participate in the treatment, but have not
#'  participated yet.
#' @param anticipation the number of time periods before participating
#'  in the treatment where units can anticipate participating in the
#'  treatment and therefore it can affect their untreated potential outcomes.
#' @param weightsname the name of the column containing the sampling weights.
#'  If not set, all observations have same weight.
#' @param alp the significance level, default is 0.05
#' @param bstrap boolean for whether or not to compute standard errors using
#'  the multiplier bootstrap.  If standard errors are clustered, then one
#'  must set `bstrap=TRUE`. Default is `TRUE` (in addition, cband
#'  is also by default `TRUE` indicating that uniform confidence bands
#'  will be returned.  If bstrap is `FALSE`, then analytical
#'  standard errors are reported.
#' @param cband boolean for whether or not to compute a uniform confidence
#'  band that covers all of the group-time average treatment effects
#'  with fixed probability `1-alp`.  In order to compute uniform confidence
#'  bands, `bstrap` must also be set to `TRUE`.  The default is
#' `TRUE`.
#' @param biters the number of bootstrap iterations to use.  The default is 1000,
#'  and this is only applicable if `bstrap=TRUE`.
#' @param clustervars a vector of variables names to cluster on.  At most, there
#'  can be two variables (otherwise will throw an error) and one of these
#'  must be the same as idname which allows for clustering at the individual
#'  level. By default, we cluster at individual level (when `bstrap=TRUE`)
#' @param est_method the method to compute group-time average treatment effects.  The default is "dr" which uses the doubly robust
#' approach in the `DRDID` package.  Other built-in methods
#' include "ipw" for inverse probability weighting (Hajek type) and "reg" for
#' first step regression estimators.
#' @param overlap the treatment of units that violate overlap conditions when the `est_method` is "dr" or "ipw".
#' The default, "trim", is to drop the unit but report the calculated ATT for further analysis. Overlap is violated if the maximum pscore exceeds 0.999
#' The other option, "retain" retains these units for inference.
#' @param base_period whether to use a "varying" base period or a
#'  "universal" base period for placebo tests. A varying base period calculates a pseudo-ATT for every two consecutive pre-treatment periods.
#'  A universal base period fixes the base period to always be (g-anticipation-1).  Either choice results in the same
#'  post-treatment estimates of ATT(g,t)'s.
#' @param print_details whether or not to show details/progress of computations.
#'   Default is `FALSE`.
#' @param pl whether or not to use parallel processing
#' @param cores the number of cores to use for parallel processing
#'
#' @return An [`MP_i`] object containing all the results for unit-time treatment effects.
#' @export
#'
#' @examples
#' # run the function with the default settings and default data
#' attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = sim_data())
#'
att_it <- function(yname,
                   tname,
                   idname,
                   gname,
                   cohortnames = NULL,
                   xformla = NULL,
                   data,
                   panel=TRUE,
                   control_group=c("nevertreated","notyettreated"),
                   anticipation=0,
                   weightsname=NULL,
                   alp=0.05,
                   bstrap=TRUE,
                   cband=TRUE,
                   biters=1000,
                   clustervars=NULL,
                   est_method="dr",
                   overlap="trim",
                   base_period="varying",
                   print_details=FALSE,
                   pl=FALSE,
                   cores=1){

  dp <- pre_process_did_i(yname=yname,
                         tname=tname,
                         idname=idname,
                         gname=gname,
                         cohortnames = cohortnames,
                         xformla=xformla,
                         data=data,
                         panel=panel,
                         control_group=control_group,
                         anticipation=anticipation,
                         weightsname=weightsname,
                         alp=alp,
                         bstrap=bstrap,
                         cband=cband,
                         biters=biters,
                         clustervars=clustervars,
                         est_method=est_method,
                         overlap=overlap,
                         base_period=base_period,
                         print_details=print_details,
                         pl=pl,
                         cores=cores,
                         call=match.call()
  )

  results <- compute.att_it(dp)

  # extract ATT(g,t) and influence functions
  attgt.list <- results$attgt.list
  inffunc <- results$inffunc

  # process results
  attgt.results <- process_attit(attgt.list)
  id <- attgt.results$id
  group <- attgt.results$group
  att <- attgt.results$att
  tt <- attgt.results$tt
  ipwqual <- attgt.results$ipwqual
  attcalc <- attgt.results$attcalc
  count <- attgt.results$count

  # analytical standard errors
  # estimate variance
  # this is analogous to cluster robust standard errors that
  # are clustered at the unit level

  n <- dp$n
  V <- Matrix::t(inffunc)%*%inffunc/n
  se <- sqrt(Matrix::diag(V)/n)

  # Zero standard error replaced by NA
  se[se <= sqrt(.Machine$double.eps)*10] <- NA

  # if clustering along another dimension...we require using the
  # bootstrap (in principle, could come up with analytical standard
  # errors here though)
  if ( (length(clustervars) > 0) & !bstrap) {
    warning("clustering the standard errors requires using the bootstrap, resulting standard errors are NOT accounting for clustering")
  }

  # Identify entries of main diagonal V that are zero or NA
  zero_na_sd_entry <- unique(which(is.na(se)))

  # bootstrap variance matrix
  if (bstrap) {

    bout <- did::mboot(inffunc, DIDparams=dp, pl=pl, cores=cores)
    bres <- bout$bres

    if(length(zero_na_sd_entry)>0) {
      se[-zero_na_sd_entry] <- bout$se[-zero_na_sd_entry]
    } else {
      se <- bout$se
    }
  }
  # Zero standard error replaced by NA
  se[se <= sqrt(.Machine$double.eps)*10] <- NA

  #-----------------------------------------------------------------------------
  # compute confidence intervals / bands
  #-----------------------------------------------------------------------------

  # critical value from N(0,1), for pointwise
  cval <- stats::qnorm(1-alp/2)

  # in order to get uniform confidence bands
  # HAVE to use the bootstrap
  if (bstrap){
    if (cband) {
      # for uniform confidence band
      # compute new critical value
      # see paper for details
      bSigma <- apply(bres, 2,
                      function(b) (stats::quantile(b, .75, type=1, na.rm = T) -
                                     stats::quantile(b, .25, type=1, na.rm = T))/(stats::qnorm(.75) - stats::qnorm(.25)))

      bSigma[bSigma <= sqrt(.Machine$double.eps)*10] <- NA

      # sup-t confidence band
      bT <- apply(bres, 1, function(b) max(abs(b/bSigma), na.rm = TRUE))
      cval <- stats::quantile(bT, 1-alp, type=1, na.rm = T)
      if(cval >= 7){
        warning("Simultaneous critical value is arguably `too large' to be realible. This usually happens when number of observations per group is small and/or there is no much variation in outcomes.")
      }
    }
  }

  return(MP_i(id=id ,group=group, t=tt, att=att, V_analytical=V, se=se, c=cval, inffunc=inffunc, n=n, alp = alp, ipwqual=ipwqual,attcalc=attcalc, count=count, DIDparams=dp))
}
