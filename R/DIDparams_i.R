#' Title DIDparams_i
#'
#' @inheritParams att_it
#' @inheritParams pre_process_did_i
#' @param n The number of individual and time observations
#' @param nG The number of units and not just treatment cohorts
#' @param nT The number of time periods
#' @param tlist A vector containing all time periods
#' @param glist A vector containing each treatment-time group and allows repetitions.
#' @param idlist A vector containing each id that corresponds to the glist vector.
#'
#' @return a DIDparams_i object
#' @export
#'
#' @examples
#' # Helper function for [pre_process_did_i()]. See documentation of that function for an example.
#'
#'
DIDparams_i <- function(yname,
                       tname,
                       idname,
                       gname,
                       cohortnames=NULL,
                       xformla=NULL,
                       data,
                       control_group,
                       anticipation=0,
                       weightsname=NULL,
                       weightfs=FALSE,
                       alp=0.05,
                       bstrap=TRUE,
                       biters=1000,
                       clustervars = NULL,
                       cband=TRUE,
                       print_details=TRUE,
                       pl=FALSE,
                       cores=1,
                       est_method="dr",
                       overlap = "trim",
                       base_period="varying",
                       panel=TRUE,
                       n=NULL,
                       nG=NULL,
                       nT=NULL,
                       tlist=NULL,
                       glist=NULL,
                       idlist =NULL,
                       call=NULL) {

  out <- list(yname=yname,
              tname=tname,
              idname=idname,
              gname=gname,
              cohortnames = cohortnames,
              xformla=xformla,
              data=data,
              control_group=control_group,
              anticipation=anticipation,
              weightsname=weightsname,
              weightfs = weightfs,
              alp=alp,
              bstrap=bstrap,
              biters=biters,
              clustervars=clustervars,
              cband=cband,
              print_details=print_details,
              pl=pl,
              cores=cores,
              est_method=est_method,
              overlap=overlap,
              base_period=base_period,
              panel=panel,
              n=n,
              nG=nG,
              nT=nT,
              tlist=tlist,
              glist=glist,
              idlist = idlist,
              call=call)
  class(out) <- "DIDparams_i"
  out
}
