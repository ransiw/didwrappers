#' Issues tabulated aggite() results
#'
#' @param AGGITE an object of class AGGITEobj that collects the results from a `aggite()` call
#'
#' @return a data-frame with the aggregation type `egt`,
#' the corresponding att calculation `att.egt`, standard errors `se.egt`, and confidence intervals.
#' @export
#'
aggite_table <- function(AGGITE){
  results = data.frame(egt=AGGITE$egt,att.egt=AGGITE$att.egt,se.egt=AGGITE$se.egt)
  #colnames(results) = c(idname, gname , tname,"att","se","attcalc","ipwqual")
  results$lowci = results$att.egt - (AGGITE$crit.val.egt * results$se.egt)
  results$highci = results$att.egt + (AGGITE$crit.val.egt * results$se.egt)
  return(results)
}
