#' Issues tabulated aggite() results
#'
#' @param AGGITE an object of class AGGITEobj that collects the results from a `aggite()` or `aggite2()` call
#'
#' @return a data-frame with the aggregation type `egt`,
#' the corresponding att calculation `att.egt`, standard errors `se.egt`, and confidence intervals.
#' @export
#'
#' @examples
#' # first run the att_it() function
#' simdata = sim_data(posttreat=6, pretreat=2)
#' attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
#'
#' # aggregate all post-treatment effects of each unit
#' agtobject = aggite(attobject,type="unit")
#' agttable = aggite_table(agtobject)
#'
#'
aggite_table <- function(AGGITE){
  # results = data.frame(egt=AGGITE$egt,att.egt=AGGITE$att.egt,se.egt=AGGITE$se.egt)
  if (is.null(AGGITE$crit.val.egt)){
    results = data.frame(egt=AGGITE$egt,att.egt=AGGITE$att.egt,se.egt=AGGITE$se.egt,lci.egt=AGGITE$lci.egt,uci.egt=AGGITE$uci.egt)
  } else {
    results = data.frame(egt=AGGITE$egt,att.egt=AGGITE$att.egt,se.egt=AGGITE$se.egt)
    results$lci.egt = results$att.egt - AGGITE$crit.val.egt * results$se.egt
    results$uci.egt = results$att.egt + AGGITE$crit.val.egt * results$se.egt
  }
  if (!is.null(AGGITE$egt2)){
    results = data.frame(egt=AGGITE$egt,egt2=AGGITE$egt2,att.egt=AGGITE$att.egt,se.egt=AGGITE$se.egt,lci.egt=AGGITE$lci.egt,uci.egt=AGGITE$uci.egt)
  }
  # if (is.null(AGGITE$lci.egt) & !is.null(crit.val.egt)){
  #   results$lci.egt = results$att.egt - AGGITE$crit.val.egt * results$se.egt
  #   results$uci.egt = results$att.egt + AGGITE$crit.val.egt * results$se.egt
  # }
  return(results)
}
