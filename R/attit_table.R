#' Issues tabulated att_it() results
#'
#' @param MP an `MP_i` object that contains the results from an `att_it()` call
#'
#' @return a dataframe with the id, group, time, att estimates, standard errors and confidence intervals.
#' @export
#'
#' @examples
#' simdata = sim_data()
#' attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
#' attdf = attit_table(attobject)
#'
#'
attit_table <- function(MP){
  #idname = MP$DIDparams$idname
  #gname = MP$DIDparams$gname
  #tname = MP$DIDPparams$tname
  results = data.frame(id=MP$id,group=MP$group,t=MP$t,att=MP$att,se=MP$se,attcalc=MP$attcalc,ipwqual=MP$ipwqual,count=MP$count)
  #colnames(results) = c(idname, gname , tname,"att","se","attcalc","ipwqual")
  results$lowci = results$att - (MP$c * results$se)
  results$highci = results$att + (MP$c * results$se)
  return(results)
}
