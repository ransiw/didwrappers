#' Process Results from [compute.att_it()]
#'
#' @param attgt.list list of results from [compute.att_gt()]
#'
#' @return list with elements:
#' \item{id}{which id a set of results belongs to}
#' \item{group}{which group the id belongs to}
#' \item{tt}{which time period a set of results belongs to}
#' \item{att}{the unit time average treatment effect (NA if there were propensity score problems)}
#' \item{ipwqual}{the maximum propensity score}
#' \item{attcalc}{the unit time average treatment effect (always complete unless NaN)}
#' @export
#'
process_attit <- function(attgt.list) {
  nG <- length(unique(unlist(BMisc::getListElement(attgt.list, "id"))))
  nT <- length(unique(unlist(BMisc::getListElement(attgt.list, "year"))))

  # create vectors to hold the results
  group <- c()
  att <- c()
  tt <- c()
  id <- c()
  ipwqual <- c()
  attcalc <- c()
  i <- 1

  # populate result vectors and matrices
  for (f in 1:nG) {
    for (s in 1:nT) {
      id[i] <- attgt.list[[i]]$id
      group[i] <- attgt.list[[i]]$group
      tt[i] <- attgt.list[[i]]$year
      att[i] <- attgt.list[[i]]$att
      ipwqual[i] <- attgt.list[[i]]$ipwqual
      attcalc[i] <- attgt.list[[i]]$attcalc
      i <- i+1
    }
  }

  list(group=group, att=att, tt=tt, id=id, ipwqual=ipwqual,attcalc=attcalc)
}
