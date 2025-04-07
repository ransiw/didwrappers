#' @title Compute Pair Aggregated Treatment Effect Parameters
#'
#' @description For computing pair aggregated group-time
#'  average treatment effects
#'
#' @inheritParams aggite
#' @param call The function call to aggite2
#'
#' @return [`AGGITEobj`] object
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' # This is a helper function for [aggite2()]. See examples in the documentation there.
#'
#'
compute.aggite2 <- function(MP,
                           type = "group",
                           type2 = "dynamic",
                           balance_e = NULL,
                           min_e = -Inf,
                           max_e = Inf,
                           min_agg = 2,
                           na.rm = FALSE,
                           bstrap = NULL,
                           biters = NULL,
                           cband = NULL,
                           alp = NULL,
                           clustervars = NULL,
                           call = NULL) {

  #-----------------------------------------------------------------------------
  # unpack MP object
  #-----------------------------------------------------------------------------
  # load parameters
  group <- MP$group
  t <- MP$t
  id <- MP$id
  att <- MP$att
  se <- MP$se
  dp <- MP$DIDparams
  inffunc1 <- MP$inffunc
  n <- MP$n


  gname <- dp$gname
  data <- as.data.frame(dp$data)
  tname <- dp$tname
  idname <- dp$idname
  cohort <- dp$cohort
  customnames <- dp$customnames


  if(is.null(clustervars)){
    clustervars <- dp$clustervars
  }
  if(is.null(bstrap)){
    bstrap <- dp$bstrap
  }
  if(is.null(biters)){
    biters <- dp$biters
  }
  if(is.null(alp)){
    alp <- dp$alp
  }
  if(is.null(cband)){
    cband <- dp$cband
  }

  tlist <- dp$tlist
  glist <- dp$glist
  idlist <- dp$idlist
  panel <- dp$panel

  # overwrite MP objects (so we can actually compute bootstrap)
  MP$DIDparams$clustervars <- clustervars
  MP$DIDparams$bstrap <- bstrap
  MP$DIDparams$biters <- biters
  MP$DIDparams$alp <- alp
  MP$DIDparams$cband <- cband
  dp <- MP$DIDparams

  if(!(type %in% c("group",customnames))) {
    stop('`type` must be one of c("group", or custom aggregators)')
  }

  if(!(type2 %in% c("dynamic"))) {
    stop('`type2` must "dynamic')
  }

  if(na.rm){
    notna <- !is.na(se)
    group <- group[notna]
    t <- t[notna]
    id <- id[notna]
    att <- att[notna]
    se <- se[notna]
    inffunc1 <- inffunc1[, notna]
    #tlist <- sort(unique(t))

    # If aggte is of the group type, ensure we have non-missing post-treatment ATTs for each group
    if(type == "group"){
      glist <- sort(unique(group))
      # Get the groups that have some non-missing ATT(g,t) in post-treatmemt periods
      gnotna <- sapply(glist, function(g) {
        # look at post-treatment periods for group g
        whichg <- which( (group == g) & (g <= t))
        attg <- att[whichg]
        group_select <- !is.na(mean(attg))
        return(group_select)
      })
      gnotna <- glist[gnotna]
      # indicator for not all post-treatment ATT(g,t) missing
      not_all_na <- group %in% gnotna
      # Re-do the na.rm thing to update the groups
      group <- group[not_all_na]
      t <- t[not_all_na]
      id <- id[not_all_na]
      att <- att[not_all_na]
      se <- se[not_all_na]
      inffunc1 <- inffunc1[, not_all_na]
      #tlist <- sort(unique(t))
      glist <- sort(unique(group))
    }

    if(type %in% c(customnames)){
      idlist <- sort(unique(id))
      # Get the units that have some non-missing ATT(g,t) in post-treatmemt periods
      gnotna <- sapply(idlist, function(g) {
        # look at post-treatment periods for group g
        whichg <- which( (id == g) & (group <= t))
        attg <- att[whichg]
        group_select <- !is.na(mean(attg))
        return(group_select)
      })
      gnotna <- idlist[gnotna]
      # indicator for not all post-treatment ATT(g,t) missing
      not_all_na <- id %in% gnotna
      # Re-do the na.rm thing to update the groups
      group <- group[not_all_na]
      t <- t[not_all_na]
      id <- id[not_all_na]
      att <- att[not_all_na]
      se <- se[not_all_na]
      inffunc1 <- inffunc1[, not_all_na]
      #tlist <- sort(unique(t))
      # redoing the glist here to drop any NA observations
      glist <- unique(data.frame(id,group))$group
      idlist <- sort(unique(id))
    }
  }


  if((na.rm == FALSE) && base::anyNA(att)) stop("Missing values at att_gt found. If you want to remove these, set `na.rm = TRUE'.")


  # if the type is a cohort, create cohort variable of the size of ATT(g,t) cohortlist and check that each unit is uniquely mapped to a cohort
  if (type %in% c(customnames)){
    cohortlist <- unique(data[,c(idname,type)])
    idcohort <- data.frame(id = idlist)
    colnames(idcohort) <- idname
    idcohort$save <- rep(1,nrow(idcohort))
    cohortlist <- merge(cohortlist,idcohort, by=idname, sort=FALSE)
    # drop if save is missing
    idcohort <- cohortlist[!is.na(cohortlist$save), , drop = FALSE]
    # find duplicates
    has_multiple_types <- any(duplicated(idcohort$id) | duplicated(idcohort$id, fromLast = TRUE))
    if (has_multiple_types) {
      stop("Some ids belong to multiple cohorts. Consider dropping duplicates")
    }
    # add this to an att_gt
    idcohortatt <- data.frame(id=id)
    colnames(idcohortatt) <- idname
    idcohortatt = merge(idcohortatt,idcohort, by=idname, sort=FALSE)
    ccohort = idcohortatt[,type]
    cohortlist = sort(unique(idcohort[,type]))
  }


  # recover a data-frame with only cross-sectional observations
  if(panel){
    # data from first period
    dta <- data[ data[,tname]==tlist[1], ]
  }else {
    #aggregate data
    dta <- base::suppressWarnings(stats::aggregate(data, list((data[,idname])), mean)[,-1])
  }

  #-----------------------------------------------------------------------------
  # data organization and recoding
  #-----------------------------------------------------------------------------

  # if the na.rm is FALSE the glist for group should be unique
  if (type == "group"){
    glist <- sort(unique(group))
  }

  # do some recoding to make sure time periods are 1 unit apart
  # and then put these back together at the end
  originalt <- t
  originalgroup <- group
  originalglist <- glist
  originaltlist <- tlist
  # In case g's are not part of tlist
  originalgtlist <- sort(unique(c(originaltlist,originalglist)))
  uniquet <- seq(1,length(unique(originalgtlist)))
  # function to switch from "new" t values to  original t values
  t2orig <- function(t) {
    unique(c(originalgtlist,0))[which(c(uniquet,0)==t)]
  }
  # function to switch between "original"
  #  t values and new t values
  orig2t <- function(orig) {
    new_t <- c(uniquet,0)[which(unique(c(originalgtlist,0))==orig)]
    out <- ifelse(length(new_t) == 0, NA, new_t)
    out
  }
  t <- sapply(originalt, orig2t)
  group <- sapply(originalgroup, orig2t)
  glist <- unique(data.frame(id,group))$group
  if (type == "group"){
    glist <- sort(unique(group))
  }
  tlist <- unique(t)
  maxT <- max(t)

  # Set the weights
  weights.ind  <-  dta$.w

  # which group time average treatment effects are post-treatment
  keepers <- which(group <= t & t<= (group + max_e)) ### added second condition to allow for limit on longest period included in att

  # n x 1 vector of group variable
  G <-  unlist(lapply(dta[,gname], orig2t))


  #-----------------------------------------------------------------------------
  # Compute the event-study estimators
  #-----------------------------------------------------------------------------

  if (type2 == "dynamic") {


    # event times
    # this looks at all available event times
    # note: event times can be negative here.
    # note: event time = 0 corresponds to "on impact"
    #eseq <- unique(t-group)
    eseq <- unique(originalt - originalgroup)
    eseq <- eseq[order(eseq)]

    # if the user specifies balance_e, then we are going to
    # drop some event times and some groups; if not, we just
    # keep everything (that is what this variable is for)
    include.balanced.gt <- rep(TRUE, length(originalgroup))

    # if we balance the sample with respect to event time
    if (!is.null(balance_e)) {
      include.balanced.gt <- (t2orig(maxT) - originalgroup >= balance_e)

      eseq <- unique(originalt[include.balanced.gt] - originalgroup[include.balanced.gt])
      eseq <- eseq[order(eseq)]

      eseq <- eseq[ (eseq <= balance_e) & (eseq >= balance_e - t2orig(maxT) + t2orig(1))]

    }

    # only looks at some event times
    eseq <- eseq[ (eseq >= min_e) & (eseq <= max_e) ]

    if (type == "group"){

      # change the pg to pi to extract the weights for the treated groups
      pg <- dta[dta[,idname] %in% idlist,".w"]

      # length of this is equal to number of treated units or number of units in idlist
      pgg <- pg

      # same but length is equal to the number of ATT(g,t)
      pg <- pg[match(id, idlist)]

      # compute atts that are specific to each group and event time
      egtlist = lapply(glist, function(g) {
        lapply(eseq, function(e){list(egt=g,egt2=e)
          })
      })
      egtlist = do.call(c,egtlist)

      dynamic.att.e = sapply(glist, function(g) {
        sapply(eseq, function(e){
          # keep att(g,t) for the right g&t as well as ones that
          # are not trimmed out from balancing the sample
          whiche <- which( (group == g) & (originalt - originalgroup == e) & (include.balanced.gt) )
          if (length(whiche)<min_agg){
            NA
          }
          else{
            atte <- att[whiche]
            pge <- pg[whiche]/(sum(pg[whiche]))
            sum(atte*pge)
          }
          })
      })

      dynamic.att.e = c(dynamic.att.e)

      dynamic.se.inner <- lapply(glist, function(g) {
        lapply(eseq, function(e){
          whiche <- which( (group == g) & (originalt - originalgroup == e) & (include.balanced.gt) )
          if (length(whiche)<min_agg){
            # inf.func.e = rep(0,dim(inffunc1)[1])
            inf.func.e <- NA
            se.e <- NA
            lci.e <- NA
            uci.e <- NA
          } else{
            pge <- pg[whiche]/(sum(pg[whiche]))

            inf.func.e <- replicate(biters, {
              random_draws <- sapply(1:length(whiche), function(j) stats::rnorm(1, mean = att[whiche][j], sd = se[whiche][j]))
              sd.e <- stats::sd(att[whiche])/sqrt(length(att[whiche]))
              sum(random_draws*pge) + stats::rnorm(1,sd=sd.e)
            })
            se.e <- stats::sd(inf.func.e)
            lci.e <- stats::quantile(inf.func.e,alp/2)
            uci.e <- stats::quantile(inf.func.e,1-alp/2)
          }

          list(inf.func=inf.func.e, se=se.e, lci=lci.e, uci=uci.e)
        })
      })

      dynamic.se.inner = do.call(c,dynamic.se.inner)

      dynamic.se.e <- unlist(BMisc::getListElement(dynamic.se.inner, "se"))
      dynamic.se.e[dynamic.se.e <= sqrt(.Machine$double.eps)*10] <- NA

      dynamic.lci.e <- unlist(BMisc::getListElement(dynamic.se.inner, "lci"))
      dynamic.uci.e <- unlist(BMisc::getListElement(dynamic.se.inner, "uci"))

      dynamic.inf.func.e <- simplify2array(BMisc::getListElement(dynamic.se.inner, "inf.func"))


      # get overall average treatment effect
      # by averaging over positive dynamics
      epos <- (unlist(BMisc::getListElement(egtlist, "egt2")) >= 0 & !is.na(dynamic.att.e))

      # recalculate the weights
      pgg <- sapply(glist, function(g) {
        sapply(eseq, function(e){
          # keep att(g,t) for the right g&t as well as ones that
          # are not trimmed out from balancing the sample
          whiche <- which( (group == g) & (originalt - originalgroup == e) & (include.balanced.gt) )
          if (length(whiche)<min_agg){
            0
          }
          else{
            sum(pg[whiche])
          }
        })
      })

      pgg <- c(pgg)

      dynamic.att <- sum(dynamic.att.e[epos]*pgg[epos])/sum(pgg[epos])

      if (sum(epos)<2) {
        dynamic.inf.func <- NA
        dynamic.se <- NA
        dynamic.lci <- NA
        dynamic.uci <- NA
      } else{
        dynamic.inf.func <- replicate(biters, {
          random_draws <- sapply(1:sum(epos), function(j) stats::rnorm(1, mean = dynamic.att.e[which(epos)][j], sd = dynamic.se.e[which(epos)][j]))
          sd.e <- stats::sd(random_draws)/sqrt(sum(epos))
          sum((pgg[epos]/sum(pgg[epos]))*random_draws) + stats::rnorm(1,sd=sd.e)
        })

        dynamic.se <- stats::sd(dynamic.inf.func)
        dynamic.lci <- stats::quantile(dynamic.inf.func,alp/2)
        dynamic.uci <- stats::quantile(dynamic.inf.func,1-alp/2)
      }

      if(!is.na(dynamic.se)){
        if (dynamic.se <= sqrt(.Machine$double.eps)*10) dynamic.se <- NA
      }

      return(AGGITEobj(overall.att=dynamic.att,
                       overall.se=dynamic.se,
                       overall.lci = dynamic.lci,
                       overall.uci = dynamic.uci,
                       type=type,
                       type2=type2,
                       egt= sapply(unlist(BMisc::getListElement(egtlist, "egt")),t2orig),
                       egt2 = unlist(BMisc::getListElement(egtlist, "egt2")),
                       att.egt=dynamic.att.e,
                       se.egt=dynamic.se.e,
                       lci.egt=dynamic.lci.e,
                       uci.egt=dynamic.uci.e,
                       crit.val.egt=NULL,
                       inf.function = list(dynamic.inf.func.e = dynamic.inf.func.e,
                                           dynamic.inf.func = dynamic.inf.func),
                       call=call,
                       min_e=min_e,
                       max_e=max_e,
                       balance_e=balance_e,
                       DIDparams=dp
      ))
    }

    if (type %in% c(customnames)){

      # change the pg to pi to extract the weights for the treated groups
      pg <- dta[dta[,idname] %in% idlist,".w"]

      # length of this is equal to number of treated units or number of units in idlist
      pgg <- pg

      # same but length is equal to the number of ATT(g,t)
      pg <- pg[match(id, idlist)]

      # compute atts that are specific to each group and event time
      egtlist = lapply(cohortlist, function(g) {
        lapply(eseq, function(e){list(egt=g,egt2=e)
        })
      })
      egtlist = do.call(c,egtlist)

      dynamic.att.e = sapply(cohortlist, function(g) {
        sapply(eseq, function(e){
          # keep att(g,t) for the right g&t as well as ones that
          # are not trimmed out from balancing the sample
          whiche <- which( (ccohort == g) & (originalt - originalgroup == e) & (include.balanced.gt) )
          if (length(whiche)<min_agg){
            NA
          }
          else{
            atte <- att[whiche]
            pge <- pg[whiche]/(sum(pg[whiche]))
            sum(atte*pge)
          }
          })
      })

      dynamic.att.e = c(dynamic.att.e)

      dynamic.se.inner <- lapply(cohortlist, function(g) {
        lapply(eseq, function(e){
          whiche <- which( (ccohort == g) & (originalt - originalgroup == e) & (include.balanced.gt) )
          if (length(whiche)<min_agg){
            # inf.func.e = rep(0,dim(inffunc1)[1])
            inf.func.e <- NA
            se.e <- NA
            lci.e <- NA
            uci.e <- NA
          } else{
            pge <- pg[whiche]/(sum(pg[whiche]))

            inf.func.e <- replicate(biters, {
              random_draws <- sapply(1:length(whiche), function(j) stats::rnorm(1, mean = att[whiche][j], sd = se[whiche][j]))
              sd.e <- stats::sd(random_draws)/sqrt(length(random_draws))
              sum(random_draws*pge) + stats::rnorm(1,sd=sd.e)
            })

            se.e <- stats::sd(inf.func.e)
            lci.e <- stats::quantile(inf.func.e,alp/2)
            uci.e <- stats::quantile(inf.func.e,1-alp/2)
          }
          #list(inf.func=inf.func.e, se=se.e)
          list(inf.func=inf.func.e, se=se.e, lci=lci.e, uci=uci.e)
        })
      })

      dynamic.se.inner = do.call(c,dynamic.se.inner)

      dynamic.se.e <- unlist(BMisc::getListElement(dynamic.se.inner, "se"))
      dynamic.se.e[dynamic.se.e <= sqrt(.Machine$double.eps)*10] <- NA

      dynamic.lci.e <- unlist(BMisc::getListElement(dynamic.se.inner, "lci"))
      dynamic.uci.e <- unlist(BMisc::getListElement(dynamic.se.inner, "uci"))

      dynamic.inf.func.e <- simplify2array(BMisc::getListElement(dynamic.se.inner, "inf.func"))

      # get overall average treatment effect
      # by averaging over positive dynamics
      epos <- (unlist(BMisc::getListElement(egtlist, "egt2")) >= 0 & !is.na(dynamic.att.e))

      # recalculate the weights
      pgg <- sapply(cohortlist, function(g) {
        sapply(eseq, function(e){
          # keep att(g,t) for the right g&t as well as ones that
          # are not trimmed out from balancing the sample
          whiche <- which( (ccohort == g) & (originalt - originalgroup == e) & (include.balanced.gt) )
          if (length(whiche)<min_agg){
            0
          }
          else{
            sum(pg[whiche])
          }
        })
      })

      pgg <- c(pgg)

      dynamic.att <- sum(dynamic.att.e[which(epos)]*pgg[which(epos)])/sum(pgg[which(epos)])

      if (sum(epos)<2) {
        dynamic.inf.func <- NA
        dynamic.se <- NA
        dynamic.lci <- NA
        dynamic.uci <- NA
      } else{
        dynamic.inf.func <- replicate(biters, {
          random_draws <- sapply(1:sum(epos), function(j) stats::rnorm(1, mean = dynamic.att.e[which(epos)][j], sd = dynamic.se.e[which(epos)][j]))
          sd.e <- stats::sd(random_draws)/sqrt(sum(epos))
          sum((pgg[epos]/sum(pgg[epos]))*random_draws) + stats::rnorm(1,sd=sd.e)
        })

        dynamic.se <- stats::sd(dynamic.inf.func)
        dynamic.lci <- stats::quantile(dynamic.inf.func,alp/2)
        dynamic.uci <- stats::quantile(dynamic.inf.func,1-alp/2)
      }


      if(!is.na(dynamic.se)){
        if (dynamic.se <= sqrt(.Machine$double.eps)*10) dynamic.se <- NA
      }

      return(AGGITEobj(overall.att=dynamic.att,
                       overall.se=dynamic.se,
                       overall.lci=dynamic.lci,
                       overall.uci=dynamic.uci,
                       type=type,
                       type2=type2,
                       egt= unlist(BMisc::getListElement(egtlist, "egt")),
                       egt2 = unlist(BMisc::getListElement(egtlist, "egt2")),
                       att.egt=dynamic.att.e,
                       se.egt=dynamic.se.e,
                       lci.egt=dynamic.lci.e,
                       uci.egt=dynamic.uci.e,
                       crit.val.egt=NULL,
                       inf.function = list(dynamic.inf.func.e = dynamic.inf.func.e,
                                           dynamic.inf.func = dynamic.inf.func),
                       call=call,
                       min_e=min_e,
                       max_e=max_e,
                       balance_e=balance_e,
                       DIDparams=dp
      ))

    }


  }
}
