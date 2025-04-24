#' @title Compute Aggregated Treatment Effect Parameters
#'
#' @description Computes aggregated average treatment effects
#'
#' @inheritParams aggite
#' @param call The function call to aggite
#'
#' @return [`AGGITEobj`] object
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' # This is a helper function for [aggite()]. See that function for examples.
#'
#'
compute.aggite <- function(MP,
                          type = "group",
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
  baseline <- MP$baseline
  baset <- MP$baset
  outcome <- MP$outcome


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

  if(!(type %in% c("simple", "dynamic", "group", "unit", "calendar", customnames))) {
    stop('`type` must be one of c("simple", "dynamic", "group", "unit", "calendar", or a custom aggregator)')
  }

  # create the pre-treatment sds
  if (ignorebackp){
    sdibase <- rep(0,length(id))
  } else{
    sdibase <- lapply(1:length(id), function(l){
      backlook <- outcome[id==id[l] & t<=(baset[l]) & t>=(baset[l]-maxbackp)]
      backtime <- t[id==id[l] & t<=(baset[l]) & t>=(baset[l]-maxbackp)]
      if (sum(!is.na(backlook))<2){
        return(0)
      } else{
        cent_reg = stats::lm(backlook~backtime)
        return(stats::sd(cent_reg$residuals))
      }
    })
    sdibase <- unlist(sdibase)
  }

  if (length(sdibase)!=length(baseline)){
    stop("base vectors are not the same length")
  }

  if(na.rm){
    notna <- !is.na(se)
    group <- group[notna]
    t <- t[notna]
    id <- id[notna]
    att <- att[notna]
    se <- se[notna]
    baseline <- baseline[notna]
    sdibase <- sdibase[notna]
    inffunc1 <- inffunc1[, notna]
    #tlist <- sort(unique(t))

    # If aggte is of the group type, ensure we have non-missing post-treatment ATTs for each group
    # if(type == "group"){
    #   glist <- sort(unique(group))
    #   # Get the groups that have some non-missing ATT(g,t) in post-treatmemt periods
    #   gnotna <- sapply(glist, function(g) {
    #     # look at post-treatment periods for group g
    #     whichg <- which( (group == g) & (g <= t))
    #     attg <- att[whichg]
    #     group_select <- !is.na(mean(attg))
    #     return(group_select)
    #   })
    #   gnotna <- glist[gnotna]
    #   # indicator for not all post-treatment ATT(g,t) missing
    #   not_all_na <- group %in% gnotna
    #   # Re-do the na.rm thing to update the groups
    #   group <- group[not_all_na]
    #   t <- t[not_all_na]
    #   id <- id[not_all_na]
    #   att <- att[not_all_na]
    #   se <- se[not_all_na]
    #   baseline <- baseline[not_all_na]
    #   sdibase <- sdibase[not_all_na]
    #   inffunc1 <- inffunc1[, not_all_na]
    #   #tlist <- sort(unique(t))
    #   glist <- sort(unique(group))
    # }

    if(type %in% c("unit", "simple", "group", customnames)){
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
      baseline <- baseline[not_all_na]
      sdibase <- sdibase[not_all_na]
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
      stop("Some ids belong to multiple cohorts or custom aggregators. Consider dropping duplicates")
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
    idlist <- sort(unique(id))
  }

  if (type == "unit"){
    idlist <- sort(unique(id))
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
  # Compute simple aggregate using the unit-level
  #-----------------------------------------------------------------------------

  # change the pg to pi to extract the weights for the treated groups
  pg <- dta[dta[,idname] %in% idlist,".w"]

  # length of this is equal to number of treated units or number of units in idlist
  pgg <- pg

  # same but length is equal to the number of ATT(g,t)
  pg <- pg[match(id, idlist)]

  # get group specific ATTs
  # note: there are no estimated weights here
  simple.att.i <- sapply(idlist, function(g) {
    # look at post-treatment periods for group g
    whichi <- which( (id == g) & (group <= t) & (t<= (group + max_e))) ### added last condition to allow for limit on longest period included in att
    atti <- att[whichi]
    # Take out of if else condition
    sum(atti*(pg[whichi]/sum(pg[whichi])))
    # if (length(whichi)<min_agg){
    #   NA
    # } else {
    #   sum(atti*(pg[whichi]/sum(pg[whichi])))
    # }
  })
  simple.att.i[is.nan(simple.att.i)] <- NA


  # get standard errors for each group specific ATT
  simple.se.inner <- lapply(idlist, function(g) {
    whichi <- which( (id == g) & (group <= t) & (t<= (group + max_e)))  ### added last condition to allow for limit on longest period included in att

    if (length(whichi)<min_agg){
      inf.func.i <- NA
      se.i <- NA
      lci.i <- NA
      uci.i <- NA
    } else{
      inf.func.i <- replicate(biters, {
        random_draws <- stats::rnorm(length(whichi), mean = att[whichi], sd = se[whichi])
        random_draws <- random_draws + baseline[whichi] - stats::rnorm(1,baseline[whichi],sdibase[whichi])
        if (length(random_draws)<2){
          random_draws <- random_draws
        } else{
          random_draws <- random_draws + stats::rnorm(length(whichi),sd=stats::sd(random_draws))
        }
        sum(random_draws*(pg[whichi]/sum(pg[whichi])))
      })
      # se.e <- getSE(inf.func.e, dp)
      se.i <- stats::sd(inf.func.i)
      lci.i <- stats::quantile(inf.func.i,alp/2)
      uci.i <- stats::quantile(inf.func.i,1-alp/2)

    }
    list(inf.func=inf.func.i, se=se.i, lci=lci.i, uci=uci.i)
  })

  # recover standard errors separately by group
  simple.se.i <- unlist(BMisc::getListElement(simple.se.inner, "se"))
  simple.se.i[simple.se.i <= sqrt(.Machine$double.eps)*10] <- NA

  # recover influence function separately by group
  # simple.if <- simplify2array(BMisc::getListElement(simple.se.inner, "inf.func"))

  # add a positional argument to notate the missings
  epos <- !is.na(simple.att.i)

  # get overall att under selective treatment timing
  # (here use pgg instead of pg because we can just look at each group)
  simple.att <- sum(simple.att.i[which(epos)] * pgg[which(epos)])/sum(pgg[which(epos)])


  if (sum(epos)<min_agg) {
    simple.inf.func <- NA
    simple.se <- NA
    simple.lci <- NA
    simple.uci <- NA
  } else{
    simple.inf.func <- replicate(biters, {
      random_draws <- sapply(1:sum(epos), function(j)
        if (is.na(simple.se.i[which(epos)][j])){
          simple.att.i[which(epos)][j]
        } else{
          stats::rnorm(1, mean = simple.att.i[which(epos)][j], sd = simple.se.i[which(epos)][j])
        })
      random_draws <- random_draws + stats::rnorm(sum(epos),sd = stats::sd(random_draws))
      sum((pgg[which(epos)]/sum(pgg[which(epos)]))*random_draws)
    })

    simple.se <- stats::sd(simple.inf.func)
    simple.lci <- stats::quantile(simple.inf.func,alp/2)
    simple.uci <- stats::quantile(simple.inf.func,1-alp/2)
  }


  if(!is.na(simple.se)){
    if((simple.se <= sqrt(.Machine$double.eps)*10)) simple.se <- NA
  }

  if (type == "simple"){
    return(AGGITEobj(overall.att = simple.att,
                     overall.se = simple.se,
                     overall.lci = simple.lci,
                     overall.uci = simple.uci,
                     type = type,
                     inf.function = NULL,
                     call=call,
                     DIDparams=dp))
  }

  #-----------------------------------------------------------------------------
  # Compute unit level aggregate
  #-----------------------------------------------------------------------------

  if (type == "unit") {

    # Not run since same as above

    # # change the pg to pi to extract the weights for the treated groups
    # pg <- dta[dta[,idname] %in% idlist,".w"]
    #
    # # length of this is equal to number of treated units or number of units in idlist
    # pgg <- pg
    #
    # # same but length is equal to the number of ATT(g,t)
    # pg <- pg[match(id, idlist)]
    #
    # # get group specific ATTs
    # # note: there are no estimated weights here
    # selective.att.i <- sapply(idlist, function(g) {
    #   # look at post-treatment periods for group g
    #   whichi <- which( (id == g) & (group <= t) & (t<= (group + max_e))) ### added last condition to allow for limit on longest period included in att
    #   atti <- att[whichi]
    #   if (length(whichi)<min_agg){
    #     NA
    #   } else {
    #     sum(atti*(pg[whichi]/sum(pg[whichi])))
    #   }
    # })
    # selective.att.i[is.nan(selective.att.i)] <- NA
    #
    #
    # # get standard errors for each group specific ATT
    # selective.se.inner <- lapply(idlist, function(g) {
    #   whichi <- which( (id == g) & (group <= t) & (t<= (group + max_e)))  ### added last condition to allow for limit on longest period included in att
    #
    #   if (length(whichi)<min_agg){
    #     inf.func.i <- NA
    #     se.i <- NA
    #     lci.i <- NA
    #     uci.i <- NA
    #   } else{
    #     inf.func.i <- replicate(biters, {
    #       random_draws <- stats::rnorm(length(whichi), mean = att[whichi], sd = se[whichi])
    #       random_draws <- random_draws + baseline[whichi] - stats::rnorm(1,baseline[whichi],sdibase[whichi])
    #       random_draws <- random_draws + stats::rnorm(length(whichi),sd=stats::sd(random_draws))
    #       sum(random_draws*(pg[whichi]/sum(pg[whichi])))
    #     })
    #     se.i <- stats::sd(inf.func.i)
    #     lci.i <- stats::quantile(inf.func.i,alp/2)
    #     uci.i <- stats::quantile(inf.func.i,1-alp/2)
    #
    #   }
    #   list(inf.func=inf.func.i, se=se.i, lci=lci.i, uci=uci.i)
    # })
    #
    # # recover standard errors separately by group
    # selective.se.i <- unlist(BMisc::getListElement(selective.se.inner, "se"))
    # selective.se.i[selective.se.i <= sqrt(.Machine$double.eps)*10] <- NA
    #
    # selective.lci.i <- unlist(BMisc::getListElement(selective.se.inner, "lci"))
    # selective.uci.i <- unlist(BMisc::getListElement(selective.se.inner, "uci"))
    #
    # # recover influence function separately by group
    # selective.inf.func.i <- simplify2array(BMisc::getListElement(selective.se.inner, "inf.func"))
    #
    # # add a positional argument to notate the missings
    # epos <- !is.na(selective.att.i)
    #
    # # get overall att under selective treatment timing
    # # (here use pgg instead of pg because we can just look at each group)
    # selective.att <- sum(selective.att.i[which(epos)] * pgg[which(epos)])/sum(pgg[which(epos)])
    #
    #
    # if (sum(epos)<2) {
    #   selective.inf.func <- NA
    #   selective.se <- NA
    #   selective.lci <- NA
    #   selective.uci <- NA
    # } else{
    #   selective.inf.func <- replicate(biters, {
    #     random_draws <- sapply(1:sum(epos), function(j) stats::rnorm(1, mean = selective.att.i[which(epos)][j], sd = selective.se.i[which(epos)][j]))
    #     random_draws <- random_draws + stats::rnorm(sum(epos),sd = stats::sd(random_draws))
    #     sum((pgg[which(epos)]/sum(pgg[which(epos)]))*random_draws)
    #   })
    #
    #   selective.se <- stats::sd(selective.inf.func)
    #   selective.lci <- stats::quantile(selective.inf.func,alp/2)
    #   selective.uci <- stats::quantile(selective.inf.func,1-alp/2)
    # }
    #
    #
    # if(!is.na(selective.se)){
    #   if((selective.se <= sqrt(.Machine$double.eps)*10)) selective.se <- NA
    # }

    # Add the confidence intervals to component estimates
    simple.lci.i <- unlist(BMisc::getListElement(simple.se.inner, "lci"))
    simple.uci.i <- unlist(BMisc::getListElement(simple.se.inner, "uci"))

    return(AGGITEobj(overall.att=simple.att,
                     overall.se=simple.se,
                     overall.lci=simple.lci,
                     overall.uci=simple.uci,
                     type=type,
                     egt=idlist,
                     att.egt=simple.att.i,
                     se.egt=simple.se.i,
                     lci.egt=simple.lci.i,
                     uci.egt=simple.uci.i,
                     crit.val.egt=NULL,
                     inf.function = NULL,
                     call=call,
                     DIDparams=dp))

  }

  #-----------------------------------------------------------------------------
  # Compute the group (i.e., selective) treatment timing estimators
  #-----------------------------------------------------------------------------

  if (type == "group") {

    # change the pg to pi to extract the weights for the treated groups
    pg <- dta[dta[,idname] %in% idlist,".w"]

    # length of this is equal to number of treated units or number of units in idlist
    pgg <- pg

    # same but length is equal to the number of ATT(g,t)
    pg <- pg[match(id, idlist)]

    # get group specific ATTs
    # note: there are no estimated weights here
    selective.att.g <- sapply(glist, function(g) {
      # look at post-treatment periods for group g
      whichg <- which( (group == g) & (group <= t) & (t<= (group + max_e))) ### added last condition to allow for limit on longest period included in att
      attg <- att[whichg]
      if (length(whichg)<min_agg){
        NA
      } else {
        sum(attg*(pg[whichg]/sum(pg[whichg])))
      }
    })
    selective.att.g[is.nan(selective.att.g)] <- NA


    # get standard errors for each group specific ATT
    selective.se.inner <- lapply(glist, function(g) {
      whichg <- which( (group == g) & (group <= t) & (t<= (group + max_e)))  ### added last condition to allow for limit on longest period included in att
      if (length(whichg)<min_agg){
        inf.func.g <- NA
        se.g <- NA
        lci.g <- NA
        uci.g <- NA
      } else{
        inf.func.g <- replicate(biters, {
          # random_draws <- stats::rnorm(length(whichg), mean = att[whichg], sd = se[whichg])
          # random_draws <- random_draws + baseline[whichg] - unlist(lapply(unique(id[whichg]),function(j){
          #   whichj <- id[whichg]==j
          #   basedraw <- stats::rnorm(1,mean=baseline[whichg][whichj],sd=sdibase[whichg][whichj])
          #   return(rep(basedraw,sum(whichj)))
          #   }))
          # random_draws <- random_draws + stats::rnorm(length(whichg),sd=stats::sd(random_draws))
          # sum(random_draws*(pg[whichg]/sum(pg[whichg])))
          random_draws <- sapply(unique(id[whichg]), function(j){
            if (is.na(simple.se.i[which(idlist==j)])){
              return(simple.att.i[which(idlist==j)])
            } else{
              sd.impute = simple.se.i[which(idlist==j)]
            }
            return(stats::rnorm(1,mean = simple.att.i[which(idlist==j)],sd=sd.impute))
            })
          if (length(random_draws)>1){
            random_draws <- random_draws + stats::rnorm(length(unique(id[whichg])),sd = stats::sd(random_draws))
          } else{
            random_draws <- random_draws
          }
          sum((pgg[which(idlist %in% unique(id[whichg]))]/sum(pgg[which(idlist %in% unique(id[whichg]))]))*random_draws)
        })
        # se.e <- getSE(inf.func.e, dp)
        se.g <- stats::sd(inf.func.g)
        lci.g <- stats::quantile(inf.func.g,alp/2)
        uci.g <- stats::quantile(inf.func.g,1-alp/2)

      }
      list(inf.func=inf.func.g, se=se.g, lci=lci.g, uci=uci.g)
    })

    # recover standard errors separately by group
    selective.se.g <- unlist(BMisc::getListElement(selective.se.inner, "se"))
    selective.se.g[selective.se.g <= sqrt(.Machine$double.eps)*10] <- NA

    selective.lci.g <- unlist(BMisc::getListElement(selective.se.inner, "lci"))
    selective.uci.g <- unlist(BMisc::getListElement(selective.se.inner, "uci"))

    # recover influence function separately by group
    # selective.inf.func.g <- simplify2array(BMisc::getListElement(selective.se.inner, "inf.func"))

    #Not run since the aggregate comes from the simple

    # # add a positional argument to notate the missings
    # epos <- !is.na(selective.att.g)
    #
    # # get overall att under selective treatment timing
    # # (here use pgg instead of pg because we can just look at each group)
    # selective.att <- sum(selective.att.g[which(epos)] * (pgg[which(epos)]/sum(pgg[which(epos)])))
    #
    #
    # if (sum(epos)<2) {
    #   selective.inf.func <- NA
    #   selective.se <- NA
    #   selective.lci <- NA
    #   selective.uci <- NA
    # } else{
    #   selective.inf.func <- replicate(biters, {
    #     random_draws <- sapply(1:sum(epos), function(j) stats::rnorm(1, mean = selective.att.g[which(epos)][j], sd = selective.se.g[which(epos)][j]))
    #     random_draws <- random_draws + stats::rnorm(sum(epos),sd = stats::sd(random_draws))
    #     sum((pgg[which(epos)]/sum(pgg[which(epos)]))*random_draws)
    #   })
    #
    #   selective.se <- stats::sd(selective.inf.func)
    #   selective.lci <- stats::quantile(selective.inf.func,alp/2)
    #   selective.uci <- stats::quantile(selective.inf.func,1-alp/2)
    # }


    return(AGGITEobj(overall.att=simple.att,
                    overall.se=simple.se,
                    overall.lci = simple.lci,
                    overall.uci = simple.uci,
                    type=type,
                    egt=originalglist,
                    att.egt=selective.att.g,
                    se.egt=selective.se.g,
                    lci.egt = selective.lci.g,
                    uci.egt = selective.uci.g,
                    crit.val.egt=NULL,
                    inf.function = NULL,
                    call=call,
                    DIDparams=dp))

  }



  #-----------------------------------------------------------------------------
  # Compute the cohort level aggregates
  #-----------------------------------------------------------------------------

  if (type %in% c(customnames)) {

    # change the pg to pi to extract the weights for the treated groups
    pg <- dta[dta[,idname] %in% idlist,".w"]

    # length of this is equal to number of treated units or number of units in idlist
    pgg <- pg

    # same but length is equal to the number of ATT(g,t)
    pg <- pg[match(id, idlist)]

    # get group specific ATTs
    # note: there are no estimated weights here
    selective.att.c <- sapply(cohortlist, function(g) {
      # look at post-treatment periods for group g
      whichc <- which( (ccohort == g) & (group <= t) & (t<= (group + max_e))) ### added last condition to allow for limit on longest period included in att
      attc <- att[whichc]
      if (length(whichc)<min_agg){
        NA
      } else{
        sum(attc*(pg[whichc]/sum(pg[whichc])))
      }
    })
    selective.att.c[is.nan(selective.att.c)] <- NA


    # get standard errors for each group specific ATT
    selective.se.inner <- lapply(cohortlist, function(g) {
      whichc <- which( (ccohort == g) & (group <= t) & (t<= (group + max_e)))  ### added last condition to allow for limit on longest period included in att

      if (length(whichc)<min_agg){
        inf.func.c <- NA
        se.c <- NA
        lci.c <- NA
        uci.c <- NA
      } else{
        inf.func.c <- replicate(biters, {
          # random_draws <- sapply(1:length(whichc), function(j) stats::rnorm(1, mean = att[whichc][j], sd = se[whichc][j]))
          # random_draws <- random_draws + stats::rnorm(length(whichc),sd=stats::sd(random_draws))
          # sum(random_draws*(pg[whichc]/sum(pg[whichc])))
          random_draws <- sapply(unique(id[whichc]), function(j){
            if (is.na(simple.se.i[which(idlist==j)])){
              return(simple.att.i[which(idlist==j)])
            } else{
              sd.impute = simple.se.i[which(idlist==j)]
            }
            return(stats::rnorm(1,mean = simple.att.i[which(idlist==j)],sd=sd.impute))
          })
          if (length(random_draws)>1){
            random_draws <- random_draws + stats::rnorm(length(unique(id[whichc])),sd = stats::sd(random_draws))
          } else{
            random_draws <- random_draws
          }
          sum((pgg[which(idlist %in% unique(id[whichc]))]/sum(pgg[which(idlist %in% unique(id[whichc]))]))*random_draws)
        })
        # se.e <- getSE(inf.func.e, dp)
        se.c <- stats::sd(inf.func.c)
        lci.c <- stats::quantile(inf.func.c,alp/2)
        uci.c <- stats::quantile(inf.func.c,1-alp/2)

      }
      list(inf.func=inf.func.c, se=se.c, lci=lci.c, uci=uci.c)
    })

    # recover standard errors separately by group
    selective.se.c <- unlist(BMisc::getListElement(selective.se.inner, "se"))
    selective.se.c[selective.se.c <= sqrt(.Machine$double.eps)*10] <- NA

    selective.lci.c <- unlist(BMisc::getListElement(selective.se.inner, "lci"))
    selective.uci.c <- unlist(BMisc::getListElement(selective.se.inner, "uci"))

    # recover influence function separately by group
    selective.inf.func.c <- simplify2array(BMisc::getListElement(selective.se.inner, "inf.func"))


    # # positional argument to denote missings
    # epos <- !is.na(selective.att.c)
    #
    # # get overall att under selective treatment timing
    # # (here use pgg instead of pg because we can just look at each group)
    # selective.att <- sum(selective.att.c[which(epos)] * pgg[which(epos)])/sum(pgg[which(epos)])
    #
    #
    # if (sum(epos)<2) {
    #   selective.inf.func <- NA
    #   selective.se <- NA
    #   selective.lci <- NA
    #   selective.uci <- NA
    # } else{
    #   selective.inf.func <- replicate(biters, {
    #     random_draws <- sapply(1:sum(epos), function(j) stats::rnorm(1, mean = selective.att.c[which(epos)][j], sd = selective.se.c[which(epos)][j]))
    #     random_draws <- random_draws + stats::rnorm(sum(epos),sd = stats::sd(random_draws))
    #     sum((pgg[which(epos)]/sum(pgg[which(epos)]))*random_draws)
    #   })
    #
    #   selective.se <- stats::sd(selective.inf.func)
    #   selective.lci <- stats::quantile(selective.inf.func,alp/2)
    #   selective.uci <- stats::quantile(selective.inf.func,1-alp/2)
    # }
    #
    #
    # if(!is.na(selective.se)){
    #   if((selective.se <= sqrt(.Machine$double.eps)*10)) selective.se <- NA
    # }

    return(AGGITEobj(overall.att=simple.att,
                     overall.se=simple.se,
                     overall.lci=simple.lci,
                     overall.uci=simple.uci,
                     type=type,
                     egt=cohortlist,
                     att.egt=selective.att.c,
                     se.egt=selective.se.c,
                     lci.egt=selective.lci.c,
                     uci.egt=selective.uci.c,
                     crit.val.egt=NULL,
                     inf.function = NULL,
                     call=call,
                     DIDparams=dp))

  }


  #-----------------------------------------------------------------------------
  # Compute the event-study estimators
  #-----------------------------------------------------------------------------

  if (type == "dynamic") {

    # change the pg to pi to extract the weights for the treated groups
    pg <- dta[dta[,idname] %in% idlist,".w"]

    # length of this is equal to number of treated units or number of units in idlist
    pgg <- pg

    # same but length is equal to the number of ATT(g,t)
    pg <- pg[match(id, idlist)]

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

    # compute atts that are specific to each event time
    dynamic.att.e <- sapply(eseq, function(e) {
      # keep att(i,t) for the right i&t and
      # ones that are not trimmed out from balancing the sample and
      whiche <- which( (originalt - originalgroup == e) & (include.balanced.gt) )
      atte <- att[whiche]
      pge <- pg[whiche]/(sum(pg[whiche]))
      # ones that aggregate over fewer than min_agg outcomes should be set to missing
      if (length(whiche)<min_agg){
        NA
      } else{
        sum(atte*pge)
      }
    })

    # compute standard errors for dynamic effects
    dynamic.se.inner <- lapply(eseq, function(e) {
      whiche <- which( (originalt - originalgroup == e) & (include.balanced.gt) )
      pge <- pg[whiche]/(sum(pg[whiche]))

      if (length(whiche)<min_agg){
        inf.func.e <- NA
        se.e <- NA
        lci.e <- NA
        uci.e <- NA
      } else{
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

    dynamic.se.e <- unlist(BMisc::getListElement(dynamic.se.inner, "se"))
    dynamic.se.e[dynamic.se.e <= sqrt(.Machine$double.eps)*10] <- NA

    dynamic.lci.e <- unlist(BMisc::getListElement(dynamic.se.inner, "lci"))
    dynamic.uci.e <- unlist(BMisc::getListElement(dynamic.se.inner, "uci"))

    dynamic.inf.func.e <- simplify2array(BMisc::getListElement(dynamic.se.inner, "inf.func"))

    # # get overall average treatment effect
    # # by averaging over positive dynamics
    # epos <- (eseq >= 0 & !is.na(dynamic.att.e))
    #
    # # recalculate the weights
    # pgg <- sapply(eseq[which(epos)], function(e) sum( ((originalt - originalgroup == e) & (include.balanced.gt)) * pg))
    #
    # dynamic.att <- sum(pgg*dynamic.att.e[which(epos)])/sum(pgg)
    #
    # if (sum(epos)<2) {
    #   dynamic.inf.func <- NA
    #   dynamic.se <- NA
    #   dynamic.lci <- NA
    #   dynamic.uci <- NA
    # } else{
    #   dynamic.inf.func <- replicate(biters, {
    #     random_draws <- sapply(1:sum(epos), function(j) stats::rnorm(1, mean = dynamic.att.e[which(epos)][j], sd = dynamic.se.e[which(epos)][j]))
    #     sd.e <- stats::sd(random_draws)/sqrt(sum(epos))
    #     sum((pgg/sum(pgg))*random_draws) + stats::rnorm(1,sd=sd.e)
    #   })
    #
    #   dynamic.se <- stats::sd(dynamic.inf.func)
    #   dynamic.lci <- stats::quantile(dynamic.inf.func,alp/2)
    #   dynamic.uci <- stats::quantile(dynamic.inf.func,1-alp/2)
    # }
    #

    return(AGGITEobj(overall.att=simple.att,
                    overall.se=simple.se,
                    overall.lci = simple.lci,
                    overall.uci = simple.uci,
                    type=type,
                    egt=eseq,
                    att.egt=dynamic.att.e,
                    se.egt=dynamic.se.e,
                    lci.egt = dynamic.lci.e,
                    uci.egt = dynamic.uci.e,
                    crit.val.egt= NULL,
                    inf.function = NULL,
                    call=call,
                    min_e=min_e,
                    max_e=max_e,
                    balance_e=balance_e,
                    min_agg = min_agg,
                    DIDparams=dp
    ))
  }

  #-----------------------------------------------------------------------------
  # calendar time effects
  #-----------------------------------------------------------------------------

  if (type == "calendar") {


    # drop time periods where no one is treated yet
    # (can't get treatment effects in those periods)
    minG <- min(group)
    calendar.tlist <- tlist[tlist>=minG]

    # change the pg to pi to extract the weights for the treated times
    pg <- dta[dta[,idname] %in% idlist,".w"]

    # length of this is equal to number of treated units or number of units in idlist
    pgg <- pg

    # same but length is equal to the number of ATT(g,t)
    pg <- pg[match(id, idlist)]

    # calendar time specific atts
    calendar.att.t <- sapply(calendar.tlist, function(t1) {
      # look at post-treatment periods for group g
      whicht <- which( (t == t1) & (group <= t))
      attt <- att[whicht]
      pgt <- pg[whicht]/(sum(pg[whicht]))
      if (length(whicht)<min_agg){
        NA
      } else{
        sum(pgt * attt)
      }
    })

    # get standard errors and influence functions
    # for each time specific att
    calendar.se.inner <- lapply(calendar.tlist, function(t1) {
      whicht <- which( (t == t1) & (group <= t))
      pgt <- pg[whicht]/(sum(pg[whicht]))


      if (length(whicht)<min_agg){
        inf.func.t <- NA
        se.t <- NA
        lci.t <- NA
        uci.t <- NA
      } else{
        inf.func.t <- replicate(biters, {
          random_draws <- sapply(1:length(whicht), function(j) stats::rnorm(1, mean = att[whicht][j], sd = se[whicht][j]))
          sd.e <- stats::sd(random_draws)/sqrt(length(att[whicht]))
          sum(random_draws*pgt) + stats::rnorm(1,sd=sd.e)
        })

        se.t <- stats::sd(inf.func.t)
        lci.t <- stats::quantile(inf.func.t,alp/2)
        uci.t <- stats::quantile(inf.func.t,1-alp/2)

      }
      list(inf.func=inf.func.t, se=se.t, lci=lci.t, uci=uci.t)


    })

    # recover standard errors separately by time
    calendar.se.t <- unlist(BMisc::getListElement(calendar.se.inner, "se"))
    calendar.se.t[calendar.se.t <= sqrt(.Machine$double.eps)*10] <- NA

    calendar.lci.t <- unlist(BMisc::getListElement(calendar.se.inner, "lci"))

    calendar.uci.t <- unlist(BMisc::getListElement(calendar.se.inner, "uci"))


    # recover influence function separately by time
    calendar.inf.func.t <- simplify2array(BMisc::getListElement(calendar.se.inner, "inf.func"))

    # # index for non-missing
    # epos <- !is.na(calendar.att.t)
    #
    # pgg <- sapply(calendar.tlist, function(t1) sum(((t == t1) & (group <= t)) * pg))
    #
    # # get overall att under calendar time effects
    # # this is just average over all time periods
    # calendar.att <- sum(calendar.att.t[which(epos)]*pgg[which(epos)])/sum(pgg[which(epos)])
    #
    # if (sum(epos)<2) {
    #   calendar.inf.func <- NA
    #   calendar.se <- NA
    #   calendar.lci <- NA
    #   calendar.uci <- NA
    # } else{
    #   calendar.inf.func <- replicate(biters, {
    #     random_draws <- sapply(1:sum(epos), function(j) stats::rnorm(1, mean = calendar.att.t[which(epos)][j], sd = calendar.se.t[which(epos)][j]))
    #     sd.e <- stats::sd(random_draws)/sqrt(length(random_draws))
    #     sum((pgg[which(epos)]/sum(pgg[which(epos)]))*random_draws) + stats::rnorm(1,sd=sd.e)
    #   })
    #
    #   calendar.se <- stats::sd(calendar.inf.func)
    #   calendar.lci <- stats::quantile(calendar.inf.func,alp/2)
    #   calendar.uci <- stats::quantile(calendar.inf.func,1-alp/2)
    # }
    #
    #
    # # get overall standard error
    # if(!is.na(calendar.se)){
    #   if (calendar.se <= sqrt(.Machine$double.eps)*10) calendar.se <- NA
    # }

    return(AGGITEobj(overall.att=simple.att,
                    overall.se=simple.se,
                    overall.lci = simple.lci,
                    overall.uci = simple.uci,
                    type=type,
                    egt=sapply(calendar.tlist,t2orig),
                    att.egt=calendar.att.t,
                    se.egt=calendar.se.t,
                    lci.egt=calendar.lci.t,
                    uci.egt=calendar.uci.t,
                    crit.val.egt=NULL,
                    inf.function = NULL,
                    call=call,
                    DIDparams=dp
    ))

  }


}

