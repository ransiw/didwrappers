#' @title Compute Pair Aggregated Treatment Effect Parameters
#'
#' @description For computing pair aggregated group-time
#'  average treatment effects
#'
#' @inheritParams aggite
#' @param call The function call to aggte
#'
#' @return [`AGGITEobj`] object
#'
#' @keywords internal
#'
#' @export
compute.aggite2 <- function(MP,
                           type = "group",
                           type2 = "dynamic",
                           balance_e = NULL,
                           min_e = -Inf,
                           max_e = Inf,
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
  dp <- MP$DIDparams
  inffunc1 <- MP$inffunc
  n <- MP$n


  gname <- dp$gname
  data <- as.data.frame(dp$data)
  tname <- dp$tname
  idname <- dp$idname
  cohortnames <- dp$cohortnames


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

  if(!(type %in% c("group", cohortnames))) {
    stop('`type` must be one of c("group" or a custom cohort)')
  }

  if(!(type2 %in% c("dynamic"))) {
    stop('`type2` must "dynamic')
  }

  if(na.rm){
    notna <- !is.na(att)
    group <- group[notna]
    t <- t[notna]
    id <- id[notna]
    att <- att[notna]
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
      inffunc1 <- inffunc1[, not_all_na]
      #tlist <- sort(unique(t))
      glist <- sort(unique(group))
    }

    if(type %in% c(cohortnames)){
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
      inffunc1 <- inffunc1[, not_all_na]
      #tlist <- sort(unique(t))
      # redoing the glist here to drop any NA observations
      glist <- unique(data.frame(id,group))$group
      idlist <- sort(unique(id))
    }
  }


  if((na.rm == FALSE) && base::anyNA(att)) stop("Missing values at att_gt found. If you want to remove these, set `na.rm = TRUE'.")


  # if the type is a cohort, create cohort variable of the size of ATT(g,t) cohortlist and check that each unit is uniquely mapped to a cohort
  if (type %in% cohortnames){
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
    cohort = idcohortatt[,type]
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

  # since estimates are at the unit-time level, aggregation into simple does not require reweighting to group size
  pg <- sapply(idlist, function(g) mean(weights.ind*(dta[,idname]==g)))

  # length of this is equal to number of groups
  pgg <- pg

  # same but length is equal to the number of ATT(g,t)
  pg <- pg[match(group, glist)]


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
          if (length(whiche)==0){
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
          if (length(whiche)==0){
            inf.func.e = rep(0,dim(inffunc1)[1])
            se.e <- 0
          } else{
            pge <- pg[whiche]/(sum(pg[whiche]))
            wif.e <- wif(whiche, pg, weights.ind, G, group)
            inf.func.e <- as.numeric(get_agg_inf_func(att=att,
                                                      inffunc1=inffunc1,
                                                      whichones=whiche,
                                                      weights.agg=pge,
                                                      wif=wif.e))
            se.e <- getSE(inf.func.e, dp)
          }
          list(inf.func=inf.func.e, se=se.e)
        })
      })

      dynamic.se.inner = do.call(c,dynamic.se.inner)

      dynamic.se.e <- unlist(BMisc::getListElement(dynamic.se.inner, "se"))
      dynamic.se.e[dynamic.se.e <= sqrt(.Machine$double.eps)*10] <- NA

      dynamic.inf.func.e <- simplify2array(BMisc::getListElement(dynamic.se.inner, "inf.func"))

      dynamic.crit.val <- stats::qnorm(1 - alp/2)
      if(dp$cband==TRUE){
        if(dp$bstrap == FALSE){
          warning('Used bootstrap procedure to compute simultaneous confidence band')
        }
        dynamic.crit.val <- did::mboot(dynamic.inf.func.e, dp)$crit.val

        if(is.na(dynamic.crit.val) | is.infinite(dynamic.crit.val)){
          warning('Simultaneous critival value is NA. This probably happened because we cannot compute t-statistic (std errors are NA). We then report pointwise conf. intervals.')
          dynamic.crit.val <- stats::qnorm(1 - alp/2)
          dp$cband <- FALSE
        }

        if(dynamic.crit.val < stats::qnorm(1 - alp/2)){
          warning('Simultaneous conf. band is somehow smaller than pointwise one using normal approximation. Since this is unusual, we are reporting pointwise confidence intervals')
          dynamic.crit.val <- stats::qnorm(1 - alp/2)
          dp$cband <- FALSE
        }

        if(dynamic.crit.val >= 7){
          warning("Simultaneous critical value is arguably `too large' to be realible. This usually happens when number of observations per group is small and/or there is no much variation in outcomes.")
        }
      }

      # get overall average treatment effect
      # by averaging over positive dynamics
      epos <- (unlist(BMisc::getListElement(egtlist, "egt2")) >= 0 & !is.na(dynamic.att.e))

      dynamic.att <- mean(dynamic.att.e[epos])
      dynamic.inf.func <- get_agg_inf_func(att=dynamic.att.e[epos],
                                           inffunc1=as.matrix(dynamic.inf.func.e[,epos]),
                                           whichones=(1:sum(epos)),
                                           weights.agg=(rep(1/sum(epos), sum(epos))),
                                           wif=NULL)

      dynamic.inf.func <- as.numeric(dynamic.inf.func)
      dynamic.se <- getSE(dynamic.inf.func, dp)
      if(!is.na(dynamic.se)){
        if (dynamic.se <= sqrt(.Machine$double.eps)*10) dynamic.se <- NA
      }

      return(AGGITEobj(overall.att=dynamic.att,
                       overall.se=dynamic.se,
                       type=type,
                       type2=type2,
                       egt= unlist(BMisc::getListElement(egtlist, "egt")),
                       egt2 = unlist(BMisc::getListElement(egtlist, "egt2")),
                       att.egt=dynamic.att.e,
                       se.egt=dynamic.se.e,
                       crit.val.egt=dynamic.crit.val,
                       inf.function = list(dynamic.inf.func.e = dynamic.inf.func.e,
                                           dynamic.inf.func = dynamic.inf.func),
                       call=call,
                       min_e=min_e,
                       max_e=max_e,
                       balance_e=balance_e,
                       DIDparams=dp
      ))
    }

    if (type %in% cohortnames){

      # we can work in overall probabilities because conditioning will cancel out
      # cause it shows up in numerator and denominator
      pg <- sapply(cohortlist, function(g) mean(weights.ind*(dta[,type]==g)))

      # length of this is equal to number of groups
      pgg <- pg

      # same but length is equal to the number of ATT(g,t)
      pg <- pg[match(cohort, cohortlist)]

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
          whiche <- which( (cohort == g) & (originalt - originalgroup == e) & (include.balanced.gt) )
          if (length(whiche)==0){
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
          whiche <- which( (cohort == g) & (originalt - originalgroup == e) & (include.balanced.gt) )
          if (length(whiche)==0){
            inf.func.e = rep(0,dim(inffunc1)[1])
            se.e <- 0
          } else{
            pge <- pg[whiche]/(sum(pg[whiche]))
            wif.e <- wif(whiche, pg, weights.ind, G, group)
            inf.func.e <- as.numeric(get_agg_inf_func(att=att,
                                                      inffunc1=inffunc1,
                                                      whichones=whiche,
                                                      weights.agg=pge,
                                                      wif=wif.e))
            se.e <- getSE(inf.func.e, dp)
          }
          list(inf.func=inf.func.e, se=se.e)
        })
      })

      dynamic.se.inner = do.call(c,dynamic.se.inner)

      dynamic.se.e <- unlist(BMisc::getListElement(dynamic.se.inner, "se"))
      dynamic.se.e[dynamic.se.e <= sqrt(.Machine$double.eps)*10] <- NA

      dynamic.inf.func.e <- simplify2array(BMisc::getListElement(dynamic.se.inner, "inf.func"))

      dynamic.crit.val <- stats::qnorm(1 - alp/2)
      if(dp$cband==TRUE){
        if(dp$bstrap == FALSE){
          warning('Used bootstrap procedure to compute simultaneous confidence band')
        }
        dynamic.crit.val <- did::mboot(dynamic.inf.func.e, dp)$crit.val

        if(is.na(dynamic.crit.val) | is.infinite(dynamic.crit.val)){
          warning('Simultaneous critival value is NA. This probably happened because we cannot compute t-statistic (std errors are NA). We then report pointwise conf. intervals.')
          dynamic.crit.val <- stats::qnorm(1 - alp/2)
          dp$cband <- FALSE
        }

        if(dynamic.crit.val < stats::qnorm(1 - alp/2)){
          warning('Simultaneous conf. band is somehow smaller than pointwise one using normal approximation. Since this is unusual, we are reporting pointwise confidence intervals')
          dynamic.crit.val <- stats::qnorm(1 - alp/2)
          dp$cband <- FALSE
        }

        if(dynamic.crit.val >= 7){
          warning("Simultaneous critical value is arguably `too large' to be realible. This usually happens when number of observations per group is small and/or there is no much variation in outcomes.")
        }
      }

      # get overall average treatment effect
      # by averaging over positive dynamics
      epos <- (unlist(BMisc::getListElement(egtlist, "egt2")) >= 0 & !is.na(dynamic.att.e))

      dynamic.att <- mean(dynamic.att.e[epos])
      dynamic.inf.func <- get_agg_inf_func(att=dynamic.att.e[epos],
                                           inffunc1=as.matrix(dynamic.inf.func.e[,epos]),
                                           whichones=(1:sum(epos)),
                                           weights.agg=(rep(1/sum(epos), sum(epos))),
                                           wif=NULL)

      dynamic.inf.func <- as.numeric(dynamic.inf.func)
      dynamic.se <- getSE(dynamic.inf.func, dp)
      if(!is.na(dynamic.se)){
        if (dynamic.se <= sqrt(.Machine$double.eps)*10) dynamic.se <- NA
      }

      return(AGGITEobj(overall.att=dynamic.att,
                       overall.se=dynamic.se,
                       type=type,
                       type2=type2,
                       egt= unlist(BMisc::getListElement(egtlist, "egt")),
                       egt2 = unlist(BMisc::getListElement(egtlist, "egt2")),
                       att.egt=dynamic.att.e,
                       se.egt=dynamic.se.e,
                       crit.val.egt=dynamic.crit.val,
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

#-----------------------------------------------------------------------------
# Internal functions for getting standard errors
#-----------------------------------------------------------------------------

#' @title Compute extra term in influence function due to estimating weights
#'
#' @description A function to compute the extra term that shows up in the
#'  influence function for aggregated treatment effect parameters
#'  due to estimating the weights
#'
#' @param keepers a vector of indices for which group-time average
#'  treatment effects are used to compute a particular aggregated parameter
#' @param pg a vector with same length as total number of group-time average
#'  treatment effects that contains the probability of being in particular group
#' @param weights.ind additional sampling weights (nx1)
#' @param G vector containing which group a unit belongs to (nx1)
#' @param group vector of groups
#'
#' @return nxk influence function matrix
#'
#' @keywords internal
wif <- function(keepers, pg, weights.ind, G, group) {
  # note: weights are all of the form P(G=g|cond)/sum_cond(P(G=g|cond))
  # this is equal to P(G=g)/sum_cond(P(G=g)) which simplifies things here

  # effect of estimating weights in the numerator
  if1 <- sapply(keepers, function(k) {
    (weights.ind * 1*BMisc::TorF(G==group[k]) - pg[k]) /
      sum(pg[keepers])
  })
  # effect of estimating weights in the denominator
  if2 <- base::rowSums( sapply( keepers, function(k) {
    weights.ind*1*BMisc::TorF(G==group[k]) - pg[k]
  })) %*%
    t(pg[keepers]/(sum(pg[keepers])^2))

  # return the influence function for the weights
  if1 - if2
}


#' @title Get an influence function for particular aggregate parameters
#'
#' @title This is a generic internal function for combining influence
#'  functions across ATT(g,t)'s to return an influence function for
#'  various aggregated treatment effect parameters.
#'
#' @param att vector of group-time average treatment effects
#' @param inffunc1 influence function for all group-time average treatment effects
#'  (matrix)
#' @param whichones which elements of att will be used to compute the aggregated
#'  treatment effect parameter
#' @param weights.agg the weights to apply to each element of att(whichones);
#'  should have the same dimension as att(whichones)
#' @param wif extra influence function term coming from estimating the weights;
#'  should be n x k matrix where k is dimension of whichones
#'
#' @return nx1 influence function
#'
#' @keywords internal
get_agg_inf_func <- function(att, inffunc1, whichones, weights.agg, wif=NULL) {
  # enforce weights are in matrix form
  weights.agg <- as.matrix(weights.agg)

  # multiplies influence function times weights and sums to get vector of weighted IF (of length n)
  thisinffunc <- inffunc1[,whichones]%*%weights.agg

  # Incorporate influence function of the weights
  if (!is.null(wif)) {
    thisinffunc <- thisinffunc + wif%*%as.matrix(att[whichones])
  }

  # return influence function
  return(thisinffunc)
}


#' @title Take influence function and return standard errors
#'
#' @description Function to take an nx1 influence function and return
#'  a standard error
#'
#' @param thisinffunc An influence function
#' @inheritParams compute.aggite
#'
#' @return scalar standard error
#'
#' @keywords internal
getSE <- function(thisinffunc, DIDparams=NULL) {
  alp <- .05
  bstrap <- FALSE
  if (!is.null(DIDparams)) {
    bstrap <- DIDparams$bstrap
    alp <- DIDparams$alp
    cband <- DIDparams$cband
    n <- length(thisinffunc)
  }

  if (bstrap) {
    bout <- did::mboot(thisinffunc, DIDparams)
    return(bout$se)
  } else {
    return(sqrt( mean((thisinffunc)^2)/n ))
  }
}
