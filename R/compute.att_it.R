#' @title Compute Group-Time Average Treatment Effects
#'
#' @description
#' `compute.att_it` does the main work for computing
#'  multiperiod group-time average treatment effects while balancing for the effects at each time period
#'
#'
#' @param dp a DIDparams_i object
#'
#' @return a list object with the calculated att_it() and corresponding influence functions
#' @export
#'
#' @examples
#' # This is a helper function for [att_it()], but useful for debugging if problems occur.
#' # See documentation of that function for examples.
#'
compute.att_it <- function(dp) {

  #-----------------------------------------------------------------------------
  # unpack DIDparams_i
  #-----------------------------------------------------------------------------
  data <- as.data.frame(dp$data)
  yname <- dp$yname
  tname <- dp$tname
  idname <- dp$idname
  xformla <- dp$xformla
  weightsname <- dp$weightsname
  weightfs <- dp$weightfs
  est_method <- dp$est_method
  overlap <- dp$overlap
  base_period <- dp$base_period
  panel <- dp$panel
  print_details <- dp$print_details
  control_group <- dp$control_group
  anticipation <- dp$anticipation
  gname <- dp$gname
  n  <- dp$n
  nT <- dp$nT
  nG <- dp$nG
  tlist <- dp$tlist
  glist <- dp$glist
  idlist <- dp$idlist

  #-----------------------------------------------------------------------------
  # main computations
  #-----------------------------------------------------------------------------

  # will populate with all att(g,t)
  attgt.list <- list()

  # place holder in lists
  counter <- 1

  # number of time periods
  tlist.length <- length(tlist)
  tfac <- 0

  if (base_period != "universal") {
    tlist.length <- tlist.length - 1
    tfac <- 1
  }

  # influence function
  inffunc <- Matrix::Matrix(data=0,nrow=n, ncol=nG*(nT-tfac), sparse=TRUE)

  # never treated option
  nevertreated <- (control_group[1] == "nevertreated")

  if(nevertreated) {
    data$.C <- 1*(data[,gname] == 0)
  }

  # rename yname to .y
  data$.y <- data[,yname]

  # determining if the weights are turned on or off
  if (weightfs==FALSE){
    data$.w <- rep(1, nrow(data))
  }


  # Create a dataframe with id and gname
  dataids = data.frame(ids=idlist,
                       gs = glist)
  colnames(dataids) = c(idname,gname)

  # loop over groups
  for (g in 1:nG) {

    # Set up .G once
    data$.G <- 1*(data[,idname] == idlist[g])

    # loop over time periods
    for (t in 1:tlist.length) {

      #-----------------------------------------------------------------------------
      # Set pret

      # varying base period
      pret <- t

      # universal base period
      if (base_period == "universal") {
        # use same base period as for post-treatment periods
        pret <- utils::tail(which( (tlist+anticipation) < glist[g]),1)
      }

      # use "not yet treated as control"
      # that is, never treated + units that are eventually treated,
      # but not treated by the current period (+ anticipation)
      if(!nevertreated) {
        data$.C <- 1 * ((data[,gname] == 0) |
                            ((data[,gname] > (tlist[max(t,pret)+tfac]+anticipation)) &
                               (data[,gname] != glist[g])))
      }


      # check if in post-treatment period
      if ((glist[g]<=tlist[(t+tfac)])) {

        # update pre-period if in post-treatment period to
        # be  period (g-delta-1)
        pret <- utils::tail(which( (tlist+anticipation) < glist[g]),1)

        # print a warning message if there are no pre-treatment period
        if (length(pret) == 0) {
          warning(paste0("There are no pre-treatment periods for the idgroup first treated at ", idlist[g], "\nUnits from this group are dropped"))

          # if there are not pre-treatment periods, code will
          # jump out of this loop
          break
        }
      }


      #-----------------------------------------------------------------------------
      # if we are in period (g-1), normalize results to be equal to 0
      # and break without computing anything
      if (base_period == "universal") {
        if (tlist[pret] == tlist[(t+tfac)]) {
          attgt.list[[counter]] <- list(att=0, id=idlist[g], group=glist[g], year=tlist[(t+tfac)],ipwqual=NA,attcalc=NA, post=0, count=0)
          inffunc[,counter] <- rep(0,n)
          counter <- counter+1
          next
        }
      }

      # print the details of which iteration we are on
      if (print_details) {
        cat(paste("current period:", tlist[(t+tfac)]), "\n")
        cat(paste("current idgroup:", idlist[g]), "\n")
        cat(paste("current group:", glist[g]), "\n")
        cat(paste("set pretreatment period to be", tlist[pret]), "\n")
      }


      # post treatment dummy variable
      post.treat <- 1*(glist[g] <= tlist[t+tfac])

      # total number of units (not just included in G or C)
      disdat <- data[data[,tname] == tlist[t+tfac] | data[,tname] == tlist[pret],]


      n0 <- nrow(disdat)

      # pick up the indices for units that will be used to compute ATT(g,t)
      # these conditions are (1) you are observed in the right period and
      # (2) you are in the right group (it is possible to be observed in
      # the right period but still not be part of the treated or control
      # group in that period here
      rightids <- disdat[,idname][ disdat$.G==1 | disdat$.C==1]

      # rightids should be observed pre-treatment
      table_rightids <- table(rightids)

      rightids <- rightids[rightids %in% names(table_rightids[table_rightids==2])]


      # this is the fix for unbalanced panels; 2nd criteria shouldn't do anything
      # with true repeated cross sections, but should pick up the right time periods
      # only with unbalanced panel
      disidx <- (data[,idname] %in% rightids) & ( (data[,tname] == tlist[t+tfac]) | (data[,tname]==tlist[pret]))

      # pick up the data that will be used to compute ATT(g,t)
      disdat <- data[disidx,]

      # drop missing factors
      disdat <- droplevels(disdat)

      # give short names for data in this iteration
      G <- disdat$.G
      C <- disdat$.C
      Y <- disdat[,yname]
      post <- 1*(disdat[,tname] == tlist[t+tfac])
      # num obs. for computing ATT(g,t), have to be careful here
      n1 <- sum(G+C)
      w <- disdat$.w

      #-----------------------------------------------------------------------------
      # checks to make sure that we have enough observations
      skip_this_att_gt <- FALSE
      if ( sum(G*post) == 0 ) {
        message(paste0("No units for id ", idlist[g], " in time period ", tlist[t+tfac]))
        skip_this_att_gt <- TRUE
      }
      if ( sum(G*(1-post)) == 0) {
        message(paste0("No units for id ", idlist[g], " in time period ", tlist[t]))
        skip_this_att_gt <- TRUE
      }
      if (sum(C*post) == 0) {
        message(paste0("No available control units for id ", idlist[g], " in time period ", tlist[t+tfac]))
        skip_this_att_gt <- TRUE
      }
      if (sum(C*(1-post)) == 0) {
        message(paste0("No availabe control units for group ", idlist[g], " in time period ", tlist[t]))
        skip_this_att_gt <- TRUE
      }

      if (skip_this_att_gt) {
        attgt.list[[counter]] <- list(att=NA, id=idlist[g],  group=glist[g], year=tlist[(t+tfac)], ipwqual=NA, post=post.treat, attcalc = NA, count=0)
        inffunc[,counter] <- NA
        counter <- counter+1
        next
      }

      # Now force to a panel because it balances both sides
      disdat <- BMisc::panel2cs2(disdat, yname, idname, tname, balance_panel=FALSE)
      # drop missing factors
      disdat <- droplevels(disdat)


      # save the indices for the inffunc
      disidx <- data.frame(id = data[,idname],tt = data[,tname],idx = disidx)
      disidx <- disidx[disidx$tt == tlist[t+tfac],]
      allids <- data.frame(id=sort(unique(data[,idname])))
      allids <- merge(allids,disidx,by="id",all.x=TRUE,sort=TRUE)
      allids$idx[is.na(allids$idx)] <- FALSE
      disidx <- allids$idx

      # sort disdat by idname too
      disdat <- disdat[order(with(disdat, get(idname))), ]

      # give short names for data in this iteration
      G <- disdat$.G
      C <- disdat$.C

      # handle pre-treatment universal base period differently
      # we need to do this because panel2cs2 just puts later period
      # in .y1, but if we are in a pre-treatment period with a universal
      # base period, then the "base period" is actually the later period
      Ypre <- if(tlist[(t+tfac)] > tlist[pret]) disdat$.y0 else disdat$.y1
      Ypost <- if(tlist[(t+tfac)] > tlist[pret]) disdat$.y1 else disdat$.y0
      w <- disdat$.w


      # matrix of covariates
      covariates <- stats::model.matrix(xformla, data=disdat)

      # if using custom estimation method, skip this part
      custom_est_method <- class(est_method) == "function"
      maxpscore = NA

      if (!custom_est_method) {
        pscore_problems_likely <- FALSE
        reg_problems_likely <- FALSE

        # checks for pscore based methods
        if (est_method %in% c("dr", "ipw")) {
          preliminary_logit <- stats::glm(G ~ covariates, family=stats::binomial(link="logit"))
          preliminary_pscores <- stats::predict(preliminary_logit, type="response")
          if (max(preliminary_pscores) >= 0.999) {
            pscore_problems_likely <- TRUE
            warning(paste0("overlap condition violated for ", glist[g], " in time period ", tlist[t+tfac]))
          }
          maxpscore = max(preliminary_pscores)
        }

        # check if can run regression using control units
        if (est_method %in% c("dr", "reg")) {
          control_covs <- covariates[G==0,,drop=FALSE]
          #if (determinant(t(control_covs)%*%control_covs, logarithm=FALSE)$modulus < .Machine$double.eps) {
          if ( rcond(t(control_covs)%*%control_covs) < .Machine$double.eps) {
            reg_problems_likely <- TRUE
            warning(paste0("Not enough control units for group ", glist[g], " in time period ", tlist[t+tfac], " to run specified regression"))
          }
        }

        if (est_method %in% c("ipw")) {
          attgt <- DRDID::std_ipw_did_panel(Ypost, Ypre, G,
                                            covariates=covariates,
                                            i.weights=w,
                                            boot=FALSE, inffunc=FALSE)
          attcalc = attgt$ATT
          if(is.nan(attgt$ATT)){
            attcalc = NA
          }
        }

        if (est_method %in% c("dr")) {
          attgt <- DRDID::drdid_panel(Ypost, Ypre, G,
                                      covariates=covariates,
                                      i.weights=w,
                                      boot=FALSE, inffunc=FALSE)
          attcalc = attgt$ATT
          if(is.nan(attgt$ATT)){
            attcalc = NA
          }
        }

        if (est_method %in% c("reg")) {
          attgt <- DRDID::reg_did_panel(Ypost, Ypre, G,
                                        covariates=covariates,
                                        i.weights=w,
                                        boot=FALSE, inffunc=FALSE)
          attcalc = attgt$ATT
          if(is.nan(attgt$ATT)){
            attcalc = NA
          }
        }


        if (reg_problems_likely | (pscore_problems_likely & overlap=="trim")) {
          attgt.list[[counter]] <- list(att=NA, id=idlist[g], group=glist[g], year=tlist[(t+tfac)],ipwqual=maxpscore, post=post.treat, attcalc=attcalc, count=n1/2)
          inffunc[,counter] <- NA
          counter <- counter+1
          next
        }
      }


      #-----------------------------------------------------------------------------
      # code for actually computing att(g,t)
      #-----------------------------------------------------------------------------

      if (inherits(est_method,"function")) {
        # user-specified function
        attgt <- est_method(y1=Ypost, y0=Ypre,
                            D=G,
                            covariates=covariates,
                            i.weights=w,
                            inffunc=TRUE)
      } else if (est_method == "ipw") {
        # inverse-probability weights
        attgt <- DRDID::std_ipw_did_panel(Ypost, Ypre, G,
                                          covariates=covariates,
                                          i.weights=w,
                                          boot=FALSE, inffunc=TRUE)
      } else if (est_method == "reg") {
        # regression
        attgt <- DRDID::reg_did_panel(Ypost, Ypre, G,
                                      covariates=covariates,
                                      i.weights=w,
                                      boot=FALSE, inffunc=TRUE)
      } else {
        # doubly robust, this is default
        attgt <- DRDID::drdid_panel(Ypost, Ypre, G,
                                    covariates=covariates,
                                    i.weights=w,
                                    boot=FALSE, inffunc=TRUE)
      }

      # adjust influence function to account for only using
      # subgroup to estimate att(g,t)
      attgt$att.inf.func <- (n0/n1)*attgt$att.inf.func

      # If ATT is NaN, replace it with NA, and make Influence functions equal to zero
      if(is.nan(attgt$ATT)){
        attgt$ATT <- NA
        attgt$att.inf.func <- 0 * attgt$att.inf.func
      }


      attgt.list[[counter]] <- list(
        att = attgt$ATT, id=idlist[g],  group = glist[g], year = tlist[(t+tfac)],ipwqual=maxpscore, post = post.treat,
        attcalc = attgt$ATT, count=n1/2
      )


      # recover the influence function
      # start with vector of 0s because influence function
      # for units that are not in G or C will be equal to 0
      inf.func <- rep(0, dp$n)

      # populate the influence function in the right places
      if (length(disidx)!=length(inf.func)){
        stop("The indexing vector is a different size to the number of cross-sectional units")
      }

      inf.func[disidx] <- attgt$att.inf.func


      # save it in influence function matrix
      # inffunc[g,t,] <- inf.func
      inffunc[,counter] <- inf.func

      # update counter
      counter <- counter+1
    } # end looping over t
  } # end looping over g

  return(list(attgt.list=attgt.list, inffunc=inffunc))
}
