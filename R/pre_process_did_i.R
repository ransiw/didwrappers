#' Pre-processes for the DiD calculation
#'
#' @description
#' Pre-processes data within the `att_it` function before `compute.att_it`
#'
#'
#' @inheritParams att_it
#' @param call Function call to att_it
#'
#' @return returns an object DIDparams_i
#' @export
#'
pre_process_did_i <- function(yname,
                             tname,
                             idname,
                             gname,
                             cohortnames = NULL,
                             xformla = NULL,
                             data,
                             panel = TRUE,
                             control_group = c("nevertreated","notyettreated"),
                             anticipation = 0,
                             weightsname = NULL,
                             alp = 0.05,
                             bstrap = FALSE,
                             cband = FALSE,
                             biters = 1000,
                             clustervars = NULL,
                             est_method = "dr",
                             overlap="trim",
                             base_period = "varying",
                             print_details = TRUE,
                             pl = FALSE,
                             cores = 1,
                             call = NULL) {
  #-----------------------------------------------------------------------------
  # Data pre-processing and error checking
  #-----------------------------------------------------------------------------
  # set control group
  control_group <- control_group[1]
  if(!(control_group %in% c("nevertreated","notyettreated"))){
    stop("control_group must be either 'nevertreated' or 'notyettreated'")
  }
  # make sure dataset is a data.frame
  # this gets around RStudio's default of reading data as tibble
  if (!all( class(data) == "data.frame")) {
    data <- as.data.frame(data)
  }

  # make sure time periods are numeric
  if (! (is.numeric(data[, tname])) ) stop("data[, tname] must be numeric")

  #  make sure gname is numeric
  if (! (is.numeric(data[, gname])) ) stop("data[, gname] must be numeric")

  #  make sure every time is strictly positive
  if (min(data[,tname]) < 1 ) stop("data[, tname] must be greater than zero")

  # make sure idname is unique in each period observation
  ## # Check if idname is unique by tname
  ## n_id_year = all( table(data[, idname], data[, tname]) <= 1)
  ## if (! n_id_year) stop("The value of idname must be the unique (by tname)")


  # put in blank xformla if no covariates
  if (is.null(xformla)) {
    xformla <- ~1
  }

  # drop irrelevant columns from data
  data <- cbind.data.frame(data[,c(idname, tname, yname, gname, weightsname, clustervars,cohortnames)], stats::model.frame(xformla, data=data, na.action=stats::na.pass))

  # check if any covariates were missing
  n_orig <- nrow(data)
  data <- data[stats::complete.cases(data),]
  n_diff <- n_orig - nrow(data)
  if (n_diff != 0) {
    warning(paste0("dropped ", n_diff, " rows from original data due to missing data"))
  }


  # weights if null
  ifelse(is.null(weightsname), w <- rep(1, nrow(data)), w <- data[,weightsname])

  if (".w" %in% colnames(data)) stop("`did` tried to use column named \".w\" internally, but there was already a column with this name")
  data$.w <- w

  # Outcome variable will be denoted by y
  # data$.y <- data[, yname]

  # figure out the dates
  # list of dates from smallest to largest
  tlist <- unique(data[,tname])[order(unique(data[,tname]))]

  # Groups with treatment time bigger than max time period are considered to be never treated
  asif_never_treated <- (data[,gname] > max(tlist, na.rm = TRUE))
  asif_never_treated[is.na(asif_never_treated)] <- FALSE
  data[asif_never_treated, gname] <- 0

  # create a dataframe with idname, dosename and gname dropping all other
  idtog <- data[, c(idname, gname)]
  idtog <- unique(idtog)

  # list of treated groups (by time) from smallest to largest
  idtog <- idtog[order(idtog[,gname]), ]

  glist <- idtog[,gname]

  # Check if there is a never treated group
  if ( length(glist[glist==0]) == 0) {
    if(control_group=="nevertreated"){
      stop("There is no available never-treated group")
    } else {
      # Drop all time periods with time periods >= latest treated
      data <- subset(data,(data[,tname] < (max(glist)-anticipation)))
      # Replace last treated time with zero
      # lines.gmax <- data[,gname]==max(glist, na.rm = TRUE)
      # data[lines.gmax,gname] <- 0

      idtog <- data[, c(idname, gname)]
      idtog <- unique(idtog)

      tlist <- sort(unique(data[,tname]))

      idtog <- idtog[,gname]

      # don't compute ATT(i,t) for groups that are only treated at end
      # and only play a role as a comparison group
      idtog <- idtog[idtog[,gname]<max(glist),]
      glist <- idtog[,gname]
    }
  }

  # Only the treated groups
  idtog <- idtog[idtog[,gname] > 0, ]
  # glist <- glist[glist>0]

  # drop groups treated in the first period or before
  first.period <- tlist[1]
  idtog <- idtog[idtog[,gname] > first.period + anticipation, ]
  glist <- idtog[,gname]
  # glist <- glist[glist > first.period + anticipation]

  # check for groups treated in the first period and drop these
  # nfirstperiod <- length(unique(data[ !((data[,gname] > first.period) | (data[,gname]==0)), ] )[,idname])
  treated_first_period <- ( data[,gname] <= first.period ) & ( !(data[,gname]==0) )
  treated_first_period[is.na(treated_first_period)] <- FALSE
  nfirstperiod <- ifelse(panel, length(unique(data[treated_first_period,][,idname])), nrow(data[treated_first_period,]))
  if ( nfirstperiod > 0 ) {
    warning(paste0("Dropped ", nfirstperiod, " units that were already treated in the first period."))
    data <- data[ data[,gname] %in% c(0,glist), ]
    # update tlist and glist
    tlist <- unique(data[,tname])[order(unique(data[,tname]))]

    idtog <- data[, c(idname, gname)]
    idtog <- unique(idtog)
    idtog <- idtog[idtog[,gname] > 0, ]

    # drop groups treated in the first period or before
    first.period <- tlist[1]
    idtog <- idtog[idtog[,gname] > first.period + anticipation, ]
    glist <- idtog[,gname]

  }




  #-----------------------------------------------------------------------------
  # setup data in panel case
  #-----------------------------------------------------------------------------
  if (panel) {
    # this is the case where we coerce balanced panel

    # check for complete cases
    keepers <- stats::complete.cases(data)
    n0 <- length(unique(data[,idname]))
    n.keep <- length(unique(data[keepers,idname]))
    if (nrow(data[keepers,]) < nrow(data)) {
      warning(paste0("Dropped ", (n0-n.keep), " observations that had missing data."))
      data <- data[keepers,]
    }

    # make it a balanced data set
    n.old <- length(unique(data[,idname]))
    data <- BMisc::makeBalancedPanel(data, idname, tname)
    data <- as.data.frame(data)
    n0 <- length(unique(data[,idname]))
    if (n0 < n.old) {
      warning(paste0("Dropped ", n.old-n0, " observations while converting to balanced panel."))
    }

    # If drop all data, you do not have a panel.
    if (nrow(data)==0) {
      stop("All observations dropped to converted data to balanced panel. Consider setting `panel = FALSE' and/or revisit 'idname'.")
    }

    n0 <- nrow(data[ data[,tname]==tlist[1], ])

    # slow, repeated check here...
    ## # check that first.treat doesn't change across periods for particular individuals
    ## if (!all(sapply( split(data, data[,idname]), function(df) {
    ##   length(unique(df[,gname]))==1
    ## }))) {
    ##   stop("The value of gname must be the same across all periods for each particular individual.")
    ## }

  }

  #-----------------------------------------------------------------------------
  # code for setting up unbalanced panel: I do not consider repeated cross-sections
  #-----------------------------------------------------------------------------
  if (!panel) {

    # check for complete cases
    keepers <- stats::complete.cases(data)
    if (nrow(data[keepers,]) < nrow(data)) {
      warning(paste0("Dropped ", nrow(data) - nrow(data[keepers,]), " observations that had missing data."))
      data <- data[keepers,]
    }

    # If drop all data, you do not have a panel.
    if (nrow(data)==0) {
      stop("All observations dropped due to missing data problems.")
    }

    # n-row data.frame to hold the influence function
    data$.rowid <- data[, idname]
    rowname <- ".rowid"
    # if (true_repeated_cross_sections) {
    #   data$.rowid <- seq(1:nrow(data))
    #   rowname <- ".rowid"
    # } else {
    #   # set rowid to idname for repeated cross section/unbalanced
    #   data$.rowid <- data[, idname]
    #   rowname <- ".rowid"
    # }

    # n0 is unique number of cross section observations
    n0 <- length(unique(data[,rowname]))
  }

  ## # Update tlist and glist because of data handling
  ## # figure out the dates
  ## # list of dates from smallest to largest
  ## tlist <- unique(data[,tname])[order(unique(data[,tname]))]
  ## # list of treated groups (by time) from smallest to largest
  ## glist <- unique(data[,gname])[order(unique(data[,gname]))]

  ## # Only the treated groups
  ## glist <- glist[glist>0]

  ## # drop groups treated in the first period or before
  ## first.period <- tlist[1]
  ## glist <- glist[glist > first.period + anticipation]

  # Check if groups is empty (usually a problem with the way people defined groups)
  if(length(glist)==0){
    stop("No valid groups. The variable in 'gname' should be expressed as the time a unit is first treated (0 if never-treated).")
  }

  # if there are only two time periods, then uniform confidence
  # bands are the same as pointwise confidence intervals
  if (length(tlist)==2) {
    cband <- FALSE
  }

  #-----------------------------------------------------------------------------
  # more error handling after we have balanced the panel

  # check against very small groups
  gsize <- stats::aggregate(data[,gname], by=list(data[,gname]), function(x) length(x)/length(tlist))

  # how many in each group before give warning
  # 5 is just a buffer, could pick something else, but seems to work fine
  reqsize <- length(BMisc::rhs.vars(xformla)) + 5

  # which groups to warn about
  gsize <- subset(gsize, get(colnames(gsize)[2]) < reqsize) # x is name of column from aggregate

  # warn if some groups are small
  if (nrow(gsize) > 0) {
    gpaste <-  paste(gsize[,1], collapse=",")
    warning(paste0("Be aware that there are some small groups in your dataset.\n  Check groups: ", gpaste, "."))

    if ( (0 %in% gsize[,1]) & (control_group == "nevertreated") ) {
      stop("never treated group is too small, try setting control_group=\"notyettreated\"")
    }
  }
  #----------------------------------------------------------------------------

  # How many time periods
  nT <- length(tlist)
  # How many treated groups
  nG <- length(glist)

  # idlist
  idlist = unique(idtog[,idname])

  # Warn if idlist and glist are not the same length
  if (length(idlist)!=length(glist)) stop("glist and idlist are not the same length")

  # order dataset wrt idname and tname
  data <- data[order(data[,idname], data[,tname]),]

  # store parameters for passing around later
  dp <- DIDparams_i(yname=yname,
                   tname=tname,
                   idname=idname,
                   gname=gname,
                   cohortnames = cohortnames,
                   xformla=xformla,
                   data=as.data.frame(data),
                   control_group=control_group,
                   anticipation=anticipation,
                   weightsname=weightsname,
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
                   n=n0,
                   nG=nG,
                   nT=nT,
                   tlist=tlist,
                   glist=glist,
                   idlist=idlist,
                   call=call)
}
