#' Creates a simulated dataset with treatment dosages and a control group
#'
#' @param basetreat a numeric vector that specifies the pre-treatment outcomes
#' @param timetreat a numeric vector of the same length as `basetreat` that specifies the treatment time (avoid negative or zero times)
#' @param dosage a numeric vector of the same length as `basetreat` that specifies the treatment dosage received by a treatment unit. Zero for control units.
#' @param tef a numeric vector of the same length as `basetreat` that specifies the treatment effect. The argument `dosage` multiplies the treatment effect by the number of doses.
#' @param basecontrol a numeric vector that specifies the outcome level at the earliest treatment time
#' @param cohort a vector of the length of `basecontrol` and `basetreat` combined that identifies the cohort of the unit
#' @param pretreat the number of pre-treatment periods for the earliest treated unit
#' @param posttreat the number of post-treatment periods for the earliest treated unit. If units are treated `posttreat` periods after the earliest treated unit, they will never be treated.
#' @param ttr the time trend
#' @param siget the standard error of the error term of the treatment group with default 1
#' @param sigec the standard error of the error term of the control group with default 1
#'
#' @return a dataframe with identifies for unit, time, treatment time, cohort, dosage, and the outcome
#' @export
#'
#' @examples
#' # A default dataframe with only one dosage level and one cohort
#' sim_data()
#' # A default dataframe with two different dosage levels
#' sim_data(dosage = rep(c(1,2),each=5))
#' # A default dataframe with 5 different cohorts
#' sim_data(cohort = rep(c(1,2,3,4,5),6))
sim_data <- function(basetreat = seq(10,100,10),
                     timetreat = rep(c(10,15),5),
                     dosage = rep(c(1,1),each=5),
                     tef=rep(10,10),
                     basecontrol=seq(5,100,5),
                     cohort=rep(c(1,1,1,1,1),6),
                     pretreat=5,
                     posttreat=20,
                     ttr=2,
                     siget=1,
                     sigec=1){

  # check for the lengths
  if (length(basetreat)!=length(timetreat)){
    stop("basetreat and timetreat are of different lengths")
  }
  if (length(tef)!=length(basetreat)){
    stop("basetreat and tef are of different lengths")
  }
  if (length(cohort)>0 & length(cohort)!=length(c(basetreat,basecontrol))){
    stop("Cohorts are of different length to the basetreat+basecontrol vectors. Set cohorts=NULL if they do not apply")
  }
  if (length(dosage)!=length(basetreat)){
    stop("basetreat and dosage are of different lengths. Set dosage=NULL if they do not apply")
  }

  # stop if the treatment time is zero
  if (0 %in% timetreat){
    stop("Treatment time cannot be zero")
  }

  # stop if there are non-numeric values
  if (!(is.numeric(basetreat) & is.numeric(timetreat) & is.numeric(tef) & c(is.numeric(dosage)|is.null(dosage)) & c(is.numeric(basecontrol)|is.null(basecontrol)))){
    stop("There are non-numeric vectors in either basetreat, timetreat, tef, dosage, or basecontrol")
  }

  # determine the cohorts
  if (is.null(cohort)){
    cohort = rep(NA,length(c(basetreat,basecontrol)))
  }

  # determine dosage
  if (is.null(dosage)){
    cohort = rep(1,length(c(basetreat)))
  }

  ## create the treatment data frame
  # total time periods
  timevector = unique((min(timetreat)-pretreat):(min(timetreat)+posttreat))
  Ntime = length(timevector)

  # create treatment time periods
  bstreatdf = data.frame(unit = 1:length(timetreat),baselevel = basetreat, tef=tef, cohort=cohort[1:length(basetreat)], dosage=dosage)
  dftreat = data.frame(unit = rep(1:length(timetreat),each=Ntime),treatg = rep(timetreat,each=Ntime),time = rep(timevector, length(timetreat)))

  # define the post-treatment period
  dftreat$posttreat <- as.numeric(dftreat$time >= dftreat$treatg)

  # merge the treatment effect and cohort information into the dftreat dataframe
  dftreat = merge(dftreat, bstreatdf, by = "unit", all.x = TRUE)

  # add an error term with a stated standard error
  dftreat$error = stats::rnorm(nrow(dftreat),0,siget)

  # create the timelag variable
  dftreat$timelag = dftreat$time - dftreat$treatg

  # create the outcome variable y
  dftreat$y = dftreat$baselevel+dftreat$timelag*ttr+dftreat$posttreat*dftreat$tef*dftreat$dosage+dftreat$error

  ## create the control dataframe which also works if there are no control units
  dfcont = data.frame(unit=NA, treatg = NA,time=NA,baselevel=NA,tef=NA,cohort=NA,dosage=NA,posttreat=NA,error=NA,timelag=NA,y=NA)

  # the case where the control group is not NULL
  if (!is.null(basecontrol)){

    #create control time periods
    bscontdf = data.frame(unit = (length(basetreat)+1):(length(basetreat)+length(basecontrol)),
                          baselevel = basecontrol,
                          cohort = cohort[(length(basetreat)+1):(length(basetreat)+length(basecontrol))])
    dfcont = data.frame(unit = rep((length(basetreat)+1):(length(basetreat)+length(basecontrol)),each=Ntime),
                        time = rep(timevector, length(basecontrol)))

    # add the zero treatment group and all post-treatment indicators as 0
    dfcont$treatg = rep(0, nrow(dfcont))
    dfcont$posttreat = rep(0, nrow(dfcont))

    # add cohort level information
    dfcont = merge(dfcont, bscontdf, by = "unit", all.x = TRUE)

    # add a missing treatment effect and dosage
    dfcont$tef = rep(NA, nrow(dfcont))
    dfcont$dosage = rep(0, nrow(dfcont))

    # add an error term
    dfcont$error = stats::rnorm(nrow(dfcont),0,sigec)

    # add a timelag from minimum treated time
    dfcont$timelag = dfcont$time - min(timetreat)

    # create the y for a control group
    dfcont$y = dfcont$baselevel+dfcont$timelag*ttr+dfcont$error
  }

  ## The final data object
  df = rbind(dftreat,dfcont)

  # remove any variables that have missing values on unit and time
  df <- df[stats::complete.cases(df$unit, df$time), ]

}
