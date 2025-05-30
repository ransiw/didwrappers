% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_data.R
\name{sim_data}
\alias{sim_data}
\title{Creates a simulated dataset with treatment dosages and a control group}
\usage{
sim_data(
  basetreat = seq(10, 100, 10),
  timetreat = rep(c(10, 15), 5),
  dosage = rep(c(1, 1), each = 5),
  tef = rep(10, 10),
  basecontrol = seq(5, 100, 5),
  cohort = rep(c(1, 1, 1, 1, 1), 6),
  pretreat = 5,
  posttreat = 20,
  ttr = 2,
  siget = 1,
  sigec = 1,
  fixedbase = FALSE
)
}
\arguments{
\item{basetreat}{a numeric vector that specifies the pre-treatment outcomes}

\item{timetreat}{a numeric vector of the same length as \code{basetreat} that specifies the treatment time (avoid negative or zero times)}

\item{dosage}{a numeric vector of the same length as \code{basetreat} that specifies the treatment dosage received by a treatment unit. Zero for control units.}

\item{tef}{a numeric vector of the same length as \code{basetreat} that specifies the treatment effect. The argument \code{dosage} multiplies the treatment effect by the number of doses.}

\item{basecontrol}{a numeric vector that specifies the outcome level at the earliest treatment time}

\item{cohort}{a vector of the length of \code{basecontrol} and \code{basetreat} combined that identifies the cohort of the unit}

\item{pretreat}{the number of pre-treatment periods for the earliest treated unit}

\item{posttreat}{the number of post-treatment periods for the earliest treated unit. If units are treated \code{posttreat} periods after the earliest treated unit, they will never be treated.}

\item{ttr}{the time trend}

\item{siget}{the standard error of the error term of the treatment group with default 1}

\item{sigec}{the standard error of the error term of the control group with default 1}

\item{fixedbase}{baseline observation is known and therefore is not perturbed.}
}
\value{
a dataframe with identifies for unit, time, treatment time, cohort, dosage, and the outcome
}
\description{
Creates a simulated dataset with treatment dosages and a control group
}
\examples{
# A default dataframe with only one dosage level and one cohort
sim_data()
# A default dataframe with two different dosage levels
sim_data(dosage = rep(c(1,2),each=5))
# A default dataframe with 5 different cohorts
sim_data(cohort = rep(c(1,2,3,4,5),6))
}
