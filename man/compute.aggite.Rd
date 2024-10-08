% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute.aggite.R
\name{compute.aggite}
\alias{compute.aggite}
\title{Compute Aggregated Treatment Effect Parameters}
\usage{
compute.aggite(
  MP,
  type = "group",
  balance_e = NULL,
  min_e = -Inf,
  max_e = Inf,
  na.rm = FALSE,
  bstrap = NULL,
  biters = NULL,
  cband = NULL,
  alp = NULL,
  clustervars = NULL,
  call = NULL
)
}
\arguments{
\item{MP}{an MP_i object (i.e., the results of the \code{\link[=att_it]{att_it()}} method)}

\item{type}{Which type of aggregated treatment effect parameter to compute.
One option is "simple" (this just computes a weighted average of all
group-time average treatment effects with weights proportional to group
size).  Other options are "dynamic" (this computes average effects across
different lengths of exposure to the treatment and is similar to an
"event study"; here the overall effect averages the effect of the
treatment across all positive lengths of exposure); "group" (this
is the default option and
computes average treatment effects across different groups; here
the overall effect averages the effect across different groups); and
"calendar" (this computes average treatment effects across different
time periods; here the overall effect averages the effect across each
time period).}

\item{balance_e}{If set (and if one computes dynamic effects), it balances
the sample with respect to event time.  For example, if \code{balance.e=2},
\code{aggite} will drop groups that are not exposed to treatment for
at least three periods. (the initial period when \code{e=0} as well as the
next two periods when \code{e=1} and the \code{e=2}).  This ensures that
the composition of groups does not change when event time changes.}

\item{min_e}{For event studies, this is the smallest event time to compute
dynamic effects for.  By default, \code{min_e = -Inf} so that effects at
all lengths of exposure are computed.}

\item{max_e}{For event studies, this is the largest event time to compute
dynamic effects for.  By default, \code{max_e = Inf} so that effects at
all lengths of exposure are computed.}

\item{na.rm}{Logical value if we are to remove missing Values from analyses. Defaults is FALSE.}

\item{bstrap}{Boolean for whether or not to compute standard errors using
the multiplier bootstrap.  If standard errors are clustered, then one
must set \code{bstrap=TRUE}. Default is value set in the MP object.  If bstrap is \code{FALSE}, then analytical
standard errors are reported.}

\item{biters}{The number of bootstrap iterations to use.  The default is the value set in the MP object,
and this is only applicable if \code{bstrap=TRUE}.}

\item{cband}{Boolean for whether or not to compute a uniform confidence
band that covers all of the group-time average treatment effects
with fixed probability \code{1-alp}.  In order to compute uniform confidence
bands, \code{bstrap} must also be set to \code{TRUE}.  The default is
the value set in the MP object}

\item{alp}{the significance level, default is value set in the MP object.}

\item{clustervars}{A vector of variables to cluster on.  At most, there
can be two variables (otherwise will throw an error) and one of these
must be the same as idname which allows for clustering at the individual
level. Default is the variables set in the MP object}

\item{call}{The function call to aggite}
}
\value{
\code{\link{AGGITEobj}} object
}
\description{
Does the heavy lifting on computing aggregated group-time
average treatment effects
}
\examples{
# This is a helper function for [aggite()]. See that function for examples.


}
\keyword{internal}
