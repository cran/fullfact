\name{ciMANA3}
\alias{ciMANA3}
\title{Bootstrap confidence intervals 3}
\description{
Extracts bootstrap-\emph{t} confidence intervals for additive genetic, non-additive genetic, and maternal
variance components. Also extracts intervals for additional fixed and random effects.
}
\usage{
ciMANA3(comp, level = 95, rnd_r = 3, rnd_p = 1, bias = NULL, accel = NULL,
remain = NULL, trait = NULL)
}
\arguments{
  \item{comp}{Data frame of bootstrap resampling results.}
  \item{level}{Confidence level, as a percentage. Default is 95.}
  \item{rnd_r}{Number of decimal places to round the confidence interval of raw values.}
  \item{rnd_p}{Number of decimal places to round the confidence interval of percentage values.}
  \item{bias}{
Optional vector of raw observed additive, non-additive, and maternal variance components
for bias correction. Followed by any other components in the order of the vector \emph{remain},
i.e. c(additive, non-additive, maternal, component1, component2, ...).
}
  \item{accel}{Optional data frame of jackknifed data model results for acceleration correction.}
  \item{remain}{Vector of column names for additional effects.}
  \item{trait}{Optional label for the phenotypic trait.}
}
\details{
Used for bootstrap resampling results produced using \emph{resampLmer3} for normal data or
\emph{resampGlmer3} for non-normal data.
Bootstrap-\emph{t} confidence intervals, including bias and acceleration correction methods are
described by Efron and Tibshirani (1993).
Jackknife data model results for acceleration correction can be produced using \emph{JackLmer3},
for normal data or \emph{JackGlmer3} for non-normal data.
The 'bias fail' warning is if the bias calculation is Inf or -Inf, e.g. \emph{bias} contains
a zero value, so the uncorrected confidence interval is displayed.
}
\value{
Prints a data frame containing the lower, median, and upper values of the bootstrap-\emph{t} confidence
interval for additive genetic, non-additive genetic, maternal, and any additional fixed effect
and random effect variance components. Values are presented as raw and percentages of the total variance
value within each row.
}
\references{
Efron B, Tibshirani R. 1993. An introduction to the Bootstrap. Chapman and Hall, New York.
}
\seealso{ \code{\link{ciMANA}}, \code{\link{ciMANA2}} }
\examples{

#Import bootstrap resampled data model results
data(chinook_bootL) #Chinook salmon offspring length

#Extract un-corrected confidence interval
ciMANA3(comp=chinook_bootL,remain=c("tray","Residual"))

#Extract bias corrected confidence interval
ciMANA3(comp=chinook_bootL,bias=c(0.0000000,0.7192253,0.2029684,0.1077423,0.5499255),
remain=c("tray","Residual"))
#see details for 'bias' fail

#Extract bias and accelerated corrected confidence interval
#Import jackknife resampled data model results
data(chinook_jackL)
#
ciMANA3(comp=chinook_bootL,bias=c(0.0000000,0.7192253,0.2029684,0.1077423,0.5499255),
accel=chinook_jackL,remain=c("tray","Residual"))
}