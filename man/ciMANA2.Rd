\name{ciMANA2}
\alias{ciMANA2}
\title{Bootstrap confidence intervals 2}
\description{
Extracts bootstrap-\emph{t} confidence intervals for additive genetic, non-additive genetic, and maternal
variance components. Also extracts intervals for optional position and block variance components.
}
\usage{
ciMANA2(comp, level = 95, rnd_r = 3, rnd_p = 1, position = NULL, block = NULL,
bias = NULL, accel = NULL, trait = NULL)
}
\arguments{
  \item{comp}{Data frame of bootstrap resampling results.}
  \item{level}{Confidence level, as a percentage. Default is 95.}
  \item{rnd_r}{Number of decimal places to round the confidence interval of raw values.}
  \item{rnd_p}{Number of decimal places to round the confidence interval of percentage values.}
  \item{position}{Optional column name containing position factor information.}
  \item{block}{Optional column name containing block factor information.}
  \item{bias}{
Optional vector of raw observed additive, non-additive, maternal, position and/or block
variance component values for bias correction, i.e. c(additive, non-additive, maternal, position/block).
If there is a position and a block c(..., maternal, position, block).
}
  \item{accel}{Optional data frame of jackknifed data model results for acceleration correction.}
  \item{trait}{Optional label for the phenotypic trait.}
}
\details{
Used for bootstrap resampling results produced using \emph{resampLmer2} for normal data or
\emph{resampGlmer2} for non-normal data.
Bootstrap-\emph{t} confidence intervals, including bias and acceleration correction methods are
described by Efron and Tibshirani (1993).
Jackknife data model results for acceleration correction can be produced using \emph{JackLmer2},
for normal data or \emph{JackGlmer2} for non-normal data.
The 'bias fail' warning is if the bias calculation is Inf or -Inf, e.g. \emph{bias} contains
a zero value, so the uncorrected confidence interval is displayed.
}
\value{
Prints a data frame containing the lower, median, and upper values of the bootstrap-\emph{t} confidence
interval for additive genetic, non-additive genetic, maternal, and optional position and/or block
variance components. Values are presented as raw and percentages of the total variance value
within each row.
}
\references{
Efron B, Tibshirani R. 1993. An introduction to the Bootstrap. Chapman and Hall, New York.
}
\seealso{ \code{\link{ciMANA}}, \code{\link{ciMANA3}} }
\examples{

#Import bootstrap resampled data model results
data(chinook_bootL) #Chinook salmon offspring length

#Extract un-corrected confidence interval
ciMANA2(comp=chinook_bootL,position="tray")

#Extract bias corrected confidence interval
ciMANA2(comp=chinook_bootL,bias=c(0.0000000,0.7192253,0.2029684,0.1077423),position="tray")
#see details for 'bias' fail

#Extract bias and accelerated corrected confidence interval
#Import jackknife resampled data model results
data(chinook_jackL)
#
ciMANA2(comp=chinook_bootL,bias=c(0.0000000,0.7192253,0.2029684,0.1077423),
accel=chinook_jackL,position="tray")
#see details for 'bias' fail
}