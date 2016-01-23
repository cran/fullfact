\name{ciJack2}
\alias{ciJack2}
\title{Jackknife confidence intervals 2}
\description{
Extracts jackknife confidence intervals for additive genetic, non-additive genetic, and maternal
variance components. Also extracts intervals for optional position and block variance components.
}
\usage{
ciJack2(comp, full, level = 95, rnd_r = 3, rnd_p = 1, position = NULL, block = NULL,
trait = NULL)
}
\arguments{
  \item{comp}{Data frame of jackknife resampling results.}
  \item{full}{
A vector of raw observed additive, non-additive, maternal, and total variance component values
for from the full observed data set, i.e. c(additive, non-additive, maternal, total, position/block).
If there is a position and a block c(..., maternal, position, block).
}
  \item{level}{Confidence level, as a percentage. Default is 95.}
  \item{rnd_r}{Number of decimal places to round the confidence interval of raw values.}
  \item{rnd_p}{Number of decimal places to round the confidence interval of percentage values.}
  \item{position}{Optional column name containing position factor information.}
  \item{block}{Optional column name containing block factor information.}
  \item{trait}{Optional label for the phenotypic trait.}
}
\details{
Used for jackknife resampling results produced using \emph{JackLmer2} for normal data or
\emph{JackGlmer2} for non-normal data.
Jackknife confidence intervals, using pseudo-values are described by Efron and Tibshirani (1993).
The standard errors are calculated from the pseudo-values and the Student's \emph{t} distribution is used
to provide the lower and upper confidence values. For delete-\emph{d} jackknife resampling,
\emph{M} degrees of freedom are used for producing the confidence interval (Martin et al. 2004):
\emph{M = N / d}, where \emph{N} is the total number of observations and d is the number of
deleted observations. That is, \emph{M} is the number of row in the jackknife resampling results.
Large values of \emph{M}, such as 1,000, can translate to the delete-d jackknife resampling
method approaching bootstrap resampling expectations (Efron & Tibshirani 1993).
}
\value{
Prints a data frame containing the lower, median, and upper values of the jackknife confidence
interval for additive genetic, non-additive genetic, maternal variance components, and optional
position and/or block variance components. Values are presented as raw and percentages
of the total variance value within each row.
}
\references{
Efron B, Tibshirani R. 1993. An introduction to the Bootstrap. Chapman and Hall, New York.

Martin, H., Westad, F. & Martens, H. (2004). Imporved Jackknife Variance Estimates of Bilinear
Model Parameters. COMPSTAT 2004 -- Proceedings in Computational Statistics 16th Symposium Held
in Prague, Czech Republic, 2004 (ed J. Antoch), pp. 261-275. Physica-Verlag HD, Heidelberg.
}
\seealso{ \code{\link{ciJack}}, \code{\link{ciJack3}} }
\examples{
data(chinook_jackL) #Chinook salmon offspring length, delete-one jackknife
ciJack2(chinook_jackL,c(0.0000000,0.7192253,0.2029684,1.0404425,0.1077423),position="tray")
}