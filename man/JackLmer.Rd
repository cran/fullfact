\name{JackLmer}
\alias{JackLmer}
\title{Jackknife components for normal data}
\description{
Extracts additive genetic, non-additive genetic, and maternal variance components
from a linear mixed-effect model using the \emph{lmer} function of the \emph{lme4} package.
Model random effects are dam, sire, and dam by sire.
}
\usage{
JackLmer(observ, dam, sire, response, ml = F, size = 1, first = NULL)
}
\arguments{
  \item{observ}{Data frame of observed data.}
  \item{dam}{Column name containing dam (female) parent identity information.}
  \item{sire}{Column name containing sire (male) parent identity information.}
  \item{response}{Column name containing the offspring (response) phenotype values.}
  \item{ml}{Default is FALSE for restricted maximum likelihood. Change to TRUE for maximum likelihood.}
  \item{size}{Default is 1 for delete-one jackknife resampling. If \emph{size} > 1,
delete-\emph{d} jackknife resampling occurs removing a block d equal to \emph{size}.}
  \item{first}{
Number of initial sub-samples to run. Useful for examing if there is variation among
sub-samples before jackknife resampling the entire data set. There can be little variation for
delete-one jackknife resampling with large data sets, and delete-\emph{d} jackknife resampling
should be considered.
}
}
\details{
Uses delete-one jackknife resampling (Efron & Tibshirani 1993, p. 141-145). For the option of
delete-\emph{d} jackknife resampling, the rows of the observed data frame are shuffled and a block of
observations of size \emph{d} is deleted sequentially.
Extracts the dam, sire, dam, dam by sire, and residual variance components.
Calculates the total variance component. Calculates the additive genetic, non-additive genetic, and
maternal variance components (see Lynch and Walsh 1998, p. 603).
}
\value{
A data frame with columns containing the raw variance components for dam, sire, dam by sire,
residual, total, additive genetic, non-additive genetic, and maternal. The number of rows
in the data frame matches the total number of observations (\emph{N}) for delete-one jackknife
resampling or \emph{M} groups for delete-\emph{d} jackknife resampling to the lowest integer.
Each row represents a deleted single observation or deleted \emph{d} observations group.
}
\references{
Efron B, Tibshirani R. 1993. An introduction to the Bootstrap. Chapman and Hall, New York.

Lynch M, Walsh B. 1998. Genetics and Analysis of Quantitative Traits. Sinauer Associates, Massachusetts.
}
\note{
Maximum likelihood (ML) estimates the parameters that maximize the likelihood of the observed data
and has the advantage of using all the data and accounting for non-independence (Lynch and Walsh 1998,
p. 779; Bolker et al. 2009). On the other hand, ML has the disadvantage of assuming that all fixed
effects are known without error, producing a downward bias in the estimation of the residual variance
component. This bias can be large if there are lots of fixed effects, especially if sample sizes are
small. Restricted maximum likelihood (REML) has the advantage of not assuming the fixed effects are
known and averages over the uncertainty, so there can be less bias in the estimation of the residual
variance component. However, REML only maximizes a portion of the likelihood to estimate the effect
parameters, but is the preferred method for analyzing large data sets with complex structure.
}
\seealso{ \code{\link{JackLmer2}}, \code{\link{JackLmer3}} }
\examples{
data(chinook_length) #Chinook salmon offspring length

#Delete-one
#length_jack1<- JackLmer(observ=chinook_length,dam="dam",sire="sire",response="length")
length_jack1<- JackLmer(observ=chinook_length,dam="dam",sire="sire",response="length",
first=2) #first 2

#Delete-d, d=5
#length_jackD<- JackLmer(observ=chinook_length,dam="dam",sire="sire",response="length",
#size=5)
length_jackD<- JackLmer(observ=chinook_length,dam="dam",sire="sire",response="length",
size=5,first=2) #first 2
}