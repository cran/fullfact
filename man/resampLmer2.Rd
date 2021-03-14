\name{resampLmer2}
\alias{resampLmer2}
\title{Bootstrap components for normal data 2}
\description{
Extracts additive genetic, non-additive genetic, and maternal variance components
from a linear mixed-effect model using the \emph{lmer} function of the \emph{lme4} package.
Model random effects are dam, sire, and dam by sire.
Options to include one random position and/or one random block effect(s).
}
\usage{
resampLmer2(resamp, dam, sire, response, start, end, position = NULL, block = NULL,
ml = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resamp}{Data frame of bootstrap resampled data.}
  \item{dam}{Column name containing dam (female) parent identity information.}
  \item{sire}{Column name containing sire (male) parent identity information.}
  \item{response}{Column name containing the offspring (response) phenotype values.}
  \item{start}{Starting model number.}
  \item{end}{Ending model number.}
  \item{position}{Optional column name containing position factor information.}
  \item{block}{Optional column name containing block factor information.}
  \item{ml}{Default is FALSE for restricted maximum likelihood. Change to TRUE for maximum likelihood.}
}
\details{
Used for bootstrap resampled data set produced using \emph{resampRepli} or \emph{resampFamily}.
Extracts the dam, sire, dam, dam by sire, and residual variance components.
Extracts optional position and block variance components.
Calculates the total variance component. Calculates the additive genetic, non-additive genetic, and
maternal variance components (see Lynch and Walsh 1998, p. 603).
}
\value{
A data frame with columns containing the raw variance components for dam, sire, dam by sire,
residual, total, additive genetic, non-additive genetic, and maternal. Also columns containing
the raw variance components for the options of position and/or block. The number of rows in the
data frame matches the number of iterations in the resampled data set and each row represents a
model number.
}
\references{
Bolker BM, Brooks ME, Clark CJ, Geange SW, Poulsen JR, Stevens MHH, White J-SS. 2009.
Generalized linear mixed models: a practical guide for ecology and evolution.
Trends in Ecology and Evolution 24(3): 127-135. DOI: 10.1016/j.tree.2008.10.008

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
\seealso{ \code{\link{resampLmer}}, \code{\link{resampLmer3}} }
\examples{
data(chinook_resampL) #5 iterations

#length_rcomp2<- resampLmer2(resamp=length_datR,dam="dam",sire="sire",response="length",
#start=1,end=1000,position="tray")
length_rcomp2<- resampLmer2(resamp=chinook_resampL,dam="dam",sire="sire",response="length",
start=1,end=5,position="tray")
}