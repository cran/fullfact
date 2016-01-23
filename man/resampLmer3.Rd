\name{resampLmer3}
\alias{resampLmer3}
\title{Bootstrap components for normal data 3}
\description{
Extracts additive genetic, non-additive genetic, and maternal variance components
from a linear mixed-effect model using the \emph{lmer} function of the \emph{lme4} package.
Model random effects are dam, sire, dam by sire, and any additional fixed and/or random effects.
}
\usage{
resampLmer3(resamp, dam, sire, response, start, end, remain, ml = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resamp}{Data frame of bootstrap resampled data.}
  \item{dam}{Column name containing dam(female) parent identity information.}
  \item{sire}{Column name containing sire(male) parent identity information.}
  \item{response}{Column name containing the offspring (response) phenotype values.}
  \item{start}{Starting model number.}
  \item{end}{Ending model number.}
  \item{remain}{
Remaining formula using \emph{lme4} package format with # sign (see column names),
e.g. fixed# + (1|random#).
}
  \item{ml}{Default is FALSE for restricted maximum likelihood. Change to TRUE for maximum likelihood.}
}
\details{
Used for bootstrap resampled data set produced using \emph{resampRepli} or \emph{resampFamily}.
Extracts the dam, sire, dam, dam by sire, and residual variance components.
Extracts any additional fixed effect and random effect variance components. The fixed-effect variance
component is as a single group using the method described by Nakagawa and Schielzeth (2013).
Calculates the total variance component. Calculates the additive genetic, non-additive genetic, and
maternal variance components (see Lynch and Walsh 1998, p. 603).
}
\value{
A data frame with columns containing the raw variance components for dam, sire, dam by sire,
residual, total, additive genetic, non-additive genetic, and maternal. Also columns containing
the raw variance components for remaining formula components. The number of rows in the
data frame matches the number of iterations in the resampled data set and each row represents a
model number.
}
\references{
Bolker BM, Brooks ME, Clark CJ, Geange SW, Poulsen JR, Stevens MHH, White J-SS. 2009.
Generalized linear mixed models: a practical guide for ecology and evolution.
Trends in Ecology and Evolution 24(3): 127-135. DOI: 10.1016/j.tree.2008.10.008

Lynch M, Walsh B. 1998. Genetics and Analysis of Quantitative Traits. Sinauer Associates, Massachusetts.

Nakagawa S, Schielzeth H. 2013. A general and simple method for obtaining R2 from generalized linear
mixed-effects models. Methods in Ecology and Evolution 4(2): 133-142.
DOI: 10.1111/j.2041-210x.2012.00261.x
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
\seealso{ \code{\link{resampLmer}}, \code{\link{resampLmer2}} }
\examples{
data(chinook_resampL)

#length_rcomp3<- resampLmer3(resamp=length_datR,dam="dam",sire="sire",response="length",
#start=1,end=1000,remain="egg_size# + (1|tray#)")
length_rcomp3<- resampLmer3(resamp=chinook_resampL,dam="dam",sire="sire",response="length",
start=1,end=5,remain="egg_size# + (1|tray#)")
}