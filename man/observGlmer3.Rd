\name{observGlmer3}
\alias{observGlmer3}
\title{Variance components for non-normal data 3}
\description{
Extracts additive genetic, non-additive genetic, and maternal variance components
from a generalized linear mixed-effect model using the \emph{glmer} function of the \emph{lme4} package.
Model random effects are dam, sire, dam by sire, and any additional fixed and/or random effects.
}
\usage{
observGlmer3(observ, dam, sire, response, fam_link, remain, quasi = F, iter = 1000)
}
\arguments{
  \item{observ}{Data frame of observed data.}
  \item{dam}{Column name containing dam (female) parent identity information.}
  \item{sire}{Column name containing sire (male) parent identity information.}
  \item{response}{Column name containing the offspring (response) phenotype values.}
  \item{fam_link}{
The family and link in family(link) format. Supported options are binomial(link="logit"), binomial(link="probit"), poisson(link="log"), and poisson(link="sqrt").
}
  \item{remain}{Remaining formula using \emph{lme4} package format.}
  \item{quasi}{Incorporate overdispersion or quasi-error structure.}
  \item{iter}{Number of iterations for computing the parametric bootstrap significance value
  for any fixed effects.}
}
\details{
Laplace approximation parameter estimation is used, which is a true likelihood method (Bolker et al. 2009).
For the overdispersion option, an observation-level random effect is added to the model (Atkins et al. 2013).
Extracts the dam, sire, dam, and dam by sire variance components.
Extracts any additional fixed effect and random effect variance components. The fixed-effect variance
component is as a single group using the method described by Nakagawa and Schielzeth (2013).
The residual variance component for the \emph{fam_links} are described by Nakagawa and Schielzeth (2010, 2013).
Calculates the total variance component. Calculates the additive genetic, non-additive genetic, and
maternal variance components (see Lynch and Walsh 1998, p. 603).
Significance values for the random effects are determined using likelihood ratio tests (Bolker et al. 2009).
Significance values for any fixed effects are determined using likelihood ratio tests and a parametric
bootstrap method (Bolker et al. 2009) from the \emph{mixed} function of the \emph{afex} package.
}
\value{
A list object containing the raw variance components, the variance components as a percentage
of the total variance component. Contains the difference in AIC and BIC, likelihood ratio
test Chi-square and p-value for random and/or fixed effects. Also contains the parametric
bootstrap Chi-square and p-value for any fixed effects.
}
\references{
Atkins DC, Baldwin SA, Zheng C, Gallop RJ, Neighbors C. 2013. A tutorial on count regression and
zero-altered count models for longitudinal substance use data.
Psychology of Addictive Behaviors 27(1): 166-177. DOI: 10.1037/a0029508

Bolker BM, Brooks ME, Clark CJ, Geange SW, Poulsen JR, Stevens MHH, White J-SS. 2009.
Generalized linear mixed models: a practical guide for ecology and evolution.
Trends in Ecology and Evolution 24(3): 127-135. DOI: 10.1016/j.tree.2008.10.008

Lynch M, Walsh B. 1998. Genetics and Analysis of Quantitative Traits. Sinauer Associates, Massachusetts.

Nakagawa S, Schielzeth H. 2010. Repeatability for Gaussian and non-Gaussian data: a practical guide for biologists.
Biological Reviews 85(4): 935-956. DOI: 10.1111/j.1469-185X.2010.00141.x

Nakagawa S, Schielzeth H. 2013. A general and simple method for obtaining R2 from generalized linear mixed-effects models.
Methods in Ecology and Evolution 4(2): 133-142. DOI: 10.1111/j.2041-210x.2012.00261.x
}
\note{
The Laplace approximation is used because there were fewer disadvantages relative to penalized
quasi-likelihood and Gauss-Hermite quadrature parameter estimation (Bolker et al. 2009).
That is, penalized quasi-likelihood is not recommended for count responses with means less than 5 and
binary responses with less than 5 successes per group. Gauss-Hermite quadrature is not recommended for
more than two or three random effects because of the rapidly declining analytical speed with the
increasing number of random effects.
}
\seealso{ \code{\link{observGlmer}}, \code{\link{observGlmer2}} }
\examples{
data(chinook_survival) #Chinook salmon offspring survival
## Convert replicate-level recorded data to individual-level (binary) data
chinook_survival2<- buildBinary(dat=chinook_survival,copy=c(2:6,9),one="alive",zero="dead")
#just a few iterations for the p-value of fixed effect
\dontrun{survival_mod3<- observGlmer3(observ=chinook_survival2,dam="dam",sire="sire",
response="status",fam_link=binomial(link="logit"),remain="egg_size + (1|tray)",iter=5)
survival_mod3}
}