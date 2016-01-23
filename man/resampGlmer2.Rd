\name{resampGlmer2}
\alias{resampGlmer2}
\title{Bootstrap components for non-normal data 2}
\description{
Extracts additive genetic, non-additive genetic, and maternal variance components
from a generalized linear mixed-effect model using the \emph{glmer} function of the \emph{lme4} package.
Model random effects are dam, sire, and dam by sire.
Options to include one random position and/or one random block effect(s).
}
\usage{
resampGlmer2(resamp, dam, sire, response, fam_link, start, end, position = NULL,
block = NULL, quasi = F)
}
\arguments{
  \item{resamp}{Data frame of bootstrap resampled data.}
  \item{dam}{Column name containing dam(female) parent identity information.}
  \item{sire}{Column name containing sire(male) parent identity information.}
  \item{response}{Column name containing the offspring (response) phenotype values.}
  \item{fam_link}{
The family and link in family(link) format.
Supported options are binomial(logit), binomial(probit), poisson(log), and poisson (sqrt).
}
  \item{start}{Starting model number.}
  \item{end}{Ending model number.}
  \item{position}{Optional column name containing position factor information.}
  \item{block}{Optional column name containing block factor information.}
  \item{quasi}{Incorporate overdispersion or quasi-error structure.}
}
\details{
Used for bootstrap resampled data set produced using \emph{resampRepli} or \emph{resampFamily}.
Laplace approximation parameter estimation is used, which is a true likelihood method (Bolker et al. 2009).
For the overdispersion option, an observation-level random effect is added to the model (Atkins et al. 2013).
Extracts the dam, sire, dam, and dam by sire variance components.
Extracts optional position and block variance components.
The residual variance component for the \emph{fam_links} are described by Nakagawa and Schielzeth (2010, 2013).
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
\seealso{ \code{\link{resampGlmer}}, \code{\link{resampGlmer3}} }
\examples{
data(chinook_resampS) #5 iterations

\dontrun{survival_rcomp2<- resampGlmer2(resamp=survival_datR,dam="dam",sire="sire",response="status",
fam_link=binomial(logit),start=1,end=1000,position="tray")}
}