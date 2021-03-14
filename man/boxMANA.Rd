\name{boxMANA}
\alias{boxMANA}
\title{Boxplot of resampled results}
\description{
A simple boxplot function for bootstrap and jackknife resampled results of additive
genetic, non-additive genetic, and maternal variance components.
}
\usage{
boxMANA(comp, type = "perc", ymax = NULL, ymin = NULL, yunit = NULL, leg = "topright",
cex_ylab = 1, cex_yaxis = 1, cex_names = 1)
}
\arguments{
  \item{comp}{Data frame of bootstrap or jackknife resampling results.}
  \item{type}{
Default is "perc" for percentage values of variance components. Other option is "raw"
for raw values of variance components.
}
  \item{ymax}{Maximum value of the y-axis.}
  \item{ymin}{Minimum value of the y-axis.}
  \item{yunit}{Unit increment of the y-axis.}
  \item{leg}{Position of the simple legend.}
  \item{cex_ylab}{Magnification of the y-axis label.}
  \item{cex_yaxis}{Magnification of the y-axis units.}
  \item{cex_names}{Optional magnification of trait labels.}
}
\details{
Plots an R boxplot. Uses an object produced by any of the bootstrap resampling
functions, i.e. \emph{resampLmer}, \emph{resampLmer2}, \emph{resampLmer3},
\emph{resampGlmer}, \emph{resampGlmer2}, and \emph{resampGlmer3}. Or any of the
jackknife resampling functions, i.e. \emph{JackLmer}, \emph{JackLmer2},
\emph{JackLmer3}, \emph{JackGlmer}, \emph{JackGlmer2}, and \emph{JackGlmer3}.
Produces a simple legend.
}
\examples{

##Import bootstrap resampled data model results
data(chinook_bootL) #Chinook salmon length
boxMANA(comp=chinook_bootL) #Default plot
boxMANA(comp=chinook_bootL,yunit=20,ymax=100,cex_ylab=1.3,leg="topleft")

##Group length and survival together in the same plot
data(chinook_bootS) #Chinook salmon survival
chinook_bootL$trait<- "length"; chinook_bootS$trait<- "survival"
comb_boot<- rbind(chinook_bootL[,-2],chinook_bootS) #remove 'tray'
comb_boot$trait<- as.factor(comb_boot$trait)
#
boxMANA(comp=comb_boot) #Default plot
boxMANA(comp=comb_boot,yunit=20,ymax=100,cex_ylab=1.3)
}