\name{barMANA}
\alias{barMANA}
\title{Bargraph of confidence intervals}
\description{
A simple bargraph function for confidence intervals of additive genetic, non-additive
genetic, and maternal variance components. Also, plots the median for the bootstrap
resampling method or mean of the pseudo-values for the jackknife resampling method.
}
\usage{
barMANA(ci_dat, type = "perc", bar_len = 0.1, ymax = NULL, ymin = NULL, yunit = NULL,
leg = "topright", cex_ylab = 1, cex_yaxis = 1, cex_names = 1)
}
\arguments{
  \item{ci_dat}{Data frame of a confidence interval function.}
  \item{type}{
Default is "perc" for percentage values of variance components. Other option is "raw"
for raw values of variance components.
}
  \item{bar_len}{Length of error bar in inches.}
  \item{ymax}{Maximum value of the y-axis.}
  \item{ymin}{Minimum value of the y-axis.}
  \item{yunit}{Unit increment of the y-axis.}
  \item{leg}{Position of the simple legend.}
  \item{cex_ylab}{Magnification of the y-axis label.}
  \item{cex_yaxis}{Magnification of the y-axis units.}
  \item{cex_names}{Optional magnification of trait labels.}
}
\details{
Plots a bargraph with the median or mean as the top of the shaded bar and error bars covering the
range of the confidence interval. Uses an object produced by any of the bootstrap resampling CI
functions, i.e. \emph{ciMANA}, \emph{ciMANA2}, and \emph{ciMANA3} or jackknife resampling functions,
i.e. \emph{ciJack}, \emph{ciJack2}, and \emph{ciJack3}. The median is plotted for bootstrap
resampling and the mean of pseudo-value for jackknife resampling. Produces a simple legend.
The function can plot several bar graphs grouped by \emph{label} to visualize several
phenotypic traits.
}
\examples{

##Import jackknife resampling results
data(chinook_jackL) #Chinook salmon length
length_ci<- ciJack(comp=chinook_jackL,full=c(0,0.7192,0.2030,1.0404))
barMANA(ci_dat=length_ci)  #default plot
barMANA(ci_dat=length_ci,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3) 

##Group length and survival together in the same plot
data(chinook_bootS) #Chinook salmon survival (bootstrap resampling)
length_ci<- ciJack(comp=chinook_jackL,full=c(0,0.7192,0.2030,1.0404),trait="length")
survival_ci<- ciMANA(comp=chinook_bootS,trait="survival")
colnames(length_ci$raw)[3]<- "median"; colnames(length_ci$percentage)[3]<- "median"
comb_bar<- list(raw=rbind(length_ci$raw,survival_ci$raw),
percentage=rbind(length_ci$percentage,survival_ci$percentage))
#
barMANA(ci_dat=comb_bar) #default plot
barMANA(ci_dat=comb_bar,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3)
}