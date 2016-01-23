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

##Import bootstrap resampling results
data(chinook_bootS) #Chinook salmon offspring survival
#Extract un-corrected confidence interval
survival_ci<- ciMANA(comp=chinook_bootS,trait="survival")
survival_ci

#Default plot
barMANA(ci_dat=survival_ci)
#Add plot modifications
barMANA(ci_dat=survival_ci,bar_len=0.3,yunit=5,ymax=20,cex_ylab=1.3)

##Import jackknife resampling results
data(chinook_jackL)  #Chinook salmon offspring length
#Extract jackknife confidence interval
length_ci<- ciJack2(comp=chinook_jackL,full=c(0.0000000,0.7192253,0.2029684,1.0404425,
0.1077423),position="tray",trait="length")
length_ci

#Default plot
barMANA(ci_dat=length_ci)
#Add plot modifications
barMANA(ci_dat=length_ci,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3)

##Group survival and length together in the same plot
data(chinook_bootL) #Chinook salmon offspring length
length_ci2<- ciMANA2(comp=chinook_bootL,position="tray",trait="length")
length_ci2
#
comb_r<- rbind(survival_ci$raw,length_ci2$raw)
comb_p<- rbind(survival_ci$percentage,length_ci2$percentage)
comb_ci<- list(raw=comb_r,percentage=comb_p)

#Default plot
barMANA(ci_dat=comb_ci)
#Add plot modifications
barMANA(ci_dat=comb_ci,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3,leg="topleft")
}