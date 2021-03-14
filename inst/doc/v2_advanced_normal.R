## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE, message= FALSE, comment = "#>")

## ----package-data-------------------------------------------------------------
library("fullfact")     
data(chinook_length)
head(chinook_length)

## ----observed-vc--------------------------------------------------------------
length_mod2<- observLmer2(observ=chinook_length,dam="dam",sire="sire",response="length",
position="tray")
length_mod2

## ----power-analysis-----------------------------------------------------------
#>powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077),nval=c(11,11,10,110),position=11) #full
#25 simulations
powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077),nval=c(11,11,10,110),position=11,nsim=25)

#Block examples using 8 dams, 8 sires (as four 2x2 blocks), and 20 offspring per family
#>powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077),nval=c(8,8,20,4),block=c(2,2)) 
#>powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077,0.1077),nval=c(8,8,20,40,4),position=8,
#>block=c(2,2)) #with position

## ----power-analysis2----------------------------------------------------------
#>powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077),nval=c(7,7,10,49),position=10) #full
#25 simulations
powerLmer2(varcomp=c(0.2030,0,0.1798,0.5499,0.1077),nval=c(7,7,10,49),position=10,nsim=25)

## ----resample, eval=FALSE-----------------------------------------------------
#  #>resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",iter=1000) #full
#  #>resampFamily(dat=chinook_length,copy=c(3:8),family="family",iter=1000) #family only
#  resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",iter=5) #5 iterations

## ----boot-vc------------------------------------------------------------------
#>length_datR<- read.csv("resamp_datR.csv") #1000 iterations
#>length_rcomp2<- resampLmer2(resamp=length_datR,dam="dam",sire="sire",response="length",
#>position="tray",start=1,end=1000) #full
data(chinook_resampL) #5 iterations
head(chinook_resampL)
length_rcomp2<- resampLmer2(resamp=chinook_resampL,dam="dam",sire="sire",response="length",
position="tray",start=1,end=5)
length_rcomp2[1:5,]

## ----boot-ci------------------------------------------------------------------
#>ciMANA(comp=length_rcomp1) #full
data(chinook_bootL) #same as length_rcomp2 1000 models
ciMANA2(comp=chinook_bootL,position="tray")

## ----boot-ci-bias-------------------------------------------------------------
ciMANA2(comp=chinook_bootL,position="tray",bias=c(0,0.7192,0.2030,0.1077)) #bias only

## ----boot-ci-bias-accel-------------------------------------------------------
data(chinook_jackL)
#bias and acceleration
ciMANA2(comp=chinook_bootL,position="tray",bias=c(0,0.7192,0.2030,0.1077),accel=chinook_jackL)

## ----jack-vc------------------------------------------------------------------
#>length_jack2<- JackLmer2(observ=chinook_length,dam="dam",sire="sire",response="length",
#>position="tray") #full, all observations
length_jack2<- JackLmer2(observ=chinook_length,dam="dam",sire="sire",response="length",
position="tray",first=10) #first 10 observations
head(length_jack2)

## ----jack-vc-d----------------------------------------------------------------
#>length_jack2D<- JackLmer2(observ=chinook_length,dam="dam",sire="sire",response="length",
#>position="tray",size=5) #full
length_jack2D<- JackLmer2(observ=chinook_length,dam="dam",sire="sire",response="length",
position="tray",size=5,first=10) #first 10
head(length_jack2D)

## ----jack-ci------------------------------------------------------------------
#full, all observations
#>ciJack2(comp=length_jack2,position="tray",full=c(0,0.7192,0.2030,1.0404,0.1077))
data(chinook_jackL) #same as length_jack2 all observations
ciJack2(comp=chinook_jackL,position="tray",full=c(0,0.7192,0.2030,1.0404,0.1077))

## ----barplot, fig.width=7, fig.height=10--------------------------------------
length_ci<- ciJack2(comp=chinook_jackL,position="tray",full=c(0,0.7192,0.2030,1.0404,0.1077))
oldpar<- par(mfrow=c(2,1))
barMANA(ci_dat=length_ci) #basic, top
barMANA(ci_dat=length_ci,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3) #modified, bottom

## ----barplot-comb, fig.width=7, fig.height=5----------------------------------
length_ci1<- ciJack2(comp=chinook_jackL,position="tray",full=c(0,0.7192,0.2030,1.0404,0.1077),
trait="length_1")
length_ci2<- ciJack2(comp=chinook_jackL,position="tray",full=c(0,0.7192,0.2030,1.0404,0.1077),
trait="length_2")
comb_bar<- list(raw=rbind(length_ci1$raw,length_ci2$raw),
percentage=rbind(length_ci1$percentage,length_ci2$percentage)) 
barMANA(ci_dat=comb_bar,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3)

## ----boxplot, fig.width=7, fig.height=10--------------------------------------
oldpar<- par(mfrow=c(2,1))
boxMANA(comp=chinook_bootL) #from resampLmer2, basic, top 
boxMANA(comp=chinook_bootL,yunit=20,ymax=100,cex_ylab=1.3,leg="topleft") #modified, bottom

## ----boxplot-comb, fig.width=7, fig.height=5----------------------------------
chinook_bootL1<- chinook_bootL; chinook_bootL2<- chinook_bootL #from resampLmer2
chinook_bootL1$trait<- "length_1"; chinook_bootL2$trait<- "length_2"
comb_boot<- rbind(chinook_bootL1,chinook_bootL2)
comb_boot$trait<- as.factor(comb_boot$trait)
boxMANA(comb_boot,yunit=20,ymax=100,cex_ylab=1.3,leg="topleft")

