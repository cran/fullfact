## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE, message= FALSE, comment = "#>")

## ----package-data-------------------------------------------------------------
library("fullfact")     
data(chinook_length)
head(chinook_length)

## ----observed-vc--------------------------------------------------------------
length_mod1<- observLmer(observ=chinook_length,dam="dam",sire="sire",response="length")
length_mod1

## ----power-analysis-----------------------------------------------------------
#>powerLmer(varcomp=c(0.1900,0,0.1719,0.6315),nval=c(11,11,10)) #full
powerLmer(varcomp=c(0.1900,0,0.1719,0.6315),nval=c(11,11,10),nsim=25) #25 simulations

## ----power-analysis2----------------------------------------------------------
#>powerLmer(varcomp=c(0.1900,0,0.1719,0.6315),nval=c(7,7,10)) #full
powerLmer(varcomp=c(0.1900,0,0.1719,0.6315),nval=c(7,7,10),nsim=25) #25 simulations

## ----resample, eval=FALSE-----------------------------------------------------
#  #>resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",iter=1000) #full
#  #>resampFamily(dat=chinook_length,copy=c(3:8),family="family",iter=1000) #family only
#  resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",iter=5) #5 iterations

## ----boot-vc------------------------------------------------------------------
#>length_datR<- read.csv("resamp_datR.csv") #1000 iterations
#>length_rcomp1<- resampLmer(resamp=length_datR,dam="dam",sire="sire",response="length",
#>start=1,end=1000) #full
data(chinook_resampL) #5 iterations
head(chinook_resampL)
length_rcomp1<- resampLmer(resamp=chinook_resampL,dam="dam",sire="sire",response="length",
start=1,end=5)
length_rcomp1[1:5,]

## ----boot-ci------------------------------------------------------------------
#>ciMANA(comp=length_rcomp1) #full
data(chinook_bootL) #same as length_rcomp1 1000 models
ciMANA(comp=chinook_bootL)

## ----boot-ci-bias-------------------------------------------------------------
ciMANA(comp=chinook_bootL,bias=c(0,0.7192,0.2030)) #bias only
#>ciMANA(comp=length_rcomp1,bias=c(0,0.6878,0.1900)) #full, observLmer components

## ----boot-ci-bias-accel-------------------------------------------------------
data(chinook_jackL)
ciMANA(comp=chinook_bootL,bias=c(0,0.7192,0.2030),accel=chinook_jackL) #bias and acceleration
#>ciMANA(comp=length_rcomp1,bias=c(0,0.6878,0.1900),accel=length_jack1) #full, observLmer

## ----jack-vc------------------------------------------------------------------
#full, all observations
#>length_jack1<- JackLmer(observ=chinook_length,dam="dam",sire="sire",response="length")
#first 10 observations
length_jack1<- JackLmer(observ=chinook_length,dam="dam",sire="sire",response="length",first=10) 
head(length_jack1)

## ----jack-vc-d----------------------------------------------------------------
#full
#>length_jack1D<- JackLmer(observ=chinook_length,dam="dam",sire="sire",response="length",size=5)
#first 10
length_jack1D<- JackLmer(observ=chinook_length,dam="dam",sire="sire",response="length",
size=5,first=10)
head(length_jack1D)

## ----jack-ci------------------------------------------------------------------
data(chinook_jackL) #similar to length_jack1 all observations
ciJack(comp=chinook_jackL,full=c(0,0.7192,0.2030,1.0404))
#full, all observations, observLmer components
#>ciJack(comp=length_jack1,full=c(0,0.6878,0.1900,0.9935)) 

## ----barplot, fig.width=7, fig.height=10--------------------------------------
length_ci<- ciJack(comp=chinook_jackL,full=c(0,0.7192,0.2030,1.0404))
oldpar<- par(mfrow=c(2,1))
barMANA(ci_dat=length_ci) #basic, top
barMANA(ci_dat=length_ci,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3) #modified, bottom

## ----barplot-comb, fig.width=7, fig.height=5----------------------------------
length_ci1<- ciJack(comp=chinook_jackL,full=c(0,0.7192,0.2030,1.0404),trait="length_1")
length_ci2<- ciJack(comp=chinook_jackL,full=c(0,0.7192,0.2030,1.0404),trait="length_2")
comb_bar<- list(raw=rbind(length_ci1$raw,length_ci2$raw),
percentage=rbind(length_ci1$percentage,length_ci2$percentage)) 
barMANA(ci_dat=comb_bar,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3)

## ----boxplot, fig.width=7, fig.height=10--------------------------------------
oldpar<- par(mfrow=c(2,1))
boxMANA(comp=chinook_bootL) #from resampLmer, basic, top 
boxMANA(comp=chinook_bootL,yunit=20,ymax=100,cex_ylab=1.3,leg="topleft") #modified, bottom

## ----boxplot-comb, fig.width=7, fig.height=5----------------------------------
chinook_bootL1<- chinook_bootL; chinook_bootL2<- chinook_bootL #from resampLmer
chinook_bootL1$trait<- "length_1"; chinook_bootL2$trait<- "length_2"
comb_boot<- rbind(chinook_bootL1,chinook_bootL2)
comb_boot$trait<- as.factor(comb_boot$trait)
boxMANA(comb_boot,yunit=20,ymax=100,cex_ylab=1.3,leg="topleft")

