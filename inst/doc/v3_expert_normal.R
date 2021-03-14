## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE, message= FALSE, comment = "#>")

## ----package-data-------------------------------------------------------------
library("fullfact")     
data(chinook_length)
head(chinook_length)

## ----observed-vc--------------------------------------------------------------
#>length_mod3<- observLmer3(observ=chinook_length,dam="dam",sire="sire",response="length",
#>remain="egg_size + (1|tray)",iter=1000) #full
length_mod3<- observLmer3(observ=chinook_length,dam="dam",sire="sire",response="length",
remain="egg_size + (1|tray)",iter=100)
length_mod3

## ----power-analysis-----------------------------------------------------------
#Reworking the Chinook salmon data set to contain only integers for `design`
#dam ID, sire ID, family ID
desn0<- data.frame(dam=rep(1:11,each=11),sire=rep(1:11,11),family=1:(11*11))
#replicate for offspring sample size (10)
desn<- do.call("rbind", replicate(10,desn0,simplify=F)); rm(desn0) 
desn$tray<- rep(1:11,each=nrow(desn)/11) #equal number of offspring per tray 
desn$tray<- sample(desn$tray,nrow(desn)) #shuffle tray numbers
desn$egg_size<- desn$dam #egg size is related to dam
head(desn)
#>powerLmer3(var_rand=c(0.1461,0,0.1788,0.5499,0.1198),n_rand=c(11,11,121,11),var_fix=0.0597,
#>n_fix=11,design=desn,remain="(1|tray) + egg_size") #full with LR
#full with PB and 1000 iterations
#>powerLmer3(var_rand=c(0.1461,0,0.1788,0.5499,0.1198),n_rand=c(11,11,121,11),var_fix=0.0597,
#>n_fix=11,design=desn,remain="(1|tray) + egg_size",ftest="PB",iter=1000) 
powerLmer3(var_rand=c(0.1461,0,0.1788,0.5499,0.1198),n_rand=c(11,11,121,11),var_fix=0.0597,
n_fix=11,design=desn,remain="(1|tray) + egg_size",nsim=25) #25 simulations with LR

## ----power-analysis2----------------------------------------------------------
#dam ID, sire ID, family ID
desn0_2<- data.frame(dam=rep(1:7,each=7),sire=rep(1:7,7),family=1:(7*7))
#replicate for offspring sample size (10)
desn_2<- do.call("rbind", replicate(10,desn0_2,simplify=F)); rm(desn0) 
desn_2$tray<- rep(1:10,each=nrow(desn_2)/10) #equal number of offspring per tray 
desn_2$tray<- sample(desn_2$tray,nrow(desn_2)) #shuffle tray numbers
desn_2$egg_size<- desn_2$dam #egg size is related to dam
#>powerLmer3(var_rand=c(0.1461,0,0.1788,0.5499,0.1198),n_rand=c(7,7,49,10),var_fix=0.0597,
#>n_fix=7,design=desn_2,remain="(1|tray) + egg_size") #full with LR
#full with PB and 1000 iterations
#>powerLmer3(var_rand=c(0.1461,0,0.1788,0.5499,0.1198),n_rand=c(7,7,49,10),var_fix=0.0597,
#>n_fix=7,design=desn_2,remain="(1|tray) + egg_size",ftest="PB",iter=1000) 
powerLmer3(var_rand=c(0.1461,0,0.1788,0.5499,0.1198),n_rand=c(7,7,49,10),var_fix=0.0597,
n_fix=7,design=desn_2,remain="(1|tray) + egg_size",nsim=25) #25 simulations with LR

## ----resample, eval=FALSE-----------------------------------------------------
#  #>resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",iter=1000) #full
#  #>resampFamily(dat=chinook_length,copy=c(3:8),family="family",iter=1000) #family only
#  resampRepli(dat=chinook_length,copy=c(3:8),family="family",replicate="repli",iter=5) #5 iterations

## ----boot-vc------------------------------------------------------------------
#>length_datR<- read.csv("resamp_datR.csv") #1000 iterations
#>length_rcomp3<- resampLmer3(resamp=length_datR,dam="dam",sire="sire",response="length",
#>remain="egg_size# + (1|tray#)",start=1,end=1000) #full
data(chinook_resampL) #5 iterations
head(chinook_resampL)
length_rcomp3<- resampLmer3(resamp=chinook_resampL,dam="dam",sire="sire",response="length",
remain="egg_size# + (1|tray#)",start=1,end=5)
length_rcomp3[1:5,]

## ----boot-ci------------------------------------------------------------------
#>ciMANA3(comp=length_rcomp3,remain=c("tray","Fixed")) #full, with egg size as Fixed
data(chinook_bootL) #similar to length_rcomp3 1000 models, but has no Fixed
ciMANA3(comp=chinook_bootL,remain=c("tray","Residual"))

## ----boot-ci-bias-------------------------------------------------------------
#bias only
ciMANA3(comp=chinook_bootL,remain=c("tray","Residual"),bias=c(0,0.7192,0.2030,0.1077,0.5499))
#full, with egg size as Fixed, observLmer3 components
#>ciMANA3(comp=length_rcomp3,remain=c("tray","Fixed"),bias=c(0,0.7152,0.1461,0.1198,0.0567)) 

## ----boot-ci-bias-accel-------------------------------------------------------
data(chinook_jackL)
#bias and acceleration
ciMANA3(comp=chinook_bootL,remain=c("tray","Residual"),bias=c(0,0.7192,0.2030,0.1077,0.5499),
accel=chinook_jackL)
#full, with egg size as Fixed, observLmer3 components
#>ciMANA3(comp=length_rcomp3,remain=c("tray","Fixed"),bias=c(0,0.7152,0.1461,0.1198,0.0567),
#>accel=length_jack3) 

## ----jack-vc------------------------------------------------------------------
#>length_jack3<- JackLmer3(observ=chinook_length,dam="dam",sire="sire",response="length",
#>remain="egg_size + (1|tray)") #full, all observations
length_jack3<- JackLmer3(observ=chinook_length,dam="dam",sire="sire",response="length",
remain="egg_size + (1|tray)",first=10) #first 10 observations
head(length_jack3)

## ----jack-vc-d----------------------------------------------------------------
#>length_jack3D<- JackLmer3(observ=chinook_length,dam="dam",sire="sire",response="length",
#>remain="egg_size + (1|tray)",size=5) #full
length_jack3D<- JackLmer3(observ=chinook_length,dam="dam",sire="sire",response="length",
remain="egg_size + (1|tray)",size=5,first=10) #first 10 
head(length_jack3D)

## ----jack-ci------------------------------------------------------------------
data(chinook_jackL) #similar to length_jack3 all observations
ciJack3(comp=chinook_jackL,remain=c("tray","Residual"),full=c(0,0.7192,0.2030,1.0404,0.1077,
0.5499))
#full, all observations, with egg size as Fixed, observLmer3 components
#>ciJack3(comp=length_jack3,remain=c("tray","Fixed"),full=c(0,0.7152,0.1461,1.0543,0.1198,
#>0.0597)) 

## ----barplot, fig.width=7, fig.height=10--------------------------------------
length_ci<- ciJack3(comp=chinook_jackL,remain=c("tray","Residual"),full=c(0,0.7192,0.2030,1.0404,
0.1077,0.5499))
oldpar<- par(mfrow=c(2,1))
barMANA(ci_dat=length_ci) #basic, top
barMANA(ci_dat=length_ci,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3) #modified, bottom

## ----barplot-comb, fig.width=7, fig.height=5----------------------------------
length_ci1<- ciJack3(comp=chinook_jackL,remain=c("tray","Residual"),full=c(0,0.7192,0.2030,1.0404,
0.1077,0.5499),trait="length_1")
length_ci2<- ciJack3(comp=chinook_jackL,remain=c("tray","Residual"),full=c(0,0.7192,0.2030,1.0404,
0.1077,0.5499),trait="length_2")
comb_bar<- list(raw=rbind(length_ci1$raw,length_ci2$raw),
percentage=rbind(length_ci1$percentage,length_ci2$percentage)) 
barMANA(ci_dat=comb_bar,bar_len=0.3,yunit=20,ymax=100,cex_ylab=1.3)

## ----boxplot, fig.width=7, fig.height=10--------------------------------------
oldpar<- par(mfrow=c(2,1))
boxMANA(comp=chinook_bootL) #from resampLmer3, basic, top 
boxMANA(comp=chinook_bootL,yunit=20,ymax=100,cex_ylab=1.3,leg="topleft") #modified, bottom

## ----boxplot-comb, fig.width=7, fig.height=5----------------------------------
chinook_bootL1<- chinook_bootL; chinook_bootL2<- chinook_bootL #from resampLmer3
chinook_bootL1$trait<- "length_1"; chinook_bootL2$trait<- "length_2"
comb_boot<- rbind(chinook_bootL1,chinook_bootL2)
comb_boot$trait<- as.factor(comb_boot$trait)
boxMANA(comb_boot,yunit=20,ymax=100,cex_ylab=1.3,leg="topleft")

