observLmer3 <-
function(observ,dam,sire,response,remain,ml=F,iter=1000) {  #change to 1000
  if (missing(observ)) stop("Need the observed data frame")
  if (missing(dam)) stop("Need the dam column name")
  if (missing(sire)) stop("Need the sire column name")
  if (missing(response)) stop("Need the response column name")
  if (missing(remain)) stop("Need the remaining formula")
  print(time1<- Sys.time()) #start time
#model
if (ml == F) {
  m<- lmer(formula= noquote(paste(response,"~ (1| ",dam,") + (1| ",sire,") + (1|",dam,":",sire,") +",remain,sep="")),
    data=observ) }
if (ml == T) {
  m<- lmer(formula= noquote(paste(response,"~ (1| ",dam,") + (1| ",sire,") + (1|",dam,":",sire,") +",remain,sep="")),
    data=observ, REML=F) }
#other calcs
n<- length(as.data.frame(VarCorr(m))$vcov)- 1 #minus residual
 if (length(fixef(m)) > 1) { 
  var_fixed <- var(as.vector(fixef(m) %*% t(m@pp$X))) #total variation of fixed effects
  tot<- sum(as.data.frame(VarCorr(m))$vcov[1:n],attr(VarCorr(m),"sc")^2,var_fixed) }
 if (length(fixef(m)) == 1) { tot<- sum(as.data.frame(VarCorr(m))$vcov[1:n],attr(VarCorr(m),"sc")^2) }
#
  comp<- data.frame(effect= c(as.data.frame(VarCorr(m))$grp[1:n]),effect2=c(as.data.frame(VarCorr(m))$var1[1:n]),
      variance= c(as.data.frame(VarCorr(m))$vcov[1:n]))
  other<- data.frame(component= c("Residual","Total"), variance=c(attr(VarCorr(m),"sc")^2,tot))
  comp$percent<- 100*comp$variance/tot; other$percent<- 100*other$variance/tot
#Bootstrap: fixed effects
 if (length(fixef(m)) > 1) {
   fix_eff<- data.frame(effect=c(paste(attributes(terms(m))$term.labels),"Fix_Tot"),
    variance=c(rep(NA,length(attributes(terms(m))$term.labels)),var_fixed))
   fix_eff$percent<- 100*fix_eff$variance/tot
   fix_eff$Chi.sq<- NA;fix_eff$p.value<- NA
 #afex package
   p_fixed<- mixed(m,data=observ,method = "PB",args_test = list(nsim = iter))$anova_table[c(1,4)]
 #fixed term matching
   f_term<- paste(attributes(terms(m))$term.labels)
 for (i in  1:length(f_term)) {
   fix_eff[,c(4,5)][which(fix_eff$effect==f_term[i]),]<- p_fixed[which(rownames(p_fixed)==f_term[i]),]   } }
#Likelihood ratio test
   p_rand<- randLmer(model=m,observ=observ)  #internal function
   if (length(fixef(m)) > 1) { p_fix<- fixedLmer(model=m,observ=observ) }  #internal function
#Maternal, additive, nonadditive
  temp<- comp #to not override column names
  temp$effect<- factor(temp$effect)
  levels(temp$effect)[which(levels(temp$effect)==dam)]<- "dam"
  levels(temp$effect)[which(levels(temp$effect)==sire)]<- "sire"
  levels(temp$effect)[which(levels(temp$effect)==noquote(paste(dam,":",sire,sep="")))]<- "dam:sire"
  comp2<- data.frame(component=c("additive","nonadd","maternal"),variance=0,percent=0)
  comp2$variance<- c(4*temp$variance[which(temp$effect=="sire")],4*temp$variance[which(temp$effect=="dam:sire")],
    temp$variance[which(temp$effect=="dam")]- temp$variance[which(temp$effect=="sire")])
  comp2$percent<- 100*comp2$variance/tot
#object
  if (length(fixef(m)) == 1) { var_obj<- list(random=comp,LRT=p_rand,other=other,calculation=comp2) }
  if (length(fixef(m)) > 1) { var_obj<- list(fixed=fix_eff,LRT.fixed=p_fix,random=comp,LRT.random=p_rand,other=other,calculation=comp2) }
#Finish
   print(Sys.time()- time1) #end time, keep no quote in one line
   invisible(var_obj)  #after time
}
