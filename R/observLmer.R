observLmer <-
function(observ,dam,sire,response,ml=F) {
  if (missing(observ)) stop("Need the observed data frame")
  if (missing(dam)) stop("Need the dam column name")
  if (missing(sire)) stop("Need the sire column name")
  if (missing(response)) stop("Need the response column name")
  print(time1<- Sys.time()) #start time
#Model:
if (ml == F) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,")",sep="")),data=observ) }
if (ml == T) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,")",sep="")),data=observ,REML=F) }
#Components
  tot<- sum(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2)
  comp<- data.frame(effect= as.data.frame(VarCorr(m))$grp[1:3],variance= colSums(diag(VarCorr(m)))[1:3])
  other<- data.frame(component= c("Residual","Total"), variance=c(attr(VarCorr(m),"sc")^2,tot))
  comp$percent<- 100*comp$variance/tot; other$percent<- 100*other$variance/tot
#Likelihood ratio test: random effects
   comp$d.AIC<- NA;comp$d.BIC<- NA;comp$Chi.sq<- NA;comp$p.value<- NA
   p_rand<- randLmer(model=m,observ=observ)  #internal function
 #random term matching
   r_term0<- sapply(findbars(formula(m)),function(x) paste0("(", deparse(x), ")"))
   r_term1<- unlist(strsplit(sapply(findbars(formula(m)),function(x) deparse(x))," | "))  #split around | text
   r_term<- r_term1[seq(3,length(r_term1),3)]  #every third matches comp
 for (i in  1:length(r_term0)) {
   comp[,c(4:7)][which(comp$effect==r_term[i]),]<- p_rand[,c(2:5)][which(p_rand$term==r_term0[i]),]  }
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
  var_obj<- list(random=comp,other=other,calculation=comp2)
#Finish
   print(Sys.time()- time1) #end time, keep no quote in one line
   invisible(var_obj)  #after time
}
