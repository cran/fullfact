JackLmer2 <-
function(observ,dam,sire,response,position=NULL,block=NULL,ml=F,size=1,first=NULL) {
  if (missing(observ)) stop("Need the observed data frame")
  if (missing(dam)) stop("Need the dam column name")
  if (missing(sire)) stop("Need the sire column name")
  if (missing(response)) stop("Need the response column name")
  if (size %% 1 != 0) stop("Size needs to be a positive integer")
  if (size %% 1 == 0 && size < 0) stop("Size needs to be a positive integer")
  if (!is.null(first) && first %% 1 != 0) stop("First needs to be a positive integer")
  if (!is.null(first) && first %% 1 == 0 && first < 0) stop("First needs to be a positive integer")
  print(time1<- Sys.time()) #start time
##Drop a single-observation
if (size == 1) {
#conditions
 if (is.null(position) && is.null(block)) { jack<- matrix(0,ncol=5,nrow=nrow(observ)) }
 if (!is.null(position) && is.null(block)) { jack<- matrix(0,ncol=6,nrow=nrow(observ)) }
 if (is.null(position) && !is.null(block)) { jack<- matrix(0,ncol=6,nrow=nrow(observ)) }
 if (!is.null(position) && !is.null(block)) { jack<- matrix(0,ncol=7,nrow=nrow(observ)) }
#
 if (is.null(first)) { iter<- nrow(observ) }
 if (!is.null(first)) { iter<- first }
  for (i in 1:iter) {
    print(paste0("Removing observation: ",i," of ",length(observ[,1])))
#Model
if (is.null(position) && is.null(block)) {
  if (ml == F) {
    m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,")",sep="")),data=observ[-i,]) }
  if (ml == T) {
    m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,")",sep="")),data=observ[-i,],REML=F) } }
if (!is.null(position) && is.null(block)) {
  if (ml == F) {
    m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",position,")",sep="")),
      data=observ[-i,]) }
  if (ml == T) {
    m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",position,")",sep="")),
      data=observ[-i,],REML=F) } }
if (is.null(position) && !is.null(block)) {
  if (ml == F) {
    m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",block,")",sep="")),
      data=observ[-i,]) }
  if (ml == T) {
    m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",block,")",sep="")),
      data=observ[-i,],REML=F) } }
if (!is.null(position) && !is.null(block)) {
  if (ml == F) {
    m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",position,") + (1|",block,")",sep="")),
      data=observ[-i,]) }
  if (ml == T) {
    m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",position,") + (1|",block,")",sep="")),
      data=observ[-i,],REML=F) } }
#Components
  tot<- sum(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2)
  jack[i,]<- c(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2,tot)
  col_names<- c(as.data.frame(VarCorr(m))$grp,"Total")  } #end loop
} #end single Jack
##Drop a a block
if (size > 1) {
 observS1 <- observ[sample(nrow(observ)),];rm(observ) #shuffle once
 observS2 <- observS1[sample(nrow(observS1)),];rm(observS1) #shuffle twice
 n_mod<- floor(nrow(observS2)/size)  #number of models in data frame
#conditions
 if (is.null(position) && is.null(block)) { jack<- matrix(0,ncol=5,nrow=n_mod) }
 if (!is.null(position) && is.null(block)) { jack<- matrix(0,ncol=6,nrow=n_mod) }
 if (is.null(position) && !is.null(block)) { jack<- matrix(0,ncol=6,nrow=n_mod) }
 if (!is.null(position) && !is.null(block)) { jack<- matrix(0,ncol=7,nrow=n_mod) }
#
 if (is.null(first)) { iter<- n_mod }
 if (!is.null(first)) { iter<- first }
#begining values
 beg<- matrix(0,ncol=1,nrow=n_mod)
 beg[1]<- 1 #always
 for (i in 2:n_mod) { beg[i]<- beg[i-1] + size }
#finishing values
 finn<- matrix(0,ncol=1,nrow=n_mod)
 finn[1]<- size #always
 for (i in 2:n_mod) { finn[i]<- finn[i-1] + size }
#jacking
 for (i in 1:iter) {
  print(paste0("Removing block: ",i," of ",n_mod))
#Model
if (is.null(position) && is.null(block)) {
  if (ml == F) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,")",sep="")),
    data=observS2[-c(beg[i]:finn[i]),]) }
  if (ml == T) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,")",sep="")),
    data=observS2[-c(beg[i]:finn[i]),],REML=F) } }
if (!is.null(position) && is.null(block)) {
  if (ml == F) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",position,")",sep="")),
    data=observS2[-c(beg[i]:finn[i]),]) }
  if (ml == T) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",position,")",sep="")),
    data=observS2[-c(beg[i]:finn[i]),],REML=F) } }
if (is.null(position) && !is.null(block)) {
  if (ml == F) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",block,")",sep="")),
    data=observS2[-c(beg[i]:finn[i]),]) }
  if (ml == T) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",block,")",sep="")),
    data=observS2[-c(beg[i]:finn[i]),],REML=F) } }
if (!is.null(position) && !is.null(block)) {
  if (ml == F) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",position,") + (1|",block,")",sep="")),
    data=observS2[-c(beg[i]:finn[i]),]) }
  if (ml == T) {
  m<- lmer(formula= noquote(paste(response,"~ (1|",dam,") + (1|",sire,") + (1|",dam,":",sire,") + (1|",position,") + (1|",block,")",sep="")),
    data=observS2[-c(beg[i]:finn[i]),],REML=F) } }
#Components
  tot<- sum(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2)
  jack[i,]<- c(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2,tot)
  col_names<- c(as.data.frame(VarCorr(m))$grp,"Total")  } #end loop
}  #end block Jack
#convert to data frame
 jack<- as.data.frame(jack); colnames(jack)<- col_names
#Maternal, additive, nonadditive
  temp<- jack #to not override column names
  colnames(temp)[which(colnames(temp)==dam)]<- "dam"
  colnames(temp)[which(colnames(temp)==sire)]<- "sire"
  colnames(temp)[which(colnames(temp)==noquote(paste(dam,":",sire,sep="")))]<- "dam:sire"
  jack$additive<- 4*temp$sire
  jack$nonadd<- 4*temp$'dam:sire'
  jack$maternal<- temp$dam- temp$sire
#
 print(Sys.time()- time1) #end time
 invisible(jack)  #after time
}
