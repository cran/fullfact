resampLmer2 <-
function(resamp,dam,sire,response,start,end,position=NULL,block=NULL,ml=F) {
  if (missing(resamp)) stop("Need the resampled data frame")
  if (missing(dam)) stop("Need the dam column name")
  if (missing(sire)) stop("Need the sire column name")
  if (missing(response)) stop("Need the response column name")
  if (missing(start)) stop("Need the starting model number")
  if (missing(end)) stop("Need the ending model number")
  print(time1<- Sys.time()) #start time
#column labels
  response2<- colnames(resamp)[grep(paste(response), colnames(resamp))]
  dam2<- colnames(resamp)[grep(paste(dam), colnames(resamp))]
  sire2<- colnames(resamp)[grep(paste(sire), colnames(resamp))]
#Model: no position and no block
 if (is.null(position) && is.null(block)) {
  mod<- matrix(0,ncol=4,nrow=length(response2))   #variance
 for (j in start:end) {
  print(paste("Working on model: ", j, sep=""))
 if (ml == F) {
  m<- lmer(formula= noquote(paste(response2[j],"~ (1|",dam2[j],") + (1|",sire2[j],") + (1|",dam2[j],":",sire2[j],")",sep="")),
    data=resamp) }
 if (ml == T) {
  m<- lmer(formula= noquote(paste(response2[j],"~ (1|",dam2[j],") + (1|",sire2[j],") + (1|",dam2[j],":",sire2[j],")",sep="")),
    data=resamp, REML=F) }
  mod[j,]<- c(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2)
  col_names<- as.data.frame(VarCorr(m))$grp; rm(m)   } }
#Model: YES position and no block
 if (!is.null(position) && is.null(block)) {
  posi<- colnames(resamp)[grep(paste(position), colnames(resamp))]
  mod<- matrix(0,ncol=5,nrow=length(response2))   #variance
 for (j in start:end) {
  print(paste("Working on model: ", j, sep=""))
 if (ml == F) {
  m<- lmer(formula= noquote(paste(response2[j],"~ (1|",dam2[j],") + (1|",sire2[j],") + (1|",dam2[j],":",sire2[j],") + (1|",posi[j],")",sep="")),
    data=resamp) }
 if (ml == T) {
  m<- lmer(formula= noquote(paste(response2[j],"~ (1|",dam2[j],") + (1|",sire2[j],") + (1|",dam2[j],":",sire2[j],") + (1|",posi[j],")",sep="")),
    data=resamp, REML=F) }
  mod[j,]<- c(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2)
  col_names<- as.data.frame(VarCorr(m))$grp; rm(m)    } }
#Model: no position and YES block
 if (is.null(position) && !is.null(block)) {
  bloc<- colnames(resamp)[grep(paste(block), colnames(resamp))]
  mod<- matrix(0,ncol=5,nrow=length(response2))   #variance
 for (j in start:end) {
  print(paste("Working on model: ", j, sep=""))
 if (ml == F) {
  m<- lmer(formula= noquote(paste(response2[j],"~ (1|",dam2[j],") + (1|",sire2[j],") + (1|",dam2[j],":",sire2[j],") + (1|",bloc[j],")",sep="")),
    data=resamp) }
 if (ml == T) {
  m<- lmer(formula= noquote(paste(response2[j],"~ (1|",dam2[j],") + (1|",sire2[j],") + (1|",dam2[j],":",sire2[j],") + (1|",bloc[j],")",sep="")),
    data=resamp, REML=F) }
  mod[j,]<- c(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2)
  col_names<- as.data.frame(VarCorr(m))$grp; rm(m)    } }
#Model: YES position and YES block
 if (!is.null(position) && !is.null(block)) {
  posi<- colnames(resamp)[grep(paste(position), colnames(resamp))]
  bloc<- colnames(resamp)[grep(paste(block), colnames(resamp))]
  mod<- matrix(0,ncol=6,nrow=length(response2))   #variance
 for (j in start:end) {
  print(paste("Working on model: ", j, sep=""))
 if (ml == F) {
  m<- lmer(formula= noquote(paste(response2[j],"~ (1|",dam2[j],") + (1|",sire2[j],") + (1|",dam2[j],":",sire2[j],") + (1|",posi[j],") + (1|",bloc[j],")",sep="")),
    data=resamp) }
 if (ml == T) {
  m<- lmer(formula= noquote(paste(response2[j],"~ (1|",dam2[j],") + (1|",sire2[j],") + (1|",dam2[j],":",sire2[j],") + (1|",posi[j],") + (1|",bloc[j],")",sep="")),
    data=resamp, REML=F) }
  mod[j,]<- c(colSums(diag(VarCorr(m))),attr(VarCorr(m),"sc")^2)
  col_names<- as.data.frame(VarCorr(m))$grp; rm(m)   } }
#combining together
   comp<- as.data.frame(mod)
   colnames(comp)<- col_names
   comp$Total<- rowSums(comp)
#remove last number
colnames(comp)<- gsub(end,'', colnames(comp))
#Maternal, additive, nonadditive
  temp<- comp #to not override column names
  colnames(temp)[which(colnames(temp)==dam)]<- "dam"
  colnames(temp)[which(colnames(temp)==sire)]<- "sire"
  colnames(temp)[which(colnames(temp)==noquote(paste(dam,":",sire,sep="")))]<- "dam:sire"
  comp$additive<- 4*temp$sire
  comp$nonadd<- 4*temp$'dam:sire'
  comp$maternal<- temp$dam- temp$sire
#
   print(Sys.time()- time1) #end time
   invisible(comp)  #after time
}
