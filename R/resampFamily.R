resampFamily <-
function(dat,copy,family,iter) {
 if (missing(dat)) stop("Need data frame to resample")
 if (missing(copy)) stop("Need column numbers to copy")
 if (missing(family)) stop("Need the family column name")
 if (missing(iter)) stop("Need number of iterations")
 print(time1<- Sys.time())  #start time
#family column number
 fam2<- grep(paste(family), colnames(dat))
 families<- length(levels(as.factor(dat[,fam2])))  #count the number of families
#loop through all families
 for (fam in 1:families) {
#get the family name for this iteration, with the fam variable
  family2<- levels(as.factor(dat[,fam2]))[fam]
  print(paste("Starting family: ", fam, " - id: ", family2, sep=""))
#select data with the family name
  target<- dat[dat[,fam2]==family2,]
#constants
    num<- length(target[,1]) #number of individuals in family
    samp<- num
    print(paste("Individuals found: ", num, sep=""))
    resamp<- array(0,dim=c(samp,length(copy),iter))
#randomly select a number from 1 to the size of the family
    for (i in 1:samp) { for (j in 1:length(copy)) { for (k in 1:iter) {
      resamp[,1,k]<- sample(1:num,samp,replace=T) }}}  #temporary: column 1 contains the sampled row number
#replace row number with the information in the data
    for (i in 1:samp) { for (k in 1:iter) { for (z in 1:num) {   #row number gone, copied over
      if (resamp[i,1,k] == z) { resamp[i,,k][1:length(copy)]<- as.character(target[z,][copy]) } }}}
#saving file as .csv
    file_name<- paste(family2, "_resampF.csv", sep="")
    write.table(resamp,file_name,sep=",",row.names=F)
 } #end family loop
#combine files
  files <- list.files(pattern="_resampF.csv")
  combine <- do.call("rbind", lapply(files, read.csv, header = TRUE))
#column names
  colnames(combine)<- rep(colnames(dat)[copy],iter)  #general copy
  first<- seq(1,length(colnames(combine)),length(copy))  #find first of iteration
  last<- seq(length(copy),length(colnames(combine)),length(copy))  #find last of iteration
  for (i in 1:iter) {
    colnames(combine)[first[i]:last[i]]<- paste(colnames(combine)[first[i]:last[i]],i,sep="") }
   write.table(combine,"resamp_datF.csv",sep=",",row.names=F)
#
print(Sys.time()- time1) #end time
}
