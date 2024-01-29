resampRepli <-
function(dat,copy,family,replicate,iter) {
 if (missing(dat)) stop("Need data frame to resample")
 if (missing(copy)) stop("Need column numbers to copy")
 if (missing(family)) stop("Need the family column name")
 if (missing(replicate)) stop("Need the replicate column name")
 if (missing(iter)) stop("Need number of iterations")
 print(time1<- Sys.time())  #start time
#family and replicate column number
 fam2<- grep(paste(family), colnames(dat))
 rep2<- grep(paste(replicate), colnames(dat))
 families<- length(levels(as.factor(dat[,fam2])))  #count the number of families
#Loop through all families
 for (fam in 1:families) { 
#get the family name for this iteration, with the fam variable
  family2<- levels(as.factor(dat[,fam2]))[fam]
  print(paste("Starting family: ", fam, " - id: ", family2, sep=""))
#select data with the family name
  target1<- dat[dat[,fam2]==family2,]
#find out how many replicates are in this family
  num_rep<- length(levels(as.factor(target1[,rep2])))
  print(paste("Replicates found: ", num_rep, sep=""))
#loop through each replicate for this family
  for (rep in 1:num_rep) {
    print(paste("Working on replicate: ", rep, sep=""))
#select a replicate of family, with the rep variable
    target<- target1[target1[,rep2]==levels(as.factor(target1[,rep2]))[rep],]
#constants
    num<- length(target[,rep2]) #number of individuals in replicate
    samp<- num
    print(paste("Individuals found: ", num, sep=""))
    resamp<- array(0,dim=c(samp,length(copy),iter))
#randomly select a number from 1 to the size of replicate
    for (i in 1:samp) { for (j in 1:length(copy)) { for (k in 1:iter) {
      resamp[,1,k]<- sample(1:num,samp,replace=T) }}}  #temporary: column 1 contains the sampled row number
#replace row number with the information in the data
    for (i in 1:samp) { for (k in 1:iter) { for (z in 1:num) {   #row number gone, copied over   
      if (resamp[i,1,k] == z) { resamp[i,,k][1:length(copy)]<- as.character(target[z,][copy]) } }}}
#saving file as .csv
    file_name<- paste(family2,"_",rep,"_resampR.csv", sep="")
    write.table(resamp,file_name,sep=",",row.names=F)
  } #end replicant loop
 } #end family loop
#combine files
  files <- list.files(pattern="_resampR.csv")
  combine <- do.call("rbind", lapply(files, read.csv, header = TRUE))
#column names
  colnames(combine)<- rep(colnames(dat)[copy],iter)  #general copy
  first<- seq(1,length(colnames(combine)),length(copy))  #find first of iteration
  last<- seq(length(copy),length(colnames(combine)),length(copy))  #find last of iteration
  for (i in 1:iter) {
    colnames(combine)[first[i]:last[i]]<- paste(colnames(combine)[first[i]:last[i]],i,sep="") }
   write.table(combine,"resamp_datR.csv",sep=",",row.names=F)
#
print(Sys.time()- time1) #end time
}
