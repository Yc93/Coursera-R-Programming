## This function reads a directory full of files and reports the number of completely observed cases in each data file
complete<- function(directory,id=1:332){
  ## Set working directory  
    setwd(file.path(getwd(),directory))
    nobs<- vector("numeric",length=length(id))
    avaliable<-as.character(list.files())
    for(i in id){
        data<- read.csv(avaliable[i])
        count<- sum(as.numeric(complete.cases(data)))
        nobs[i]<-count 
    }
    get<-data.frame(id,nobs[id])
    ## Return to previous directory
    setwd("..")
    return(get)
}