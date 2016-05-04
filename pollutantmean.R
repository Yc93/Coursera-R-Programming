#This function calculates the mean of a pollutant(sulfate or nitrate) across a specific list of monitors
pollutantmean<- function (directory,pollutant,id=1:332){
  ## Select working directory  
  setwd(file.path(getwd(),directory))
  ##List all the files in the working directory and store the names to a new vector "currentfile"
    currentfile<- as.character(list.files())
    ##To calculate its mean, get the total count of ID and the total value of these pollutant
    mytotal<- vector("numeric",length=length(id))
    mylength<- c()
    for (i in id){
        raw.data<-read.csv(currentfile[i])
        ## Remove NA Value
        data<- raw.data[complete.cases(raw.data),]
        if(pollutant=="sulfate"){
            total<- sum(data$sulfate)
            count<-length(data$sulfate)
        } else{
            total<- sum(data$nitrate)
            count<-length(data$nitrate)}
        mytotal[i]<- total
        mylength[i]<- count}
    answer<-sum(mytotal[id])/sum(mylength[id])
    ## Return to the original working directory
    setwd("..")
    print(answer)
}