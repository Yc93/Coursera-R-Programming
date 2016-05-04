#This function calculates the mean of a pollutant(sulfate or nitrate) across a specific list of monitors
pollutantmean<- function (directory,pollutant,id=1:332){
  ## Select working directory  
  setwd(file.path(getwd(),directory))
  ##List all the files in the working directory and store the names to a new vector "currentfile"
    currentfile<- as.character(list.files())
    ##To calculate its mean, get the total count of ID and the total value of these pollutant
    mytotal<- vector("numeric",length=length(id))
    mylength<- c()
    times<- 1
    while (times<= length(id)){
        raw.data<-read.csv(currentfile[(id)[times]])
        ## Remove NA Value
        data<- raw.data[complete.cases(raw.data),]
        if(pollutant=="sulfate"){
            total<- sum(data$sulfate)
            count<-length(data$sulfate)
        }else{
            total<- sum(data$nitrate)
            count<-length(data$nitrate)}
        mytotal[times]<- total
        mylength[times]<- count
        times<- times +1
        }
    answer<-sum(mytotal)/sum(mylength)
    ## Return to the original working directory
    setwd("..")
    print(answer)
}