## This function takes a directory of data files and threshold for complete cases and calculate the correlation between S and N
corr<- function(directory,threshold =0){
  ## Set working directory
    setwd(file.path(getwd(),directory))
    avaliable<- as.character(list.files())
    ## Making a vector to store answers
    answer<-c()
    for(i in 1:332){
        data1<-read.csv(avaliable[i])
        mydata<-data1[complete.cases(data1),]
        consid<-sum(complete.cases(data1)) ## Length of All NA Removed Cases
        if(consid>threshold){
            correlation<-cor(mydata$sulfate, mydata$nitrate)
        }else{
            correlation<-NA}
        answer[i]<-correlation
    }
    get<-answer[!is.na(answer)]
    ## Return to previous directory
    setwd("..")
    return(as.numeric(get))
}