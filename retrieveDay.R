retrieveDay<-function(day){
        y<-x[x$attempt==1,]
        y<-y[order(y[,5]),]
        index<-(day(y$created_at)==day)
        return y[index,]
}
