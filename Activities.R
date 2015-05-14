files <- list.files(path='StudentMC',full.names=TRUE)
result <- do.call(rbind, lapply(files, read.csv))
name1<-x[x[,4]==1,2]
index1<-result[,1] %in% name1
p1<-sum(as.logical(result[index1,2]))/sum(index1)
name2<-x[x[,4]==2,2]
index2<-result[,1] %in% name2
p2<-sum(as.logical(result[index2,2]))/sum(index2)

pbinom(sum(as.logical(result[index2,2])),sum(index2),p1*1.1)