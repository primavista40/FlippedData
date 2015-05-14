help<-vector(mode='numeric',length=length(add[,1]))
par(mfrow=c(1,2))
hist(x[,1],main='Before')
for (i in 1:length(add[,1])){
help[i]<-sum(add[i,])/35
}
#a,b is the range of score we want to adjust
adjust3<-function(data,max,a,b){
        new=data  
        for (i in 1:length(add[,1])){
                if (data[i]>a & data[i]<b){
                new[i]<- min(data[i]+max*help[i],b)}
        }
        hist(new,main='After',breaks=10)
        return(new)
}
After<-adjust3(raw,5,0,70)