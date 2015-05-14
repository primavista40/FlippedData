x<-read.csv("mark1.csv",header=FALSE)
par(mfrow=c(1,2))
hist(x[,1],main='Before')
grade<-function(x){
        A<-sum(x >= 80)
        B<-sum(x >= 70)-sum(x >= 80)
        C<-sum(x >= 60)-sum(x >= 70)
        D<-sum(x >= 50)-sum(x >= 60)
        F<-sum(x >= 0)-sum(x >= 50)
        return(c(F,D,C,B,A))
}
raw<-x[,1]
BeforeGrade<-grade(raw)


adjust2<-function(data,factor){
        new<-rep(mean(data),length(raw))+
                (data-rep(mean(data),length(raw)))*factor
        for (i in 1:length(raw)){
          if (new[i]>100){
                new[i]<-100
        }
        }
                
        hist(new,main='After',breaks=10,xlim=c(0,100))
        return(new)
}
After<-adjust2(raw,0.85)
AfterGrade<-grade(After)
#To compare grade
# par(mfrow=c(1,1))
#barplot(rbind(BeforeGrade,AfterGrade),beside=TRUE,col=c("darkblue","red"),names.arg=c('F','D','C','B','A'),main='Grade Comparison',legend=c("before","after"))