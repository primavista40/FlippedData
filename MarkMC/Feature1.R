 x<-read.csv("mark1.csv",header=FALSE)
 hist(x[,1])
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

 
 adjust<-function(data,factor){
         new=data +(rep(100,length(raw))-data)*factor
         hist(new,main='After')
         return(new)
 }
 After<-adjust(raw,0.3)
 AfterGrade<-grade(After)
 #To compare grade
 # par(mfrow=c(1,1))
 #barplot(rbind(BeforeGrade,AfterGrade),beside=TRUE,col=c("darkblue","red"),names.arg=c('F','D','C','B','A'),main='Grade Comparison',legend=c("before","after"))