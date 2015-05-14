BeforeGrade<-grade(raw)
AfterGrade<-grade(After)
par(mfrow=c(1,1))
barplot(rbind(BeforeGrade,AfterGrade),beside=TRUE,col=c("darkblue","red"),names.arg=c('F','D','C','B','A'),main='Grade Comparison',legend=c("before","after"))