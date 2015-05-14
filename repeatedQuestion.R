x<-read.csv('student-history.txt')
colnames(x)[1]<-"question_id"
summary(glm(is_correct~attempt,data=x,family='binomial'))
inverseodd<- function(value){
       1-(1+exp(value))^(-1)
 }
library(lubridate)
day("2015-04-04T12:46:35.894Z")
secondAttempt<-x[x$attempt==2,c(1,3,7)]
firstAttempt<-vector(length=584)
for (i in 1:584){
        firstAttempt[i]<-subset(subset(subset(x,question_id==secondAttempt[i,1])
                                       ,student_id==secondAttempt[i,3]),
                                attempt==1,select=is_correct)
}
firstAttempt<-as.numeric(firstAttempt)
newTable<-cbind(firstAttempt,secondAttempt)[,c(1,3)]
colnames(newTable)[2]="secondAttempt"
RightRight<-sum(newTable[,1]==1 & newTable[,2]==1)
RightWrong<-sum(newTable[,1]==1 & newTable[,2]==0)
WrongWrong<-sum(newTable[,1]==0 & newTable[,2]==0)
WrongRight<-sum(newTable[,1]==0 & newTable[,2]==1)
'''confident interval on getting second one right is [0.57,0.74]
If we use p as probability of memorising
-if not memorised next time correct w.p 0.5
-if memorised next time correct w.p. 1
Then get the confident interval for the learning rate as [0.14,0.48]
'''
count=0
for (i in 1:584){
        m<-month(subset(subset(x,question_id==secondAttempt[i,1])
                      ,student_id==secondAttempt[i,3])$created_at[1])
        if (m==4){
                count<-count+1
        }
}
