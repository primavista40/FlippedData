#10,12   11,12

library(lubridate)
library(parsedate)
x<-read.csv("skill-improvement.csv")
compareDays<-function(day1,day2,skillId,low=0,high=0.7){
        y<-x[x$attempt==1,]
        y<-y[y$skill_id==skillId,]
        y<-y[order(y[,5]),]
        index<-(day(y$created_at)==day1)
        index2<-(day(y$created_at)==day2)
        firstDay<-y[index,]
        secondDay<-y[index2,]
        meanEach<-vector(length=length(unique(firstDay$student_id)))
        i<-1
        for (student in unique(firstDay$student_id)){
                meanEach[i]=mean(firstDay[firstDay$student_id==student,]$is_correct,na.rm=TRUE)
                i<-i+1
        }
        lowIndex<- (meanEach<=high) & (meanEach>=low)
        meanEach<-meanEach[lowIndex]
        lowScore<-unique(firstDay$student_id)[lowIndex]
        difference<-vector(length=length(unique(lowScore)))
        i<-1
        for (student in lowScore){
                if (sum(secondDay$student_id==student)==0){
                        difference[i]<-0}
                else{
                        difference[i]<-mean(secondDay[secondDay$student_id==student,]$is_correct,na.rm=TRUE)-meanEach[i]
                }
                i<-(i+ 1)
        }
        exclude<-(difference != 0)
        difference<-difference[difference != 0]
        t.test(subset(meanEach,exclude)+difference,subset(meanEach,exclude),paired=TRUE)
}
'''
y<-x[x$attempt==1,]
y<-y[order(y[,5]),]
index<-(day(y$created_at)==11)
index2<-(day(y$created_at)==12)
firstDay<-y[index,]
secondDay<-y[index2,]
meanEach<-vector(length=length(unique(firstDay$student_id)))
i<-1
for (student in unique(firstDay$student_id)){
        meanEach[i]=mean(firstDay[firstDay$student_id==student,]$is_correct,na.rm=TRUE)
        i<-i+1
}
lowIndex<- (meanEach!=0.7)
meanEach<-meanEach[lowIndex]
lowScore<-unique(firstDay$student_id)[lowIndex]
difference<-vector(length=length(unique(lowScore)))
i<-1
for (student in lowScore){
        if (sum(secondDay$student_id==student)==0){
                difference[i]<-0}
        else{
                difference[i]<-mean(secondDay[secondDay$student_id==student,]$is_correct,na.rm=TRUE)-meanEach[i]
        }
        i<-(i+ 1)
}
exclude<-(difference != 0)
difference<-difference[difference != 0]
t.test(subset(meanEach,exclude)+difference,subset(meanEach,exclude),paired=TRUE)
'''
'''

skills<-unique(x$skill_id)
for (skill in skills){
        print(unique(day(x[x$skill_id==skill,5])))
}
firstIndex<-day(x[x$skill_id=='630',5]) == 25
secondIndex<-day(x[x$skill_id=='630',5]) == 8
firstDay<-x[firstIndex,]
secondDay<-x[secondIndex,]
firstDay<-firstDay[firstDay$attempt==1,]
secondDay<-secondDay[secondDay$attempt==1,]
''''