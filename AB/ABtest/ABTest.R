#read data
x<-read.csv("list_students_score.csv")
y1<-x[,4]==1
y2<-x[,4]==2
mean=mean(x[y1,3])
sd
#test if the data set from algorithm 2 comes from the same distribution
ppois(sum(x[y2,3]),mean*sum(y2))
#as the probability is so close to 1 that we can't spot the difference
#we then test if it has 20% greater mean than algo 1
ppois(sum(x[y2,3]),1.25*mean*sum(y2))

#Try to test with Normal dist
sig<-sd(x[y1,3])
pnorm(sum(x[y2,3]),1.2*mean*sum(y2),sig)

#compare histrogram
par(mfrow=c( 1,2))
hist(x[y1,3],freq=10,main='Algo 1',breaks=10)
hist(x[y2,3],freq=10,main='Algo 2')