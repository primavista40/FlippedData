x<-read.csv("mark1.csv",header=FALSE)
add<-read.csv("Add.csv",header=TRUE)
add[is.na(add)] <- 0

grade<-function(x){
        A<-sum(x >= 80)
        B<-sum(x >= 70)-sum(x >= 80)
        C<-sum(x >= 60)-sum(x >= 70)
        D<-sum(x >= 50)-sum(x >= 60)
        F<-sum(x >= 0)-sum(x >= 50)
        return(c(F,D,C,B,A))
}