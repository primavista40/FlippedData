#exploratory skill
#qu triggered
require(plotrix)

skillTrigger<-function(){
  lt<-vector()
  for (skill in unique(x$skill_id)){
    selected<-x[x$skill_id ==skill,]
    correct<-sum(selected$correct_count)
    incorrect<-sum(selected$incorrect_count)
    lowerBound<-binom.test(correct,correct+incorrect,p=0.5,alternative='t')$conf.int[1]
    if (lowerBound<0.75){
      lt<-rbind(lt,c(skill))
    }
  }
  return(lt)
}
skillYellowTrigger<-function(){
        lt<-vector()
        for (skill in unique(x$skill_id)){
                selected<-x[x$skill_id ==skill,]
                correct<-sum(selected$correct_count)
                incorrect<-sum(selected$incorrect_count)
                lowerBound<-binom.test(correct,correct+incorrect,p=0.5,alternative='g')$conf.int[1]
                if (lowerBound<skillMean(skill)){
                        lt<-rbind(lt,c(skill))
                }
        }
        return(lt)
}
skillMean<-function(skill){
    selected<-x[x$skill_id ==skill,]
    correct<-sum(selected$correct_count)
    incorrect<-sum(selected$incorrect_count)
    skillMean<-correct/(correct+incorrect)
  }

questionTrigger<-function(){
  lt<-vector()
  for (qu in unique(x$question_id)){
    selected<-x[x$question_id ==qu,]
    skill_id=selected$skill_id[1]
    correct<-sum(selected$correct_count)
    incorrect<-sum(selected$incorrect_count)
    lowerBound<-binom.test(correct,correct+incorrect,p=0.5)$conf.int[1]
    if (lowerBound<0.5){
      lt<-rbind(lt,c(qu))
    }
  }
  return(lt)
}

skillCI<-function(skills){
        L<-vector()
        U<-vector()
        M<-vector()
        for (skill in skills){
        selected<-x[x$skill_id ==skill,]
        correct<-sum(selected$correct_count)
        incorrect<-sum(selected$incorrect_count)
        Li<-binom.test(correct,correct+incorrect,p=0.5,alternative='t')$conf.int[1]
        Ui<-binom.test(correct,correct+incorrect,p=0.5,alternative='t')$conf.int[2]
        Mi<-correct/(correct+incorrect)
        L<-rbind(L,Li)
        U<-rbind(U,Ui)
        M<-rbind(M,Mi)
        }
        plotCI(1:length(skills), M, ui=U, li=L,xlab='skill id',xaxt='n',ylim=c(min(U)-0.05,1))
        axis(1,at=1:length(skills),labels=skills)
}
