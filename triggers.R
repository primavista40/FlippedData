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