#analyse when students use our system

hist(hour(x$created_at),xlim=c(0,23),breaks=24)