load(url("http://michaelgastner.com/birthweight.Rda"))
plot(weight~gest, ylab="Weight at birth", )
regress<- lm(weight~gest)
summary(regress)
abline(regress)

load(url("http://michaelgastner.com/epl2017_2018.Rda"))
win<-wins*3
points<-win+draws
team[order(points, goals_for, decreasing = TRUE)]

count<-0 
for (i in 1:10000) {
  birthday<-sample(366,23,replace=TRUE,
                   prob=c(rep(1,365),0.2425))
  count <- count+ anyDuplicated(birthday) !=0
}
print(count/10000)