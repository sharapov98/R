load("~/Downloads/dowjones.Rda")

d<-diff(close_value.na)
e<-insert(d, 0, 1)
c<-close_value[!close_val]
rel_change<-100*d/close_value.na[-length(close_value.na)]
month<-month[-1]
jan<-rel_change[seq(1,length(month),12)]
summary(jan)
boxplot(rel_change~match(month,month.name),names=month.abb, ylim=c(-10,20))

hist(rel_change)
#Deviation from norm - QQplot
qqnorm(rel_change)

df<-data.frame(rel_change,month)
aov(rel_change~month)
?sample
colors<-c("blue","red","orange","brown","green", "yellow")
s<-sample(colors,30,replace= TRUE,prob = c(0.1,.2,0.1,0.3,0.1,0.2))
table(s)

pythag<- function(x,y) {
  sqrt(x^2+y^2)
}



zscore<- function(x) {
  if (!is.numeric(x)){
    stop("Nonnumeric element inputted!")
  }
  (x - mean(x))/sd(x)
}
scale(c(50,100,150,200))

even_or_odd <- function(x) {
  if (x%%2==0) {
    print("is even")
  } if (x%%2==1) {
    print("is odd")
  } else {
    print("is neither even nor odd")
  }
}

even_or_odd <- function(x) {
  ifelse (x%%2 == 0, 'even', ifelse ()) {
    print("is even")
  } ifelse (x%%2 ==1, 'odd') {
    print("is odd")
  } else {
    print("is neither even nor odd")
  }
}
even_or_odd(c(1,2,3,4,5,6.5))

load(url("http://michaelgastner.com/dowjones_all.Rda"))
index<- which(month=="July" & year ==1914)
close_value.na<- append(close_value, rep(NA, 4), after = index)
month_na<- append(month, month.name[8:11], after =index)
year_na <- append(year,rep(1914,4), after=index)
plot(close_value[year==1913&year<=1915], 
     type = "o",
     xlab="Months open since January 1914",
     ylab="Dow Jones closing value")

plot(close_value.na[year_na==1913 & year_na<=1915],
     type = "o",
     xlab="Months open since January 1914",
     ylab="Dow Jones closing value")

rel_change <-diff(close_value)
for (i in 1:12) {
  print(range(rel_change[month==month.name[i]]))
}
?names
x<-c(1,2,3,4)
names(x) <- c(NA,"A","B","C")
x[NA] <-20
x
x[as.character(NA)] <- 20

