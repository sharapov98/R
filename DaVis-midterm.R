#1a.
load(url("http://michaelgastner.com/men100mButterfly.Rda"))
olympics<-data.frame(sec, athlete, heat) 
semif<-olympics[order(olympics$sec),][1:16,]
print(semif$athlete)
#b. 5 of the qualified athletes participated in heat 6.
sum(semif$heat==6)

#2
load(url("http://michaelgastner.com/geyser.Rda"))
library(graphics)
library(Biobase)  # Needed for openPDF().
pdf_name <- "Problem2.pdf"
pdf(file = pdf_name)  

hist(waiting,main = "Waiting times between eruptions", freq = FALSE,
     xlab = "Minutes", breaks= 20, xlim=c(30,100),
     ylab = "Density", col=c("light green"))
mtext("Old Faithful geyser in Yellowstone National Park, USA")
lines(density(waiting), col="blue", lwd=2)
grid()

dev.off() 
openPDF(pdf_name)

#3a All values are unique.
load(url("http://michaelgastner.com/alumni_starfleet.Rda"))
any(duplicated(alum_name))
#b 
alum_country<-factor(alum_country, levels=unique(alum_country))
#c
levels(alum_country)<- code2country[levels(alum_country)]
#d
table(alum_country)
#e
alum_country<-levels(alum_country)[alum_country]
names(alum_country) <-alum_name

#4   i,j = 1/(i+j−1)
#i,j = 1 if j=1 or if i divides j , 0 otherwise
mkrow <- function(j,n) {
  row_j <- rep(0, n)
  for (i in 0:n) {
    if (i %% j == 0)
      row_j[i] <- 1
    else 
      row_j[i] <- 0
  }
  return (row_j)
}

if (i %% j == 0) {
  row_j[i] <- 1
  
}
else 
{
  row_j[i] <- 0
  
}

mkmatrix <- function(n) { 
  m <- NULL
  for (j in 0:n) {
    row_j <- rep(0, n)
    for (i in 0:n) {
      sprintf("I am computing row %i and column %i bleh %i!", i, j, i%%j)
      ifelse((i %%j == 0), row_j[i] <- 1, row_j[i] <- 0)
    }
    m <- rbind(m, row_j)
    
  }
  return(m)
  }


redheffer<-function(n){
  if(!is.numeric(n)| length(n)!=1||n<1) {
    stop("redheffer() needs a single number >= 1 as input.")
    } 
  matrix(r, nrow = n)
}

#5a
haar_ifelse<-function(x) {
  ifelse(x>=0 ||x<0.5 ,1,ifelse(x>=0.5 ||x<1,-1,0))}
haar_ifelse(seq(-0.5,1.5,by=0.25))
#b
haar_alternative<-function(x){
  
}



#6a 8 age values missing and 0 for net worth
load(url("http://michaelgastner.com/sg_richest.Rda"))
sum(is.na(age))
sum(is.na(net_worth))
#b Data doesn't exhibit a linear pattern with lots of outliers and large clusters
plot(net_worth~age)
#c I chose log10 as the range extended into several powers of 10
plot(log10(net_worth)~age)
#d P-value is quite large to be signficant and R-squared is less than 1%,
#Net worth increases by 0.004 of a power of 10 for every increase in age of 1 
summary(lm(log10(net_worth)~age))