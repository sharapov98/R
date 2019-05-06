a<-seq(3,36,by =3)
b<-seq(1,34,by =3)
0.1^a * 0.2^b

a[1]
a[1:2]
a[c(4,1)]

rep(1,10)
rep(c(1,2),10)
c(rep(1,10), rep(2,10))

x<-c(2,3,5,7,11)
y<-rep(c(2,4),each=2)
x[y]

x<-seq(16,30,by=4)
y<-x%%7+1
z<-LETTERS[]

color <-c("blue", "brown", "green", "orange", "red","yellow")
weight <-c(1, 3, 1, 1, 2, 2)
bag <-sample(color, 30, TRUE, weight)
print(table(bag))

summary(aov(rel_change~month))
#check normality
qqnorm
#test that rejects the null hypothesis of normality
shapiro.test(rel_change)

zscore <-function(x) {if(!is.numeric(x)) 
  {stop("Nonnumeric argument x in zscore()")
}m <-mean(x)
  s <-sd(x)(x-m)/s}

days_in_month <-function(month_name) 
  {if(!is.character(month_name))
    stop("Argument month_name not a character string.")
  days <-switch(month_name,January   = "31",
                February  = "28 (in leap years 29)",March     = "31",
                April     = "30",May       = "31",June      = "30",
                July      = "31",August    = "31",September = "30",
                October   = "31",November  = "30",December  = "31","unknown")
print(days, quote = FALSE)}