install.packages("viridis")
library(viridis)
ggplot(airquality, aes(Month, Temp)) + 
         geom_boxplot(colour="orange")
data(Oxboys,package = "nlme")
str(airquality)
ordered(airquality$Month, levels = c(5,6,7,8,9), labels= c("May", "Jun", "Jul", "Aug", "Sep"))

load(url("http://michaelgastner.com/ASEAN_tourism.Rda"))
barplot(tourism[7,,8], horiz=TRUE)
ggplot(tourism, aes()) +barplot()

v <- c("Breakfast", "Lunch","Dinner")
df<- data.frame(meal=factor(v, levels=v), 
                time= c(8,12,18), 
                total_bill = c(14.89, 17.29,24.89))
ggplot(df, aes(meal,total_bill, colour= time,group=1)) +
      geom_line() + 
      geom_point()
m<-read.csv("measles_long.csv")
ggplot(m, aes(Year, factor(State, levels=rev(levels(State))), 
              fill=Incidence)) + ylab("State") + 
  theme(axis.text.y= element_text(size=6))+
  geom_tile(colour="white")+ scale_fill_viridis() + coord_equal() 
