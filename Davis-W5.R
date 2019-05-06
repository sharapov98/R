install.packages("knitr")
BiocManager::install("Biobase", version = "3.8")
library(knitr)
load(url("http://michaelgastner.com/NYtemperature.Rda"))  # Read data.
pname<-"davis-temp.pdf"
pdf(file = pname)  # Start exporting to PDF.
hist(temp, mtext="May to Sept 1973", breaks=20,
     main = "Max daily temp in NY",
     xlab = "Temperature",
     ylab = "Number of days ", col=c("orange"), side =3)

p2name<-"davis-boxlot.pdf"
pdf(file=p2name, width=5.5, length=4.5)
boxplot(temp~month, main = "Max daily temp in NY",
        xlab = "Month",
        ylab = "Temp ", col=c("orange"), names=c("May", "June", "July", "August", 
                                                 "September") )

dev.off()  # Stop exporting to PDF.
openPDF(pname)

load(url("http://michaelgastner.com/michelson.Rda")) 
sd(speed)# Read data.
hist(speed, main = "Michelson Speed-of-Light Measurements", freq = FALSE,
     xlab = "Speed", breaks= 20, xlim=c(600,1100),
     ylab = "Density", col=c("light green"))
curve(dnorm(x, mean = mean(speed), sd = sd(speed)), add = TRUE)
lines(density(speed), lty= "dashed", col="red", lwd=2)

m <- matrix(c(5, 6, 4, 1, 8, 4), nrow = 2)
colnames(m) <- c("left", "middle", "right")
rownames(m) <- c("top", "bottom")
print(m)
m[2:1, 2:3] #or 
m[2:1, -1]

m[, -1]

n <- matrix(rep(-100, 4), nrow = 2)
m[seq_len(2), 2:3] <- n
print(m)

load(url("http://michaelgastner.com/women400m.Rda")) 
min(sec[heat==1], runner)
olympics<-data.frame(sec, athlete, heat) 
semif<-olympics[order(olympics$sec),][1:16,]
print(semif$athlete)
min(runner, olympics[heat=="1"])
?min
boxplot(sec[heat=="1"])
boxplot(sec[heat=="2"])
boxplot(sec~heat, col=c("light green"), main="Women's 400m heats", 
        xlab="Heat", ylab="Time (seconds)")
points(factor(sec), heat)
stripchart(sec)
sec[45]<-NA
?stripchart
mean(na.omit(sec))
aov(lm(sec~heat))