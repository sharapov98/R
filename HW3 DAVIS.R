#Problem Set 3 - DAVIS
#Group: ZhongXhuan, Yuchen, Damon, Abdul, Tram, Jacob

#install.packages('plotrix') #UNCOMMENT IF NOT INSTALLED
#install.packages('vioplot') #UNCOMMENT IF NOT INSTALLED
library(vioplot)
library(plotrix)
library(knitr)
library(Biobase)

# QUESTION 1

load(url("http://michaelgastner.com/tuition95.Rda"))

# (a) Make 2 Histograms
tuition.private <- tuition[is_private]
tuition.public <- tuition[!is_private]

pdf_name <- "TuitionHist.pdf"
pdf(file = pdf_name, width = 6, height = 5)
par(mfrow = c(2,1),
    mar = c(4, 6, 4, 4),
    mgp = c(2, 0.75, 0),
    mai = c(0, 0.9, 0.8, 0.4))
hist(tuition.public, 
     breaks = seq(0,25000, by = 1000), 
     col = 'lightgreen', 
     ylab = "Frequency", 
     xlab = NULL,
     main = "Out-of-state Tuition at 777 Colleges in 1995",  
     xaxt = "n", 
     ylim = c(0,60))
abline(h=seq(10, 70, by = 10), v = seq(5000, 25000, by = 5000), col = "lightgray", lty = "dotted")
text(2500,55, "Public")
mtext("Source: U.S. News and World Report", line = 0.6, cex = 0.8)
par(mai = c(0.8,0.9,0,0.4))
hist(tuition.private, breaks = seq(0,25000, by = 1000), col = 'lightgreen', ylab = "Frequency", xlab = "US$", main = "")
abline(h=seq(10, 70, by = 10), v = seq(5000, 25000, by = 5000), col = "lightgray", lty = "dotted")
text(2500,55, "Private")
dev.off()
plot_crop(pdf_name)
openPDF(pdf_name)

# (b) Make side by side Boxplot
public_private <- factor(ifelse(is_private, "Private", "Public"), levels = c("Public", "Private"))

pdf_name <- "TuitionBox.pdf"
pdf(file = pdf_name, width = 4, height = 5)
par(mar = c(4, 8, 4, 10),
    mgp = c(2, 0.75, 0),
    mai = c(0.4, 1.1, 0.8, 1.1))
boxplot(tuition ~ public_private, 
        col = "lightgreen", 
        varwidth = TRUE,
        main = "Out-of-state tuition at 777 colleges in 1995",
        ylab = "US$")
abline(h=seq(5000, 20000, by = 5000), v = c(1,1.5,2), col = "lightgray", lty = "dotted")
mtext("Source: U.S. News and World Report", line = 0.5)
dev.off()
plot_crop(pdf_name)
openPDF(pdf_name)

# (c) Violin Plot
pdf_name <- "TuitionViolin.pdf"
pdf(file = pdf_name, width = 4, height = 5)
par(mar = c(4, 8, 4, 10),
    mgp = c(2, 0.75, 0),
    mai = c(0.4, 1.1, 0.8, 1.1))
vioplot(tuition ~ public_private,
        col = "lightgreen",
        main = "Out-of-state tuition at 777 colleges in 1995",
        ylab = "US$",
        areaEqual = TRUE)
abline(h=seq(5000, 20000, by = 5000), v = c(0.5,1,1.5,2,2.5), col = "lightgray", lty = "dotted")
mtext("Source: U.S. News and World Report", line = 0.5)
dev.off()
plot_crop(pdf_name)
openPDF(pdf_name)






# QUESTION 2 
percentage_share_change <- c(5.5, 9.5, -1.7, -0.5, -2.1, -10.8)
names(percentage_share_change) <- c("CON", "LAB", "SNP", "LD", "GRN", "UKIP")
percentage_share_change # to check up on my matrix 


pdf_name <- "UKElections.pdf" # naming the pdf file
pdf(file = pdf_name, height = 6, width=7)
par(mar = c(5, 5, 5, 2),
    mgp = c(2, 0.5, 0))
color <- c("red1", # creating a vector comprising of traditional UK party colours 
           "mediumblue",
           "orange1",
           "yellow1",
           "green2",
           "purple3")
b <- barplot(sort(percentage_share_change, decreasing = TRUE),
           col = color, ylim = c(-12, 11),
           xlim = c(0, 7), # to answer the part on reasonably font size relative to the plot region
           ylab = "Change (in %)", names.arg = FALSE,
           main = "UK vote share change between 2015 and 2017")
text(b,c(10.4,6,-1,-2.5,-3,-11.25), paste(symnum(sort(percentage_share_change, decreasing = TRUE),
                                                 c(-Inf, 0, Inf), c("", "+")),sort(percentage_share_change, 
                                                                                   decreasing = TRUE), sep="") ,cex=1) 
text(b,c(-0.5,-0.5,0.5,0.5,0.5,0.5), names(sort(percentage_share_change, decreasing = TRUE)) ,cex=1) 
abline(h=0)
grid(NA, NULL)
dev.off()
openPDF(pdf_name)







#QUESTION 3

load(url("http://michaelgastner.com/LifeExpectancy.Rda"))

#a) Create a subset with just the 10 ASEAN countries (Brunei Darussalam, Cambodia, Indonesia, Lao PDR,
#Malaysia, Myanmar, Philippines, Singapore, Thailand, Vietnam). Please shorten “Brunei Darussalam”
#to Brunei and “Lao PDR” to Laos to make it easier for visualization

tenASEAN <- LifeExpectancy[c("Brunei Darussalam", "Cambodia", "Indonesia", "Lao PDR",
                             "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam"), ]

rownames(tenASEAN)[rownames(tenASEAN) == "Brunei Darussalam"] <- "Brunei"
rownames(tenASEAN)[rownames(tenASEAN) == "Lao PDR"] <- "Laos"

#b) Plot the life expectancy trend on a graph with each line representing a country. Remember to label
#and color the points to distinguish the countries and to include a clear legend. Export your graph to
#PDF. Briefly comment on the plot.

pdf_name <- "lifeext.pdf"
pdf(file = pdf_name, width = 10, height = 6)
par(mar = c(3,3,1.5,8),
    mgp = c(2,0.75,0))
year <- as.numeric(colnames(tenASEAN))
matplot(year,
        t(tenASEAN),
        type = "b",
        col = 1:nrow(tenASEAN),
        lwd = 2,
        xlab = "Year",
        pch = seq_len(nrow(tenASEAN)),
        cex = 0.7,
        ylab = "Life Expectancy",
        main = "Life Expectancy at Birth in 10 ASEAN countries")
legend(x = max(year) + 2.5,
       y = max(tenASEAN),
       legend = rownames(tenASEAN),
       col = 1:nrow(tenASEAN),
       lwd = 2,
       pch = seq_len(nrow(tenASEAN)),
       bg = "ghostwhite",
       xpd = TRUE)
grid()
dev.off()
plot_crop(pdf_name)
openPDF(pdf_name)

#This graph shows evidence of the historical events such as the Vietnam War and the Cambodia genocide,
#where we see drops in life expectancies.


#c) Please create a factor variable group that states if
#the country is “High/Upper middle income” (n=4) or “Lower middle income” (n=6). Make sure this
#variable is aligned with your matrix subset from 3a.
group <- factor(rownames(tenASEAN))
levels(group) <- list("Lower middle income" = c("Cambodia", "Indonesia", "Laos",
                                                "Myanmar", "Philippines", "Vietnam"),
                      "High/Upper middle income" = c("Singapore", "Brunei",
                                                     "Thailand", "Malaysia"))

#d) Let us check if the life expectancy in 2016 varies by the country’s income status. Use the 
#stripchart() command to generate a figure similar to Figure 5.5
#Indicate each country’s position in the plot. The red squares represent the group means.
tenASEAN2016 <- tenASEAN[, "2016"]
high <- tenASEAN2016[group == "High/Upper middle income"]
low <- tenASEAN2016[group == "Lower middle income"]

pdf_name <- "grouped.pdf"
pdf(file = pdf_name, width = 5.5, height = 4)
par(mar = c(3,3,3,3),
    mgp = c(2,0.75,0))
stripchart(tenASEAN2016 ~ group,
           method = "jitter",
           vertical = TRUE,
           group.names = c("Lower Middle Income", "High/Upper middle income"),
           pch = 16,
           col = "black",
           main = "Life Expectancy at birth",
           xlab = "Income",
           ylab = "Life Expectancy (years)",
           ylim = c(65,85))
points(1, mean(low), pch = 15, col = "red")
points(2, mean(high), pch = 15, col = "red")
text(1, low["Vietnam"], labels = "Vietnam", pos = 4, offset = 2)
text(1, seq(65, 71, 1.5), labels = names(sort(low[(names(low) != "Vietnam")])), pos = 4, offset = 2)
text(2, high["Singapore"], labels= "Singapore", pos=2, offset = 2)
text(2, seq(74, 77, 1.5), labels = names(sort(high[names(high) != "Singapore"])), pos = 2, offset = 2)
legend('topleft', legend = 'mean', pch = 15, col = "red")
mtext("ASEAN countries in 2016")
grid(nx = NA, ny = NULL)
dev.off()
plot_crop(pdf_name)
openPDF(pdf_name)

#e) Can you briefly explain (with a comment in your R code) why the strip chart used in Problem 3d is
#more appropriate than using box plots?

#There isn't a distribution of values for each data point for us to be able to plot a box plot 
#for all countries. In other words, as each country only has 1 value as a point - we can't plot
#a box plot without a median, inter-quartile range, and other attributes.


#f) The bar plot in Figure 6 shows the gain in life expectancy from 1960 to 2016 for those countries that
#are classified as “low middle income” on the left and for “high/upper middle income” countries on the
#right. Notice how the y-axis limits are the same for both bar charts, which allows us to compare across
#graphs. Please produce a similar graph and export the figure as a PDF.
highgain <- tenASEAN[group == "High/Upper middle income", "2016"] - 
  tenASEAN[group == "High/Upper middle income", "1960"]
highgain
lowgain <- tenASEAN[group == "Lower middle income","2016"] - 
  tenASEAN[group == "Lower middle income", "1960"]
lowgain

pdf_name <- "twoplots.pdf"
pdf(file = pdf_name, width = 7, height = 6)
par(mar = c(6,3,9,16),
    mgp = c(2,0.75,0))
barplot(sort(lowgain, decreasing = TRUE), las = 3,
        col = "darkseagreen1",
        ylim = c(0,25),
        ylab = "Years",
        main = "Gain in life expectancy from 1960 to 2016")
grid(nx = NA, ny = NULL)
mtext("Lower middle
      income countries", line = 1.5)
par(new = TRUE,
    mar = c(6,19,9,4))
barplot(sort(highgain, decreasing = TRUE), las = 3,
        col = "darkolivegreen",
        ylim = c(0,25),
        axes = FALSE)
grid(nx = NA, ny = NULL)
mtext("High/Upper middle
      income countries", line = 1.5)
dev.off()
openPDF(pdf_name)







# QUESTION 4

load(url("http://michaelgastner.com/singapore_residents.Rda"))

# (a) What is the class and what are the attributes of this object? 
class(sg_residents)
attributes(sg_residents)

# (b) Write a function that extracts the data for a particular year and generates a pyramid plot.
pyramid_year <- function(year){
  # Error Checks
  if(length(year) > 1){
    stop("year must not be a vector")
  }
  year = as.character(year)
  if(!is.element(year,unlist(dimnames(sg_residents)[3]))){
    stop("year not between 1960 - 2018")
  }
  Malecount <- sg_residents[,"Male", year]/1000
  Femalecount <- sg_residents[,"Female",year]/1000
  Agecat <- unlist(dimnames(sg_residents)[1])
  gap = 20
  pyramid.plot(Malecount, 
               Femalecount, 
               labels = Agecat,
               top.labels = c("Male", "Age", "Female"),
               lxcol = "blue3",
               rxcol = "red",
               unit = NULL,
               laxlab = c(0,50,100,150),
               raxlab = c(0,50,100,150),
               gap = gap)
  abline(v = c(50, 100, 150) + gap, col = "gray", lty = "dotted")
  abline(v = -(c(50, 100, 150)+gap), col = "gray", lty = "dotted")
  title(year, cex.main = 1.5)
  mtext("Residents (in thousands)", side = 1, cex = 0.85, line = 2)
}

# Testing the function
pyramid_year(1968)

# (c) Visualising the population pyramid for 1968 - 2018
pdf_name <- "SGPopulation.pdf"
pdf(file = pdf_name, width = 15, height = 8)
par(mfrow = c(2,3),
    mgp = c(2, 0.75, 0))
pyramid_year(1968)
pyramid_year(1978)
pyramid_year(1988)
pyramid_year(1998)
pyramid_year(2008)
pyramid_year(2018)
dev.off()
plot_crop(pdf_name)
openPDF(pdf_name)




