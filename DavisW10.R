library("RColorBrewer")
library("rcolorbrewer")
w<-scan("alice.txt", character())
grep("\\?$", w, value=TRUE)
w1<-gsub("^[[:punct:]]" ,"",w)
identical(w, w1)

wordcloud(w)
updateR()
emit<-read.csv("CAIT Country CO2 Emissions.csv")
colnames(emit)[3] <- "CO2"
Code <- countrycode(emit$Country, "country.name", "iso3c")
spdf<- joinCountryData2Map(emit, nameJoinColumn = "code")
spdf<- subset(spdf, continent!= "Antarctica")


spdf<- spTransform(spdf, CRSobj = CRS("+proj=gall"))
breaks<- c(0,10,20,50,100,500,1000,2000,5000,10000,20000)
pdf_name = "emit.pdf"
mapDevice("pdf", file=pdf_name)
mapCountryData(spdf, nameColumnToPlot = "CO2", catMethod = breaks,
               mapTitle<-"CO2 emissions in the year 2014"
               colourPalette = crp(length(breaks)-1)
               addLegend = FALSE)
addMapLegend(colourVector = crp(length(breaks)-1), 
             cutVector = breaks,
             legendIntervals = "page",
             legendLabels = "all",
             legendShrink = 0.9)
dev.off
openPDF(pdf_name)
title(expression(bold("CO[2]")))
 