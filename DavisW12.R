library(dplyr)
library(countrycode)
library(RColorBrewer)
library(rworldmap)
library(reshape2)
install.packages("tweenr")
library(tweenr)
# Skip first line of the .csv file. Keep country names as strings.
cait <-read.csv("CAIT Country CO2 Emissions.csv",skip = 1,
                stringsAsFactors = FALSE)%>%rename(CO2 = 3)%>%
  # Shorten the name of the third column.
filter(!(Country%in% c("European Union (28)", "World")))%>%
  mutate(Country =gsub("Micronesia","Federated States of Micronesia",Country),
         Code =countrycode(Country, "country.name", "iso3c")) %>%
dcast(Code~Year, value.var = "CO2")
spdf <-joinCountryData2Map(cait, nameJoinColumn = "Code")

spdf <-subset(spdf, continent!="Antarctica")
spdf <-spTransform(spdf, CRS =CRS("+proj=gall"))
# Gall projection.
crp <-colorRampPalette(brewer.pal(9, "YlOrBr"))
# Colours for maps.# Cut points for turning numerical values into categories on the map.
breaks <-c(0, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000)

snapshot <-function(year) {
  mapCountryData(spdf,nameColumnToPlot = "CO2",catMethod = breaks,
                 colourPalette =crp(length(breaks)-1),addLegend = FALSE,
                 borderCol = "black",mapTitle = "",missingCountryCol = "lightgrey")
  # See https://stackoverflow.com/questions/28664385/include-formatted-
  # subscript-in-mtext for how to get the subscript [2] in CO[2].
  title(main =expression(bold(CO[2]~"emissions in the year 2014")),line =-2)
  addMapLegend(colourVector =crp(length(breaks)-1),cutVector = breaks,
               legendLabels = "all",# Show all cut points in legend.
               legendArgs =list(text = "emissions in million of tons per year",
                                side = 3,line = 0.25),mgp =c(2, 0.75, 0),# Move tick marks closer to colour 
                                bar.legendIntervals = "page")
}

saveHTML(sapply(year, snapshot),
         interval = 0.2,
         ani.width= 1200,
         ani.height=800)

t<-read.delim("DatasaurusDozen.tsv")
t.list<- split(t, t$dataset)
snapshot <-function(df){
  plot(df$x, df$y)
}
saveHTML(sapply(ttween.list, snapshot))

saurus_tween <-tween_states(t.list,tweenlength = 1,statelength = 1,ease = "cubic-in-out",
                   nframes = 200)
ttween.list<- split(saurus_tween, saurus_tween$.frame)
library(datasauRus)
library(ggplot2)
library(gganimate)
ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 1)