library(rvest)
library(magrittr)
library(dplyr)
library(countrycode)
library(ggplot2)
library(Biobase)
library(tm)
library(RColorBrewer)
library(scales)
library(wordcloud)
library(rworldmap)
library(rgdal)

# PROBLEM 1

# a) Re-generate data Life Expectancy
LE_url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy#List_by_the_World_Health_Organization_(2015)'

WHO_table <- LE_url %>%
  read_html() %>%
  html_nodes('.wikitable') %>%
  html_table(fill = TRUE) %>%
  extract2(1) %>%
  select(Country = 'Country and regions', WHO_LE = 'Both sexes lifeexpectancy') %>%
  transmute(WHO_LE, Country = countrycode(Country, "country.name", "iso3c"))

UN_table <- LE_url %>%
  read_html() %>%
  html_nodes('.wikitable') %>%
  html_table(fill = TRUE) %>%
  extract2(2) %>%
  select(Country = 'State/Territory', UN_LE = 'Overall') %>%
  transmute(UN_LE, Country = countrycode(Country, "country.name", "iso3c"))

WHO_and_UN_LE <- inner_join(filter(WHO_table, !is.na(Country)), 
                         filter(UN_table, !is.na(Country)), 
                         by = "Country") %>%
  mutate(LE_mean = (WHO_LE + UN_LE)/2, LE_diff = WHO_LE - UN_LE)

# b) Per Capita Income

PCI_url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita"

IMF_table <- PCI_url %>%
  read_html() %>%
  html_nodes('.wikitable') %>%
  html_table(fill = TRUE) %>%
  extract2(1) %>%
  select(Country = 'Country/Territory', IMF_PCI = "Int$" ) %>%
  transmute(Country = countrycode(Country, "country.name", "iso3c"), 
            IMF_PCI = as.numeric(gsub(",", "", IMF_PCI))) %>%
  filter(!is.na(Country))

WB_table <- PCI_url %>%
  read_html() %>%
  html_nodes('.wikitable') %>%
  html_table(fill = TRUE) %>%
  extract2(2) %>%
  select(Country = 'Country/Territory', WB_PCI = "Int$" ) %>%
  transmute(Country = countrycode(Country, "country.name", "iso3c"), 
            WB_PCI = as.numeric(gsub(",", "", WB_PCI))) %>%
  filter(!is.na(Country))

# c) Plot Bland-Altman, and label countries that fall outside

IMF_and_WB_PCI <- inner_join(IMF_table, WB_table, by = "Country") %>%
  mutate(PCI_mean = (IMF_PCI + WB_PCI)/2, PCI_diff = IMF_PCI - WB_PCI)

PCI_bias <- c(mean(IMF_and_WB_PCI$PCI_diff), 
              mean(IMF_and_WB_PCI$PCI_diff) + 1.96*sd(IMF_and_WB_PCI$PCI_diff),
              mean(IMF_and_WB_PCI$PCI_diff) - 1.96*sd(IMF_and_WB_PCI$PCI_diff))

ggplot(IMF_and_WB_PCI, aes(x = PCI_mean, y = PCI_diff)) +
  geom_point(aes(color = I("darkblue")), alpha = 0.5) +
  geom_hline(yintercept = PCI_bias,
             linetype="dashed",
             color = "red") +
  xlab("(IMF estimate + WB estimate) / 2") +
  ylab("IMF estimate - WB estimate") +
  ggtitle("Bland-Altman plot for Per Capita Income") +
  geom_text(aes(label=ifelse(PCI_diff> PCI_bias[2] | PCI_diff < PCI_bias[3], 
                             countrycode(Country, "iso3c", "country.name"), 
                             '')),
            vjust = "outward", hjust = "outward") +
  theme_bw() +
  theme(plot.title = element_text(face="bold", hjust = 0.5))

# We see a reverse in the phenomena we noticed in life-expectency - the variation from the mean seems
# to increase the higher the PCI, whereas the countries with lower PCI tend to revolve more around the mean.

# d) Plotting Overall Life Expectency vs Per Capita Income

LEvsPCI <- IMF_and_WB_PCI %>%
  left_join(WHO_and_UN_LE) %>%
  transmute(Country, Continent = countrycode(Country, 'iso3c', 'continent'), LE = LE_mean, PCI = PCI_mean) %>%
  filter(!is.na(LE))

g <- ggplot(LEvsPCI, aes(PCI, LE)) +
  geom_point(aes(color = Continent)) +
  geom_smooth(se = FALSE) +
  xlab("Gross Domestic Product (PPP)") +
  ylab("Life Expectancy (years)") +
  ggtitle("Preston Curve circa 2015") +
  xlim(0,130000) +
  scale_x_continuous(breaks = c(0, 50000, 100000), labels = c("0", "50k", "100k"))
  theme_bw()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("LEvsPCI.pdf", plot = g, height = 6, width = 8)
openPDF("LEvsPCI.pdf")

LEPCI_Loess <- loess(LEvsPCI$LE ~ LEvsPCI$PCI)
LEvsPCI %>% mutate(CountryN = countrycode(Country, "iso3c", "country.name"), 
                   Code = Country,
                   Dev = LE - LEPCI_Loess$fitted) %>%
  arrange(Dev) %>%
  select(Continent, CountryN, Dev) %>%
  head(n = 10)

# Continent          CountryN        Dev
# 1     Africa Equatorial Guinea -18.458737
# 2     Africa            Angola -15.787194
# 3     Africa           Nigeria -13.548948
# 4     Africa      South Africa -13.152663
# 5     Africa     Côte d’Ivoire -11.937597
# 6     Africa           Lesotho -11.819406
# 7     Africa      Sierra Leone -10.027826
# 8     Africa             Gabon  -9.498864
# 9     Africa          Botswana  -9.216588
# 10    Africa              Chad  -9.040245

# We see that most of countries can be considered developing countries, which could explain
# the discrepency.

LEvsPCI %>% transmute(country = countrycode(Country, "iso3c", "country.name"), 
                   code = Country, 
                   gdp = PCI,
                   life_expectancy = LE,
                   continent = Continent) %>%
  write.csv(file = "life_expectancy.csv")



#PROBLEM 2

#a)
txt <- scan("debate.txt", what = character(), sep = "\n")
txt <- paste(txt, collapse = " ") #1 long character string

#b)
txtsplit <- strsplit(txt, " ")[[1]]
inparen <- grep("\\(\\w+\\)", txtsplit, value = TRUE)

check[check == "(APPLAUSE)"] <- TRUE
check[check == "(CROSSTALK)"] <- TRUE
check[check == "(LAUGHTER)"] <- TRUE
all(as.logical(check)) #TRUE, ie all values in are just those we substituted

txt <- gsub("(APPLAUSE)|(CROSSTALK)|(LAUGHTER)", "", txt) #Remove from main text


#c)
txtCLINTON <- strsplit(txt, "CLINTON:")[[1]][-1]
#all strings in txtCLINTON don't have CLINTON:, and the first doesn't start with CLINTON so I removed it.
noTRUMP <- gsub("TRUMP:.*", "", txtCLINTON)
noWALLACEclin <- gsub("WALLACE:.*", "", noTRUMP)
clean1 <- gsub("^ ", "", noWALLACEclin)
clean2 <- gsub(" $", "", clean1)
clinton <- clean2
clinton

txtTRUMP <- strsplit(txt, "TRUMP:")[[1]][-1]
noCLINTON <- gsub("CLINTON:.*", "", txtTRUMP)
noWALLACEtrump <- gsub("WALLACE:.*", "", noCLINTON)
clean3 <- gsub("^ ", "", noWALLACEtrump)
clean4 <- gsub(" $", "", clean3)
trump <- clean4
trump


#d)

#Working on clinton vector
combined <- paste(clinton, collapse = " ")
wordsclinton <- strsplit(combined, " ")[[1]]
wordsclinton <- tolower(wordsclinton)
#grep("[[:punct:]]", wordsclinton, value = TRUE) #to find punctuations
wordsclinton <- gsub("[[:punct:]]", "", wordsclinton)
wordsclinton <- wordsclinton[!(wordsclinton %in% stopwords())]
wordsclinton <- wordsclinton[!(wordsclinton == "")]
sort(table(wordsclinton), decreasing = TRUE)[1:10]
clinton <- paste(wordsclinton, collapse = " ")


#Working on clinton vector
combinedtrump <- paste(trump, collapse = " ")
wordstrump <- strsplit(combinedtrump, " ")[[1]]
wordstrump <- tolower(wordstrump)
#grep("[[:punct:]]", wordstrump, value = TRUE) #to find punctuations
wordstrump <- gsub("[[:punct:]]", "", wordstrump)
wordstrump <- wordstrump[!(wordstrump %in% stopwords())]
wordstrump <- wordstrump[!(wordstrump == "")]
sort(table(wordstrump), decreasing = TRUE)[1:10]
trump <- paste(wordstrump, collapse = " ")


#e)
term_frequency_matrix <- c(clinton, trump) %>%
  VectorSource() %>%
  Corpus() %>%
  TermDocumentMatrix() %>%
  as.matrix()
colnames(term_frequency_matrix) <- c("Clinton", "Trump")

#f)

pdf_name <- "comparison.pdf"
pdf(file = pdf_name, width = 9, height = 9)
comparison.cloud(term_frequency_matrix, max.words = 300)
dev.off()
openPDF(pdf_name)

pdf_name <- "commonality.pdf"
pdf(file = pdf_name, width = 9, height = 9)
commonality.cloud(term_frequency_matrix, max.words = 300)
dev.off()
openPDF(pdf_name)


# PROBLEM 3

#a) 
LE <- read.csv("life_expectancy.csv")
spdf0 <- joinCountryData2Map(LE, "ISO3", "code")
spdf <- spTransform(spdf0, CRS("+proj=wintri")) 

#b) 
bound1 <- cbind(-180, -90:90)
bound2 <- cbind(180, 90:-90)
bound <- rbind(bound1,bound2)
frm0 <- SpatialPoints(bound, proj4string = CRS("+proj=longlat"))
frm <- spTransform(frm0, CRS("+proj=wintri"))

#c) 

mapCountryData(mapToPlot = spdf,
               addLegend = FALSE)
polygon(coordinates(frm), col = "lightblue")

#d) Including part c) as well

m <- mapCountryData(mapToPlot = spdf, nameColumnToPlot = "life_expectancy",
                    catMethod = "pretty")
ncol <- length(m$cutVector) - 1

pdf_name <- "question3.pdf"
mapDevice("pdf", file = pdf_name)
crp <- colorRampPalette(brewer.pal(9, "YlGn"))
mapCountryData(mapToPlot = spdf,
               addLegend = FALSE,
               mapTitle = "Life expectancy in the year 2015")
polygon(coordinates(frm), col = "lightblue")
mapCountryData(mapToPlot = spdf, 
               nameColumnToPlot = "life_expectancy",
               colourPalette = crp(ncol),
               catMethod = m$cutVector,
               missingCountryCol = "lightgrey",
               addLegend = FALSE,
               mapTitle = "Life expectancy in the year 2015",
               add = TRUE)
llgridlines(spdf,
            easts = seq(-180, 180, 30),
            norths = seq(-90, 90, 30),
            plotLabels = FALSE,
            col = "darkgrey",
            lty = 3)
addMapLegend(cutVector = m$cutVector, 
             colourVector = crp(ncol),
             legendLabels = "all",
             legendShrink = 0.8,
             legendArgs = mtext("Life Expectancy In Years"))
dev.off()
openPDF(pdf_name)

?addMapLegend

# PROBLEM 4
# a) Import CSVs and join the data
life_expectancy <- read.csv("life_expectancy.csv", stringsAsFactors=FALSE)
population <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_10473719.csv", stringsAsFactors=FALSE, skip = 4, header = TRUE)


bubble_df <- population %>%
  select(code = Country.Code,
         pop = X2015) %>%
  right_join(life_expectancy, by = 'code')

# b) Make a bubble plot
graph <- ggplot(bubble_df, aes(gdp, life_expectancy)) +
  geom_point(aes(colour = continent, size = pop), alpha = 0.5) +
  xlab("GDP per capita (PPP)") +
  ylab("Life Expectancy (years)") +
  ggtitle("Health and Income of Nations in 2015") +
  theme_bw() +
  theme(plot.title = element_text(face="bold", hjust = 0.5))
graph

# c) Change to Logarithmic scale
graph <- graph + 
  scale_x_log10(breaks = c(1000, 10000, 100000), labels = c("$1,000", "$10,000", "$100,000")) 
graph
# ggplot2 prefers the log10 scale because it works for the full scale of numbers,
# whereas gapminder does a modified log2 scale, and it works only for their specific range

# d) scale_size_area()
graph <- graph +
  scale_size_area(name = "Population Size",
                  breaks = c(1000000, 10000000, 100000000, 1000000000),
                  labels = c("1 million", "10 million", "100 million", "1 billion"),
                  max_size = 25) 
graph

# e) change color scale

graph <- graph +
  scale_colour_brewer(palette= "Dark2",
                      direction = -1,
                      name = "Continents") 
graph

#f)
graph +
  geom_text(aes(label = ifelse((rank(gdp) <= 5 | rank(gdp) >= length(gdp) - 4 |
                                 rank(life_expectancy) <= 5 | 
                                 rank(life_expectancy) >= length(life_expectancy) - 4 |
                                 rank(pop) >= length(pop) - 4 |
                                 rank(life_expectancy) <= 5 | 
                                 rank(life_expectancy) >= length(life_expectancy) - 4) & 
                                 country != "Australia",
                               country,
                               ''),
                color = continent),
            vjust = 'inward', hjust = 'inward', size = 3) +
  geom_text(aes(label = ifelse(country == "Australia", country, ''), color = continent),
            nudge_y = 0.7, size = 3)

#g)
ggsave("bubble_chart.pdf", height = 9, width = 11)
openPDF("bubble_chart.pdf")

