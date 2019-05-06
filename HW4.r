################################ Imports ################################
library(MASS)
library(vioplot)
library(rvest)
library(magrittr)
library(dplyr)
library(countrycode)
library(knitr)
library(Biobase)


################################ Question 1 ################################
#(a) 
ChickenWeights_Feed <- read.csv("ChickenWeights_Feed.csv", stringsAsFactors = FALSE)
ChickenWeights_Feed$feed <- factor(ChickenWeights_Feed$feed)
# after importing the ChickenWeights_Feed file into my Rstudio. I input this code in case I wipe out my environment pane. 
View(ChickenWeights_Feed) # to do a check up on my data set.
str(ChickenWeights_Feed)

# Here is the structure of the object str(ChickenWeights_Feed):
# 'data.frame':	71 obs. of  3 variables:
#  $ animal: chr  "chick1" "chick2" "chick3" "chick4" ...
#  $ feed  : Factor w/ 6 levels "casein","horsebean",..: 4 3 4 6 2 2 6 5 3 4 ...
#  $ weight: int  344 213 263 334 227 179 340 230 271 258 ...

#(b)
ChickenWeights_Feed %<>% mutate(weight_kg = weight/1000)
# creating a new column called "weight_kg", which contains the Chicks' weight in kilograms. 
head(ChickenWeights_Feed, 5) # this code allows us to see the first few rows of the data frame. 

# animal      feed weight weight_kg
# 1 chick1  meatmeal    344     0.344
# 2 chick2   linseed    213     0.213
# 3 chick3  meatmeal    263     0.263
# 4 chick4 sunflower    334     0.334
# 5 chick5 horsebean    227     0.227

#(c)
ChickenWeights_Feed$weight_kg <- NULL # to remove the column called "weight_kg". 
tail(ChickenWeights_Feed, 5) # to show the bottom 5 rows.

# animal      feed     weight
# 67 chick67   soybean    248
# 68 chick68 horsebean    217
# 69 chick69   linseed    148
# 70 chick70 horsebean    136
# 71 chick71 sunflower    320


#(d) 
length(levels(ChickenWeights_Feed$feed)) 
# from this code, there are 6 unique feed supplements in this data set. 
# casein horsebean linseed meatmeal soybean sunflower
counts <- ChickenWeights_Feed %>%
  group_by(feed) %>%
  summarise(count=n())
# the number of animals that receive each type of feed are as such:
# feed      count
# <fct>     <int>
# 1 casein       12
# 2 horsebean    10
# 3 linseed      12
# 4 meatmeal     11
# 5 soybean      14
# 6 sunflower    12

#(e)
feedorder <- ChickenWeights_Feed %>% 
  group_by(feed) %>% 
  summarize(median(weight)) %>%
  arrange(desc(UQ(sym('median(weight)'))))
# the median weight of the chicks for each feed supplement are as such: 
# feed      `median(weight)`
# <fct>                <dbl>
# 1 casein                342 
# 2 sunflower             328 
# 3 meatmeal              263 
# 4 soybean               248 
# 5 linseed               221 
# 6 horsebean             152

#(f)
overall <- left_join(feedorder, counts, by = 'feed')
pdf_name <- "Chickweight_6weeks.pdf"
pdf(file = pdf_name, width = 8, height = 6)
par(mgp = c(3, 1.5, 0))
vioplot(ChickenWeights_Feed$weight ~ factor(ChickenWeights_Feed$feed, levels = overall$feed),
        main = "Chick weight after 6 weeks on feed supplements",
        ylab = "Weight (grams)",
        xlab = "Type of feed supplement",
        col = "orange",
        names = paste0(overall$feed, "\n {n=", overall$count, "}"),
        cex.lab = 1,
        areaEqual = TRUE)
abline(h = seq(100,400, by = 50), v = seq_len(6), col = 'gray', lty = 'dotted')
mtext('Source: McNeil (1977) Interactive Data Analysis', side = 3, line = 0.5)
dev.off()
openPDF(pdf_name)

#(g)
# We can do an Analysis of Variance test to see if there is a recommended feed

# Check Nearly Normal condition
a <- aov(ChickenWeights_Feed$weight ~ ChickenWeights_Feed$feed)
r <- residuals(a)
qqnorm(r)
qqline(r)
# Seems generally okay

# Bartlett test for equal variance
bartlett.test(ChickenWeights_Feed$weight ~ ChickenWeights_Feed$feed)
# p-value of 0.66 is too high for us to question if variances are equal. Thus, we proceed.

summary(a)
# p-value of 5.94e-10 means that it is very likely that one of the feeds has a statistically significant effect. 

# However, we are unsure which one is the one with the significant effect. 
# From the violin plot, we can tell that horsebean is very ineffective in increasing weight, more so than the rest.


################################ Question 2 ################################
data(Animals)
dim(Animals)
head(Animals)

#a) Using syntax to manipulate data frames, add a column of brain-to-body ratio and use it to remove the
#three species with the lowest ratios (i.e. the dinosaurs). After filtering, please remove the ratio column
#as it is no longer needed.

# install.packages("tibble") 
library(tibble) #package for preserving rownames

data(Animals)
animals <- Animals %>% 
  rownames_to_column('anim') %>%
  mutate(ratio = brain/body) %>%
  filter(ratio > sort(ratio)[3]) %>%
  column_to_rownames('anim')
animals$ratio <- NULL

#b) Add a numeric column is_primate indicating whether the animal species belongs to the primate family
#(1=Yes, 0=No).
is_primate <- numeric(nrow(animals))
names(is_primate) <- rownames(animals)
for(i in c('Human', 'Rhesus monkey', 'Chimpanzee', 'Potar monkey', 'Gorilla')){
  is_primate[i] <- 1
}

animals %<>% mutate(is_primate = as.vector(is_primate))

#c) Let us fit a linear regression model for brain mass as a function of body mass and whether the species
#belongs to the taxonomic order of primates. Remember to take both mass values on a log scale. (Recall
#the Tukey’s ladder exercise in problem set 2). Store the output of the linear regression as “fit” and
#run an str() on it.

fit <- lm(log(brain) ~ log(body) + is_primate, data = animals)
str(fit)

#d) Demonstrate that the “fit” object is indeed a list. Extract the names of the elements in this list.

is.list(fit) #returns TRUE
#we can also use double bracket subsetting
fit[[1]] #returns the "coefficients" list, ie. includes the Intercept, and coefficients of our variables
names(fit)

#e) Plot the residuals vs. fitted values by extracting them directly from “fit” with list-style subsetting 
#(i.e. without using residuals(fit) or fitted(fit)). Remember to label your axes and titles
#appropriately. (No need to export to PDF.)

plot(fit[["residuals"]] ~ fit[["fitted.values"]],
     main = "Residuals vs. Fitted values of fit",
     ylab = "Residuals",
     xlab = "Fitted Values")
abline(h=0, col = 'red')

#f) The object “fit” actually contains the original data that was entered for linear regression. Identify
#it and show the top six lines. Then remove the original data from “fit”, which is sometimes a
#useful exercise to save space when analyzing extremely large datasets (rest of the functionality seems
#unaffected).
data.frame(1)
head(fit[["model"]])
fit["model"] <- data.frame(1)

#g) Run s <- summary(fit) and inspect the contents. From the fitted coefficients, extract the estimate
#and p-value for log10(body) and for being a primate from s as a matrix. That is, s should contain
#two columns (Estimate and Pr(>|t|)) and two rows (log10(body) and is_primate).

s <- summary(fit)
names(s)
mtrx <- as.matrix(s[["coefficients"]])
mtrx <- mtrx[-1,c("Estimate","Pr(>|t|)")]
print(mtrx)

################################ Question 3 ################################
#a
medal <- read.csv("medal.csv")

medal <- medal %>% 
  arrange(-Gold, -Silver, -Bronze)

rank <- numeric(length(medal$Country))
rank[1] <- 1
running = 1
for(i in seq(2,length(medal$Country))){
  if(!all(medal[i,-1] == medal[i-1,-1])){
    running <- i
    rank[i] <- i
  } else {
    rank[i] <- running
  }
}

medal %<>% mutate(Rank = rank)
write.csv(medal, file = "medal_rank_conventional.csv")

#b
WorldBank_raw <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_10473719.csv", stringsAsFactors = FALSE)
WorldBank <- WorldBank_raw[-c(1:4),]
names(WorldBank) <- WorldBank_raw[4,]

WorldBank %<>% dplyr::select(CountryName = 'Country Name', CountryCode = 'Country Code', Population = '2016')
#c
setdiff(medal$Country, WorldBank$Country)

#d
medal %<>% mutate(CountryCode = countrycode(Country,'country.name','iso3c'))

#e
newranks <- medal %>%
  inner_join(WorldBank, by = 'CountryCode') %>%
  mutate(Total = Gold + Silver + Bronze, 
         Population_in_millions = Population/1000000,
         Medals_per_million = Total / Population_in_millions, 
         Population = NULL,
         CountryName = NULL,
         CountryCode = NULL,
         Rank = NULL) %>%
  filter(!is.na(Population_in_millions))
  arrange(desc(Medals_per_million))
rank <- numeric(length(newranks$Country))
rank[1] <- 1
running = 1
for(i in seq(2,length(newranks$Country))){
  if(!(newranks[i,"Medals_per_million"] == newranks[i-1, "Medals_per_million"])){
    running <- i
    rank[i] <- i
  } else {
    rank[i] <- running
  }
}
newranks %<>% mutate(Rank = rank)

################################ Question 4 ################################


# a) Webscrape the URL

LE_url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy#List_by_the_World_Health_Organization_(2015)'

WHO_table <- LE_url %>%
  read_html() %>%
  html_nodes('.wikitable') %>%
  html_table(fill = TRUE) %>%
  extract2(1) %>%
  dplyr::select(Country = 'Country and regions', Overall = 'Both sexes lifeexpectancy')

UN_table <- LE_url %>%
  read_html() %>%
  html_nodes('.wikitable') %>%
  html_table(fill = TRUE) %>%
  extract2(2) %>%
  dplyr::select(Country = 'State/Territory', 'Overall')

# b) Differences in the Tables

print(setdiff(WHO_table$Country, UN_table$Country)) # Countries in WHO list, not in UN list
# [1] "France"                "Macedonia"             "Micronesia"            "Sao Tome and Principe" "DR Congo"              "Cote d'Ivoire"   

print(setdiff(UN_table$Country, WHO_table$Country)) # Countries in UN list, not in WHO list
#  [1] "Hong Kong"                        "France (metropol.)"               "Martinique ( France)"             "Guadeloupe ( France)"            
# [5] "Channel Islands ( UK)"            "Macau"                            "U.S. Virgin Islands ( US)"        "Réunion ( France)"               
# [9] "Mayotte ( France)"                "Taiwan"                           "Puerto Rico ( US)"                "French Guiana ( France)"         
# [13] "Guam ( US)"                       "Curaçao ( Netherlands)"           "New Caledonia ( France)"          "French Polynesia ( France)"      
# [17] "Aruba ( Netherlands)"             "Republic of Macedonia"            "Palestine"                        "Federated States of Micronesia"  
# [21] "Western Sahara"                   "São Tomé and Príncipe"            "Democratic Republic of the Congo" "Côte d'Ivoire"  

# c) countrycode()
WHO_table$Country <- countrycode(WHO_table$Country, "country.name", "iso3c")
UN_table$Country <- countrycode(UN_table$Country, "country.name", "iso3c")

print(setdiff(WHO_table$Country, UN_table$Country)) 
print(setdiff(UN_table$Country, WHO_table$Country))

# d) Intersection and Merging
length(intersect(WHO_table$Country, UN_table$Country)) # 183 in intersection, but 182 countries if we don't count NA

merged_table <- inner_join(filter(WHO_table, !is.na(Country)), filter(UN_table, !is.na(Country)), by = "Country")
names(merged_table) <- c("Country", "WHO_Overall", "UN_Overall")

# e) Plotting Bland-Altman Plot
merged_table %<>% mutate(mean = (WHO_Overall + UN_Overall)/2, diff = WHO_Overall - UN_Overall)

lr = lm(merged_table$UN_Overall ~ merged_table$WHO_Overall)

pdf_name <- "BlandAltman.pdf"
pdf(file = pdf_name, width = 10, height = 5)
par(mfrow = c(1,2),
    mar = c(4, 6, 4, 4),
    mgp = c(2, 0.75, 0),
    mai = c(1, 0.7, 0.8, 0.5))
plot(merged_table$WHO_Overall,
     merged_table$UN_Overall,
     main = "Overall Life Expectancy",
     ylab = "UN estimate",
     xlab = "WHO estimate")
abline(lr, col = "blue")
plot(merged_table$mean,
     merged_table$diff,
     main = "Bland-Altman plot for Overall Life Expectancy",
     ylab = "WHO estimate - UN estimate",
     xlab = "(WHO estimate + UN estimate) / 2")
abline(h = 0, col = "black")
abline(h = c(mean(merged_table$diff), 
             mean(merged_table$diff) + 1.96*sd(merged_table$diff),
             mean(merged_table$diff) - 1.96*sd(merged_table$diff)),
       col = "red",
       lty = "dashed")
dev.off()
plot_crop(pdf_name)
openPDF(pdf_name)

# COMMENT:
# There seems to be a trend where the UN and WHO estimates seem to agree the higher the life expectancy is
# in a country. As there could be a link between the level of development of a country and the life expectancy,
# it could be a case of the more developed a country is, the higher the life expectancy, and also the more 
# available / accurate the information available on national statistics like life expectancy. Of course, this 
# is merely a guess from observing the data.

# f) List of countries with difference of 5 years or more

bigdiff <- merged_table %>%
  mutate(absdiff = abs(diff)) %>%
  filter(absdiff >= 5) %>%
  arrange(desc(absdiff)) %>%
  dplyr::select(Country, diff)
print(bigdiff)