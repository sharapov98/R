install.packages("e1071")
install.packages("purrr")
library(dplyr)
test<-list(year=2018, arr=array(seq_len(24), dim = c(3,4,2),
                                dimnames = list(c("A","B","C"), 
                                                c("a","b","c","d"), 
                                                c("I","II"))))
str(test)
?str
x<-test[[2]]

b2<- beaver2
b1$id<-seq_len(1)
b2$id<-seq_len(2)

bt<-rbind(b1,b2)
b.act<-bt[bt$activ==1,]

iris2<-iris
iris_sorted<-iris2[order(iris2$Petal.Length, iris2$Petal.Width),]
colnames(iris_sorted)[3]<-"Length"
colnames(iris_sorted)[4]<-"Width"

iris_sorted <- iris_sorted[,c(5,4,3,1,2)]

car<-cars
is.data.frame(car)
is.list(car)
str(car)
attributes(car)
copy<-car
class(car)<-NULL
attributes(car)
cars

install.packages("rvest")
library(rvest)
url <- "https://en.wikipedia.org/wiki/2015%E2%80%9316_Premier_League"

# Reading the HTML code from the website
webpage <- read_html(url) %>%

# Using CSS selectors as second argument to select the relevant part of
# the HTML.
data_html <- html_nodes(webpage, ".mw-headline")

# Converting the HTML to text.
data_text <- html_text(data_html)
head(data_text)
library(magrittr)
wikitable<- url %>% 
  read_html() %>% 
  html_nodes(".wikitable") %>%
  html_table(fill=TRUE) %>%
  extract2(2)
url<- tinyurl.com/y67t4zxh 
boxoffice<- "http://tinyurl.com/yy75hobg" %>% 
  read_html() %>% 
  html_nodes(".wikitable") %>%
  html_table(fill=TRUE) %>%
  extract2(1)
death<-read.csv("bodycount.csv")

#elect() - select columns  filter() - select by row content summarize() - create precise summaries
#group_by() - group together according to a variable5. arrange() - sort
#join() - merge mutate() & transmute() - excel-like functions
install.packages("hflights")
library(hflights)
data(hflights)

airline_codes <-"http://tinyurl.com/yyty5gp4" %>%
  read_html() %>%
  html_node("table") %>%
  html_table()

hflights2<- hflights %>%
  mutate(FlightNum=paste0(UniqueCarrier, FlightNum))
  left_join(airline_codes, by = c("UniqueCarrier"="IATA Carrier Code")) %>%
  select(Airline="Carrier Name", FlightNum,
         Origin, Dest, Cancelled)
table(hflights$Origin)
table(hflights$UniqueCarrier)
hflights%>%
  group_by(FlightNum, Origin, Dest, Cancelled)%>%
  summarise(n_flights=n()) %>%
  arrange(desc(n_flights))

