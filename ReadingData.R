setwd("~/Documents/JuniorCalPoly/Stat331/final proj/stat331Final/")

library(maps)
library(rvest)
data("state.fips")


respondents <- read.delim("atusresp_0315.dat", sep = ",")
roster <-read.delim("atusrost_0315.dat", sep = ",")
activity <- read.delim("atusact_0315.dat", sep = ",")
summary <- read.delim("atussum_0315.dat", sep = ",")
who <- read.delim("atuswho_0315.dat", sep=",")
cps <- read.delim("atuscps_0315.dat", sep=",")
eldercare <- read.delim("atusrostec_1115.dat", sep=",")
weights <-read.delim("atuswgts_0315.dat", sep=",") 


#CREATE state factor and add to user.data along with PRAGE
st <- factor(cps$GESTFIPS)
levels(st) <- c(state.abb[1:8], "DC", state.abb[9:50])
cps$state <- st
user.data <-data.frame(respondents$TUCASEID)
colnames(user.data) <- c("TUCASEID")
user.data <- merge(user.data, cps[, c("TUCASEID", "state", "PRTAGE", "PEHRACTT", "PEEDUCA")], by="TUCASEID", all.x=TRUE)
# NOTE: incluedes DC - to ignore DC, simply [-9] on stCounts 


#Map Plot Stuff 


# reading data for average hours worked tab
user.data.hours <- user.data[user.data$PEHRACTT != -1 & user.data$PEEDUCA != -1,]
#hours.data <- aggregate(PRTAGE ~ state, user.data.hours, mean)

colnames(user.data.hours) <- c("id", "state", "age", "hoursWorked", "education")
table(user.data.hours$education)

hs_incomplete <- user.data.hours[user.data.hours$education <= 38,]
hs_complete <- user.data.hours[user.data.hours$education ==39,]
some_college <- user.data.hours[user.data.hours$education ==40,]
ba_complete <- user.data.hours[ (41 <= user.data.hours$education) &(user.data.hours$education <= 43),]
masters_above <- user.data.hours[user.data.hours$education >=44,]



#State populations in 2003 --> data from study is from 2003-2015 - this could be cool for mapping 
url <- "http://www.infoplease.com/ipa/A0004986.html"

link <- read_html(url)

html <- html_nodes(link, css="table")

statePopulations <- html_table(html, fill=T)
statePopulations <- statePopulations[[2]]
statePopulations[-1] <- as.data.frame( lapply(statePopulations[-1], function(x) { as.numeric(as.character(gsub("[^0-9]", "", x))) }))

# lat long for labeling states 
centers <- read.csv('state_latlon.csv')
centers <- centers[c(-1, -4,-9, -13, -27,-42, -50),]
centers
