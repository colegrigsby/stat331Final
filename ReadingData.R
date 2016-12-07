setwd("~/Documents/JuniorCalPoly/Stat331/final proj/stat331Final/")

library(maps)
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
user.data <- data.frame(respondents$TUCASEID)
colnames(user.data) <- c("TUCASEID")
user.data <- merge(user.data, cps[, c("TUCASEID", "state", "PRTAGE", "PEHRACTT", "PEEDUCA")], by="TUCASEID", all.x=TRUE)
# NOTE: incluedes DC - to ignore DC, simply [-9] on stCounts 
stCounts <- data.frame(table(st))$Freq

#Map Plot Stuff 
us <- map_data("state")
arr <- USArrests %>% 
  add_rownames("region") %>% 
  mutate(region=tolower(region))
arr
counts<-data.frame(rep(NA, 50))
counts$stCounts <- stCounts[-9]
counts$region <- arr$region

# hours.data
user.data <- user.data[user.data$PEHRACTT != -1 & user.data$PEEDUCA != -1,]
hours.data <- aggregate(PRTAGE ~ state, user.data, mean)






