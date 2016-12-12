#setwd("~/Documents/JuniorCalPoly/Stat331/final proj/stat331Final/")

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

us <- map_data("state")

arr <- USArrests %>% 
  tibble::rownames_to_column("region") %>% 
  mutate(region=tolower(region))

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









clustering<-function(n=2,df,clustcolumns,plotcolumns){
  set.seed(20)
  clusters <- kmeans(df[,clustcolumns], n, nstart = 20)$cluster
  message("#pastit")perscare <-c("010101","010102","010199","010201","010299","010301","010399","010401","010499","010501","010599","019999")
household <-c("020101","020102","020103","020104","020199","020201","020203","020299","020301","020302","020303","020399","020401","020402","020499","020501","020502","020599","020681","020699","020701","020799","020801","020899","020901","020902","020903","020904","020905","020999","029999")
caringhh <- c("030101","030102","030103","030104","030105","030186","030108","030109","030110","030111","030112","030199","030201","030202","030203","030204","030299","030301","030302","030303","030399","030401","030402","030403","030404","030405","030499","030501","030502","030503","030504","030599","039999")
caringnhh <- c("040101","040102","040103","040104","040105","040186","040108","040109","040110","040111","040112","040199","040201","040202","040203","040204","040299","040301","040302","040303","040399","040401","040402","040403","040404","040405","040499","040501","040502","040503","040504","040505","040506","040507","040508","040599","049999")
Work <- c("050101","050102","050103","050189","050201","050202","050203","050204","050289","050301","050302","050303","050304","050389","050481","050403","050404","050405","050499","059999")
Education <-c("060101","060102","060103","060104","060199","060201","060202","060203","060289","060301","060302","060303","060399","060401","060402","060403","060499","069999")
shopping <-c("070101","070102","070103","070104","070105","070199","070201","070299","070301","070399","079999")
careserv <-c("080101","080102","080199","080201","080202","080203","080299","080301","080302","080399","080401","080402","080403","080499","080501","080502","080599","080601","080602","080699","080701","080702","080799","080801","080899","089999")
householdserv <-c("090101","090102","090103","090104","090199","090201","090202","090299","090301","090302","090399","090401","090402","090499","090501","090502","090599","099999")
govserv <- c("100101","100102","100103","100199","100201","100299","100381","100383","100399","100401","100499","109999")
eat <- c("110101","110199","110281","110289","119999")
Social <- c("120101","120199","120201","120202","120299","120301","120302","120303","120304","120305","120306","120307","120308","120309","120310","120311","120312","120313","120399","120401","120402","120403","120404","120405","120499","120501","120502","120503","120504","120599","129999")
Sports<-c("130101","130102","130103","130104","130105","130106","130107","130108","130109","130110","130111","130112","130113","130114","130115","130116","130117","130118","130119","130120","130121","130122","130123","130124","130125","130126","130127","130128","130129","130130","130131","130132","130133","130134","130135","130136","130199","130201","130202","130203","130204","130205","130206","130207","130208","130209","130210","130211","130212","130213","130214","130215","130216","130217","130218","130219","130220","130221","130222","130223","130224","130225","130226","130227","130228","130229","130230","130231","130232","130299","130301","130302","130399","130401","130402","130499","139999")
Religious<-c(140101:140105,149999)
Volunteer<-c(150101:150106,150199,150201:150204, 150299,150301:150302,150399,150401,150402,150499,150501,150599,150601,150602,150699,159989)
telephone<-c(160101:160108,169989)
travel<-c(180101,180199,180280,180381,180382,180399,180481,180482,180499,180501,180502,180589,180601,180682,180699,180701,180782,180801:180807,180899,180901,180902,180903,180904,180905,180999,181081,181002,181099,181101,181199,181201,181202,181283,181204,181299,181301,181302,181399,181401,181499,181501,181599,181601,181699,181801,181899,189999)
falsehoods<-c(500101,500103:500107,509989)
all<-c(perscare,household,caringhh,caringnhh,work,education,shopping,careserv,householdserv,govserv,eat,social,religious,volunteer,telephone,travel,falsehoods,sports)
#"Work","Education", "Social","Sports","Age", "Weekly Earnings","Religious",Volunteer"
answers<-c("perscare","household","caringhh","caringnhh","Work","Education","shopping","careserv","householdserv","govserv","eat","Social","Religious","Volunteer","telephone","travel","falsehoods","Sports")

summary$Age=summary$TEAGE
summary$WeeklyEarnings=summary$TRERNWA
summary$household=0
summary$perscare=0
summary$caringhh=0
summary$caringnhh=0
summary$Work=0
summary$Education=0
summary$shopping=0
summary$careserv=0
summary$householdserv=0
summary$govserv=0
summary$eat=0
summary$Social=0
summary$Religious=0
summary$Volunteer=0
summary$telephone=0
summary$travel=0
summary$falsehoods=0
summary$Sports=0
for (i in 1:length(answers)){
  for (j in 1:length(get(answers[i]))){
    #message(answers[i])
    #message(paste("t",get(answers[i])[j],sep=""))
    summary[answers[i]]=summary[answers[i]]+summary[paste("t",get(answers[i])[j],sep="")]
  }
}

nodegrees<-c(31:38)
hsdegree<-c(39)
somecollege<-c(40:42)
bachelors<-c(43)
masters<-c(44)
otherhigher<-c(45,46)
all<-c("nodegrees","hsdegree","somecollege","bachelors","masters","otherhigher")
summary$Education="NULL-needs UPDATING"

for (i in 1:length(all)){
  currentlist<-get(all[i])
  for (j in 1:length(currentlist)){
    message(currentlist)
    summary[summary$PEEDUCA==currentlist[j],]$educ=all[i]
  }
}

days<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
summary$Day="Sunday"
for (i in 2:7){
  summary[summary$TUDIARYDAY==i,]$day=days[i]
}

race<-c("White","Black","Native American","Asian","PIslander")
summary$Race="Mixed"
for (i in 1:5){
  summary[summary$PTDTRACE==i,]$race=race[i]
}
  if (length(plotcolumns)!=2){
  } else{
    return(ggplot(df)+geom_point(aes_string(x=plotcolumns[1],y=plotcolumns[2],col=clusters),alpha=0.2))
  }
}



plotter<-function(x,y,cat=c(-5),dframe){
  #xf=unlist(dframe[x])
  #yf=unlist(dframe[y])
  if ((class(x)=="integer" || class(x)=="numeric") && (class(y)=="integer" || class(y)=="numeric")){
    message("yo")
    if (cat[1]==-5){
      message("dawg")
      return(ggplot(dframe)+geom_point(aes(x=x,y=y),alpha=.2)+geom_smooth(method="lm"))
      #qplot(data=dframe,x=x,y=y)
    } else {
      message("entered")
      return(ggplot(dframe)+geom_point(aes_string(x=x,y=y,col=cat),alpha=0.2))
      #    qplot(data=dframe,x=x,y=y,col=cat)
    }
  } else if (class(x)=="character"){
    message("wrong")
    return(ggplot(dframe) +geom_bar(aes_string(x,y,fill="blue"),col="black",fill="blue",position = "dodge", stat = "summary", fun.y ="mean"))
    
  }
}

capitalize <- function(x) {
  up <- paste(toupper(substring(x, 1,1)) , substring(x, 2), sep="")
  return(up)
}


colnames(summary)
plotter("perscare", "TRYHHCHILD", "sports", dframe=summary)
class(summary$perscare)
head(summary)
