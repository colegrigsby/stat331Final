library(maps)
library(ggplot2)

plotNation <- function (data, age) {
  stCounts <- data.frame(table(user.data$state[user.data$PRTAGE >= age[1] & user.data$PRTAGE <= age[2]]))$Freq
  counts<-data.frame(rep(NA, 50))
  counts$stCounts <- stCounts[-9]
  message(data)
  
  if (data == "Percent of State Population Participating"){
    counts$stCounts <- stCounts[-9] / statePopulations$`2010pop.`[c(-9,-52)]
    
  }
  if(data == "Percentage that hold a bachelors"){ #TODO 
    bachelorCounts <- sapply(sapply(split(user.data$PEEDUCA[user.data$PRTAGE >= age[1] & user.data$PRTAGE <= age[2]], user.data$state), function(f){ return(f==43)}), sum)
    
    bachelorCounts <- data.frame(bachelorCounts)
    
    counts$stCounts <- bachelorCounts$bachelorCounts[-9] / stCounts[-9]
  }
  if(data == "Average Age of Participants"){ #TODO should it just be the counts and not the mean...? 
    #ageCounts is the average age per state 
    
    ageCounts <- sapply(split(user.data$PRTAGE[user.data$PRTAGE >= age[1] & user.data$PRTAGE <= age[2]], user.data$state), mean)
    
    ageCounts <- data.frame(ageCounts)
    
    counts$stCounts <- ageCounts$ageCounts[-9]
  }
  
  #TODO change color based off what were showing :) 
  
  counts$region <- arr$region
  
#variables that could be plotted

  return(counts)
  #returning the dataframe with plotting information so gVis plot can be generated

#  gg <- ggplot()
#  gg <- gg + geom_map(data=us, map=us,
#                      aes(x=long, y=lat, map_id=region),
#                      fill="#ffffff", color="#ffffff", size=0.15)
  ### FILL HERE WITH WHAT WE want to see 
#  gg <- gg + geom_map(data=counts, map=us,
#                      aes(fill=stCounts, map_id=region),
#                      color="#ffffff", size=0.15)
#  gg <- gg + scale_fill_continuous(low='thistle2', high='darkblue', guide=guide_colorbar(title=data))
#  gg <- gg + labs(x=NULL, y=NULL)  
#  gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
#  gg <- gg + theme(panel.border = element_blank(),panel.background = element_blank(),axis.ticks = element_blank(), axis.text = element_blank())
#  return(gg)
  
}

plotNationEducation <- function(education) {
 
  plotThis <- hs_incomplete
  
  if (education == "High School Incomplete") {
    plotThis <- hs_incomplete
  }
  if (education == "High School Diploma") {
    plotThis <- hs_complete
  }
  if (education == "Some College") {
    plotThis <- some_college
  }
  if (education == "Bachelor's/Associate's") {
    plotThis <- ba_complete
  }
  if (education == "Masters and Above") {
    plotThis <- masters_above
  }
  head(ba_complete)
  
  
  averages <- aggregate(hoursWorked ~ state, plotThis, mean)
  averages <- averages[-9,] 
  averages$region <- arr$region


  

  
  test <-  merge(arr, averages, by="region")[c(1,7)]
  
  gg <- ggplot()
  gg <- gg + geom_map(data=us, map=us,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)

  gg <- gg + geom_map(data=test, map=us,
                      aes(fill=hoursWorked, map_id=region),
                      color="#ffffff", size=0.15)
  
  # gg <- gg + geom_text(aes(x = longitude, y = latitude, label = state), # USE this to label states if need be 
  #                      data= centers,
  #                      alpha = 1,
  #                      color = "black")
  # 
  gg <- gg + scale_fill_continuous(low='thistle2', high='purple', 
                                   guide='colorbar')
  gg <- gg + labs(x=NULL, y=NULL)
  gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
  gg <- gg + theme(panel.border = element_blank())
  gg <- gg + theme(panel.background = element_blank())
  gg <- gg + theme(axis.ticks = element_blank())
  gg <- gg + theme(axis.text = element_blank())
  return(gg)
  
  
}

