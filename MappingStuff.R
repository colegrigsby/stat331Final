library(maps)
library(ggplot2)

plotNation <- function (data, age) {
  stCounts <- data.frame(table(user.data$state[user.data$PRTAGE >= age[1] & user.data$PRTAGE <= age[2]]))$Freq
  counts<-data.frame(rep(NA, 50))
  counts$stCounts <- stCounts[-9]
  
  
  #if(data == "Percentage that hold a bachelors"){
    #counts$stCounts <- bachelorCounts / stCounts[-9]
  #}
  if(data == "Average Age of Participants"){
    #ageCounts is the average age per state 
    
    ageCounts <- sapply(split(user.data$PRTAGE[user.data$PRTAGE >= age[1] & user.data$PRTAGE <= age[2]], user.data$state), mean)
    
    ageCounts <- data.frame(ageCounts)
    
    counts$stCounts <- ageCounts$ageCounts[-9]
  }
  
  #TODO change color based off what were showing :) 
  
  counts$region <- arr$region
  
#variables that could be plotted


#mapPlot <- function(type) {

  gg <- ggplot()
  gg <- gg + geom_map(data=us, map=us,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)
  ### FILL HERE WITH WHAT WE want to see 
  gg <- gg + geom_map(data=counts, map=us,
                      aes(fill=stCounts, map_id=region),
                      color="#ffffff", size=0.15)
  gg <- gg + scale_fill_continuous(low='thistle2', high='darkblue', guide=guide_colorbar(title=data))
  gg <- gg + labs(x=NULL, y=NULL)  
  gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
  gg <- gg + theme(panel.border = element_blank(),panel.background = element_blank(),axis.ticks = element_blank(), axis.text = element_blank())
  return(gg)
  
}
# 
#   
#   ### FILL HERE WITH WHAT WE want to see 
#   
#   if (type == "stCounts") {
#     gg <- gg + geom_map(data=counts, map=us,
#                         aes(fill=stCounts, map_id=region),
#                         color="#ffffff", size=0.15)  
#   }
#   if (type == "avgWorkedHours") {
#     gg <- gg + geom_map(data=counts, map=us,
#                         aes(fill=avgWorkedHours, map_id=region),
#                         color="#ffffff", size=0.15)
#   }
#   
#   
#   gg <- gg + scale_fill_continuous(low='thistle2', high='darkblue', 
#                                    guide='colorbar')
#   gg <- gg + labs(x=NULL, y=NULL)
#   gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
#   gg <- gg + theme(panel.border = element_blank(),panel.background = element_blank(),axis.ticks = element_blank(), axis.text = element_blank())
#   gg  
# }

