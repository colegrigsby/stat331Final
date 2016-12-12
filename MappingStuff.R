library(maps)
library(ggplot2)

#state names 
arr <- data.frame(state.name)
colnames(arr) <- "region"


#plot nation returns the counts and name of the data for the map to be updated for general user data 
plotNation <- function (data, age) {
  stCounts <- data.frame(table(user.data$state[user.data$PRTAGE >= age[1] & user.data$PRTAGE <= age[2]]))$Freq
  counts<-data.frame(rep(NA, 50))
  counts$Count <- stCounts[-9]
  #message(data)
  name <- "Count"
  
  if (data == "Percent of State Population Participating"){
    counts$Percent <- stCounts[-9] / statePopulations$`2010pop.`[c(-9,-52)]
    name <- "Percent"
    
  }
  if(data == "Percentage that hold a bachelors"){ #TODO 
    bachelorCounts <- sapply(sapply(split(user.data$PEEDUCA[user.data$PRTAGE >= age[1] & user.data$PRTAGE <= age[2]], user.data$state), function(f){ return(f==43)}), sum)
    
    bachelorCounts <- data.frame(bachelorCounts)
    
    counts$Percent <- bachelorCounts$bachelorCounts[-9] / stCounts[-9]
    name <- "Percent"
    
  }
  if(data == "Average Age of Participants"){ #TODO should it just be the counts and not the mean...? 
    #ageCounts is the average age per state 
    ageCounts <- sapply(split(user.data$PRTAGE[user.data$PRTAGE >= age[1] & user.data$PRTAGE <= age[2]], user.data$state), mean)
    ageCounts <- data.frame(ageCounts)
    counts$Age <- ageCounts$ageCounts[-9]
    name <- "Age"
    
  }
  
  counts$region <- arr$region
  return(list(counts, name))
}


#handles the data for the education plot of the national map 
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
  averages <- aggregate(hoursWorked ~ state, plotThis, mean)
  averages <- averages[-9,] 
  averages$region <- arr$region

  colnames(averages) <- c("state", "Hours Worked", "region")
  
  return(averages)
}

