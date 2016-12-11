#install.packages('rsconnect')
#install.packages(c('ggplot2', 'shiny'))
#install.packages("googleVis")
#install.packages("devtools")
library(devtools)
library(dplyr)
library(rsconnect)
library(ggplot2)
library(shiny)
library(googleVis)
library(maps)
library(rvest)



#source("ReadingData.R")
#source("MappingStuff.R")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("National Map", fluidRow(column(8,h1(textOutput("header")),offset=2)),
                            
                            fluidRow(column(8,htmlOutput("gvis"), offset=3)),
                            fluidRow(column(5,radioButtons("whichPlot", label="Change Plot", 
                                                           choices = c("Participant Data" = 2, "Education Data" = 1), selected=1,inline=TRUE), offset=4)),
             
                                fluidRow(      
                                  column(4,
                                  h5("Participant Demographics Plot"),
                                  sliderInput("ageRange","Age",min(user.data$PRTAGE),max(user.data$PRTAGE),c(min(user.data$PRTAGE),max(user.data$PRTAGE))),
                                  selectInput("mapData", 
                                               "Select what you would like to look at",
                                               choices=c("Percentage that hold a bachelors",
                                                         "General Number of Participants",
                                                         "Average Age of Participants",
                                                         "Percent of State Population Participating"), 
                                                          selected="General Number of Participants")
                                  ,offset=1, style = "background-color:#D1D0CE;"
                                  ),
                                  
                                  column(4,
                                         h5("Education Plot Options"),
                                         radioButtons("educationLevel1", label="Highest level of Education Obtained",
                                                     choices = c("High School Incomplete",
                                                       "High School Diploma",
                                                       "Some College",
                                                       "Bachelor's/Associate's",
                                                       "Masters and Above"), selected = "Bachelor's/Associate's"),
                                          offset=2, style = "background-color:#D1D0CE;"
                                         )                                    
                                )
                       #TODO add another sidebar panel or something with some quick facts 
                        # max, min, mean etc.... 
                        # ACTIVITIES PER STATE WOULD BE DOPE 
                              
             


                              ),
    tabPanel("Clustering",
             h1("Clustering"),
             sidebarPanel(radioButtons("chooseX", label="Choose an X variable", choices= c("Education", 
                                                                                           "Social", 
                                                                                           "Eat", 
                                                                                           "Social", 
                                                                                           "Religious"), 
                                       selected="Education"),
                          
                          radioButtons("chooseY", label="Choose a Y variable", choices= c("Education", 
                                                                                          "Social", 
                                                                                          "Eat", 
                                                                                          "Social", 
                                                                                          "Religious"), 
                                       selected="Education")               
             ),
             mainPanel(
               plotOutput("clusters")
             )
             
             
             
    ),
    tabPanel("Regression?",
             h1("Regression"),
             sidebarPanel(radioButtons("chooseX", label="Choose an X variable", choices= c("Education", 
                                                                                        "Social", 
                                                                                        "Eat", 
                                                                                        "Social", 
                                                                                        "Religious"), 
                                    selected="Education"),
                          
                          radioButtons("chooseY", label="Choose a Y variable", choices= c("Education", 
                                                                                           "Social", 
                                                                                           "Eat", 
                                                                                           "Social", 
                                                                                           "Religious"), 
                                       selected="Education"),
                          radioButtons("chooseCat", label="Choose a categorical variable", choices= c("Education", 
                                                                                          "Social", 
                                                                                          "Eat", 
                                                                                          "Social", 
                                                                                          "Religious"), 
                                       selected="Education")
             ),
             mainPanel(
               plotOutput("plotter")
             ),
             
             fluidRow(),
             fluidRow(),
             fluidRow()
    )
    
    
    
  )
  
)



server <- function(input,output) {
  colors <- c("darkblue", "darkgreen", "blueviolet")
  
  output$clusters <- renderPlot({
    clustering(n=2,df=10,summary$GTMETSTA,summary$PEEDUCA)
  }) 
  
  output$plotter <- renderPlot({
    plotter("perscare", "social", dframe=summary)
  }) 
  
  startAge <- reactive({
    input$ageRange[1]
  })
  endAge <- reactive({
    input$ageRange[2]
  })
  output$header <- renderText({
    if (input$whichPlot == 2) {
      paste("Demographics of Participants aged", startAge(), "to", endAge())
    }
    else {
      paste("Data on Participant Education Level and Average Hours Worked")
    }
    
  })
  
  output$mainPlot <- renderPlot({
    if (input$whichPlot == 2) {
      plotNation(input$mapData, input$ageRange)
    }
    else {
      plotNationEducation(input$educationLevel)
    }
  })

  
  
  output$gvis <- renderGvis({
    
    if (input$whichPlot == 1) {
      plotThis <- hs_incomplete
      education <- input$educationLevel1
      
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
      
      us <- map_data("state")
      
      arr <- USArrests %>% 
        tibble::rownames_to_column("region") %>% 
        mutate(region=tolower(region))
      
      
      
      test <-  merge(arr, averages, by="region")[c(1,7)]
      
      
      gvisGeoChart(test,
                   locationvar="region", colorvar="hoursWorked",
                   options=list(region="US", displayMode="regions",
                                resolution="provinces",
                                width=500, height=400,
                                colorAxis="{colors:['#FFFFFF', '#8E35EF']}"
                   ))      
    }
    else {
      participants <- plotNation(input$mapData, input$ageRange)
      gvisGeoChart(participants,
                   locationvar="region", colorvar="stCounts",
                   options=list(region="US", displayMode="regions",
                                resolution="provinces",
                                width=500, height=400,
                                colorAxis="{colors:['#FFFFFF', '#8E35EF']}"
                   ))  
      
    }


  })
  

  #shinyjs::onclick("mapData", print("hi"))

  
}

shinyApp(ui=ui, server=server)