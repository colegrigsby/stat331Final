#install.packages('rsconnect')
#install.packages(c('ggplot2', 'shiny'))
#install.packages("googleVis")
#install.packages("devtools")
#install.packages("shinyjs")
library(devtools)
library(dplyr)
library(rsconnect)
library(ggplot2)
library(shiny)
library(googleVis)
library(maps)
library(rvest)
library(shinyjs)

#enable the source files when running to have all data and methods available to app :)
source("ReadingData.R")
source("MappingStuff.R")

ui <- fluidPage(
  useShinyjs(),
  tabsetPanel(
    tabPanel(
      "National Map",
      fluidRow(column(8, h1(
        textOutput("header")
      ), offset = 2)),
      
      fluidRow(column(8, htmlOutput("gvis"), offset =
                        3)),
      fluidRow(column(
        5,
        radioButtons(
          "whichPlot",
          label = "Change Plot",
          choices = c("Participant Data" = 2, "Education Data" = 1),
          selected = 2,
          inline = TRUE
        ),
        offset = 4
      )),
      
      fluidRow(
        column(
          4,
          h5("Participant Demographics Plot"),
          sliderInput(
            "ageRange",
            "Age",
            min(user.data$PRTAGE),
            max(user.data$PRTAGE),
            c(min(user.data$PRTAGE), max(user.data$PRTAGE))
          ),
          selectInput(
            "mapData",
            "Select what you would like to look at",
            choices = c(
              "Percentage that hold a bachelors",
              "General Number of Participants",
              "Average Age of Participants",
              "Percent of State Population Participating"
            ),
            selected = "General Number of Participants"
          )
          ,
          offset = 1,
          style = "background-color:#D1D0CE;"
        ),
        
        column(
          4,
          h5("Education Plot Options"),
          radioButtons(
            "educationLevel1",
            label = "Highest Level of Education Obtained",
            choices = c(
              "High School Incomplete",
              "High School Diploma",
              "Some College",
              "Bachelor's/Associate's",
              "Masters and Above"
            ),
            selected = "Bachelor's/Associate's"
          ),
          offset = 2,
          style = "background-color:#D1D0CE;"
        )
      )
      #TODO add another sidebar panel or something with some quick facts
      # max, min, mean etc....
      # ACTIVITIES PER STATE WOULD BE DOPE
      
      
      
      
    ),
    tabPanel(
      "Clustering",
      h1("Clustering"),
      fluidRow(plotOutput("clusters")),
      sidebarPanel(
        radioButtons(
          "chooseX",
          label = "Choose an X variable",
          choices = c(
            "Work",
            "Education",
            "Social",
            "Sports",
            "Age",
            "WeeklyEarnings",
            "Religious",
            "Volunteer"
          ),
          selected = "WeeklyEarnings"
        )
      ),
      sidebarPanel(
        radioButtons(
          "chooseY",
          label = "Choose a Y variable",
          choices = c(
            "Work",
            "Education",
            "Social",
            "Sports",
            "Age",
            "WeeklyEarnings",
            "Religious",
            "Volunteer"
          ),
          selected = "Age"
        )
      )
    ,
    sidebarPanel(
      radioButtons(
        "chooseN",
        label = "Choose Number of Clusters",
        choices = c(2, 3, 4, 5),
        selected = 2
      )
    )
    
    
  ),
  tabPanel(
    "Regression",
    h1("Regression"),
    fluidRow(plotOutput("plotter")),
    sidebarPanel(
      radioButtons(
        "chooseX",
        label = "Choose an X variable",
        choices = c(
          "Work",
          "Education",
          "Social",
          "Sports",
          "Age",
          "WeeklyEarnings",
          "Religious",
          "Volunteer"
        ),
        selected = "Education"
      )),
    sidebarPanel(
      radioButtons(
        "chooseY",
        label = "Choose a Y variable",
        choices = c(
          "Work",
          "Education",
          "Social",
          "Sports",
          "Age",
          "WeeklyEarnings",
          "Religious",
          "Volunteer"
        ),
        selected = "Age"
      )),
    sidebarPanel(
      radioButtons(
        "chooseCat",
        label = "Choose a categorical variable",
        choices = c("N/A", "Year", "Education Level", "Day", "Race"),
        selected = "N/A"
      )
    )
  )
    

  
  
  
  
))



server <- function(input, output) {
  #disabling buttons for mapping
  enable("mapData")
  enable("ageRange")
  disable("educationLevel1")
  observeEvent(input$whichPlot, {
    #"Participant Data" = 2, "Education Data" = 1
    if (input$whichPlot == 1) {
      disable("mapData")
      disable("ageRange")
      enable("educationLevel1")
    } else {
      enable("mapData")
      enable("ageRange")
      disable("educationLevel1")
    }
  })
  
  colors <- c("darkblue", "darkgreen", "blueviolet")
  
  
  output$clusters <- renderPlot({
    clustering(
      n = input$chooseN,
      df = summary[1:10000,],
      c(input$chooseX, input$chooseY),
      c(input$chooseX, input$chooseY)
    )
  })
  
  output$plotter <- renderPlot({
    message("inthisbitch")
    
    #ggplot(summary[1:100,])+geom_point(aes_string(x=input$chooseX,y=input$chooseY),alpha=.2)
    ggplot(summary) + geom_point(aes_string(x = input$chooseX, y = input$chooseY), alpha =
                                   .2)
    #plotter(summary[1:100,input$chooseX],summary[1:100,input$chooseY], dframe=summary[1:100,])
    message("out")
  })
  
  
  startAge <- reactive({
    input$ageRange[1]
  })
  endAge <- reactive({
    input$ageRange[2]
  })
  output$header <- renderText({
    if (input$whichPlot == 2) {
      paste("Demographics of Participants aged",
            startAge(),
            "to",
            endAge())
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
      # plotThis <- hs_incomplete
      # education <- input$educationLevel1
      #
      # if (education == "High School Incomplete") {
      #   plotThis <- hs_incomplete
      # }
      # if (education == "High School Diploma") {
      #   plotThis <- hs_complete
      # }
      # if (education == "Some College") {
      #   plotThis <- some_college
      # }
      # if (education == "Bachelor's/Associate's") {
      #   plotThis <- ba_complete
      # }
      # if (education == "Masters and Above") {
      #   plotThis <- masters_above
      # }
      # averages <- aggregate(hoursWorked ~ state, plotThis, mean)
      # averages <- averages[-9,]
      # averages$region <- arr$region
      #
      # us <- map_data("state")
      #
      # arr <- USArrests %>%
      #   tibble::rownames_to_column("region") %>%
      #   mutate(region=tolower(region))
      #
      # test <-  merge(arr, averages, by="region")[c(1,7)]
      
      
      test <- plotNationEducation(input$educationLevel1)
      
      #print(head(test))
      gvisGeoChart(
        test,
        locationvar = "region",
        colorvar = "Hours Worked",
        options = list(
          region = "US",
          displayMode = "regions",
          resolution = "provinces",
          width = 500,
          height = 400,
          colorAxis = "{colors:['#FFFFFF', '#8E35EF']}"
        )
      )
    }
    else {
      participants <- plotNation(input$mapData, input$ageRange)
      print(participants)
      gvisGeoChart(
        participants[[1]],
        locationvar = "region",
        colorvar = participants[[2]],
        options = list(
          region = "US",
          displayMode = "regions",
          resolution = "provinces",
          width = 500,
          height = 400,
          colorAxis = "{colors:['#FFFFFF', '#8E35EF']}"
        )
      )
      
    }
    
    
  })
  
  
  #shinyjs::onclick("mapData", print("hi"))
  
  
}

shinyApp(ui = ui, server = server)