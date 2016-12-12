#install packages

#install.packages('rsconnect')
#install.packages(c('ggplot2', 'shiny'))
#install.packages("googleVis")
#install.packages("devtools")
#install.packages("shinyjs")

#load the libraries required for the application to run

library(devtools)
library(dplyr)
library(rsconnect)
library(ggplot2)
library(shiny)
library(googleVis)
library(maps)
library(rvest)
library(shinyjs)
library(ggplot2)


#enable the source files when running to have all data and methods available to app :)
source("ReadingData.R")
source("MappingStuff.R")

#ui for the shiny app
ui <- fluidPage(#use JS for the enable and disable to work
  useShinyjs(),
  tabsetPanel(
    #map tab
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
      
    ),
    
    #clustering panel
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
    
    #regression panel
    tabPanel(
      "Regression",
      h1("Regression"),
      fluidRow(plotOutput("plotter")),
      fluidRow(
        sidebarPanel(
          radioButtons(
            "choosePred",
            label = "Choose a Predictor variable",
            choices = c(
              "Work",
              "Education",
              "Social",
              "Sports",
              "Age",
              "WeeklyEarnings",
              "Religious",
              "Volunteer",
              "Year",
              "EducationLevel",
              "Day",
              "Race"
            ),
            selected = "Education"
          )
        ),
        sidebarPanel(
          radioButtons(
            "chooseResp",
            label = "Choose a Response variable",
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
        ),
        
        
        sidebarPanel(
          textInput(
            "nums",
            label = h3("Numeric Input For Prediction"),
            value = "Enter positive number..."
          ),
          verbatimTextOutput("value")
        )
      ),
      fluidRow(verbatimTextOutput("modelInfo")),
      fluidRow(verbatimTextOutput("modelCorr"))
    )
  ))


#server function for shiny app to run, this handles input and output
server <- function(input, output) {
  #disabling buttons for mapping initialization
  enable("mapData")
  enable("ageRange")
  disable("educationLevel1")
  
  #observing the plot type for mapping and enabling/disabling certain fields
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
  
  
  
  #initialize color vector
  colors <- c("darkblue", "darkgreen", "blueviolet")
  
  
  
  #header for map output
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
  
  
  #mapping output for the nation plots on tab 1
  #uses gvis for an interactive map
  output$gvis <- renderGvis({
    if (input$whichPlot == 1) {
      test <- plotNationEducation(input$educationLevel1)
      
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
  
  #clustering plot output calls the clustering method with the inputs provided by user
  output$clusters <- renderPlot({
    clustering(
      n = input$chooseN,
      df = summary[1:60000,],
      c(input$chooseX, input$chooseY),
      c(input$chooseX, input$chooseY)
    )
  })
  
  
  #plotting the plot based on user input for regression
  #this also sets some text output variables that are displayed to the user
  output$plotter <- renderPlot({
    #handle labels
    x.title <- axis.labeler(input$choosePred)
    y.title <- axis.labeler(input$chooseResp)
    
    if ((input$choosePred == "EducationLevel") ||
        (input$choosePred == "Year") || (input$choosePred == "Day")
        || (input$choosePred == "Race")) {
      means <-
        split(summary[, input$chooseResp], summary[, input$choosePred])
      barplot(
        sapply(means, mean),
        col = "lightblue",
        main = paste("Mean", input$chooseResp, "for", input$choosePred),
        xlab = x.title,
        ylab = y.title
      )
      
    } else{
      plot(
        summary[, input$choosePred],
        summary[, input$chooseResp],
        main = paste(input$choosePred, "vs.", input$chooseResp),
        col = rgb(0, 100, 0, 30, maxColorValue = 255),
        pch = 16,
        xlab = x.title,
        ylab = y.title
      )
      model <-
        lm(summary[, input$chooseResp] ~ summary[, input$choosePred])
      
      
      output$modelInfo <- renderPrint({
        summary(model)$coef
      })
      output$modelCorr <- renderPrint({
        anova(model)
      })
      abline(model, col = "red") # regression line (y~x)
    }#plotter(summary[1:100,input$chooseX],summary[1:100,input$chooseY], dframe=summary[1:100,])
  })
  
  
  #refactoring input ages into their own variables
  startAge <- reactive({
    input$ageRange[1]
  })
  endAge <- reactive({
    input$ageRange[2]
  })
  
  ?as.numeric
  #regression predictor output
  output$value <- renderText({
    if (length(input$nums) > 0 && !is.na(as.numeric(input$nums))) {
      paste(
        "If the value of",
        input$choosePred,
        "is",
        input$nums,
        "then then data suggests that the value of",
        input$chooseResp,
        "will be",
        round(
          lm(summary[, input$chooseResp] ~ summary[, input$choosePred])[[1]][[1]] +
            lm(summary[, input$chooseResp] ~ summary[, input$choosePred])[[1]][[2]] *
            as.numeric(input$nums),
          3
        ),
        input$chosePred
      )
    } else {
      return("Please enter a positive numeric value")
    }
  })
  
}

#run the shiny App!!
shinyApp(ui = ui, server = server)
