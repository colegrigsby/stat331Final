# ------------------------------------------------
# Found this too. its pretty cool and would work well.
# I really have no clue what any of this code is doing though
#
# http://www.showmeshiny.com/wp-content/uploads/2014/01/Italian-Population.png
# Couldnt find the app, but this is what it would look like.
# ------------------------------------------------




# Load libraries
library(pyramid)

# Set working directory (to run code within RStudio)
#setwd("ShinyApps/pyramid")

# Load data
load("data.Rdata")

# Define useful functions
roundAny = function(x, accuracy, f = round) {f(x/accuracy) * accuracy}



### Main shinyServer function
shinyServer(function(input, output) {
  
  ### Plot output: pyramid
  output$pyramid <- renderPlot({
    # Define data column from slider input
    col = input$anno - 2002 + 1
    
    # Class widths from slider input
    agg = rep(1:1000, length.out = nrow(pop$MALES), each = input$ampiezza)
    
    # Build data frame with current year and class widths 
    df = data.frame(
      M = tapply(pop$MALES[, col], INDEX = agg, FUN = sum),
      F = tapply(pop$FEMALES[, col], INDEX = agg, FUN = sum)
    )
    
    # Given the class width, data frame with highest value
    dfMax = data.frame(
      M = tapply(pop$MALES[, ncol(pop$MALES)], INDEX = agg, FUN = sum),
      F = tapply(pop$FEMALES[, ncol(pop$FEMALES)], INDEX = agg, FUN = sum)
    )
    
    # Age classes labels
    lab = seq(from = 0, to = 1000, by = input$ampiezza)[1:nrow(df)]
    
    if(input$ampiezza == 1) {
      row.names(df) = lab
    } else {
      row.names(df) = paste(lab, lab+input$ampiezza-1, sep = " - ")
    }
    row.names(df)[nrow(df)] = paste0(lab[nrow(df)], "+")
    
    # Graphical parameter (adjusted for class width)
    val.Cadj = -0.01
    val.Cstep = 1
    if(input$ampiezza == 1) {val.Cadj = -0.030; val.Cstep = 3}
    if(input$ampiezza == 2) {val.Cadj = -0.025; val.Cstep = 2}
    if(input$ampiezza == 3) {val.Cadj = -0.020}
    if(input$ampiezza == 4) {val.Cadj = -0.015}
    
    # Data scale
    ord = nchar(max(dfMax)) - 1
    df = df/(10^ord)
    if(ord == 6) {val.Laxis = seq(0, roundAny(max(dfMax)/(10^ord), 0.5, ceiling), by = 0.5); val.main = "(millions)"}
    if(ord == 5) {val.Laxis = seq(0, roundAny(max(dfMax)/(10^ord), 1, ceiling), by = 1); val.main = "(thousands x 100)"}
    
    # Finally, draw the plot
    pyramid(df, Cstep = val.Cstep, Cadj = val.Cadj, AxisFM = "g", Laxis = val.Laxis, main = paste("Population", val.main))
    
  })
})






require(shiny)

shinyUI(pageWithSidebar(
  
  ###  Application title
  headerPanel("Italian population between 2002 and 2011"),
  
  ### Sidebar with sliders and HTML
  sidebarPanel(
    # Slider: choose class width
    sliderInput("ampiezza", "Class width (years):", min=1, max=10, value=5),
    # Slider: choose year
    sliderInput("anno", "Year:", min=2002, max=2011, value=2011, format = "0000", animate = TRUE), 
    # HTML info
    div(style = "margin-top: 30px; width: 200px; ", HTML("Developed by")),
    div(style = "margin-top: 10px; ", HTML("<a href='http://www.quantide.com'><img style='width: 150px;' src='http://www.quantide.com/images/quantide.png' /></a><img style='float: right; width: 150px;' src='http://www.nicolasturaro.name/logoWeb300.png' />")),
    div(style = "margin-top: 30px;", HTML("Source: <a href='http://demo.istat.it/'>ISTAT - Istituto Nazionale di Statistica</a>"))
  ),
  
  ### Main Panel
  mainPanel(
    # Show the plot
    plotOutput("pyramid", height="600px")
  )
))