#install.packages('rsconnect')
#install.packages(c('ggplot2', 'shiny'))

library(rsconnect)
library(ggplot2)
library(shiny)

ui <- fluidPage()

server <- function(input,output) {
  
}

shinyApp(ui=ui, server=server)