#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(ggplot2)
library(rsconnect)
library(gdata)

# Define UI for application that draws a histogram
ui <- fluidPage(
    selectInput("variable1", "X-axis:",c("july population" = "july11pop", 
                                        "region of country" = "region",
                                        "change in population" = "popChange",
                                        "Percent change in population" = "percentChange")),
    selectInput("variable2", "Y-axis:",c("july population" = "july11pop", 
                                        "region of country" = "region",
                                        "change in population" = "popChange",
                                        "Percent change in population" = "percentChange")),
    selectInput("variable3", "Size:",c("july population" = "july11pop", 
                                        "region of country" = "region",
                                        "change in population" = "popChange",
                                        "Percent change in population" = "percentChange")),
        # Show a plot of the generated distribution
           plotOutput("plot")
        )
Numberize <- function(inputVector)
{
    # Get rid of commas
    inputVector<-str_replace_all(inputVector,",","")
    # Get rid of spaces
    inputVector<-str_replace_all(inputVector," ","")
    
    return(as.numeric(inputVector))
}
readCensus <- function(){
    testFrame <- read.xls("~/nst-est2011-01.xls")
    testFrame <- testFrame[-1:-8,]
    testFrame <- testFrame[,1:5]
    testFrame$stateName <- testFrame [,1]
    testFrame <- testFrame[,-1]
    testFrame <- testFrame[-52:-58,]
    
    testFrame$stateName <- gsub("\\.","",testFrame$stateName)
    
    testFrame$aprill10census <- Numberize(testFrame$X)
    testFrame$aprill10base <- Numberize(testFrame$X.1)
    testFrame$july10pop <- Numberize(testFrame$X.2)
    testFrame$july11pop <- Numberize(testFrame$X.3)
    testFrame <- testFrame[,-1:-4]
    
    rownames(testFrame) <- NULL
    
    return(testFrame)
}
# Define server logic required to draw a histogram
server <- function(input, output) {
    dfStates <- readCensus()
    dfStates <- dfStates[dfStates$stateName != "District of Columbia",]
    
    dfStates$region <- state.region
    dfStates$stateName <-
        tolower(dfStates$stateName)
    
    dfStates$popChange <- dfStates$july11pop - dfStates$july10pop
    
    dfStates$percentChange <- dfStates$popChange/dfStates$july10pop * 100
    
    output$plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(dfStates, aes(x = dfStates[,input$variable1], y = dfStates[,input$variable2])) + geom_point(aes(size = dfStates[,input$variable3], color = july11pop))

        })
}

# Run the application 
shinyApp(ui = ui, server = server)

