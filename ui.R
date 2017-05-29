library(shiny)
library(dygraphs)
library(zoo)
library(shinycssloaders)

# read the csv files
fvv_header <- read.csv("data/Observed_Velocity.csv")
rain_header <- read.csv("data/RainGuage.csv")

# the UI
shinyUI(
  fluidPage(
    fluidRow(
      column(2,
             selectInput("flowmeters", label = h5("Flow Meter :", style="font-weight: bold; color:orange"),
                         choices = colnames(fvv_header)[-1], selected = colnames(fvv_header)[1], width = "150px"),
             offset = 0, style = "padding: 10px 20px"
        
      ),
      column(2,
             selectInput("rainguage", label = h5("Rain Guage :", style="font-weight: bold; color: orange"),
                         choices = colnames(rain_header)[-1], selected = colnames(rain_header)[1], width = "120px"),
             offset = 0, style = "padding: 10px 0px"
      )
    ),
    fluidRow(
      column(12,
        fluidRow(
          column(11,
                 withSpinner(dygraphOutput("rainintensity", height = "200px"))),
          column(1,
                 htmlOutput("rainfallstats"),
                 style='padding:0px')
        ),
        fluidRow(
          column(11,
                 withSpinner(dygraphOutput("depth", height = "200px"))),
          column(1,
                 htmlOutput("depthstats"),
                 style='padding:0px')
        ),
        fluidRow(
          column(11,
                 withSpinner(dygraphOutput("volume", height = "200px"))),
          column(1,
                 htmlOutput("volumestats"),
                 style='padding:0px')
        ),
        fluidRow(
          column(11,
                 withSpinner(dygraphOutput("velocity", height = "200px"))),
          column(1,
                 htmlOutput("velocitystats"),
                 style='padding:0px')
        )
      )
    )
  )
)