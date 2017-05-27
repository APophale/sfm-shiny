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
             selectInput("flowmeters", label = h5("Flow Meter", style="font-weight: bold"),
                         choices = colnames(fvv_header)[-1], selected = colnames(fvv_header)[1]),
             offset = 0, style = 'padding:10px;'),
      column(1,
             selectInput("rainguage", label = h5("Rain Guage", style="font-weight: bold"),
                         choices = colnames(rain_header)[-1], selected = colnames(rain_header)[1]),
             offset = 0, style = 'padding:10px;')
    ),
    mainPanel(
      withSpinner(dygraphOutput("rainintensity", height = "200px")),
      withSpinner(dygraphOutput("depth", height = "200px")),
      withSpinner(dygraphOutput("volume", height = "200px")),
      withSpinner(dygraphOutput("velocity", height = "200px")),
      width = 12
    )
  )
)