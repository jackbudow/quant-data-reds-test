library(tidyverse)
library(maps)
library(ggthemes)
library(mapproj)
library(stringr)
library(DataExplorer)
library(dplyr)
library(rgdal)
library(lubridate)
library(GGally)
library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Cincinnati Reds Pitcher Dashboard")

sidebar <- dashboardSidebar(
  selectInput("Pitcher", "Pitcher ID:", choices = "")
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML('
                    .content-wrapper {
                      background-color: white;
                    }
                    .box {
                      box-shadow: rgb(0 0 0 / 35%) 0px 5px 15px;
                    }
                    '))
    ),
  fluidRow(
    column(width = 1,
      valueBoxOutput("kBox", width = NULL),
      valueBoxOutput("bbBox", width = NULL),
      valueBoxOutput("hrBox", width = NULL)
    ),
    column(width = 1,
           valueBoxOutput("k9Box", width = NULL),
           valueBoxOutput("bb9Box", width = NULL),
           valueBoxOutput("hitBox", width = NULL)
    ),
    column(width = 1,
           valueBoxOutput("kpercBox", width = NULL),
           valueBoxOutput("bbpercBox", width = NULL),
           valueBoxOutput("ipBox", width = NULL)
    ),
    column(width = 1,
           valueBoxOutput("fipBox", width = NULL),
           valueBoxOutput("whipBox", width = NULL),
           valueBoxOutput("obaBox", width = NULL)
    ),
    column(width = 4,
           plotOutput("pitchLocationPlot", height = 600, width = NULL)
    ),
    column(width = 4,
           box(
            radioButtons("pitchType", "Pitch Type: ", choices = "")
           )
    )
  ),
  fluidRow(
    column(width = 4),
    column(width = 8,
          plotOutput("pitchPercPlot", height = 160, width = NULL)
    )
  )
)

ui <- dashboardPage(skin="red",
                    header, 
                    sidebar, 
                    body)