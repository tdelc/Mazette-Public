library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(flexdashboard)
library(shinydashboardPlus)
library(ggridges)
library(ggstats)
# library(waffle)
library(plotly)
library(shinybusy)
library(DT)
library(lubridate)
# library(codeModules)

source("ui/header.R")
source("ui/controlbar.R")
source("ui/sidebar.R")
source("ui/body.R")

ui <- dashboardPage(
  header = header(),
  # controlbar = controlbar(),
  sidebar = sidebar(),
  body = body()
  # footer = dashboardFooter(
  #   left = "By Divad Nojnarg",
  #   right = textOutput("footer")
  # )
)