
library(shiny)
load("BBWorkspace.RData")
#source("Baseball.R")

pageWithSidebar(
  headerPanel("2018 MLB Postseason wOBA"),
  sidebarPanel(
    selectInput('player', 'Player Name', c('All',levels(our_data$player_name))),
    selectInput('opp_team', 'Opposing Team', c('All',levels(our_data$opp_team))),
    selectInput('pitch_type', 'Pitch Type', c('All',levels(our_data$pitch_type)))
  ),
  mainPanel(
    plotOutput("plot1")
  )
)


































