library(tidyverse)
library(RColorBrewer)
load("BBWorkspace.RData")
#source("Baseball.R")

function(input, output, session) {
  
  output$plot1 <- renderPlot({
    
    
    ##### Removes NA's from data
    e <- is.na(our_data$events)
    our_data <- subset(our_data, e == F)
    
    
    ##### Filters the data the app will look through to produce plots based on user's input
    if (input$player != 'All') {
      our_data <- subset(our_data, player_name == input$player)
    }
    if (input$opp_team != 'All') {
      our_data <- subset(our_data, opp_team == input$opp_team)      
      if (length(our_data$player_name) == 0) {
          stop("The combination you have selected had 0 occurances during the 2018 MLB Playoffs.
                Please select a different combination.")
      }
    }
    if (input$pitch_type != 'All') {
      our_data <- subset(our_data, pitch_type == input$pitch_type)
      if (length(our_data$player_name) == 0) {
          stop("The combination you have selected had 0 occurances during the 2018 MLB Playoffs.
                Please select a different combination.")
      }
    }
    
        
    ##### Creates current team and opposing team variables
    for (i in 1:length(our_data$pitch_type)) {
      if (our_data$inning_topbot[i] == "Bot") {
        our_data$team[i] <- our_data$home_team[i]
        our_data$opp_team[i] <- our_data$away_team[i]
      } else {
        our_data$team[i] <- our_data$away_team[i]
        our_data$opp_team[i] <- our_data$home_team[i]
      }
    }
    
    
    ##### Renames the pitchtypes to be more informative  
    our_data$pitch_type[(our_data$pitch_type) == "CH"] <- "Changeup"
    our_data$pitch_type[(our_data$pitch_type) == "CU"] <- "Curveball"
    our_data$pitch_type[(our_data$pitch_type) == "FC"] <- "Cutter"
    our_data$pitch_type[(our_data$pitch_type) == "FF"] <- "4-Seam Fastball"
    our_data$pitch_type[(our_data$pitch_type) == "FS"] <- "Splitter"
    our_data$pitch_type[(our_data$pitch_type) == "FT"] <- "Two-Seam Fastball"
    our_data$pitch_type[(our_data$pitch_type) == "KC"] <- "Knuckle Curveball"
    our_data$pitch_type[(our_data$pitch_type) == "SI"] <- "Sinker"
    our_data$pitch_type[(our_data$pitch_type) == "SL"] <- "Slider"
    
    
    ##### Changes these variables to factors in our data
    our_data$player_name <- as.factor(our_data$player_name)
    our_data$opp_team <- as.factor(our_data$opp_team)
    our_data$team <- as.factor(our_data$team)
    our_data$pitch_type <- as.factor(our_data$pitch_type)
    
    
    ##### Creates our own 'x-axis' bins or scale 
    x_zoning <- function(x) {
      if (is.na(x)){
        return (0)
      } else if (x <= -0.708){
        return(1)
      } else if (x > -0.708 & x <= -0.2357) {
        return(2)
      } else if (x > -0.2357 & x <= 0.2357) {
        return(3)
      } else if (x > .2357 & x <= .708) {
        return(4) 
      } else if (x > .708) {
        return(5)
      } else {
        return(0)
      }
    }
   
    
    ##### Creates our own 'y-axix' bins or scale but uses what baseball metrics calls 'z'
    z_zoning <- function(z) {
      if (is.na(z)) {
        return (0)
      } else if (z <= 1.5) {
        return(1)
      } else if (z > 1.5 & z <= 2.167) {
        return(2)
      } else if (z > 2.167 & z <= 2.8337) {
        return(3)
      } else if (z > 2.8337 & z <= 3.5) {
        return(4) 
      } else if (z > 3.5) {
        return(5)
      } else {
        return(0)
      }
    }
  
    
    ##### Assigns every pitch our x-zone and z-zone  
    for (i in 1:length(our_data$pitch_type)){
      our_data$x_zone[i] <- x_zoning(our_data$plate_x[i])
      our_data$z_zone[i] <- z_zoning(our_data$plate_z[i])
    }
    
    
    ##### Creates x and y axis pairs/zones for us to use later in our graph 
    for (i in 1:length(our_data$pitch_type)){
      if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 1){
        our_data$doub_zone[i] = 11
      } else if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 2){
        our_data$doub_zone[i] = 12
      } else if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 3){
        our_data$doub_zone[i] = 13
      } else if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 4){
        our_data$doub_zone[i] = 14
      } else if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 5){
        our_data$doub_zone[i] = 15
      } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 1){
        our_data$doub_zone[i] = 21
      } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 2){
        our_data$doub_zone[i] = 22
      } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 3){
        our_data$doub_zone[i] = 23
      } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 4){
        our_data$doub_zone[i] = 24
      } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 5){
        our_data$doub_zone[i] = 25
      } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 1){
        our_data$doub_zone[i] = 31
      } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 2){
        our_data$doub_zone[i] = 32
      } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 3){
        our_data$doub_zone[i] = 33
      } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 4){
        our_data$doub_zone[i] = 34
      } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 5){
        our_data$doub_zone[i] = 35
      } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 1){
        our_data$doub_zone[i] = 41
      } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 2){
        our_data$doub_zone[i] = 42
      } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 3){
        our_data$doub_zone[i] = 43
      } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 4){
        our_data$doub_zone[i] = 44
      } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 5){
        our_data$doub_zone[i] = 45
      } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 1){
        our_data$doub_zone[i] = 51
      } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 2){
        our_data$doub_zone[i] = 52
      } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 3){
        our_data$doub_zone[i] = 53
      } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 4){
        our_data$doub_zone[i] = 54
      } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 5){
        our_data$doub_zone[i] = 55
      } else {
        our_data$doub_zone[i] = 99
      }
    }
    
    
    ##### Calculates the mean wOBA value based on the zone of the pitch from before
    bin_woba_value <- our_data %>%
      group_by(doub_zone) %>%
      summarise(bin_woba_value = mean(woba_value))
    wobas <- bin_woba_value

    
    ##### Creates the final wOBA variable in the data to be graphed
    for (i in 1:length(our_data$doub_zone)) { 
      for (j in 1:length(bin_woba_value$doub_zone)) {
        if (our_data$doub_zone[i] == bin_woba_value$doub_zone[j]) {
          our_data$bin_woba[i] <- bin_woba_value$bin_woba_value[j]
        }
      }
    }
    
    
    ##### Creates the graph of the wOBA values using ggplot and r-ColorBrewer
    mycolors <- brewer.pal(3, "Reds")
    ggplot(data = our_data,
           aes(x = x_zone,
               y = z_zone,
               fill = bin_woba)) +
      geom_tile() +
      geom_rect(aes(xmin = 1.5, ymin = 1.5, xmax = 4.5, ymax = 4.5), color = 'black', alpha = 0, size = 2) +
      scale_fill_gradient(low = mycolors[1], high = mycolors[3], name = "wOBA") +
      xlim(0.5,5.5) +
      ylim(0.5,5.5) +
      xlab("Catcher's Perspective") +
      ylab("") +
      ggtitle("wOBA by Pitch Location")
    }) 

  }
  
