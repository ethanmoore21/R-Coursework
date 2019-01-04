#heat map - type of pitch, team, pitchers handiness, pitch location 
big_data <- read.csv("/Users/ajaypatel21/Desktop/Shiny App/savant_data.csv", na.strings = "null", stringsAsFactors = F)
our_data <- subset(big_data, select = c(pitch_type, release_speed, player_name, events, plate_x, plate_z, home_team, away_team, 
                                           woba_value, woba_denom, if_fielding_alignment, inning_topbot))
library(tidyverse)




e <- is.na(our_data$events)
our_data <- subset(our_data, e == F)

for (i in 1:length(our_data$pitch_type)) {
  if (our_data$inning_topbot[i] == "Bot") {
    our_data$team[i] <- our_data$home_team[i]
    our_data$opp_team[i] <- our_data$away_team[i]
  } else {
    our_data$team[i] <- our_data$away_team[i]
    our_data$opp_team[i] <- our_data$home_team[i]
  }
}

our_data$pitch_type[(our_data$pitch_type) == "CH"] <- "Changeup"
our_data$pitch_type[(our_data$pitch_type) == "CU"] <- "Curveball"
our_data$pitch_type[(our_data$pitch_type) == "FC"] <- "Cutter"
our_data$pitch_type[(our_data$pitch_type) == "FF"] <- "4-Seam Fastball"
our_data$pitch_type[(our_data$pitch_type) == "FS"] <- "Splitter"
our_data$pitch_type[(our_data$pitch_type) == "FT"] <- "Two-Seam Fastball"
our_data$pitch_type[(our_data$pitch_type) == "KC"] <- "Knuckle Curveball"
our_data$pitch_type[(our_data$pitch_type) == "SI"] <- "Sinker"
our_data$pitch_type[(our_data$pitch_type) == "SL"] <- "Slider"

our_data$player_name <- as.factor(our_data$player_name)
our_data$opp_team <- as.factor(our_data$opp_team)
our_data$team <- as.factor(our_data$team)
our_data$pitch_type <- as.factor(our_data$pitch_type)

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

# our_data <- subset(our_data,
#                     player_name == 'Aaron Judge' &
#                     opp_team == 'BOS' &
#                     pitch_type == '4-Seam Fastball')

for (i in 1:length(our_data$pitch_type)){
  our_data$x_zone[i] <- x_zoning(our_data$plate_x[i])
  our_data$z_zone[i] <- z_zoning(our_data$plate_z[i])
}

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

our_data$doub_zone <- as.factor(our_data$doub_zone)

bin_woba_value <- our_data %>%
    group_by(doub_zone) %>%
    summarise(bin_woba_value = mean(woba_value))
wobas <- bin_woba_value

for (i in 1:length(our_data$doub_zone)) { 
  for (j in 1:length(bin_woba_value$doub_zone)) {
    if (our_data$doub_zone[i] == bin_woba_value$doub_zone[j]) {
      our_data$bin_woba[i] <- bin_woba_value$bin_woba_value[j]
    }
  }
}



# for (i in 1:length(our_data$pitch_type)){
#   if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 1){
#     our_data$bin_woba[i] = wobas$bin_woba_value[1]
#   } else if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 2){
#     our_data$bin_woba[i] = wobas$bin_woba_value[2]
#   } else if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 3){
#     our_data$bin_woba[i] = wobas$bin_woba_value[3]
#   } else if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 4){
#     our_data$bin_woba[i] = wobas$bin_woba_value[4]
#   } else if (our_data$x_zone[i] == 1 & our_data$z_zone[i] == 5){
#     our_data$bin_woba[i] = wobas$bin_woba_value[5]
#   } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 1){
#     our_data$bin_woba[i] = wobas$bin_woba_value[6]
#   } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 2){
#     our_data$bin_woba[i] = wobas$bin_woba_value[7]
#   } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 3){
#     our_data$bin_woba[i] = wobas$bin_woba_value[8]
#   } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 4){
#     our_data$bin_woba[i] = wobas$bin_woba_value[9]
#   } else if (our_data$x_zone[i] == 2 & our_data$z_zone[i] == 5){
#     our_data$bin_woba[i] = wobas$bin_woba_value[10]
#   } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 1){
#     our_data$bin_woba[i] = wobas$bin_woba_value[11]
#   } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 2){
#     our_data$bin_woba[i] = wobas$bin_woba_value[12]
#   } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 3){
#     our_data$bin_woba[i] = wobas$bin_woba_value[13]
#   } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 4){
#     our_data$bin_woba[i] = wobas$bin_woba_value[14]
#   } else if (our_data$x_zone[i] == 3 & our_data$z_zone[i] == 5){
#     our_data$bin_woba[i] = wobas$bin_woba_value[15]
#   } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 1){
#     our_data$bin_woba[i] = wobas$bin_woba_value[16]
#   } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 2){
#     our_data$bin_woba[i] = wobas$bin_woba_value[17]
#   } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 3){
#     our_data$bin_woba[i] = wobas$bin_woba_value[18]
#   } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 4){
#     our_data$bin_woba[i] = wobas$bin_woba_value[19]
#   } else if (our_data$x_zone[i] == 4 & our_data$z_zone[i] == 5){
#     our_data$bin_woba[i] = wobas$bin_woba_value[20]
#   } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 1){
#     our_data$bin_woba[i] = wobas$bin_woba_value[21]
#   } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 2){
#     our_data$bin_woba[i] = wobas$bin_woba_value[22]
#   } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 3){
#     our_data$bin_woba[i] = wobas$bin_woba_value[23]
#   } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 4){
#     our_data$bin_woba[i] = wobas$bin_woba_value[24]
#   } else if (our_data$x_zone[i] == 5 & our_data$z_zone[i] == 5){
#     our_data$bin_woba[i] = wobas$bin_woba_value[25]
#   } 
# }


ggplot(data = our_data,
       aes(x = x_zone,
           y = z_zone,
           fill = bin_woba)) +
  geom_tile() +
  scale_fill_gradient(low = 'white', high = 'red')


