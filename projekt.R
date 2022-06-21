install.packages(c("ggplot2","tidyverse", "dplyr","scales", "gridExtra"))

library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(gridExtra)

data = read.csv("data.csv", header = T, sep = ",")

View(data)
head(data)


#                                             #
##  Basic visulatisation of fighter details  ## 
#                                             #


winPlot <- ggplot(data, aes(x = wins, y = fighter)) + geom_bar(stat="identity", fill="green") + labs(main = "Overview of fighter win count", x = "Win count", y = "Fighter")
lossPlot <-ggplot(data, aes(x = loss, y = fighter)) + geom_bar(stat="identity", fill="red") + theme_bw() + labs(main = "Overview of fighter loss count", x = "Loss count", y = "Fighter")

grid.arrange(winPlot, lossPlot)

par(mfrow=c(1,2))

hwinPlot <- hist(data$wins, main = "Maximum fighter win count", xlab="Win count", col = "steelblue", freq = F)
hlossPlot <- hist(data$loss, main = "Maximum fighter loss count", xlab="Loss count", col = "steelblue", freq = F)


data = mutate(data, win_percentage = (wins / total))
data = mutate(data, loss_rate = (loss / total))

winRatePlot <- ggplot(data, aes(x = win_percentage, y = fighter)) + geom_point() + labs(main = "Overview of fighter win rate", x = "Win rate", y = "Fighter")
lossRatePlot <- ggplot(data, aes(x = loss_rate, y = fighter)) + geom_point() + labs(main = "Overview of fighter loss rate", x = "Loss rate", y = "Fighter")

grid.arrange(winRatePlot, lossRatePlot)


#                                  #
##  Simulation of a single fight  ## 
#                                  #


# Shuffle the roster
shuffled_data <- data[sample(1:nrow(data)),]

# Simulation config
set.seed(100)
simSize <- 3

sim_fight = function(fighters, probabilities) {
  recFightSimRes = sample(fighters, probabilities, replace = TRUE, size = simSize)
  
  str_interp("Fight: ${fighters[1]} | ${fighters[2]}")
  
  occurences = as.data.frame(table(unlist(recFightSimRes)))[,2]
  
  odds_L = occurences[1] / simSize
  odds_R = occurences[2] / simSize
  
  str_interp("odds_L: ${odds_L}, odds_R: ${odds_R}")

  winner = names(which.max(table(recFightSimRes)))
  winnerProb = select(filter(data, fighter == winner), win_percentage)
  return (c(fighter = winner, winnerProb)) 
}


#                         #
## Tournament simulation ## 
#                         #


sim_tournament = function() {
  
  
  #                                #
  ## Initilalize winner pool df-s ## 
  #                                #
  
  
  WinnersOfFirstQuarter <- data.frame()
  WinnersOfSecondQuarter <- data.frame()
  WinnersOfSemiFinals <- data.frame()
  WinnersOfFinals <- data.frame()
  
  
  #                                     #
  ## Functions for appending into df-s ## 
  #                                     #
  
  
  addToFirstQuarterWinnerPool = function(winner, win_percentage) {
    df <- data.frame(fighter = winner, win_percentage = win_percentage)
    df2 <- rbind(WinnersOfFirstQuarter, df)
    return(df2)
  }
  
  addToSecondQuarterWinnerPool = function(winner, win_percentage) {
    df <- data.frame(fighter = winner, win_percentage = win_percentage)
    df2 <- rbind(WinnersOfSecondQuarter, df)
    return(df2)
  }
  
  addToSemiFinalWinnerPool = function(winner, win_percentage) {
    df <- data.frame(fighter = winner, win_percentage = win_percentage)
    df2 <- rbind(WinnersOfSemiFinals, df)
    return(df2)
  }
  
  addToFinalWinnerPool = function(winner, win_percentage) {
    df <- data.frame(fighter = winner, win_percentage = win_percentage)
    df2 <- rbind(WinnersOfFinals, df)
    return(df2)
  }
  
  
  #################
  # First Quarter #
  #################
  
  
  ## FIGHT #1
  winner = sim_fight(shuffled_data$fighter[c(1,2)], shuffled_data$win_percentage[c(1,2)])
  WinnersOfFirstQuarter = addToFirstQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  ## FIGHT #2
  winner = sim_fight(shuffled_data$fighter[c(3,4)], shuffled_data$win_percentage[c(3,4)])
  WinnersOfFirstQuarter = addToFirstQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  ## FIGHT #3
  winner = sim_fight(shuffled_data$fighter[c(5,6)], shuffled_data$win_percentage[c(5,6)])
  WinnersOfFirstQuarter = addToFirstQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  
  ## FIGHT #4
  winner = sim_fight(shuffled_data$fighter[c(7,8)], shuffled_data$win_percentage[c(7,8)])
  WinnersOfFirstQuarter = addToFirstQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  ## FIGHT #5
  winner = sim_fight(shuffled_data$fighter[c(9,10)], shuffled_data$win_percentage[c(9,10)])
  WinnersOfFirstQuarter = addToFirstQuarterWinnerPool(winner$fighter, winner$win_percentage)
 
  ## FIGHT #6
  winner = sim_fight(shuffled_data$fighter[c(11,12)], shuffled_data$win_percentage[c(11,12)])
  WinnersOfFirstQuarter = addToFirstQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  ## FIGHT #7
  winner = sim_fight(shuffled_data$fighter[c(13,14)], shuffled_data$win_percentage[c(13,14)])
  WinnersOfFirstQuarter = addToFirstQuarterWinnerPool(winner$fighter, winner$win_percentage)

  ## FIGHT #8
  winner = sim_fight(shuffled_data$fighter[c(15,16)], shuffled_data$win_percentage[c(15,16)])
  WinnersOfFirstQuarter = addToFirstQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  
  ##################
  # Second Quarter #
  ##################
  
  
  ## FIGHT #1
  winner = sim_fight(WinnersOfFirstQuarter$fighter[c(1,2)], WinnersOfFirstQuarter$win_percentage[c(1,2)])
  WinnersOfSecondQuarter = addToSecondQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  ## FIGHT #2
  winner = sim_fight(WinnersOfFirstQuarter$fighter[c(3,4)], WinnersOfFirstQuarter$win_percentage[c(3,4)])
  WinnersOfSecondQuarter = addToSecondQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  ## FIGHT #3
  winner = sim_fight(WinnersOfFirstQuarter$fighter[c(5,6)], WinnersOfFirstQuarter$win_percentage[c(5,6)])
  WinnersOfSecondQuarter = addToSecondQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  ## FIGHT #4
  winner = sim_fight(WinnersOfFirstQuarter$fighter[c(7,8)], WinnersOfFirstQuarter$win_percentage[c(7,8)])
  WinnersOfSecondQuarter = addToSecondQuarterWinnerPool(winner$fighter, winner$win_percentage)
  
  
  #################
  #  Semi Finals  #
  #################
  
  
  ## FIGHT #1
  winner = sim_fight(WinnersOfSecondQuarter$fighter[c(1,2)], WinnersOfSecondQuarter$win_percentage[c(1,2)])
  WinnersOfSemiFinals = addToSemiFinalWinnerPool(winner$fighter, winner$win_percentage)
  
  ## FIGHT #2
  winner = sim_fight(WinnersOfSecondQuarter$fighter[c(3,4)], WinnersOfSecondQuarter$win_percentage[c(3,4)])
  WinnersOfSemiFinals = addToSemiFinalWinnerPool(winner$fighter, winner$win_percentage)
  
  
  ############
  #  Finals  #
  ############
  
  
  ## FIGHT #1
  winner = sim_fight(WinnersOfSemiFinals$fighter[c(1,2)], WinnersOfSemiFinals$win_percentage[c(1,2)])
  WinnersOfFinals = addToFinalWinnerPool(winner$fighter, winner$win_percentage)
  
  return (WinnersOfFinals[1]$fighter[1])
}


#                                 #
## Post simulation visualisation ## 
#                                 #


tournamentSimulationResults <- rerun(1000, sim_tournament())
tourDf <- as.data.frame(table(unlist(tournamentSimulationResults)))

colnames(tourDf) <- c("fighter", "win_count")

par(mfrow=c(1,1))

ggplot(tourDf, aes(x = fighter, y = win_count, fill=win_count)) + geom_bar(stat="identity")  + theme_bw() + labs(main = "Simulated win count by fighetr", x = "Fighter", y = "Win count")
hist(tourDf$win_count, main="Distribution of wins", freq = F, xlab = "Win count")
lines(density(tourDf$win_count), lwd = 2, col = "red")

