#Entire Import
AFL_Stats_DF = read.csv("https://raw.githubusercontent.com/hinestein/AFL_Work/main/Datasets/AFL_Stats_DF")

library(tidyr)
library(dplyr)

Players = unique(AFL_Stats_DF$Player)

library(stringi)
library(stringr)


s1 = strsplit(Players, ', ')
Player_names_vec = NULL
for(i in 1:length(s1)){
  Player_names_vec = c(Player_names_vec, paste(s1[[i]][2], s1[[i]][1], sep = ' '))
}


Player_Positions = NULL
Pos = NULL
for(i in 1:length(Player_names_vec)){
  Player_Positions_Subset = PlayerPosition_df %>% filter(Player == Player_names_vec[i], Year == 2016)
  Pos = c(Pos, Player_Positions_Subset$Position[1])
}

AFL_Player_Stats_list = list()
k = 1
for(i in 1:length(Players)){
  AFL_Player_Stats_list[[k]] = AFL_Stats_DF %>% filter(Player == Players[i])
  k = k + 1
}


for(i in 1:length(AFL_Player_Stats_list)){
  AFL_Player_Stats_list[[i]]$Position = Pos[i]
}


#Summary Statistics for each player

SummaryFunction = function(X){
  c1 = colnames(X)
  Y = NULL
  for(i in 1:ncol(X)){
    Y = c(Y, quantile(X[,i], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), mean(X[,i], na.rm = TRUE))
  }
  Y = t(Y)
  colnames(Y) = paste0(rep(c1, each = 4), c('_lq', '_med', '_uq', '_mean'))
  return(Y)
}

AFL_Player_Summary = NULL
for(i in 1:length(AFL_Player_Stats_list)){
  X = AFL_Player_Stats_list[[i]]
  Y = X %>% select(-c(Number, Player, PointDifference, Team, Season, Round, GameID, Position))
  #If you want the data for only a specific year, uncomment the following line and replace the word Year with the Year of data you want
  #Y %>% filter(Season == Year)
  W = SummaryFunction(Y)
  A = data.frame(Number = X$Number[1], Player = X$Player[1])
  A = cbind(A, W)
  A$Position = X$Position[1]
  AFL_Player_Summary = rbind(AFL_Player_Summary, A)
}


write.csv(AFL_Player_Summary, file = 'AFL_Player_Summary_DF', row.names = FALSE)

AFL_Player_Summary_Known = AFL_Player_Summary %>% filter(!is.na(Position))

AFL_Player_Summary_Unknown = AFL_Player_Summary %>% filter(is.na(Position))


write.csv(AFL_Player_Summary_Known, file = 'AFL_Player_Summary_Known_DF', row.names = FALSE)
write.csv(AFL_Player_Summary_Unknown, file = 'AFL_Player_Summary_Unknown_DF', row.names = FALSE)















