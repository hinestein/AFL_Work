library(rvest)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(lubridate)
years = 2012:2023
t1 = 1
Ladder_Position_List = list()
for(m in years){
  Games = read_html(paste0("https://afltables.com/afl/seas/", m, ".html"))
  
  Games1 = html_nodes(Games, "table") 
  
  Games2 = Games %>% html_elements("body") %>% html_elements("center")
  
  Games3 = html_table(html_nodes(Games2, "table"), header = FALSE, fill = TRUE)
  
  Games.list = list()
  
  
  Team.Numbers = data.frame(Number = c("14", "03", "07", "11", "20", "21", "15", "01", "18", "04", "10", "05", "16", "13", "09", "12", "08", "19"),
                            Name = c("Richmond", "Carlton", "Western Bulldogs", "Melbourne", "Gold Coast", "Greater Western Sydney",
                                     "St Kilda", "Adelaide", "West Coast", "Collingwood", "Hawthorn", "Essendon",
                                     "Sydney", "Port Adelaide", "Geelong", "North Melbourne", "Fremantle", "Brisbane Lions"),
                            Abb = c("RI", "CA", "WB", "ME", "GC", "GW", "SK", "AD", "WC", "CW", "HW", "ES", "SY", "PA", "GE", "NM", "FR", "BL"))
  
  
  Rounds = paste0("Round ", 1:24)
  R2 = data.frame(Rounds = Rounds, Number = 0)
  for(j in 1:length(Rounds)){
    tryCatch({
      R1 = NULL
      for(i in 1:length(Games3)){
        if(gregexpr(Rounds[j], Games3[[i]][1,1])[[1]][1] > 0){
          R1 = c(R1, i)
        }
      }
      R2[j,2] = R1[1]
    }, error = function(e){})
  }
  
  R3 = data.frame(Round = c(R2$Rounds, "End"), Number = c(R2$Number, R2$Number[nrow(R2)] + 12))
  R3 = R3 %>% filter(Number != 0)
  R3[nrow(R3), 2] = R3[nrow(R3) - 1, 2] + 12
  
  Rd_Ladder = paste0("Rd ", 1:24, " Ladder")
  Rd_Ladder_DF = data.frame(Rounds = Rd_Ladder, Number = 0)
  for(j in 1:length(Rd_Ladder)){
    tryCatch({
      R1 = NULL
      for(i in 1:length(Games3)){
        if(gregexpr(Rd_Ladder[j], Games3[[i]][1,1])[[1]][1] > 0){
          R1 = c(R1, i)
        }
      }
      Rd_Ladder_DF[j,2] = R1[1]
    }, error = function(e){})
  }
  
  Rd_Ladder_DF_1 = data.frame(Round = c(Rd_Ladder_DF$Rounds, "End"), Number = c(Rd_Ladder_DF$Number, Rd_Ladder_DF$Number[nrow(Rd_Ladder_DF)] + 12))
  Rd_Ladder_DF_1 = Rd_Ladder_DF_1 %>% filter(Number != 0)
  Rd_Ladder_DF_1[nrow(Rd_Ladder_DF_1), 2] = Rd_Ladder_DF_1[nrow(Rd_Ladder_DF_1) - 1, 2] + 12
  Rd_Ladder_DF_1 = Rd_Ladder_DF_1[-nrow(Rd_Ladder_DF_1),]
  
  Ladder_Year = list()
  for(j in 1:length(Rd_Ladder_DF_1[,2])){
    k = Rd_Ladder_DF_1[j,2]
    Ladder = as.data.frame(Games3[[k]][-1,])
    colnames(Ladder) = c("Team", "Wins", "Points", "Percentage")
    Ladder$Season = m
    Ladder$Round = j
    Ladder_Year[[j]] = Ladder
  }
  Ladder_Position_List[[m]] = Ladder_Year
  
}
