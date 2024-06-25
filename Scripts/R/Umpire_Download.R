library(rvest)
library(stringr)
library(stringi)
years = 2016:2023
for(m in years){
  Games = read_html(paste0("https://afltables.com/afl/seas/", m, ".html"))
  
  Games1 = html_nodes(Games, "table") 
  
  Games2 = Games %>% html_elements("body") %>% html_elements("center")
  
  Games3 = html_table(html_nodes(Games2, "table"), header = FALSE, fill = TRUE)
  
  Games.list = list()
  
  
  Team.Numbers = data.frame(Number = c("14", "03", "07", "11", "20", "21", "15", "01", "18", "04", "10", "05", "16", "13", "09", "12", "08", "19"),
                            Name = c("Richmond", "Carlton", "Western Bulldogs", "Melbourne", "Gold Coast", "Greater Western Sydney",
                                     "St Kilda", "Adelaide", "West Coast", "Collingwood", "Hawthorn", "Essendon",
                                     "Sydney", "Port Adelaide", "Geelong", "North Melbourne", "Fremantle", "Brisbane Lions"))
  
  
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
  
  Games.list = list()
  for(i in 1:(nrow(R3) - 1)){
    Temp = NULL
    for(j in (R3[i,2] + 2):(R3[i + 1, 2] - 2)){
      
      HT = as.character(Games3[[j]][1,1])
      AT = as.character(Games3[[j]][2,1])
      
      HS = as.numeric(Games3[[j]][1,3])
      AS = as.numeric(Games3[[j]][2,3])
      
      D1 = Games3[[j]][1,4]
      D2 = substr(D1, 5, 6)
      D3 = substr(D1, 8, 10)
      D4 = substr(D1, 12, 15)
      Temp = rbind(Temp, c(HT, AT, HS, AS, D2, D3, D4))
    }
    Temp = Temp[complete.cases(Temp),]
    colnames(Temp) = c("HomeTeam", "AwayTeam", "HomeScore", "AwayScore", "Day", "Month", "Year")
    Games.list[[i]] = as.data.frame(Temp)
  }
  
  
  Umpire.list = list()
  for(i in 1:length(Games.list)){
    Umpire.list[[i]] = list()
    R1 = Games.list[[i]]
    for(j in 1:nrow(Games.list[[i]])){
      N1 = Team.Numbers[which(Team.Numbers$Name == R1$HomeTeam[j]), 1]
      N2 = Team.Numbers[which(Team.Numbers$Name == R1$AwayTeam[j]), 1]
      Year = R1$Year[j]
      Month = R1$Month[j]
      Month1 = ifelse(Month == "Jan", "01",
                      ifelse(Month == "Feb", "02",
                             ifelse(Month == "Mar", "03",
                                    ifelse(Month == "Apr", "04",
                                           ifelse(Month == "May", "05",
                                                  ifelse(Month == "Jun", "06",
                                                         ifelse(Month == "Jul", "07",
                                                                ifelse(Month == "Aug", "08",
                                                                       ifelse(Month == "Sep", "09",
                                                                              ifelse(Month == "Oct", "10",
                                                                                     ifelse(Month == "Nov", "11", "12")))))))))))
      Day = R1$Day[j]
      file1 = paste0("https://afltables.com/afl/stats/games/", Year, "/",sort(c(N1, N2))[1],
                     sort(c(N1, N2))[2], Year, Month1, Day, ".html")
      
      HomeStatsParse <- read_html(file1)
      
      HomeStatsNodes <- html_nodes(HomeStatsParse, "table")
      k1 = 1
      
      HomeStats <- html_table(HomeStatsNodes, header = TRUE, fill = TRUE)[[k1]]
      
      cn = as.character(HomeStats[5,])[3]
      s1 = strsplit(cn, ", ")[[1]]
      Umps = NULL
      for(k in 1:length(s1)){
        s2 = strsplit(s1[k], " \\(")[[1]]
        Umps = rbind(Umps, c(s2[1], substr(s2[2], 1, (nchar(s2[2]) - 1))))
      }
      colnames(Umps) = c("Umpire_Name", "Games_Umpired")
      Umps = as.data.frame(Umps)
      Umps$Games_Umpired = as.numeric(Umps$Games_Umpired)
      Umpire.list[[i]][[j]] = Umps
      
    }
    print(i)
  }
  
  
  setwd(paste0("C:/Users/Ben/Documents/AFL", m))
  
  for(i in 1:length(Umpire.list)){
    for(j in 1:length(Umpire.list[[i]])){
      write.csv(Umpire.list[[i]][[j]], file = paste0("AFL", m, "_Umpire_Round", ifelse(nchar(i) == 1, paste0("0", i), i),
                                                     "Game", j), row.names = FALSE)
    }
  }
}


