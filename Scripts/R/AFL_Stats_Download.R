library(rvest)

Games = read_html("https://afltables.com/afl/seas/2023.html")

Games1 = html_nodes(Games, "table") 

Games2 = Games %>% html_elements("body") %>% html_elements("center")

Games3 = html_table(html_nodes(Games2, "table"), header = FALSE, fill = TRUE)

Games.list = list()


Team.Numbers = data.frame(Number = c("14", "03", "07", "11", "20", "21", "15", "01", "18", "04", "10", "05", "16", "13", "09", "12", "08", "19"),
                          Name = c("Richmond", "Carlton", "Western Bulldogs", "Melbourne", "Gold Coast", "Greater Western Sydney",
                                   "St Kilda", "Adelaide", "West Coast", "Collingwood", "Hawthorn", "Essendon",
                                   "Sydney", "Port Adelaide", "Geelong", "North Melbourne", "Fremantle", "Brisbane Lions"))


Rounds = paste0("Round ", 1:23)
R2 = data.frame(Rounds = Rounds, Number = 0)
for(j in 1:length(Rounds)){
  R1 = NULL
  for(i in 1:length(Games3)){
    if(gregexpr(Rounds[j], Games3[[i]][1,1])[[1]][1] > 0){
      R1 = c(R1, i)
    }
  }
  R2[j,2] = R1[1]
}


R3 = data.frame(Round = c(R2$Rounds, "End"), Number = c(R2$Number, R2$Number[nrow(R2)] + 12))


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


Game.stats = list()
for(i in 1:length(Games.list)){
  Game.stats[[i]] = list()
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
    k1 = 3
    k2 = 5
    if(nrow(html_table(HomeStatsNodes, header = TRUE, fill = TRUE)[[3]]) == 0){
      k1 = 4
      k2 = 6
    }
    
    HomeStats <- html_table(HomeStatsNodes, header = TRUE, fill = TRUE)[[k1]]
    
    cn = as.character(HomeStats[1,])
    HomeStats2 <- HomeStats[-1, ]
    colnames(HomeStats2) = NULL
    DF1 = data.frame(Number = HomeStats2[1:23,1], Player = HomeStats2[1:23,2], Kicks = (HomeStats2[1:23,3]),
                     Marks = (HomeStats2[1:23,4]), Handballs = (HomeStats2[1:23,5]), Disposals = (HomeStats2[1:23,6]),
                     Goals = (HomeStats2[1:23,7]), Behinds = (HomeStats2[1:23,8]), Hitouts = (HomeStats2[1:23,9]),
                     Tackles = (HomeStats2[1:23,10]), Rebounds50s = (HomeStats2[1:23,11]), Inside50s = (HomeStats2[1:23,12]),
                     Clearances = (HomeStats2[1:23,13]), Clangers = (HomeStats2[1:23,14]), FreesFor = (HomeStats2[1:23,15]),
                     FreesAgainst = (HomeStats2[1:23,16]), BrownlowVotes = (HomeStats2[1:23,17]), ContestedPossessions = (HomeStats2[1:23,18]),
                     UncontestedPossessions = (HomeStats2[1:23,19]), ContestedMarks = (HomeStats2[1:23,20]), MarksInside50 = (HomeStats2[1:23,21]),
                     OnePercenters = (HomeStats2[1:23,22]), Bounces = (HomeStats2[1:23,23]), GoalAssists = (HomeStats2[1:23,24]),
                     PercentagePlayed = (HomeStats2[1:23,25]))
    
    DF2 = DF1[,-2]
    DF2 = as.matrix(DF2)
    
    DF3 = data.frame(Number = as.numeric(DF2[,1]), Player = as.character(DF1[,2]), Kicks = as.numeric(DF2[,2]),
                     Marks = as.numeric(DF2[,3]), Handballs = as.numeric(DF2[,4]), Disposals = as.numeric(DF2[,5]),
                     Goals = as.numeric(DF2[,6]), Behinds = as.numeric(DF2[,7]), Hitouts = as.numeric(DF2[,8]),
                     Tackles = as.numeric(DF2[,9]), Rebounds50s = as.numeric(DF2[,10]), Inside50s = as.numeric(DF2[,11]),
                     Clearances = as.numeric(DF2[,12]), Clangers = as.numeric(DF2[,13]), FreesFor = as.numeric(DF2[,14]),
                     FreesAgainst = as.numeric(DF2[,15]), BrownlowVotes = as.numeric(DF2[,16]), ContestedPossessions = as.numeric(DF2[,17]),
                     UncontestedPossessions = as.numeric(DF2[,18]), ContestedMarks = as.numeric(DF2[,19]), MarksInside50 = as.numeric(DF2[,20]),
                     OnePercenters = as.numeric(DF2[,21]), Bounces = as.numeric(DF2[,22]), GoalAssists = as.numeric(DF2[,23]),
                     PercentagePlayed = as.numeric(DF2[,24]), PointDifference = as.numeric(R1$HomeScore[j]) - as.numeric(R1$AwayScore[j]),
                     Team = R1$HomeTeam[j], Season = Year, Round = i)
    
    
    DF3[is.na(DF3)] = 0
    
    AwayStatsParse <- read_html(file1)
    
    AwayStatsNodes <- html_nodes(AwayStatsParse, "table")
    
    AwayStats <- html_table(AwayStatsNodes, header = TRUE, fill = TRUE)[[k2]]
    
    cn = as.character(AwayStats[1,])
    AwayStats2 <- AwayStats[-1, ] 
    colnames(AwayStats2) = NULL
    DF1 = data.frame(Number = AwayStats2[1:23,1], Player = AwayStats2[1:23,2], Kicks = (AwayStats2[1:23,3]),
                     Marks = (AwayStats2[1:23,4]), Handballs = (AwayStats2[1:23,5]), Disposals = (AwayStats2[1:23,6]),
                     Goals = (AwayStats2[1:23,7]), Behinds = (AwayStats2[1:23,8]), Hitouts = (AwayStats2[1:23,9]),
                     Tackles = (AwayStats2[1:23,10]), Rebounds50s = (AwayStats2[1:23,11]), Inside50s = (AwayStats2[1:23,12]),
                     Clearances = (AwayStats2[1:23,13]), Clangers = (AwayStats2[1:23,14]), FreesFor = (AwayStats2[1:23,15]),
                     FreesAgainst = (AwayStats2[1:23,16]), BrownlowVotes = (AwayStats2[1:23,17]), ContestedPossessions = (AwayStats2[1:23,18]),
                     UncontestedPossessions = (AwayStats2[1:23,19]), ContestedMarks = (AwayStats2[1:23,20]), MarksInside50 = (AwayStats2[1:23,21]),
                     OnePercenters = (AwayStats2[1:23,22]), Bounces = (AwayStats2[1:23,23]), GoalAssists = (AwayStats2[1:23,24]),
                     PercentagePlayed = (AwayStats2[1:23,25]))
    
    DF2 = DF1[,-2]
    DF2 = as.matrix(DF2)
    
    DF4 = data.frame(Number = as.numeric(DF2[,1]), Player = as.character(DF1[,2]), Kicks = as.numeric(DF2[,2]),
                     Marks = as.numeric(DF2[,3]), Handballs = as.numeric(DF2[,4]), Disposals = as.numeric(DF2[,5]),
                     Goals = as.numeric(DF2[,6]), Behinds = as.numeric(DF2[,7]), Hitouts = as.numeric(DF2[,8]),
                     Tackles = as.numeric(DF2[,9]), Rebounds50s = as.numeric(DF2[,10]), Inside50s = as.numeric(DF2[,11]),
                     Clearances = as.numeric(DF2[,12]), Clangers = as.numeric(DF2[,13]), FreesFor = as.numeric(DF2[,14]),
                     FreesAgainst = as.numeric(DF2[,15]), BrownlowVotes = as.numeric(DF2[,16]), ContestedPossessions = as.numeric(DF2[,17]),
                     UncontestedPossessions = as.numeric(DF2[,18]), ContestedMarks = as.numeric(DF2[,19]), MarksInside50 = as.numeric(DF2[,20]),
                     OnePercenters = as.numeric(DF2[,21]), Bounces = as.numeric(DF2[,22]), GoalAssists = as.numeric(DF2[,23]),
                     PercentagePlayed = as.numeric(DF2[,24]), PointDifference = as.numeric(R1$AwayScore[j]) - as.numeric(R1$HomeScore[j]),
                     Team = R1$AwayTeam[j], Season = Year, Round = i)
    
    
    DF4[is.na(DF4)] = 0
    Game.stats[[i]][[j]] = rbind(DF3, DF4)
  }
  print(i)
}

setwd("C:/Users/Ben/Documents/AFL2023")

for(i in 1:length(Game.stats)){
  for(j in 1:length(Game.stats[[i]])){
    write.csv(Game.stats[[i]][[j]], file = paste0("AFL2023_Round", ifelse(nchar(i) == 1, paste0("0", i), i),
                                                  "Game", j), row.names = FALSE)
  }
}














