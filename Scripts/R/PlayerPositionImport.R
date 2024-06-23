PlayerPosition <- read.csv("https://raw.githubusercontent.com/hinestein/AFL_Work/main/Datasets/Position%20Work/PlayerPosition.txt")

Teams = c('Port Adelaide', 'Adelaide', 'Brisbane', 'Carlton', 'Collingwood', 'Essendon', 'Fremantle', 'Geelong', 'GWS Giants', 'Gold Coast', 'Hawthorn','North Melbourne',
          'Melbourne', 'St Kilda', 'Sydney', 'Richmond', 'Western Bulldogs', 'West Coast')

Years = 2003:2023

Positions = c('General forward', 'Ruck', 'Defensive midfielder', 'Outside midfielder', 'Inside midfielder', 'Key forward', 'Key defender',
              'General defender')

Names_Lines = NULL
Names_Column = NULL
Team_lines = NULL
Team_Column = NULL
Years_Lines = NULL
Years_Column = NULL
Position_Lines = NULL
Position_Column = NULL
for(i in 1:nrow(PlayerPosition)){
  String = PlayerPosition[i,]
  j = 1
  while(j <= length(Teams)){
    G1 = gregexpr(Teams[j], String)
    if(G1[[1]][1] == -1){
      j = j + 1
    }else if(G1[[1]][1] == 1){
      Team_lines = c(Team_lines, i)
      Team_Column = c(Team_Column, Teams[j])
      j = 1000
    }else{
      Team_lines = c(Team_lines, i)
      Team_Column = c(Team_Column, Teams[j])
      Names_Lines = c(Names_Lines, i)
      Names_Column = c(Names_Column, substr(String, 1, G1[[1]][1] - 2))
      j = 1000
    }
  }
  k = 1
  while(k <= length(Years)){
    G2 = gregexpr(Years[k], String)
    if(G2[[1]][1] > -1){
      Years_Lines = c(Years_Lines, i)
      Years_Column = c(Years_Column, Years[k])
      k = 1000
    }else{
      k = k + 1
    }
  }
  l = 1
  while(l <= length(Positions)){
    G3 = gregexpr(Positions[l], String)
    if(G3[[1]][1] > -1){
      Position_Lines = c(Position_Lines, i)
      Position_Column = c(Position_Column, Positions[l])
      l = 1000
    }else{
      l = l + 1
    }
  }
  if(j < 1000 & k < 1000 & l < 1000){
    Names_Lines = c(Names_Lines, i)
    Names_Column = c(Names_Column, String[[1]])
  }
}

PlayerPosition_df = data.frame(Player = Names_Column, Team = Team_Column, Year = Years_Column, Position = Position_Column)



