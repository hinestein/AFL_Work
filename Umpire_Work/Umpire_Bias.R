# Umpire Bias Script
# Packages
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(GGally)
library(DescTools)
library(gganimate)
library(av)
library(gapminder)
library(berryFunctions)
library(gifski)
library(magick)
library(reshape2)

# Import 2016 to 2023 data
AFL_Stats_DF = read.csv("https://raw.githubusercontent.com/hinestein/AFL_Work/main/Datasets/AFL_Stats_DF")

# Import Umpire data
Umpire_DF =read.csv("https://raw.githubusercontent.com/hinestein/AFL_Work/main/Umpire_Work/Umpire_DF")

# Look at Umpire breakdown
# Umpires
Umpire_Names = unique(c(Umpire_DF$Umpire_1, Umpire_DF$Umpire_2, Umpire_DF$Umpire_3, Umpire_DF$Umpire_4))
# Number of Umpires
n_umpires = length(Umpire_Names)

for(i in Umpire_Names){
  Umpire_single = Umpire_DF %>% filter(Umpire_1 == i | Umpire_2 == i| Umpire_3 == i | Umpire_4 == i)
}


entropy = function(x){
  y = x/sum(x)
  out = - sum( y * log(y))
  return(out)
}


standardised_entropy = function(x){
  y = x/sum(x)
  out = - sum( y * log(y))
  out = out/log(length(x))
  return(out)
}

# Will give 
# Average games umpire per game
Umpired_Games_Summary_DF = NULL

for(i in 1:nrow(Umpire_DF)){
  Game = Umpire_DF[i,]
  Games_Umpired = Game %>% select(c("Umpire_1_Games", "Umpire_2_Games", "Umpire_3_Games", "Umpire_4_Games"))
  Umpired_Games_Summary = data.frame(Average = mean(as.numeric(Games_Umpired), na.rm = TRUE), 
                                     Entropy = entropy(as.numeric(Games_Umpired[!is.na(Games_Umpired)]))/log(sum(!is.na(Games_Umpired))),
                                     Variance = var(as.numeric(Games_Umpired), na.rm = TRUE), Season = Game$Season, 
                                     Round = Game$Round, Attendance = Game$Attendance, Venue = Game$Venue,
                                     Home_Team = Game$Home_Team, Away_Team = Game$Away_Team)
  
  Umpired_Games_Summary_DF = rbind(Umpired_Games_Summary_DF, Umpired_Games_Summary)
}


ggplot(Umpired_Games_Summary_DF) + geom_point(aes(x = Attendance, y = Entropy, colour = Average), size = 1.2) + facet_wrap(~Venue) +
  theme_bw()+ 
  labs(title = "Attendance vs Umpire Games Entropy", col = "Average\nGames") +
  ylab("Standardised Entropy") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"))+ scale_colour_continuous(type = "viridis")

ggplot(Umpired_Games_Summary_DF) + geom_point(aes(x = Attendance, y = Variance, colour = Average), size = 1.2) + facet_wrap(~Venue) +
  theme_bw()+ 
  labs(title = "Attendance vs Umpire Games Variance", col = "Average\nGames") +
  ylab("Variance") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"))+ scale_colour_continuous(type = "viridis")

ggplot(Umpired_Games_Summary_DF %>% filter(Venue == "M.C.G.")) + geom_point(aes(x = Attendance, y = Entropy, colour = Average), size = 2.5) +
  theme_bw()+ 
  labs(title = "Attendance vs Umpire Games\nEntropy (M.C.G.)", col = "Average\nGames") +
  ylab("Standardised Entropy") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"))+ scale_colour_continuous(type = "viridis")

ggplot(Umpire_DF) + geom_point(aes(x = Attendance, y = Home_Frees + Away_Frees)) + facet_wrap(~Venue)

ggplot(Umpire_DF) + geom_boxplot(aes(x = Venue, y = Home_Frees + Away_Frees)) + theme_bw() + 
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.75, hjust = 0.75),
        strip.text = element_text(size = 12), plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14)) +ylab("Frees Awarded") + 
  labs(title = "Venue Frees Boxplot") + geom_hline(yintercept = mean(Umpire_DF$Home_Frees + Umpire_DF$Away_Frees))

ggplot(Umpire_DF) + geom_boxplot(aes(x = Umpire_1, y = Attendance))

plot(Umpired_Games_Summary_DF$Attendance, Umpired_Games_Summary_DF$Entropy)




# Mean Free kicks per umpire

Umpire_Summary = NULL
for(i in Umpire_Names){
  Umpire_single = Umpire_DF %>% filter(Umpire_1 == i | Umpire_2 == i| Umpire_3 == i | Umpire_4 == i)
  mean(Umpire_single$Home_Frees + Umpire_DF$Away_Frees)
}

Umpire_DF

# Mean Free kicks per Team
Teams = unique(Umpire_DF$Home_Team)
Team_Frees = NULL
for(i in Teams){
  Team_Games = Umpire_DF %>% filter(Home_Team == i | Away_Team == i)
  Team_Home_Games = Umpire_DF %>% filter(Home_Team == i)
  Team_Away_Games = Umpire_DF %>% filter(Away_Team == i)
  Temp_DF_1 = data.frame(Team = i, Frees = Team_Games$Home_Frees, Venue = "All")
  Temp_DF_2 = data.frame(Team = i, Frees = Team_Home_Games$Home_Frees, Venue = "Home")
  Temp_DF_3 = data.frame(Team = i, Frees = Team_Away_Games$Away_Frees, Venue = "Away")
  Team_Frees = rbind(Team_Frees, Temp_DF_1, Temp_DF_2, Temp_DF_3)
}

Team_Frees$Venue = factor(Team_Frees$Venue, levels = c("All", "Home", "Away"))


ggplot(Team_Frees) + geom_boxplot(aes(x = Team, y = Frees)) + theme_bw() + facet_wrap(~Venue, nrow = 3) +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.75, hjust = 0.75),
        strip.text = element_text(size = 12), plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14)) +ylab("Frees Awarded") + 
  labs(title = "Team Frees Boxplots")


ggplot(Umpire_DF) + geom_point(aes(x = Home_Frees, y = Away_Frees)) + facet_grid(Home_Team ~ Away_Team)




# Free Kick Pyramid

Team.Numbers = data.frame(Number = c("14", "03", "07", "11", "20", "21", "15", "01", "18", "04", "10", "05", "16", "13", "09", "12", "08", "19"),
                          Name = c("Richmond", "Carlton", "Western Bulldogs", "Melbourne", "Gold Coast", "Greater Western Sydney",
                                   "St Kilda", "Adelaide", "West Coast", "Collingwood", "Hawthorn", "Essendon",
                                   "Sydney", "Port Adelaide", "Geelong", "North Melbourne", "Fremantle", "Brisbane Lions"),
                          Abb = c("RI", "CA", "WB", "ME", "GC", "GW", "SK", "AD", "WC", "CW", "HW", "ES", "SY", "PA", "GE", "NM", "FR", "BL"))


Frees_Pyramid_DF = NULL
Teams = unique(Umpire_DF$Home_Team)
years = 2012:2023
for(m in years){
  print(m)
  Season = Umpire_DF %>% filter(Season == m)
  Season_DF = NULL
  for(i in Teams){
    Team_Season = Season %>% filter(Home_Team == i | Away_Team == i) %>% select(c("Home_Frees", "Away_Frees", "Round", "Season", "Home_Team"))
    bye_Round = which(diff(Team_Season$Round) > 1) + 1
    Frees_For = ifelse(Team_Season$Home_Team == i, Team_Season$Home_Frees, Team_Season$Away_Frees)
    Frees_Against = ifelse(Team_Season$Home_Team == i, Team_Season$Away_Frees, Team_Season$Home_Frees)
    Cum_Frees_For = cumsum(Frees_For)
    Cum_Frees_Against = cumsum(Frees_Against)
    Ladder_Position = NULL
    for(j in 1:max(Team_Season$Round)){
      Ladder = Ladder_Position_List[[m]][[j]]
      Abb = (Team.Numbers %>% filter(Name == i) %>% select(Abb))[1,1]
      Ladder_Position = c(Ladder_Position, which(Ladder$Team == Abb))
    }
    Rounds = 1:max(Team_Season$Round)
    Rounds_Complete = ifelse(nchar(Rounds) == 1, paste0("0", Rounds), Rounds)
    Temp_DF = data.frame(Team = i, Cum_Frees_For = Cum_Frees_For, Cum_Frees_Against = Cum_Frees_Against)
    if(max(diff(Team_Season$Round)) == 2){
      Temp_DF = insertRows(Temp_DF, bye_Round, new = Temp_DF[bye_Round - 1,])
    }else if(max(diff(Team_Season$Round)) == 3){
      Temp_DF = insertRows(Temp_DF, bye_Round, new = Temp_DF[bye_Round - 1,])
      Temp_DF = insertRows(Temp_DF, bye_Round, new = Temp_DF[bye_Round - 1,])
    }
    Temp_DF$Round = Rounds
    Temp_DF$Season = m
    Temp_DF$Ladder_Position = Ladder_Position
    Temp_DF$Rounds_Complete = paste(m, "Round", Rounds_Complete, sep = " ")
    Temp_DF_1 = data.frame(Team = i, Frees = c(-Temp_DF$Cum_Frees_For, Temp_DF$Cum_Frees_Against), Round = Temp_DF$Round,
                           Season = Temp_DF$Season, Ladder_Position = Ladder_Position, Round_Complete = Temp_DF$Rounds_Complete,
                           Class = c(rep("Frees For", nrow(Temp_DF)), rep("Frees Against", nrow(Temp_DF))),
                           Difference = Temp_DF$Cum_Frees_For - Temp_DF$Cum_Frees_Against)
    Season_DF = rbind(Season_DF, Temp_DF_1)
  }
  Frees_Pyramid_DF = rbind(Frees_Pyramid_DF, Season_DF)
}

Frees_Pyramid_DF_1 = Frees_Pyramid_DF[order(Frees_Pyramid_DF$Ladder_Position),]
Frees_Pyramid_DF_1 = Frees_Pyramid_DF_1[order(Frees_Pyramid_DF_1$Round_Complete),]
Frees_Pyramid_DF_1$Ladder_Position_alt = paste(Frees_Pyramid_DF_1$Round_Complete, Ladder_Position, sep = " ")

Team_Abb = NULL
for(i in 1:nrow(Frees_Pyramid_DF_1)){
  Team_Abb = c(Team_Abb, Team.Numbers$Abb[Team.Numbers$Name == Frees_Pyramid_DF_1$Team[i]])
}

Frees_Pyramid_DF_1$Abb = Team_Abb

setwd("/Users/bhines/Documents/AFL_Modelling/GG_Animate_Work")
Frees_Pyramid_Animation = ggplot(Frees_Pyramid_DF_1,
       aes(x = Frees,
           y = factor(Ladder_Position),
           fill = Class)) +
  geom_vline(xintercept = 0) +
  geom_col() +
  geom_point(aes(x = -Difference)) +
  geom_text(aes(x = -250, label = Abb, y = 19 - Ladder_Position)) +
  scale_y_discrete(limits=rev) + scale_x_continuous(labels = abs) + theme_bw() +
  # Here comes the gganimate specific bits
  labs(title = '{closest_state}', x = 'Frees', y = 'Ladder Position') +
  transition_states(Round_Complete) +
  ease_aes('sine-in')+ 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12), plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14))

anim = animate(Frees_Pyramid_Animation, nframes = length(unique(Frees_Pyramid_DF_1$Round_Complete)) * 20, renderer = gifski_renderer(), fps = 20)
anim_save("Frees_Pyramid_Animation.gif", anim)

setwd("/Users/bhines/Documents/AFL_Modelling")


#Frees Per Umpire
Umpire_Frees_DF = NULL
Umpire_2023_DF = Umpire_DF %>% filter(Season == 2023)
Umpire_Names_2023 = unique(unlist(as.vector(Umpire_2023_DF %>% select(c("Umpire_1", "Umpire_2", "Umpire_3", "Umpire_4")))))
Umpire_Names_2023 = Umpire_Names_2023[!is.na(Umpire_Names_2023)]
for(i in Umpire_Names_2023){
  Umpire = Umpire_DF %>% filter(Umpire_1 == i | Umpire_2 == i | Umpire_3 == i | Umpire_4 == i) 
  Umpire_Free = Umpire$Home_Frees + Umpire$Away_Frees
  Temp_DF = data.frame(Umpire = i, Frees = Umpire_Free)
  Temp_DF$Seniority = (Umpire %>% mutate(Seniority = case_when(Umpire_1 == i ~ "Umpire 1",
                                           Umpire_2 == i ~ "Umpire 2",
                                           Umpire_3 == i ~ "Umpire 3",
                                           Umpire_4 == i ~ "Umpire 4")))$Seniority
  Umpire_Frees_DF = rbind(Umpire_Frees_DF, Temp_DF)
}

ggplot(Umpire_Frees_DF) + geom_boxplot(aes(x = Seniority, y = Frees)) + facet_wrap(~ Umpire) + theme_bw()

# Umpire experience by team
Teams = unique(Umpire_DF$Home_Team)
Team_Average_Umpire_Games_DF = NULL
for(i in Teams){
  Team = Umpire_DF %>% filter(Home_Team == i | Away_Team == i)
  Team_Average_Umpire_Games = rowMeans(Team %>% select(c("Umpire_1_Games", "Umpire_2_Games", "Umpire_3_Games", "Umpire_4_Games")), na.rm = TRUE)
  Temp_DF = data.frame(Team = i, Ave_Umpire_Games = Team_Average_Umpire_Games)
  Team_Average_Umpire_Games_DF = rbind(Team_Average_Umpire_Games_DF, Temp_DF)
}

ggplot(Team_Average_Umpire_Games_DF, aes(x = Team, y = Ave_Umpire_Games)) + geom_boxplot() + theme_bw() +
  labs(title = "Umpired Games Average", x = "Team", y = "Average Umpired Game") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


anova(lm(Ave_Umpire_Games ~ factor(Team), data = Team_Average_Umpire_Games_DF))

# Free kicks per team game

Teams = unique(Umpire_DF$Home_Team)
Team_Games_Free_Average_DF = NULL
for(i in Teams){
  Team = Umpire_DF %>% filter(Home_Team == i | Away_Team == i)
  Team_Games_Free_Average = rowSums(Team %>% select(c("Home_Frees", "Away_Frees")), na.rm = TRUE)
  Temp_DF = data.frame(Team = i, Frees = Team_Games_Free_Average)
  Team_Games_Free_Average_DF = rbind(Team_Games_Free_Average_DF, Temp_DF)
}

ggplot(Team_Games_Free_Average_DF, aes(x = Team, y = Frees)) + geom_boxplot() + theme_bw() +
  labs(title = "Team Frees Per Game", x = "Team", y = "Frees Per Game") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Free Kicks per team

Teams = unique(Umpire_DF$Home_Team)
Team_Free_Average_DF = NULL
Years = unique(Umpire_DF$Season)
for(i in Teams){
  Team = Umpire_DF %>% filter(Home_Team == i | Away_Team == i)
  for(j in Years){
    Team_Year = Team %>% filter(Season == j)
    Team_Year_Home_Frees = Team_Year %>% filter(Home_Team == i) %>% select("Home_Frees")
    Team_Year_Away_Frees = Team_Year %>% filter(Away_Team == i) %>% select("Away_Frees")
    Temp_DF = data.frame(Team = i, Season = j,
                         Frees = c(Team_Year_Home_Frees$Home_Frees, Team_Year_Away_Frees$Away_Frees))
    Team_Free_Average_DF = rbind(Team_Free_Average_DF, Temp_DF)
  }
}

g_anim = ggplot(Team_Free_Average_DF, aes(x = Team, y = Frees)) + geom_boxplot() + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  # Here comes the gganimate specific bits
  labs(title = 'Year: {closest_state}', x = 'Team', y = 'Frees For Awarded') +
  transition_states(Season) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in')
anim = animate(g_anim, renderer = gifski_renderer(), nframes = length(Years) * 20)
anim_save("FreesForAwardedPerTeam.gif", anim)


# Free Kicks Difference per team

Teams = unique(Umpire_DF$Home_Team)
Team_Free_Average_DF = NULL
Years = unique(Umpire_DF$Season)
for(i in Teams){
  Team = Umpire_DF %>% filter(Home_Team == i | Away_Team == i)
  for(j in Years){
    Team_Year = Team %>% filter(Season == j)
    Team_Year_Home_Frees = (Team_Year %>% filter(Home_Team == i) %>% select("Home_Frees"))[,1] - (Team_Year %>% filter(Home_Team == i) %>% select("Away_Frees"))[,1]
    Team_Year_Away_Frees = (Team_Year %>% filter(Away_Team == i) %>% select("Away_Frees"))[,1] - (Team_Year %>% filter(Away_Team == i) %>% select("Home_Frees"))[,1]
    Temp_DF = data.frame(Team = i, Season = j,
                         Frees = c(Team_Year_Home_Frees, Team_Year_Away_Frees))
    Team_Free_Average_DF = rbind(Team_Free_Average_DF, Temp_DF)
  }
}

g_anim = ggplot(Team_Free_Average_DF, aes(x = Team, y = Frees)) + geom_boxplot() + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  # Here comes the gganimate specific bits
  labs(title = 'Year: {closest_state}', x = 'Team', y = 'Frees Difference') +
  transition_states(Season) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in') + geom_hline(yintercept = 0)
anim = animate(g_anim, renderer = gifski_renderer(), nframes = length(Years) * 20)
anim_save("FreesDifferencePerTeam.gif", anim)

# Free kicks per round
Frees_Per_Round = NULL
Frees_Per_Season = NULL
Seasons = unique(Umpire_DF$Season)
Rounds = unique(Umpire_DF$Round)
k = 1
for(i in Seasons){
  Umpire_Season = Umpire_DF %>% filter(Season == i)
  Temp_Season_DF = data.frame(Season = i, Frees = mean(Umpire_Season$Home_Frees + Umpire_Season$Away_Frees))
  Frees_Per_Season = rbind(Frees_Per_Season, Temp_Season_DF)
  for(j in Rounds){
    Umpire_Round = Umpire_Season %>% filter(Round == j)
    if(nrow(Umpire_Round) != 0){
      Round_Complete = paste0(i, ifelse(nchar(j) == 1, paste0("0", j), j))
      Temp_Round_DF = data.frame(Season = i, Round = j, Round_Complete = Round_Complete, Frees = mean(Umpire_Round$Home_Frees + Umpire_Round$Away_Frees), Date = k)
      k = k + 1
      Frees_Per_Round = rbind(Frees_Per_Round, Temp_Round_DF)
    }
  }
}


ggplot(Frees_Per_Season, aes(x = Season, y = Frees)) + geom_line() + theme_bw()

ggplot(Frees_Per_Round, aes(x = Date, y = Frees)) + geom_line() + theme_bw()


table(Umpire_DF %>% select(c("Umpire_1", "Umpire_2")))


Frees_Points_DF = NULL
Teams = unique(Umpire_DF$Home_Team)
for(i in Teams){
  Team_Home_DF = Umpire_DF %>% filter(Home_Team == i)
  Team_Away_DF = Umpire_DF %>% filter(Away_Team == i)
  Temp_DF = data.frame(Team = i, Frees_Difference = c(Team_Home_DF$Home_Frees -Team_Home_DF$Away_Frees, 
                                                      Team_Away_DF$Away_Frees -Team_Away_DF$Home_Frees),
                       Points_Difference = c(Team_Home_DF$Home_Score - Team_Home_DF$Away_Score, 
                                             Team_Away_DF$Away_Score - Team_Away_DF$Home_Score),
                       Venue = factor(c(rep("Home", nrow(Team_Home_DF)), rep("Away", nrow(Team_Away_DF))),
                                      levels = c("Home", "Away")), Season = c(Team_Home_DF$Season, Team_Away_DF$Season))
  Frees_Points_DF = rbind(Frees_Points_DF, Temp_DF)
}

g_anim = ggplot(Frees_Points_DF, aes(x = Frees_Difference, y = Points_Difference, colour = Venue), alpha = 0.5) + geom_point() + facet_wrap(~Team, nrow = 6) + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), legend.title=element_text(size=12), legend.text=element_text(size=12))+
  scale_colour_brewer(palette = "Set1") + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
  # Here comes the gganimate specific bits
  labs(title = 'Frees Difference Against Points Difference for Year: {closest_state}', x = 'Frees DIfference', y = 'Points Difference') +
  transition_states(Season) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in')
anim = animate(g_anim, renderer = gifski_renderer(), nframes = length(Years) * 20)  
anim_save("FreesDifferencePointsDifference.gif", anim)


Frees_Points_Correlation_DF = NULL
for(i in Teams){
  Team_DF = Frees_Points_DF %>% filter(Team == i)
  All_Correlation = cor(Team_DF$Frees_Difference, Team_DF$Points_Difference)
  Home_Correlation = cor(Team_DF %>% filter(Venue == "Home") %>% select("Frees_Difference"),
                         Team_DF %>% filter(Venue == "Home") %>% select("Points_Difference"))
  Away_Correlation = cor(Team_DF %>% filter(Venue == "Away") %>% select("Frees_Difference"),
                         Team_DF %>% filter(Venue == "Away") %>% select("Points_Difference"))
  Temp_DF = data.frame(Team = i, Correlation = c(All_Correlation, Home_Correlation, Away_Correlation),
                       Venue = factor(c("All", "Home", "Away"), levels = c("All", "Home", "Away")))
  Frees_Points_Correlation_DF = rbind(Frees_Points_Correlation_DF, Temp_DF)
}

ggplot(Frees_Points_Correlation_DF, aes(x = Team, y = Correlation, colour = Venue)) + geom_point(size = 4)+ theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title=element_text(size=12), legend.text=element_text(size=12)) + geom_hline(yintercept = 0) +
  labs(title = "Frees-Points Discrepency Correlation", x = "Team", y = "Correlation")+
  scale_colour_brewer(palette = "Set1")


Seasons = unique(Frees_Points_DF$Season)
Frees_Points_Correlation_Seasons_DF = NULL
for(i in Teams){
  for(j in 1:(length(Seasons) - 2)){
    Team_DF = Frees_Points_DF %>% filter(Team == i) %>% filter(Season == Seasons[j] | Season == Seasons[j + 1] | Season == Seasons[j + 2])
    All_Correlation = cor(Team_DF$Frees_Difference, Team_DF$Points_Difference)
    Home_Correlation = cor(Team_DF %>% filter(Venue == "Home") %>% select("Frees_Difference"),
                           Team_DF %>% filter(Venue == "Home") %>% select("Points_Difference"))
    Away_Correlation = cor(Team_DF %>% filter(Venue == "Away") %>% select("Frees_Difference"),
                           Team_DF %>% filter(Venue == "Away") %>% select("Points_Difference"))
    Temp_DF = data.frame(Team = i, Correlation = c(All_Correlation, Home_Correlation, Away_Correlation),
                         Venue = factor(c("All", "Home", "Away"), levels = c("All", "Home", "Away")),
                         Seasons = factor(paste0(Seasons[j], " - ", Seasons[j + 2])))
    Frees_Points_Correlation_Seasons_DF = rbind(Frees_Points_Correlation_Seasons_DF, Temp_DF)
  }
}


g_anim = ggplot(Frees_Points_Correlation_Seasons_DF, aes(x = Team, y = Correlation, colour = Venue)) + geom_point(size = 4)+ theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title=element_text(size=12), legend.text=element_text(size=12)) + geom_hline(yintercept = 0) +
  scale_colour_brewer(palette = "Set1")+
  # Here comes the gganimate specific bits
  labs(title = 'Seasons: {closest_state}', x = 'Team', y = 'Correlation') +
  transition_states(Seasons) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in')
anim = animate(g_anim, renderer = gifski_renderer(), nframes = length(unique(Frees_Points_Correlation_Seasons_DF$Seasons)) * 40, fps = 30) 
anim_save("Frees_Points_Correlation_Seasons.gif", anim)


#Frees per year
Frees_Per_Year_DF = NULL
Seasons = unique(Umpire_DF$Season)
for(i in Seasons){
  Season_DF = Umpire_DF %>% filter(Season == i) %>% select(c("Home_Frees", "Away_Frees", "Round", "Game", "GameID", "Home_Team", "Away_Team"))
  Temp_DF = data.frame(Season = i, Round = Season_DF$Round, Game = Season_DF$Game,
                       GameID = Season_DF$GameID, Frees = Season_DF$Home_Frees + Season_DF$Away_Frees,
                       Home_Team = Season_DF$Home_Team, Away_Team = Season_DF$Away_Team)
  Frees_Per_Year_DF = rbind(Frees_Per_Year_DF, Temp_DF)
}

Mean_Frees = NULL
for(i in Years){
  Year = Frees_Per_Year_DF %>% filter(Season == i)
  Mean_Frees = c(Mean_Frees, mean((Year %>% select("Frees"))[,1], na.rm = TRUE))
}


ggplot(Frees_Per_Year_DF) + geom_boxplot(aes(x = factor(Season), y = Frees)) + theme_bw()+ geom_hline(yintercept = mean(Mean_Frees)) +
  labs(title = "Frees Per Year Boxplots", x = "Year", y = "Number of Frees Awarded")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), legend.title=element_text(size=12), legend.text=element_text(size=12))


# Umpire Venues
Umpires = unique(c(Umpire_DF$Umpire_1, Umpire_DF$Umpire_2, Umpire_DF$Umpire_3, Umpire_DF$Umpire_4))
Umpires = Umpires[!is.na(Umpires)]
Umpire_Venue_DF = NULL
for(i in Umpires){
  Umpire = Umpire_DF %>% filter(Umpire_1 == i | Umpire_2 == i | Umpire_3 == i | Umpire_4 == i)
  if(nrow(Umpire) > 100){
    Temp_DF = data.frame(Umpire = i, Venue = Umpire$Venue, Seniority = case_when(Umpire$Umpire_1 == i ~ "Umpire 1",
                                                                                 Umpire$Umpire_2 == i ~ "Umpire 2",
                                                                                 Umpire$Umpire_3 == i ~ "Umpire 3",
                                                                                 Umpire$Umpire_4 == i ~ "Umpire 4"))
    Umpire_Venue_DF = rbind(Umpire_Venue_DF, Temp_DF)
  }
}

g_anim = ggplot(Umpire_Venue_DF) + geom_bar(aes(x = Venue, fill = Seniority)) + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), legend.title=element_text(size=12), legend.text=element_text(size=12),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1))+
  scale_colour_brewer(palette = "Set1")+
  # Here comes the gganimate specific bits
  labs(title = 'Umpire: {closest_state}', x = 'Venue', y = 'Count') +
  transition_states(Umpire) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in')
anim = animate(g_anim, renderer = gifski_renderer(), nframes = length(Umpires) * 30, fps = 40) 
anim_save("UmpireVenue.gif", anim)

# Umpire Teams
Umpires = unique(c(Umpire_DF$Umpire_1, Umpire_DF$Umpire_2, Umpire_DF$Umpire_3, Umpire_DF$Umpire_4))
Umpires = Umpires[!is.na(Umpires)]
Umpire_Team_DF = NULL
for(i in Umpires){
  Umpire = Umpire_DF %>% filter(Umpire_1 == i | Umpire_2 == i | Umpire_3 == i | Umpire_4 == i)
  if(nrow(Umpire) > 100){
    Temp_DF = data.frame(Umpire = i, Home_Team = Umpire$Home_Team, Away_Team = Umpire$Away_Team,
                         Seniority = case_when(Umpire$Umpire_1 == i ~ "Umpire 1",
                                                                                 Umpire$Umpire_2 == i ~ "Umpire 2",
                                                                                 Umpire$Umpire_3 == i ~ "Umpire 3",
                                                                                 Umpire$Umpire_4 == i ~ "Umpire 4"))
    Umpire_Team_DF = rbind(Umpire_Team_DF, Temp_DF)
  }
}

Umpire_Team_DF = melt(Umpire_Team_DF, id.vars = c("Umpire", "Seniority"))
colnames(Umpire_Team_DF) = c("Umpire", "Seniority", "Class", "Team")

Umpire_Team_DF$Venue = factor(ifelse(Umpire_Team_DF$Class == "Home_Team", "Home Team", "Away Team"), levels = c("Home Team", "Away Team"))

g_anim = ggplot(Umpire_Team_DF) + geom_bar(aes(x = Team, fill = Venue)) + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), legend.title=element_text(size=12), legend.text=element_text(size=12),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 12))+
  scale_colour_brewer(palette = "Set1")+
  # Here comes the gganimate specific bits
  labs(title = 'Umpire: {closest_state}', x = 'Team', y = 'Count') +
  transition_states(Umpire) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in')
anim = animate(g_anim, renderer = gifski_renderer(), nframes = length(Umpires) * 30, fps = 40)
anim_save("UmpireTeam.gif", anim)

Umpires = unique(c(Umpire_DF$Umpire_1, Umpire_DF$Umpire_2, Umpire_DF$Umpire_3, Umpire_DF$Umpire_4))
Umpires = Umpires[!is.na(Umpires)]
Umpire_Greater_100 = unique(Umpire_Team_DF$Umpire)
Umpire_Team_Entropy_DF = NULL
for(i in Umpire_Greater_100){
  Umpire = Umpire_Team_DF %>% filter(Umpire == i)
  table_DF = data.frame(table(Umpire$Team))
  Temp_DF = data.frame(Umpire = i, Standardised_Entropy = standardised_entropy(table_DF$Freq),
                       Entropy = entropy(table_DF$Freq))
  Umpire_Team_Entropy_DF = rbind(Umpire_Team_Entropy_DF, Temp_DF)
}

hist(Umpire_Team_Entropy_DF$Entropy)
Umpire_Team_Entropy_DF$Class = "Team"

Umpires = unique(c(Umpire_DF$Umpire_1, Umpire_DF$Umpire_2, Umpire_DF$Umpire_3, Umpire_DF$Umpire_4))
Umpires = Umpires[!is.na(Umpires)]
Umpire_Greater_100 = unique(Umpire_Team_DF$Umpire)
Umpire_Venue_Entropy_DF = NULL
for(i in Umpire_Greater_100){
  Umpire = Umpire_Venue_DF %>% filter(Umpire == i)
  table_DF = data.frame(table(Umpire$Venue))
  Temp_DF = data.frame(Umpire = i, Standardised_Entropy = standardised_entropy(table_DF$Freq),
                       Entropy = entropy(table_DF$Freq))
  Umpire_Venue_Entropy_DF = rbind(Umpire_Venue_Entropy_DF, Temp_DF)
}

hist(Umpire_Venue_Entropy_DF$Standardised_Entropy)

Umpire_Venue_Entropy_DF$Class = "Venue"

Umpire_Entropy_DF = rbind(Umpire_Team_Entropy_DF, Umpire_Venue_Entropy_DF)

ggplot(Umpire_Entropy_DF) + geom_boxplot(aes(y = Standardised_Entropy, x = Class), fill = "white", colour = "black") + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), legend.title=element_text(size=12), legend.text=element_text(size=12),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  labs(title = "Standardised Entropy Boxplots", x = "Variable", y = "Stardardised Entropy")
  

# Votes per team

Umpire_DF$Winning_Team = case_when(Umpire_DF$Home_Score > Umpire_DF$Away_Score ~ Umpire_DF$Home_Team,
                                   Umpire_DF$Home_Score < Umpire_DF$Away_Score ~ Umpire_DF$Away_Team, 
                                   Umpire_DF$Home_Score == Umpire_DF$Away_Score ~ "Draw")

Umpire_Vote_Teams_DF = NULL
for(i in 1:nrow(Umpire_DF)){
  Game = Umpire_DF[i,]
  Temp_DF = data.frame(Vote_Team = c(Game$Vote_3_Team, Game$Vote_2_Team, Game$Vote_1_Team), Votes = c("3 Votes", "2 Votes", "1 Vote"))
  Temp_DF$Result = ifelse(Temp_DF$Vote_Team == Game$Winning_Team, "Win",
                          ifelse(Game$Winning_Team == "Draw", "Draw", "Loss"))
  Umpire_Vote_Teams_DF = rbind(Umpire_Vote_Teams_DF, Temp_DF)
}

Umpire_Vote_Teams_DF$Votes = factor(Umpire_Vote_Teams_DF$Votes, levels = c("3 Votes", "2 Votes", "1 Vote"))

Umpire_Vote_Teams_DF$Result = factor(Umpire_Vote_Teams_DF$Result, levels = c("Win", "Draw", "Loss"))

ggplot(Umpire_Vote_Teams_DF) + geom_bar(aes(x = Vote_Team, fill = Result)) + facet_wrap(~ Votes, nrow = 3) + theme_bw()+
  labs(title = "Votes Per Team", x = "Team", y = "Vote Count", fill = "Game Result") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), strip.text = element_text(size = 12))

# Special Games

Umpire_DF$Month_Day = format(as.Date(Umpire_DF$Date), "%m/%d")
Umpire_DF$Month = format(as.Date(Umpire_DF$Date), "%m")

ANZAC_DF = Umpire_DF %>% filter((Home_Team == "Essendon" & Away_Team == "Collingwood") | (Home_Team == "Collingwood" & Away_Team == "Essendon")) %>%
  filter(Month_Day == "04/25")

ANZAC_DF$Special = "ANZAC Day"

Season_Openner = Umpire_DF %>% filter((Home_Team == "Richmond" & Away_Team == "Carlton") | (Home_Team == "Carlton" & Away_Team == "Richmond")) %>%
  filter(Round == 1)

Season_Openner$Special = "Season Opener"

ANZAC_Eve_DF = Umpire_DF %>% filter((Home_Team == "Richmond" & Away_Team == "Melbourne") | (Home_Team == "Melbourne" & Away_Team == "Richmond")) %>% 
  filter(Month_Day == "04/24")

ANZAC_Eve_DF$Special = "ANZAC Eve"

Good_Friday_DF = Umpire_DF %>% filter(Home_Team == "North Melbourne") %>% 
  filter(Month == "03" | Month == "04") %>% filter(Day == "Fri")  %>% filter(Season >= 2016)

Good_Friday_DF$Special = "Good Friday"

Easter_Monday_DF = Umpire_DF %>% filter((Home_Team == "Hawthorn" & Away_Team == "Geelong") | (Home_Team == "Geelong" & Away_Team == "Hawthorn")) %>% 
  filter(Month == "03" | Month == "04")

Easter_Monday_DF$Special = "Easter Monday"

Blue_Ribbon_Cup_DF = Umpire_DF %>% filter((Home_Team == "St Kilda" & Away_Team == "Hawthorn") | (Home_Team == "Hawthorn" & Away_Team == "St Kilda")) %>%
  filter(Month_Day != "04/28") %>% filter(Month_Day != "07/30") %>% filter(Month_Day != "05/27")
Blue_Ribbon_Cup_DF$Special = "Blue Ribbon Cup"

Dream_Time_DF = Umpire_DF %>% filter((Home_Team == "Richmond" & Away_Team == "Essendon") | (Home_Team == "Essendon" & Away_Team == "Richmond")) %>%
  filter(Venue == "M.C.G.") %>% filter(Month == "05")

Dream_Time_DF$Special = "Dream Time"

Kings_Birthday_Eve_DF = Umpire_DF %>% filter((Home_Team == "Essendon" & Away_Team == "Carlton") | (Home_Team == "Carlton" & Away_Team == "Essendon")) %>%
  filter(Month == "06")

Kings_Birthday_Eve_DF$Special = "King's/Queen's Birthday Eve"


Kings_Birthday_DF = Umpire_DF %>% filter((Home_Team == "Melbourne" & Away_Team == "Collingwood") | (Home_Team == "Collingwood" & Away_Team == "Melbourne")) %>%
  filter(Month == "06")

Kings_Birthday_DF$Special = "King's/Queen's Birthday"

Country_DF = Umpire_DF %>% filter((Home_Team == "Essendon" & Away_Team == "Geelong") | (Home_Team == "Geelong" & Away_Team == "Essendon"))

Country_DF$Special = "Country Match" 

Maddies_DF = Umpire_DF %>% filter((Home_Team == "St Kilda" & Away_Team == "Richmond") | (Home_Team == 'Richmond' & Away_Team == 'St Kilda')) %>%
  filter(Venue == "Docklands")

Maddies_DF$Special = "Maddie's Match"

Special_DF = rbind(ANZAC_DF, ANZAC_Eve_DF, Season_Openner, Good_Friday_DF, Easter_Monday_DF, Blue_Ribbon_Cup_DF, Dream_Time_DF,
                   Kings_Birthday_Eve_DF, Kings_Birthday_DF, Country_DF, Maddies_DF)

Special_DF$Mean_Umpired = apply(Special_DF %>% select(c("Umpire_1_Games", "Umpire_2_Games", "Umpire_3_Games", "Umpire_4_Games")),
                                1, mean, na.rm = TRUE)


ggplot(Special_DF) + geom_boxplot(aes(x = Special, y = Mean_Umpired)) + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), strip.text = element_text(size = 12)) +
  labs(title = "Average Game Umpired (Special Games)", y = "Average Games Umpired", x = "Special Game")




# Derby Games

Q_Clash = Umpire_DF %>% filter((Home_Team == "Brisbane Lions" & Away_Team == "Gold Coast") | (Home_Team == "Gold Coast" & Away_Team == "Brisbane Lions"))

Q_Clash$Special = "Q Clash"

Sydney_DF = Umpire_DF %>% filter((Home_Team == "Sydney" & Away_Team == "Greater Western Sydney") | (Home_Team == "Greater Western Sydney" & Away_Team == "Sydney"))

Sydney_DF$Special = "Battle of the Bridge"

Western_Derby = Umpire_DF %>% filter((Home_Team == "Fremantle" & Away_Team == "West Coast") | (Home_Team == "West Coast" & Away_Team == "Fremantle"))

Western_Derby$Special = "Western Derby"

Showdown = Umpire_DF %>% filter((Home_Team == "Adelaide" & Away_Team == "Port Adelaide") | (Home_Team == "Port Adelaide" & Away_Team == "Adelaide"))

Showdown$Special = "Showdown"

Derby_DF = rbind(Q_Clash, Sydney_DF, Western_Derby, Showdown)

Derby_DF$Mean_Umpired = apply(Derby_DF %>% select(c("Umpire_1_Games", "Umpire_2_Games", "Umpire_3_Games", "Umpire_4_Games")),
                                1, mean, na.rm = TRUE)

ggplot(Derby_DF) + geom_boxplot(aes(x = Special, y = Mean_Umpired)) + theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust=1), strip.text = element_text(size = 12)) +
  labs(title = "Average Game Umpired (Derby Games)", y = "Average Games Umpired", x = "Special Game")


#Point Difference Votes Correlation

Votes_3_Bin = ifelse(Umpire_DF$Vote_3_Team == Umpire_DF$Winning_Team, 1, 0)
Votes_2_Bin = ifelse(Umpire_DF$Vote_2_Team == Umpire_DF$Winning_Team, 1, 0)
Votes_1_Bin = ifelse(Umpire_DF$Vote_1_Team == Umpire_DF$Winning_Team, 1, 0)

Vote_3_Points = ifelse(Umpire_DF$Home_Team == Umpire_DF$Vote_3_Team, Umpire_DF$Home_Score - Umpire_DF$Away_Score, Umpire_DF$Away_Score - Umpire_DF$Home_Score)
Vote_2_Points = ifelse(Umpire_DF$Home_Team == Umpire_DF$Vote_3_Team, Umpire_DF$Home_Score - Umpire_DF$Away_Score, Umpire_DF$Away_Score - Umpire_DF$Home_Score)
Vote_1_Points = ifelse(Umpire_DF$Home_Team == Umpire_DF$Vote_3_Team, Umpire_DF$Home_Score - Umpire_DF$Away_Score, Umpire_DF$Away_Score - Umpire_DF$Home_Score)


Votes_Bin_DF = rbind(cbind(Votes_3_Bin, Vote_3_Points), cbind(Votes_2_Bin, Vote_2_Points), cbind(Votes_1_Bin, Vote_1_Points))
Votes_Bin_DF = as.data.frame(Votes_Bin_DF)
colnames(Votes_Bin_DF) = c("Binary_Vote", "Point_Difference")
Votes_Bin_DF$Votes = factor(c(rep(3,length(Votes_3_Bin)), rep(2,length(Votes_2_Bin)), rep(1,length(Votes_3_Bin))))

cor(Votes_3_Bin, Vote_3_Points)
cor(Votes_2_Bin, Vote_2_Points)
cor(Votes_1_Bin, Vote_1_Points)


for(i in 1:nrow(Umpire_DF)){
  Game = Umpire_DF[i,]
  if(Game$Vote_3_Team == Game$Winning_Team){
    Votes_3_DF = rbind(Votes_3_DF, c(1, ))
  }
}


# Senior Umpires Votes

Senior_Umpires = unique(Umpire_DF$Umpire_1)
for(i in Senior_Umpires){
  Senior_ump = Umpire_DF %>% filter(Umpire_1 == i)
  
}


# Average Games per Team
Teams = unique(Umpire_DF$Home_Team)
Seasons = unique(Umpire_DF$Season)
Umpired_Teams_DF = NULL
for(i in Teams){
  Team = Umpire_DF %>% filter(Home_Team == i | Away_Team == i)
  Means = apply(Team %>% select(c("Umpire_1_Games", "Umpire_2_Games", "Umpire_3_Games", "Umpire_4_Games")),
        1, mean, na.rm = TRUE)
  Temp_DF = data.frame(Team = i, Means = Means, Season = Team$Season, Attendance = Team$Attendance)
  Ladder_Pos_Vec = NULL
  for(j in Seasons){
    Season_Temp = Temp_DF %>% filter(Season == j)
    Abb = (Team.Numbers %>% filter(Name == i) %>% select("Abb"))[1,1]
    Ladder_Pos = which(Ladder_Position_List[[j]][[length(Ladder_Position_List[[j]])]]$Team == Abb)
    Ladder_Pos_Vec = c(Ladder_Pos_Vec, rep(Ladder_Pos, nrow(Season_Temp)))
  }
  Temp_DF$Ladder_Pos = Ladder_Pos_Vec
  Umpired_Teams_DF = rbind(Umpired_Teams_DF, Temp_DF)
}

g_anim = ggplot(Umpired_Teams_DF) + geom_boxplot(aes(x = Team, y = Means)) + theme_bw()+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        plot.title = element_text(size = 16, face = "bold"), legend.title=element_text(size=12), legend.text=element_text(size=12),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 12))+
  geom_text(aes(x = Team, label = factor(Ladder_Pos), y = 30)) +
  geom_text(aes(x = Team, label = mean(Attendance), y = 400)) + 
  scale_colour_brewer(palette = "Set1")+
  # Here comes the gganimate specific bits
  labs(title = 'Season: {closest_state}', x = 'Team', y = 'Average Umpired Games') +
  transition_states(Season) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in')
anim = animate(g_anim, renderer = gifski_renderer(), nframes = length(Umpires) * 30, fps = 40)
anim_save("UmpireTeam.gif", anim)


