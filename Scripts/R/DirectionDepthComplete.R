####################
# Script with all the directional depth subset
####################

library(dplyr)
library(tidyr)

AFL_StatsPosFix_2016 <- read.csv("https://raw.githubusercontent.com/hinestein/AFL_Work/main/Datasets/2016StatsPosFix.csv")

AFL_StatsPosFix_2016 = AFL_StatsPosFix_2016 %>% select(-c(X.1, X))


# Adding in kicks because someone forgot about them
AFL_StatsPosFix_2016$Kicks = AFL_StatsPosFix_2016$Disposals - AFL_StatsPosFix_2016$Handballs




# Start with Rucks
HitoutRatio = NULL
for(i in unique(AFL_StatsPosFix_2016$Pairs)){
  Game = AFL_StatsPosFix_2016 %>% filter(Pairs == i)
  HitoutRatio = c(HitoutRatio, Game$Hitout/sum(Game$Hitout))
}

AFL_StatsPosFix_2016$HitoutRatio = HitoutRatio

Ruck_subset = AFL_StatsPosFix_2016 %>% filter(Position == "Ruck") %>% select(c("Hitouts", "Kicks", "Disposals", "SuperCoachScore", "FreesFor",
                                                                               "ContestedMarks", "Marks", "PointDifference",
                                                                               "Clearances", "Handballs", "Tackles", "HitoutRatio"))

SuperCoachScore Rank Marks Handballs Disposals Goals Tackles FreesAgainst ContestedPossessions UncontestedPossessions ContestedMarks MarksInside50 OnePercenters
GoalAssists PointDifference Kicks

Ruck_Brownlow_votes = AFL_StatsPosFix_2016 %>% filter(Position == "Ruck") %>% select(c("BrownlowVotes", "BrownlowBinary"))

Ruck_list = list()
for(i in 1:ncol(Ruck_subset)){
  Ruck_list[[i]] = matrix(Ruck_subset[,i], ncol = 1)
}

Ruck_Direction_Depth = Directional.Depth(Ruck_list, scale = TRUE)
Ruck_Correlation = cor(Ruck_Direction_Depth, Ruck_Brownlow_votes$BrownlowBinary)


# Key Forwards

Key_Forward_subset = AFL_StatsPosFix_2016 %>% filter(Position == "Key forward") %>% select(c("Disposals", "Kicks", "MarksInside50",
                                                                               "ContestedMarks", "PointDifference", "FreesFor", "Handballs", "Goals",
                                                                               "OnePercenters"))


Key_Forward_Brownlow_votes = AFL_StatsPosFix_2016 %>% filter(Position == "Key forward") %>% select(c("BrownlowVotes", "BrownlowBinary"))

Key_Forward_list = list()
for(i in 1:ncol(Key_Forward_subset)){
  Key_Forward_list[[i]] = matrix(Key_Forward_subset[,i], ncol = 1)
}

Key_Forward_Direction_Depth = Directional.Depth(Key_Forward_list, scale = TRUE)
Key_Forward_Correlation = cor(Key_Forward_Direction_Depth, Key_Forward_Brownlow_votes$BrownlowBinary)

# Inside mid

Inside_Mid_subset = AFL_StatsPosFix_2016 %>% filter(Position == "Inside midfielder") %>% select(c("SuperCoachScore", "Rank", "Marks",
                                                                                           "Disposals", "Goals", "Tackles", "Rebounds50s",
                                                                                           "FreesAgainst", "ContestedPossessions",
                                                                                           "UncontestedPossessions", "OnePercenters",
                                                                                           "GoalAssists", "PointDifference"))

SuperCoachScore Marks Handballs Disposals Goals Tackles Clangers FreesFor MarksInside50 OnePercenters GoalAssists PointDifference Kicks

Inside_Mid_Brownlow_votes = AFL_StatsPosFix_2016 %>% filter(Position == "Inside midfielder") %>% select(c("BrownlowVotes", "BrownlowBinary"))

Inside_Mid_list = list()
for(i in 1:ncol(Inside_Mid_subset)){
  Inside_Mid_list[[i]] = matrix(Inside_Mid_subset[,i], ncol = 1)
}

Inside_Mid_Direction_Depth = Directional.Depth(Inside_Mid_list, scale = TRUE)
Inside_Mid_Correlation = cor(Inside_Mid_Direction_Depth, Inside_Mid_Brownlow_votes$BrownlowBinary)

# General Defender

General_Defender_subset = AFL_StatsPosFix_2016 %>% filter(Position == "General defender") %>% select(c("SuperCoachScore", "Marks", "Handballs", "Disposals",
                                                                                                       "Goals", "Tackles", "Rebounds50s", "Clearances", "Clangers",
                                                                                                       "FreesFor", "FreesAgainst", "UncontestedPossessions",
                                                                                                       "OnePercenters"))

General_Defender_Brownlow_votes = AFL_StatsPosFix_2016 %>% filter(Position == "General defender") %>% select(c("BrownlowVotes", "BrownlowBinary"))

General_Defender_list = list()
for(i in 1:ncol(General_Defender_subset)){
  General_Defender_list[[i]] = matrix(General_Defender_subset[,i], ncol = 1)
}

General_Defender_Direction_Depth = Directional.Depth(General_Defender_list, scale = TRUE)
General_Defender_Correlation = cor(General_Defender_Direction_Depth, General_Defender_Brownlow_votes$BrownlowBinary)


# Key Defender

Key_Defender_subset = AFL_StatsPosFix_2016 %>% filter(Position == "Key defender") %>% select(c("SuperCoachScore", "Handballs", "Disposals",
                                                                                                       "Goals", "Rebounds50s", 
                                                                                                       "FreesFor", "UncontestedPossessions",
                                                                                                       "OnePercenters", "ContestedMarks"))

Key_Defender_Brownlow_votes = AFL_StatsPosFix_2016 %>% filter(Position == "Key defender") %>% select(c("BrownlowVotes", "BrownlowBinary"))

Key_Defender_list = list()
for(i in 1:ncol(Key_Defender_subset)){
  Key_Defender_list[[i]] = matrix(Key_Defender_subset[,i], ncol = 1)
}

Key_Defender_Direction_Depth = Directional.Depth(Key_Defender_list, scale = TRUE)
Key_Defender_Correlation = cor(Key_Defender_Direction_Depth, Key_Defender_Brownlow_votes$BrownlowBinary)

# General Forward

General_Forward_subset = AFL_StatsPosFix_2016 %>% filter(Position == "General forward") %>% select(c("Rank", "Goals", "Behinds", "Tackles", "Inside50s",
                                                                                                          "Clangers", "FreesFor", "FreesAgainst",
                                                                                                          "UncontestedPossessions", "MarksInside50",
                                                                                                          "OnePercenters", "GoalAssists"))

General_Forward_Brownlow_votes = AFL_StatsPosFix_2016 %>% filter(Position == "General forward") %>% select(c("BrownlowVotes", "BrownlowBinary"))

General_Forward_list = list()
for(i in 1:ncol(General_Forward_subset)){
  General_Forward_list[[i]] = matrix(General_Forward_subset[,i], ncol = 1)
}

General_Forward_Direction_Depth = Directional.Depth(General_Forward_list, scale = TRUE)
General_Forward_Correlation = cor(General_Forward_Direction_Depth, General_Forward_Brownlow_votes$BrownlowBinary)


# Defensive Midfielder

Defensive_Mid_subset = AFL_StatsPosFix_2016 %>% filter(Position == "Defensive midfielder") %>% select(c("Marks", "Disposals", "Goals", "Tackles",
                                                                                                   "Rebounds50s", "Inside50s", "Clearances",
                                                                                                   "Clangers", "FreesFor", "FreesAgainst",
                                                                                                   "UncontestedPossessions", "MarksInside50",
                                                                                                   "OnePercenters", "PointDifference"))

Defensive_Mid_Brownlow_votes = AFL_StatsPosFix_2016 %>% filter(Position == "Defensive midfielder") %>% select(c("BrownlowVotes", "BrownlowBinary"))

Defensive_Mid_list = list()
for(i in 1:ncol(Defensive_Mid_subset)){
  Defensive_Mid_list[[i]] = matrix(Defensive_Mid_subset[,i], ncol = 1)
}

Defensive_Mid_Direction_Depth = Directional.Depth(Defensive_Mid_list, scale = TRUE)
Defensive_Mid_Correlation = cor(Defensive_Mid_Direction_Depth, Defensive_Mid_Brownlow_votes$BrownlowBinary)

# Outside Midfielder

Outside_Mid_subset = AFL_StatsPosFix_2016 %>% filter(Position == "Outside midfielder") %>% select(c("SuperCoachScore", "Rank", "Handballs", "Goals",
                                                                                                    "Rebounds50s", "Clearances", "Inside50s",
                                                                                                    "Clangers", "FreesFor", "FreesAgainst",
                                                                                                    "UncontestedPossessions", "MarksInside50",
                                                                                                    "OnePercenters", "GoalAssists", "PointDifference"))

Outside_Mid_Brownlow_votes = AFL_StatsPosFix_2016 %>% filter(Position == "Outside midfielder") %>% select(c("BrownlowVotes", "BrownlowBinary"))

Outside_Mid_list = list()
for(i in 1:ncol(Outside_Mid_subset)){
  Outside_Mid_list[[i]] = matrix(Outside_Mid_subset[,i], ncol = 1)
}

Outside_Mid_Direction_Depth = Directional.Depth(Outside_Mid_list, scale = TRUE)
Outside_Mid_Correlation = cor(Outside_Mid_Direction_Depth, Outside_Mid_Brownlow_votes$BrownlowBinary)



