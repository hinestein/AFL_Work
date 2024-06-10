library(dplyr)

AFL_StatsPosFix_2016 <- read.csv("C:/Users/Ben/Downloads/2016StatsPosFix.csv")

AFL_StatsPosFix_2016 = AFL_StatsPosFix_2016 %>% select(-c(X.1, X))

# Inside Midfielder

Inside_Mid  = AFL_StatsPosFix_2016 %>% filter(Position == "Inside midfielder")

Inside_Mid_Reduced = Inside_Mid %>% select(-c(FirstName, LastName, FullName, Team, Year, Position, Round,
                                              Injured, Number, BrownlowVotes, Pairs, Bounces, PercentagePlayed, Hitouts,
                                              Behinds, BrownlowBinary))

n = ncol(Inside_Mid_Reduced)

comb_max = 2^n - 1
pb <- txtProgressBar(min = 1, max = comb_max, style = 3)
t1 = 1
cor_high = 0
for(i in 1:n){
  possiblities = combn(1:n, i)
  for(j in 1:ncol(possiblities)){
    if(i == 1){
      X_new = matrix(Inside_Mid_Reduced[, possiblities[,j]], ncol = 1)
    }else{
      X_new = Inside_Mid_Reduced[, possiblities[,j]]
    }
    X.list = list()
    for(k in 1:ncol(X_new)){
      X.list[[k]] <- matrix(X_new[,k],ncol=1)
    }
    setTxtProgressBar(pb, t1)
    t1 = t1 + 1
    tryCatch({
      D1 = Directional.Depth(X = X.list, scale = TRUE)
      cor_new = cor(D1, Inside_Mid$BrownlowBinary)
      if(abs(cor_new) > abs(cor_high)){
        cor_high = cor_new
        combination = possiblities[,j]
      }
    }, error = function(e){})
  }
}

# Inside Mid Combination
# SuperCoachScore Rank Marks Disposals Goals Tackles Rebounds50s FreesAgainst
# ContestedPossessions UncontestedPossessions MarksInside50 PointDifference

head(Inside_Mid_Reduced[,combination])
