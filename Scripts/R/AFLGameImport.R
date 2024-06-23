#Game by game import
URL_Start = 'https://raw.githubusercontent.com/hinestein/AFL_Work/main/Datasets/AFL'
Years = 2016:2023
Rounds = 1:23
Games = 1:9
AFL_Stats = list()
l = 1
library(progress)
pb = progress_bar$new(total = length(Years) * length(Rounds) * length(Games))
for(i in 1:length(Years)){
  for(j in 1:length(Rounds)){
    for(k in 1:length(Games)){
      pb$tick()
      tryCatch({
        Year = Years[i]
        Round = ifelse(nchar(Rounds[j]) == 1, paste0('0', Rounds[j]), Rounds[j])
        Game = Games[k]
        URL_all = paste0(URL_Start, Year, '/AFL', Year, "_Round", Round, 'Game', Game)
        AFL_Stats[[l]] = read.csv(URL_all)
        l = l + 1
      }, error = function(e){})
    }
  }
}

#Yearly Combined Import
#CSV files are large and may take a while
URL_Start = 'https://raw.githubusercontent.com/hinestein/AFL_Work/main/Datasets/YearlyCombined/AFL_Stats_'
Years = 2016:2023
AFL_Stats_Yearly = list()
for(i in 1:length(Years)){
  Year = Years[i]
  URL_all = paste0(URL_Start, Year)
  AFL_Stats[[i]] = read.csv(URL_all)
}




