SuperCoach <- read.csv("C:/Users/Ben/Downloads/AFL_Supercoach_Player_Scores2016_Ranked") #Import data
SuperCoach = SuperCoach[,-c(1,2)] #Get rid of the first two unnessecary columns

Injured_Column = rep(0, nrow(SuperCoach)) #Create a clumn of zeros which will be used to indicate if someone was injured

G1 = gregexpr('Injured', SuperCoach$Player) #Finding the entries in the Players column that include the term 'Injured', if the term injured is present, it give the location in the string where it is, otherwise returns a -1
for(i in 1:nrow(SuperCoach)){ #For loop across the rows of SuperCoach
  if(G1[[i]][1] > 0){  #Check if the entry of the player column has injured
    SuperCoach$Player[i] = substr(SuperCoach$Player[i], 1, G1[[i]][1] - 3) #If the entry include the term 'Injured', get a substring of everything up to that term (included the \n)
    Injured_Column[i] = 1 #Change the zero in the injured column to a 1 as this player was injured
  }
}

SuperCoach$Injured = Injured_Column #adding the injured column to the data frame
