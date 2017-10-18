#reading batting CSV file
library(readr)
batting <- read_csv("~/Desktop/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Capstone Project/Batting.csv")
print(head(batting))
# checking table structure
str(batting)
# adding a Batting average column to data
batting$BA <- batting$H / batting$AB
# just to check
tail(batting$BA)
# Adding ON BASE Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)
# Creating X1B (Singles)
batting$`1B` <- batting$H - batting$`2B` - batting$`3B` - batting$HR
# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$`1B`) + (2 * batting$`2B`) + (3 * batting$`3B`) + (4 * batting$HR) ) / batting$AB
batting <- subset(batting,yearID >= 1985)
summary(batting)
## Merging Salary Data with Batting Data
Salaries <- read_csv("~/Desktop/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Capstone Project/Salaries.csv")
combo <- merge(batting,Salaries,by=c('playerID','yearID'))

# Analyzing the lost Players
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players <- subset(lost_players,yearID == 2001)
# picking coloumns of interest for lost players
lost_players <- lost_players[,c('playerID','H','2B','3B','HR','OBP','SLG','BA','AB')]
# Finding replacement players
library(dplyr)
avail.players <- filter(combo,yearID==2001)
#plot a quick scatterplot for OBP vs Salary to determine cut-off
library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()
# eleminate players with OBP == 0
avail.players <- filter(avail.players,salary<8000000,OBP>0)
# get players with AB >= 500
avail.players <- filter(avail.players,AB >= 500)
possible <- head(arrange(avail.players,desc(OBP)),10)
# potential replacements:
PotentialReplacements <- possible[,c('playerID','OBP','AB','salary')]
PotentialReplacements