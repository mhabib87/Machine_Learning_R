# ReCreating a graph from the Economist magazine
# Corruption & human Development Graph
# Read Economist data csv file
library(readr)
library(ggplot2)
library(data.table)
df <- fread('~/Desktop/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Data Visualization Project/Economist_Assignment_Data.csv', drop=1)
head(df)
# visualization using ggplot2
pl <- ggplot(df, aes(x=CPI, y=HDI, color=Region)) + geom_point(size=4, shape =1)
# add a trend line (log reg)
pl2 <- pl + geom_smooth(aes(group=1), method = 'lm', formula = y~log(x),se=F, color='red')
#adding specific countries
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)
pl4 <- pl3 + scale_x_continuous(limits=c(.9,10.5), breaks=1:10)
print(pl4)