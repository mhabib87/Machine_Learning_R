# importing Titanic Training data
df.train <- read.csv('~/Desktop/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_train.csv')
head(df.train)
##Data Visualization##
str(df.train)
# Checking missing data
install.packages('Amelia')
library(Amelia)
help("missmap")
# visual representation of missing data
missmap(df.train, main='Missing Map',col=c('yellow','black'), legend = FALSE)
library(ggplot2)
# Survived (1) vs. Dead (0)
ggplot(df.train, aes(Survived)) + geom_bar()
# Count number per class
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))
#Count men vs. women onboard
ggplot(df.train, aes(Sex)) + geom_bar(aes(fill=factor(Sex)))
# ages on board
ggplot(df.train,aes(Age))+ geom_histogram()
# Siblings & spouse vs single on board
ggplot(df.train,aes(SibSp)) + geom_bar()
#
ggplot(df.train, aes(Fare)) + geom_histogram(fill="blue", color='white', alpha=0.5)
## Data Cleaning##
#Filling age by passenger class
pl <-ggplot(df.train, aes(Pclass, Age))
pl <- pl + geom_boxplot(aes(group=Pclass, fill=factor(Pclass)))+scale_y_continuous(breaks=seq(min(0), max(80),by=2))
pl

# Fill in age function
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}
fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages

  ## applying fill in age function to age column
missmap(df.train, main= 'Imputation Check', col=c('yellow','black'))
# getting rid of column we won't need
library(dplyr)
head(df.train)
df.train <- select(df.train,-PassengerId, -Ticket,-Cabin)
head(df.train)

str(df.train)

#convert to factor
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)
str(df.train)
#building a moded

summary(log.model)
##### split data#####
library(caTools)
set.seed(101)
split <-sample.split(df.train$Survived, SplitRatio = 0.7)
final.train <- subset(df.train, split==TRUE)
final.train <- subset(df.train, split=FALSE)

final.log.model <- glm(Survived ~ . , family = binomial(link = 'logit'),data=final.train)