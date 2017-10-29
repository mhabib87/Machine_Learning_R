# Obtaining Data
df.train <- read.csv('~/Desktop/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_train.csv')
head(df.train)
# Visualizing missing data
library(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
# Exploratory Data#
library(ggplot2)
# Dead(0) vs Survived(1)
ggplot(df.train,aes(Survived)) + geom_bar()
## Count number per class
ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)),alpha=0.5)
# Male vs Female onboard
ggplot(df.train,aes(Sex)) + geom_bar(aes(fill=factor(Sex)),alpha=0.5)
## ages on board
ggplot(df.train,aes(Age)) + geom_histogram(fill='blue',bins=20,alpha=0.5)
# Siblings & spouse vs single on board
ggplot(df.train,aes(SibSp)) + geom_bar(fill='red',alpha=0.5)
# fare price count
ggplot(df.train,aes(Fare)) + geom_histogram(fill='green',color='black',alpha=0.5)
# Median of age per class
pl <- ggplot(df.train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) 
pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2))
# Data Cleaning#
# filling in missing ages by repalcing NA values with median age per class


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
# Applying imputation function on missing data
fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
str(df.train)

head(df.train,3)
# getting rid of columns we won't use
library(dplyr)
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
head(df.train,3)
# Changing int to factor
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)
# building logistic model
#log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = df.train)
#summary(log.model)





# Predicting survival
######### Splitting original (train data) to  train and Test data#########
library(caTools)
set.seed(101)
split <-sample.split(df.train$Survived, SplitRatio = 0.7)
final.train = subset(df.train, split == TRUE)
final.test = subset(df.train, split == FALSE)

final.log.model <- glm(formula = Survived ~ ., family = binomial(link='logit'), data=final.train)
summary(final.log.model)
 #Testing our model
fitted.probabilites <- predict(final.log.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilites >0.5,1,0)
misClassError <- mean(fitted.results != final.test$Survived)
 #Accuracy
print (1-misClassError)

# Confusion Matrix
table(final.test$Survived, fitted.probabilites>0.5)







#Read Test.csv file
#library(readr)
#titanic_test <- read_csv("~/Desktop/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_test.csv")
# eleminate unwanted columns
#titanic_test <- select(titanic_test,-PassengerId,-Name,-Ticket,-Cabin)
#head(titanic_test)

#missmap(titanic_test, main="Titanic Test Data - Missings Map", 
      #  col=c("yellow", "black"), legend=FALSE)

# Applying imputation function on missing data
#fixed_test_ages <- impute_age(titanic_test$Age,titanic_test$Pclass)
#titanic_test$Age <- fixed_test_ages

#Mean_Fare <- mean(titanic_test$Fare,na.rm=TRUE)
#titanic_test$Fare[is.na(titanic_test$Fare)] <-mean(titanic_test$Fare,na.rm=TRUE)


# checking our imputation application
#missmap(titanic_test, main="Titanic Test Data - Missings Map", 
      #  col=c("yellow", "black"), legend=FALSE)
# Changin INT to factor

#titanic_test$Survived <- factor(titanic_test$Survived)
#titanic_test$Pclass <- factor(titanic_test$Pclass)
#titanic_test$Parch <- factor(titanic_test$Parch)
#titanic_test$SibSp <- factor(titanic_test$SibSp)
#str(titanic_test)
#head(titanic_test)
#head(df.train)
#str(titanic_test$Parch)
#predicting model 
#fitted.prob <- predict(log.model, titanic_test, type = 'response')
#fitted.results <- ifelse(fitted.prob >0.5,1,0)
#misClassError <- mean(fitted.results != df.train$Survived)
# Model Accuracy
#print (1-misClassError)

## Confusion Matrix
#table(df.train$Survived, fitted.prob>0.5)



