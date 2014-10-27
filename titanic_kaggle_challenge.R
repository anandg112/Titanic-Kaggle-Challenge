#loading test & training data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#exploring the dataset
str(train)
table(train$Survived)
prop.table(table(train$Survived))

#first prediction, assuming everyone dies on the ship
test$Survived <- rep(0,418)

#creating a new csv file with PassengerID & survived prediction
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file="naivemodel.csv", row.names=FALSE)

#more data exploration
summary(train$Sex) #majority of passengers were male
prop.table(table(train$Sex, train$Survived),1) #proportion of male/female who survived
summary(train$Age) #Median age was 28

#gender-based model
test$Survived = 0
test$Survived[test$Sex == "female"] <- 1

#creating new variable 'child'
train$Child <- 0
train$Child[train$Age < 18] <- 1

#seeing survival proportions with new variable 'child'
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#exploring "fare" and "class" variables & binning the fares into 3 categories
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#seeing survival proportions with new "Fare" and "class" variable
aggregate(Survived ~ Fare2 + Pclass +Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#new prediction based on gender-class model
test$Survived <- 0 
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3  & test$Fare >=20] <- 0

# .csv prediction file for gender-based model
submit1 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit1, file="gender_class_model.csv", row.names=FALSE)


#decision tree model
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

#predicting on test data set
Prediction <- predict(fit,test, type="class")
submit3 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit3, file = "Decision_tree_model", row.names = FALSE)