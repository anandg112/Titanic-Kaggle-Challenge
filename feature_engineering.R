#loading test & training data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#combining train & test dataset 
test$Survived <- NA
combined <- rbind(train, test)
combined$Name <- as.character(combined$Name) #casting Name into a text string


#splitting title from Name and applying across the entire dataset
combined$Title <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

#stripping off the space from title
combined$Title <- sub(' ', '', combined$Title)

#creating a table for titles
table(combined$Title)

#combining obscure titles with common ones
combined$Title[combined$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined$Title[combined$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined$Title[combined$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#changing the variable type back into a factor
combined$Title <- factor(combined$Title)

#creating a new feature - family size
combined$FamilySize <- combined$SibSp + combined$Parch + 1

#combining the surname with family size
combined$Surname <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined$FamilyID <- paste(as.character(combined$FamilySize), combined$Surname, sep="")

#any family with a size of 2 or less is "small"
combined$FamilyID[combined$FamilySize <= 2] <- 'small'

table(combined$FamilyID)
famIDs <- data.frame(table(combined$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

#converting all family sizes of 2 or less to "small" as the previous attempt left some of them
combined$FamilyID[combined$FamilyID %in% famIDs$var1] <- 'small'
combined$FamilyID <- factor(combined$FamilyID)

#splitting dataset into training & test set
train <- combined[1:891,]
test <- combined[892:1309,]

#creating a decision tree model
library(rpart)
fit <-  rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data=train, method="class")

plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

#predicting on test data set
Prediction <- predict(fit,test, type="class")
submit4 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit4, file = "feature_engineered_Decision_tree_model", row.names = FALSE)