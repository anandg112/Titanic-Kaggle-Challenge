#loading libraries
library(party)
library(rpart)
library(randomForest)


#loading test & training data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#combining train & test dataset with row bind
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

#replacing NA values in Age column with a ANOVA decision tree model
Agefit <- rpart(Age ~  Pclass + Sex + SibSp + Parch + Fare + Embarked +  Title + FamilySize, data=combined[!is.na(combined$Age),], method="anova")
combined$Age[is.na(combined$Age)] <- predict(Agefit, combined[is.na(combined$Age),])

summary(combined)

#replacing blank values in Embarked column
which(combined$Embarked == '')
combined$Embarked[c(62,830)] = 'S'
combined$Embarked <- factor(combined$Embarked)

#replacing NA value in fare column
summary(combined$Fare)
which(is.na(combined$Fare))
combined$Fare[1044] <- median(combined$Fare, na.rm=TRUE)

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

#Random Forest in R can only digest factors with up to 32 levels so we need to reduce the FamilyID factor
combined$FamilyID2 <- combined$FamilyID
combined$FamilyID2 <- as.character(combined$FamilyID2)
combined$FamilyID2[combined$FamilySize <= 3] <- 'Small'
combined$FamilyID2 <- factor(combined$FamilyID2)

#splitting the dataset and building ensemble model and predicting on test set
train <- combined[1:891,]
test <- combined[892:1309,]


set.seed(100)
fit <- randomForest(as.factor(Survived) ~ Pclass +  Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data=train, importance=TRUE, nTree=2000)
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

#building a random forest model based on conditional inference tree

set.seed(100)
fit2 <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction2 <- predict(fit2, test, OOB=TRUE, type = "response")
submit2 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction2)
write.csv(submit2, file = "conditional_inference_tree.csv", row.names = FALSE)