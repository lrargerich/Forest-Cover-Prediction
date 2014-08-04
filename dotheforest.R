library("randomForest")
library("class")
library (autoencoder)
library("e1071")
library("neuralnet")
library("gbm")
library("kernlab")
library("party")

train<-read.csv("train.csv")
test<-read.csv("test.csv")

cover_type<-as.factor(train$Cover_Type)
id<-test$Id

train$Cover_Type<-NULL

combi <- rbind(train, test)

combi$Soil_Type7<-NULL
combi$Soil_Type15<-NULL
combi$Id<-NULL

# FEATURE ENGINEERING HERE IF NEEDED
#combi$Horizontal_Distance_To_Hydrology_Large <- as.numeric(as.factor(combi$Horizontal_Distance_To_Hydrology > 600))
#combi$Horizontal_Distance_To_Hydrology[combi$Horizontal_Distance_To_Hydrology>600] <- 600
cols <- names(combi)[11:52]
cols1 <- names(combi)[c(1:10)]


train <- combi[1:15120,]
test <- combi[15121:581012,]

# Encode all sparse features into 8 new features
# Do the Dance
source("auto_enc.R")
train <- train2
test <- test2

###########################################################

label <- cover_type

# Re combi
combi <- rbind(train,test)
combis <- scale(combi)
trains <- as.data.frame(combis[1:15120,])
tests <- as.data.frame(combis[15121:581012,])

## Now do the Random Forest First
clf <- randomForest(train, as.factor(cover_type), ntree=500, mtry=8, importance=TRUE)

varImpPlot(clf)

clf

result=predict(clf, test)
submit <- data.frame(Id = id, Cover_Type = result)
write.csv(submit, file = "basic2_random_forest.csv", row.names = FALSE)

# Try with a C5.0 Tree (not good)
cfit <- C5.0(train,as.factor(cover_type))
result <- predict(cfit,test)
submit <- data.frame(Id = id, Cover_Type = result)
write.csv(submit, file = "c50_forest.csv", row.names = FALSE)
