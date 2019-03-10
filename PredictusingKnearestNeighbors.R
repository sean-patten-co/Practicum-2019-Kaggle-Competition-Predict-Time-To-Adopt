####K nearest neighbors####
#The RF approach yielded the best results so far, being able to correctly classify data more than half the time.
#K-Nearest Neighbors is a statistical approach classifying based on the data found in each classification
#
library(ggplot2)
library(dplyr)
library(randomForest)
library(class)
library(clue)
library(nnet)
library(irr)

#build the dataset from the saved test and train set created earlier
#update the column names to something typable
#divide the data by photograph and non photograph data
#drop unuseable data
#create factor vectors 

sttestdata <- read.csv("F:/testsamp.csv")

test.data.set <- read.csv("F:/testdataset2.csv")
train.data.set <- read.csv("F:/totaldataset.csv")

colnames(test.data.set)[colnames(test.data.set)=="ï..Type"] <- "Type"
colnames(train.data.set)[colnames(train.data.set)=="ï..Type"] <- "Type"

test.data.set <- test.data.set[,order(names(test.data.set))]
train.data.set <- train.data.set[,order(names(train.data.set))]
test.data.set <- test.data.set[complete.cases(test.data.set),]
train.data.set <- train.data.set[complete.cases(train.data.set),]

droplist3 <- c("X","Description", "PetID", "RescuerID", "Name", "Breed1", "Breed2")
train.adoptspeed <- as.factor(train.data.set$AdoptionSpeed)
test.adoptspeed <- as.factor(test.data.set$AdoptionSpeed)
train.data.set <- select(train.data.set, -c(droplist3))
test.data.set <- select(test.data.set, -c(droplist3))

#name.set <- c("AdoptionSpeed", "Sterilized", "Dewormed", "FurLength", "Gender", "Health", "Type", "MaturitySize", "Vaccinated", "Breed1", "Breed2", "State", "Color1", "Color2", "Color3")

name.set <- c("AdoptionSpeed", "Sterilized", "Dewormed", "FurLength", "Gender", "Health", "Type", "MaturitySize","Vaccinated", "State", "Color1", "Color2", "Color3")

for(i in 1:length(name.set)) { j = name.set[i]

train.data.set[,j] <- as.factor(train.data.set[,j])
test.data.set[,j] <- as.factor(test.data.set[,j])

levels(test.data.set[,j]) <- levels(train.data.set[,j])
levels(train.data.set[,j]) <- levels(test.data.set[,j])

}

####How about Nearest Neigbors###

#will photographs work w/KNN? 

AdoptionSpeed <- train.data.set$AdoptionSpeed
knntrain.set <- train.data.set[,29:7528]
AdoptionSpeed.test <- test.data.set$AdoptionSpeed
knntest.set <- test.data.set[,29:7528]

knnstep <- knn(knntrain.set, knntest.set, cl = AdoptionSpeed, k = 5)
KNNSTEP.DF <- as.data.frame(cbind(knnstep, AdoptionSpeed.test))
agree(KNNSTEP.DF)
kappa2(KNNSTEP.DF)

#####text only####

#How will only text work with KNN

text.trainset <- select(train.data.set, c(2:24))
text.testset <- select(test.data.set, c(2:24))

knnstep.text <- knnstep <- knn(text.trainset, text.testset, cl = AdoptionSpeed, k = 1)

testout <- as.data.frame(cbind(knnstep.text, AdoptionSpeed.test))
KNNSTEP.DFT <- cbind(knnstep.text, AdoptionSpeed.test)
agree(KNNSTEP.DFT)
kappa2(KNNSTEP.DFT)