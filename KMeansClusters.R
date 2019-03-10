###########KMEANS CLUSTERING FOR PHOTOGRAPHS###############
#See if clustering photographs using Kmeans will give us better accuracy in any of the methods

library(ggplot2)
library(dplyr)
library(randomForest)
library(class)
library(clue)
library(nnet)
library(irr)
library(class)
library(e1071)
library(neuralnet)

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

vtime <- kmeans(train.data.set[,29:7528], 10)
vtimeclstr <- as.data.frame(vtime$cluster)
ggplot(vtimeclstr, aes(vtimeclstr$`vtime$cluster`)) + geom_bar()

vtime.pred <- as.factor(cl_predict(vtime, test.data.set[,29:7528]))
table(vtime.pred)

traindata.block<- cbind(pictype = as.factor(vtimeclstr$`vtime$cluster`), train.data.set[,2:28])

testdata.block <- cbind(pictype = as.factor(vtime.pred), test.data.set[,2:28])

levels(testdata.block$pictype) <- levels(traindata.block$pictype)

###########R Forest

RFCLSTR <- randomForest(traindata.block, train.adoptspeed, ntree = 1000)
RFCLSTR.pred <- predict(RFCLSTR, testdata.block)
RFCLSTR.df <- cbind(RFCLSTR.pred, test.adoptspeed)
agree(RFCLSTR.df)
kappa2(RFCLSTR.df)
importance(RFCLSTR)

###########Naive Bayes

RFCLSTR.NB <- naiveBayes(traindata.block, train.adoptspeed)
RFCLSTR.NB.pred <- predict(RFCLSTR.NB, testdata.block)
RFCLSTR.NBDF <- cbind(RFCLSTR.NB.pred, test.adoptspeed)
agree(RFCLSTR.NBDF)
kappa2(RFCLSTR.NBDF)

###########KNN


RFCLSTR.KNN<-knn(traindata.block, testdata.block, cl = train.adoptspeed, k = 1)
RFCLSTR.KNNDF <- cbind(RFCLSTR.KNN, test.adoptspeed)
agree(RFCLSTR.KNNDF)
kappa2(RFCLSTR.KNNDF)

##########Mixed Modeling

MMKNN <- knn(traindata.block, traindata.block, cl = train.adoptspeed, k = 1)
MM.DATA <- cbind(MMKNN, traindata.block)
MMRF <- randomForest(MM.DATA, train.adoptspeed, ntree = 1000)
testdata.block <- cbind(testdata.block, MMKNN = RFCLSTR.KNN)
MMRF.PRED <- predict(MMRF, testdata.block)
MMRF.DF <- cbind(test.adoptspeed, MMRF.PRED)
agree(MMRF.DF)
kappa2(MMRF.DF)
importance(MMRF)

########Neural Net

#create formula for NN 

#Create formula for model.matrix
formula.names <- names(traindata.block)  #https://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html/2
formula.names <- names(traindata.block)  #https://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html/2
formula.names <- paste0(formula.names, collapse = ' + ')
formula.names <- paste0(" ~ ", formula.names)

use.traindata <- as.data.frame(model.matrix(formula(formula.names), data=traindata.block))
use.traindata <- use.traindata[,-1]

#create formula for NN

formula.names <- c()
formula.names <- names(use.traindata)  #https://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html/2
formula.names <- paste0(formula.names, collapse = ' + ')
formula.names <- paste0(" ~ ", formula.names)

testform <- paste0("train.adoptspeed", formula.names)

use.traindata.block <- cbind(use.traindata, train.adoptspeed)


#run nn #Neural Net does not work for this data due to errors when calculating. 

#NNPicPets <- neuralnet(testform, use.traindata.block, hidden = 10, lifesign = 'full', stepmax = 250000, lifesign.step = 10000, rep = 10)
