##################Random Forest and Naive Bayes#############################
#Try forecasting data with RF and Naive Bayes

library(randomForest)
library(dplyr)
library(irr)


#Import data sets

#Read data from previously created CSV's 
test.data.set <- read.csv("F:/testdataset2.csv")
train.data.set <- read.csv("F:/totaldataset.csv")

#cleanse data of unusable data
#Breed cannot be used because the factor is too large for randomForest to use
#Train data set
droplist <- c("X","Description", "PetID", "RescuerID", "Name", "Breed1", "Breed2")
train.data.set <- train.data.set[complete.cases(train.data.set),c(1:12, 7513:7535)]
train.data.set <- train.data.set[, !(names(train.data.set) %in% droplist)]

#Test Data Set for text data only
droplist2 <- c("X", "Description", "PetID", "RescuerID", "Name", "Breed1", "Breed2")
test.data.set <- select(test.data.set, -c(droplist2))
test.data.set <- (test.data.set[complete.cases(test.data.set),7501:7528])
testAdptSped <- as.factor(test.data.set$AdoptionSpeed)

#Usable Type factors
colnames(test.data.set)[colnames(test.data.set)=="ï..Type"] <- "Type"
colnames(train.data.set)[colnames(train.data.set)=="ï..Type"] <- "Type"


#Imported data is all in NUM (int) form and needs to be updated to factor data as appropriate
name.set <- c("AdoptionSpeed", "Sterilized", "Color1", "Color2", "Color3", "Dewormed", "FurLength", "Gender", "Health", "Type", "MaturitySize", "State", "Vaccinated")

for(i in 1:length(name.set)) { j = name.set[i]

train.data.set[,j] <- as.factor(train.data.set[,j])
test.data.set[,j] <- as.factor(test.data.set[,j])
  
levels(test.data.set[,j]) <- levels(train.data.set[,j])
levels(train.data.set[,j]) <- levels(test.data.set[,j])
  
}

i #check for the right number of columns updated


#Random Forest, Text 
petrf <- randomForest(AdoptionSpeed ~., data=train.data.set)
petrf
summary(petrf)
importance(petrf)

RFText <- predict(petrf, test.data.set)

testout <- as.data.frame(cbind(RFText, testAdptSped))
outputRFTX <- as.numeric(ifelse(testout$RFText==testout$testAdptSped,1,0))
RFTXoutput <- sum(outputRFTX)/length(outputRFTX)
RFTXoutput

kappa2(cbind(RFText, testAdptSped))


#######################
#Reimport test and train data since our picture data has been eliminated

test.data.set <- read.csv("F:/testdataset2.csv")
train.data.set <- read.csv("F:/totaldataset.csv")

test.data.set <- test.data.set[,order(names(test.data.set))]
train.data.set <- train.data.set[,order(names(train.data.set))]
test.data.set <- test.data.set[complete.cases(test.data.set), c(1:7532)]
train.data.set <- train.data.set[complete.cases(train.data.set),c (1:7535)]
AdptSpd <- as.factor(train.data.set$AdoptionSpeed)
testAdptSped <- as.factor(test.data.set$AdoptionSpeed)


pic.test <- test.data.set[,33:7532]
pic.train <- train.data.set[,36:7535]


names(pic.test) <- names(pic.train) #make sure names match

#Run RF on picture data only

petrf2 <- randomForest(x=pic.train, y=AdptSpd, xtest=pic.test, ytest=testAdptSped)
petrf2
summary(petrf2)

RFPicts <- predict(petrf2, pic.test)
testout <- as.data.frame(cbind(RFPicts, testAdptSped))
outputRFPC <- ifelse(testout$testout==testout$testAdptSped,1,0)
RFPCoutput <- sum(outputRFPC)/length(outputRFPC)
RFPCoutput

kappa2(cbind(RFPicts, testAdptSped))

######Naive Bayes##################

library(e1071)

test.data.set <- read.csv("F:/testdataset2.csv")
train.data.set <- read.csv("F:/totaldataset.csv")

test.data.set <- test.data.set[,order(names(test.data.set))]
train.data.set <- train.data.set[,order(names(train.data.set))]
test.data.set <- test.data.set[complete.cases(test.data.set), c(1:7535)]
train.data.set <- train.data.set[complete.cases(train.data.set),c (1:7535)]

AdptSpd <- as.factor(train.data.set$AdoptionSpeed)
testAdptSped <- as.factor(test.data.set$AdoptionSpeed)

#text assemble

droplist3 <- c("X","Description", "PetID", "RescuerID", "Name", "AdoptionSpeed")

testtext.set <- test.data.set[,1:35]
AdptSpd.test.text <- as.factor(test.data.set$AdoptionSpeed)
testtext.set <- select(testtext.set, -c(droplist3))


                       
traintext.set <- train.data.set[,1:35]
AdptSpd.train.text <- as.factor(train.data.set$AdoptionSpeed)
traintext.set <- select(traintext.set, -c(droplist3))



test.data.set <- test.data.set[,36:7535]
train.data.set <- train.data.set[,36:7535]

#############Photo Data##################

colnames(test.data.set) <- names(train.data.set)

NBModel <- naiveBayes(train.data.set, AdptSpd)
NBPredict <- predict(NBModel, test.data.set)
table(NBPredict, AdptSpd)
summary(NBPredict)

testout <- as.data.frame(cbind(NBPredict, AdptSpd))
outputPDNB <- as.numeric(ifelse(testout$NBPredict==testout$AdptSpd,1,0))
PDNBoutput <- sum(outputPDNB)/length(outputPDNB)
PDNBoutput

#The output suggests that the photodata is not clear enough to make a contribution here

###############Text Data#######################

name.set <- c("Breed1", "Breed2", "Sterilized", "Color1", "Color2", "Color3", "Dewormed", "FurLength", "Gender", "Health", "Type", "MaturitySize", "State", "Vaccinated")

colnames(testtext.set)[colnames(testtext.set)=="ï..Type"] <- "Type"
colnames(traintext.set)[colnames(traintext.set)=="ï..Type"] <- "Type"

for(i in 1:length(name.set)) { j = name.set[i]

traintext.set[,j] <- as.factor(traintext.set[,j])
testtext.set[,j] <- as.factor(testtext.set[,j])

levels(testtext.set[,j]) <- levels(traintext.set[,j])
levels(traintext.set[,j]) <- levels(testtext.set[,j])

}

i



NBTModel <- naiveBayes(traintext.set, AdptSpd.train.text)
NBTPredict <- predict(NBTModel, testtext.set)
table(NBTPredict, AdptSpd.test.text)
summary(NBTPredict)

testout <- as.data.frame(cbind(NBTPredict, AdptSpd.test.text))
outputNBTD <- as.numeric(ifelse(testout$NBTPredict==testout$AdptSpd.test.text,1,0))
NBTDoutput <- sum(outputNBTD)/length(outputNBTD)
NBTDoutput


################altogether now##################

all.train.set <- cbind(traintext.set, train.data.set)
all.test.set <- cbind(testtext.set, test.data.set)

NBAllModel <- naiveBayes(all.train.set, AdptSpd)
NBAllPredict <- predict(NBAllModel, all.test.set)
table(NBAllPredict, all.test.set)
summary(NBAllPredict)

testout <- as.data.frame(cbind(NBAllPredict, AdptSpd))
outputNBAll <- as.numeric(ifelse(testout$NBAllPredict==testout$AdptSpd,1,0))
NBAlloutput <- sum(outputNBAll)/length(outputNBAll)
NBAlloutput

###kAPPA TESTS for Naive Bayes

kappa2(cbind(NBTPredict, AdptSpd.test.text))
kappa2(cbind(NBAllPredict, AdptSpd.test.text))
kappa2(cbind(NBPredict, AdptSpd.test.text))
