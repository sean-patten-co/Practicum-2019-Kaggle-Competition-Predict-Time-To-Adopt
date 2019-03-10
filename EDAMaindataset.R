library(dplyr)
library(plyr)
library(ggplot2)
library(sqldf)
library(corrplot)

###Load data set 

main.data.set <- read.csv("F:/totaldataset.csv")
main.data.set <- main.data.set[,order(names(main.data.set))]

###Set up data set by dropping unneeded columns and cleaning up 

mds.analysis <- select(main.data.set, -Name, -RescuerID, -Description)
colnames(mds.analysis)[colnames(mds.analysis)=="ï..Type"] <- "Type"
mds.analysis$Gender <- as.factor(mds.analysis$Gender)
mds.analysis %>% group_by(Vaccinated) %>% summarize(Vacc = length(Vaccinated))


### Define factor data 
name.set <- c("AdoptionSpeed", "Sterilized", "Dewormed", "FurLength", "Gender", "Health", "Type", "MaturitySize","Vaccinated", "State", "Color1", "Color2", "Color3", "Breed1", "Breed2")

for(i in 1:length(name.set)) { j = name.set[i]

mds.analysis[,j] <- as.factor(mds.analysis[,j])

}

#Update levels for Vaccinated Dewormed and sterilized

levels(mds.analysis$Vaccinated) <- c("Yes", "No", "Maybe")
levels(mds.analysis$Dewormed) <-  c("Yes D","No","Maybe")
levels(mds.analysis$Sterilized) <- c("Yes S", "No","Maybe")

#Establish baseline adoption speed distribution

ggplot(mds.analysis, aes(AdoptionSpeed)) + geom_bar()

#Check for relationship between Vaccinated Sterlized Dewormed and Adoption speed

ggplot(mds.analysis, aes(Vaccinated, AdoptionSpeed)) + geom_jitter()
ggplot(mds.analysis, aes(Sterilized, AdoptionSpeed)) + geom_jitter()
ggplot(mds.analysis, aes(Dewormed, AdoptionSpeed)) + geom_jitter()

#check relationship between adoption speed and age

chet <- aov(Age~AdoptionSpeed, data=mds.analysis)
summary(chet)

ggplot(mds.analysis, aes(AdoptionSpeed, Age)) + geom_boxplot()

# Maturity Size analysis 

table(mds.analysis$MaturitySize, mds.analysis$AdoptionSpeed)

ggplot(mds.analysis, aes(AdoptionSpeed, MaturitySize)) + geom_jitter()
ggplot(mds.analysis, aes(MaturitySize)) + geom_bar()

# Photo Amount Analysis

table(mds.analysis$PhotoAmt, mds.analysis$AdoptionSpeed)

ggplot(mds.analysis, aes(PhotoAmt)) + geom_bar()

main.data.set <- main.data.set[,order(names(main.data.set))]

#Type Analysis
#It is more likely that a cat will be adopted on the same day or in the first week

table(mds.analysis$Type, mds.analysis$AdoptionSpeed)

table(mds.analysis$Type, mds.analysis$MaturitySize)
