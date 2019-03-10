#Sean Patten MSDS 692 Code

#set up paralell processing
#Create a test Data Set 

library(dplyr)
library(corpus)

#end point - Dataset of pictures converted to data, dataset of description words, descriptions broken into individual 
#rows w/ pet ID on them, cleaned of stop words and finally the other data

library(OpenImageR)

#Import Data Set

main.test.set <- file.choose()
main.test.set <- read.csv(main.test.set)
main.test.set <- as.data.frame(main.test.set)

factor.columns <- c(1,4,5,7,8,9,10,11,12,13,14,15,18,24)
main.test.set[factor.columns] <- lapply(main.test.set[factor.columns], as.factor) #create factors for factor data
string.columns <- c(2,21)
main.test.set[string.columns] <- lapply(main.test.set[string.columns], as.character)


#Import picture names

library(stringr)
picture.source <- "C:/Users/Sean/Desktop/MSDS 692/train_images"
filenames.test <- list.files(picture.source)

#edit picture names to match pet ID 

#select a sample of photos
sample.size <- length(testsample)
photosample <- testsample
picturesize <- 50

###convert pictures to RBG data set###
#get photo, resize image 100x100
#add to data.frame


start_time <- Sys.time()

start_time

testbigframe <- data.frame()
processtestframe <- data.frame()

filenames.test <- list.files(picture.source)

filenames.test <- filenames.test[photosample]                #get a sample of photos from photosample

PetID <- c()

dashnumjpg <- "-\\d.jpg$"                          #remove trailing characters
PetID <- str_replace(filenames.test, dashnumjpg, "") #create list of matching pet ID's

dashnumjpg <- "-\\d\\d.jpg$"                      #remove trailing characters
PetID <- str_replace(PetID, dashnumjpg, "") #create list of matching pet ID's

###Load Pictures###

library(dplyr)
library(SnowballC)
library(doParallel)
library(foreach)
library(OpenImageR)


cl <- makeCluster(3)
registerDoParallel(cl)
photoresize <- picturesize

start_time <- Sys.time()



x <- foreach (i = 1:length(filenames.test), .packages = 'OpenImageR') %do% {
  errorbooltest <- FALSE
  processfiletest <- readImage(paste(picture.source, filenames.test[i], sep = "/")) #Get a picture
  
  #image resizing
  errorbooltest <- tryCatch({processfiletest <- resizeImage(processfiletest, width = photoresize, height = photoresize, method = 'nearest')},
                        error = function(e){errorbooltest <- TRUE
                        return(errorbooltest)}, warning = function(w){errorbooltest <- FALSE
                        return(errorbooltest)}, finally = function(){return(errorbooltest)}
  ) #end of try catch
  
  if(class(errorbooltest)=="array") #if image resizing was successful process file into array
  {
    processfiletest <- svd(processfiletest)
    processfiletest <- processfiletest$u[1:(photoresize*photoresize*3)]
    processtestframe <- rbind(processtestframe, processfiletest)
  } else #this just makes it look neat and not throw a weird warning
  {
    print(cat("Error: Photo Not Processed ",PetID[i]," ",i)) #Gives idea of what photo did not process
    PetID <- PetID[-i] #need to reduce the list of PetID
    print(Sys.time())
  }
  
}

if(nrow(processtestframe) != length(PetID)) PetID <- PetID[-length(PetID)]

colnames(processtestframe) <- c(as.character(1:(picturesize*picturesize*3)), "PetID")

testbigframe <- processtestframe

stopCluster(cl)

end_time <- Sys.time()

end_time - start_time

####end get pictures

#store PetID as second column



testbigframe <- cbind(testbigframe, PetID)

#text analysis
library(tidyr)
library(tidytext)
library(corpus)
library(Matrix)
#break out description

description.work.test <- data.frame(description = as.character(main.test.set$Description), PetID = main.test.set$PetID, stringsAsFactors = FALSE)

#create corpus of description words

corpus.desc.test <- tidytext::unnest_tokens(description.work.test, words, description, token = "words", format = "text", to_lower = TRUE)
colnames(corpus.desc.test) <- c("PetID", "word")
corpus.desc.test <- corpus.desc.test %>% anti_join(stop_words)

#need an english word dictionary to delete non-english words and other issues.
twooftwelve <- as.data.frame(read.csv("C:/Users/Sean/Desktop/MSDS 692/dictionaries/American/2of12.txt", sep="", stringsAsFactors=FALSE, header = FALSE))

colnames(twooftwelve) <- "word"

#stemming

corpus.desc.test <- semi_join(corpus.desc.test, twooftwelve)

corpus.desc.test %>% count(word, sort = TRUE)

#analyze sentiment

sentiment <- read.csv("C:/Users/Sean/Desktop/MSDS 692/lexicons/lexiconcsv.csv")

#conversion of the header for the sentiment file did not come through correctly, fixing this problem
colnames(sentiment) <- c("word", "positive", "negative", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust") 
corpus.desc.test <- inner_join(corpus.desc.test, sentiment)

#examine the sentiments of the descriptions

PetIDtestdf <- as.data.frame(PetID) #to build a sentiment data set we need to match the PetID's to the descriptions

colnames(PetIDtestdf) <- "PetID"

test.desc.sample <- full_join(PetIDtestdf, corpus.desc.test)

#remove the word column and sum the sentiment columns for each pet ID
test.desc.sample <- select(test.desc.sample, -word) %>% group_by(PetID) %>% summarise_all(funs(sum))


#we want to makes sure we keep our full sample. 
#When we join the sentiment data and the collection of words in corpus.desc
#some PetID's will be deleted because there is no description OR there are no words that match our sentiment sample.
#To adjust for this we add the PetID's back in with a zero for each descriptive field. 

test.desc.sample <- inner_join(test.desc.sample, PetIDtestdf) #add back in the missing PetID's

test.desc.sample[is.na(test.desc.sample)] <- 0 # replace NA's with 0

#join the three  data sets

test.data.set <- inner_join(testbigframe, main.test.set, by = "PetID")

test.data.set <- full_join(test.data.set, test.desc.sample)

test.data.set <- distinct(test.data.set)

write.csv(test.data.set, file = "C:/Users/Sean/Desktop/MSDS 692/testdataset3.csv")
