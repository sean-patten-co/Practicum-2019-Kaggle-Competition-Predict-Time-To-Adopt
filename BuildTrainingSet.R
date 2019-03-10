#Training data set
#Create a data set about 2500 observations long

#set up paralell processing

library(dplyr)
library(corpus)
library(dplyr)
library(SnowballC)
library(doParallel)
library(OpenImageR)
library(foreach)

#end point - Dataset of pictures converted to data, dataset of description words, descriptions broken into individual 
#rows w/ pet ID on them, cleaned of stop words and finally the other data

library(OpenImageR)

#Import Data Set

main.data.set <- file.choose() # Run just me first
main.data.set <- read.csv(main.data.set)
main.data.set <- as.data.frame(main.data.set)

factor.columns <- c(1,4,5,7,8,9,10,11,12,13,14,15,18,24)
main.data.set[factor.columns] <- lapply(main.data.set[factor.columns], as.factor) #create factors for factor data
string.columns <- c(2,21)
main.data.set[string.columns] <- lapply(main.data.set[string.columns], as.character)


#Import picture names

library(stringr)
picture.source <- "C:/Users/Sean/Desktop/MSDS 692/train_images"
filenames <- list.files(picture.source)

#edit picture names to match pet ID

#select a sample of photos


###convert pictures to RBG data set###
#get photo, resize image 100x100
#add to data.frame


start_time <- Sys.time()
bigframe <- data.frame() #DF for storing
PetIDloopdf <- as.character()
picturesize <- 50

set.seed(Sys.time())

testsample <- sample(1:58311, 500) #kaggle did not provide a 'test' sample with AdoptionSpeed. So we need to build a test sample. 

trainsample <- c(1:58311)

trainsample <- trainsample[-testsample]



for(i in 1:10){
  
processframe <- data.frame() #DF for processing

set.seed(Sys.time())
photosample <- sample(trainsample, 250)

filenames <- list.files(picture.source)

filenames <- filenames[photosample]                #get a sample of photos from photosample

dashnumjpg <- "-\\d.jpg$"                          #remove trailing characters
PetID <- str_replace(filenames, dashnumjpg, "") #create list of matching pet ID's

dashnumjpg <- "-\\d\\d.jpg$"                      #remove trailing characters
PetID <- str_replace(PetID, dashnumjpg, "") #create list of matching pet ID's

###Load Pictures###



cl <- makeCluster(3)
registerDoParallel(cl)

x <- foreach (i = 1:length(filenames), .packages = 'OpenImageR') %do% {
  errorbool <- FALSE
  processfile <- readImage(paste(picture.source, filenames[i], sep = "/")) #Get a picture
  
  #image resizing
  errorbool <- tryCatch({processfile <- resizeImage(processfile, width = picturesize, height = picturesize, method = 'nearest')},
                        error = function(e){errorbool <- TRUE
                        return(errorbool)}, warning = function(w){errorbool <- FALSE
                        return(errorbool)}, finally = function(){return(errorbool)}
  ) #end of try catch
  
  if(class(errorbool)=="array") #if image resizing was successful process file into array
  {
    processfile <- svd(processfile)
    processfile <- processfile$u[1:(picturesize*picturesize*3)]
    processframe <- rbind(processframe, processfile)
  } else #this just makes it look neat and not throw a weird warning
  {
    print(cat("Error: Photo Not Processed ",PetID[i]," ",i)) #incase you were unaware
    PetID <- PetID[-i] #need to reduce the list of PetID
    print(Sys.time())
  }
  

  
}



####end get pictures

#store PetID as first column

processframe <- cbind(processframe, PetID)

colnames(processframe) <- c(as.character(1:(picturesize*picturesize*3)), "PetID")

bigframe <- rbind(bigframe, processframe)

}#end 5/4 loop

end_time <- Sys.time()

end_time - start_time

stopCluster(cl)

PetID <- bigframe$PetID

#text analysis
library(tidyr)
library(tidytext)
library(corpus)
library(Matrix)
#break out description

description.work <- data.frame(description = as.character(main.data.set$Description), PetID = main.data.set$PetID, stringsAsFactors = FALSE)

#create corpus of description words

corpus.desc <- tidytext::unnest_tokens(description.work, words, description, token = "words", format = "text", to_lower = TRUE)
colnames(corpus.desc) <- c("PetID", "word")
corpus.desc <- corpus.desc %>% anti_join(stop_words)

#need an english word dictionary to delete non-english words and other issues.
twooftwelve <- as.data.frame(read.csv("C:/Users/Sean/Desktop/MSDS 692/dictionaries/American/2of12.txt", sep="", stringsAsFactors=FALSE, header = FALSE))

colnames(twooftwelve) <- "word"

corpus.desc <- semi_join(corpus.desc, twooftwelve)

corpus.desc %>% count(word, sort = TRUE)

#analyze sentiment

sentiment <- read.csv("C:/Users/Sean/Desktop/MSDS 692/lexicons/lexiconcsv.csv")
colnames(sentiment) <- c("word", "positive", "negative", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
corpus.desc <- inner_join(corpus.desc, sentiment)
corpus.sentiment.mu <- mean(corpus.desc$positive)

#examine the sentiments of the descriptions

PetIDdf <- as.data.frame(PetID) #to build a sentiment data set we need to match the PetID's to the descriptions

colnames(PetIDdf) <- "PetID"

pet.desc.sample <- semi_join(corpus.desc, PetIDdf)

#remove the word column and sum the sentiment columns for each pet ID
pet.desc.sample <- select(pet.desc.sample, -word) %>% group_by(PetID) %>% summarise_all(funs(sum))


#we want to makes sure we keep our full sample. 
#When we join the sentiment data and the collection of words in corpus.desc
#some PetID's will be deleted because there is no description OR there are no words that match our sentiment sample.
#To adjust for this we add the PetID's back in with a zero for each descriptive field. 

pet.desc.sample <- full_join(pet.desc.sample, PetIDdf) #add back in the missing PetID's

pet.desc.sample[is.na(pet.desc.sample)] <- 0 # replace NA's with 0

#join the three  data sets

total.data.set <- inner_join(bigframe, main.data.set)

total.data.set <- full_join(pet.desc.sample, total.data.set, by = "PetID")

total.data.set <- distinct(total.data.set)

write.csv(total.data.set, "C:/Users/Sean/Desktop/MSDS 692/totaldataset2.csv") #saved to different file to prevent overwrite

end_time <- Sys.time()

end_time - start_time