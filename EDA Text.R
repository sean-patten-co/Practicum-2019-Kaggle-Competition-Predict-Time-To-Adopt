#Run training data assembly file first. 

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(corrplot)
library(tidytext)



##############################Word Cloud Generator###################################################################################

#++++++++++++++++++++++++++++++++++
# rquery.wordcloud() : Word cloud generator
# - http://www.sthda.com
#+++++++++++++++++++++++++++++++++++
# x : character string (plain text, web url, txt file path)
# type : specify whether x is a plain text, a web page url or a file path
# lang : the language of the text
# excludeWords : a vector of words to exclude from the text
# textStemming : reduces words to their root form
# colorPalette : the name of color palette taken from RColorBrewer package, 
# or a color name, or a color code
# min.freq : words with frequency below min.freq will not be plotted
# max.words : Maximum number of words to be plotted. least frequent terms dropped
# value returned by the function : a list(tdm, freqTable)
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=3, max.words=200)
{ 
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer") 
  
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
            random.order=FALSE, rot.per=0.35, 
            use.r.layout=FALSE, colors=colors)
  
  invisible(list(tdm=tdm, freqTable = d))
}
#++++++++++++++++++++++
# Helper function
#++++++++++++++++++++++
# Download and parse webpage
html_to_text<-function(url){
  library(RCurl)
  library(XML)
  # download html
  html.doc <- getURL(url)  
  #convert to plain text
  doc = htmlParse(html.doc, asText=TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also don't want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
}
#References
#http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need#usage


#################################################################################################################

#need time to adopt (AdoptionSpeed) to be a numeric variable

#main.data.set <- file.choose() # Run just me first
#main.data.set <- read.csv(main.data.set)
#main.data.set <- as.data.frame(main.data.set)

total.data.set <- read.csv("C:/Users/Sean/Desktop/MSDS 692/totaldataset.csv")
main.data.set <- total.data.set

factor.columns <- c(1,4,5,7,8,9,10,11,12,13,14,15,18,24)
main.data.set[factor.columns] <- lapply(main.data.set[factor.columns], as.factor) #create factors for factor data
string.columns <- c(2,21)
main.data.set[string.columns] <- lapply(main.data.set[string.columns], as.character)

filepath.lexicon <- "C:/Users/Sean/Desktop/MSDS 692/lexicons/lexiconcsv.csv"
#filepath.lexicon <- "unknown"

#break out description

description.work <- data.frame(description = as.character(main.data.set$Description), PetID = main.data.set$PetID, stringsAsFactors = FALSE)

#create corpus of description words

corpus.desc <- tidytext::unnest_tokens(description.work, words, description, token = "words", format = "text", to_lower = TRUE)
colnames(corpus.desc) <- c("PetID", "word")

#To make the word clouds and graphics more meaningful we drop home adopt and adoption from the corpus
#Obviously these will be common words and this is the point of the website. 

stop_words <- rbind(stop_words, c("home", "sean"))
stop_words <- rbind(stop_words, c("adopt", "sean"))
stop_words <- rbind(stop_words, c("adoption", "sean"))

corpus.desc <- corpus.desc %>% anti_join(stop_words)

#need an english word dictionary to delete non-english words and other issues.
twooftwelve <- as.data.frame(read.csv("C:/Users/Sean/Desktop/MSDS 692/dictionaries/American/2of12.txt", sep="", stringsAsFactors=FALSE, header = FALSE))

colnames(twooftwelve) <- "word"

corpus.desc <- semi_join(corpus.desc, twooftwelve)

count.corpus.desc <- dplyr::count(corpus.desc, word, sort = TRUE)

ggplot(count.corpus.desc, aes(nn), stat="identity") + geom_histogram(show.legend = FALSE, bins = 50)

#most frequent words

topwords <- count.corpus.desc[1:15, 1:2]
topwords %>% ggplot(aes(x = reorder(word, nn), y = nn)) + geom_bar(stat = "identity")

rquery.wordcloud(corpus.desc)


#tf_idf analysis
PetID <- total.data.set$PetID

corpus.desc <- inner_join(corpus.desc, count.corpus.desc)
corpus.desc <- corpus.desc %>% bind_tf_idf(word, PetID, n)

#convert words into binary vector

count.corpus.desc.test <- count.corpus.desc

count.corpus.desc.test <- count.corpus.desc.test %>% mutate(value = 1, word = paste0(word)) %>% spread(word, value, fill = 0) #https://stackoverflow.com/questions/33990760/converting-factors-to-binary-in-r


topwords <- corpus.desc[order(corpus.desc$tf_idf),]
topwords <- topwords %>% top_n(10, tf_idf)
topwords %>% ggplot(aes(x=reorder(word, tf_idf), y=tf_idf)) + geom_bar(stat="identity")

####Sentiment analysis####

summary(pet.desc.sample)
tester <- summary(select(total.data.set, c("positive", "negative", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "AdoptionSpeed")))
cor(select(total.data.set, positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust, AdoptionSpeed), use = "complete.obs")
cov(select(total.data.set, positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust, AdoptionSpeed), use = "complete.obs")

tester.df <- data.frame()
tester.df <- c("Positive", "Negative", "Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust")
tester.dfc <- c(5.461, 2.054, 0.788, 1.936, .474, .915, 3.122, 1.03, .9114, 3.285)
tester.df <- as.data.frame(cbind(tester.df, tester.dfc))
colnames(tester.df) <- c("wordtype", "mean")
ggplot(tester.df, aes(wordtype, mean)) + geom_bar(aes(), stat="identity")

########################Other Data

colnames(total.data.set)[colnames(total.data.set)=="ï..Type"] <- "Type"
table(select(total.data.set, Type))

table(select(total.data.set, AdoptionSpeed))
