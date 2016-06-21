##############################################
# File: 	EDA_BA_2.0.R
# Author: 	Brett Amdur
# Project: 	Home Depot Kaggle Competition
# Purpose: 	Feature Engineering
# Notes: 	This file contains the primary functionality 
#			associated with feature engineering
##############################################



##############################################
######## READ IN DATA ########################
##############################################
library(tm)
library(dplyr)
library(ggplot2)
library(stringr)
source('./typeSpecific.R')

# choices are 'train', 'test', 'origTrain', 'origTest'
buildType <- 'train'
buildNumber <- '4.0'

train <- buildData(buildType, buildNumber)
# train <- train[, c('id', 'product_uid', 'product_title', 'search_term', 'product_description')]


###############################################
######## PROCESSING ###########################
###############################################


# To work with a random subset of observations:
# indexes <- sample(1:nrow(train), 1000, replace=FALSE)
# trainSub <- train[indexes, ]

# For working with the entire training set:
trainSub <- train 

# use tm package for NLP work
trainCorp <- Corpus(VectorSource(trainSub))

#### TEXT CLEAN UP FUNCTIONS ####

# Not executing text cleam up functions on data received from text cleanup 
# processes run before handing data off to me for feature engineering.  These
# are here for exploratory work on data not received from text cleanup functions
# comnpleted by team members.

#trainCorp <- tm_map(trainCorp, content_transformer(tolower))
#trainCorp <- tm_map(trainCorp, stemDocument, language = "english")
#train.corpus <- tm_map(trainCorp, removePunctuation)
# doc.corpus <- tm_map(doc.corpus, removeNumbers)
#trainCorp <- tm_map(trainCorp, removeWords, stopwords("english"))

# STRUCTURE OF THE TM OBJECT :
# trainCorp: VCorpus of 5 documents (each column from train)
# 	VCorpus of one Document (list of one)
# 		PlainTextDocument of two elements (list of 2)
# 			$content -- content of the row from train
# 			$meta -- metadata (not sure where this comes from)

trainDF <- data.frame(trainCorp[[1]]$content, trainCorp[[2]]$content, 
					  trainCorp[[3]]$content, trainCorp[[4]]$content, 
					  trainCorp[[5]]$content
					  )
names(trainDF) <- names(train)

# Data type manipulation
trainDF$product_title <- as.character(trainDF$product_title)
trainDF$search_term <- as.character(trainDF$search_term)
trainDF$product_description <- as.character(trainDF$product_description)



########################################################
######## FEATURE ENGINEERING ###########################
########################################################

# Matchcount is used frequently throughout this system to identify search_term / 
# target matches.  It takes needles and haystack, and returns a) how many
# needles are found in haystack, and b) list of needles found in haystack.
matchCount <- function(needles, haystack){
	count <- 0
	needleWords <- unlist(strsplit(needles, split = " "))
	haystackWords <- strsplit(haystack, split = " ")
	wordCount <- 0
	matchedWords <- NULL
	for(i in 1:length(needleWords)){
		wordCount <- wordCount + length(grep(needleWords[i], haystack))
		if(is.na(needleWords[i])){break()}
		if(grepl(needleWords[i], haystack)){
			matchedWords <- c(matchedWords, needleWords[i])
		}
	}
	return(list(wordCount, matchedWords))
}

# Add columns related to TITLE match counts
trainDF <- rowwise(trainDF) %>% mutate(. , 
									   # number of words in search_term that match target:
									   matchCount = matchCount(search_term, product_title)[[1]] ,
									   # word length of search term:
  									 search_term_count = length(strsplit(search_term,' ')[[1]]),
 									   # word length of target:
 									   product_title_count = length(strsplit(product_title,' ')[[1]]),
 									   # percentage of search terms that matched words in target:
 									   stHitRatio = matchCount / search_term_count,
 									   # percentage of target terms that matched words in search_term:
 									   titleHitRatio = matchCount / product_title_count,
 									   # list of words in search_term that matched words in target:
 									   matchedWords = list(matchCount(search_term, product_title)[[2]])
									   )

# Add columns related to DESCRIPTION match counts
trainDF <- rowwise(trainDF) %>% mutate(. , 
									   # number of words in search_term that match target:
									   matchCount_desc = matchCount(search_term, product_description)[[1]] ,
									   # word length of target:
									   product_description_count = length(strsplit(product_description,' ')[[1]]),
									   # percentage of search terms that matched words in target:
									   stHitRatio_desc = matchCount_desc / search_term_count,
									   # percentage of target terms that matched words in search_term:
									   descHitRatio = matchCount_desc / product_description_count,
									   # list of words in search_term that matched words in target:
									   matchedWords_desc = list(matchCount(search_term, product_description)[[2]])
)

# build the brand matching
trainDF <-buildBrand(trainDF, buildType, buildNumber)

# bring relevance back into trainDF
# DON'T DO THIS FOR TEST
x <- read.csv('../data/trainChris_3.0.csv', 
			  stringsAsFactors = FALSE)
trainDF$relevance <- as.numeric(x$relevance)


##############################################################
#### WORD POWER SCORES #######################################
##############################################################
## TAKES ABOUT 10 MINUTES TO RUN ###

# Note that these are just run on product_titles, not descriptions

# START HERE FOR TRAIN
# Create lookup table for searched words
searchCorpus <- VCorpus(VectorSource(trainDF$search_term))
# only include words >= 3 characters, and >= 5 matches
dtm <- DocumentTermMatrix(searchCorpus, control=list(wordLengths=c(3, Inf), 
                                                     bounds = list(global = c(5,Inf))))
searchWordsDTM <- inspect(dtm)
# searchWordsDTM$relevance <- trainDF$relevance
searchWordsDTM <- cbind(searchWordsDTM, trainDF$relevance)

# pre-create the wordlookup table, to avoid the need to rbind
wordPowersTrain <- data.frame(word = character(ncol(searchWordsDTM)-1), 
                              relScore = numeric(ncol(searchWordsDTM)-1),
                              stringsAsFactors = FALSE)

# this calculates total relevance score for each word searched and builds the lookup df
for(i in 1:ncol(searchWordsDTM)-1){ # remember, the columns in the dtm are the matched words
  x <- which(as.logical(searchWordsDTM[ , i])) # x is the index value of all doc 
  # (a row in traindf) matches
  wordPowersTrain$word[i] <- colnames(searchWordsDTM)[i] # insert the word into lookup df
  wordPowersTrain$relScore[i] <- mean(searchWordsDTM[x, ncol(searchWordsDTM)])  # insert the avg. rel score for that word 
  # into the df
}

# START HERE FOR TEST -- USING THE TRAIN LOOKUP TABLE FOR TEST DATA, SINCE 
# WE DON'T HAVE RELEVANCE SCORES IN TEST DATA
trainDF$wordPowerScore_title <- 0 # prepopulate the new column for word power score for each row

for(i in 1:nrow(trainDF)){
  if(! is.null(trainDF$matchedWords[[i]])){
    wordScoreObs<- sapply(trainDF$matchedWords[[i]], function(x){
      x <- paste('^', x, '$', sep = '')
      if(any(grepl(x, wordPowersTrain$word))){
        return(wordPowersTrain$relScore[grepl(x, wordPowersTrain$word)])
      }
      else{
        return(0)
      }
    })
    trainDF$wordPowerScore_title[i] <- sum(wordScoreObs)
  }
}

##############################################################
#### SYNONYM SCORES #######################################
##############################################################

# bring in synonym columns
# DON'T DO SYNONYMS FOR VERSION 6.0 -- THEY'RE BUILT INTO ORIGINAL
# COLUMNS
# CHANGE FILE NAME IN NEXT LINE TO TEST OR TRAIN
latest <- read.csv('../data/trainChris_6.0.csv', stringsAsFactors = FALSE,
                   strip.white = TRUE)
toBind <- select(latest, product_description_syn, product_title_syn,
                 search_term_syn, brand_syn)
# get rid of leading and trailing white space, next line, but 
# don't need this bcse of strip.white in read.csv.
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# clean toBind here
trainDF <- cbind(trainDF, toBind)
# need to change chris' '__' for no match in search term, since
# otherwise they'll match with "__" in description and title
trainDF$search_term_syn[which(trainDF$search_term_syn == '__')] <- 'xxxxxxxx'

# Add columns related to SYNONYM match counts
trainDF <- rowwise(trainDF) %>% mutate(. , 
                                       # number of words in search_term that match title:
                                       matchCount_synTitle = matchCount(search_term_syn, product_title_syn)[[1]],
                                       # number of words in search_term that match description
                                       matchCount_synDesc = matchCount(search_term_syn, product_description_syn)[[1]] ,
                                       # number of words in search_term that match brand
                                       matchCount_synBrand = matchCount(search_term_syn, brand_syn)[[1]]
)

########################################################
#### LAST WORD MATCH ###################################
########################################################
lastWord <- function(phrase){
  return(tail(strsplit(phrase,split=" ")[[1]],1))
}

trainDF <- rowwise(trainDF) %>% mutate(. , 
                                       # number of words in search_term that match last word of title
                                       # (should never be > 1:
                                       matchCount_lastWord_Title = matchCount(search_term, 
                                                                              lastWord(product_title))[[1]]
)

# change the >1 entries
trainDF$matchCount_lastWord_Title[trainDF$matchCount_lastWord_Title > 1] <- 1


#######################################################

# for handoff to machine learning processes:
writeFile(trainDF, buildType, '6')


##################################################################################

