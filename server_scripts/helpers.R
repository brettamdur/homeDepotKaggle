##############################################
# File: 		helpers.R
# Author: 	Brett Amdur
# Project: 	Home Depot Kaggle Competition
# Purpose: 	Feature Engineering
# Notes: 	This file contains various miscelleaneous
#					functions associated with feature engineering.  It is something of 
#					a "scratch pad".
##############################################


######## For building Upper Left -- delivered to Chris for assessing "upper left" problem via
######## machine learning techniques.  See blog post for explanation of "upper left" problem
x <- read.csv('C:/Users/bamdur/Documents/NYCDSA/projects/HomeDepotKaggle/data/trainChris_2.2.csv', 
			  stringsAsFactors = FALSE)
trainDF$relevance <- as.numeric(x$relevance)
scaledDF <- select(trainDF, id, search_term, product_description, relevance, matchCount, 
				   	search_term_count, product_title_count, stHitRatio, 
	   				titleHitRatio, matchCount_desc, product_description_count, stHitRatio_desc,
	   				descHitRatio, brandMatch)

scaledDF$stHitRatio[is.infinite(scaledDF$stHitRatio)] <- 0
scaledDF$stHitRatio_desc[is.infinite(scaledDF$stHitRatio_desc)] <- 0

valuesOnly <- scaledDF[, c('matchCount', 'stHitRatio', 'titleHitRatio', 'matchCount_desc', 
								  'stHitRatio_desc', 'descHitRatio')]

newScaledDF <- data.frame(matchCount = scale(valuesOnly$matchCount), 
					   stHitRatio = scale(valuesOnly$stHitRatio),
					   titleHitRatio = scale(valuesOnly$titleHitRatio), 
					   matchCount_desc = scale(valuesOnly$matchCount_desc),
					   stHitRatio_desc = scale(valuesOnly$stHitRatio_desc),
					   descHitRatio = scale(valuesOnly$descHitRatio))

upperLeft <- data.frame(id = trainDF$id, search_term = trainDF$search_term, 
						product_title = trainDF$product_title, 
						product_description = trainDF$product_description)
upperLeft <- cbind(upperLeft, relevance = trainDF$relevance, 
				   titleMatchCount = trainDF$matchCount, 
				   descMatchCount = trainDF$matchCount_desc, 
				   scaledScore = newScaledDF$scaledScore)

upperLeft <- upperLeft %>% filter(scaledScore < -8, relevance > 2.5)
write.csv(upperLeft, 'upperLeft.csv')
#########################################


##############################################################
#### WORD POWER SCORES #######################################
##############################################################
## TAKES ABOUT 10 MINUTES TO RUN ###

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



