##############################################
# File: 	typeSpecific.R
# Authoer: 	Brett Amdur
# Project: 	Home Depot Kaggle Competition
# Purpose: 	Feature Engineering
# Notes: 	This file contains several funcions used to build various data frames, 
#			the structure of which depend on whether the data frame to be built is 
#			related to training or test data.
##############################################




####################################################################################
# functions to build various data frames, the structure of which depend on whether #
# we're working on training or test data.                                          #
####################################################################################

# build starting data frame
buildData <- function (buildType, buildNumber){
	fileName <- paste('../data/', buildType, 'Chris_', buildNumber, '.csv', sep = '')
	train <-read.csv(fileName, stringsAsFactors = FALSE)
	if(grepl('^orig', buildType)){
		descrips <- read.csv('../data/product_descriptions.csv', stringsAsFactors = FALSE)
		train <- left_join(train, descrips, by = "product_uid")		
	}
	#train <- train[, c('id', 'product_uid', 'product_title', 'search_term', 'product_description')]
	return(train)
}

# build brand name column
buildBrand <- function(dataDF, buildType, buildNumber){
	fileName <- paste('../data/', buildType, 'Chris_', buildNumber, '.csv', sep = '')
	brandDF <-read.csv(fileName, stringsAsFactors = FALSE)
	if(grepl('^origTrain', buildType)){
		brandCol <- read.csv('./brandColumn_train.csv', stringsAsFactors = FALSE)
		brandDF$brand <- brandCol$x
	}
	if(grepl('^origTest', buildType)){
		brandCol <- read.csv('./brandColumn_test.csv', stringsAsFactors = FALSE)
		brandDF$brand <- brandCol$x
	}
	dataDF$brand <- brandDF$brand
	# do the brand match
	beginTest <- function(brand, search_term){
		return(grepl(brand, search_term))	
	}
	dataDF <- dataDF %>% rowwise() %>% mutate(brandMatch = ifelse(beginTest(brand, search_term), 1, 0))
	return(dataDF)
}

# write file for transfer of data to machine learning processes
writeFile <- function(dataDF, buildType, buildNumber){
	writeable <- select(dataDF, id, matchCount, search_term_count, product_title_count, stHitRatio, 
						titleHitRatio, matchCount_desc, product_description_count, stHitRatio_desc,
						descHitRatio, brandMatch, wordPowerScore_title, matchCount_synTitle, matchCount_synDesc, 
						matchCount_synBrand, matchCount_lastWord_Title)
	fileName <- paste('../results/', buildType, '_toML_', buildNumber, '.csv', sep = '')
	write.csv(writeable, fileName)
}

###############################
# for building orig files
train_orig <-read.csv('./data/train.csv', stringsAsFactors = FALSE)
trtest <- read.csv('./data/test.csv', stringsAsFactors = FALSE)
attribs <- read.csv('./data/attributes.csv', stringsAsFactors = FALSE)
# need original descrips for building orig data set.
descrips <- read.csv('./data/product_descriptions.csv', stringsAsFactors = FALSE)
train <- left_join(train, descrips, by = "product_uid")