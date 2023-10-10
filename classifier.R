# Naive Bayers Spam Filter

library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)

f1Score <- function(y_true, y_pred) {
    tp <- sum(y_true == y_pred & y_true == TRUE)
    fp <- sum(y_true != y_pred & y_true == FALSE)
    fn <- sum(y_true != y_pred & y_true == TRUE)
	
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
	
    return (2 * precision * recall / (precision + recall))
}

accuracy <- function(y_true, y_pred)
{
    return (sum(y_true == y_pred) / length(y_true))
}

freqDataframe <- function(dataframe, column, stop_words = NULL)
{
	dataframe %>%
		unnest_tokens(word, column) %>%
		count(word, sort = TRUE) %>%
		filter(!word %in% stop_words)
}

stop_words <- read_file("data/stop_words.txt")
# https://stackoverflow.com/questions/27195912/why-does-strsplit-return-a-list
splitted <- strsplit(stop_words, split='\n')
splitted <- splitted[[1]]


naiveBayes <- setRefClass("naiveBayes",                      
    # here it would be wise to have some vars to store intermediate result
    # frequency dict etc. Though pay attention to bag of words! 
    fields = list(
	hamFreq = "data.frame",
	spamFreq = "data.frame",
	hamCount = "numeric",
	spamCount = "numeric"
    ),
    
    methods = list(
        # prepare your training data as X - bag of words for each of your
	# messages and corresponding label for the message encoded as 0 or 1 
        # (binary classification task)
	fit = function(X, y)
	{
	    print("Fitting model...")
	    
	    hamFreq <<- freqDataframe(X[X$Category=="ham", ], "Message", splitted)
	    spamFreq <<- freqDataframe(X[X$Category=="spam", ], "Message", splitted)

	    hamCount <<- sum(X$Category=="ham")
	    spamCount <<- sum(X$Category=="spam")
		
	    print("Model fitted!")
	},
                    
	# return prediction for a single message 
	predict = function(message)
	{
	    if(nchar(message) == 0) { 
		print("Please provide a message to predict!")
		return (NULL)
	    }
	    # Convert message to bag of words
	    wrapperDf <- data.frame(word = message)
	    tokens <- unnest_tokens(wrapperDf, word, word) %>% filter(!word %in% splitted)
	    
	    # Total messages count
	    totalCount <- hamCount + spamCount
	    hamClassProb <- hamCount / totalCount
	    spamClassProb <- spamCount / totalCount
 

	    # Calculate probability of message being ham
	    hamProb <- hamClassProb

	    #print("tehre")
	    for (i in 1:nrow(tokens))
	    {
    		word <- tokens[i, ]
		
		inHamCount <- 0
		if(!is.na(any(hamFreq$word == word)) && any(hamFreq$word == word)) { inHamCount <- hamFreq[hamFreq$word == word, "n"]$n }

		hamProb <- hamProb * (inHamCount + 1) / (hamCount + totalCount)
	    }


	    # Calculate probability of message being spam
	    spamProb <- spamClassProb

	    for (i in 1:nrow(tokens))
	    {
		word <- tokens[i, ]	
		
		inSpamCount <- 0
		if(!is.na(any(spamFreq$word == word)) && any(spamFreq$word == word)) { inSpamCount <- spamFreq[spamFreq$word == word, "n"]$n }

		spamProb <- spamProb * (inSpamCount + 1) / (spamCount + totalCount)
    	    }

	    return (hamProb > spamProb)
	},
                    
        # score you test set so to get the understanding how well you model
        # works.
        # look at f1 score or precision and recall
	# visualize them 
        # try how well your model generalizes to real world data! 
	score = function(X_test, y_test)
	{
	    resultVector <- c()
	    for(i in 1:nrow(X_test))
	    {
		message <- X_test[i, ]
		prediction <- predict(message$Message)
		
		resultVector <- c(resultVector, prediction)
	    }

	    return (f1Score(y_test, resultVector))
	}
))

test <- read_csv("data/test.csv", show_col_types = FALSE)
train <- read_csv("data/train.csv", show_col_types = FALSE)

model = naiveBayes()

model$fit(train, train$Category)
model$score(test, test$Category == "ham")

model$predict("")


