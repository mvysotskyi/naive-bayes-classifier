# Naive Bayers Spam Filter

library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)

recall <- function(y_true, y_pred) {
    tp <- sum(y_true == y_pred & y_true == TRUE)
    fn <- sum(y_true != y_pred & y_true == TRUE)
	
    return (tp / (tp + fn))
}

precision <- function(y_true, y_pred) {
    tp <- sum(y_true == y_pred & y_true == TRUE)
    fp <- sum(y_true != y_pred & y_true == FALSE)
	
    return (tp / (tp + fp))
}

f1Score <- function(y_true, y_pred) {
    recall <- recall(y_true, y_pred)
    precision <- precision(y_true, y_pred)
	
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

splittedStopWords <- strsplit(stop_words, split='\n')
splittedStopWords <- splittedStopWords[[1]]


naiveBayes <- setRefClass("naiveBayes",                      
    # here it would be wise to have some vars to store intermediate result
    # frequency dict etc. Though pay attention to bag of words! 
    fields = list(
	hamFreq = "data.frame",
	spamFreq = "data.frame",
	hamCount = "numeric",
	spamCount = "numeric",

	totalCount = "numeric",
	hamClassProb = "numeric",
	spamClassProb = "numeric"
    ),
    
    methods = list(
        # prepare your training data as X - bag of words for each of your
	# messages and corresponding label for the message encoded as 0 or 1 
        # (binary classification task)
	fit = function(X, y)
	{	    
	    hamFreq <<- freqDataframe(X[X$Category=="ham", ], "Message", splittedStopWords)
	    spamFreq <<- freqDataframe(X[X$Category=="spam", ], "Message", splittedStopWords)

	    hamCount <<- sum(X$Category=="ham")
	    spamCount <<- sum(X$Category=="spam")

	    totalCount <<- hamCount + spamCount
	    hamClassProb <<- hamCount / totalCount
	    spamClassProb <<- spamCount / totalCount
	},
                    
	# return prediction for a single message 
	predict = function(message)
	{
	    # Check if message is not empty
	    if(nchar(message) == 0) { 	
		return (NULL)
	    }

	    # Convert message to bag of words
	    wrapperDf <- data.frame(word = message)
	    tokens <- unnest_tokens(wrapperDf, word, word) %>% filter(!word %in% splittedStopWords) 

	    hamProb <- hamClassProb
	    spamProb <- spamClassProb

	    for (i in 1:nrow(tokens))
	    {
    		word <- tokens[i, ]
		
		inHamCount <- ifelse(!is.na(any(hamFreq$word == word)) && any(hamFreq$word == word),
				     hamFreq[hamFreq$word == word, "n"]$n,
				     0)
		inSpamCount <- ifelse(!is.na(any(spamFreq$word == word)) && any(spamFreq$word == word),
				      spamFreq[spamFreq$word == word, "n"]$n,
				      0)
		
		hamProb <- hamProb * (inHamCount + 1) / (hamCount + totalCount)
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
	    y_pred <- lapply(X_test$Message, function(message) { predict(message) })
	    
	    scores = c(
		"accuracy"  = accuracy(y_test, y_pred),
		"precision" = precision(y_test, y_pred),
		"recall"    = recall(y_test, y_pred),
		"f1Score"   = f1Score(y_test, y_pred)
	    )

	    return (scores)
	}
))

test <- read_csv("data/test.csv", show_col_types = FALSE)
train <- read_csv("data/train.csv", show_col_types = FALSE)

# Create and fit model
model = naiveBayes()
model$fit(train, train$Category)

# Score model
model$score(test, test$Category == "ham")

# Test on single message
model$predict("Hello, how are you?") # should return TRUE
model$predict("WINNER!! This is the secret code to unlock the money: C3421.") # should return FALSE

# Test on empty string
model$predict("") # should return NULL

