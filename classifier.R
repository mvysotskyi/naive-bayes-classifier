# Naive Bayers Spam Filter

library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)

f1Score <- function(y_true, y_pred)
{
    true_positives <- sum(y_true * y_pred)
    predicted_positives <- sum(y_pred)
    possible_positives <- sum(y_true)

    precision <- true_positives / predicted_positives
    recall <- true_positives / possible_positives

    return (2 * precision * recall) / (precision + recall)
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
	    # Convert message to bag of words
	    wrapperDf <- data.frame(word = message)
	    tokens <- unnest_tokens(wrapperDf, word, word)
	    
	    # Total messages count
	    totalCount <- hamCount + spamCount
	    hamClassProb <- hamCount / totalCount
	    spamClassProb <- spamCount / totalCount
 

	    # Calculate probability of message being ham
	    hamProb <- 1

	    #print("tehre")
	    for (i in 1:nrow(tokens))
	    {
    		word <- tokens[i, ]
		
		a <- 0
			
		if(!is.na(any(hamFreq$word == word)) && any(hamFreq$word == word))
		{
		    a <- hamFreq[hamFreq$word == word, "n"]$n
		}	


		if (identical(a, integer(0)))
		{
		    a <- 0
		}

		hamProb <- hamProb * (a + 1) / (totalCount)
	    }

	    hamProb <- hamProb * hamClassProb

	    # Calculate probability of message being spam
	    spamProb <- 1

	    for (i in 1:nrow(tokens))
	    {
		word <- tokens[i, ]	
		
		a <- 0
		if(!is.na(any(spamFreq$word == word)) && any(spamFreq$word == word))
		{
		    a <- spamFreq[spamFreq$word == word, "n"]$n
		}

		if (identical(a, integer(0)))
		{
		    a <- 0
		}

		spamProb <- spamProb * (a + 1) / (totalCount)
    	    }

	    spamProb <- spamProb * spamClassProb

	    # Return prediction
	    if (hamProb > spamProb)
	    {
		return ("ham")
	    }
	    else
	    {
		return ("spam")
	    }
	},
                    
        # score you test set so to get the understanding how well you model
        # works.
        # look at f1 score or precision and recall
	# visualize them 
        # try how well your model generalizes to real world data! 
	score = function(X_test, y_test)
	{
	    counter <- 0
	    for(i in 1:nrow(X_test))
	    {
		message <- X_test[i, ]
		prediction <- predict(message$Message)

		if (prediction == y_test[i])
		{
		    counter <- counter + 1
		}
	    }

	    print(counter / nrow(X_test))

	    #return (f1Score(y_test, predict(X_test$Message)))
	}
))

test <- read_csv("data/test.csv")
train <- read_csv("data/train.csv")

# Calcutate spam and ham frequencies in test set
spam = nrow(test[test$Category=="spam", ])
ham = nrow(test[test$Category=="ham", ])

print(c(spam, ham))

model = naiveBayes()
model$fit(train, train$Category)
model$predict("i see. When we finish we have loads of loans to pay")

print("Scoring model...")

model$score(test, test$Category)

print(c(spam, ham))

