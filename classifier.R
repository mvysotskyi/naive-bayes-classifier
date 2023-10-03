# Naive Bayers Spam Filter

f1Score <- function(y_true, y_pred)
{
    true_positives <- sum(y_true * y_pred)
    predicted_positives <- sum(y_pred)
    possible_positives <- sum(y_true)

    precision <- true_positives / predicted_positives
    recall <- true_positives / possible_positives

    return (2 * precision * recall) / (precision + recall)
}

naiveBayes <- setRefClass("naiveBayes",                      
    # here it would be wise to have some vars to store intermediate result
    # frequency dict etc. Though pay attention to bag of words! 
    fields = list(),
    
    methods = list(
        # prepare your training data as X - bag of words for each of your
	# messages and corresponding label for the message encoded as 0 or 1 
        # (binary classification task)
	fit = function(X, y)
	{
	    # TODO
	},
                    
	# return prediction for a single message 
	predict = function(message)
	{
	    # TODO
	},
                    
        # score you test set so to get the understanding how well you model
        # works.
        # look at f1 score or precision and recall
	# visualize them 
        # try how well your model generalizes to real world data! 
	score = function(X_test, y_test)
	{
	    # Return f1 score
	    # TODO
	}
))

model = naiveBayes()
model$fit()

