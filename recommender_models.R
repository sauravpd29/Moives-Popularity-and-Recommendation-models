library(dplyr)
library(ggplot2)
library(knitr)
library(recommenderlab)


data(MovieLense)
MovieLense

head(names(colCounts(MovieLense)))

vector_ratings <- as.vector(MovieLense@data) #sparse rating matrix
kable(table(vector_ratings), caption="Rating frequency")


# Since a rating of 0 represents absence of a rating in this data set, we can remove such
# ratings from the ratings vector.
vector_ratings = vector_ratings[vector_ratings != 0]
hist(vector_ratings, main="Histogram of Ratings", xlab="Rating Value")


ratings = MovieLense[rowCounts(MovieLense) > 50, colCounts(MovieLense) > 100]
dim(ratings)


ratings.n = normalize(ratings)
ratings.n.vec = as.vector(ratings.n@data)
ratings.n.vec = ratings.n.vec[ratings.n.vec != 0]
hist(ratings.n.vec, main="Histogram of Normalized Ratings", xlab="Rating")

percent_train = 0.8
items_to_keep = 15        # items to use for each user
rating_threshold = 3      # good rating implies >=3
n_eval = 1                # number of times to run eval

eval_sets = evaluationScheme(data = ratings, method = "split",
                             train = percent_train, given = items_to_keep,
                             goodRating = rating_threshold, k = n_eval)
eval_sets


eval_recommender = Recommender(data = getData(eval_sets, "train"), method = "UBCF", parameter = NULL)
items_to_recommend = 10
eval_prediction = predict(object = eval_recommender,
                          newdata = getData(eval_sets, "known"),
                          n = items_to_recommend,
                          type = "ratings")
eval_accuracy = calcPredictionAccuracy(x = eval_prediction,
                                       data = getData(eval_sets, "unknown"),
                                       byUser = TRUE)
head(eval_accuracy)

#---------

eval_recommender = Recommender(data = getData(eval_sets, "train"), method = "IBCF", parameter = NULL)
items_to_recommend = 10
eval_prediction = predict(object = eval_recommender,newdata = getData(eval_sets, "known"),
                          n = items_to_recommend,
                          type = "ratings")
eval_accuracy = calcPredictionAccuracy(x = eval_prediction,
                                       data = getData(eval_sets, "unknown"),
                                       byUser = TRUE)
head(eval_accuracy)



models_to_evaluate = list(IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
                          IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
                          UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
                          UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
                          random = list(name = "RANDOM", param=NULL))

n_recommendations = c(1, 3, 5, 10, 15, 20)
results = evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)




# Draw ROC curve
plot(results, y = "ROC", annotate = 1, legend="topleft")
title("ROC Curve")



# Draw precision / recall curve
plot(results, y = "prec/rec", annotate=1)
title("Precision-Recall")