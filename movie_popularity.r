library(ggplot2)
library(dplyr)
library(statsr)
library(gridExtra)
library(corrplot)
library(Hmisc)
load("G:\\rprojects\\clgstuf\\project\\movies.Rdata")
movies1 = data.frame(movies)
# selecting only those attributes that we would need
movies_new <- movies?%>% select(title, title_type, genre, 
                                runtime, imdb_rating, imdb_num_votes, critics_rating, 
                                critics_score, audience_rating, audience_score, 
                                best_pic_win, best?actor_win, best_actress_win, best_dir_win)
# description of the attributes of the dataset
str(movies_new)
#summary 
summary(movies_new)
# there is one N/A value in runtime, so we drop the row , for simplicity
movies_new <- na.omit(movies_new)
# boxplot of ?arious genres to runtime
p_genrerun <- ggplot(movies_new, aes(x=factor(genre), y=runtime)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genrerun + ggtitle("Genre to runtime") + 
  geom_hline(yintercept =median(movies_new$?untime, na.rm = TRUE), col = "royalblue",lwd = 1)
# We can see all genres are oscilating around median runtime value of 103 minutes. 
# There are lot of outliers in data, mostly documentary genre

# boxplot of various genres to IMDB rating
p_genreimdb <- g?plot(movies_new, aes(x=factor(genre), y=imdb_rating)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_genreimdb + ggtitle("Genre to IMDB rating") + 
  geom_hline(yintercept =median(movies_new$imdb_rating, na.rm = TRUE), col ? "royalblue",lwd = 1)
#OBSERVATIONS:-
#We can see sci-fi has biggest variance between 1Q and 3Q
#median of sci-fi genre is lower then median of all genres.
#Documentary performs best as Musical and performing arts movies. 

#we split the data into training?and testing data
set.seed(2017)
split <- sample(seq_len(nrow(movies_new)), size = floor(0.999 * nrow(movies_new)))
train <- movies_new[split, ]
test <- movies_new[-split, ]
dim(train)

# plot between imdb rating and runtime
plot(movies$imdb_rating,movies$r?ntime, main="IMDB rating to runtime", 
     xlab = "IMDB rating", ylab="Runtime in minutes")
abline(lm(movies$runtime~movies$imdb_rating),col = "royalblue",lwd = 1)
#From simple linear regression used to IMDB rating and runtime of movie.
#We can see that w? can expect better IMDB rating if runtime is longer.

#plot critique score to audiance score
ggplot(data = movies, aes(x = critics_score, y = audience_score)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Critics score to audienc? score - Rotten")
#We see there is correlation between critics score of Rotten Tomato and audience score. 

#plot critique score to imdb
ggplot(data = movies, aes(x = imdb_rating, y = audience_score)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FA?SE) +
  ggtitle("Critics score to audience score - IMDB")
#very strong correlation

#plot imdb vs rotten
ggplot(data = movies, aes(x = critics_score, y = imdb_rating)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE) + ggtitle("IMDB vs. Rotten")?#We definitely have correlation between critics and IMDB rating.

#____________________________________________________________________________________
#now we analyze the training data

#HISTOGRAM OF NUMERIC VALUES

#AUDIENCE SCORE
hist(train$audience_sco?e)
summary(train$audience_score)
#audiences are unlikely to give very high or low values

#histogram of runtime , imdb, log(imdb), critics score
p1 <- ggplot(aes(x=runtime), data=train) + 
  geom_histogram(aes(y=100*(..count..)/sum(..count..)), color='blac?', fill='white', binwidth = 5) + 
  ylab('percentage') + ggtitle('Run Time')
p2 <- ggplot(aes(x=imdb_rating), data=train) +
  geom_histogram(aes(y=100*(..count..)/sum(..count..)), color='black', fill='white', binwidth = 0.2) + 
  ylab('percentage') + ggtit?e('IMDB rating')
p3 <- ggplot(aes(x=log10(imdb_num_votes)), data=train) +
  geom_histogram(aes(y=100*(..count..)/sum(..count..)), color='black', fill='white') + 
  ylab('percentage') + ggtitle('log(IMDB number of votes)')
#plot(p3)
p4 <- ggplot(aes(x=criti?s_score), data=train) +
  geom_histogram(aes(y=100*(..count..)/sum(..count..)), color='black', fill='white', binwidth = 2) + 
  ylab('percentage') + ggtitle('Critics Score')
grid.arrange(p1, p2, p3, p4, ncol=2)

# bar plot of catagorical data
p1 <- ggplot(?es(x=title_type), data=train) + geom_bar(aes(y=100*(..count..)/sum(..count..))) + 
  ylab('percentage') +
  ggtitle('Title Type') + coord_flip()
p2 <- ggplot(aes(x=genre), data=train) + geom_bar(aes(y=100*(..count..)/sum(..count..))) + 
  ylab('percentage'? +
  ggtitle('Genre') + coord_flip()
p3 <- ggplot(aes(x=critics_rating), data=train) + geom_bar(aes(y=100*(..count..)/sum(..count..))) + 
  ylab('percentage') +
  ggtitle('Critics Rating') + coord_flip()
p4 <- ggplot(aes(x=audience_rating), data=train) + g?om_bar(aes(y=100*(..count..)/sum(..count..))) + 
  ylab('percentage') + ggtitle('Audience Rating') + coord_flip()
grid.arrange(p1, p2, p3, p4, ncol=2)

#correlation of numericle values
vars <- names(train) %in% c('runtime', 'imdb_rating', 'imdb_num_votes',?'critics_score')
selected_train <- train[vars]
mydata.cor = cor(selected_train, method = c("spearman"))
corrplot(mydata.cor, method = "color", outline = T, addgrid.col = "darkgray",
         order = "hclust", addrect = 4, rect.col = "black", rect.lwd = 5, ?l.pos = "b",
         t1.col = "indianred4", t1.cex = 1.5, cl.cex = 1.5, addCoef.col = "white",
         number.digits = 2, number.cex = 0.75, 
         col = colorRampPalette(c("darkred","white","midnightblue"))(100))

#correlation between catagorical val?es
boxplot(audience_score~critics_rating, data=train, main='Audience score vs. Critics rating', 
        xlab='Critics Rating', ylab='Audience Score')
by(train$audience_score, train$critics_rating, summary)
boxplot(audience_score~audience_rating, data=trai?, main='Audience Score vs. Audience Rating', 
        xlab='Audience rating', ylab='Audience Score')
by(train$audience_score, train$audience_rating, summary)
boxplot(audience_score~title_type, data=train, main='Audience score vs. Title type', 
        xlab?'Title_type', ylab='Audience Score')
by(train$audience_score, train$title_type, summary)
boxplot(audience_score~genre, data=train, main='Audience score vs. Genre', 
        xlab='Genre', ylab='Audience score')
by(train$audience_score, train$genre, summary)?#_________________________________________________________________________________
#MODEL MAKING
#We will be using stepwise model forward selection method
#We will start with an empty model
#then add variables one at a time until a parsimonious model is re?ched.
#parsimonius model is the simplest model required to generate required outcome
#____________________
#choosing predictors
full_model <- lm(audience_score~imdb_rating+title_type+genre+runtime+imdb_num_votes+critics_rating+audience_rating+best_pic_win+?est_actor_win+best_actress_win+best_dir_win, data=train)
summary(full_model)
#imdb rating has the lowest p value and is the most correlated variable to our response variable. 
#So we choose imdb rating as the first predictor.
fit1 <- lm(audience_score ~ im?b_rating, data=train)
summary(fit1)
#The 0.75 R-squared and almost zero p value indicate that imdb rating is a statistically significant
#predictor of audience score.
#now we find the next predictor attribute
fit_model <- lm(audience_score~title_type+genre?runtime+imdb_num_votes+critics_rating+audience_rating+best_pic_win+best_actor_win+best_actress_win+best_dir_win, data=train)
summary(fit_model)
#in fit_model , audiance rating has the lowest p-value , so it will be our next predictor
fit2 <- lm(audience_sc?re ~ imdb_rating + audience_rating, data=train)
summary(fit2)
# again the p value is almost 0 so our predictors are significant
#next we tried the critics_rating , imdb_num_votes and genre as predictors
#we only got significant results for genre as our pre?ictor
fit3 <- lm(audience_score ~ imdb_rating + audience_rating + genre, data=train)
anova(fit3)
#so fit3 is our final model
summary(fit3)

#model diagnostics
ggplot(data = fit3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,?linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")
#There is clear a linear relationship between imdb rating and audience score. 
#The linearity condition is met by our model.
ggplot(data = fit3, aes(x = .resid)) +
  geom_histogram(binwid?h = 1, fill='white', color='black') +
  xlab("Residuals")
#Constant variance of residuals condition met, No fan shape in residuals plot.
ggplot(data = fit3, aes(sample = .resid)) +
  stat_qq()
#______________________________________________________________?__________________
#PREDICTION
newmovie <- test %>% select(genre, imdb_rating, audience_rating)
predict(fit3, newmovie)
predict(fit3, newmovie, interval = "prediction", level = 0.95)
test$audience_score
#the actual value is 81, our prediction contains this?value
#Our model demonstrates that it is possible to predict a movie's popularity, as measured by audience score with only three predictors - imdb score, audience rating and genre. Movie industries can use the similar methods when producing movies that are?more likely to be liked by the target audience.
#However, the potential shortcoming is that our model's predictive power is limited because the sample data is not representative. Therefore, a larger number of observations to capture more variability in the?population data in our testing data set is required to have a better measure of the model's accuracy.