-
# Packages needed for report
library(lubridate)
library(dplyr)
library(tibble)
library(stringr)
library(caret)
library(tidyr)
library(tidyverse)
library(partitions)
library(iterpc)
library(gridExtra)

################################################################
# Code supplied from EDX to download datasets
###############################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")


test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

names(edx)
dim(edx)

movielens <- edx 
movielens %>% as_tibble()

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Get the dimensions of the full MovieLens dataset
dim(movielens)

# Names of each column
names(movielens)

# Unique userIds, MovieIds and genre combinations
movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId),
            n_genres = n_distinct(genres))

# View the first 5 rows of the dataset
movielens[1:5,]

# extract the movie release year from the title and rating year from the timestamp
# into two new columns column called "movie_year" and "rating_year" Year".
edx <- edx %>% 
  extract(title, regex = "([0-9][0-9][0-9][0-9])", into = "movie_year"
          , remove = FALSE,convert = TRUE) %>%
  mutate(rating_year = year(as.Date(as.POSIXlt(timestamp, origin="1970-01-01"))))

validation <- validation %>% 
  extract(title, regex = "([0-9][0-9][0-9][0-9])", into = "movie_year"
          , remove = FALSE,convert = TRUE) %>%
  mutate(rating_year = year(as.Date(as.POSIXlt(timestamp, origin="1970-01-01"))))


# Create data partitions on the 'edx' training data   
set.seed(1)
test_index <- createDataPartition(edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx %>% slice(-test_index)
test_set_temp <- edx %>% slice(test_index)

set.seed(1)
test_set <- test_set_temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

removed <- anti_join(test_set_temp, test_set)
train_set <- rbind(train_set, removed)

#Movie Ratings

Movie_Avg_Rating_Histogram <- edx%>% 
group_by(movieId) %>%
summarize(avg_rating  = mean(rating)) %>%
qplot(avg_rating ,geom ="histogram", xlab = "Avg Rating", ylab = "count",bins = 30, data = ., color = I("light Blue"), main = "Movie Avg Rating")

Movie_Ratings_count <- edx %>% 
     dplyr::count(movieId) %>% 
     ggplot(aes(n)) + 
     geom_histogram(bins = 30, color = "light Blue") + 
     scale_x_log10() + 
     ggtitle("Movies")

grid.arrange(Movie_Ratings_count, Movie_Avg_Rating_Histogram, ncol = 2)


User_Avg_Rating_Histogram <- edx%>% 
group_by(userId) %>%
summarize(avg_rating  = mean(rating)) %>%
qplot(avg_rating ,geom ="histogram", xlab = "Avg Rating", ylab = "count",bins = 30, data = ., color = I("light Green"), main = "User Avg Rating")

User_Ratings_count <- edx %>% 
     dplyr::count(userId) %>% 
     ggplot(aes(n)) + 
     geom_histogram(bins = 30, color = "light Green") + 
     scale_x_log10() + 
     ggtitle("Users")

grid.arrange(User_Ratings_count, User_Avg_Rating_Histogram, ncol = 2)

Genres_Avg_Rating_Histogram <- edx%>% 
group_by(genres) %>%
summarize(avg_rating  = mean(rating)) %>%
qplot(avg_rating ,geom ="histogram", xlab = "Avg Rating", ylab = "count",bins = 30, data = ., color = I("light Pink"), main = "Avg Rating by Genres")

Genres_Ratings_Count <- edx %>% 
     dplyr::count(genres) %>% 
     ggplot(aes(n)) + 
     geom_histogram(bins = 30, color = "light Pink") + 
     scale_x_log10() + 
     ggtitle("Genres")

grid.arrange(Genres_Ratings_Count , Genres_Avg_Rating_Histogram , ncol = 2)

movie_year_plot <- edx %>% 
filter(between( movie_year, 1900, 2018)) %>%
group_by(movie_year) %>%
summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(movie_year, avg_rating, label = movie_year)) +
  geom_point(size = 2, color ="Dark Orange") +  
  xlab("Release Year") + 
  ylab("Avg Rating") +
  scale_y_continuous(limits = c(3.4, 4.1)) +
  scale_x_continuous(limits = c(1900, 2018)) +
  ggtitle("Release Year - Avg Rating")

movie_rating_year_plot <- edx %>% 
group_by(rating_year) %>%
summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(rating_year, avg_rating, label = rating_year)) +
  geom_point(size = 2, color ="Gold") +  
  xlab("Rating Year") + 
  ylab("Avg Rating") +
  scale_y_continuous(limits = c(3.4, 4.1)) +  
  ggtitle("Rating Year - Avg Rating")

grid.arrange(movie_year_plot, movie_rating_year_plot, ncol = 2)

# Model 1 take the mean of the train_set ratings. 
# Note: the _t is used to tag variables from the train_set/test_set

mu_hat_t <- mean(train_set$rating)
# Use the RMSE formula to 
model_1_rmse_t <- RMSE(test_set$rating, mu_hat_t)
model_1_rmse_t

# Calculate beta_i using the train_set
movie_avg_t <- train_set %>%
  group_by(movieId) %>%
  summarize(beta_i = mean(rating - mu_hat_t))  

# Join on to the test_set and make the predictions
predicted_ratings_2_t <- test_set %>%
  left_join(movie_avg_t, by = "movieId") %>%
  mutate(pred = mu_hat_t + beta_i) %>%  
  pull(pred)

# Pull the RMSE
model_2_rmse_t <- RMSE(predicted_ratings_2_t, test_set$rating)
model_2_rmse_t

# Select the Lambda values
# create a grid of potential values for lambda_1 and Lambda_2.
lambdas <- seq(0, 6, 0.5)
lamdas_matrix <- expand.grid(Lam_1 = lambdas , Lam_2 = lambdas)

# create a function to select the optimal values for lambda 
# Note: using the test_set and train_sets as the validation_set 
# is reserved for the final testing only)
cross_val_R3  <- function(L1,L2){
  mu_hat <- mean(train_set$rating)
  
  movie_avg <- train_set %>% 
    group_by(movieId) %>%
    summarize(beta_i = sum(rating - mu_hat)/(n()+L1))
  
  user_bias <- train_set %>% 
    left_join(movie_avg, by = "movieId") %>%
    group_by(userId) %>%
    summarize(beta_u = sum(rating - beta_i - mu_hat)/(n()+L2))
  
  predicted_ratings <- test_set %>% 
    left_join(movie_avg, by = "movieId") %>%
    left_join(user_bias , by= "userId") %>%	 
    mutate(pred = mu_hat + beta_i + beta_u) %>%	
    pull(pred)
  rmse <-  RMSE(predicted_ratings, test_set$rating)
  return(rmse)
}
# Use "mapply" to apply the cross_val_R3 function to the potential lambdas
rmses_R3  <- mapply(cross_val_R3 ,lamdas_matrix[,1],lamdas_matrix[,2]) 

# Get the optimal lambdas 
Lambda_R3  <- lamdas_matrix[which.min(rmses_R3 ),]
Lambda_R3 

# Calculate beta_i using the train_set and penalization parameters
#"_R" denotes regularized 
movie_avg_t_R <- train_set %>% 
  group_by(movieId) %>%
  summarize(beta_i = sum(rating - mu_hat_t)/(n()+Lambda_R3 [,1]))

# Calculate beta_u using the train_set and regularization
user_bias_t_R <- train_set %>% 
  left_join(movie_avg_t_R, by = "movieId") %>%
  group_by(userId) %>%
  summarize(beta_u = sum(rating - beta_i - mu_hat_t)/(n()+Lambda_R3 [,2]))

# make prediction using the test_set
predicted_ratings_R3_t <- test_set %>% 
  left_join(movie_avg_t_R, by = "movieId") %>%
  left_join(user_bias_t_R , by= "userId") %>%	 
  mutate(pred = mu_hat_t + beta_i + beta_u) %>%	
  pull(pred)

# get the RMSE of model 3
model_R3_rmse_t <- RMSE(predicted_ratings_R3_t, test_set$rating)
model_R3_rmse_t

# create a grid of potential values for the penalization parameter for beta_g
lambdas <- seq(0, 6, 0.5)
# create a function to select the optimal values for lambda 
# Note:  using the test_set and train_sets as the validation_set 
#is reserved for the final testing only)

cross_val_R4 <- function(L4){
  
  mu_hat <- mean(train_set$rating)
  
  movie_avg <- train_set %>% 
    group_by(movieId) %>%
    summarize(beta_i = sum(rating - mu_hat)/(n()+4.5))
  
  user_bias <- train_set %>% 
    left_join(movie_avg_t, by = "movieId") %>%
    group_by(userId) %>%
    summarize(beta_u = sum(rating - beta_i - mu_hat)/(n()+5))
  
  genres_bias <- train_set %>%
    left_join(movie_avg , by = "movieId") %>%
    left_join(user_bias, by = "userId") %>%
    group_by(genres) %>% 
    summarize(beta_g = sum(rating - mu_hat - beta_i - beta_u)/(n()+L4))
  
  predicted_ratings <- test_set %>% 
    left_join(movie_avg, by= "movieId") %>%
    left_join(user_bias, by= "userId") %>%
    left_join(genres_bias, by= "genres") %>%
    mutate(pred = mu_hat + beta_i + beta_u + beta_g) %>%
    pull(pred)
  
  rmse <-  RMSE(predicted_ratings, test_set$rating)
  return(rmse)
}
# Use "sapply" to apply the cross_val_R4 function to the lambda vector
rmses_R4  <- sapply(lambdas,cross_val_R4 ) 
# Get the optimal lambda for the beta_g
Lambda_R4  <- lambdas[which.min(rmses_R4 )]
Lambda_R4 

# Calculate beta_g using the train_set 
genres_bias_t_R <- train_set %>%
  left_join(movie_avg_t_R , by = "movieId") %>%
  left_join(user_bias_t_R, by = "userId") %>%
  group_by(genres) %>% 
  summarize(beta_g = sum(rating - mu_hat_t - beta_i - beta_u)/(n()+Lambda_R4))

# make prediction using the test_set
predicted_ratings_R4_t <- test_set %>% 
  left_join(movie_avg_t_R, by= "movieId") %>%
  left_join(user_bias_t_R, by= "userId") %>%
  left_join(genres_bias_t_R, by= "genres") %>%
  mutate(pred = mu_hat_t + beta_i + beta_u + beta_g) %>%
  pull(pred)

# get the RMSE of model R4
model_R4_rmse_t <- RMSE(predicted_ratings_R4_t, test_set$rating)
model_R4_rmse_t


# create a grid of potential values for the lambda associated with the movie release year
lambdas <- seq(0, 6, 0.5)
# create a function to select the optimal values for lambda
cross_val_R5 <- function(L5){
  
  mu_hat <- mean(train_set$rating)
  
  movie_avg <- train_set %>% 
    group_by(movieId) %>%
    summarize(beta_i = sum(rating - mu_hat)/(n()+4.5))
  
  user_bias <- train_set %>%  
    left_join(movie_avg, by = "movieId") %>%
    group_by(userId) %>%
    summarize(beta_u = sum(rating - beta_i - mu_hat)/(n()+5))
  
  genres_bias <- train_set %>%
    left_join(movie_avg , by = "movieId") %>%
    left_join(user_bias, by = "userId") %>%
    group_by(genres) %>% 
    summarize(beta_g = sum(rating - mu_hat - beta_i - beta_u)/(n()+0))
  
  movie_year_bias <- train_set %>% 
    left_join(movie_avg, by= "movieId") %>%
    left_join(user_bias, by= "userId") %>% 
    left_join(genres_bias, by= "genres") %>%
    group_by(movie_year) %>%
    summarize(beta_d = sum(rating - mu_hat - beta_i - beta_u - beta_g)/(n()+ L5))
  
  predicted_ratings <- test_set %>% 
    left_join(movie_avg, by= "movieId") %>%
    left_join(user_bias, by= "userId") %>%
    left_join(genres_bias, by= "genres")%>%
    left_join(movie_year_bias, by= "movie_year") %>%
    mutate(pred = mu_hat + beta_i + beta_u + beta_g + beta_d) %>%
    pull(pred)
  
  rmse <-  RMSE(predicted_ratings, test_set$rating)
  return(rmse)
}
# Use "sapply" to apply the cross_val_R5 function to the potential lambdas
rmses_R5  <- sapply(lambdas,cross_val_R5 ) 
# Get the optimal lambda for the beta_d
Lambda_R5  <- lambdas[which.min(rmses_R5 )]
Lambda_R5 


# Calculate beta_d using the train_set 
movie_year_bias_t_R <- train_set %>% 
  left_join(movie_avg_t_R, by= "movieId") %>%
  left_join(user_bias_t_R, by= "userId") %>% 
  left_join(genres_bias_t_R, by= "genres") %>%
  group_by(movie_year) %>%
  summarize(beta_d = sum(rating - mu_hat_t - beta_i - beta_u - beta_g)/(n()+ Lambda_R5))

# make prediction using the test_set
predicted_ratings_R5_t <- test_set %>% 
  left_join(movie_avg_t_R, by= "movieId") %>%
  left_join(user_bias_t_R, by= "userId") %>%
  left_join(genres_bias_t_R, by= "genres")%>%
  left_join(movie_year_bias_t_R, by= "movie_year") %>%
  mutate(pred = mu_hat_t + beta_i + beta_u + beta_g + beta_d) %>%
  pull(pred)

# get the RMSE of model R5
model_R5_rmse_t <- RMSE(predicted_ratings_R5_t, test_set$rating)
model_R5_rmse_t


# create a grid of potential values for the lambda associated with the beta_r
lambdas <- seq(0, 6, 0.5)

# create a function to select the optimal values for lambda
cross_val_R6 <- function(L6){
  
  mu_hat <- mean(train_set$rating)
  
  movie_avg <- train_set %>% 
    group_by(movieId) %>%
    summarize(beta_i = sum(rating - mu_hat)/(n()+4.5))
  
  user_bias <- train_set %>% 
    left_join(movie_avg, by = "movieId") %>%
    group_by(userId) %>%
    summarize(beta_u = sum(rating - beta_i - mu_hat)/(n()+5))
  
  genres_bias <- train_set %>%
    left_join(movie_avg , by = "movieId") %>%
    left_join(user_bias, by = "userId") %>%
    group_by(genres) %>% 
    summarize(beta_g = sum(rating - mu_hat - beta_i - beta_u)/(n()+0))
  
  movie_year_bias <- train_set %>% 
    left_join(movie_avg, by= "movieId") %>%
    left_join(user_bias, by= "userId") %>% 
    left_join(genres_bias, by= "genres") %>%
    group_by(movie_year) %>%
    summarize(beta_d = sum(rating - mu_hat - beta_i - beta_u - beta_g)/(n()+ 6))
  
  movie_rating_year <- train_set %>% 
    left_join(movie_avg , by= "movieId") %>%
    left_join(user_bias , by= "userId") %>% 
    left_join(genres_bias , by= "genres") %>%
    left_join(movie_year_bias , by= "movie_year") %>%
    group_by(rating_year) %>%	
    summarize(beta_r = sum(rating - mu_hat - beta_i - beta_u - beta_g - beta_d)/(n()+L6))
  
  predicted_ratings  <- test_set %>% 
    left_join(movie_avg , by= "movieId") %>%
    left_join(user_bias , by= "userId") %>%
    left_join(genres_bias , by= "genres")%>%
    left_join(movie_year_bias , by= "movie_year") %>%
    left_join(movie_rating_year, by= "rating_year") %>%
    mutate(pred = mu_hat  + beta_i + beta_u + beta_g + beta_d + beta_r) %>%
    pull(pred)
  
  
  rmse <-  RMSE(predicted_ratings, test_set$rating)
  return(rmse)
}
# Use "sapply" to apply the cross_val_R6 function to the potential lambdas
rmses_R6  <- sapply(lambdas,cross_val_R6) 
# Get the optimal lambda for the beta_r
Lambda_R6  <- lambdas[which.min(rmses_R6)]
Lambda_R6 


# Calculate beta_r using the train_set 
movie_rating_year_t_R <- train_set %>% 
  left_join(movie_avg_t_R , by= "movieId") %>%
  left_join(user_bias_t_R , by= "userId") %>% 
  left_join(genres_bias_t_R , by= "genres") %>%
  left_join(movie_year_bias_t_R , by= "movie_year") %>%
  group_by(rating_year) %>%	
  summarize(beta_r = sum(rating - mu_hat_t - beta_i - beta_u - beta_g - beta_d)/(n()+ Lambda_R6))	

# make prediction using the test_set
predicted_ratings_R6_t  <- test_set %>% 
  left_join(movie_avg_t_R , by= "movieId") %>%
  left_join(user_bias_t_R , by= "userId") %>%
  left_join(genres_bias_t_R , by= "genres")%>%
  left_join(movie_year_bias_t_R , by= "movie_year") %>%
  left_join(movie_rating_year_t_R, by= "rating_year") %>%
  mutate(pred = mu_hat_t  + beta_i + beta_u + beta_g + beta_d + beta_r) %>%
  pull(pred)

# get the rmse of model 6
model_R6_rmse_t  <- RMSE(predicted_ratings_R6_t,test_set$rating)
model_R6_rmse_t

#Model results from the test_set so far
model_results <- as.data.frame(rbind(
  c(model = "model 1", RMSE =    round(model_1_rmse_t,4))
  ,c(model = "model 2", RMSE =   round(model_2_rmse_t,4))
  ,c(model = "model R3", RMSE =  round(model_R3_rmse_t,4))
  ,c(model = "model R4", RMSE =  round(model_R4_rmse_t,4))
  ,c(model = "model R5", RMSE =  round(model_R5_rmse_t,4))
  ,c(model = "model R6", RMSE =  round(model_R6_rmse_t,4))))
model_results

# Combine the predictions of the 4 models 
prediction_matrix <- cbind(predicted_ratings_R3_t
                           ,predicted_ratings_R4_t
                           ,predicted_ratings_R5_t
                           ,predicted_ratings_R6_t)

# to determine the weightings, create a matrix of possible proportions such that each row adds to 1. 
# Use the restrictedparts function from the partitions package

weights <- as.matrix(t(restrictedparts(100,ncol(prediction_matrix)
                                       ,include.zero = TRUE
                                       , decreasing = FALSE)/100))

weights <- rbind(
  weights
  ,cbind(weights[,2],weights[,3],weights[,4],weights[,1])
  ,cbind(weights[,3],weights[,4],weights[,1],weights[,2])
  ,cbind(weights[,4],weights[,1],weights[,2],weights[,3]))

# create a function "ensemble weights" to evaluate the RMSE of the predictions
ensemble_weights <- function(w1,w2,w3,w4){
  
  ensemble_weights <- as.matrix(c(w1,w2,w3,w4)) 
  ensemble_prediction <- cbind(prediction_matrix %*% ensemble_weights,test_set$rating)
  rmse <- RMSE(ensemble_prediction[,1],ensemble_prediction[,2])
}
  #Apply the ensemble weights function to each row of the weights matrix using mapply
ens_rmse <- mapply(ensemble_weights,weights[,1],weights[,2],weights[,3],weights[,4])
ensemble_weights <- as.matrix(weights[which.min(ens_rmse),])

#optimal ensemble weights are give
ensemble_weights 


# Generate the ensemble prediction on the test_set
ensembele_Pred_cv = prediction_matrix %*%  ensemble_weights
ensembele_rmse_cv <- RMSE(ensembele_Pred_cv,test_set$rating)
ensembele_rmse_cv
#Final Ensemble RMSE

model_results <- as.data.frame(
  
  rbind(
    c(model = "model 1", RMSE = round(model_1_rmse_t,6))
    ,c(model = "model 2", RMSE =  round(model_2_rmse_t,6))
    ,c(model = "model R3", RMSE =  round(model_R3_rmse_t,6))
    ,c(model = "model R4", RMSE = round(model_R4_rmse_t,6))
    ,c(model = "model R5", RMSE =  round(model_R5_rmse_t,6))
    ,c(model = "model R6", RMSE =  round(model_R6_rmse_t,6))
    ,c(model = "Ensemble", RMSE = round(ensembele_rmse_cv,6))))

model_results


# Model R5 using the EDX Data

mu_hat <- mean(edx$rating)

movie_avg_R <- edx %>% 
  group_by(movieId) %>%
  summarize(beta_i = sum(rating - mu_hat)/(n()+Lambda_R3 [,1]))

user_bias_R <- edx  %>% 
  left_join(movie_avg_R, by = "movieId") %>%
  group_by(userId) %>%
  summarize(beta_u = sum(rating - beta_i - mu_hat)/(n()+Lambda_R3 [,2]))

genres_bias_R  <- edx   %>%
  left_join(movie_avg_R  , by = "movieId") %>%
  left_join(user_bias_R , by = "userId") %>%
  group_by(genres) %>% 
  summarize(beta_g = sum(rating - mu_hat - beta_i - beta_u)/(n()+Lambda_R4))

movie_year_bias_R <- edx %>% 
  left_join(movie_avg_R, by= "movieId") %>%
  left_join(user_bias_R, by= "userId") %>% 
  left_join(genres_bias_R, by= "genres") %>%
  group_by(movie_year) %>%
  summarize(beta_d = sum(rating - mu_hat - beta_i - beta_u - beta_g)/(n()+ Lambda_R5))

predicted_ratings_R5 <- validation %>% 
  left_join(movie_avg_R, by= "movieId") %>%
  left_join(user_bias_R, by= "userId") %>%
  left_join(genres_bias_R, by= "genres")%>%
  left_join(movie_year_bias_R, by= "movie_year") %>%
  mutate(pred = mu_hat + beta_i + beta_u + beta_g + beta_d) %>%
  pull(pred)

# Model R6 using the EDX Data

movie_rating_year_R <- edx %>% 
  left_join(movie_avg_R , by= "movieId") %>%
  left_join(user_bias_R , by= "userId") %>% 
  left_join(genres_bias_R , by= "genres") %>%
  left_join(movie_year_bias_R , by= "movie_year") %>%
  group_by(rating_year) %>%	
  summarize(beta_r = sum(rating - mu_hat - beta_i - beta_u - beta_g - beta_d)/(n()+ Lambda_R6))	

predicted_ratings_R6  <- validation %>% 
  left_join(movie_avg_R , by= "movieId") %>%
  left_join(user_bias_R , by= "userId") %>%
  left_join(genres_bias_R , by= "genres")%>%
  left_join(movie_year_bias_t_R , by= "movie_year") %>%
  left_join(movie_rating_year_R, by= "rating_year") %>%
  mutate(pred = mu_hat  + beta_i + beta_u + beta_g + beta_d + beta_r) %>%
  pull(pred)


## Predict with the Ensemble Model

Apply the optimal ensemble weightings to generate the final RMSE




# Conclusion

6 models were trained with the training subset of the edx data. The model_R6 that contained all the attributes examined had the best RMSE. However by combining it with model_R5 in an ensemble model this achieved the best result

The final RMSE of the Ensemble model when making predictions on the Validation set is __0.86416__ and has beaten the target of __0.87750__
