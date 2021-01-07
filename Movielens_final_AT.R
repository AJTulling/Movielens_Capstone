if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],title = as.character(title),genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% semi_join(edx, by = "movieId") %>% semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(imputeTS)) install.packages("imputeTS", repos = "http://cran.us.r-project.org")
if(!require(permutations)) install.packages("permutations", repos = "http://cran.us.r-project.org")
if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")

# Making a list of lambdas
lambdas <- seq(0,5,0.5)

# Calculating mu
mu <- mean(edx$rating)

# Mutating a year of release variable in the edx and validation data set
edx <- edx %>% mutate(year = as.numeric(str_sub(edx$title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(validation$title,-5,-2)))


# Making a function to determine which parameter is most effective

REG <- function(lambda){
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda)) %>% ungroup()
  
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n())) %>% ungroup()
  
  b_g <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres)%>%
    summarise(b_g = sum(rating - b_i - b_u - mu)/(n())) %>% ungroup()
  
  b_y <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by= "genres")%>%
    group_by(year)%>%
    summarise(b_y = sum(rating - b_i - b_u - b_g - mu)/(n())) %>% ungroup()
  
  predicted_ratings <- edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year")%>%
    mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
    .$pred
  
  rmse(edx$rating,predicted_ratings)
}

# Testing all the lambdas on the function
fun_lambdas <- sapply(lambdas, REG)

# Establishing the best lambda parameter
final_lambda <- lambdas[which.min(fun_lambdas)]

# Calculating all averages, implementing them and incorparating the parameter lambda

b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+final_lambda)) %>% ungroup()

b_u <- edx %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n())) %>% ungroup()

b_g <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres)%>%
  summarise(b_g = sum(rating - b_i - b_u - mu)/(n())) %>% ungroup()

b_y <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by= "genres")%>%
  group_by(year)%>%
  summarise(b_y = sum(rating - b_i - b_u - b_g - mu)/(n())) %>% ungroup()

# Joining the predictions with the movies,
# users, genres and year of release in the validation set

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year")%>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  .$pred

# Showing the predictions based on this model 
predicted_ratings

# Calculating the RMSE using this method
(RMSE_with_regularization <- rmse(validation$rating,predicted_ratings))