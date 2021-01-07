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

summary(edx)

# Plotting the amount of movies users rated
edx %>% 
  dplyr::count(userId) %>%
  ggplot(aes(n))+
  scale_x_log10()+
  geom_histogram(bins = 100,color="white")+
  xlab("Frequentie a user rated")+
  ggtitle("Number of ratings per user")+
  theme_classic()

# Determining the users with the lowest number of ratings
bottom <- edx %>% dplyr::count(userId) %>% arrange(n)

# Determining the users with the highest number of ratings
top <- edx %>% dplyr::count(userId)%>% arrange(desc(n))

# Combining these two in a table
data.frame(Bottom = bottom$n[1:5], Top = top$n[1:5])%>% 
  knitr::kable()

# Plotting the frequency movies were rated
edx %>%
  dplyr::count(movieId) %>%
  ggplot(aes(n))+
  geom_histogram(bins = 50,color="white")+
  scale_x_log10()+
  xlab("Frequentie a film was rated")+
  ggtitle("Number of ratings per film")+
  theme_classic()

# Films rated one time
edx %>% dplyr::count(movieId) %>% filter(n==1) %>%
  nrow(.)

# The amount of times a particular rating was given
edx %>%
  group_by(rating) %>% summarise(count=n()) %>% ungroup() %>%
  ggplot(aes(rating,count)) +
  geom_col() +
  xlab("Frequentie of a rating") +
  ggtitle("Number of times a rating was given") +
  theme_classic()

# The lowest and highest number of the timestamp faeture
min(edx$timestamp)
max(edx$timestamp)

# Converting the timestamp variable into years, months, days and hours
edx_timestamp <- edx %>% mutate(date_time = as_datetime(timestamp)) %>%
  mutate(year = year(date_time), month = month(date_time),
         day = day(date_time), hour = hour(date_time))

# Calculating the mean ratings per year, month, day and hour
Rating_per_year<- edx_timestamp %>% group_by(year) %>% 
  summarise( Rating = mean(rating)) %>% ungroup()
Rating_per_month <-edx_timestamp %>% group_by(month) %>% 
  summarise( Rating = mean(rating)) %>% ungroup()
Rating_per_day <- edx_timestamp %>% group_by(day) %>% 
  summarise( Rating = mean(rating)) %>% ungroup()
Rating_per_hour<- edx_timestamp %>% group_by(hour) %>% 
  summarise( Rating = mean(rating)) %>% ungroup()

# Plotting the average ratings per year, month, day and hour
Graf_1<-Rating_per_year%>%
  ggplot(aes(year,Rating)) +
  geom_col() +
  ggtitle("Average rating per year") +
  scale_y_continuous(limits = c(0,5))+
  theme_classic()

Graf_2<-Rating_per_month%>%
  ggplot(aes(month,Rating)) +
  geom_col() +
  ggtitle("Average rating per month") +
  scale_y_continuous(limits = c(0,5))+
  theme_classic()

Graf_3<-Rating_per_day%>%
  ggplot(aes(day,Rating)) +
  geom_col() +
  ggtitle("Average rating per day of the month") +
  scale_y_continuous(limits = c(0,5))+
  theme_classic()

Graf_4<-Rating_per_hour%>%
  ggplot(aes(hour,Rating)) +
  geom_col() +
  ggtitle("Average rating per hour") +
  scale_y_continuous(limits = c(0,5))+
  theme_classic()

ggarrange(Graf_1, Graf_2, Graf_3, Graf_4,
          ncol = 2, nrow = 2)


# Most frequently rated genres
edx %>% group_by(genres) %>% summarise( Rating = mean(rating), times_rated = n())%>% ungroup()%>%
  arrange(desc(times_rated)) %>% top_n(10) %>% knitr::kable()

# Highest rated genres
edx %>% group_by(genres) %>% summarise( Rating = mean(rating), times_rated = n())%>% ungroup() %>% 
  arrange(desc(Rating)) %>% top_n(10) %>% knitr::kable()

# Plot of the year of release of the film and their average ratings
edx %>% mutate(year = as.numeric(str_sub(edx$title,-5,-2))) %>%
  group_by(year) %>% summarise(Rating = mean(rating)) %>% ungroup() %>%
  ggplot(aes(year,Rating))+
  scale_x_continuous()+
  geom_point()+
  geom_smooth(method = "loess")

# Setting a seed for randomization
set.seed(123, sample.kind = "Rounding")

# Guessing the rating
y_hat <- sample(seq(0.5 ,5 ,0.5), nrow(validation) , replace = TRUE)

# Calculating the RMSE using random guessing
RMSE_by_guessing <- rmse(validation$rating, y_hat)

# We will save this RMSE value for comparing purposes
(RMSE <- data.frame( Method = "RMSE by guessing",
                     Result = round(RMSE_by_guessing,4))) %>% knitr::kable()

#  The average of all ratings
mu <- mean(edx$rating)

# Predicting the average
y_hat <- rep(mu, nrow(validation))

# Calculating the RMSE using this method
RMSE_by_average <- rmse(validation$rating, y_hat)

# We will save this RMSE value for comparing purposes
(RMSE <- bind_rows(RMSE, data.frame(Method = "RMSE by average rating", Result = round(RMSE_by_average,4)))) %>% 
  knitr::kable()

# Calculating the average rating per movie
edx_MovieRating <- edx %>% group_by(movieId) %>% 
  summarise( MovieRating = mean(rating)) %>% ungroup()

# Predicting the average rating for every movie in the validation set
Validation_MovieRating <- left_join(validation, edx_MovieRating, by = "movieId")

# Calculating the RMSE using this method
RMSE_by_movie_average <- rmse(validation$rating, Validation_MovieRating$MovieRating) 

# We will save this RMSE value for comparing purposes
(RMSE <- bind_rows(RMSE, data.frame( Method = "RMSE by movie averages", Result = round(RMSE_by_movie_average,4)))) %>% knitr::kable()

# Calculating the average rating per user
edx_UserRating<-edx %>% group_by(userId) %>% 
  summarise(UserRating=mean(rating)) %>% ungroup()

# Predicting the average rating for every user in the validation set
Validation_UserRating<-left_join( validation, 
                                  edx_UserRating, by = "userId")

# Calculating the RMSE using this method
RMSE_by_user_average <- rmse(validation$rating, 
                             Validation_UserRating$UserRating) 

# We will save this RMSE value for comparing purposes
(RMSE <- bind_rows(RMSE, data.frame( Method = "RMSE by user averages",
                                     Result = round(RMSE_by_user_average,4))))%>%
  knitr::kable()

# For conveiniance we will use the mu as the average of all ratings,b_i as the movie bias and b_u as the user bias. 

mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu)) %>% ungroup()

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean( rating - b_i - mu)) %>% ungroup()

# Joining the predictions with the movies en user in the validation set
y_hat <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  summarise(y_hat <- mu + b_i + b_u) %>%
  .$y_hat

# Calculating the RMSE using this method
RMSE_by_user_movie_average <- rmse(validation$rating, y_hat)

# We will save this RMSE value for comparing purposes
(RMSE <- bind_rows(RMSE, data.frame( Method = "RMSE by user and movie averages", 
                                     Result = round(RMSE_by_user_movie_average,4))))%>%
  knitr::kable()

# Again for conveiniance we will use the mu as the average of all ratings, b_i as the movie bias and b_u as the user bias Now we will add b_g for the genre bias and b_y for the year variable

mu <- mean(edx$rating)

edx <- edx %>% mutate(year = as.numeric(str_sub(edx$title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(validation$title,-5,-2)))

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu)) %>% ungroup()

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - b_i - mu)) %>% ungroup()

b_g <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres)%>%
  summarise(b_g = mean(rating - b_i - b_u - mu)) %>% ungroup()

b_y <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by= "genres")%>%
  group_by(year) %>%
  summarise(b_y = mean(rating - b_i - b_u - b_g - mu)) %>% ungroup()

# Joining the predictions with the movies, users, genres and year of release in the validation set

y_hat <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year")%>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  .$pred

# Calculating the RMSE using this method
RMSE_by_movie_user_genre_year <- rmse(validation$rating,y_hat)

# We will save this RMSE value for comparing purposes
(RMSE <- bind_rows(RMSE, data.frame( Method = "RMSE by user, movie, enre and year", 
                                     Result = round(RMSE_by_movie_user_genre_year,4))))%>%
  knitr::kable()

# Making a list of lambdas
lambdas <- seq(0,5,0.5)

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

# Plotting which lambda results in the lowest RMSE
plot(lambdas,fun_lambdas)

# Establishing the best lambda parameter
final_lambda <- lambdas[which.min(fun_lambdas)]

# Calculating all averages and implementing them and incorparating the parameter lambda

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

# Calculating the RMSE using this method
RMSE_with_regularization <- rmse(validation$rating,predicted_ratings)

# We will save this RMSE value for comparing purposes
(RMSE <-  bind_rows(RMSE, data.frame( Method = "RMSE with regularization", 
                                      Result = round(RMSE_with_regularization,4)))) %>% 
  knitr::kable()

RMSE %>% knitr::kable()