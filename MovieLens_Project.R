#Movie Lens project for Harvard data science captone on Edx
#Author: Mark Richards
#Date: 4/27/29

#Load required libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(dplyr)
library(lubridate)

#check to see if test/training data exist and if not run code to build them
test<-file_test("-f","validation.rds") #checks to see if test set data exists
train<-file_test("-f","edx.rds") #checks to see if training set data exists

if (test & train == FALSE) { #run code block if onr or both files are not available
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

  set.seed(1)
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  rm(test,train)
}else { #run if files do exist
  edx<-readRDS("edx.rds") #reads the generated edx training set
  validation<-readRDS("validation.rds") #reads the generated test set
  rm(test,train)
}
#------------------------------------------

#date info--- Convert dates to only year
edx$timestamp<-date(as_datetime(edx$timestamp)) #convert timestamp to human dates
#--------------

#basic data set information
edxdim<-dim(edx)
n_distinct(edx$movieId) #number of movies
n_distinct(edx$userId) #numbr of users
#----------------

#build RMSE function to check algos
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#--------------------------


#star rating into ---- start analysis with looking at given information on ratings
hist(x=edx$rating, main = "Frequency of Star Ratings", xlab = "Star Ratings", col ="yellow", breaks = seq(0,5,0.5)) #plot of star ratings in training set
#------------


#builds plot of guessing fixed values for start rating, measures resulting RMSE and plots
fixed<-integer()
for (i in seq(0,5,0.1)){
  fixed<-append(fixed,RMSE(validation$rating,i))
  
}
plot(x=seq(0,5,0.1), y=fixed,main = "RMSE vs Fixed Guesses of Star Ratings", ylab = "RMSE", xlab = "Star Rating")
points(y=min(fixed), x=(which.min(fixed)/10)-0.1,col="Red", pch=18,bg="Red", labs="Min", cex=2)
with(text(x = (which.min(fixed)/10)-0.1, y = min(fixed)+.1, labels = "(3.5, 1.06)", pos = 3))
#----------------------------- best case is RMSE of 1.06 goal is 0.87750









#genre info----
genre<-file_test("-f","genres.rds") #checks to see if test set data exists
if (genre == FALSE) {
  genres<-separate_rows(edx, genres, sep = "[|]") #create a df showing 1 genre tag per row
  rm(genre)
}else {
  genres<-readRDS("genres.rds") #reads the generated edx training set
  rm(genre)
}

#----------

#generate df with genre averages, then plot the averages -------
genreList<-data.frame(genres=unique(genres$genres), avg=0)
for (i in seq(1,length(genreList$genres),1)){
  inter<-genres %>% filter(genres == genreList$genres[i])
  genreList$avg[i]<-mean(inter$rating)
}
rm(inter)
barplot(names.arg =genreList$genres, height =genreList$avg, las=2, main = "Average Rating by Genre", ylab = "Avg Rating")
abline(a=3.5,b=0, col="Red")

#generate frequency of indivigual genres
genres<-data.frame(table(genres$genres))
barplot(height = genres$Freq, names.arg = genres$Var1, main = "Genre frequency", las=2) #plot the indivigual genres and their frequency
saveRDS(genres, "genreFreq.rds")
saveRDS(genreList, "GenreAvg.rds")
#-----------------------------------


MovieYear<-character()
for(i in seq(1,1000,1)){  
  MovieYear<-append(MovieYear,str_match_all(edx$title[i],"\\([^\\d]*(\\d+)[^\\d]*\\)")[[1]][2])
}
MovieYear<-as.numeric(MovieYear)
hist(MovieYear,las=2)

head(edx$MovieYear)
a<-data.frame(table(edx$MovieYear))

hist(edx$timestamp, format = "%Y", breaks = 14) #distro of when revirews happen
a<-character()
a<-append(a,str_match_all(edx$title[i],"\\([^\\d]*(\\d+)[^\\d]*\\)")[[1]][2])


# if (x==3){
#   print("3")
# }else if (x==4){
#   print("4")
# }else{
#   print("5")
# }


a<-lm(rating~as.factor(movieId), data = edx) #cant allocate memory for this
mu<-mean(edx$rating)
movieAvg<-edx %>% group_by(movieId) %>% summarise(b_i=mean(rating-mu))

predicted_ratings <- mu + validation %>% 
  left_join(movieAvg, by='movieId') %>%
  .$b_i

RMSE(predicted_ratings, validation$rating)


user_avgs <- validation %>% 
  left_join(movieAvg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- validation %>% 
  left_join(movieAvg, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
RMSE(predicted_ratings, validation$rating)


#-------------------------------------
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()
#-------------------------------------------------
user_avgs <- validation %>% 
  left_join(movieAvg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()
#-------------------------------------------
