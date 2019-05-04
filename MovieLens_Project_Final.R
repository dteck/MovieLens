library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)


#--Loading training and validation sets or generating them--
test<-file_test("-f","validation.rds") #checks for test set data
train<-file_test("-f","edx.rds") #checks for training set data

if (test & train == FALSE) { #Run if one/both files are not available
  dl <- tempfile() # Code provided by Harvard EdX Capstone Course.
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  ratings <- read.table(text = gsub("::", "\t", 
                                    readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% 
    mutate(movieId = as.numeric(levels(movieId))[movieId],
           title = as.character(title),
           genres = as.character(genres))
  movielens <- left_join(ratings, movies, by = "movieId")
  # Validation set will be 10% of MovieLens data
  set.seed(1)
  test_index <- createDataPartition(y = movielens$rating, 
                                    times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  rm(test,train) #remove file checks
}else { #run if both files exist
  edx<-readRDS("edx.rds") #read training set
  validation<-readRDS("validation.rds") #read validation set
  rm(test,train) #remove file checks
}
#------


#--Converting datestamps to years--
edx$timestamp<-date(as_datetime(edx$timestamp)) #convert time to years
#------


#--Basic info about traing set data--
edxdim<-dim(edx) #dimentions of the data set
NumMovies<-n_distinct(edx$movieId) #number of movies
NumUsers<-n_distinct(edx$userId) #number of users
#------

#--Histogram of average for training set--
hist(x=edx$rating, main = "Frequency of Star Ratings", 
     xlab = "Star Ratings",
     col ="yellow", breaks = seq(0,5,0.5)) #plot ratings
#------


#--calculate average of traing set ratings-- 
trainMean<-mean(edx$rating) #average for all ratings
#------


#--Create function to calculate RMSE--
RMSE <- function(true_ratings, predicted_ratings){ #function to calc RMSE 
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#------


#--Get RMSE for prediction set to average rating--
FixedAvg<-RMSE(validation$rating,mean(edx$rating))
#------


#--Get a list of RMSE's for different fixed values--
fixed<-integer() #initialize list
for (i in seq(0,5,0.1)){ #iterate over values 0 to 5
  fixed<-append(fixed,RMSE(validation$rating,i)) #calculate RMSE
  
}
plot(x=seq(0,5,0.1), y=fixed,main = "RMSE vs Fixed Guesses of Star Ratings",
     ylab = "RMSE", xlab = "Star Rating") #build plot of results
points(y=min(fixed), x=(which.min(fixed)/10)-0.1,col="Red",
       pch=18,bg="Red", cex=2) #add red point at lowest RMSE value
#------


#--Separeate Genre tags into indivigual rows--
genre<-file_test("-f","genres.rds") #checks data exists
if (genre == FALSE) {
  genres<-separate_rows(edx, genres, sep = "[|]") #df showing 1 genre per row
  rm(genre)
}else {
  genres<-readRDS("genres.rds") #read generated set
  rm(genre)
}
#------


#--Calculate and graph avg's for each genre--
genreList<-data.frame(genres=unique(genres$genres), avg=0)
for (i in seq(1,length(genreList$genres),1)){
  inter<-genres %>% filter(genres == genreList$genres[i])
  genreList$avg[i]<-mean(inter$rating)
}
rm(inter)
barplot(names.arg =genreList$genres, height =genreList$avg, las=2, 
        main = "Average Rating by Genre", ylab = "Avg Rating")
abline(a=3.5,b=0, col="Red")
#------


#--Plot frequency of genre tags--
genres<-data.frame(table(genres$genres))
barplot(height = genres$Freq, names.arg = genres$Var1, main = "Genre frequency", las=2) 
#------


#--Calculate movie averages and plot difference from training set mean--
movieAvg <- edx %>% group_by(movieId) %>% summarize(movieAvg = mean(rating - trainMean))
hist(movieAvg$movieAvg, main = "Movie Avg Difference From Overall Average", 
     xlab = "Movie Average Difference", col="Yellow")
#------


#--build predictions where movies averages add/subtract from set mean, get RMSE--
moviePredict <- trainMean + validation %>% left_join(movieAvg, by='movieId') %>%
  .$movieAvg

movieRMSE <- RMSE(moviePredict,validation$rating)
#------


#--calcuate user averages, plot user averages against total avg--
userEffect<-edx %>% group_by(userId) %>% 
  summarize(userEffect = mean(rating- trainMean)) %>% 
  filter(n()>=100) 

plot(y=userEffect$userEffect, x=userEffect$userId, 
     main = "Difference in Average Star Rating by User", 
     ylab = "Difference from Overall Average Rating", xlab = "User ID")
abline(a=0,b=0, col="Red")
#------


#--Build user predictions and get RMSE--
userPredict <- trainMean + validation %>% left_join(userEffect, by='userId') %>%
  .$userEffect
userRMSE <- RMSE(userPredict,validation$rating)
#------


#--build combined movie/user effect model, get RMSE--
usermovie <- validation %>% 
  left_join(movieAvg, by='movieId') %>%
  left_join(userEffect, by='userId') %>%
  mutate(pred = trainMean + movieAvg + userEffect) %>%
  .$pred

combinRMSE <- RMSE(usermovie, validation$rating)
#------


#--Add regularization to movie/user model, get RMSE--
lambda<-3
movieRegular <- edx %>% group_by(movieId) %>%
  summarize(movieRegular = sum(rating - trainMean)/(n()+lambda))
userRegular <- edx %>% 
  left_join(movieRegular, by="movieId") %>% group_by(userId) %>%
  summarize(userRegular = sum(rating - movieRegular - trainMean)/(n()+lambda))
predicted_ratings <- validation %>% 
  left_join(movieRegular, by = "movieId") %>%
  left_join(userRegular, by = "userId") %>%
  mutate(pred = trainMean + movieRegular + userRegular) %>% .$pred
regularized<-RMSE(predicted_ratings, validation$rating)
#------

#--Results--
Results<-data.frame(Method=c("Overall Average","Movie Effects","User Effects","Combined Movie/User","Regularized Movie/User"), RMSE=c(FixedAvg,movieRMSE,userRMSE,combinRMSE,regularized))
Results
#------