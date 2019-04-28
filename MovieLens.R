#############################################################
# Create edx set, validation set, and submission file
############################################################# 

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

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

set.seed(1)
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

#-----------------------------------------------------------------------
edx<-readRDS("edx.rds")
validation<-readRDS("validation.rds")

#question 1
dim.data.frame(edx)


#question 2
sum(edx$rating == 0)
sum(edx$rating == 3)
# official answer "edx %>% filter(rating == 0) %>% tally()"

#question3
a<-unique(unlist(edx$movieId))
length(a)
#official answer "n_distinct(edx$movieId)"

#question 4
n_distinct(edx$userId)

#question 5
genres<-separate_rows(edx, genres, sep = "[|]")
sum(genres$genres == "Drama")
sum(genres$genres == "Comedy")
sum(genres$genres == "Thriller")
sum(genres$genres == "Romance")
# officail answer 
# edx %>% separate_rows(genres, sep = "\\|") %>%
# group_by(genres) %>%
#   summarize(count = n()) %>%
#   arrange(desc(count))

#question 6
sum(edx$title == "Forrest Gump (1994)")
sum(edx$title == "Jurassic Park (1993)")
sum(edx$title == "Pulp Fiction (1994)")
sum(edx$title == "Shawshank Redemption, The (1994)")
sum(edx$title == "Speed 2: Cruise Control (1997)")
#official answer
# edx %>% group_by(movieId, title) %>%
#   summarize(count = n()) %>%
#   arrange(desc(count))

#question 7
ratings<-data.frame(rating=seq(0,5,by=0.5))
ratings$num[1]<- sum(edx$rating == 0)
ratings$num[2]<-sum(edx$rating == 0.5)
ratings$num[3]<-sum(edx$rating == 1)
ratings$num[4]<-sum(edx$rating == 1.5)
ratings$num[5]<-sum(edx$rating == 2)
ratings$num[6]<-sum(edx$rating == 2.5)
ratings$num[7]<-sum(edx$rating == 3)
ratings$num[8]<-sum(edx$rating == 3.5)
ratings$num[9]<-sum(edx$rating == 4)
ratings$num[10]<-sum(edx$rating == 4.5)
ratings$num[11]<-sum(edx$rating == 5)
#official answer
# edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
#   arrange(desc(count))

#queation 8
half<-sum(c(ratings$num[2],ratings$num[4],ratings$num[6],ratings$num[8],ratings$num[10]))
whole<-sum(c(ratings$num[1],ratings$num[3],ratings$num[5],ratings$num[7],ratings$num[9],ratings$num[11]))
whole/half
#official answer
# edx %>%
#   group_by(rating) %>%
#   summarize(count = n()) %>%
#   ggplot(aes(x = rating, y = count)) +
#   geom_line()

