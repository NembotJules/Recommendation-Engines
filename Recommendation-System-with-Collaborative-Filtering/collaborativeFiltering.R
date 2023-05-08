library(readr)
library(dplyr)
library(data.table)
library(reshape2)

ratings = read_csv("movie_rating.csv")
#Print the first six rows of the data set...
head(ratings)
#Printing the dimension of the data set...

dim(ratings)
str(ratings)

movie_ratings = as.data.frame(acast(ratings, title~critic, value.var ="rating"))
movie_ratings

#Calculating the similarity between users
sim_users = cor(movie_ratings[,1:6],use = "complete.obs")
View(sim_users)

#Extract Toby titles...
rating_critic = setDT(movie_ratings[colnames(movie_ratings)[6]], keep.rownames = TRUE)[]
names(rating_critic) = c('title', 'rating')
View(rating_critic)

#Isolate the non rated movies...
titles_na_critic = rating_critic$title[is.na(rating_critic$rating)]
titles_na_critic

#print the ratings of the other critics, on the movie not rated by Toby(titles_na_critic)
ratings_t = ratings[ratings$title %in% titles_na_critic,]
View(ratings_t)

#Now let's add a new variable similarity using the similarity values of each critic.
x = (setDT(data.frame(sim_users[,6]), keep.rownames = TRUE)[])
names(x) = c('critic', 'similarity')
ratings_t = merge(x = ratings_t, y=x, by = "critic", all.x = TRUE)
View(ratings_t)

#Multiply ratings with the similarity values
ratings_t$sim_rating = ratings_t$rating * ratings_t$similarity
ratings_t

#Now, for each movie with need to sum up the sim_rating value given by each user, and then divide by the sum of the similarity coefficients...
#This will give us an estimation of the ratings Toby can possibly give to the non-rated movies...
result = ratings_t %>%group_by(title)%>%
  summarise(sum(sim_rating)/sum(similarity))
result

#Calculating how much in average Toby give as ratings
mean(rating_critic$rating, na.rm = T)

#Now that we know that the average rate give by Toby is 3.166 we can recommend any movies with more than that...in this case
#We will recommend The Night Listener...
#Generating recommendations for all the users can be easily extended as follows

generateRecommendations <- function(userId) {
  rating_critic = setDT(movie_ratings[colnames(movie_ratings)[userId]],keep.rownames = TRUE)[]
  names(rating_critic) = c('title', 'rating')
  titles_na_critic = rating_critic$title[is.na(rating_critic$rating)]
  ratings_t = ratings[ratings$title %in% titles_na_critic,]
  #Add similarity for each user as new variable
  x = (setDT(data.frame(sim_users[,userId]), keep.rownames = TRUE)[])
  names(x) = c('critic', 'similarity')
  ratings_t = merge(x = ratings_t, y=x, by = "critic", all.x = TRUE)
  #Multiply rating with similarity values...
  ratings_t$sim_rating = ratings_t$rating * ratings_t$similarity
  #Predicting the rates of the non rated movies...
  result = ratings_t %>%group_by(title)%>%
    summarise(sum(sim_rating)/sum(similarity))
  return(result)
}

