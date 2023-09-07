library(readxl)
library(tidyverse)
#install.packages("purr)")
#install.packages("Hmisc")
#install.packages("MASS")

View(Movies)

#Make copy of database

movies<-Movies

#Explore data/ structure/ descriptive stats
str(movies)
summary(movies)

library(Hmisc)
describe(movies)

#Plot Data to understand distribution
library(purrr)
library(tidyr)
library(ggplot2)

movies %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


movies %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot()

#Visual to understand the most popular genre and least popular
countTopGenre<-table(movies$Top_Genre)
barplot(countTopGenre,xlab="Top Genre")
countTopGenre[which.max(countTopGenre)]
countTopGenre[which.min(countTopGenre)]

##Visual to understand the most popular production comapny
countProd<-table(movies$Main_production_company)
barplot(countProd,xlab="Production Companies")
countProd[which.max(countProd)]
countProd[which.min(countProd)]

##Visual to understand the most popular production country
countCountry<-table(movies$Top_production_country)
barplot(countCountry,xlab="Production Countries")
countCountry[which.max(countCountry)]
countCountry[which.min(countCountry)]

#Plot line graphs to view trend overtime
movies %>% ggplot()+
  geom_line(aes(x=release_date, y=budget))

movies %>% ggplot()+
  geom_line(aes(x=release_date, y=popularity))

movies %>% ggplot()+
  geom_line(aes(x=release_date, y=revenue))

movies %>% ggplot()+
  geom_line(aes(x=release_date, y=runtime))

movies %>% ggplot()+
  geom_line(aes(x=release_date, y=vote_count))

movies %>% ggplot()+
  geom_line(aes(x=release_date, y=vote_average))

#Understand strong correlations between variables
cor(movies %>%
      keep(is.numeric))

#Plot line graphs to understand strong correlations
movies %>% ggplot()+
  geom_line(aes(x=revenue, y=budget))

movies %>% ggplot()+
  geom_line(aes(x=vote_count, y=revenue))

movies %>% ggplot()+
  geom_line(aes(x=budget, y=vote_average))

movies %>% ggplot()+
  geom_line(aes(x=revenue, y=vote_average))

movies %>% ggplot()+
  geom_line(aes(x=popularity, y=vote_average))

movies %>% ggplot()+
  geom_line(aes(x=budget, y=popularity))

movies %>% ggplot()+
  geom_line(aes(x=revenue, y=popularity))

#Regression to predict Movie Revenue (R-Squared of .7, this is the best model)
install.packages("MASS")
library(MASS)

full_model1<-lm(revenue ~., data = movies %>%
                 keep(is.numeric))
step_model1<-stepAIC(full_model1, direction="both", trace=TRUE)

summary(step_model1)

#Regression to predict Movie Budget

full_model2<-lm(budget ~., data = movies %>%
                  keep(is.numeric))
step_model2<-stepAIC(full_model2, direction="both", trace=TRUE)

summary(step_model2)

#Regression to predict Movie vote_average (worst model)

full_model3<-lm(vote_average ~., data = movies %>%
                  keep(is.numeric))
step_model3<-stepAIC(full_model3, direction="both", trace=TRUE)

summary(step_model3)
