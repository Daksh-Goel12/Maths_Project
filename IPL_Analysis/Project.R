library(tidyverse)
library(lubridate)  
library(ggplot2) 
library(tidyr) 
library(dplyr)
library(readxl)

#Load the data
deliveries =read.csv("C:\\Users\\daksh\\Downloads\\IPL_Ball_by_Ball_2008_2022.csv\\IPL_Ball_by_Ball_2008_2022.csv")
matches = read.csv("C:\\Users\\daksh\\Downloads\\IPL_Matches_2008_2022.csv")

head(deliveries)

head(matches)

#Matches

class(matches)
str(matches)
colnames(matches)
summary(matches)


## deliveries 

class(deliveries)
str(deliveries)
colnames(deliveries)
summary(deliveries)

#Total number of matches till

count(matches)

#Which team has won most number of matches ?

matches %>% 
  group_by(WinningTeam) %>%
  summarize(wins = n() , .groups = 'drop')

#Plot the graph

matches %>% 
  group_by(WinningTeam) %>%
  summarize(wins = n(), .groups= 'drop') %>%
  ggplot(aes(x=wins, y=WinningTeam, fill=WinningTeam)) + geom_col(position="dodge") + 
  labs(x="Number of Wins", y="Team", title = "Number  of Matches by Team")

#Who has got number of man of the match awards

matches %>% 
  group_by(Player_of_Match) %>%
  summarize(awards = n())

#Top 10 player got the man of the match awards

matches %>% 
  group_by(Player_of_Match) %>%
  summarize(awards = n()) %>%
  top_n(10)

#Plot the Top 10 players man of the match

matches %>% 
  group_by(Player_of_Match) %>%
  summarize(awards = n()) %>% 
  top_n(10) %>%
  ggplot(aes(x = Player_of_Match, y=awards, fill=Player_of_Match)) + geom_col(position="dodge") +
  labs(x="Player_of_match", y = "Awards" , title = "Top 10 Player Man of the Match") + coord_flip()

#Convert the date column

matches$day <- format(as.Date(matches$Date), "%d")
matches$month <- format(as.Date(matches$Date), "%m")
matches$year <- format(as.Date(matches$Date), "%Y")

#How many seasons got in the dataset

season_count <- length(unique(matches$year))
season_count

#Which team won by wickets or runs

Runs <- matches %>% filter(WonBy == "Runs") %>%
  select('WinningTeam', 'WonBy' )

Runs
count(Runs)

Wickets <- matches %>% filter(WonBy == "Wickets") %>%
  select('WinningTeam', 'WonBy')

Wickets
count(Wickets)

#Which season has most number of matches

matches %>% 
  group_by(year) %>%
  summarize(number_of_matches = n())

#Plot the season wise number of matches 

matches %>% 
  group_by(year) %>%
  summarize(number_of_matches=n(),.groups='drop') %>%
  ggplot(aes(x=year, y= number_of_matches, fill=year)) + geom_bar(stat = "identity") +
  labs(x="Season",y="Number ff Matches", title ="Season wise number of matches")

#In season from 2011 to 2013 the matches played are above 60..

#Which Team is dominating in certain cities

matches %>% 
  filter(WonBy != 'No result') %>%
  group_by(WinningTeam,City) %>%
  summarize(wins = n(),.groups='drop') %>%
  arrange(desc(wins)) %>%
  top_n(10)

#Which team is not able to perform in the no-home locations

matches %>% 
  filter(WonBy != 'No result') %>%
  group_by(WinningTeam,City) %>%
  summarize(wins = n(), .groups='drop') %>%
  arrange(City)


#Who's the best bolwer still dates

head(deliveries)
deliveries %>% 
  group_by(bowler) %>%
  summarize(total_run = sum(total_run)) %>%
  arrange(total_run)

#Run scored and wickets lost in power play

head(matches)
head(deliveries)

#Combine both the dataset

data <- bind_rows(matches,deliveries)
head(data)

#Dataframe contains only powerplay data

power_play <- data %>%
  group_by(overs < 6)
head(power_play)

#Total powerplay runs, wickets

colnames(data)

