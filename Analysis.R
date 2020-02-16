
#load libraries

library(readxl)
library(janitor)
library(skimr)
library(ggplot2)
library(tidyverse)

#data loading

user_sessions <- read_xlsx("dataset.xlsx", na = c("", "NA")) %>% clean_names()
ingame_purchase <- read_xlsx("dataset.xlsx", sheet = 2, na = c("", "NA")) %>% clean_names()


#overall summary
#game has been running from Nov 17 - March 18
summary(user_sessions)

#31101 distinct users
#160067 sessions
#on average 5.15 sessions per user
count(user_sessions %>% distinct(user_id_hash)) 

# session frequency overtime
ggplot(user_sessions, aes(x = session_date)) + geom_histogram(bins = 18, color = "white") + labs(x = "Date", y = "Active Session")



# purchase frequency overtime
ggplot(ingame_purchase, aes(x = date, y = )) + geom_bar(bins = 18, color = "white") + labs(x = "Date", y = "Purchases Made")


summary(ingame_purchase)


summary(user_sessions)

# user frequency overtime