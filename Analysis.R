
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

#distinct users in purchase
#1,459 users making purchase
count(ingame_purchase %>% distinct(user_id_hash))

#54,297 purchases made in total
#4,621 purchase sessions
ingame_purchase %>% summarize(total_purchase =  sum(purchases_on_date))


#group purchase by user id
purchase_peruser <- ingame_purchase %>% 
  group_by(user_id_hash) %>%
  summarize(total_purchase =  sum(purchases_on_date))


#order
purchase_peruser_top <- purchase_peruser %>%
  arrange(desc(total_purchase)) %>%
  filter(total_purchase >= 50)

purchase_peruser_top %>%
  summarize(total = sum(total_purchase))

#creating plot of user purchase
ggplot(purchase_peruser, aes(x = total_purchase)) + geom_histogram(bins = 100, color = "white") + xlim(c(0,500)) + ylim(c(0,400)) +labs(x = "Purchase Amount", y = "User Count") + theme(axis.text.x = element_text(size = 16), 
                                                                                                                                                        axis.text.y = element_text(size = 16),
                                                                                                                                                        axis.title.x = element_text(size = 16),
                                                                                                                                                        axis.title.y = element_text(size = 16))

#purchase trend over time
#change to overtime
purchase_bymonth <- ingame_purchase %>%
  mutate(month = format(date,"%b"))

#group for summary
purchase_bymonth <- purchase_bymonth %>%
  group_by(month) %>%
  summarize(total_monthly_purchase = sum(purchases_on_date))


#re-level factors
purchase_bymonth$month <- factor(purchase_bymonth$month,levels = c("Nov", "Dec", "Jan", "Feb", "Mar"))

#creating monthly purchase graph
Monthly_purchase_graph <- ggplot(purchase_bymonth, aes(x = month, y = total_monthly_purchase)) 
Monthly_purchase_graph + geom_bar(stat = "identity") + labs(x = "Month", y = "Total Monthly Purchase") + theme(axis.text.x = element_text(size = 16), 
                                                                                                               axis.text.y = element_text(size = 16),
                                                                                                               axis.title.x = element_text(size = 16),
                                                                                                               axis.title.y = element_text(size = 16))

#active user over time
#change to overtime
user_session_bymonth <- user_sessions %>%
  mutate(month_session = format(session_date,"%b")) %>%
  mutate(month_install = format(install_date, "%b"))

#re-level factors
user_session_bymonth$month_session <- factor(user_session_bymonth$month_session,levels = c("Nov", "Dec", "Jan", "Feb", "Mar"))
user_session_bymonth$month_install <- factor(user_session_bymonth$month_install,levels = c("Nov", "Dec", "Jan", "Feb", "Mar"))

#group for summary
user_session_bymonth_install <- user_session_bymonth %>%
  group_by(month_install) %>%
  summarize(total_monthly_purchase = sum(purchases_on_date))

#active users by month
active_users <- user_session_bymonth %>%
  select(user_id_hash, month_session)

#keep only unique activity
active_users <- unique(active_users)


#draw monthly unique user graph
Monthly_user_graph <- ggplot(active_users, aes(x = month_session))  + geom_bar()
Monthly_user_graph + geom_bar() + labs(x = "Month", y = "Active Users within that Month") + theme(axis.text.x = element_text(size = 16), 
                                                                                                               axis.text.y = element_text(size = 16),
                                                                                                               axis.title.x = element_text(size = 16),
                                                                                                               axis.title.y = element_text(size = 16))



#new users by month
newusers <- user_session_bymonth %>%
  select(user_id_hash, month_install)

#keep only unique users
newusers <- unique(newusers)

#draw new users user graph
Installation_graph <- ggplot(newusers, aes(x = month_install))  + geom_bar()
Installation_graph + geom_bar() + labs(x = "Month", y = "New Installations within that Month") + theme(axis.text.x = element_text(size = 16), 
                                                                                                  axis.text.y = element_text(size = 16),
                                                                                                  axis.title.x = element_text(size = 16),
                                                                                                  axis.title.y = element_text(size = 16))



# session frequency overtime
ggplot(user_sessions, aes(x = session_date)) + geom_histogram(bins = 18, color = "white") + labs(x = "Date", y = "Active Session") + theme(axis.text.x = element_text(size = 16), 
                                                                                                                                           axis.text.y = element_text(size = 16),
                                                                                                                                           axis.title.x = element_text(size = 16),
                                                                                                                                           axis.title.y = element_text(size = 16))



# purchase frequency overtime
ggplot(ingame_purchase, aes(x = date)) + geom_histogram(bins = 18, color = "white") + labs(x = "Date", y = "Purchases Made") + theme(axis.text.x = element_text(size = 16), 
                                                                                                                                                     axis.text.y = element_text(size = 16),
                                                                                                                                                     axis.title.x = element_text(size = 16),
                                                                                                                                                     axis.title.y = element_text(size = 16))


#draw device graph
Device_graph <- ggplot(user_sessions, aes(x = device_type))  + geom_bar()
Device_graph + geom_bar() + labs(x = "Device Type", y = "Sessions") + theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1), 
                                                                                                  axis.text.y = element_text(size = 16),
                                                                                                  axis.title.x = element_text(size = 14),
                                                                                                  axis.title.y = element_text(size = 16)) 


#draw device graph
user_sessions_country <- user_sessions %>%
  group_by(country) %>%
  summarize(freq = n()) %>%
  filter(freq > 1000)



Country_graph <- ggplot(user_sessions_country, aes(x = country, y = freq)) 
Country_graph + geom_bar(stat = "identity") + labs(x = "Country", y = "Sessions") + theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1), 
                                                                            axis.text.y = element_text(size = 16),
                                                                            axis.title.x = element_text(size = 14),
                                                                            axis.title.y = element_text(size = 16)) 
#analysis by country
users_country <- user_sessions %>%
  select(user_id_hash, country)

#select unique entries
#31101
users_country <- unique(users_country)

#user device
users_device <- user_sessions %>%
  select(user_id_hash, device_type)

users_device <- unique(users_device)


#join user country with user payment



