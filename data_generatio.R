library(dplyr)
library(lubridate)
library(zoo)

setwd("/home/guillaume/Documents/Polytechnique/MAP536 - Python/projects/MAP536Data")

x <- read.csv("../FinalProject-MAP536/data/train.csv.bz2", stringsAsFactors = F)
y <- read.csv("../FinalProject-MAP536/data/test.csv.bz2", stringsAsFactors = F)
head(x)
head(y)

x <- x %>% rbind(y)

x <- x %>%
  mutate(date = as.Date(DateOfDeparture)) %>% 
  select(-c(DateOfDeparture, WeeksToDeparture, std_wtd)) %>% 
  mutate(
    month = lubridate::month(date),
    weekday = as.numeric(lubridate::wday(date)) - 1,
    year = year(date),
    week = lubridate::week(date)
  )

monthly_x_dep <- x %>% 
  group_by(Departure, month) %>%
  summarise(monthly_avg_logPAX = mean(log_PAX)) %>% 
  select(Departure, month, monthly_avg_logPAX)

weekday_x_dep <- x %>% 
  group_by(Departure, weekday) %>%
  summarise(weekday_avg_logPAX = mean(log_PAX)) %>% 
  select(Departure, weekday, weekday_avg_logPAX)

monthly_x_arr <- x %>% 
  group_by(Arrival, month) %>%
  summarise(monthly_avg_logPAX = mean(log_PAX)) %>% 
  select(Arrival, month, monthly_avg_logPAX)

weekday_x_arr <- x %>% 
  group_by(Arrival, weekday) %>%
  summarise(weekday_avg_logPAX = mean(log_PAX)) %>% 
  select(Arrival, weekday, weekday_avg_logPAX)


weekday_x_dep %>% group_by(Departure) %>% summarise(nb = n()) %>% arrange(nb) 

write.csv(monthly_x_dep, "aggregated_monthly_PAX_dep.csv", quote = F)
write.csv(weekday_x_dep, "aggregated_weekday_PAX_dep.csv", quote = F)
write.csv(monthly_x_arr, "aggregated_monthly_PAX_arr.csv", quote = F)
write.csv(weekday_x_arr, "aggregated_weekday_PAX_arr.csv", quote = F)