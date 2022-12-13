library(tidyverse)
library(lubridate)
library(ggplot2)

tripdata <-rbind(april_20, may_20, june_20, july_20, august_20, september_20, october_20, november_20, december_20, january_21, february_21, march_21)

dim(tripdata)

str(tripdata)

head(tripdata)

colnames(tripdata)

glimpse(tripdata)

tripdata_cylist <-tripdata[!duplicated(tripdata$ride_id),]
print(paste("Removed", nrow(tripdata)- nrow(tripdata_cylist), "duplicated rows"))

tripdata_cylist$started_at <- as.POSIXct(tripdata_cylist$started_at, "%Y-%m-%d %H:%M:%S")
tripdata_cylist$ended_at <- as.POSIXct(tripdata_cylist$ended_at, "%Y-%m-%d %H:%M:%S")

tripdata_cylist$date <- as.Date(tripdata_cylist$started_at) #The default format is yyyy-mm-dd
tripdata_cylist$month <- format(as.Date(tripdata_cylist$date), "%m")
tripdata_cylist$day <- format(as.Date(tripdata_cylist$date), "%d")
tripdata_cylist$year <- format(as.Date(tripdata_cylist$date), "%Y")
tripdata_cylist$day_of_week <- format(as.Date(tripdata_cylist$date), "%A")

tripdata_cylist$ride_length <- difftime(tripdata_cylist$ended_at,tripdata_cylist$started_at)
str(tripdata_cylist)


is.factor(tripdata_cylist$ride_length)
tripdata_cylist$ride_length <- as.numeric(as.character(tripdata_cylist$ride_length))
is.numeric(tripdata_cylist$ride_length)

tripdata_cylist <- tripdata_cylist %>%
  mutate(ride_time_m = as.numeric(tripdata_cylist$ended_at - tripdata_cylist$started_at) / 60)
summary(tripdata_cylist$ride_time_m)

tripdata_cylist <- tripdata_cylist %>%
  mutate(year_month = paste(strftime(tripdata_cylist$started_at, "%Y"),
                            "-",
                            strftime(tripdata_cylist$started_at, "%m"),
                            paste("(",strftime(tripdata_cylist$started_at, "%b"), ")", sep="")))
unique(tripdata_cylist$year_month)

tripdata_cylist <- tripdata_cylist %>%
  mutate(weekday = paste(strftime(tripdata_cylist$ended_at, "%u"), "-", strftime(tripdata_cylist$ended_at, "%a")))
unique(tripdata_cylist$weekday)



tripdata_cylist_v2 <- tripdata_cylist[!(tripdata_cylist$start_station_name == "HQ QR" | tripdata_cylist$ride_length<0),]

tripdata_cylist %>%
  write.csv("tripdata_cylist_clean_data.csv")

mean(tripdata_cylist_v2$ride_length) #straight average (total ride length / rides)
median(tripdata_cylist_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(tripdata_cylist_v2$ride_length) #longest ride
min(tripdata_cylist_v2$ride_length) #shortest ride



#the four lines above can be condensed to one line using summary()
summary(tripdata_cylist_v2$ride_length)

#for a refresher let us see an overview of our v2 dataset
summary(tripdata_cylist_v2)

aggregate(tripdata_cylist_v2$ride_length ~ tripdata_cylist_v2$member_casual, FUN = mean)
aggregate(tripdata_cylist_v2$ride_length ~ tripdata_cylist_v2$member_casual, FUN = median)
aggregate(tripdata_cylist_v2$ride_length ~ tripdata_cylist_v2$member_casual, FUN = max)
aggregate(tripdata_cylist_v2$ride_length ~ tripdata_cylist_v2$member_casual, FUN = min)

tripdata_cylist_v2 %>% 
  group_by(member_casual) %>% 
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(tripdata_cylist_v2)) * 100)



aggregate(tripdata_cylist_v2$ride_length ~ tripdata_cylist_v2$member_casual + tripdata_cylist_v2$day_of_week, FUN = mean)

tripdata_cylist_v2$day_of_week <- ordered(tripdata_cylist_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(tripdata_cylist_v2$ride_length ~ tripdata_cylist_v2$member_casual + tripdata_cylist_v2$day_of_week, FUN = mean)

#creates weekday field using wday(), then groups by usertype and weekday, calculates the number of rides and average duration 
#calculates the average duration and sort

tripdata_cylist_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

tripdata_cylist_v2 %>%
  group_by(year_month) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(tripdata_cylist_v2)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            'Member x Casual Perc Difer' = members_p - casual_p)



ggplot(tripdata_cylist_v2, aes(member_casual, fill=member_casual)) +
  geom_bar() +
  labs(x="Casuals x Members", title="Chart 01 - Casuals x Members distribution")

tripdata_cylist_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col

tripdata_cylist_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)	%>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title="Total Number of Ride by Month", x = "Month", y = "Number of Rides") + theme(axis.text.x = element_text(angle = 60, hjust = 1))

tripdata_cylist_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

tripdata_member <-  filter(tripdata_cylist_v2, member_casual == "member")

tripdata_member %>% 
  group_by(rideable_type, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type, month)	%>% 
  ggplot(aes(x = month, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + labs(title="Total Number of Member Rides by Month", x = "Month", y = "Number of Member Rides") + theme(axis.text.x = element_text(angle = 60, hjust = 1))


tripdata_member %>% 
  group_by(rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(rideable_type, day_of_week)%>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + labs(title="Total Number of Member Rides by Day", x = "Week Day", y = "Number of Member Rides") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
