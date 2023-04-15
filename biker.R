#load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)


#importing data but we have data from January to december 
#therefore we need to join all the csv file into one file


january_2022 <- read_csv("202201-divvy-tripdata.csv")
#View(january_2022)

february_2022 <- read_csv("202202-divvy-tripdata.csv")
#View(february_2022)

march_2022 <- read_csv("202203-divvy-tripdata.csv")
#View(march_2022)

april_2022 <- read_csv("202204-divvy-tripdata.csv")
#View(april_2022)

may_2022 <- read_csv("202205-divvy-tripdata.csv")
#View(may_2022)

june_2022 <- read_csv("202206-divvy-tripdata.csv")
#View(june_2022)

july_2022 <- read_csv("202207-divvy-tripdata.csv")
#View(july_2022)

august_2022 <- read_csv("202208-divvy-tripdata.csv")
#View(august_2022)

september_2022 <- read_csv("202209-divvy-tripdata.csv")
#View(september_2022)

october_2022 <- read_csv("202210-divvy-tripdata.csv")
#View(october_2022)

november_2022 <- read_csv("202211-divvy-tripdata.csv")
#View(november_2022)

december_2022 <- read_csv("202212-divvy-tripdata.csv")
#View(december_2022)

#Data Validation

colnames(january_2022)
colnames(february_2022)
colnames(march_2022)
colnames(april_2022)
colnames(may_2022)
colnames(june_2022)
colnames(july_2022)
colnames(august_2022)
colnames(september_2022)
colnames(october_2022)
colnames(november_2022)
colnames(december_2022)

# Total number of rows [1] 5667717
sum(nrow(january_2022) + nrow(february_2022) 
    + nrow(march_2022) + nrow(april_2022) + nrow(may_2022) 
    + nrow(june_2022) + nrow(july_2022) + nrow(august_2022)
    + nrow(september_2022) + nrow(october_2022) + nrow(november_2022) + nrow(december_2022))


# Combine Data of 12 month into for smooth workflow
trip_final <- rbind(january_2022,february_2022,march_2022,april_2022,
                    may_2022,june_2022,july_2022,august_2022,september_2022,october_2022,november_2022,december_2022)

# Save the combined files 
#but the file is too large it can not be saved and read by the system therefore we need to clean the data to proceed
#write_csv(trip_final, file = "biker")


# Setting global variable size to inf
options(future.globals.maxSize = Inf)


#Final data validation
str(trip_final)
View(head(trip_final))
View(tail(trip_final))
dim(trip_final)
summary(trip_final)
names(trip_final)


#Data Cleaning

#Count rows with "na" values
colSums(is.na(trip_final))



#Remove missing
clean_trip_final <- trip_final[complete.cases(trip_final),]
#Remove duplicates
clean_trip_final1 <- distinct(clean_trip_final)
#Remove data with greater start_at than end_at
clean_trip_final2<- clean_trip_final1 %>% 
  filter(started_at < ended_at)
#Remove na
clean_trip_final3 <- drop_na(clean_trip_final2)
clean_trip_final4 <- remove_empty(clean_trip_final3)
clean_trip_final5 <- remove_missing(clean_trip_final4)


#Check Cleaned data
colSums(is.na(clean_trip_final5))
View(filter(clean_trip_final5, clean_trip_final5$started_at > clean_trip_final5$ended_at))



#Renaming column for better context
clean_trip_final6 <- rename(clean_trip_final5, costumer_type = member_casual, bike_type = rideable_type)


#Separate date in date, day, month, year for better analysis
clean_trip_final6$date <- as.Date(clean_trip_final6$started_at)
clean_trip_final6$week_day <- format(as.Date(clean_trip_final6$date), "%A")
clean_trip_final6$month <- format(as.Date(clean_trip_final6$date), "%b_%y")
clean_trip_final6$year <- format(clean_trip_final6$date, "%Y")


#Separate column for time
clean_trip_final6$time <- as.POSIXct(clean_trip_final6$started_at, format = "%Y-%m-%d %H:%M:%S")
clean_trip_final6$time <- format(clean_trip_final6$time, format = "%H:%M")


#Add ride length column
clean_trip_final6$ride_length <- difftime(clean_trip_final6$ended_at, clean_trip_final6$started_at, units = "mins")

#Select the data we are going to use
clean_trip_final7 <- clean_trip_final6 %>% 
  select(bike_type, costumer_type, month, year, time, started_at, week_day, ride_length)

#Remove stolen bikes
clean_trip_final8 <- clean_trip_final7[!clean_trip_final7$ride_length>1400,] 
clean_trip_final9 <- clean_trip_final8[!clean_trip_final8$ride_length<5,]


tail(clean_trip_final9)

#Save the cleaned data
write.csv(clean_trip_final9,file = "clean_trip_final9.csv",row.names = FALSE)


str(clean_trip_final9)
names(clean_trip_final9)
head(clean_trip_final9)
tail(clean_trip_final9)


#order the data
#clean_trip_final9$month <- ordered(clean_trip_final9$month,levels=c("Jan_22","Feb_22","Mar_22", 
                                                                  #"Apr_22","May_22","Jun_22","Jul_22", 
                                                                  #"Aug_22","Sep_22","Oct_22","Nov_22","Dec_2022"))

#clean_trip_final9$week_day <- ordered(clean_trip_final9$week_day, levels = c("Sunday", "Monday", "Tuesday", 
                                                                           #"Wednesday", "Thursday", 
                                                                           #"Friday", "Saturday"))




#Analysis:- min, max, median, average
View(describe(clean_trip_final9$ride_length, fast=TRUE))


#Total no. of customers
View(table(clean_trip_final9$costumer_type))

#Total rides for each customer type in minutes
View(setNames(aggregate(ride_length ~ costumer_type, clean_trip_final9, sum), c("customer_type", "total_ride_len(mins)")))



#Differences between members and casual riders in terms of length of ride
View(clean_trip_final9 %>% 
       group_by(costumer_type) %>% 
       summarise(min_length_mins = min(ride_length), max_length_min = max(ride_length),
                 median_length_mins = median(ride_length), mean_length_min = mean(ride_length)))



#Average ride_length for users by day_of_week and Number of total rides by day_of_week
View(clean_trip_final9 %>% 
       group_by(week_day) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))



#Average ride_length by month
View(clean_trip_final9 %>% 
       group_by(month) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#Average ride length comparison by each week day according to each customer type
View(aggregate(clean_trip_final9$ride_length ~ clean_trip_final9$costumer_type + 
                 clean_trip_final9$week_day, FUN = mean))


#Average ride length comparison by each month according to each customer type
View(aggregate(clean_trip_final9$ride_length ~ clean_trip_final9$costumer_type + 
                 clean_trip_final9$month, FUN = mean))

#Analyze rider length data by customer type and weekday
View(clean_trip_final9 %>% 
       group_by(costumer_type, week_day) %>% 
       summarise(number_of_ride = n(),
                 avgerage_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))


#Analyze rider length data by customer type and month
View(clean_trip_final9 %>% 
       group_by(costumer_type, month) %>% 
       summarise(nummber_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))


#Save the data for data visualization
write.csv(clean_trip_final9,file = "clean_trip_final_tableau.csv",row.names = FALSE)



