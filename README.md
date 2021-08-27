# GoogleDataAnalyticsCyclisticCaseStudy


---
title: "CyclisticCaseStudy"
author: "Madhu Santhanam"
date: "21/08/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# CYCLISTIC DATA ANALYSIS
### Cyclistic Data Analysis to determine how annual members and casual riders use the Cyclistic bike share program differently
## Executive summary: 
Cyclistic is a bike share program with more than 5,800 bicylces of three kinds and over 600 docking stations in Chicago. There are two types of riders - members and casual. Members have annual membership with Cyclistic whereas casual riders either get a day pass or single-ride pass. The company's financial analysts have concluded that annual members are much more profitable than casual riders.The director of marketing wants her team to analyze the data and tell her how to increase the membership.In order to do that we need to understand how the two rider types differ in using the bike share program , why would casual riders buy memberships and how can the company use digital media to influence casual riders to become members.

## Business task: The Ask
The director of Marketing, my manager, wants me, a junior data analyst in her team, to analyse and answer the question: how do annual members and casual riders use the Cyclistic bikes differently?

## Prepare:
In order to complete the analysis and to answer the question, I will use the past 12 months' ridership data from the data source: <https://divvy-tripdata.s3.amazonaws.com/index.html>. As the data is a first party data, the data source is deemed trust worthy. The data is in comma separated format.The data includes unique ride_ID for each ride, date and time the rides started and ended, starting and ending station name, id, latitude and longitude. The data does not contain any personally identifiable information about the riders. 

There are some nulls/blanks in the data which we will ignore while doing our analysis as they are less than 0.001% of the total data.

## Process

I downloaded the data into one of my folders and converted them to .xlsx files. I removed any rows that had a start_at time later than the end_at time as they are the data from Cyclistic employees checking the bikes for quality and not actual ride data.
As this is a large amount of  data of around 4.45 million observations, I will be using R to complete the analysis.

First I begin by installing the required packages ( tidyverse and readxl) necessary for the analysis and setting up the working directory.

```{r SetUp, results = "hide"}
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

setwd("~/Cyclistic data")
```

### Importing the 12 monthly data in xlsx into R

Data for files from the month of July 2020 to June 2021 were imported into R for analysis

```{r}
jan21 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/012021CleanData.xlsx")
                                                                                                                         
feb21 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/022021CleanData.xlsx")
                                                                                                                          

mar21 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/032021CleanData.xlsx")
                                                                                                                       
apr21 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/042021CleanData.xlsx")
                                                                                                                         
may21 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/052021CleanData.xlsx")
                                                                                                                       
jun21 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/062021CleanData.xlsx")
                                                                                                                          
jul20 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/072020CleanData.xlsx")
                                                                                                                          
aug20 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/082020CleanData.xlsx")
                                                                                                                           
sep20 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/092020CleanData.xlsx")
                                                                                                                           
oct20 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/102020CleanData.xlsx")
                                                                                                                         
nov20 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/112020CleanData.xlsx")
                                                                                                                           
dec20 <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Google Data Analytics/CleanData Cyclistic/122020CleanData.xlsx")
 
```

Checking the data to see if all the files have the same data types for all the variables before combining the 12 files.Cleaning the files to have the same data types. 

```{r results = "hide"}
nov20 <- mutate(nov20, start_station_id = as.character(start_station_id), end_station_id =as.character(end_station_id))
str(nov20)

oct20 <- mutate(oct20, start_station_id =as.character(start_station_id), end_station_id = as.character(end_station_id))
str(oct20)

sep20 <- mutate(sep20, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
str(sep20)

aug20 <- mutate(aug20, start_station_id =as.character(start_station_id), end_station_id =as.character(end_station_id))
str(aug20)

jul20 <- mutate(jul20, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
jul20 <- mutate(jul20, started_at = as.character(started_at), ended_at = as.character(ended_at), ride_length = as.character(ride_length))
str(jul20)
```

Combining the 12 monthly  files into one data frame to get the data for one year.

```{r results = "hide"}
one_year_trip_data <- bind_rows(jul20, aug20, sep20, oct20, nov20, dec20, jan21, feb21, mar21, apr21, may21, jun21)
str(one_year_trip_data)
head(one_year_trip_data)
```

Checking the combined one year data to see the data types, column names etc.

```{r}
colnames(one_year_trip_data)
dim(one_year_trip_data)
```

Checking the data types in the combined data frame using str() function.

```{r strFunction, include=FALSE}
str(one_year_trip_data)
```


Checking to see how many different values the member_casual column and the rideable_type columns have. In other words, we are checking to see if there are only two rider types and three bike types. 

```{r}
table(one_year_trip_data$member_casual)
table(one_year_trip_data$rideable_type)
```

Converting the started_at and ended_at columns from string to date using lubridate ymd_hms 

```{r}
one_year_trip_data <- mutate(one_year_trip_data, started_at = ymd_hms(started_at), ended_at = ymd_hms(ended_at))
```

A quick check to see if the changes have happened with str() function.

```{r}
str(one_year_trip_data)
```

Removing the column ride_length and day_of_week that I previously added to the .xlsx file, because I want to calculate it within R; Renaming the resulting data frame to final_data

```{r results = "hide"}
final_data <- one_year_trip_data %>% select(-c(ride_length, day_of_week))
str(final_data)
```

Adding columns that list the date, month, day, and year of each ride (start of the ride). This will allow us to aggregate ride data for each month, day, or year. If we don't complete these operations we could only aggregate at the ride level.

```{r}
final_data$date <- as.Date(final_data$started_at) #The default format is yyyy-mm-dd
final_data$month <- format(as.Date(final_data$date), "%m")
final_data$day <- format(as.Date(final_data$date), "%d")
final_data$year <- format(as.Date(final_data$date), "%Y")
final_data$day_of_week <- format(as.Date(final_data$date), "%A")
```

Quick check to see if the new columns have been added and if the contents look right using colnames()...

```{r}
colnames(final_data)
```

...and str() functions.

```{r results = "hide"}
str(final_data)
```

Adding a "ride_length"column to calculate  the difference between between starting and ending time to final_data (in seconds).

```{r}
final_data$ride_length <- difftime(final_data$ended_at,final_data$started_at)
colnames(final_data)
```

Converting the ride_length to numeric so that we can perform calculations.

```{r}
final_data$ride_length <- as.numeric(as.character(final_data$ride_length))
```

Calculating the mean, max,min of ride_length in general for both membership types.

```{r echo=FALSE}
mean(final_data$ride_length, na.rm = TRUE)
```

rm.na = TRUE is used to tell r to ignore na values in the calculations.

Cleaning the data to remove any residual negative ride_length values and renaming the resulting data frame to clean_data.

```{r}
clean_data <- final_data[!(final_data$ride_length<0),]
```

Summarizing the ride_length to find out the min, max, median, mean etc using summary().

```{r}
summary(clean_data$ride_length)
```

Comparing the mean, median, maximum and minimum ride lengths for members and casual users.

```{r AggregateData, echo=FALSE}
aggregate(clean_data$ride_length ~ clean_data$member_casual, FUN = mean)
aggregate(clean_data$ride_length ~ clean_data$member_casual, FUN = median)
aggregate(clean_data$ride_length ~ clean_data$member_casual, FUN = max)
aggregate(clean_data$ride_length ~ clean_data$member_casual, FUN = min)
```

Ordering the days of the week in the right order as they are not in the right order in the above output. 

```{r}
clean_data$day_of_week <- ordered(clean_data$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

Checking the average ride time by each day for members vs casual users, in the right day of the week order.

```{r}
aggregate(clean_data$ride_length ~ clean_data$member_casual + clean_data$day_of_week, FUN = mean)
```

We will visualize the above results shortly below, so that it will make more sense and will be easy to understand. 

## Analyze

### Analysis of the one year Cyclistic data to see how the members and casual riders use the ride share services differently.

Analyzing the  ridership data by weekday and rider type: the below pipe creates a weekday field using wday() from started_at date, and then groups it by rider type, weekday and then, summarizes the number of rides and the average duration and then orders them by rider type first and then weekday.

```{r}
clean_data %>% 
  filter(!is.na(started_at)) %>% 
  mutate(weekday = wday(started_at, label =TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(desc(member_casual), weekday)
```

Analyzing the data to see how many total number of rides per rider type.

```{r}
clean_data %>% 
  filter(!is.na(member_casual)) %>% 
  group_by(member_casual) %>% 
  summarize(total_rides = n(), average_duration_seconds = mean(ride_length)) %>% 
  arrange(desc(member_casual))
```

It is evident from the result above that the members have more total number of rides whereas the casual riders have ride the bike longer on average than members. The reason may be due to the probability that the members are using the service more for daily commute and most of the casual riders may be tourists or visitors to the city.


Analysis to see what type of bike was popular among the two different rider types

```{r echo=FALSE}
clean_data %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides =n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) 
```

Analysis to see which are the top 5 stations for members and casual riders

```{r echo=FALSE}
clean_data %>% 
  filter(!is.na(start_station_name)) %>% 
  group_by(member_casual, start_station_name) %>% 
  summarise(most_popular_stations = n()) %>% 
  arrange(member_casual, desc(most_popular_stations)) %>% 
  top_n(5)
```


## Visualization

#### Visualizing the different scenarios such as the total number of rider by rider type by day of the week, monthly &bike type and the average ride duration by day of the week, monthly, rider type, and by bike types.

The colours in the visualizations are bright and of high contrast as they are accessible colours and makes it easier for anyone to see the difference in the colours denoting the member and the casual type of riders.

#### Visualizing the total number of rides by rider type and day of the week:

```{r echo=FALSE}
clean_data %>% 
  filter(!is.na(started_at)) %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(), average_duration = mean(ride_length)) %>% 
  arrange(desc(member_casual), weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) + 
  geom_col(position ="dodge") +
  labs(title = "Cyclistic bike ride data July 2020 to June 2021", subtitle = "Total number of users by day of the week and the rider type", caption = "Data source: https://divvy-tripdata.s3.amazonaws.com/index.html", x = "Day of the week", y = "Number of rides", fill = "Rider Type") +
  scale_fill_manual(values = c("#FD9A00", "#7A6FF0"))

```

It is clear from the data viz above that the members complete more number of rides on weekdays and the casual riders complete more rides on weekends. 

#### Visualizing the average duration of rides by rider type and day of the week:

```{r echo=FALSE}
clean_data %>% 
  filter(!is.na(started_at)) %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x= weekday, y= average_duration, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(title = "Cyclistic bike ride data July 2020 to June 2021", subtitle = "Average duration(in secs) of bike ride by day of the week and the rider type", caption = "Data source: https://divvy-tripdata.s3.amazonaws.com/index.html",x = "Day of the week", y = "Average duration of rides", fill = "Rider Type") +
  scale_fill_manual(values = c("#FD9A00", "#7A6FF0"))
```

It is obvious from the data viz above that the casual riders ride for longer duration on average than the members on any given day of the week.


Let us fix the order of the months beginning jul 20 till jun 21 before beginning to analyze the number of bike rides by each rider type by month.

```{r}
clean_data$month <- ordered(clean_data$month, levels=c("jul","aug","sep","oct","nov","dec","jan","feb", "mar","apr", "may","jun"))
```

#### Visualization of the total number of rides by month and by user type:

```{r echo=FALSE}
clean_data %>% 
  filter(!is.na(date)) %>% 
  mutate(month_name = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month_name) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month_name) %>% 
  ggplot(mapping = aes(x= month_name, y= number_of_rides, fill = member_casual, na.rm = TRUE)) +
  geom_col(position="dodge", na.rm= TRUE) + labs(title ="Cyclistic bike ride data Jul 20 to Jun 21", subtitle = "Total number of rides by month and by rider type", caption = "Data source: https://divvy-tripdata.s3.amazonaws.com/index.html", x = "Month", y = "Number of rides", fill = "Rider Type") +
  scale_fill_manual(values = c("#FD9A00", "#7A6FF0"))
  
```

The total number of rides is very low for casual members in winter months than the members. During the summer months the total number of rides are almost the same for casual riders and members.

#### Visualization of the average duration of rides by month and by user type:

```{r echo=FALSE}
clean_data %>% 
  filter(!is.na(date)) %>% 
  mutate(month_name = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month_name) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month_name) %>% 
  ggplot(mapping = aes(x= month_name, y= average_duration, fill = member_casual, na.rm = TRUE)) +
  geom_col(position="dodge", na.rm= TRUE) + labs(title ="Cyclistic bike ride data Jul 20 to Jun 21", subtitle = "Average duration (in secs) by month and by rider type", caption = "Data source: https://divvy-tripdata.s3.amazonaws.com/index.html", x = "Month", y = "Average duration of rides", fill = "Rider Type") +
  scale_fill_manual(values = c("#FD9A00", "#7A6FF0"))
```

The average duration of rides remains more or less same all through the year for members whereas it is higher during the summer vacation months of July and August for casual members. Overall, casual riders tend to ride for longer duration than the members.

#### Visualization of total rides by bike type and by rider type:

```{r echo=FALSE}
clean_data %>% 
  filter(!is.na(member_casual)) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides =n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(mapping = aes(x=rideable_type, y= number_of_rides, fill = member_casual)) + geom_col(position = "dodge") +
  labs(title = "Cyclistic bike ride data from Jul 20 to Jun 21", subtitle = "Total number of rides by bike type and rider type",caption = "Data source: https://divvy-tripdata.s3.amazonaws.com/index.html", x = "Type of bike", y = "Number of rides", fill = "Rider Type") + 
  scale_fill_manual(values = c("#FD9A00", "#7A6FF0"))
```

Both members and casual riders seem to ride the docked bike type more than the electric or classic bikes. It is hard to say the exact position of classic bikes as they were introduced only in 2021 and we only have about 6 months worth of data for them.

#### Visualization of average duration of rides by bike type and rider type:

Showing the code as well as the results below:

```{r}
clean_data %>% 
  filter(!is.na(member_casual)) %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides =n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(mapping = aes(x=rideable_type, y= average_duration, fill = member_casual)) + geom_col(position = "dodge") +
  labs(title = "Cyclistic bike ride data from Jul 20 to Jun 21", subtitle = "Average duration (in secs) of rides by bike type and rider type",caption = "Data source: https://divvy-tripdata.s3.amazonaws.com/index.html",x = "Type of bike", y = "Average duration of rides", fill = "Rider Type") +
  scale_fill_manual(values = c("#FD9A00", "#7A6FF0"))
```

Even though the members put same average duration on all the three kinds of bikes, casual riders put in longer average duration on docked bikes than the other types.

# Share

## Key findings:

* Riders with annual membership complete more number of rides than the casual riders.
* Casual members ride for longer average duration than the members with annual membership
* Average duration of the ride for members stayed almost the same on weekdays and slightly higher on weekends.
* Average ride duration of casual riders were at least twice as much as that of members on any given day.
* Total number of rides is generally higher on warmer months compared to winter months for both members and casual riders.
* Docked bike has been the most popular among both members and casual riders.

## Recommendations:
* For casual riders that are out of town ( we need more data to determine what percentage of casual riders are in-town and out-of-town) Cyclistic can introduce a weekly or bi-weekly subscription. The week can start on a Monday at 12 a.m. and end on a Sunday at 11:59:59 p.m. This will benefit both Cyclistic and the casual rider. Make this membership a little expensive compared to annual membership but cheaper than one-ride or one-day pass to make attractive annual membership for in-town riders.
* Create a weekend subscription program with casual riders that are visitors or tourists. This will give access to the bikes for Friday, Saturday and Sunday.

# Further Actions

* Collecting data on whether a casual rider is from Chicago or from out of town will be very helpful to determine the percentage of our target market among the casual riders for annual subscriptions. 

