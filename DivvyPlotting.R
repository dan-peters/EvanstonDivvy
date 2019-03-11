library(dplyr);library(tidyverse);library(ggplot2);library(scales)

#read in csv from EvanstonDivvyData.R script output
#exclude trips longer than 12 hours (probably errors)
Divvy_df <- read_csv('evanstontrips.csv') %>% filter(duration < 43200)

#add column just for year and month name
Divvy_df$Year <- format(Divvy_df$starttime,"%Y")
Divvy_df$Month <- months(Divvy_df$starttime)
Divvy_df$StartDate = as.Date(Divvy_df$starttime, "%m/%d/%Y")
Divvy_df$MonthYear =  as.Date(Divvy_df$starttime, "%m/%Y")
#order chronologically
Divvy_df$Month = factor(Divvy_df$Month, levels = month.name)

#number of rides over time (do line plot)
Divvy_df %>% group_by(Year,Month) %>% tally()

#line plot of number of rides per day for entire period
ggplot(Divvy_df,aes(x=StartDate)) + geom_line(stat='count',col='purple') + scale_x_date(labels = date_format("%Y-%m"))+
  stat_smooth(method='auto')

#line plot of number of female and male rides per day
ggplot(Divvy_df %>% drop_na(),aes(x=StartDate,color=gender)) + 
  scale_x_date(labels = date_format("%Y-%m")) + 
  geom_line(stat='count')

#line plot of average birth year of riders
ggplot(Divvy_df  %>% drop_na(),aes(x=StartDate,y=birthyear)) + 
  scale_x_date(labels = date_format("%Y-%m")) + stat_summary(fun.y = mean, geom ='line',col='purple') +
  stat_smooth(method='auto')

#line plot of sum of length of time traveled per day 
ggplot(Divvy_df,aes(x=StartDate,y=duration)) + scale_x_date(labels = date_format("%Y-%m")) +
  stat_summary(fun.y = sum, geom ='line',col='purple')

#try the above plots but by month instead of day
