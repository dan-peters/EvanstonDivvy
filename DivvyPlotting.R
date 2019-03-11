library(dplyr);library(tidyverse);library(ggplot2);library(scales);library(lubridate);library(devtools);library(waffle)

#read in csv from EvanstonDivvyData.R script output
#exclude trips longer than 12 hours (probably errors)
Divvy_df <- read_csv('evanstontrips.csv') %>% filter(duration < 43200)

#add column just for year and month name
Divvy_df$Year <- format(Divvy_df$starttime,"%Y")
Divvy_df$Month <- months(Divvy_df$starttime)
Divvy_df$StartDate = as.Date(Divvy_df$starttime, "%m/%d/%Y")
Divvy_df$MonthYear =  format(as.Date(Divvy_df$StartDate, format="%d-%m-%Y"),"%m/%Y")

#order chronologically
Divvy_df$Month = factor(Divvy_df$Month, levels = month.name)

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

#bar plot of number of rides per month
ggplot(Divvy_df,aes(x=Month,group=1)) + geom_bar(stat='count') + facet_wrap(~Year)

#bar plot of number of female and male rides per month
ggplot(Divvy_df %>% drop_na(),aes(x=Month,group=gender,fill=gender)) + 
  geom_bar(stat='count',position='dodge')+ facet_wrap(~Year)

#bar plot of number of rides by each user type - note the contrast between winter and summer non-member users
ggplot(Divvy_df,aes(x=Month,group=usertype,fill=usertype)) + 
  geom_bar(stat='count',position='dodge')+ facet_wrap(~Year)

#bar plot of sum of length of time traveled per month 
ggplot(Divvy_df,aes(x=Month,y=duration,group=1)) + stat_summary(fun.y = sum, geom ='bar',fill='purple') + 
  facet_wrap(~Year)

#bar plot of avg of length of time traveled per year 
ggplot(Divvy_df,aes(x=Year,y=duration,group=1)) + stat_summary(fun.y = mean, geom ='bar',fill='purple')

#bar plot of sum of length of time traveled each month over years: shows which months growth is happening
#growth happens may june july august september
ggplot(Divvy_df,aes(x=Year,y=duration,group=1)) + stat_summary(fun.y = sum, geom ='bar',fill='purple') + 
  facet_wrap(~Month)

#bar plot of mean of length of time traveled each month over years: shows which months growth is happening
#growth happens may june july august september
ggplot(Divvy_df,aes(x=Year,y=duration,group=1)) + stat_summary(fun.y = mean, geom ='bar',fill='purple') + 
  facet_wrap(~Month)

#same as 3 above but with ntrips instead of trip duration
ggplot(Divvy_df,aes(x=Year,group=1)) + geom_bar(stat='count') + facet_wrap(~Month)

#waffle plot of 2016 proportion of customers 
parts = c(`Subscriber` = nrow(Divvy_df %>% filter(usertype=='Subscriber') %>% filter(Year==2016)) ,
                                `Customer` = nrow(Divvy_df %>% filter(usertype=='Customer')%>% filter(Year==2016)))
waffle(45*parts/nrow(Divvy_df %>% filter(Year==2016)),rows = 4,colors = c("#fb8072", "#8dd3c7", "white"))

#waffle plot of 2017 proportion of customers 
parts = c(`Subscriber` = nrow(Divvy_df %>% filter(usertype=='Subscriber') %>% filter(Year==2017)) ,
          `Customer` = nrow(Divvy_df %>% filter(usertype=='Customer')%>% filter(Year==2017)))
waffle(45*parts/nrow(Divvy_df %>% filter(Year==2017)),rows = 4,colors = c("#fb8072", "#8dd3c7", "white"))

#waffle plot of 2018 proportion of customers 
parts = c(`Subscriber` = nrow(Divvy_df %>% filter(usertype=='Subscriber') %>% filter(Year==2018)) ,
          `Customer` = nrow(Divvy_df %>% filter(usertype=='Customer')%>% filter(Year==2018)))
waffle(45*parts/nrow(Divvy_df %>% filter(Year==2018)),rows = 4,colors = c("#fb8072", "#8dd3c7", "white"))

#waffle plot of january 2017 proportion of customers 
parts = c(`Subscriber` = nrow(Divvy_df %>% filter(usertype=='Subscriber') %>% filter(Year==2017) %>% filter(Month=='January')) ,
          `Customer` = nrow(Divvy_df %>% filter(usertype=='Customer')%>% filter(Year==2017)))
waffle(50*parts/nrow(Divvy_df %>% filter(Year==2017)),rows = 2,colors = c("#fb8072", "#8dd3c7", "white"))

#waffle plot of july 2017 proportion of customers 
parts = c(`Subscriber` = nrow(Divvy_df %>% filter(usertype=='Subscriber') %>% filter(Year==2017) %>% filter(Month=='July')) ,
          `Customer` = nrow(Divvy_df %>% filter(usertype=='Customer')%>% filter(Year==2017)))
waffle(40*parts/nrow(Divvy_df %>% filter(Year==2017)),rows = 2,colors = c("#fb8072", "#8dd3c7", "white"))


##AGE PLOTS

#average birth year of rider by month
Divvy_df %>% drop_na() %>% group_by(Month) %>% summarise(ageavg = 2018 - mean(birthyear)) %>% 
  ggplot(aes(factor(Month),ageavg)) + geom_col(fill='blue')

#age vs. trip length in each month
#new plot
Divvy_df %>% drop_na() %>% mutate(age = 2018 - birthyear) %>% 
  ggplot(aes(x=duration,y=age)) + geom_point(size=0.5,alpha=0.9,col='black') + facet_wrap(~Month) + stat_smooth(method='lm') +
  scale_x_log10() + theme_light()

#old plot
ggplot(Divvy_df  %>% drop_na(),aes(x=duration,y=birthyear)) + 
  geom_point(size=0.5,alpha=0.9) + scale_x_log10() + stat_smooth(method='lm') + facet_wrap(~Month)


  