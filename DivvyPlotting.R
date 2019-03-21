library(dplyr);library(tidyverse);library(ggplot2);library(scales);library(lubridate);library(waffle);library(readr)

#read in csv from EvanstonDivvyData.R script output
#exclude trips longer than 12 hours (probably errors)
Divvy_df <- read_csv('evanstontrips_plotting.csv') %>% filter(duration < 43200)

#add column just for year and month name
Divvy_df$Year <- format(Divvy_df$starttime,"%Y")
Divvy_df$Month <- format(Divvy_df$starttime,"%b")
Divvy_df$StartDate = as.Date(Divvy_df$starttime, "%m/%d/%Y")
Divvy_df$MonthYear =  format(as.Date(Divvy_df$StartDate, format="%d-%m-%Y"),"%m/%Y")

#order chronologically
Divvy_df$Month = factor(Divvy_df$Month, levels = month.abb)

#bar plot of number of total rides colored by user type - note the contrast between winter and summer non-member users
Plot1.1 <- ggplot(Divvy_df,aes(x=Month,group=usertype,fill=usertype)) + 
  geom_bar(stat='count',position='stack') + facet_wrap(~Year) + 
  ggtitle('Monthly Divvy Trips by User Type') + ylab('Number of Trips') + 
  labs(fill='User Type') + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#bar plot of number of trips taken by month -  shows recent growth in ntrips over the summer.
Plot1.2 <- ggplot(Divvy_df,aes(x=Year,group=1)) + geom_bar(stat='count',fill="blue4") + 
  facet_wrap(~Month) + ggtitle('Number of Divvy Trips by Month, 2016-2018') + 
  ylab('Number of Trips')

#waffle plot showing proportion of trips taken by customers vs. subscribers in the month of January 2018
parts1 = c(`Subscriber` = nrow(Divvy_df %>% 
                                filter(usertype=='Subscriber') %>% 
                                filter(Year==2018) %>% filter(Month=='Jan')),
          `Customer` = nrow(Divvy_df %>%                                                                
                              filter(usertype=='Customer') %>% 
                              filter(Year==2018) %>% filter(Month=='Jan')))
Plot1.3 <- waffle(49.5*parts1/nrow(Divvy_df %>% filter(Year==2017) %>% 
                                    filter(Month=='Jan')),rows = 4,
                  colors = c("#8dd3c7", "#fb8072", "white"),
                  title='User Type for January 2018 Divvy Trips')

#waffle plot showing proportion of trips taken by customers vs. subscribers in the month of July 2018
parts2 = c(`Subscriber` = nrow(Divvy_df %>% 
                                filter(usertype=='Subscriber') %>% 
                                filter(Year==2018) %>% filter(Month=='Jul')),
          `Customer` = nrow(Divvy_df %>% 
                              filter(usertype=='Customer')%>% 
                              filter(Year==2018) %>% filter(Month=='Jul')))
Plot1.4 <- waffle(51*parts2/nrow(Divvy_df %>% filter(Year==2017) %>% 
                                  filter(Month=='Jul')),rows = 4,
                  colors = c("#8dd3c7", "#fb8072", "white"),
                  title='User Type for July 2018 Divvy Trips')

#Bar Plot of Monthly Count of Female and Male Riders 
Plot2.1 <- ggplot(Divvy_df %>% drop_na(),aes(x=Month,group=gender,fill=gender)) + 
  geom_bar(stat='count',position='stack')+ facet_wrap(~Year) + 
  ggtitle('Monthly Divvy Trips by Gender') + ylab('Number of Trips') + 
  labs(fill='Gender') + theme(text = element_text(size=20,family="georgia"),
                              axis.text.x = element_text(angle = 45, hjust = 1))

#Box and whisker plot showing the average ride duration per month of different Evanston age groups
Plot3.1 <- Divvy_df %>% drop_na() %>% mutate(age = 2019 - birthyear) %>% 
  mutate(agegroup = findInterval(age, c(10,20,30,40,50,60))) %>% 
  mutate(agegroup = recode_factor(agegroup,`1` = '>20 Years Old',`2` = '20s',`3`='30s',`4`='40s',`5`='50s',`6`='60s')) %>%  
  ggplot(aes(x=Month,y=duration)) + stat_boxplot(fill="orange", alpha=0.4) + 
  facet_wrap(~agegroup) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_log10() + ylab('Average Ride Duration') + ggtitle('Average Ride Duration Per Month by Age Group')

#save plots
ggsave('Plot1.1.pdf',Plot1.1,dpi=400,width=10,height=6)
ggsave('Plot1.2.pdf',Plot1.2,dpi=400,width=10,height=6)
ggsave('Plot1.3.pdf',Plot1.3,dpi=400,width=6,height=4)
ggsave('Plot1.4.pdf',Plot1.3,dpi=400,width=6,height=4)
ggsave('Plot2.1.pdf',Plot2.1,dpi=400,width=12,height=6)
ggsave('Plot3.1.pdf',Plot3.1,dpi=400,width=10,height=8)