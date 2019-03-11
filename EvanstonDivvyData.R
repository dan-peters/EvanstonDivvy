library(dplyr);library(data.table)

# Download Divvy data for entire Chicago network from Q3 2016 to Q4 2018 (whole period where evanston divvy stations were operational)
# remove trip duration column (there is a  data error where several entries are in character format. will recalculate later)
# set column names so that all files match

q1<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2016_Q3Q4.zip | tar -xf- --to-stdout *Trips_2016_Q3.csv")[,-5] 
q2<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2016_Q3Q4.zip | tar -xf- --to-stdout *Trips_2016_Q4.csv")[,-5] %>% setNames(colnames(q1))
q3<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2017_Q1Q2.zip | tar -xf- --to-stdout *Trips_2017_Q1.csv")[,-5] %>% setNames(colnames(q1))  
q4<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2017_Q1Q2.zip | tar -xf- --to-stdout *Trips_2017_Q2.csv")[,-5] %>% setNames(colnames(q1))
q5<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2017_Q3Q4.zip | tar -xf- --to-stdout *Trips_2017_Q3.csv")[,-5] %>% setNames(colnames(q1))
q6<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2017_Q3Q4.zip | tar -xf- --to-stdout *Trips_2017_Q4.csv")[,-5] %>% setNames(colnames(q1))
q7<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2018_Q1.zip | funzip")[,-5] %>% setNames(colnames(q1))
q8<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2018_Q2.zip | funzip")[,-5] %>% setNames(colnames(q1))
q9<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2018_Q3.zip | funzip")[,-5] %>% setNames(colnames(q1))
q10<-fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2018_Q4.zip | funzip")[,-5] %>% setNames(colnames(q1))

#combine into long format table; keep files with different time formats separate
divvy_trips_oldtime <- bind_rows(q1,q2,q3,q4,q5)
divvy_trips_newtime <- bind_rows(q7,q8,q9,q10)

#q6 has a different time format than the old or new data
divvy_trips_othertime <- q6

rm(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10)

#create vector with data of stations that are located in Evanston
#note that 2018 station metadata is not yet available, new divvy stations may be excluded from analysis
evanston_stations <- fread("curl https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2017_Q3Q4.zip | tar -xf- --to-stdout *Stations*.csv") %>% 
  subset(city == 'Evanston')

#select trips from larger data frames that begin or end in evanston
evanston_trips_old <- subset(divvy_trips_oldtime, from_station_id %in% evanston_stations$id | to_station_id %in% evanston_stations$id)
evanston_trips_new <- subset(divvy_trips_newtime, from_station_id %in% evanston_stations$id | to_station_id %in% evanston_stations$id)
evanston_trips_other <- subset(divvy_trips_othertime, from_station_id %in% evanston_stations$id | to_station_id %in% evanston_stations$id)

rm(divvy_trips_oldtime,divvy_trips_newtime,divvy_trips_othertime)

# convert time columns to date-time 
evanston_trips_old$starttime <- as.POSIXct(evanston_trips_old$starttime,
                                       format='%m/%d/%Y %H:%M:%S')
evanston_trips_old$stoptime <- as.POSIXct(evanston_trips_old$stoptime,
                                            format='%m/%d/%Y %H:%M:%S')
evanston_trips_new$starttime <- as.POSIXct(evanston_trips_new$starttime,
                                            format='%Y-%m-%d %H:%M:%S')
evanston_trips_new$stoptime <- as.POSIXct(evanston_trips_new$stoptime,
                                           format='%Y-%m-%d %H:%M:%S')
evanston_trips_other$starttime <- as.POSIXct(evanston_trips_other$starttime,
                                           format='%m/%d/%Y %H:%M')
evanston_trips_other$stoptime <- as.POSIXct(evanston_trips_other$stoptime,
                                          format='%m/%d/%Y %H:%M')

#combine reconciled old and new dataformats into one table
evanston_trips <- bind_rows(evanston_trips_old,evanston_trips_other,evanston_trips_new);rm(evanston_trips_new,evanston_trips_other,evanston_trips_old)

#calculate difference to replace trip duration column
evanston_trips$duration <- difftime(evanston_trips$stoptime,evanston_trips$starttime,units="secs")

#write csv file with cleaned data table
write.csv(evanston_trips,"evanstontrips.csv")
