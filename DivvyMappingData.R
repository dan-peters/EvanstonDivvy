library(bikedata);library(data.table)

#create temporary file for bike data download
bikedb <- file.path (tempdir (), "bike.sqlite")

#download all divvy bike data for same time period as plots (July 2016-December 2018)
dl_bikedata (city = 'divvy', dates = 201607:201813, quiet = TRUE)

#store bikedata in temp directory created above^
store_bikedata (data_dir = tempdir (), bikedb = bikedb, quiet = TRUE)

#create variables for chicago stations and trips
stns <- bike_stations (bikedb = bikedb, city = 'ch')
ntrips <- bike_tripmat (bikedb = bikedb, city = 'ch', long = TRUE)

#subset databse based on the birthyear of riders
later1989 <- bike_tripmat (bikedb = bikedb, city = 'ch',long=TRUE,birth_year= 1990:2020)
before1990 <- bike_tripmat (bikedb = bikedb, city = 'ch',long=TRUE,birth_year= 1800:1989)

#add trip frequency for each subset as column in complete data frame
ntrips$later1989 <- later1989$numtrips
ntrips$before1990 <- before1990$numtrips
rm(later1989,before1990)

#load evanston station table from EvanstonDivvyData.R
evanston_stations <- read.csv('evanstonstations_plotting.csv')

#subset stations and trips based on evanston_stations file to only include trips starting or ending in evanston 
#(id 92 is an evanston station with slightly different coordinates in the tables, so we manually subset it)
evanstonstations <- subset(stns,longitude %in% evanston_stations$longitude | id == 92) 
evanstontrips <- subset(ntrips,start_station_id %in% evanstonstations$stn_id | end_station_id %in% evanstonstations$stn_id)

#save csv of evanstontrips and divvystations for mapping in DivvyMapping.R
write.csv(evanstontrips,"evanstontrips_mapping.csv",row.names = FALSE)
write.csv(evanstonstations,"evanstonstations_mapping.csv",row.names=FALSE)
write.csv(stns,"divvystations_mapping.csv",row.names = FALSE)