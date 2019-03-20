library(sp);library(dplyr);library(ggplot2);library(osmplotr);library(tidyr);library(readr)

#load data tables from DivvyMappingData.R output
divvystations <- read_csv('divvystations_mapping.csv')
evanstontrips <- read_csv('evanstontrips_mapping.csv')
evanstonstations <- read_csv('evanstonstations_mapping.csv')

#create vectors with coordinate start and end points for all bike trips in evanstontrips (trips to, from, and in evanston)
x1 <- divvystations$longitude [match (evanstontrips$start_station_id, divvystations$stn_id)]
y1 <- divvystations$latitude [match (evanstontrips$start_station_id, divvystations$stn_id)]
x2 <- divvystations$longitude [match (evanstontrips$end_station_id, divvystations$stn_id)]
y2 <- divvystations$latitude [match (evanstontrips$end_station_id, divvystations$stn_id)]

#create dataframe for ggplot mapping
ggdf <- data.frame(x1=x1,x2=x2,y1=y1,y2=y2,ntrips=evanstontrips$numtrips,
                   before1990=evanstontrips$before1990,later1989=evanstontrips$later1989)

ggdflong <- gather(ggdf, demographic, ntrips, before1990:later1989)

#rename records to proper facet titles
ggdflong$demographic[ggdflong$demographic=='before1990'] <- 'Riders Born Before 1990'
ggdflong$demographic[ggdflong$demographic=='later1989'] <- 'Riders Born 1990 or Later'

##EVANSTON MAP of JUST TRIPS WITHIN EVANSTON
#determine bounding boxes from coordinates
evanstonbbox <- get_bbox(c(-87.66, 42.02, -87.71, 42.07))

#create basemap
mapevanston <- osm_basemap (bbox = evanstonbbox, bg = 'gray40')

#extract OSM street data and overlay on map
evanstonhighway <- extract_osm_objects (key = 'highway', bbox = evanstonbbox)
mapevanston <- add_osm_objects (mapevanston, evanstonhighway, col = 'gray60')

#Map 1: Within Evanston Total Trips
#subset ggdf for trips that start AND end in evanston, plot lines with color to show trip frequency
evanstontotalplot <- mapevanston + 
  geom_segment(data = ggdf %>% 
                 filter(x1 %in% evanstonstations$longitude & 
                          x2 %in% evanstonstations$longitude),
               aes(x = x1, y = y1, xend = x2, yend = y2,col=ntrips),
               size=1.8,alpha=0.7,show.legend=TRUE) +
  scale_color_gradient(low='#3399FF',high='#FF0099',trans='log10') +
  geom_point(data=evanstonstations,aes(x=longitude,y=latitude),
             shape = 19, colour = "blue", size = 3, stroke = 3,show.legend = TRUE)+ 
  theme(legend.position="bottom",plot.title = element_text(hjust=0.5),
        plot.margin=unit(c(5.5,5.5,5.5,5.5),"points")) + 
  ggtitle('Total Evanston Divvy Trip Frequency Heatmap')+
  labs(color='Number of Divvy Trips') + scale_size(guide = "none") +
  geom_text(data=evanstonstations,size=2,color='white',
            aes(x=longitude,y=latitude,label=name)) +
  guides(color = guide_colorbar(title.position="top"))

#Map 2: Within Evanston Trips by Birth Year of Rider
evanstonageplot <- mapevanston + 
  geom_segment(data = ggdflong  %>% 
                 filter(ntrips>0) %>%
                 filter(x1 %in% evanstonstations$longitude & x2 %in% 
                          evanstonstations$longitude),
               aes(x = x1, y = y1, xend = x2, yend = y2,col=ntrips),
               size=1.4,alpha=0.7,show.legend=TRUE) +
  scale_color_gradient(low='#3399FF', high='#FF0099',trans='log10') +
  geom_point(data=evanstonstations,aes(x=longitude,y=latitude),
             shape = 21, colour = "blue", fill = "lightblue",
             size = 1, stroke = 3,show.legend = TRUE)+ 
  theme(legend.position="right",plot.title = 
          element_text(hjust=0.5),plot.margin=unit(c(5.5,5.5,5.5,5.5),"points")) + 
  ggtitle('Evanston Divvy Trips By Age')  +
  labs(color='Number of \nDivvy Trips') + scale_size(guide = "none") + 
  facet_wrap(~demographic) + 
  guides(color = guide_colorbar(title.position="top", title.hjust = 0.5))

#Now do Chicago

#determine bounding boxes from coordinates
chibbox <- get_bbox(c(-87.75,41.84,-87.6,42.07))
#create basemap
mapchi <- osm_basemap (bbox = chibbox, bg = 'gray10')

#extract OSM street data and overlay on map
chihighway <- extract_osm_objects (key = 'highway', bbox = chibbox)
mapchi <- add_osm_objects (mapchi, chihighway, col = 'gray20')
smallevanstonbbox <- get_bbox(c(-87.66, 42.03, -87.7, 42.07))
dat_B <- extract_osm_objects (key = 'highway', bbox = smallevanstonbbox)
mapchi<- add_osm_objects(mapchi,dat_B,col='grey40')

# Map 3: Leaving Evanston Total Trips

leavingevanstonplot <- mapchi + 
  geom_segment(data=ggdf %>% filter(ntrips>0) 
               #filter to start in evanston
               %>% filter(x1 %in% evanstonstations$longitude) %>% 
                 #filter out trips that start and end in evanston
                 filter(!(x1 %in% evanstonstations$longitude & x2 %in% 
                            evanstonstations$longitude)),
               aes(x = x1, y = y1, xend = x2, yend = y2,col=ntrips),
               size=0.08,alpha=1,show.legend=TRUE,
               arrow = arrow(length = unit(0.25,"cm"))) + 
  scale_color_gradient(trans= "log10", low='#3399FF', high='#FF0099') +
  geom_point(data=divvystations %>% 
               filter(longitude %in% (ggdf$x1) | longitude %in% (ggdf$x2)),
             aes(x=longitude,y=latitude),
             shape = 21, colour = "blue", fill = "lightblue",
             size = 0.1,show.legend = TRUE)+ 
  theme(legend.position="right",plot.margin=unit(c(5.5,5.5,5.5,5.5),"points")) + 
  ggtitle('Divvy Trips Leaving Evanston')  +
  labs(color='Number of \nDivvy Trips') + scale_size(guide = "none")

# Map 4: Entering Evanston Total Trips

enteringevanstonplot <- mapchi + 
  geom_segment(data=ggdf %>% filter(ntrips>0) %>% 
                 #filter to end in evanston
                 filter(x2 %in% evanstonstations$longitude) %>% 
                 #filter out trips that start and end in evanston
                 filter(!(x1 %in% evanstonstations$longitude & x2 %in% 
                            evanstonstations$longitude)), 
               aes(x = x1, y = y1, xend = x2, yend = y2,col=ntrips),
               size=.08,alpha=1,show.legend=TRUE,
               arrow = arrow(length = unit(0.25,"cm"))) + 
  scale_color_gradient(trans= "log10", low='#3399FF', high='#FF0099') +
  geom_point(data=divvystations %>% 
               filter(longitude %in% (ggdf$x1) | longitude %in% (ggdf$x2)),
             aes(x=longitude,y=latitude),
             shape = 21, colour = "blue", fill = "lightblue",
             size = 0.1,show.legend = TRUE)+ 
  theme(legend.position="right",plot.margin=unit(c(5.5,5.5,5.5,5.5),"points")) + 
  ggtitle('Divvy Trips Entering Evanston')  +
  labs(color='Number of \nDivvy Trips') + scale_size(guide = "none")

#save all plots to pdf

ggsave('evanstontotalplot.pdf',evanstontotalplot,dpi=400)
ggsave('evanstonageplot.pdf',evanstonageplot,dpi=400)
ggsave('leavingevanstonplot.pdf',leavingevanstonplot,dpi=400)
ggsave('enteringevanstonplot.pdf',enteringevanstonplot,dpi=400)