

########################################################
#Data
########################################################
source(here::here("scr", "lib", "pkg.R"))

load(here::here("dat", "weather_infilled&sol_estim.Rdata"))


weather[is.na(weather$sol_nasa),] %>% nrow()
weather %>% nrow()

###################################################################################
#Outbreaks
###################################################################################

#subset station data with stations present in data set
stations_ni <- read.delim(list.files(pattern = "stations_ni.txt", recursive = T))
stations_ni[stations_ni$src_id %in% unique(weather$stno),] -> stations_ni
# weather <- left_join(weather, stations_ni, by = "src_id")

outbreaks <- read_csv(file  = here::here("dat", "outbreaks_fin.csv"))

summary(outbreaks[, c("yr", "lon", "lat")])
outbreaks$date <- lubridate::mdy(outbreaks$date)
str(outbreaks)




  #Add Irish stations
stations_ni <- 
  mutate_if(stations_ni , is.factor, as.character)

stations_ie <- 
  weather %>% 
  filter(country == "IE") %>% 
  dplyr:: select(stno, stna, lat, long) %>% 
  group_by(stno) %>% 
  summarise_all(list(unique)) %>% 
  rename(src_id = stno) %>%
  rename(Name = stna) %>% 
  rename(Latitude = lat) %>% 
  rename(Longitude = long) 

# List of all available stations with geographical reference
stations_ni <- 
  bind_rows(stations_ni, stations_ie)



#find nearest neighbours
#Search for station minimum distance from the outbreak

# Computes distance using Haversine formula.
#
# Returns the result in meters.
haversine <- function( lat1, lon1, lat2, lon2, radius = 6371 ) {
  # Convert decimal degrees to radians
  lon1 = lon1 * pi / 180
  lon2 = lon2 * pi / 180
  lat1 = lat1 * pi / 180
  lat2 = lat2 * pi / 180
  
  # Haversine formula
  dlon = lon2 - lon1
  dlat = lat2 - lat1
  a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  
  return( radius * c * 1000 )
}

#Add col with station name and col with distance to outbreaks
distances <- vector(mode = "numeric")

stations_ni$closest_1st <- NA
stations_ni$dist_1st<- NA
stations_ni$closest_2nd <- NA
stations_ni$dist_2nd<- NA
stations_ni$closest_3rd <- NA
stations_ni$dist_3rd <- NA
stations_ni$closest_4th <- NA
stations_ni$dist_4th <- NA
stations_ni$closest_5th <- NA
stations_ni$dist_5th <- NA
stations_ni$closest_6th <- NA
stations_ni$dist_6th <- NA

temp_st <- stations_ni
# i=1
# y=2
for (i in seq_along(stations_ni[["Latitude"]])) {
  for (y in seq_along(stations_ni[["Latitude"]])) {
    distances[y]<-  round(haversine(stations_ni[["Latitude"]][i], 
                                    stations_ni[["Longitude"]][i],
                                    stations_ni[["Latitude"]][y],
                                    stations_ni[["Longitude"]][y] )/1000,2)
  }
  #find closest station
  #get vector of years it was operational
  #if it is not containing year of outbreak find next closest station
  
  temp_st <- stations_ni[order(distances),]
  
  stations_ni$closest_1st[i] <- temp_st$src_id[2]
  stations_ni$dist_1st[i] <- distances[order(distances)][2]
  stations_ni$closest_2nd[i] <- temp_st$src_id[3]
  stations_ni$dist_2nd[i]<- distances[order(distances)][3]
  stations_ni$closest_3rd [i] <- temp_st$src_id[4]
  stations_ni$dist_3rd[i] <- distances[order(distances)][4]
  stations_ni$closest_4th [i] <- temp_st$src_id[5]
  stations_ni$dist_4th[i] <- distances[order(distances)][5]
  stations_ni$closest_5th [i] <- temp_st$src_id[6]
  stations_ni$dist_5th[i] <- distances[order(distances)][6]
  stations_ni$closest_6th [i] <- temp_st$src_id[7]
  stations_ni$dist_6th[i] <- distances[order(distances)][7]
  rm(i,y)
}

stations_ni[ , grep("dist", names(stations_ni))] %>% summary

#subset data for each outbreak
#name list with station identifier and distance
lss <- list()


#Search for station minimum distance from the outbreak
#Add col with station name and col with distance to outbreaks
distances <- vector(mode = "numeric")
outbreaks$closest_1st <- NA
outbreaks$dist_1st<- NA
outbreaks$closest_2nd <- NA
outbreaks$dist_2nd<- NA
outbreaks$closest_3rd <- NA
outbreaks$dist_3rd <- NA
outbreaks$closest_4th <- NA
outbreaks$dist_4th <- NA
outbreaks$closest_5th <- NA
outbreaks$dist_5th <- NA
outbreaks$closest_6th <- NA
outbreaks$dist_6th <- NA
temp_st <- stations_ni
# i=1
# y=1


for (i in seq_along(1:nrow(outbreaks))) {  #get the row number for each outbreak
  for (y in seq_along(stations_ni[["Latitude"]])) {  #Sequence through operational stations 
    distances[y]<-  round(haversine(outbreaks[["lat"]][i], outbreaks[["lon"]][i],stations_ni[["Latitude"]][y],
                                    stations_ni[["Longitude"]][y] )/1000,2)  }
  #find closest station
  #get vector of years it was operational
  #if it is not containing year of outbreak find next closest station
  
  temp_st <- stations_ni[order(distances),]
  outbreaks$closest_1st[i] <- temp_st$src_id[1]
  outbreaks$dist_1st[i] <- distances[order(distances)][1]
  
  outbreaks$closest_2nd[i] <- temp_st$src_id[2]
  outbreaks$dist_2nd[i]<- distances[order(distances)][2]
  outbreaks$closest_3rd [i] <- temp_st$src_id[3]
  outbreaks$dist_3rd[i] <- distances[order(distances)][3]
  outbreaks$closest_4th [i] <- temp_st$src_id[4]
  outbreaks$dist_4th[i] <- distances[order(distances)][4]
  outbreaks$closest_5th [i] <- temp_st$src_id[5]
  outbreaks$dist_5th[i] <- distances[order(distances)][5]
  outbreaks$closest_6th [i] <- temp_st$src_id[6]
  outbreaks$dist_6th[i] <- distances[order(distances)][6]
}

# Get the weather data before outbreaks for the analysis
lss<- list()

#Select the outbreak data that could be used for further analysis and attach to the weather data subset for the outbreak
AddMetadata <- function(x,i, outbreaks, ls_name) {
  x$variety <- outbreaks[i,]$variety
  x$loc_full <- outbreaks[i,]$loc_full
  x$comments <- outbreaks[i,]$Comments
  x$id <- ls_name
  x
}

min_latent <- 3 -1  #latency period + 1 day for model calc
warning_period <- 14+1 #define length of warning period for which data will be subsetted +1 day
# i = 1
max_missing <- 0.01

#take stations with minimum distance and acceptable sum of missing values
for (i in seq_along(1:nrow(outbreaks))){
  
  #Seuence of dates before the outbreak
  dates <- seq.Date(outbreaks[i,]$date- (warning_period + min_latent),
                    outbreaks[i,]$date-min_latent, by = "day")
  #check if the full data is available
  full_data <-  length(dates)*24
  
  x <- weather[weather$short_date %in% dates & weather$stno == outbreaks[i,]$closest_1st,]
  ls_name <- paste(outbreaks[i,]$date,  outbreaks[i,]$variety,outbreaks[i,]$loc_full, outbreaks[i,]$dist_1st, sep = "//")
  if(nrow(x)!=0){x <- AddMetadata(x,i, outbreaks, ls_name)}
  
  # If there is more than max_missing missing values in the data  or the sttion does not have data for that period
  # then take the data from 2nd closest station
  if(sum(is.na(x[,c("temp", "rhum")]))/c(nrow(x[,c("temp", "rhum")])*2)>max_missing|
     nrow(x)<full_data){
    x <- weather[weather$short_date %in% dates & weather$stno == outbreaks[i,]$closest_2nd,] 
    ls_name <- paste(outbreaks[i,]$date,  outbreaks[i,]$variety,outbreaks[i,]$loc_full, outbreaks[i,]$dist_2nd, sep = "//")
    if(nrow(x)!=0){x <- AddMetadata(x,i, outbreaks, ls_name)}
  }
  # Check the 3rd closest station if the 2nd have more than max_missing of missing values or no data
  if(sum(is.na(x[,c("temp", "rhum")]))/c(nrow(x[,c("temp", "rhum")])*2)>max_missing|
     nrow(x)<full_data){
    x <- weather[weather$short_date %in% dates & weather$stno == outbreaks[i,]$closest_3rd,] 
    ls_name <- paste(outbreaks[i,]$date,  outbreaks[i,]$variety,outbreaks[i,]$loc_full, outbreaks[i,]$dist_3rd, sep = "//")
    if(nrow(x)!=0){x <- AddMetadata(x,i, outbreaks, ls_name)}
  }
  
  # Check the 4th 
  if(sum(is.na(x[,c("temp", "rhum")]))/c(nrow(x[,c("temp", "rhum")])*2)>max_missing|
     nrow(x)<full_data){
    x <- weather[weather$short_date %in% dates & weather$stno == outbreaks[i,]$closest_4th,] 
    ls_name <- paste(outbreaks[i,]$date, outbreaks[i,]$variety, outbreaks[i,]$loc_full, outbreaks[i,]$dist_4th, sep = "//")
    if(nrow(x)!=0){x <- AddMetadata(x,i, outbreaks, ls_name)}
  }
  
  # Check the 5th 
  if(sum(is.na(x[,c("temp", "rhum")]))/c(nrow(x[,c("temp", "rhum")])*2)>max_missing|
     nrow(x)<full_data){
    x <- weather[weather$short_date %in% dates & weather$stno == outbreaks[i,]$closest_5th,] 
    ls_name <- paste(outbreaks[i,]$date, outbreaks[i,]$variety, outbreaks[i,]$loc_full, outbreaks[i,]$dist_5th, sep = "//")
    if(nrow(x)!=0){x <- AddMetadata(x,i, outbreaks, ls_name)}
  }
  
  # 6th
  if(sum(is.na(x[,c("temp", "rhum")]))/c(nrow(x[,c("temp", "rhum")])*2)>max_missing|
     nrow(x)<full_data){
    x <- weather[weather$short_date %in% dates & weather$stno == outbreaks[i,]$closest_6th,] 
    ls_name <- paste(outbreaks[i,]$date, outbreaks[i,]$variety, outbreaks[i,]$loc_full, outbreaks[i,]$dist_6th, sep = "//")
    if(nrow(x)!=0){x <- AddMetadata(x,i, outbreaks, ls_name)}
  }
  
  # return an empty data frame if there is no good data to return 
  if(sum(is.na(x[,c("temp", "rhum")]))/c(nrow(x[,c("temp", "rhum")])*2)>max_missing|
     nrow(x)<full_data){
    x <- weather[0,]
  }
  lss[[i]]<- x
  names(lss)[i] <- ls_name
  
  print(paste(ls_name, i)) #for debugging
  rm(i,x,distances, ls_name)
}

#Get the proportion of missing values 
sapply(lss, function(x) sum(is.na(x[,c( "temp", "rhum")])))%>% as.vector()
sapply(lss, function(x) sum(is.na(x[,c( "sol_nasa")])))%>% as.vector()
sapply(lss, function(x) sum(is.na(x[,c( "rain")])))%>% as.vector()
sapply(lss, nrow)%>% as.vector()

#################################################################
#visualisations
#################################################################
# Visualise the distribution of distances betwen outbreaks and weather stations
dists <- 
  sapply(names(lss), function(x)str_split(x, pattern = "//")) %>% sapply(., "[[",4) %>% as.vector() %>% as.numeric()

length(dists>50)

dists[dists>30]

 tmp.lab <- 
  data.frame(lab1 = paste("Mean distance: ", 
                                  round(mean(dists), digits = 2), 
                                  " km",
                                  " \n(IQR = ",
                                  round(quantile(dists)[c(2)], digits = 2),
                          " - ",
                          round(quantile(dists)[c(4)], digits = 2),
                          ")", sep = ""))
 p_dist <- 
   
ggplot()+ 
  geom_histogram(aes(dists), 
                 bins = max(round(dists,0)),
                 colour = "black", 
                 fill = "gray")+
  egg::theme_article()+
  geom_text(data = tmp.lab, x = 27, y = 33, label = tmp.lab$lab)+
  labs(x = "The distance between weather stations and recorded outbreaks (km)",
       y = "Outbreak reports")+
  ggsave(here::here("out", "fig", "Wth distances form outbreaks.png"),
         width = 13, height = 9, units = "cm")

shell.exec(here::here("out", "fig", "Wth distances form outbreaks.png"))

# Visualise the distribution of distances betwen outbreaks and weather stations
library(MASS) 
p_out <- 
  outbreaks %>%
  mutate(mon = month(date),
         day = day(date)) %>%
  group_by(mon, day) %>%
  unite(., date, mon, day, sep = "-") %>%
  mutate(d = as.Date(date, format = "%m-%d")) %>%
  
  ggplot(aes(d)) +
  geom_histogram(colour = "black",
                 fill = "gray",
                 binwidth = 1) +
  # geom_density() +
  labs(y = "Outbreak reports",
       x = "Date") +
  theme_article() +
  ggsave(here::here("out", "fig", "Outbreaks per date.png"),
         width = 6,
         height = 4.5,
         dpi = 620)


  
  
plotls <- list(p_dist,p_out )
ggpubr::ggarrange(plotlist = plotls, 
                  widths = c(.3,.3),
                  heights = c(.5,.5),
                  labels = c("a)","b)"),
                  nrow = 2
                  # common.legend = TRUE,
                  # legend = "bottom"
                  )+
  ggsave(filename= here::here("out", "fig", "Outbreaks&Distances.png"),
         width = 5.1, height =7, dpi = 620)

shell.exec( here::here("out", "fig", "Outbreaks&Distances.png"))
#################################################################
#Sort the dataset and save it for further use
#################################################################
data_length <- sapply(lss, function(x) nrow(x)) %>% as.numeric()
lss[data_length == full_data] -> lss
length(lss)
#362
empty <- sapply(lss, function(x) nrow(x)) %>% as.numeric()
lss[empty != 0] -> lss

rm(empty, data_length, max_missing, min_latent, warning_period, y, AddMetadata,
   distance, haversine, dates, dists, full_data, temp_st, stations_ni, outbreaks)

lss <- 
lapply(lss, function(x){
  x$dist <-str_split(x$id[1], pattern = "//")[[1]][4] %>% as.numeric()
  return(x)
  }
                                                                                              
  )

   
 

save(lss, file = here::here("dat", "outbreak&weather.Rdata"))
# load( file = here::here("dat", "outbreak&weather.Rdata"))
