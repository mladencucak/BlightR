########################################################
#Filter data sets with low number of missing values
########################################################
source(here::here("scr","lib",  "pkg.R"))

load(here::here("dat", "weather_infilled&sol_estim.Rdata"))

# Filter weather data we will use in  to estimate treatment frequencies 

nrow(weather)
weather <- unite(weather, id, c("stna", "year"), remove = FALSE)
wth_ls <- split(weather,  weather$id )
not_empty <-sapply(wth_ls, function(x) {nrow(x)>2}) 
wth_ls <- wth_ls[as.vector(not_empty, mode = "logical")]; rm(not_empty)

length(wth_ls)

for(x in seq_along(wth_ls)){
  # x <- 1
  # x <- "Roches Point.2018"
  # x <- "Roches Point.2007"
  # x <- "Roches Point.2001"
  # x <- "Roches Point.2001"
  df <- wth_ls[[x]]
  #sort problems with the dates
  df <- arrange(df, date_time)
  df <- df[!duplicated(df$date_time), ]
  dff <- data.frame(date_time = seq(ymd_hms(paste0(unique(df$year),"-01-01 00:00:00")),ymd_hms(paste0(unique(df$year),"-12-31 23:00:00")), by = 'hour'))
  df <- left_join(dff, df, by = "date_time")
  df$short_date <-  as.Date(df$date_time)
  df$stna <-  unique(df$stna)[!is.na(unique(df$stna))]
  df$stno <-  unique(df$stno)[!is.na(unique(df$stno))]
  df$year <- unique(df$year)[!is.na(unique(df$year))]
  df$country <-  unique(df$country)[!is.na(unique(df$country))]
  df$doy = yday(df$date_time)
  df$hour = hour(df$date_time)
  df <- df[!duplicated(df), ]
  wth_ls[[x]] <- df
  print(x)
}


weather <- bind_rows(wth_ls)
vars <- c("sun", "sun_i","drv_hr_sun_dur", "drv_hr_sun_dur_i", 
          "wdsp", "wdsp_i","glob_rad", "glob_rad_i", 
          "q10cm_soil_temp", "q10cm_soil_temp_i", 
          "vappr", "msl", "wddir_i", "wddir" )


weather <- 
  weather %>% 
  filter(month %in% 4:9) %>% 
  filter(month>4 | month == 4 & day ==30) %>% 
  filter(month<9 | month == 9 & day <= 16) %>% 
  select(-one_of(vars))


#Remove not-full&too many NAs data
wth_ls <- split(weather,  weather$id )

#Remove the data that doesnt contaill all the dates because the staion was closed/oppened 
datetimes <-  strptime(c("30.04", "16.09"), format = "%d.%m")
no_of_days <- difftime(datetimes[2], datetimes[1], units = "days")+1
wth_ls <-  wth_ls[sapply(wth_ls, function(x) length(unique(x$doy))==no_of_days)] #no of days
wth_ls <-  wth_ls[sapply(wth_ls, function(x) nrow(x)==c(no_of_days*24))] #no of hours

#Remove data with more than  1% of missing values

sapply(wth_ls, function(x) sum(is.na(x[,c( "temp", "rhum")])))%>% as.vector()
nas <- sapply(wth_ls, function(x) mean(is.na(x[,c( "temp", "rhum")])))%>% as.vector() %>% round(3)
sum(nas<0.01)
wth_ls <- wth_ls[nas<0.01]

length(wth_ls)

sapply(wth_ls, function(x) nrow(x)) %>% as.numeric()
# all(sapply(wth_ls, function(x) nrow(x)) %>% as.numeric()) 
wth_ls <- lapply(wth_ls, function(x) x[!duplicated(x),])
sapply(wth_ls, function(x) nrow(x)) %>% as.numeric() 


save(wth_ls, file= here::here("dat", "treatment_no_estim.Rdata"))


################################################333
#
#####################################################
load( file= here::here("dat", "treatment_no_estim.Rdata"))

weather <- bind_rows(wth_ls)

ie_stna <- 
unique(weather[weather$country!="NI","stna"]) %>% as.character()

ie_stna <- ie_stna[!ie_stna %in% c("Dublin Airport", "Cork Airport", "SherkinIsland")]


# Select Stations
stations <- c(
  "Katesbridge",
  "Aldergrove",
  "Ballykelly",
  "Magilligan No 2",
  "Castlederg",
  "Derrylin",
  "Derrylin Cornahoule",
  "Ballywatticock",
  #ROI stations
  ie_stna
)

length(unique(weather$stna))
length(stations)
#34

glimpse(weather)

#Remove stations whihc are not a part of the analysis
wth <- filter(weather,stna %in% stations)

#The number of station/years
unique(wth$id) %>% length()

wth_ls <- split(wth, wth$id)


sapply(wth_ls, function(x) mean(is.na(x[,c( "temp", "rhum")])))%>% 
  as.vector() %>% round(3) %>% sort()



# Total number of stations 
unique(weather$stna) %>% length()

#length
unique(wth$id) %>% length()

wth_ls <- split(wth, wth$id)

# Total number of year/stations 
lapply(wth_ls, function(x) length(unique(x$year))) %>% 
  bind_cols() %>% t() %>% sum()

#Weaher data avialability per year
# lapply(wth_ls, function(x) {
#   years <-  unique(x$year)
#   stna <- rep(unique(x$stna), length(years))
#   country <-  rep(unique(x$country), length((years)))
#   data.frame(stna = stna,years = years, country = country)
# }) %>% 
#   bind_rows() %>%
#   group_by(country) %>% 
#   ggplot() + 
#   ggridges::geom_ridgeline(aes(x=years,y=as.factor(stna),fill = country,height = 0.4),stat="identity")+
#   scale_y_discrete(name = "Station Name")+
#   ggtitle("Weather Data For The Treatment Evaluation")+
#   ggridges:: theme_ridges(center = TRUE, font_size = 10)


########################################################
#Map stations
########################################################

library("maps")
df_loc <- 
  wth %>%  
  mutate(stna = ifelse(stna == "JohnstownII", "Johnstown Castle", stna)) %>% 
  group_by(stna) %>% select(stna,lat,long, country) %>% summarise_all(unique) 

 df_loc$lab <-
   wth %>% 
  group_by(stna) %>% 
  summarise(years_available = length(unique(year))) %>% 
  unite( col = lab, c("stna", "years_available"), sep = " (") %>% 
    mutate(lab = paste0(lab, ")")) %>% 
    ungroup() %>% 
  unlist()
df_loc$open <-
  wth %>% 
  group_by(stna) %>% 
  summarise(open = min(unique(year)),
            closed = max(unique(year))) %>% 
  unite( col = lab, c("stna", "open", "closed")) %>% 
  unlist()

# ireland = fortify(map_data("world", region = "ireland"))
# ni = fortify(map_data("world", region = "uk"))
# ni <- ni[ni$subregion == "Northern Ireland",]
# ireland <- bind_rows(ireland,ni)
# 
# ggplot() + 
#   geom_polygon(data = ireland, aes(x=long, y = lat, group = group), fill = "darkolivegreen3")  +
#   coord_fixed(1.5)+
#   geom_point(data = df_loc, aes(x = long, y = lat, fill = "red", alpha = 0.8), size = 1, shape = 2) +
#   guides(fill=FALSE, alpha=FALSE, size=FALSE) +
#   ggrepel::geom_text_repel(aes(x = c(df_loc$long), y = c(df_loc$lat-0.05), label = df_loc$stna),size = 3)+ 
#   # annotate("text",x = c(df_loc$long), y = c(df_loc$lat-0.05), label = df_loc$stna, size = 2)+
#   ggthemes::theme_tufte()+
#   ggsave(file = paste0("./tmp/all_stationmap.png"), width = 15, height = 28, units = "cm")
# 
# #Plot NI 
# 
# ggplot() + 
#   geom_polygon(data = ni, aes(x=long, y = lat, group = group), fill = "darkolivegreen3")  +
#   coord_fixed(1.5)+
#   geom_point(data = df_loc[df_loc$country=="NI",], aes(x = long, y = lat, color = "red"), size = 1, shape = 2) +
#   guides(fill=FALSE, alpha=FALSE, size=FALSE) +
#   # ggrepel::geom_text_repel(aes(x = c(df_loc[df_loc$country=="NI",]$long), 
#   #                              y = c(df_loc[df_loc$country=="NI",]$lat-0.05), 
#   #                              label = df_loc[df_loc$country=="NI",]$stna),size = 3)+ 
#   annotate("text",x = c(df_loc[df_loc$country=="NI",]$long), 
#            y = c(df_loc[df_loc$country=="NI",]$lat-0.02), 
#            label = df_loc[df_loc$country=="NI",]$open, size = 2)+
#   ggthemes::theme_tufte()+
#   theme(legend.position = "none")+
#   labs(title = "Locations of weather stations in Northern Ireland")+
#   ggsave(file = paste0("./tmp/NI_stationmap.png"), width = 15, height = 15, units = "cm")
# 
# 
# 


library("ggspatial")
library("sf")
library("ggrepel")

#load the shape file
load(here::here("dat", "All_Ireland.RData"))


df_loc_sf <- 
  df_loc %>% 
  mutate(colstna = ifelse(stna %in% c("Oak Park", "Dunsany", "Moore Park", "Johnstown Castle", "Gurteen"), "Observed", "Observed and forecasted" )) %>%  
  st_as_sf( agr = "lab",coords= c( "long","lat"),remove = FALSE)


#Set coordinate reference system
st_crs(df_loc_sf) <- 
  st_crs(all_counties.sf)



ggplot() +
  geom_sf(
    data = all_counties.sf,
    color= "darkolivegreen3",
    fill = "darkolivegreen3"
  ) +
  geom_sf(data = df_loc_sf, aes(fill = colstna,color = colstna ),shape = 17, size =2) +
  geom_text_repel(data = df_loc_sf, 
                  aes(x = long, y = lat, label = lab),
                  size = 2.9
                  # nudge_x = c(1, -1.5, 2, 2, -1), 
                  # nudge_y = c(0.25, -0.25, 0.5, 0.5, -0.5)
  ) +
  scale_color_manual(name = "Data:",
                     labels = c("Observed &\nForecast","Observed"),
                     values = c("blue", "black")) +
  scale_fill_manual(name = "Data:",
                    labels = c("Observed &\nForecast","Observed" ),
                    values = c("blue", "black")) +
  theme_bw(base_family = "Roboto Condensed",
           base_size = 12) +
  coord_sf(xlim = c(-11.4, -4.6), ylim = c(51.2, 55.65), expand = FALSE) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.35, "in"),
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(x = "Longitude", y = "Latitude")+
  annotation_scale(location = "br", width_hint = 0.4) +
  theme(
    strip.background = element_blank(),
    legend.position = c(.137, .9),
    legend.box.background = element_rect(color = "black", size = .5),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )+
  
  ggsave(
    file = here::here("out" , "map.png"),
    width = 15,
    height = 15,
    units = "cm",
    dpi = 600
  )

shell.exec(here::here("out" , "map.png"))
shell.exec(here::here("out" ))

# library(leaflet)
# 
# leaflet(df_loc) %>%
#   setView(mean(df_loc$long),mean(df_loc$lat),7) %>%
#   addTiles(group = "OSM (default)") %>%
#   setView(-8,53.5,6)%>% 
#   addMarkers(lng = df_loc$long, lat = df_loc$lat, 
#              label = ~as.character(df_loc$stna) 
#              # icon=greenLeafIcon
#              # radius = 2
#   )


########################################################
#Stations selected for the treatment analysis
########################################################

#Visualisation
low_na <- 0.01
na_prop <- 0.1
p1 <- 
wth %>% 
  group_by(country, stna, year)  %>% 
   filter(country == "NI") %>% 
  dplyr::summarize(sum_NA = round(mean(is.na(c(temp,rhum, rain))),2)) %>% 
  # filter(stna== "St Angelo")
  mutate(perc_missing =  ifelse(sum_NA < low_na, paste("<", low_na), paste( low_na, "-", na_prop))) %>% 
  mutate(perc_missing = factor(perc_missing)) %>% 
  ggplot(., aes(year, stna))+
  geom_tile(aes(fill = perc_missing), alpha=0.5 )+
  scale_x_continuous(breaks = (seq(2000, 2018, 2)))+
  theme_minimal()+
  ggtitle("Data for the treatment evaluation")+
  labs(x = "", y="",fill = "Proportion missing:")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top")

#Visualisation
p2 <- 
wth %>% 
  group_by(stna, year)  %>% 
  filter(country == "IE") %>% 
  dplyr::summarize(sum_NA = round(mean(is.na(c(temp,rhum, rain))),2)) %>% 
  # filter(stna== "St Angelo")
  mutate(perc_missing =  ifelse(sum_NA < low_na, paste("<", low_na), paste( low_na, "-", na_prop))) %>% 
  mutate(perc_missing = factor(perc_missing)) %>% 
  ggplot(., aes(year, stna))+
  geom_tile(aes(fill = perc_missing), alpha=0.5 )+
  scale_x_continuous(breaks = (seq(2000, 2018, 2)))+
  theme_minimal()+
  labs(x = "Years", y="")+
  theme(legend.position = "none")+
  # ggtitle("Missing values for: rh and temp")+
  ggsave(here::here("tmp", "treatment_no_estim.png"))

cowplot::plot_grid(p1,p2, ncol = 1,
                   rel_heights = c(1,1.9))+
  ggsave(here::here("tmp", "Availability of the data for treatment eval.png"),
         width = 18, height = 18, units = "cm")


wth %>% 
  group_by(country) %>% 
  select(stna, id) %>% 
  summarise(`Number of stations` = length(unique(stna)),
            year_station = length(unique(id)))

wth %>% 
  mutate(rhum = ifelse(rhum<0, NA, rhum)) %>% 
  ggplot()+
  geom_histogram(aes(rhum),bins = 50)+
  scale_x_continuous(limits = c(50,100))+
  facet_wrap(~stna)



save(wth_ls, file= here::here("dat", "treatment_no_estim.Rdata"))
# load( file= here::here("dat", "treatment_no_estim.Rdata"))




