########################################################
#Filter data sets with low number of missing values
########################################################
source(here::here("scr", "pkg.R"))

# load(here::here("dat", "weather_infilled&sol_estim.Rdata"))
 load(here::here("dat", "full_IE_data_2001_2018.RData"))


load(file=paste0("./data/weather_data/", "full_IE_data_2001_2018.RData"))


#Data set descriptions
wth_ls <- split(weather, weather$stna)

# Total number of stations 
unique(weather$stna) %>% length()

# Total number of year/stations 
lapply(wth_ls, function(x) length(unique(x$year))) %>% 
  bind_cols() %>% t() %>% sum()

#Weaher data avialability per year
lapply(wth_ls, function(x) {
  years <-  unique(x$year)
  stna <- rep(unique(x$stna), length(years))
  country <-  rep(unique(x$country), length((years)))
  data.frame(stna = stna,years = years, country = country)
}) %>% 
  bind_rows() %>%
  group_by(country) %>% 
  ggplot() + 
  ggridges::geom_ridgeline(aes(x=years,y=as.factor(stna),fill = country,height = 0.4),stat="identity")+
  
  
  scale_y_discrete(name = "Year")+
  ggtitle("Weatehr data availability per year")+
  ggridges:: theme_ridges(center = TRUE, font_size = 10)


########################################################
#Check NAs and do imputation
########################################################

infil_gap <- 12 #Maximum length of the infill gap
weather$temp <- round(na.spline(weather$temp, na.rm = FALSE, maxgap = infil_gap),1)
weather$rhum <- round(na.spline(weather$rhum, na.rm = FALSE, maxgap = infil_gap),0)
weather$rhum  <- sapply(weather$rhum, function(x) ifelse(x > 100, x <- 100, x))
rm(infil_gap)



weather %>%
  filter(month>=5&month<=10) %>% 
  group_by(stna, year) %>%
  summarise(st= min(year),
            end = max(year),
            na_rh= sum(is.na(rhum)),
            na_rain= sum(is.na(rain)),
            na_temp= sum(is.na(temp))) %>% 
  data.table::melt(id.vars = 1:4, measure.vars = 5:7) %>% 
  ggplot(.,aes(x = year, y = value, fill= variable))+
  # geom_col(aes(year, na_rh), position=position_dodge(), fill = "red", width = 0.2)+
  geom_bar( stat="identity",position=position_dodge(), width = 0.7)+
  facet_wrap(~stna, ncol = 8)+
  labs(title = "")


########################################################
#Filter data sets with lower number of missing values
########################################################

# Acceptable level of missing values for main varialbes of interest
# Select any station to get the vector of time lenght for model runs (incl. leap year) 
x <- weather[with( weather, year == 2001& stna== "Aldergrove"& doy %in% c(118:276)), ]
na_prop <- 0.03
c(x$temp,x$rhum, x$rain) %>% length() *na_prop ; rm(x)
# 343.44 na for three variables should be ok. Probably a few mising days
# Set maximum alowed missing for a single variable to 10%
na_prop_single <- 0.1


# Exclude years with too many missing values during period of interest
wth_ls <- split(weather,  list(weather$stna, weather$year) )

#First check if there are any duplicate rows
wth_ls <- lapply(wth_ls, function(x) distinct(x))


acceptable <-sapply(wth_ls, function(x) {
  x <- x[x$doy %in% c(118:276),] 
  mean(is.na(c(x$temp,x$rhum, x$rain))) <=na_prop &
    mean(is.na(x$temp)) <= na_prop_single &
    mean(is.na(x$rhum)) <= na_prop_single 
}) #Check if there are any duplicate rows
wth_ls <- lapply(wth_ls, function(x) distinct(x))
beepr::beep("fanfare")

wth_ls <- wth_ls[as.vector(acceptable, mode = "logical")]; rm(acceptable)
beepr::beep("fanfare")

weather <- bind_rows(wth_ls); rm(wth_ls)


#Visualisation
low_na <- 0.01
weather %>% 
  group_by(stna, year)  %>% 
  filter(month %in% c(4:10)) %>% 
  filter(country == "NI") %>% 
  dplyr::summarize(sum_NA = round(mean(is.na(c(temp,rhum, rain))),2)) %>% 
  # filter(stna== "St Angelo")
  mutate(perc_missing =  ifelse(sum_NA < low_na, paste("<", low_na), paste( low_na, "-", na_prop))) %>% 
  mutate(perc_missing = factor(perc_missing)) %>% 
  ggplot(., aes(year, stna))+
  geom_tile(aes(fill = perc_missing), alpha=0.5 )+
  scale_x_continuous(breaks = (seq(2000, 2018, 2)))+
  theme_minimal()+
  ggtitle("Missing values for: rain, rh, temp.")

rm(na_prop, na_prop_single)



weather[weather$stna %in% c("Ballyhaise"),] %>% tail()

weather[!(weather$stna %in% c("Altnahinch Filters", "")),]$stna %>% unique() %>% sort()

#visualy explore NAs per station/year
for (i in seq_along(unique(weather$stna))) {
  y <- unique(weather$stna)[i]
  # png(file=paste0("./temp/EB_analysis/nas/", "NAs_", y,".png"))
  plot <-
    weather[weather$stna==y,] %>% 
    group_by( year)  %>% 
    summarize(temp = sum(is.na(temp)),
              rhum = sum(is.na(rhum)),
              rain=sum(is.na(rain)),
              glob_rad = sum(is.na(glob_rad))) %>% 
    data.table:: melt(measure.vars = c("temp", "rhum", "rain", "glob_rad")) %>% 
    
    ggplot() +
    geom_col(aes (variable,value), width = 0.1, show.legend = T)+
    facet_wrap(~year, nrow = 4)
  # dev.off()
  
  ggsave(plot,file = paste0("./temp/EB_analysis/nas/", "NAs_", y,".png"))
  print(y)
}


########################################################
#Map stations
########################################################

library("maps")
ireland = map_data("world", region = "ireland")
ni = map_data("world", region = "uk")
ni <- ni[ni$subregion == "Northern Ireland",]
ireland <- bind_rows(ireland,ni)
df_loc <- 
  weather %>% group_by(stna) %>% select(stna,lat,long) %>% summarise_all(unique) 
map <- 
  ggplot() + 
  geom_polygon(data = ireland, aes(x=long, y = lat, group = group), fill = "darkolivegreen3")  +
  coord_fixed(1.5)+
  geom_point(data = df_loc, aes(x = long, y = lat, fill = "red", alpha = 0.8), size = 1, shape = 2) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  ggrepel::geom_text_repel(aes(x = c(df_loc$long), y = c(df_loc$lat-0.05), label = df_loc$stna),size = 2)+ 
  # annotate("text",x = c(df_loc$long), y = c(df_loc$lat-0.05), label = df_loc$stna, size = 2)+
  ggthemes::theme_tufte()
ggsave(map,file = paste0("./temp/EB_analysis/stationmap.png"))

png(map, filename = "./temp/EB_analysis/map.png")
library(leaflet)

leaflet(df_loc) %>%
  setView(mean(df_loc$long),mean(df_loc$lat),7) %>%
  addTiles(group = "OSM (default)") %>%
  setView(-8,53.5,6)%>% 
  addMarkers(lng = df_loc$long, lat = df_loc$lat, 
             label = ~as.character(df_loc$stna) 
             # icon=greenLeafIcon
             # radius = 2
  )



########################################################
#Prepare data in eurobligh format
########################################################
# Input numeric value (-9999) instead of NA for the variables wee are using in the analysis, 
# to let the models run. 
vars <- c("rain", "rhum", "temp", "wdsp", "sol_rad" )
weather <- 
  weather %>%
  mutate_at(vars(vars), funs(replace(., is.na(.), -9999)))



wth_ls <- split(weather,  list(weather$stna, weather$year) )
not_empty <-sapply(wth_ls, function(x) {nrow(x)>2}) 
wth_ls <- wth_ls[as.vector(not_empty, mode = "logical")]; rm(not_empty)



length(wth_ls)


fails <- vector(mode = "character")

for(x in seq_along(wth_ls)){
  # x <- 1
  # x <- "Roches Point.2018"
  # x <- "Roches Point.2007"
  # x <- "Roches Point.2001"
  df <- wth_ls[[x]]
  #sort problems with the dates
  df <- arrange(df, date_time)
  df <- df[!duplicated(df$date_time), ]
  dff <- data.frame(date_time = seq(ymd_hms(paste0(unique(df$year),"-01-01 00:00:00")),ymd_hms(paste0(unique(df$year),"-12-31 23:00:00")), by = 'hour'))
  df <- left_join(dff, df, by = "date_time")
  df$short_date <-  as.Date(df$date_time)
  st_name <-  unique(df$stna)[!is.na(unique(df$stna))]
  st_no <-  unique(df$stno)[!is.na(unique(df$stno))]
  year_var <- unique(df$year)[!is.na(unique(df$year))]
  country <-  unique(df$country)[!is.na(unique(df$country))]
  df <- add_column(df, i = seq(0, length(df$date)-1,1), .before = "date_time" )
  
  df$doy = yday(df$date_time)
  df$hour = hour(df$date_time)
  
  infil_gap <- 8 #Maximum length of the infill gap
  df$temp <- round(na.spline(df$temp, na.rm = FALSE, maxgap = infil_gap),1)
  df$temp <- round(na.spline(df$temp, na.rm = FALSE, maxgap = infil_gap),1)
  df$rhum <- round(na.spline(df$rhum, na.rm = FALSE, maxgap = infil_gap),0)
  df$rhum  <- sapply(df$rhum, function(x) ifelse(x > 100, x <- 100, x))
  df$rhum <- round(na.spline(df$rhum, na.rm = FALSE, maxgap = infil_gap),0)
  df$rhum  <- sapply(df$rhum, function(x) ifelse(x > 100, x <- 100, x))
  rm(infil_gap)
  
  
  
  df <-   mutate_at(df, vars(c("rain", "rhum", "temp", "wdsp", "sol_rad" )), funs(replace(., is.na(.), -9999)))
  
  
  head(df)
  df <-   df[, c("i", "short_date","doy","hour","temp","rhum","wdsp",  "rain", "sol_rad")] 
  
  
  
  #Translate date to Danish language
  Sys.setlocale("LC_TIME","Danish")
  df$short_date <- tolower(sub("^0", "", format(df$short_date, "%d. %B %Y")))
  Sys.setlocale("LC_TIME","")
  Sys.getlocale()
  
  
  colnames(df) <- Hmisc::capitalize(colnames(df))
  names(df)[names(df) == "Short_date"] <- 'Date'
  names(df)[names(df) == "Rhum"] <- 'RH'
  names(df)[names(df) == "I"] <- 'i'
  names(df)[names(df) == "Wdsp"] <- 'Wind'
  names(df)[names(df) == "Rain"] <- 'Prec'
  names(df)[names(df) == "Doy"] <- 'DayNo'
  names(df)[names(df) == "Rain"] <- 'Prec'
  names(df)[names(df) == "Sol_rad"] <- 'Rad'
  # df <- add_column(df, Leaf =ifelse(df$RH == -9999, -9999, ifelse(df$RH >= 88, 1,0)), .after = "RH" )
  df <- add_column(df, Leaf =rep(NA, nrow(df)), .after = "RH" )
  df <- df[,c("i",	"Date",	"DayNo",	"Hour",	"Wind",	"Temp",	"RH",	"Leaf",	"Prec",	"Rad")]
  
  #Save each year/station combination in a separate file
  write_csv(df,path = paste0('./data/EuroBlight/weather_no_lw/Weather_',year_var,"_",st_no,"_",st_name,"_",country,'.csv',sep=""))
  print(paste(ifelse(file.exists(paste0('./data/EuroBlight/weather_lw_90/Weather_',year_var,"_",st_no,"_",st_name,"_",country,'.csv',sep="")), "saved", "!!!NA"),
              x, "of", length(wth_ls), st_name))
  fails[x] <- ifelse(file.exists(paste0('./data/EuroBlight/weather_lw_90/Weather_',year_var,"_",st_no,"_",st_name,"_",country,'.csv',sep="")), "saved", "check")
}

##################################################
#Check each file for missing values
##################################################

for(x in seq_along(wth_ls)){
  # x <- 1
  # x <- "Roches Point.2018"
  # x <- "Roches Point.2007"
  # x <- "Roches Point.2001"
  df <- wth_ls[[x]]
  #sort problems with the dates
  df <- arrange(df, date_time)
  df <- df[!duplicated(df$date_time), ]
  dff <- data.frame(date_time = seq(ymd_hms(paste0(unique(df$year),"-01-01 00:00:00")),ymd_hms(paste0(unique(df$year),"-12-31 23:00:00")), by = 'hour'))
  df <- left_join(dff, df, by = "date_time")
  df$short_date <-  as.Date(df$date_time)
  st_name <-  unique(df$stna)[!is.na(unique(df$stna))]
  st_no <-  unique(df$stno)[!is.na(unique(df$stno))]
  year_var <- unique(df$year)[!is.na(unique(df$year))]
  country <-  unique(df$country)[!is.na(unique(df$country))]
  df <- add_column(df, i = seq(0, length(df$date)-1,1), .before = "date_time" )
  
  df$doy = yday(df$date_time)
  df$hour = hour(df$date_time)
  
  infil_gap <- 8 #Maximum length of the infill gap
  df$temp <- round(na.spline(df$temp, na.rm = FALSE, maxgap = infil_gap),1)
  df$temp <- round(na.spline(df$temp, na.rm = FALSE, maxgap = infil_gap),1)
  df$rhum <- round(na.spline(df$rhum, na.rm = FALSE, maxgap = infil_gap),0)
  df$rhum  <- sapply(df$rhum, function(x) ifelse(x > 100, x <- 100, x))
  df$rhum <- round(na.spline(df$rhum, na.rm = FALSE, maxgap = infil_gap),0)
  df$rhum  <- sapply(df$rhum, function(x) ifelse(x > 100, x <- 100, x))
  rm(infil_gap)
  
  
  
  df <-   mutate_at(df, vars(c("rain", "rhum", "temp", "wdsp", "sol_rad" )), funs(replace(., is.na(.), -9999)))
  
  
  head(df)
  df <-   df[, c("i", "short_date","doy","hour","temp","rhum","wdsp",  "rain", "sol_rad")] 
  
  
  
  #Translate date to Danish language
  Sys.setlocale("LC_TIME","Danish")
  df$short_date <- tolower(sub("^0", "", format(df$short_date, "%d. %B %Y")))
  Sys.setlocale("LC_TIME","")
  Sys.getlocale()
  
  
  colnames(df) <- Hmisc::capitalize(colnames(df))
  names(df)[names(df) == "Short_date"] <- 'Date'
  names(df)[names(df) == "Rhum"] <- 'RH'
  names(df)[names(df) == "I"] <- 'i'
  names(df)[names(df) == "Wdsp"] <- 'Wind'
  names(df)[names(df) == "Rain"] <- 'Prec'
  names(df)[names(df) == "Doy"] <- 'DayNo'
  names(df)[names(df) == "Rain"] <- 'Prec'
  names(df)[names(df) == "Sol_rad"] <- 'Rad'
  # df <- add_column(df, Leaf =ifelse(df$RH == -9999, -9999, ifelse(df$RH >= 88, 1,0)), .after = "RH" )
  df <- add_column(df, Leaf =rep(NA, nrow(df)), .after = "RH" )
  df <- df[,c("i",	"Date",	"DayNo",	"Hour",	"Wind",	"Temp",	"RH",	"Leaf",	"Prec",	"Rad")]
  
  if(mean(is.na(c(df$Temp, df$Prec, df$RH)))>0.00001){
    print(x)
  }
  
}

mean(is.na(c(df$Temp, df$Prec, df$RH)))
mean(is.na(df$Temp))
mean(is.na(df$Temp))
