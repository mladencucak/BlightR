
#######################################################
#Load packages
#######################################################
list.of.packages <-
  c(
    "tidyverse",
    "dplyr",
    "readxl",
    "stringr",
    "lubridate",
    "readr",
    "zoo",
    "padr",
    "imputeTS",
    "foreach",
    "pbapply",
    "parallel"
  )

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#Download packages that are not already present
if(length(new.packages)) install.packages(new.packages)

packages_load <-lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load)==0)){
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
}else {
  print("All packags are succesfully loaded")
}

rm(packages_load, new.packages, list.of.packages)
#######################################################
# Forecast files
#######################################################

#Stations
stations <- 
  list.files(here::here("dat", "forecast", "ECMWF")) %>% 
  sub("\\-.*", "", .) %>% 
  str_sub(10,-1) %>%
  unique()

stations <- stations[!stations %in% c("Athenry", "Claremorris","MalinHead", "Markee")]

#Define time period
time <- 
  list.files(here::here("dat", "forecast", "ECMWF")) %>% 
  sub(".*\\-", "", .) %>% 
  sub("00.csv","",.) %>%
  unique() %>% 
  lubridate::ymd() 

#Subset for the period we are interested in

#Define period for forecast data
# !!! Change also in observed data
time <-
  time[time>= as.Date("2017-05-01") & time <= as.Date("2017-09-18")|
         time>= as.Date("2018-05-01") & time <= as.Date("2018-09-18")|
         time>= as.Date("2019-05-01") & time <= as.Date("2019-09-18")
       ] %>% 
  sort()


all_dates <- 
  c(seq.Date( as.Date("2017-05-01"),as.Date("2017-09-18"), by = "day" ), 
    seq.Date( as.Date("2018-05-01"),as.Date("2018-09-18"), by = "day" ),
    seq.Date( as.Date("2019-05-01"),as.Date("2019-09-18"), by = "day" )
  )

#total number of station/year data sets
time %>% length()*length(unique(stations))


#missing dates
all_dates[!all_dates %in% time]

rm( all_dates)

#######################################################
#Load Observed weather data non-QCd
#######################################################
obs_files <- 
  list.files(here::here("dat", "forecast", "historical"))


dff <- lapply(obs_files,function(x) 
  data.table::fread(here::here("dat", "forecast", "historical", x),drop = 14, nrows = 24)
) %>% bind_rows() %>% 
  tbl_df()

dff <- dff[, 1:13]

dff$date <-  dmy_hms(dff$date)

dff <- 
  dff %>% 
  select(-c(station_no,year, month, day, hour, dir )) %>% 
  rename(stna = station_name,
         wdsp = speed,
         rhum = rh,
         rain = rainfall,
         sol_rad =solar
  )  

dff$stna[dff$stna == "JohnstownII"|dff$stna == "JOHNSTOWNII"] <- "Johnstown"
dff$stna[dff$stna == "Oak Park"|dff$stna == "OAK PARK"|dff$stna == "Oak_Park"] <- "Oakpark"
dff$stna[dff$stna == "Moore Park"|dff$stna == "Moore_Park"|dff$stna == "MOORE PARK"] <- "Moorepark"
dff$stna[dff$stna == "DUNSANY"] <- "Dunsany"
dff$stna[dff$stna == "GURTEEN"] <- "Gurteen"

# dff[dff$stna != c("MULLINGAR", "ATHENRY","CLAREMORRIS", "FINNER","MALIN HEAD"),] 
dff<-
dff[(!(dff$stna=="ATHENRY")&!(dff$stna=="MULLINGAR")&!(dff$stna=="CLAREMORRIS")&!(dff$stna=="FINNER")&!(dff$stna=="MALIN HEAD")),]

unique(dff$stna)


obs_df <-  add_column(dff, short_date = as.Date(dff$date, format = "%Y-%m-%d"), .after = "date")

obs_df <- 
obs_df[obs_df$short_date>= as.Date("2017-05-01")-1 & obs_df$short_date <= as.Date("2017-09-18")+11|
       obs_df$short_date>= as.Date("2018-05-01")-1 & obs_df$short_date <= as.Date("2018-09-18")+11|
         obs_df$short_date>= as.Date("2019-05-01")-1 & obs_df$short_date <= as.Date("2019-09-18")+11, ]
tail(obs_df)

infil_gap <- 10 #Maximum length of the infill gap
obs_df$temp <- round(na.spline(obs_df$temp, na.rm = FALSE, maxgap = infil_gap),1)
obs_df$rhum <- round(na.spline(obs_df$rhum, na.rm = FALSE, maxgap = infil_gap),0)
obs_df$rhum  <- sapply(obs_df$rhum, function(x) ifelse(x > 100, x <- 100, x))

obs_df[is.na(obs_df$sol_rad) & hour(obs_df$date) %in% c(0:3, 21:23),]$sol_rad <- 0
obs_df$sol_rad <-  round(na.approx(obs_df$sol_rad, na.rm = FALSE, maxgap = infil_gap),3)
rm(infil_gap)

# Seems like the data arrived 2X in one day
nrow(obs_df)
obs_df <-  obs_df[!duplicated(obs_df),]
nrow(obs_df)

mutate(obs_df, year = year(date)) %>% 
split(., list(.$stna, .$year)) %>% 
  lapply(., function( fundf){
    fundf <- padr::pad(fundf, by = "date",interval = "hour")
  }) %>% 
  bind_rows() %>% str()

fundf <- 
obs_df[obs_df$stna=="Dunsany",]
# dff <-
padr::pad(fundf, by = "date", interval = "hour")
nrow(fundf)
# 
# 
# split(obs_df, obs_df$stna) %>% 
#   lapply(., function(fundf){
#     fundf <- 
#   })
# rm(dff, obs_files)
# 


#######################################################
#Load Observed weather data QCd
#######################################################


my_files <- 
  list.files(here::here("dat", "forecast", "wth"))




all_csv <-
  lapply(my_files, function (i) {
    read_csv(
      paste0(here::here("dat", "forecast", "wth"), "/", i),
      col_types = cols(
        date = col_datetime(format = "%m/%d/%Y %H:%M"),
        clamt = col_skip(),
        clht = col_skip(),
        vis = col_skip(),
        w = col_skip(),
        ww = col_skip()
      ),
      skip = 23
    )
  })

#load metadata
metadata_list <- lapply(my_files, function(x) 
  read_csv( paste0(here::here("dat", "forecast", "wth"),"/", x),
           col_names = FALSE)[1:4,1])

#Delete unwanted characters from metadata
metadata_list <- lapply(metadata_list, function(x) 
  sapply(x, function(y){
    str_replace_all(y,"Station Name: |Station Height: | M|Latitude:|Longitude: ", "" )
  }))

#Extract station numbers
st_numbers <- gsub( "hly|.csv", "", my_files)

#Assign station names to data frames
station_names <- unlist(lapply(metadata_list, function(x) as.character(x[[1]])))
names(all_csv) <- station_names


#add metadata as columns to df
all_csv <- Map(
  cbind,
  all_csv,
  sol_rad = NA,
  stna= as.character(station_names),
  stno = as.numeric(st_numbers),
  height = as.numeric(lapply(metadata_list, `[`, 2)),
  lat = as.numeric(lapply(metadata_list, `[`, 3)),
  long = as.numeric(lapply(metadata_list, `[`, 4))
)




#rbind all df into one 
df <- do.call("rbind", all_csv)                                   
rm(all_csv, metadata_list, my_files, st_numbers, station_names, sun_vec)


df<- add_column(df, short_date = as.Date(df$date), .after=1)
df<- add_column(df, year = year(df$date), .after=2)
df<- add_column(df, month = month(df$date), .after=3)
df<- add_column(df, day = day(df$date), .after=4)
df<- add_column(df, doy = yday(df$date), .after=4)
df<- add_column(df, hour = hour(df$date), .after="day")

#fix column types
str(df)
df[, -c(which( names(df) %in% c("stna", "date", "short_date")))] <- 
  sapply(df[, -c(which( names(df) %in% c("stna", "date", "short_date")))], as.numeric)


#Change factors to character
df%>% mutate_if(is.factor, as.character) -> df

 df <- 
  filter(df, year > 2016)


df$stna[df$stna == "JohnstownII"|df$stna == "JOHNSTOWNII"] <- "Johnstown"
df$stna[df$stna == "Oak Park"|df$stna == "OAK PARK"|df$stna == "Oak_Park"] <- "Oakpark"
df$stna[df$stna == "Moore Park"|df$stna == "Moore_Park"|df$stna == "MOORE PARK"] <- "Moorepark"
df$stna[df$stna == "DUNSANY"] <- "Dunsany"
df$stna[df$stna == "GURTEEN"] <- "Gurteen"


obs_df <- 
left_join(
  obs_df[, c("stna", "date","short_date",  "sol_rad")],
  df[, c("stna", "date","short_date", "temp", "rhum", "rain", "wdsp")],
  by = c("stna", "date", "short_date")
  
)

sapply(obs_df,function(x) mean(is.na(x)))

infil_gap <- 10 #Maximum length of the infill gap
obs_df$temp <- round(na.spline(obs_df$temp, na.rm = FALSE, maxgap = infil_gap),1)
obs_df$rhum <- round(na.spline(obs_df$rhum, na.rm = FALSE, maxgap = infil_gap),0)
obs_df$rhum  <- sapply(obs_df$rhum, function(x) ifelse(x > 100, x <- 100, x))
sapply(obs_df,function(x) mean(is.na(x)))


#######################################################
#Load Observed weather data QCd
#######################################################
# TODO check weather dat, infill missing and update for 2019
# #Load observed data
# obs_df <- load(here::here("dat", "weather_infilled&sol_estim.Rdata"))
# obs_df <- weather;rm(weather)
# 
# obs_df$stna[obs_df$stna == "JohnstownII"] <- "Johnstown"
# obs_df$stna[obs_df$stna == "Oak Park"] <- "Oakpark"
# obs_df$stna[obs_df$stna == "Moore Park"] <- "Moorepark"
# 
# 
# unique(obs_df$stna)
# obs_df <- rename(obs_df, date = date_time)
# 
# cols <- c("stna", "date", "short_date", "temp", "rhum", "sol_rad", "wdsp", "rain")
# 
# obs_df <- 
#   obs_df %>% 
#   filter(stna %in% c( "Johnstown", "Oakpark", "Moorepark","Dunsany","Gurteen")) %>% 
#   filter(year >2016 ) %>% 
#   select(cols)

#######################################################
#Check Observed weather data
#######################################################

obs_df$year <- year(obs_df$date)
wthls <- 
  split(obs_df, obs_df$year)

lapply(wthls, function(x) nrow(x))
lapply(wthls, function(x) summary(x[, c("temp", "rhum", "sol_rad", "wdsp", "rain")]))
rm(wthls)


#######################################################
# Load weather forecast and combine into desired format
#######################################################


cl <- makeCluster(4) # create a cluster with 2 cores
library("doParallel")
registerDoParallel(cl) # register the cluster


start_time <- Sys.time()#measure duration
data_ls <-
  lapply(stations, function (x) {
    # station <- stations[1]
    station <- x
    date_string <- vector(mode = "character")
    
    loop_ls <-
      foreach(ec_date = time,
              .export = c("obs_df"),
              .packages = c("tidyverse", "lubridate", "stringr", "padr", "zoo", "imputeTS")) %dopar%
      {
        #Testing        
        # ec_date <- time[287]
        # station <- stations[1]
        date_range <- 
          seq.Date(ec_date-1, ec_date+10, by = "day")

       available_obs<-
          obs_df[obs_df[["stna"]] == station &
                   obs_df[["short_date"]] %in% date_range,
                 "short_date"] %>% unique() %>% unlist %>%  length()

       
        date_string <- paste0(str_replace_all(ec_date, "-",""), "")
        #Some forecast files are missing 
        if(file.exists(here::here("dat", "forecast", "ECMWF", paste0("ec0103day",station, "-", date_string, "00.csv")))&
           file.exists(here::here("dat", "forecast", "ECMWF", paste0("ec0305day",station, "-", date_string, "00.csv")))&
           file.exists(here::here("dat", "forecast", "ECMWF", paste0("ec0610day",station, "-", date_string, "00.csv")))&
           #check if there is observed data as well
           available_obs==length(date_range)
        ){
          ec0103day <-
            data.table::fread(here::here("dat", "forecast", "ECMWF", paste0("ec0103day",station, "-", date_string, "00.csv")),
                              skip = 13)
          ec0305day <-
            data.table::fread(here::here("dat", "forecast", "ECMWF", 
                                         paste0("ec0305day",station, "-", date_string, "00.csv")),
                              skip = 43)
          ec0610day <-
            data.table::fread(here::here("dat", "forecast", "ECMWF", paste0("ec0610day",station, "-", date_string, "00.csv")),
                              skip = 33)
          
          #arrange colls for binding
          ec0103day <- (unite(ec0103day, date, 1, 2, sep = " "))
          ec0103day$date <- as.POSIXct(ec0103day$date, tz = "UTC")
          ec0305day <- (unite(ec0305day, date, 1, 2, sep = " "))
          ec0305day$date <- as.POSIXct(ec0305day$date, tz = "UTC")
          ec0610day <- (unite(ec0610day, date, 1, 2, sep = " "))
          ec0610day$date <- as.POSIXct(ec0610day$date, tz = "UTC")
          colnames(ec0305day) <- colnames(ec0103day)
          colnames(ec0610day) <- colnames(ec0103day)

          #Reduce for an hour, to be closer to the mmid point of the three hours/six hours
          ec0305day$date <- ec0305day$date-3600
          ec0610day$date <- ec0610day$date - c(3600*3)
          
          
          fore_df <- bind_rows(ec0103day, ec0305day, ec0610day ) %>% tbl_df()
          
          addrow <- 
          fore_df[nrow(fore_df),] %>% 
            mutate(date = date + 3*3600) 
          addrow[, 2:length(addrow)] <- NA  
          
          fore_df <- 
            bind_rows(fore_df,addrow) 
          
          #add missing hours
          fore_df <- padr::pad(fore_df, by = "date",interval = "hour")
          
          fore_df <-  add_column(fore_df, stna = station, .after = "date")
          fore_df <-  add_column(fore_df, set = "fore", .before = "date")
          fore_df <-  add_column(fore_df, for_date = as_date(ec_date), .after = "date")
          fore_df <-  add_column(fore_df, day_step = sort(rep(seq(1,10,1),24)), .after = "for_date")
          fore_df <-  add_column(fore_df, hour_step = seq(1,nrow(fore_df)), .after = "day_step")
          # fore_df$DayNo <- yday(fore_df$date)
          fore_df <- add_column(fore_df, short_date = lubridate::as_date(fore_df$date), .after = "stna")
          
          
          fore_df <-
            rename(
              fore_df,
              temp = Temp,
              rhum = RelHum,
              sol_rad = SolRad,
              wdsp = WSpd,
              rain = Rain
            ) %>%
            select(-starts_with("WDir"))
          
          
          #Interpolate values for 3 and 6 hour forecast
          infil_gap <- 8
          fore_df$temp <- round(na.spline(fore_df$temp, na.rm = FALSE, maxgap = infil_gap),1)
          fore_df$rhum <- round(na.spline(fore_df$rhum, na.rm = FALSE, maxgap = infil_gap),0)
          fore_df$rhum  <- sapply(fore_df$rhum, function(x) ifelse(x>100, x<-100, x))
          fore_df$rain <- round(na_replace(fore_df$rain, 0),1)
          fore_df$sol_rad <- na_replace(fore_df$sol_rad, 0)

                    
          fore_df$wdsp <- round(na.spline(fore_df$wdsp, na.rm = FALSE, maxgap = infil_gap),0)

          fore_df <- arrange(fore_df, date)          
          
          #get the observed data
          #we need to stitch a day before and after of observed data to let the model run 
          date_range <- seq.Date(fore_df$for_date[1]-1, fore_df$for_date[1]+10, by = "day")
          
          
          
          # cols <- c("date", "short_date", "temp_ob", "rhum_ob", "sol_rad_ob", "wdsp_ob", "rain_ob")
          cols <- c("date", "short_date", "temp", "rhum", "sol_rad", "wdsp", "rain")
          obs_df_fun <-
            obs_df[obs_df[["stna"]] == station &
                     obs_df[["short_date"]] %in% date_range,
                   cols] %>% 
            arrange(date)
          
          obs_df_fun <-
            obs_df_fun[!duplicated(obs_df_fun$date),]
          obs_df_fun <-  add_column(obs_df_fun, stna = station, .after = "date")
          obs_df_fun <-  add_column(obs_df_fun, set = "obs", .before = "date")
          obs_df_fun <-  add_column(obs_df_fun, for_date = as_date(ec_date), .after = "date")
          obs_df_fun <-  add_column(obs_df_fun, day_step = sort(rep(seq(0,11,1),24)), .after = "for_date")
          obs_df_fun <-  add_column(obs_df_fun, hour_step = seq(-23,nrow(obs_df_fun)-24), .after = "day_step")


          #add -/+1 day of observed data to forecast data
          fore_df <- 
            bind_rows(obs_df_fun[ obs_df_fun$short_date == date_range[1], ],
                      fore_df, 
                      obs_df_fun[ obs_df_fun$short_date == max(date_range), ]) %>% 
            mutate( set  = "fore")
          
          # lss <- list()
          # #create df with single variable forecast/observed data 
          # vars <- c("temp", "rhum", "sol_rad" )
          # for (i in seq(vars)) {
          #   x <- vars[i]
          #   dff <- fore_df
          #   dff[[x]] <- obs_df_fun[[x]];
          #   dff[["set"]] <- paste("fore", x, sep = "_")
          #   lss[[i]] <-  dff
          #   names(lss)[i] <- paste("fore", x, sep = "_")
          #   
          #   dff <- obs_df_fun
          #   dff[[x]] <- fore_df[[x]];
          #   dff[["set"]] <- paste("obs", x, sep = "_")
          #   lss[[i+length(vars)]] <-  dff
          #   names(lss)[i+length(vars)] <- paste("obs", x, sep = "_")
          # }
          
          fulldf <- 
            bind_rows(fore_df, obs_df_fun)
          rm( fore_df, obs_df_fun)
          
          fulldf
        }
      }
    loop_ls
  })

stopCluster(cl) # shut down the cluster
start_time -  Sys.time() #time spend on loading the data
rm(cl, obs_df, start_time)

#Checks

tail(data_ls[[3]][[1]],20) 
tail(data_ls[[1]][1]) 
nrow(data_ls[[1]][[1]]) /length(unique(data_ls[[1]][[1]]$set))


 data_ls <- unlist(data_ls, recursive=FALSE)

 length(data_ls)

 #Remove empty data frames
length(data_ls)
data_ls <- 
data_ls[!sapply(data_ls, function(x) is.null(nrow(x)))]
length(data_ls)


#Add id 
data_ls <- 
lapply(data_ls, function(x) {
  x <- unite(x, "id", c("set", "for_date", "stna"),remove =FALSE)
    return(x)

    })

#REmove the data with more than 1% of missing values
sapply(data_ls, function(x) sum(is.na(x[,c( "temp", "rhum")])))%>% as.vector()

nas <- sapply(data_ls, function(x) mean(is.na(x[,c( "temp", "rhum")])))%>% as.vector() %>% round(3)
sum(nas<0.01)

length(data_ls)
data_ls <- 
data_ls[nas<0.01] 
length(data_ls)




#Check if they all have the same names we pre-defined
sapply(data_ls, function(x) all(names(x)== names(data_ls[[1]]))) %>% all()
names(data_ls[[1]])

save(data_ls, file =here::here("out", "fore", "fore_dat_full.Rdata"))






#######################################################
# Quick analysis of the weather forecast data
#######################################################

# Convert scripts from the weather forecast project
# Spread this data set so it is in same format as previous analysis  
# Check the solar radiation data




#######################################################
# Run the models
#######################################################

source(here::here("scr","lib",  "pkg.R"))
source(here::here("scr", "lib", "funs.R"))
load(here::here("out", "fore", "fore_dat_full.Rdata"))

# Add geo references to the data
load(file = here::here("dat", "locations.Rdata"))
df_loc$stna[df_loc$stna == "JohnstownII"|df_loc$stna == "JOHNSTOWNII"] <- "Johnstown"
df_loc$stna[df_loc$stna == "Oak Park"|df_loc$stna == "OAK PARK"|df_loc$stna == "Oak_Park"] <- "Oakpark"
df_loc$stna[df_loc$stna == "Moore Park"|df_loc$stna == "Moore_Park"|df_loc$stna == "MOORE PARK"] <- "Moorepark"
df_loc$stna[df_loc$stna == "DUNSANY"] <- "Dunsany"
df_loc$stna[df_loc$stna == "GURTEEN"] <- "Gurteen"

#prop succesfull
prop_succesfull_data <- 
  c(length(data_ls)/c(time %>% length()*length(unique(stations))))
length(data_ls)
c(time %>% length()*length(unique(stations))) -length(data_ls)

100 - round(prop_succesfull_data *100 ,2)

data_ls <- 
  lapply(data_ls, function(x) {
    x <- left_join(x, df_loc, by ="stna")
    return(x)
  })

#####
#convert from w/m2 to MJ/m2
#conversion https://cliflo-niwa.niwa.co.nz/pls/niwp/wh.do_help?id=ls_rad
data_ls <- 
  lapply(data_ls, function( fundf){
    fundf$sol_rad <- fundf$sol_rad * 0.0036
    return(fundf)
  })

# wider format for the comparison
full_data <- 
lapply(data_ls, function(fundf){
  fundfob <- fundf[fundf$set == "obs",]
  colnames(fundfob)[ colnames(fundfob) %in%c("temp","rhum","sol_rad","wdsp","rain" )] <- 
    paste0(c("temp","rhum","sol_rad","wdsp","rain" ), "_ob")
  bind_cols(fundf[fundf$set == "fore",], 
            fundfob[, grep("_ob", colnames(fundfob))]) %>% 
    filter(hour_step %in% 1:240) 
}) %>% 
bind_rows() 

#Stations
full_data %>% 
  group_by(stna) %>% 
  summarise(Latitude = unique(lat),
            Longitude = unique(long)) %>% 
  write_csv(here::here("out", "fore", "Stations.csv"))


####################################################################################
#RH errors overall
####################################################################################


full_data %>% 
  group_by(hour_step, for_date) %>% 
  mutate(rhum_ob = as.numeric(rhum_ob)) %>% 
  summarise( rmse = rmse(rhum_ob, rhum),
             mse = mse(rhum_ob, rhum),
             rsq = cor(rhum_ob, rhum)
             )%>% 
  ungroup() %>% 
  mutate(var = "rhum") ->xx 

full_data %>% 
  group_by(day_step) %>% 
  mutate(rhum_ob = as.numeric(rhum_ob)) %>% 
  summarise( rmse = rmse(rhum_ob, rhum),
             mse = mse(rhum_ob, rhum),
             rsq = cor(rhum_ob, rhum),
             ccc = epiR::epi.ccc(
               rhum_ob,
               rhum,
               ci = "z-transform",
               conf.level = 0.95,
               rep.measure = FALSE
             )$rho.c[, 1])%>% 
  ungroup()  %>% 
  mutate(var = "rhum")->errors_daily_rh


#Relative humidity
ggplot(xx,aes(factor(hour_step), rmse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="RMSEs Relatve Humidity", 
       subtitle="Tufte boxplot of RMSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="RMSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rmse)-2, label =as.character(round(errors_daily_rh$rmse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "rh rmse .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)

ggplot(xx,aes(factor(hour_step), mse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="MSEs Relatve Humidity", 
       subtitle="Boxplot of MSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="MSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$mse)-2, label =as.character(round(errors_daily_rh$mse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "rh mse .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)


ggplot(xx,aes(factor(hour_step), rsq))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="MSEs Relatve Humidity", 
       subtitle="Boxplot of MSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="MSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rsq)-2, label =as.character(round(errors_daily_rh$rsq,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "rh rsq .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)



######################################################
#Temp
#################################################################
full_data %>% 
  group_by(hour_step, for_date) %>% 
  summarise( rmse = rmse(temp_ob, temp),
             mse = mse(temp_ob, temp),
             rsq = cor(temp_ob, temp)) %>% 
  ungroup() %>% 
  mutate(var = "temp") ->xx 

full_data %>% 
  group_by(day_step) %>% 
  mutate(temp_ob = as.numeric(temp_ob)) %>% 
  summarise( rmse = rmse(temp_ob, temp),
             mse = mse(temp_ob, temp),
             rsq = cor(temp_ob, temp),
             ccc = epiR::epi.ccc(
               temp_ob,
               temp,
               ci = "z-transform",
               conf.level = 0.95,
               rep.measure = FALSE
             )$rho.c[, 1]) %>% 
  ungroup() %>% 
  mutate(var = "temp") ->errors_daily_temp


ggplot(xx,aes(factor(hour_step), rmse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="RMSE Temperature", 
       subtitle="Boxplot of RMSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="RMSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rmse)-max(xx$rmse)*0.01, label =as.character(round(errors_daily_temp$rmse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "temp rmse .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)


ggplot(xx,aes(factor(hour_step), mse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="mse Temperature", 
       subtitle="Boxplot of mse hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="mse")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$mse)-max(xx$mse)*0.01, label =as.character(round(errors_daily_temp$mse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "temp mse .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)



ggplot(xx,aes(factor(hour_step), rsq))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title=expression("r" ~ R^2 ~  "Temperature"), 
       subtitle="Boxplot of rsq hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="rsq")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rsq)-max(xx$rsq)*0.01, label =as.character(round(errors_daily_temp$rsq,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "tmp rsq .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)




#####################################
#Solar Radiation
#########################################

full_data %>% 
  group_by(hour_step, for_date) %>% 
  mutate(temp_ob = as.numeric(temp_ob)) %>% 
  summarise(     rmse = rmse(sol_rad, sol_rad_ob),
                 mse = mse(sol_rad, sol_rad_ob),
             rsq = cor(sol_rad, sol_rad_ob),
             ccc = epiR::epi.ccc(
               sol_rad,
               sol_rad_ob,
               ci = "z-transform",
               conf.level = 0.95,
               rep.measure = FALSE
             )$rho.c[, 1]
             ) %>% 
  ungroup() %>% 
  mutate(var = "sol_rad") ->xx 

errors_daily_sol <- 
  full_data %>%
  group_by(id,day_step) %>%
  mutate(sol_rad_ob = as.numeric(sol_rad_ob)) %>%
  summarise(sol_rad = sum (sol_rad),
            sol_rad_ob = sum(sol_rad_ob)) %>%
  ungroup() %>%
  group_by(day_step) %>%
  summarise(
    rmse = rmse(sol_rad, sol_rad_ob),
    mse = mse(sol_rad, sol_rad_ob),
    rsq = cor(sol_rad, sol_rad_ob),
    ccc = epiR::epi.ccc(
      sol_rad,
      sol_rad_ob,
      ci = "z-transform",
      conf.level = 0.95,
      rep.measure = FALSE
    )$rho.c[, 1]
  ) %>%
  ungroup() %>%
  mutate(var = "sol_rad")

ggplot(xx,aes(factor(hour_step), rmse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="RMSE Solar Radiation", 
       subtitle="Boxplot of RMSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="RMSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rmse)-max(xx$rmse)*0.01, label =as.character(round(errors_daily$rmse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))

ggplot(xx,aes(factor(hour_step), mse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="MSE Solar Radiation", 
       subtitle="Boxplot of MSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="MSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$mse)-max(xx$mse)*0.01, label =as.character(round(errors_daily$mse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))


ggplot(errors_daily,aes(factor(daystep), rmse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="RMSE Solar Radiation", 
       subtitle="Boxplot of ccc hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="RMSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rmse)-max(xx$rmse)*0.01, label =as.character(round(errors_daily$rmse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))



bind_rows(errors_daily_rh, errors_daily_temp, errors_daily_sol) %>% 
  reshape2::melt(
    .,
    id.vars = c("var",  "day_step"),
    # measure.vars = models,
    variable.name = "ind",
    value.name = "skill",
    factorsAsStrings  = FALSE
  ) %>% 
  ggplot()+
  geom_line(aes(day_step, skill, color = var))+
  theme_article()+
  theme(legend.position = "right") +
  facet_wrap(~ind, scales = "free", ncol =1 )+
  scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
  labs(
    colour = "Idicator",
    title = "Validation of forecasted risk",
    x = "Lead time (days)"
  )+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "daily all indicators .png"),
         width = 6, 
         height = 4,
         units = "in",
         dpi = 2000)



 pcccfore <- 
bind_rows(errors_daily_rh, errors_daily_temp, errors_daily_sol) %>% 
  reshape2::melt(
    .,
    id.vars = c("var",  "day_step"),
    # measure.vars = models,
    variable.name = "ind",
    value.name = "skill",
    factorsAsStrings  = FALSE
  ) %>% 
  mutate(var = ifelse(var== "rhum", "Relative Humidity (%)", 
                      ifelse(var == "sol_rad", "Solar Radiation (MJ/m2/day)",
                             ifelse(var == "temp" ,  "Temperature (°C)", "")))) %>% 
  filter(ind == "ccc") %>% 
  ggplot()+
  geom_line(aes(day_step, skill, color = var))+
  theme_bw()+
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1),minor_breaks = seq(1, 10, 1))+
  scale_y_continuous(limits = c(0,1.03),breaks = seq(0,1,.2),labels = seq(0,1,.2),minor_breaks = seq(0,1,.2))+
  scale_color_manual(values=c("#56B4E9", "#E69F00", "salmon"))+
  labs(
    colour = "Forecasted variable:",
    x = "Lead time (days)",
    y = "Concordance Correlation Coefficient"
  )+
  theme(
    text = element_text(size=12),
    legend.position = c(.77, .82),
    legend.text = element_text(size = 11),
    legend.title = element_text(11),
    legend.key.width = unit(1, "cm")
  )+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "daily .png"),
         width = 7, 
         height = 4,
         units = "in",
         dpi = 800)

saveRDS(pcccfore, file =  here::here("out", "fore", "fig", "wth_vars", "CCCdaily_wth_vars.png"))



# Split the data into data frames for each id to run the model 
# Model needs to be run in that way to sync the outputs
data_ls <- 
  data.table::rbindlist(data_ls)
data_ls <- 
  split(data_ls, data_ls$id)

sapply(data_ls, function(x) sum(is.na(x[,c( "temp", "rhum")])))%>% as.vector()

nas <- sapply(data_ls, function(x) mean(is.na(x[,c( "temp", "rhum")])))%>% as.vector() %>% round(3)
sum(nas<0.01)
length(data_ls)



source(here::here("scr", "model", "run.R"))

source(here::here("scr", "lib", "IrishRulesModelSensitive.R"))

RunModel <- function(x, ir_run = FALSE, ir_def_run = FALSE, model_parameters = "default", run_type=run_type ) {
  # run_type can be "model" (outbreaks ) or "wth" (for weather data)
  # eval_run will parametarise model with predetermined set of parameters
  # ir_run  determine if the Irish Rules model is to be run and attached to the data. 
  y <- BlightR(x, run_type = run_type, model_parameters = model_parameters)
  
  if (ir_run == TRUE) {
    y$ir_risk <-
      IrishRulesModel(x,
                      temporal_res = "daily",
                      param = "modified",
                      replace_na = TRUE)
  }
  if (ir_def_run == TRUE) {
    y$defir_risk <-
      IrishRulesModel(x,
                      temporal_res = "daily",
                      param = "default",
                      replace_na = TRUE)
  }
  y <- y[2:c(nrow(y) - 1),]
  
  return(y)
}



cl <- makeCluster(detectCores())
clusterExport(cl, c("BlightR","IrishRulesModel", "RunModel", "ExtractCol"))

clusterEvalQ(cl, library("tidyverse", quietly = TRUE, verbose = FALSE))




out_ls <-
pblapply(data_ls, function(x)
  RunModel(
    x,
    run_type = "fore",
    ir_run = TRUE,
    ir_def_run = TRUE
  ) , cl = cl)




# save(out_ls, file =here::here("out", "fore", "fore_model_out.Rdata"))
load( file =here::here("out", "fore", "fore_model_out.Rdata"))

