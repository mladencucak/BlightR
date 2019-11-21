data_ls <-
  lapply(stations, function (x) {
    #debuging
    x <-  stations[3]
     ec_date = time[55]
    
    station <- x
    date_string <- vector(mode = "character")
    
    # loop_ls <-
      for(ec_date  in time) {
        date_string <- paste0(str_replace_all(as_date(ec_date), "-",""), "")
        #Some forecast files are missing 
        if(file.exists(here::here("dat", "forecast", "ECMWF", paste0("ec0103day",station, "-", date_string, "00.csv")))&
           file.exists(here::here("dat", "forecast", "ECMWF", paste0("ec0305day",station, "-", date_string, "00.csv")))&
           file.exists(here::here("dat", "forecast", "ECMWF", paste0("ec0610day",station, "-", date_string, "00.csv")))
        ){
          # load in forecast
          # ec_date <- format.Date(ec_date, "%Y%m%d")
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
          fore_df <- bind_rows(ec0103day, ec0305day, ec0610day) %>% tbl_df()
          
          #add missing hours
          fore_df <- padr::pad(fore_df, by = "date", interval = "hour")
          
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
              wdsd = WSpd,
              rain = Rain
            ) %>%
            select(-starts_with("WDir"))
          
          
          #Interpolate values for 3 and 6 hour forecast
          infil_gap <- 6
          fore_df$temp <- round(na.spline(fore_df$temp, na.rm = FALSE, maxgap = infil_gap),1)
          fore_df$rhum <- round(na.spline(fore_df$rhum, na.rm = FALSE, maxgap = infil_gap),0)
          fore_df$rhum  <- sapply(fore_df$rhum, function(x) ifelse(x>100, x<-100, x))
          fore_df$rain <- round(na.replace(fore_df$rain, 0),1)
          fore_df$sol_rad <- round(na.spline(fore_df$sol_rad, na.rm = FALSE, maxgap = infil_gap),1)
          fore_df$sol_rad <- ifelse(fore_df$sol_rad < 0, 0, fore_df$sol_rad)
          
          fore_df$wspd <- round(na.spline(fore_df$wdsd, na.rm = FALSE, maxgap = infil_gap),0)
          # fore_df$dir <- round(na.spline(df$dir, na.rm = FALSE, maxgap = infil_gap),0)
          
          #get the observed data
          #we need to stitch a day before and after of observed data to let the model run 
          date_range <- seq.Date(fore_df$for_date[1]-1, fore_df$for_date[1]+10, by = "day")
          
          
          
          # cols <- c("date", "short_date", "temp_ob", "rhum_ob", "sol_rad_ob", "wdsp_ob", "rain_ob")
          cols <- c("date", "short_date", "temp", "rhum", "sol_rad", "wdsp", "rain")
          obs_df_fun <-
            obs_df[obs_df[["stna"]] == station &
                     obs_df[["short_date"]] %in% date_range,
                   cols]
          obs_df_fun <-
          obs_df_fun[!duplicated(obs_df_fun$date),]
          obs_df_fun <-  add_column(obs_df_fun, stna = station, .after = "date")
          obs_df_fun <-  add_column(obs_df_fun, set = "obs", .before = "date")
          obs_df_fun <-  add_column(obs_df_fun, for_date = as_date(ec_date), .after = "date")
          obs_df_fun <-  add_column(obs_df_fun, day_step = sort(rep(seq(0,11,1),24)), .after = "for_date")
          obs_df_fun <-  add_column(obs_df_fun, hour_step = seq(-23,nrow(obs_df_fun)-24), .after = "day_step")
          # obs_df_fun <- select(obs_df_fun, - short_date)
          
          
          #add -/+1 day of observed data to forecast data
          fore_df <- 
            bind_rows(obs_df_fun[ obs_df_fun$short_date == date_range[1], ],
                      fore_df, 
                      obs_df_fun[ obs_df_fun$short_date == max(date_range), ]) %>% 
            mutate( set  = "fore")
          
          lss <- list()
          #create df with single variable forecast/observed data 
          vars <- c("temp", "rhum", "sol_rad" )
          for (i in seq(vars)) {
            x <- vars[i]
            dff <- fore_df
            dff[[x]] <- obs_df_fun[[x]];
            dff[["set"]] <- paste("fore", x, sep = "_")
            lss[[i]] <-  dff
            names(lss)[i] <- paste("fore", x, sep = "_")
            
            dff <- obs_df_fun
            dff[[x]] <- fore_df[[x]];
            dff[["set"]] <- paste("obs", x, sep = "_")
            lss[[i+length(vars)]] <-  dff
            names(lss)[i+length(vars)] <- paste("obs", x, sep = "_")
            
          }
          
          fulldf <- 
            bind_rows(fore_df, obs_df_fun, lss)
          rm(lss, fore_df, obs_df_fun)
          
          # fulldf
          print(paste(station, as.character(as_date(ec_date)), nrow(fulldf)))
        }
      }
    loop_ls
  })

