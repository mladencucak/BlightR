

########################################################
#Sporulation
########################################################
Sporulation <-
  function(temp,
           rh,
           parameters,
           rounding = NULL
           ) {
    
    if(is.null(rounding)){rounding = 6}
    #Import parameters
    #Temp factor
    TminSpor <- parameters[, "TminSpor"] %>% as.numeric()
    ToptSpor <- parameters[, "ToptSpor"]%>% as.numeric()
    TmaxSpor <- parameters[, "TmaxSpor"]%>% as.numeric()
    RfactSpor <- parameters[, "RfactSpor"]%>% as.numeric()
    ShapeSpor <- parameters[, "ShapeSpor"]%>% as.numeric()
    
    #RH factor
    KSpor <- parameters[, "KSpor"]%>% as.numeric()
    n0Spor <- parameters[, "n0Spor"]%>% as.numeric()
    rSpor <- parameters[, "rSpor"]%>% as.numeric()

    
    #Calculate temp factor
    #Function to cacluate the sporulation temperature factor
    sporulation_temp <- 
    sapply(temp, function(x) {
      sporulation_temperature <-
        RfactSpor * ((TmaxSpor - x) / (TmaxSpor - ToptSpor) * ((x - TminSpor) / (ToptSpor - TminSpor)) ^ ((ToptSpor - TminSpor) / (TmaxSpor - ToptSpor))) ^  ShapeSpor
      sporulation_temperature = ifelse(x < TminSpor | x > TmaxSpor, 0, sporulation_temperature)
      return(sporulation_temperature)
    }) %>% unlist()
    
    # Calculate the RH factor
    rh <- ifelse(rh >=80, rh - 80, 0) #resize rh scale to 1-20
    sporulation_rh <-
      sapply(rh, function(x) {
      spor_rh_hour <- 
      KSpor * n0Spor * exp(rSpor * x) / (KSpor + n0Spor * (exp(rSpor * x) - 1))
      # Change to proportion; sporulation is divided by sporulation at the equilibrium
      spor_rh_hour <-as.numeric(spor_rh_hour)/ as.numeric(KSpor)
      return(spor_rh_hour)
    })

    sporulation <-  
      round(sporulation_temp * sporulation_rh, rounding) 
    return(sporulation)
  } 

########################################################
#Survival
########################################################
SolSurv <-
  function(sol,
           params_solsurv,
           rounding = NULL
  ) {
    
    if(is.null(rounding)){rounding = 6}
    B0 <- params_solsurv[, "B0"] %>% as.numeric()
    B1 <- params_solsurv[, "B1"] %>% as.numeric()
    
    Survival <- function(x) {
      Pr <- 1 / (1 + exp(-(B0 - B1 * x)))
    }
    
    sol_surv <-  Survival(sol)
    
    return(round(sol_surv, rounding))
  }



########################################################
#Infection
########################################################

Infection <-
  function(temp,
           rh,
           params_inf,
           rounding = NULL
  ) {
    
    if(is.null(rounding)){rounding = 6}
    #Import parameters
    #Temp factor zoospore
    TminInf <- params_inf[, "TminInf"] %>% as.numeric()
    ToptInf <- params_inf[, "ToptInf"]%>% as.numeric()
    TmaxInf <- params_inf[, "TmaxInf"]%>% as.numeric()
    RfactInf <- params_inf[, "RfactInf"]%>% as.numeric()
    ShapeInf <- params_inf[, "ShapeInf"]%>% as.numeric()
    #Temp factor direct
    TminInfDir <- params_inf[, "TminInf"]%>% as.numeric()
    ToptInfDir <- params_inf[, "ToptInfDir"]%>% as.numeric()
    TmaxInfDir <- params_inf[, "TmaxInf"]%>% as.numeric()
    
    
    RfactInfDir <- params_inf[, "RfactInfDir"]%>% as.numeric()
    ShapeInfDir <- params_inf[, "ShapeInfDir"]%>% as.numeric()
    
    
    # Temperature intersect between functions for mechanisms of infection
    CalcIntersect <- function(TminInf,ToptInf,TmaxInf,RfactInf,ShapeInf,
                              TminInfDir,ToptInfDir,TmaxInfDir,RfactInfDir,ShapeInfDir){
      temp <- c(0:34)
      
      temp_inf_zoo <- sapply(temp, function(x) {
        Infection_temperature <-
          RfactInf * ((TmaxInf - x) / (TmaxInf - ToptInf) * ((x - TminInf) / (ToptInf - TminInf)) ^ ((ToptInf - TminInf) / (TmaxInf - ToptInf))) ^  ShapeInf
        Infection_temperature = ifelse(x < TminInf | x > TmaxInf, 0, Infection_temperature)
      }) %>% unlist() %>% as.numeric()
      
      #Function to calculate the Infection temperature factor     
      temp_inf_direct <- sapply(temp, function(x) {
        Infection_temperature <-
          RfactInfDir * ((TmaxInfDir - x) / (TmaxInfDir - ToptInfDir) * ((x - TminInfDir) / (ToptInfDir - TminInfDir)) ^ ((ToptInfDir - TminInfDir) / (TmaxInfDir - ToptInfDir))) ^  ShapeInfDir
        Infection_temperature = ifelse(x < TminInfDir | x > TmaxInfDir, 0, Infection_temperature)
      }) %>% unlist()
      
      # Find the curve intersect
      x <- which(temp_inf_zoo==RfactInf) #peek of zoospore germiantion
      y <- which(temp_inf_direct==RfactInfDir) #peek of direct germination
      
      # Straight lines (empirical)
      line1 <- data.frame(x = temp[x:y], y = temp_inf_direct[x:y])
      line2 <- data.frame(x = temp[x:y], y = temp_inf_zoo[x:y])
      
      intersect <-  reconPlots::curve_intersect(line1, line2) %>% as.data.frame()
      
      InterTemp <- intersect[, "x"]
      return(InterTemp)
    }
    
    InterTemp <-  
      CalcIntersect(TminInf,ToptInf,TmaxInf,RfactInf,ShapeInf,
                                TminInfDir,ToptInfDir,TmaxInfDir,RfactInfDir,ShapeInfDir)
    
    
    #Calculate temp factor
    #Function to calculate the Infection temperature factor
    temp_inf_zoo <- sapply(temp, function(x) {
      Infection_temperature <-
        RfactInf * ((TmaxInf - x) / (TmaxInf - ToptInf) * ((x - TminInf) / (ToptInf - TminInf)) ^ ((ToptInf - TminInf) / (TmaxInf - ToptInf))) ^  ShapeInf
      Infection_temperature = ifelse(x < TminInf |
                                       x > TmaxInf, 0, Infection_temperature)
    }) %>% unlist()
    
    #Function to cacluate the Infection temperature factor
    temp_inf_direct <- sapply(temp, function(x) {
      Infection_temperature <-
        RfactInfDir * ((TmaxInfDir - x) / (TmaxInfDir - ToptInfDir) * ((x - TminInfDir) / (ToptInfDir - TminInfDir)) ^ ((ToptInfDir - TminInfDir) / (TmaxInfDir - ToptInfDir))) ^  ShapeInfDir
      Infection_temperature = ifelse(x < TminInfDir |
                                       x > TmaxInfDir, 0, Infection_temperature)
    })%>% unlist()
    
    
    inf_temp <- 
      ifelse(temp <= InterTemp, temp_inf_zoo, temp_inf_direct )
    
    #RH factor
    RhminInf <- params_inf[, "RhminInf"]
    RhoptInf <- params_inf[, "RhoptInf"]
    
    
    # Calculate the RH factor
    #fit linear model
    x <- c(RhminInf, RhoptInf) %>% as.numeric()
    coefs <- lm(c(0, 1) ~ x)[["coefficients"]]
    
    inf_rh <- sapply(rh, function (rhum_val) {
      if (rhum_val > RhminInf & rhum_val <= RhoptInf) {
        SporRh <-  coefs[["(Intercept)"]] + coefs[["x"]] * rhum_val
      } else if (rhum_val > RhoptInf & rhum_val <= 100) {
        SporRh <- 1
      } else {
        SporRh <- 0
      }
    })
    
    infection = round(inf_temp * inf_rh, rounding)
    return(infection)
  } 



########################################################
#Sun info
########################################################

#https://github.com/cran/HelpersMG/blob/master/R/sun.info.R


SunInfo <- function(date, latitude, longitude, 
                    #Ireland uses Irish Standard Time (IST, UTC+01:00 in the summer months
                    #and Greenwich Mean Time (UTC+0) in the winter period.
                    UTC_zone
){
  
  d <- as.numeric(as.POSIXlt(date)$yday)+1
  Lat <- latitude
  Long <- longitude
  
  ## d is the day of year
  ## Lat is latitude in decimal degrees
  ## Long is longitude in decimal degrees (negative == West)
  
  ##This method is copied from:
  ##Teets, D.A. 2003. Predicting sunrise and sunset times.
  ##  The College Mathematics Journal 34(4):317-321.
  
  ## At the default location the estimates of sunrise and sunset are within
  ## seven minutes of the correct times (http://aa.usno.navy.mil/data/docs/RS_OneYear.php)
  ## with a mean of 2.4 minutes error.
  
  ## Function to convert degrees to radians
  rad <- function(x) pi*x/180
  
  ##Radius of the earth (km)
  R=6378
  
  ##Radians between the xy-plane and the ecliptic plane
  epsilon=rad(23.45)
  
  ##Convert observer's latitude to radians
  L=rad(Lat)
  
  ## Calculate offset of sunrise based on longitude (min)
  ## If Long is negative, then the mod represents degrees West of
  ## a standard time meridian, so timing of sunrise and sunset should
  ## be made later.
  timezone = -4*(abs(Long)%%15)*sign(Long)
  
  ## The earth's mean distance from the sun (km)
  r = 149598000
  
  theta = 2*pi/365.25*(d-80)
  
  z.s = r*sin(theta)*sin(epsilon)
  r.p = sqrt(r^2-z.s^2)
  
  t0 = 1440/(2*pi)*acos((R-z.s*sin(L))/(r.p*cos(L)))
  
  ##a kludge adjustment for the radius of the sun
  that = t0+5 
  
  ## Adjust "noon" for the fact that the earth's orbit is not circular:
  n = 720-10*sin(4*pi*(d-80)/365.25)+8*sin(2*pi*d/365.25)
  
  ## now sunrise and sunset are:
  sunrise = (n-that+timezone)/60
  sunset = (n+that+timezone)/60
  
  UTC <- (((7.5+Long)%%360)) %/% 15
  if (UTC>12) {UTC <- 12-UTC;tz <- "Etc/GMT"} else {tz <- "Etc/GMT+"}
  tz <- paste0(tz, UTC)
  
  df <-
    data.frame(
      sunrise = sunrise,
      sunset = sunset,
      day.length = sunset - sunrise,
      date.time.sunrise = as.POSIXlt(format(date, "%Y-%m-%d"), tz =
                                       tz) + sunrise * 60 * 60,
      date.time.sunset = as.POSIXlt(format(date, "%Y-%m-%d"), tz =
                                      tz) + sunset * 60 * 60
    )
  
  sunrise.UTC <-
    as.POSIXlt(
      format(df$date.time.sunrise, format = "%Y-%m-%d %H:%M:%S"),
      tz = "UTC",
      use.tz = TRUE
    )
  sunrise.UTC.dec <-
    sunrise.UTC$hour + sunrise.UTC$min / 60 + sunrise.UTC$sec / 3600
  sunset.UTC <-
    as.POSIXlt(
      format(df$date.time.sunset, format = "%Y-%m-%d %H:%M:%S"),
      tz = "UTC",
      use.tz = TRUE
    )
  sunset.UTC.dec <-
    sunset.UTC$hour + sunset.UTC$min / 60 + sunset.UTC$sec / 3600
  
  #Return 
  df <-
    data.frame(
      # df,
      sunrise = sunrise.UTC + c(UTC_zone*60*60),
      sunset = sunset.UTC  + c(UTC_zone*60*60)
      # time.sunrise.UTC = sunrise.UTC.dec,
      # time.sunset.UTC = sunset.UTC.dec
    )
  
  return(df)
}


########################################################
#Calculation of cut-off times
########################################################

GetTimes <- function(fun_df) {
  # TODO Check if there is a need to set up 
  #'Function to calculate the times of sunrise and sunset. 
  #'
  
  # Set the sunset and sunrise manually at 20/6
  # fun_df[fun_df$hour == 20, "daytime"] <- "sunset"
  # fun_df[fun_df$hour == 6, "daytime"] <- "sunrise"
  if(all(!str_detect(colnames(fun_df), fixed("lon", ignore_case=TRUE)))){  stop("No Longitude reference or it is not named: 'lon'!")}
  if(all(!str_detect(colnames(fun_df), fixed("lat", ignore_case=TRUE)))){  stop("No Latitude reference or it is not named: 'lat'!")}

  lat <- fun_df[[ExtractCol(fun_df, "lat")]] 
  lon <- fun_df[[ ExtractCol(fun_df, "lon")]] 
  
  #Extract dates and geo-coordinates
  tempdf <- 
    data.frame(
      date = unique(fun_df$short_date),
      lat = unique(lat),
      long = unique(lon)
    ) 
  
  fun_ls <-  list()
  for (i in seq_along(1:nrow(tempdf))) {
    fun_ls[[i]] <- SunInfo(tempdf[i,"date"], 
                           tempdf[i, "lat"], 
                           tempdf[i,"long"],
                           UTC_zone=1)
  }
  
  tempdf <- 
    fun_ls %>% 
    bind_rows() %>% 
    bind_cols(tempdf, .) %>% 
    mutate(sunset_hr = lubridate::hour(sunset),
           sunrise_hr = lubridate::hour(sunrise),
           doy = lubridate::yday(date)) %>% 
    select(c("doy", "sunrise_hr", "sunset_hr"))
  
  
  
  #Retun sunset and sunrise times as a data frame
  return(tempdf)
  
}


########################################################
#Calculate cumulative risk
########################################################




















































