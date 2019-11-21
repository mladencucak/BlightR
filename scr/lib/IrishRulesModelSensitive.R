# wth_ls[[8]] -> weather
# lss[[1]] -> weather
# param = NULL
# infill_gap = NULL
# rain <-  rep(NA, length(rain))
# weather$rain <- NA

# IrishRulesModel(lss[[4]])
    

IrishRulesModel <- function(weather,
                            param = "modified",
                            param_set = NULL,
                            infill_gap = NULL,
                            replace_na = FALSE,
                            max_na = NULL,
                            temporal_res = "hourly") {
  #' Irish Rules
  #'
  #' This function calculates potatolate blight risk using Irish Rules model (Bourke, 1953)
  #' @param weather The weather data in formated as data frame
  #' @param param Set of parameters to be used for model run. The default setting is to use modified version. 
  #' @param infill_gap Maximum alowed gap for missing value interpolation
  #' @keywords Irish Rules
  #'
  # wetness requirement prior to infection accumulation start
  # time window of 6 hours, 3 before/after sporulation ends
  
  #Load packages 
  pkg <- c("tidyverse","lubridate", "zoo")
  if(any(pkg %in% installed.packages())==FALSE ){
    print("Required packages need to be downloaded!")
  } else {
    lapply(pkg, require, character.only = TRUE); rm(pkg)
  }
  
  
  wet_before <- 3
  wet_after <- 3
  
  # Parameter list
  if (param == "modified") {
    rh_thresh <- 88
    temp_thresh <- 10
    hours <- 10   #sum of hours before EBH accumulation
    lw_rhum <- "rainrh"
  }
  if (param == "default") {
    rh_thresh <- 90
    temp_thresh <- 10
    hours <- 12   
    lw_rhum <- "rainrh"
  } 
   
  if (param == "custom") {
    #pass a vector of parameters
    rh_thresh <- as.numeric(param[2])
    temp_thresh <- as.numeric(param[3])
    hours <- as.numeric(param[4])
    lw_rhum <- param[5]  #if is NA then only rain data will be used
  }
  
  #threshold for estimation of leaf wetness using relative humidity
  lw_rhum_threshold <- 90
  
  
  weather[[ExtractCol(weather, "rain")]] -> rain
  weather[[ExtractCol(weather, "rh")]] -> rh
  weather[[ExtractCol(weather, "temp")]] -> temp
  
  # Add day of year if the column is not present in the data and the desired 
  # output is in daily temporal resolution
  if (!"doy" %in% colnames(weather)&temporal_res == "daily"){
    date_col <- names(weather)[sapply(weather, function(x) is.POSIXct(x)|is.POSIXlt(x)) %>% as.logical()]
    weather[["doy"]] <- yday(weather[[date_col]])
  }
  
  # This function to infil missing values to let the model run
  #If maximum infill gap is not provided it is defaulted to 7
  if (is.null(infill_gap)) {
    infill_gap <- 7
  }
  
  if (sum(is.na(with(weather, rain, temp, rhum))) > 0) {
    temp <-
      round(zoo::na.spline(temp, na.rm = FALSE, maxgap = infill_gap), 1)
    rh <-
      round(zoo::na.spline(rh, na.rm = FALSE, maxgap = infill_gap), 0)
    rh  <- sapply(rh, function(x)
      ifelse(x > 100, x <- 100, x))
  }

  #If the data has more than 1% of missing values stop the function 
  if(isTRUE(replace_na) ){
  if(is.null(max_na)) max_na <- 0.01
  if (mean(is.na(weather[, c("temp", "rhum")])) > max_na){
    stop("Percentage of missing values for relative humidity and temperature is higher than ", max_na, "!")
  }  #replace NA with 0
  rh[is.na(rh)] <- 0
  temp[is.na(temp)] <- 0
  }
  
  
  if (sum(is.na( c( temp, rh))) > 0) {
    stop(print("The sum of NAs is more than 7! Check your weather data."))
  }
  
  # "Out of boounds"
  rain <- c(rain, rep(0, 20))
  temp <- c(temp, rep(0, 20))
  rh <- c(rh, rep(0, 20))
  
  # conditions for sporulation
  criteria <- as.numeric(temp >= temp_thresh & rh >= rh_thresh)
  
  # cumulative sum of hours that meet the criteria for sporulatoion with restart at zero
  criteria_sum <-
    stats::ave(criteria, cumsum(criteria == 0), FUN = cumsum)
  
  # Initiate risk accumulation vector
  risk <- rep(0, length(temp))
  
  criteria_met12  <-
    as.numeric(criteria_sum >= hours) #accumulation of EBH starts after sporulation
  idx             <- which(criteria_sum == hours)
  
  #If there are no accumulations return vector with zeros
  if (sum(criteria_sum == hours) == 0) {
    #breaks the loop if there is no initial accumulation of 12 hours
    if(temporal_res == "daily"){ 
      final <-tapply(head(risk, -20), weather$doy, max)
    }
    if(temporal_res == "hourly"){
      final <- head(risk,-20) #remove last 20 values that were added to vectors to prevent "Out of bounds" issue
    }
    return(final)
  } else{
    for (j in 1:length(idx)) {
      #switch that looks if there was wetness: first rain, then both rain and rh, if rh exists
      if (if (lw_rhum == "rain") {
        #if only rain
        (sum(rain[(idx[j] - wet_before):(idx[j] + wet_after)]) >= 0.1)           #just see rain sum
      } else{
        #If there is no rain data just consider rh threshold
        rain_missing <-
          rain[(idx[j] - wet_before):(idx[j] + wet_after)]
        if (any(is.na(rain_missing))) {
          any((any(rh[(idx[j] - wet_before):(idx[j] + wet_after)] >= lw_rhum_threshold)))
        } else {
          any((any(rh[(idx[j] - wet_before):(idx[j] + wet_after)] >= lw_rhum_threshold)) |
              #take both as possible switches
              (sum(rain[(idx[j] - wet_before):(idx[j] + wet_after)]) >= 0.1))
        }
        
        
      })
      # outputs true or false
      {
        n <- idx[j]        #start accumulation from 12th hour
      } else {
        n <- idx[j] + 4      #start accumulation from 16th hour
      }
      s <- criteria_met12[n]
      
      # if a break of less than or equal to 5 hours
      m <- n - 1
      
      while (s == 1)
      {
        risk[n] <- risk[m] + 1
        n <- n + 1
        m <- n - 1
        s <- criteria[n]
        if (s == 0 && (criteria[n + 2] == 1)) {
          n = n + 2
          s = 1
        } else if (s == 0 && (criteria[n + 3] == 1)) {
          n = n + 3
          s = 1
        } else if (s == 0 && (criteria[n + 4] == 1)) {
          n = n + 4
          s = 1
        } else if (s == 0 && (criteria[n + 5] == 1)) {
          n = n + 5
          s = 1
        }
      }
      
    }
    if(temporal_res == "daily"){ 
      
      final <-tapply(head(risk, -20), weather$doy, max)
    }
    if(temporal_res == "hourly"){
      final <- head(risk,-20) #remove last 20 values that were added to vectors to prevent "Out of bounds" issue
    }
    return(final)
   
  }
  
}
