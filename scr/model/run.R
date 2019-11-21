# source(here::here("scr", "pkg.R"))
# load(file = here::here("dat", "outbreak&weather.Rdata"))
# data_ls[[1]]-> wth
# x-> wth

# fun_df <-lss[[1]]
# max_na = NULL
# temporal_res = "daily" #resoulution of the final data to be returned, daily or hourly. if hourly returned at noon
# run_type= "model"
# model_parameters = "default"


BlightR <- function(fun_df,
                    max_na = NULL, 
                    temporal_res = "daily", #resoulution of the final data to be returned, daily or hourly. if hourly returned at noon 
                    run_type= "wth",
                    model_parameters = "default",
                    return_cols = NULL
                    ) 
{
  #' Blight model.
  #'
  #' This function calculates potato late blight risk.
  #' The risk needs to be calculated for a single location, so if the
  #' calculation is necessary for multiple locations, data for each location needs to 
  #' run separately. 
  #' 
  #' @param fun_df The weather data formated as data frame. 
  #' @param max_na Maximum proportion of missing values. Set to 0.03 by default.  
  #' @param infill_gap Maximum alowed gap for missing value interpolation
  #' @param run_type Two running types determine the format of the final data frame. If it is "wth" then there will be more columns.
  #' @param return_cols User defined veto of column names to be returned with daily the model outputs. 
  #' @keywords potato late blight, model, decision support

  #' spor sum of sporulation
  #' inf infection 
  #' surv_prob Spore mortailty factor
  #' inf_sol Risk as sum of infectiona nd sporulation reduced by moratlity.
  #' risk Risk as product of sporulation, spore survival and infection. 
  #'  
  #' The model will calculate the risk even if there are missing values in the data.
  #' It will not return risk values for the day when 
  #'  
  
  
  #Load packages 
  pkg <- c("tidyverse","lubridate", "zoo")
  if(any(pkg %in% installed.packages())==FALSE ){
    print("Required packages need to be downloaded!")
  } else {
    lapply(pkg, require, character.only = TRUE); rm(pkg)
  }

  #If the data has more than max_na proportion of missing values stop the function 
  if(is.null(max_na)) max_na <- 0.01
  if (mean(is.na(fun_df[, c("temp", "rhum")])) > max_na){
    stop("Percentage of missing values for relative humidity and temperature is higher than ", max_na, "!")
  }
  
  source(here::here("scr", "model", "fun.R"))#Model functions

  # Parameters 
  if(model_parameters == "default"){
  parameters <- read.csv( here::here("scr", "model", "par", "par_default.csv"))
  }else {
    
    parameters <- readxl::read_xlsx(here::here("scr", "model", "par_eval", "par_eval.xlsx"))
    parameters <- 
      parameters[parameters$met_set == model_parameters,] %>% 
      select(-c("met_par", "met_level", "met_set"))
  }
  
  
  #change col name of date time column
  if( !"date" %in% colnames(fun_df)){
  date_col <- names(fun_df)[sapply(fun_df, function(x) is.POSIXct(x)|is.POSIXlt(x)) %>% as.logical()]
  fun_df <-  rename(fun_df, date = date_col)
}
  
  
  if(!"hour"%in% colnames(fun_df)) fun_df$hour <- lubridate::hour(fun_df$date)
  if(!"doy"%in% colnames(fun_df)) fun_df$doy <- lubridate::yday(fun_df$date)
  
  colnames <- c("date","doy","temp", "rhum")
  if(all(colnames %in% colnames(fun_df))==FALSE) stop("Rename column names. "); rm(colnames)
  
  
  
  
  ################################
  #Model Run
  #################################
  
  #Calculate initiation/termiantion points for sporulation and infection
  # Calculate an approximate sunrise and sunset times 
  #https://www.timeanddate.com/sun/@2963597
  timedf <- GetTimes(fun_df = fun_df)
  
  #Define number of days for risk estimation
  # Risk can be calculated for from day 2, because sporulaiton is calculated for 
  # the previous night. Same goes for the infection - it extends to the following morning
  begin <- unique(fun_df$doy)[2] 
  end <- unique(fun_df$doy)[length(unique(fun_df$doy))-1] 
  
  result_ls <- list()

  #Define the start of sporulation 
  # i = 141
  for(i in c(begin:end)){
    sporstart <- timedf[ timedf$doy == c(i-1), "sunset_hr"] - parameters[ ,"hr_before_spor"] %>% as.numeric()
    infstop <- timedf[ timedf$doy == c(i+1), "sunrise_hr"] + parameters[ ,"hr_after_inf"] %>% as.numeric()
    
    daydf <- 
      do.call("rbind", 
              list(
                fun_df[with(fun_df, hour >= sporstart & doy == c(i-1)),],# from the afternoon the day before
                fun_df[with(fun_df,  doy == i),],
                fun_df[with(fun_df, hour <= infstop & doy == c(i+1)),] #ending in the morning on the day after
              )
      )
    
    bind_rows( list(
      fun_df[with(fun_df, hour >= sporstart & doy == c(i-1)),],# from the afternoon the day before
      fun_df[with(fun_df,  doy == i),],
      fun_df[with(fun_df, hour <= infstop & doy == c(i+1)),] #ending in the morning on the day after
    ))
    # TODO finnish NAs and solar radiation column in one
    # fun_df[ 48, "temp" ] <- NA
              
    #If there are missing values just return rows with NAs
    if (any(is.na(daydf[, c("temp")])|is.na(daydf[, c("rhum")]))) {
      final <- data.frame(
        doy = i,
        spor = NA,
        spor_cond = NA,
        inf = NA,
        surv_prob =NA,
        inf_sol = NA,
        risk_si = NA,
        risk_mi = NA,
        risk = NA
      )
      
    } else{ #start of if is not NA statement
      
     ############################
     #Sporulation
     ############################
      #Calculate sporulation
      daydf$spor <-  
        Sporulation(daydf$temp, daydf$rhum, parameters)
    
      # Stop the sporulation n(hr_after_spor) hours after sunrise
      sporstop <-  timedf[timedf$doy == i, "sunrise_hr"] + parameters[ ,"hr_after_spor"] %>% as.numeric() 
      daydf[daydf$doy == i & daydf$hour >= sporstop |  daydf$doy == c(i+1), c("spor")] <- 0
      
      #cumulative sporulation per event
      # TODO check this sum
      daydf$spor_sum <- 
        ave(coalesce(daydf$spor, 0), data.table::rleid(zoo::na.locf(daydf$spor != 0,maxgap = 3)), FUN = cumsum)
      
      # daydf[daydf$doy == i & daydf$hour < sporstop |  daydf$doy == c(i-1), c("spor", "spor_sum")] 
      
      # check if there is 10 hours for sporulation
      criteria<- as.numeric(daydf$spor>0)
      
      #cumulative sum of hours that met the criteria with restart at zero
      criteria_sum <-  stats::ave(criteria, cumsum(criteria == 0), FUN = cumsum)
      
      risk <- rep(0, nrow(daydf))
      
      hours <- parameters[ ,"spor_dur"]
      criteria_met  <- as.numeric( criteria_sum >= hours )
      idx           <- which(criteria_sum == hours)
      
      
      
      # If the sporulation criteria was not met calculate risk based on mortality and infection periods
      # The infection period starts in the morning and lasts until next morning
      if(sum(criteria_met)==0){
        
        inf_start <-  timedf[timedf$doy == i, "sunrise_hr"] + parameters[ ,"hr_after_spor"] %>% as.numeric() 
        idx <- which(daydf$doy == i & daydf$hour == inf_start)
        
        ############################
        #Infection
        ############################
        # Calculate the infection period 
        # Starts after the conditions for sporulation have been met
        # Sum of Infection and sporulation for each hour reduced by survival
        
        daydf$inf <- 0
        daydf[idx:nrow(daydf), "inf"] <-
          Infection(temp = daydf[idx:nrow(daydf), "temp"], 
                    rh = daydf[idx:nrow(daydf), "rhum"], 
                    params_inf = parameters)
        
        
        
        #Cumulative sum of the infection after sporulation requirement has been met
        #The initial value is total sporulation of the day
        # daydf[idx-1,"inf"] <- max(daydf$spor_sum)
        daydf[c(idx-1):nrow(daydf),"cumul_inf"] <- 
          cumsum( daydf[c(idx-1):nrow(daydf),"inf"])
        
        # Calculation using the 
        # daydf$inf[daydf$inf==0, ] <- NA
        # daydf[c(idx-1):nrow(daydf),"cumul_inf"] <- 
        #   ave(coalesce(daydf$inf, 0), data.table::rleid(zoo::na.locf(daydf$inf != 0,maxgap = 4)), FUN = cumsum)
        
        ############################
        #Survival
        ############################
        # Calculate the mortality of spores due to solar raditaion if there is solar radiation data
        
        #Airborne sporangia survival
        #The estimated probablity of spore survival is calculated using 
        # The spore load was calculated as a product of total daily sporulation risk and the         
        # probability of spornagia survival as a function of solar radiation
        solar_rad <-
          ifelse(!all(is.na(daydf[daydf$doy == i, "sol_rad"])), #if there are measured values
                 sum(daydf[daydf$doy == i, "sol_rad"]), #sum measured values
                 sum(daydf[daydf$doy == i, "sol_nasa"], na.rm = T) #otherwise use estimated values)
          )
        surv_prob <- SolSurv(solar_rad, parameters)
        
        ############################
        #Risk calculation
        ############################
        # Risk estimation based on mortality and infection risk estimation
        risk_mi <- 
          max(daydf$cumul_inf, na.rm = TRUE)* 
          surv_prob
        
        
      final <- data.frame(
          doy = i,
          spor = max(daydf$spor_sum, na.rm = TRUE),
          spor_cond = "no",
          inf = 0,
          surv_prob = 0,
          risk_si = 0,
          risk_mi = round(risk_mi, 6),
          risk = 0,
          stringsAsFactors =FALSE
        )
      result_ls [[i]]<-final
      }else {
      ############################
      #Infection
      ############################
      # Calculate the infection period 
      # Starts after the conditions for sporulation have been met
      # Sum of Infection and sporulation for each hour reduced by survival
      daydf$inf <- 0
      daydf[idx:nrow(daydf), "inf"] <-
        Infection(temp = daydf[idx:nrow(daydf), "temp"], 
                  rh = daydf[idx:nrow(daydf), "rhum"], 
                  params_inf = parameters)
      
      
      
      #Cumulative sum of the infection after sporulation requirement has been met
      #The initial value is total sporulation of the day
      # daydf[idx-1,"inf"] <- max(daydf$spor_sum)
      daydf[c(idx-1):nrow(daydf),"cumul_inf"] <- 
        cumsum( daydf[c(idx-1):nrow(daydf),"inf"])

      # Calculation using the 
      # daydf$inf[daydf$inf==0, ] <- NA
      # daydf[c(idx-1):nrow(daydf),"cumul_inf"] <- 
      #   ave(coalesce(daydf$inf, 0), data.table::rleid(zoo::na.locf(daydf$inf != 0,maxgap = 4)), FUN = cumsum)
            
      ############################
      #Survival
      ############################
      # Calculate the mortality of spores due to solar raditaion if there is solar radiation data

      #Airborne sporangia survival
      #The estimated probablity of spore survival is calculated using 
      # The spore load was calculated as a product of total daily sporulation risk and the         
      # probability of spornagia survival as a function of solar radiation
      solar_rad <-
        ifelse(!all(is.na(daydf[daydf$doy == i, "sol_rad"])), #if there are measured values
               sum(daydf[daydf$doy == i, "sol_rad"]), #sum measured values
               sum(daydf[daydf$doy == i, "sol_nasa"], na.rm = T) #otherwise use estimated values)
        )
      surv_prob <- SolSurv(solar_rad, parameters)
      
      # TODO Needs fixing 
      # #Reduce the sporulation estimation based on the mortality due to solar radiation 
      #  reduced_spore_load <-  max(daydf$spor_sum)* surv_prob
      #   
      #    cumsum( c(surv_prob,daydf$spor_sum))
      #    
      #  daydf[c(idx-1):nrow(daydf),"cumul_inf_sol"] <- 
      #    cumsum( c(surv_prob,daydf[c(idx-1):nrow(daydf),"inf"]))[2: c(length(daydf$spor_sum)+1)] #cumulative sum of inf reduced by sol survival
        

      
      ############################
      #Risk calculation
      ############################
      # Risk is calculated as a product of sporulation, spore mortality due to the solar radiation and infection risk
      risk <- 
      max(daydf$spor_sum, na.rm = TRUE) * 
        max(daydf$cumul_inf, na.rm = TRUE)* 
        surv_prob
      
      # Risk estimation based on sporulation and infection risk estimation
      risk_si <- 
        max(daydf$spor_sum, na.rm = TRUE) * 
        max(daydf$cumul_inf, na.rm = TRUE)
      
      # Risk estimation based on mortality and infection risk estimation
      risk_mi <- 
        max(daydf$cumul_inf, na.rm = TRUE)* 
        surv_prob
      
      
      
      final <- 
        data.frame(
        doy = i,
        spor = max(daydf$spor_sum, na.rm = TRUE),
        spor_cond = "yes",
        inf = max(daydf$cumul_inf, na.rm = TRUE),
        surv_prob =surv_prob,
        risk_si = round(risk_si, 6),
        risk_mi = round(risk_mi, 6),
        risk = round(risk,6),
        stringsAsFactors =FALSE
        )

      ############################
      #Testing
      ############################
      # daydf[, c("hour", "spor", "spor_sum", "inf", "cumul_inf_sol")] 
      
      # ggplot(daydf)+
      #   geom_point(aes(date_time , spor_sum, color = "Sporulation"))+
      #   geom_line(aes(date_time , spor_sum, color = "Sporulation"))+
      #   # geom_point(aes(date_time , cumul_inf, color = "red"))+
      #   #
      #   # geom_line(aes(date_time , cumul_inf, color = "red"))+
      #   geom_point(aes(date_time , cumul_inf, color = "Infection"))+
      #   geom_line(aes(date_time , cumul_inf, color = "Infection"))+
      #   scale_color_manual("Sub-Model:",
      #                      values = c("Sporulation" = "blue",
      #                                 "Infection" = "red"))+
      #   labs(title = "Example Daily Model Outputs",
      #        # subtitle = "The infection risk starts in the morning, stagnates during the day and increses\n during the night. Heavy infection conditions.",
      #        y = "Risk",
      #        x = "Time")+
      #   theme_bw()

      }#end of if is not NA statement
  
      
      result_ls [[i]]<-final
    }
  }
  
  fin <-
    left_join( timedf, bind_rows(result_ls),by = "doy") %>%
    select(-c("sunrise_hr", "sunset_hr"))

#############################
  #Cumulative risk 
  ###########################
  # CumulRisk <- function(x){
  #   x <- fin$risk
  #   y[1:2] <- x[1:2] 
  #  for(i in c(2:length(x))){
  #    decay <- max_time_cumul/100
  #    
  #   ifelse(x[i]>0&x[i+1] ==, 
  #              x[i],
  #                 )
  #  }
  # }
  
  # 
  # fin$cumul_risk[3: nrow(fin)] <-
  #   rollapply(fin$risk,
  #             width = 3, #specify the widht of the window
  #             sum,
  #             align = "center") %>% round(6)
  # 
  #   
  #  fin
  # 
  #  fin <- fin[3:nrow(fin),]

  fin[ , c("cumul_risk_si", "cumul_risk_mi", "cumul_risk")] <- NA
  
   fin$cumul_risk_si[!is.na(fin$risk_si)] <- 
     stats::ave(fin$risk_si[!is.na(fin$risk_si)], cumsum(fin$risk_si[!is.na(fin$risk_si)] == 0), FUN = cumsum)
 
   fin$cumul_risk[!is.na(fin$risk)] <- 
     stats::ave(fin$risk[!is.na(fin$risk)], cumsum(fin$risk[!is.na(fin$risk)] == 0), FUN = cumsum)
   
   fin$cumul_risk_mi[!is.na(fin$risk_mi)] <- 
     stats::ave(fin$risk_mi[!is.na(fin$risk_mi)], cumsum(fin$risk_mi[!is.na(fin$risk_mi)] == 0), FUN = cumsum)
   
   
   ###############################
   # Final results
   ###############################
     
   DailyAt <-  function(x, time) { 
    xx <- c(sapply(x, function(x) c(rep(NA,23),x)))
    x <- c(xx[time:length(xx)], xx[2:time])
    return(x)
  }
  
   #convinience function
   # TODO remove later
   if(run_type== "model"){
     #Add variable dbo - days before the onset
     fun_df <-
       fun_df %>%
       group_by(doy) %>%
       mutate(dbo = max(fun_df$doy) - doy) %>%
       dplyr::ungroup()
   vars_to_return <- 
   c("year", "doy", "month", "short_date","variety", "lat", "long", "comments", "stna", "dist", "dbo" )
   }
   if(run_type== "wth"){
     vars_to_return <- 
       c("id", "year", "doy", "month", "short_date","lat", "long")
   }
   if(run_type== "fore"){
     vars_to_return <- 
       c("id", "set", "for_date", "doy", "stna","short_date" )
   }
   if(run_type== "custom"){
     #User needs to select the columns to be returned with the model outputs
     # VAlues gave to be same for each group, and grouping is per day (such as date, location, day of year, etc.)
     vars_to_return <- return_cols
       c("id", "set", "for_date", "doy", "stna","short_date" )
   }
   
  fun_df <-
  tbl_df(fun_df) %>% 
  select(vars_to_return)
  
  if(temporal_res == "daily") {
    final <-
      fun_df %>%
      group_by(doy) %>%
      dplyr::summarize_all(unique) %>%
      left_join(., fin, by = "doy")
  }
 
  if(temporal_res == "hourly"){
    fin_hourly <-  
      data.frame(
        spor =   DailyAt(fin$spor, 0),
        spor_inf =   DailyAt(fin$spor_inf, 2),
        inf = DailyAt(fin$inf, 5),
        surv_prob = DailyAt(fin$inf_sol, 8),
        risk_mi = DailyAt(fin$risk_mi, 11),
        risk_si = DailyAt(fin$risk_si, 14),
        risk =  DailyAt(fin$risk, 17),
        cumul_risk_mi = DailyAt(fin$cumul_risk_mi, 19),
        cumul_risk_si = DailyAt(fin$cumul_risk_si, 21),
        cumul_risk = DailyAt(fin$cumul_risk, 23)
      )
    fin_hourly <- cbind(fun_df[ ,vars_to_return],fin_hourly)
    final <- fin_hourly
  }

  return(final)
}




  