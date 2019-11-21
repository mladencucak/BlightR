

################################
#Calculate number of treatments
################################

ControlFreqFun <-
  function(cutoff,
           warn_t_df, #df with warning thresholds 
           data, #data to evaluate
           no_cal = NULL,   #
           min_prot_dur = NULL) {
    
    # data <- default_res_ls[2][[1]][[1]]
    # cutoff = 3
    # data = default_res_ls[2][[1]][[1]]
    # no_cal = 
    # min_prot_dur = min_prot_dur
      
    y <- cutoff 
    #Weather and model output data
    fun_df <- data
    
    #Each warning would cause treatment that will keep the plants protected for a period of time
    min_prot_dur <-
      ifelse(is.null(min_prot_dur), 5, min_prot_dur)#If not defined default value is 5 days
    
    
    models <-  colnames(fun_df[, grepl("risk" , names(fun_df))])
    
    #Threshold is considered reached when 
    #if more or equal to the given warning thr. then 1
    # Replace missing values with 0 so that calculation works
    for (i in models) {
      fun_df[, i] <-
        sapply(fun_df[, i],  function(x) {
          #t x[is.na(x)] <- 0
          ifelse(x >= warn_t_df[y, i], 1, 0)
        })
    }
    
    
    
    ###################################################################################
    #Calculate the number of treatments with with minimum protection duration of n days
    ###################################################################################
    
    TreatmentWindowOne <-
      function(model_output,
               min_prot_dur) {
         #t model_output <- fun_df[, models[7]]
        y <- model_output %>% unlist() %>% as.numeric()
        y[is.na(y)] <- 0
        # attach n 0s to the end of vector to enable calculation
        y <- c(y, rep(0, min_prot_dur))
        for (i in seq_along(1:c(length(y) - min_prot_dur))) {
          #Following treatment will be implemented on day 5 if there is 1 in the next five days
          if (y[i] == 1 &
              sum(y[c(i + 1):c(i + min_prot_dur)], na.rm = TRUE) > 0) {
            y[i + min_prot_dur] <- 1
            y[c(i + 1):c(i + c(min_prot_dur - 1))] <- 0
          }
        }
        #return 
        y[1:length(unlist(model_output))]
      }
    
    #Calculate the num. of treatments for specific threshold 
    sum_df <-
      fun_df[, models] %>%
      lapply(., function(x)
        TreatmentWindowOne(x, min_prot_dur)) %>%
      bind_cols() %>% 
      summarise_all(., list(trt = ~ sum(., na.rm = TRUE)))
    
    sum_df$warning_thres <-  y
    
    sum_df[, "id"] <- unique(fun_df[, "id"])
    
    return(sum_df)
  }



################################
#Caclulate TPP
#Function that calculates the true positive proportion of the data.
TPPFun <- function(cutoff, data) {
  lapply(data, function(fun_df) {
    
    # default_res_lss[1][[1]][[1]] -> fun_df
    models <-  colnames(fun_df[, grepl("risk" , names(fun_df))])
    
    for (i in models) {
      #if more or equal to the given warning threshold assign 1
      fun_df[, i] <-
        sapply(fun_df[, i],  function(x) {
          ifelse(x >= warn_t_df[cutoff, i], 1, 0)
        })
      rm(i)
    }
    fun_dff <- summarise_all(fun_df[, models], sum)
    fun_dff[1,] <- ifelse(fun_dff[1,] > 1, 1, 0) #if the threhold is reached change to one
    fun_dff$warning_thres <-  cutoff
    return(fun_dff)
  })  %>%
    bind_rows() %>%
    summarise_all(mean)
}



#####################################################################
#Calculate the number of treatments with with minimum protection duration of 5 days
#####################################################################
# Not used 
EvalTable <-  function(tpp_ev_ls, trt_ev_ls ){
models <- 
  tpp_ev_ls[ , grepl("risk", colnames(tpp_ev_ls))] %>% colnames()

trt_ev <- 
  trt_ev_ls %>% 
  bind_rows() %>% 
  select(-starts_with("id")) %>% 
  group_by(warning_thres) %>% 
  summarise_all(mean)

#Convert to 1 - specificity
trt_ev[, c(paste0(models, "_trt"))] <- 
  c(trt_ev[, c(paste0(models, "_trt"))]/max_trt)

# 
trt_ev_long <- 
  trt_ev %>% 
  reshape2::melt(., id.vars = "warning_thres", 
                 variable.name = "model", 
                 value.name = "spec",
                 factorsAsStrings  = FALSE) %>% 
  mutate(model = gsub("_trt", "", model))

tpp_ev_long <- 
  tpp_ev_ls %>% 
  reshape2::melt(., id.vars = "warning_thres", 
                 variable.name = "model", 
                 value.name = "sens",
                 factorsAsStrings  = FALSE)
eval_long <- 
left_join(trt_ev_long, tpp_ev_long, by = c("warning_thres", "model" ))

return(eval_long)
}

