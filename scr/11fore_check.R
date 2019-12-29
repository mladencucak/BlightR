
source(here::here("scr","lib",  "pkg.R"))
# library("readxl")
source(here::here("scr", "lib", "funs.R"))

load( file =here::here("out", "fore", "fore_model_out.Rdata"))

out_ls[[1]]
out_ls[[1]]$id

out_df <-
  out_ls %>%
  bind_rows() %>%
  # select(-id) %>%
  # unite("id", for_date, stna) %>%
  group_by(id) %>%
  mutate(day_step = doy - min(doy - 1)) %>%
  ungroup()



models <-
  colnames(out_df[, grepl("risk" , names(out_df))])

dff <-
  out_df %>%
  select(c(set, for_date, stna, day_step, models)) %>%
  reshape2::melt(
    .,
    id.vars = c("set", "stna", "for_date", "day_step"),
    measure.vars = models,
    variable.name = "model",
    value.name = "risk",
    factorsAsStrings  = FALSE
  ) %>%
  spread(set, risk)
# %>%
#   reshape2::melt( .,
#                   id.vars = c("stna", "for_date", "day_step", "model", "obs"),
#                   measure.vars= c("fore", "obs"),
#                   variable.name = "set",
#                   value.name = "pred",
#                   factorsAsStrings  = FALSE
#   ) %>%
#   filter(set != "obs")

dff$model <- 
  gsub("risk_", "R",  dff$model) %>% 
  gsub("risk", "R",  .) %>% 
  gsub("ir_R", "MIR",  .) %>% 
  gsub("defMIR", "IR",  .) 

dff$model <- 
  factor(dff$model, levels = c(  "R", "Rsi","Rmi", "IR","MIR" ))
 





############################################
#The ranking table for the weather forecast  eval
############################################

load(file = here::here("out", "default", "model_eval.Rdata"))
load(file = here::here("out", "default", "warning_thresholds.Rdata"))

tprs <- c(.8, .85, .9, .95)

cutoffs <- list()

#Create new waring threshold lookup table
for (i in seq_along(tprs)) {
  # #decide on what is minimum accepted proportion of true predictions
  prop_tpp <- tprs[i]
  modelsls <- list()
  for (model in models) {
    #
    # #pick the model
    # model <-  colnames(fun_df[, grepl("risk" , names(fun_df))])[1]
    
    # find two nearest warning thresholds to the accepted decision threshold
    closest_high <- sum(default_eval_lss[[1]][model] > prop_tpp)
    closest_low <-
      which(default_eval_lss[[1]][model] < prop_tpp)[1]
    
    tpp <-
      default_eval_lss[[1]][model][closest_high:closest_low,] %>% unlist()
    # Find the risk estimate for these two thresholds
    risk <-
      warn_t_df[default_eval_lss[[1]]["warning_thres"][closest_high:closest_low,] %>% unlist(), model]
    #estimate the risk for point prop_tpp
    risk_thresh <-
      predict(lm(risk ~ tpp), data.frame(tpp = prop_tpp))
    
    modelsls[[model]] <-
      data.frame(model = as.character(model),
                 tpr = prop_tpp,
                 risk = risk_thresh)
    
  }
  cutoffs[[i]] <- do.call("rbind", modelsls)
  rm(modelsls)
}

warntdf <-
  bind_rows(cutoffs) %>%
  arrange(model, tpr)
warntdf$model <- 
  gsub("risk_", "R",  warntdf$model) %>% 
  gsub("risk", "R",  .) %>% 
  gsub("ir_R", "MIR",  .) %>% 
  gsub("defMIR", "IR",  .) 

warntdf$model <- 
  factor(warntdf$model, levels = c(  "R", "Rsi","Rmi", "IR","MIR" ))


############################################
#Initial evaluation of the full weather forecast
############################################

# rescale to >.8
# Calcualte CCC, RMSE, MSE, r2 - something like false negative rate?
dff %>%
  mutate(obs = ifelse(model == "Rmi", obs*9, obs),
         fore = ifelse(model == "Rmi", fore*9, fore),
         obs = ifelse(model == "R", obs*2, obs),
         fore = ifelse(model == "R", fore*2, fore),
         obs = ifelse(model == "MIR", obs*3, obs),
         fore = ifelse(model == "MIR", fore*3, fore),
         obs = ifelse(model == "IR", obs*3, obs),
         fore = ifelse(model == "IR", fore*3, fore)) %>% 
  ggplot(aes(obs, fore, colour = stna)) +
  geom_point(size = 0.2) +
  # facet_wrap(model~day_step, ncol = 10)+
  facet_grid(model ~ day_step) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    colour = "Station",
    title = "Risk estimation using forecasted versus observed weather data over lead time",
    subtitle = paste("Outputs for ", models),
    x = "Observed",
    y = "Predicted"
  )+
  ggsave(filename = here::here("out", "fore", "fig", "full_data.png"),
         width = 9, height = 8, units = "in")

shell.exec(here::here("out", "fore", "fig", "full_data.png"))




# The Lin’s concordance correlation coefficient provides a measure of overall accuracy which takes into 
# account bias correction (closeness to the actual value) and precision (variability in the estimates).
# Bias correction is calculated from two bias measures: constant bias (location-shift) and systematic
# bias (scale-shift). The precision is the Pearson’s correlation coefficient. 
# The concordance correlation (CCC) is the product of bias correction and precision.

cccdf <-
  dff %>%
  split(., list(dff$model, dff$day_step)) %>%
  lapply(., function(tmp) {
    tmp.ccc <- epiR::epi.ccc(
      tmp$fore,
      tmp$obs,
      ci = "z-transform",
      conf.level = 0.95,
      rep.measure = FALSE
    )
    tmp$lab <-
      paste(
        "CCC: ",
        round(tmp.ccc$rho.c[, 1], digits = 3),
        sep = ""
      )
    tmp$labci <-
      paste(
        " (",
        round(tmp.ccc$rho.c[, 2], digits = 3),
        " - ",
        round(tmp.ccc$rho.c[, 3], digits = 3),
        ")",
        sep = ""
      )
    
    z <- lm(tmp$fore ~ tmp$obs)
    tmp$alpha <- summary(z)$coefficients[1, 1]
    tmp$beta <-  summary(z)$coefficients[2, 1]
    tmp
  }) %>%
  bind_rows()
  
## Concordance correlation plot
Rmifact <- 9
  Rfact <- 2
  MIRfact <- 3
  IRfact <- 3
  
cccdfplot <- 
cccdf %>%
  mutate(
    obs = ifelse(model == "Rmi", obs * Rmifact, obs),
    fore = ifelse(model == "Rmi", fore * Rmifact, fore),
    obs = ifelse(model == "R", obs * Rfact, obs),
    fore = ifelse(model == "R", fore * Rfact, fore),
    obs = ifelse(model == "MIR", obs * MIRfact, obs),
    fore = ifelse(model == "MIR", fore * MIRfact, fore),
    obs = ifelse(model == "IR", obs * IRfact, obs),
    fore = ifelse(model == "IR", fore * IRfact, fore),
    model = ifelse(model == "Rmi", paste0( "Rmi (X", Rmifact, ")"),
                   ifelse(model == "Rmi", paste0( "Rmi (X", Rmifact, ")"), 
                          ifelse(model == "R", paste0( "R (X", Rfact, ")"),
                                ifelse(model == "IR", paste0( "IR (X", IRfact, ")"),
                                       ifelse(model == "MIR", paste0( "MIR (X", MIRfact, ")"),"Rsi (X1)"
                                              )))))
  ) 

unique(cccdfplot$model)

heightlab <- max(cccdfplot$fore) * 0.92
lengthlab <- 
  max(cccdfplot$obs) * 0.52
heightlabci <- max(cccdfplot$fore) * 0.78
lengthlabci <- 
  max(cccdfplot$obs) * 0.5

cccplot <- 
cccdfplot %>% 
ggplot(., aes(obs, fore, colour = stna)) +
  geom_point(size = 0.2) +
  # facet_wrap(model~day_step, ncol = 10)+
  facet_grid(model ~ day_step) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "gray",
    size = .5,
    linetype = "dashed"
  ) +
  geom_abline(
    aes(intercept = alpha, slope = beta),
    linetype = "solid",
    size = .5,
    color = "black"
  ) +
  labs(
    # subtitle=expression(atop("Measurement of precision and accuracy /n of prediction.",paste("ss", tmp.lab$lab))),
    x = expression("Risk estimation using observed weather data"),
    y = expression("Risk estimation using forecasted weather data"),
    color = "Station"
  ) +
  geom_text(
    y = heightlab,
    x = lengthlab,
    label = cccdf$lab,
    size = 2.7,
    color = "black"
  ) +
  geom_text(
    y = heightlabci,
    x = lengthlabci,
    label = cccdf$labci,
    size = 2.3,
    color = "black"
  ) +
  theme_article() +
  coord_fixed(ratio = 1 / 1) +
  theme(legend.position = "top",
        text = element_text(size = 11))

ggsave(plot = cccplot,
    filename = here::here("out", "fore", "fig", "full_data_ccc.png"),
    width = 10,
    height = 10,
    units = "in",
    dpi = 820
  )

shell.exec(here::here("out", "fore", "fig", "full_data_ccc.png"))


    
    
    
    

    
outlscat <- 
  split(dff, dff$model) %>% 
  lapply(., function(fundf){
    #get the thresholds for specific model
    wdf <- 
      warntdf[ warntdf$model == unique(fundf$model),]
    
    
    fundf[, c("obs_cat")] <- 
      sapply(fundf[, c("obs")], function(x){
        
        ifelse(x < wdf[wdf$tpr == .95, "risk"], 1,
               ifelse(x > wdf[wdf$tpr == .95, "risk"]&x <= wdf[wdf$tpr == .90, "risk"], 2,
                      ifelse(x > wdf[wdf$tpr == .90, "risk"]&x <= wdf[wdf$tpr == .85, "risk"],3,
                             ifelse(x > wdf[wdf$tpr == .85, "risk"]&x <= wdf[wdf$tpr == .80, "risk"],4,5))))
      })
    fundf[, c("fore_cat")] <- 
      sapply(fundf[, c("fore")], function(x){
        
        ifelse(x < wdf[wdf$tpr == .95, "risk"], 1,
               ifelse(x > wdf[wdf$tpr == .95, "risk"]&x <= wdf[wdf$tpr == .90, "risk"], 2,
                      ifelse(x > wdf[wdf$tpr == .90, "risk"]&x <= wdf[wdf$tpr == .85, "risk"],3,
                             ifelse(x > wdf[wdf$tpr == .85, "risk"]&x <= wdf[wdf$tpr == .80, "risk"],4,5))))
      })
    
    fundf[, c("obsadj")] <- 
      sapply(fundf[, c("obs")], function(x){
        ifelse(x < wdf[wdf$tpr == .8, "risk"], x, wdf[wdf$tpr == .8, "risk"])
      })
    fundf[, c("foreadj")] <- 
      sapply(fundf[, c("fore")], function(x){
        ifelse(x < wdf[wdf$tpr == .8, "risk"], x, wdf[wdf$tpr == .8, "risk"])
      })
    
    
    fundf
  }) %>% 
  bind_rows()

summary(outlscat)

outlscat %>% head()






 
# # dff[1,"fore"] <- 15
# outlscat <- 
# split(dff, dff$model) %>% 
#   lapply(., function(fundf){
#     #get the thresholds for specific model
#     wdf <- 
#       warntdf[ warntdf$model == unique(fundf$model),]
#     
#     
#     fundf[, c("obs")] <- 
#     sapply(fundf[, c("obs")], function(x){
#       
#       ifelse(x < wdf[wdf$tpr == .95, "risk"], 1,
#              ifelse(x > wdf[wdf$tpr == .95, "risk"]&x <= wdf[wdf$tpr == .90, "risk"], 2,
#                     ifelse(x > wdf[wdf$tpr == .90, "risk"]&x <= wdf[wdf$tpr == .85, "risk"],3,
#                            ifelse(x > wdf[wdf$tpr == .85, "risk"]&x <= wdf[wdf$tpr == .80, "risk"],4,5))))
#     })
#     fundf[, c("fore")] <- 
#     sapply(fundf[, c("fore")], function(x){
#       
#       ifelse(x < wdf[wdf$tpr == .95, "risk"], 1,
#              ifelse(x > wdf[wdf$tpr == .95, "risk"]&x <= wdf[wdf$tpr == .90, "risk"], 2,
#                     ifelse(x > wdf[wdf$tpr == .90, "risk"]&x <= wdf[wdf$tpr == .85, "risk"],3,
#                            ifelse(x > wdf[wdf$tpr == .85, "risk"]&x <= wdf[wdf$tpr == .80, "risk"],4,5))))
#     })
#     fundf
#   }) %>% 
#   bind_rows()

outlscat[1:20,]
dff[1:20,]


outlscat %>%
  ggplot(aes(obs, fore, colour = stna)) +
  geom_point(size = 0.5) +
  # facet_wrap(model~day_step, ncol = 10)+
  facet_grid(model ~ day_step) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    colour = "Station",
    title = "Risk estimation using forecasted versus observed weather data over lead time",
    subtitle = paste("Outputs for ", models),
    x = "Observed",
    y = "Predicted"
  )

outlscat %>% 
  split(., .$obs) %>% 
  lapply(., function(x)
    x %>% 
  group_by(model,day_step) %>% 
  summarise( obs_gr = unique(obs),
             cor = summary(lm(obs~fore))$r.squared 
             ) %>% 
  ungroup()
  )%>% 
  bind_rows() %>% 
  ggplot()+
  geom_line(aes(day_step, cor, color = model))




  outlscat %>% 
    mutate(month = month(for_date)) %>% 
    mutate(month =factor(month)) %>% 
    # select(c( day_step, model)) %>% 
  # spread(set, risk_si) %>% 
  group_by(month,model,day_step) %>% 
  summarise( #rmse = Metrics::rmse(obs, fore),
             # mse = Metrics::mse(obs, fore),
             rank_cor = cor(obs,fore, method = "spearman")
  ) %>% 
  ungroup() %>% 
  reshape2::melt(
    .,
    id.vars = c("month", "model",  "day_step"),
    # measure.vars = models,
    variable.name = "ind",
    value.name = "skill",
    factorsAsStrings  = FALSE
  ) %>% 
  # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>% 
  ggplot()+
  geom_line(aes(day_step, skill, color = month))+
  theme_article()+
  theme(legend.position = "right") +
  facet_grid(model~ind, scales = "free" )+
  scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
  labs(
    colour = "Model",
    title = "Validation of forecasted risk",
    x = "Lead time (days)"
  )

  
  outlscat %>% 
    mutate(month = month(for_date)) %>% 
    # select(c( day_step, model)) %>% 
    # spread(set, risk_si) %>% 
    group_by(month,model,day_step) %>% 
    summarise( 
               rank_cor = cor(obs,fore, method = "spearman")
    ) %>% 
    ungroup() %>% 
    reshape2::melt(
      .,
      id.vars = c("month", "model",  "day_step"),
      # measure.vars = models,
      variable.name = "ind",
      value.name = "skill",
      factorsAsStrings  = FALSE
    ) %>% 
    # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>% 
    ggplot()+
    geom_smooth(aes(day_step, skill, color = model),
                span = 0.8,
                se = FALSE)+
    theme_article()+
    theme(legend.position = "right") +
    facet_wrap(month~ind, scales = "free" )+
    scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
    labs(
      colour = "Model",
      title = "Validation of forecasted risk",
      x = "Lead time (days)"
    )
  
  
  outlscat %>% 
    # select(c( day_step, model)) %>% 
    # spread(set, risk_si) %>% 
    group_by(obs_cat,model,day_step) %>% 
    summarise( #mse = mean(abs(fore - obs)),
      rank_cor = cor(obs,fore, method = "spearman",
                     na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    reshape2::melt(
      .,
      id.vars = c("obs_cat", "model",  "day_step"),
      # measure.vars = models,
      variable.name = "ind",
      value.name = "skill",
      factorsAsStrings  = FALSE
    ) %>% 
    # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>% 
    ggplot()+
    geom_smooth(aes(day_step, skill, color = model),
                span = 1,
                se = FALSE)+
    theme_article()+
    theme(legend.position = "right") +
    facet_grid(obs_cat~ind, scales = "free" )+
    scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
    labs(
      colour = "Model",
      title = "Validation of forecasted risk",
      x = "Lead time (days)"
    )
  
  
  
  /#test per different rank
  outlscat %>% 
    # mutate(obs_cat  = obs )
    split(., .$obs) %>% 
    lapply(., function(x)
      x %>% 
        group_by(model,day_step) %>% 
        summarise( obs_cat = unique(obs),
                   # cor = cor(obs,fore, method = "spearman")
                   cor = summary(lm(obs~fore))$r.squared 
        ) %>% 
        ungroup()
    )%>% 
    bind_rows() %>% 
     
    # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>% 
    ggplot()+
    geom_line(aes(day_step, cor, color = model))+
    theme_bw()+
    theme(legend.position = "right") +
    facet_wrap(~obs_cat, ncol = 5 )+
    scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
    labs(
      colour = "Model",
      title = "Validation of forecasted risk",
      x = "Lead time (days)"
    )

  
  #################################################################
#Values above .8 ae cut off


 # dff[1,"fore"] <- 1
outlrange <- 
  split(dff, dff$model) %>% 
  lapply(., function(fundf){
    #get the thresholds for specific model
    wdf <- 
      warntdf[ warntdf$model == unique(fundf$model),]
    fundf[, c("obs")] <- 
      sapply(fundf[, c("obs")], function(x){
        ifelse(x < wdf[wdf$tpr == .8, "risk"], x, wdf[wdf$tpr == .8, "risk"])
      })
    fundf[, c("fore")] <- 
      sapply(fundf[, c("fore")], function(x){
        ifelse(x < wdf[wdf$tpr == .8, "risk"], x, wdf[wdf$tpr == .8, "risk"])
      })

    fundf
  }) %>% 
  bind_rows()

  outlrange %>% 
  mutate(obs = ifelse(model == "Rmi", obs*60, obs),
         fore = ifelse(model == "Rmi", fore*60, fore),
         obs = ifelse(model == "R", obs*80, obs),
         fore = ifelse(model == "R", fore*80, fore),
         # obs = ifelse(model == "MIR", obs*2.5, obs),
         # fore = ifelse(model == "MIR", fore*2.5, fore),
         obs = ifelse(model == "IR", obs*2.5, obs),
         fore = ifelse(model == "IR", fore*2.5, fore)
         ) %>% 
  ggplot(aes(obs, fore, colour = stna)) +
    geom_point(size = 0.2) +
    # facet_wrap(model~day_step, ncol = 10)+
    facet_grid(model ~ day_step) +
    theme_bw() +
    theme(legend.position = "top") +
    labs(
      colour = "Station",
      title = "Risk estimation using forecasted versus observed weather data over lead time",
      subtitle = paste("Outputs for ", models),
      x = "Observed",
      y = "Predicted"
    )
  
  
  outlrange %>% 
  group_by(model,day_step) %>% 
  summarise( rmse = sqrt(mean(fore - obs)^2),
             mse = mean(abs(fore - obs)),
             cor = summary(lm(obs~fore))$r.squared 
  ) %>% 
  ungroup() %>% 
  reshape2::melt(
    .,
    id.vars = c("model",  "day_step"),
    # measure.vars = models,
    variable.name = "ind",
    value.name = "skill",
    factorsAsStrings  = FALSE
  ) %>% 
  # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>% 
  ggplot()+
  geom_line(aes(day_step, skill, color = model))+
  theme_article()+
  theme(legend.position = "right") +
  facet_wrap(~ind, ncol = 1, scales = "free" )+
  scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
  labs(
    colour = "Model",
    title = "Validation of forecasted risk",
    x = "Lead time (days)"
  )


  #test per different rank
  outlrange %>% 
    # mutate(obs_cat  = obs )
    split(., .$obs) %>% 
    lapply(., function(x)
      x %>% 
        group_by(model,day_step) %>% 
        summarise( obs_cat = unique(obs),
                   # cor = cor(obs,fore, method = "spearman")
                   cor = summary(lm(obs~fore))$r.squared 
        ) %>% 
        ungroup()
    )%>% 
    bind_rows() %>% 
    
    # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>% 
    ggplot()+
    geom_line(aes(day_step, cor, color = model))+
    theme_bw()+
    theme(legend.position = "right") +
    facet_wrap(~obs_cat, ncol = 5 )+
    scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
    labs(
      colour = "Model",
      title = "Validation of forecasted risk",
      x = "Lead time (days)"
    )
  
  
  

  
  
  # dff[1,"fore"] <- 15
  
  #test per different rank
  outlscat %>% 
    # mutate(obs_cat  = obs )
    split(., .$obs_cat) %>% 
    lapply(., function(x)
      x %>% 
        group_by(model,day_step) %>% 
        summarise( obs_cat = unique(obs_cat),
                   # cor = cor(obs,fore, method = "spearman")
                   cor = summary(lm(obs~fore))$r.squared 
        ) %>% 
        ungroup()
    )%>% 
    bind_rows() %>% 
    
    # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>% 
    ggplot()+
    geom_line(aes(day_step, cor, color = model))+
    theme_bw()+
    theme(legend.position = "right") +
    facet_wrap(~obs_cat, ncol = 5 )+
    scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
    labs(
      colour = "Model",
      title = "Validation of forecasted risk",
      x = "Lead time (days)"
    )
  
  outlscat %>% 
  ggplot(aes(obsadj, foreadj, colour = stna)) +
    geom_point(size = 0.5) +
    # facet_wrap(model~day_step, ncol = 10)+
    facet_grid(model ~ day_step) +
    theme_bw() +
    theme(legend.position = "top") +
    labs(
      colour = "Station",
      title = "Risk estimation using forecasted versus observed weather data over lead time",
      subtitle = paste("Outputs for ", models),
      x = "Observed",
      y = "Predicted"
    )
  
  


  
  
  
  ####################################################################################
  #CCC
  ####################################################################################
  #Concordance correlation coeeficient
  # Computes Lin's (1989, 2000) concordance correlation coefficient for agreement on a continuous measure obtained by two methods.
  # The concordance correlation coefficient combines measures of both precision and accuracy to determine how far
  # the observed data deviate from the line of perfect concordance (that is, the line at 45 degrees on a square scatter plot).
  # Lin's coefficient increases in value as a function of the nearness of the data's reduced major axis to the line of perfect concordance 
  # (the accuracy of the data) and of the tightness of the data about its reduced major axis (the precision of the data).
  
  #   library("PerformanceAnalytics")
  #   
  # cols <- c("temp","temp_ob","rhum","rhum_ob"
  #           # ,"sol_rad","sol_rad_ob","wspd", "wdsp_ob", "rain", "rain_ob"
  #           )
  #   
  # chart.Correlation(full_data [full_data$hour_step <75, cols]
  #                  , histogram = F, pch = 19)
  # chart.Correlation(full_data [full_data$hour_step >75, cols]
  #                   , histogram = F, pch = 19)
  # 
  cols <- c("sol_rad","sol_rad_ob","wspd", "wdsp_ob", "rain", "rain_ob")
  chart.Correlation(full_data [full_data$hour_step <75, cols]
                    , histogram = F, pch = 19)
  chart.Correlation(full_data [full_data$hour_step >74, cols]
                    , histogram = F, pch = 19)
  
  
  
  
  ggplot(full_data,aes(x = temp, temp_ob, colour = factor(day_step) ))+
    geom_point(size = 0.01)+
    theme(legend.position="none")
  
  
  tmp <- full_data [full_data$hour_step <73 
                    ,c("hour_step", "temp","temp_ob" )]
  
  tmp.ccc <- epiR::epi.ccc(tmp$temp_ob, tmp$temp, ci = "z-transform", conf.level = 0.95, 
                           rep.measure = FALSE)
  
  tmp.lab <- data.frame(lab = paste("CCC: ", 
                                    round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ", 
                                    round(tmp.ccc$rho.c[,2], digits = 2), " - ",
                                    round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = ""))
  
  z <- lm(tmp$temp_ob ~ tmp$temp)
  alpha <- summary(z)$coefficients[1,1]
  beta <-  summary(z)$coefficients[2,1]
  tmp.lm <- data.frame(alpha, beta)
  
  ## Concordance correlation plot:
  
  g <-  ggplot(tmp) + 
    geom_point(aes(x = tmp$temp, y = tmp$temp_ob,fill = factor(hour_step), color = factor(hour_step)), size = 0.1) +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(data = tmp.lm, aes(intercept = alpha, slope = beta), 
                linetype = "dashed") +
    labs(title="Concordance corr. coefficient", 
         subtitle=paste0("Measurement of precision and accuracy/nof prediction.",tmp.lab$lab ),
         caption="Reference: Lin (1989, 2000)",
         x="Observed temperatures",
         y="Predicted temperatures")+
    geom_text(data = tmp.lab, x = 14, y = 2, label = tmp.lab$lab) + 
    coord_fixed(ratio = 1 / 1)+
    theme(legend.position="none")
  
  ggExtra:: ggMarginal(g, type = "histogram", fill="transparent")
  
  

