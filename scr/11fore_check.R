
source(here::here("scr","lib",  "pkg.R"))
source(here::here("scr", "lib", "funs.R"))
load( file =here::here("out", "fore", "fore_model_out.Rdata"))

#Prepare the data inlong format for the analysis 
out_ls[[1]]
out_ls[[1]]$id

out_df <-
  out_ls %>%
  bind_rows() %>%
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

dff$model <- 
  gsub("risk_", "R",  dff$model) %>% 
  gsub("risk", "R",  .) %>% 
  gsub("ir_R", "MIR",  .) %>% 
  gsub("defMIR", "IR",  .) 

dff$model <- 
  factor(dff$model, levels = c(  "R", "Rsi","Rmi", "IR","MIR" ))
 



save( dff, file =here::here("out", "fore", "fore_model_out_data_frame.Rdata"))




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

rm(default_eval_lss, warn_t_df, out_ls)



############################################
#Initial evaluation of the full weather forecast
############################################


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

# tmp <-   split(dff, list(dff$model, dff$day_step))[[1]]



cccdf <-
  dff %>%
  split(., list(dff$model, dff$day_step)) %>%
  lapply(., function(tmp) {
    tmp.ccc <- epiR::epi.ccc(
      tmp$obs,
      tmp$fore,
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
    dpi = 1020
  )

shell.exec(here::here("out", "fore", "fig", "full_data_ccc.png"))


    
    

############################################
#Ranked analysis
############################################

    
# split(dff, dff$model) [[1]] -> fundf #Test
    
outlscat <- 
  split(dff, dff$model) %>% 
  lapply(., function(fundf){
    #get the thresholds for specific model
    wdf <- 
      warntdf[ warntdf$model == unique(fundf$model),]

    fundf[, c("obs_cat")] <- 
      sapply(fundf[, c("obs")], function(x){
        # (x <-  fundf[, c("obs")][1]) #test
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


#Set  color scheme
my_pair_lab <- c(unikn::seecol(pal_unikn_pair)[c(1,7,9)], "darkgray", "black")
names(my_pair_lab) <- rev(unique(outlscat$model)) %>% unlist()

prank <- 
outlscat %>% 
  group_by(model,day_step) %>% 
  summarise(
    rank_cor = cor(obs_cat,fore_cat, method = "spearman")
  ) %>% 
  ungroup()  %>% 
  mutate(model = factor(model, levels = c("Rsi", "Rmi", "R","MIR", "IR") )) %>%
  ggplot()+
  geom_line(aes(day_step, rank_cor, color = model))+
  theme_bw()+
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1),minor_breaks = seq(1, 10, 1))+
  scale_y_continuous(limits = c(0,1.03),breaks = seq(0,1,.2),labels = seq(0,1,.2),minor_breaks = seq(0,1,.2))+
  labs(
    colour = "Model:",
    x = "Lead time (days)",
    y  = "Spearman Correlation Coefficient"
  )+
  scale_color_manual(values = my_pair_lab)+
  theme(
    text = element_text(size=12),
    legend.position = c(.81, .71),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.key.width = unit(1, "cm")
  )+
  ggsave(filename = here::here("out", "fore", "fig", "rank_cor.png"),
         width = 7,
         height = 4,
         units = "in",
         dpi = 1020
  )

# shell.exec(here::here("out", "fore", "fig", "rank_cor.png"))


pcccfore <- readRDS( file =  here::here("out", "fore", "fig", "wth_vars", "CCCdaily_wth_vars.png"))

pcccfore <- 
  pcccfore+
   theme(axis.title.x = element_blank())
plist <- list(pcccfore, prank)

ggpubr::ggarrange(plotlist = plist, 
                  # widths = c(2.5,2.5),
                  # heights = c(.5,.5),
                  labels = c("a)","b)"),
                  nrow = 2)+
  ggsave(filename=  here::here("out", "fore", "fig", "ccc&rank_wth.png"),
         width = 8, height =8, dpi = 720)

shell.exec(here::here("out", "fore", "fig", "ccc&rank_wth.png"))





outlscat[1:20, ]
dff[1:20, ]






outlscat %>%
  mutate(month = month(for_date)) %>%
  # select(c( day_step, model)) %>%
  # spread(set, risk_si) %>%
  group_by(month, model, day_step) %>%
  summarise(
    rank_cor = cor(obs_cat,fore_cat, method = "spearman")
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
  ggplot() +
  geom_smooth(aes(day_step, skill, color = factor(month)),
              span = 0.8,
              se = FALSE,
              size = .7
              ) +
  theme_article()+
  theme(
    text = element_text(size=12),
    legend.position = c(.82, .22),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.key.width = unit(1, "cm")
  )+
  # theme(legend.position = "right") +
  facet_wrap( ~factor(model) , scales = "free") +
  scale_x_continuous(breaks = seq(1, 10, 1), labels = seq(1, 10, 1)) +
  scale_y_continuous(limits = c(0,1))+
  labs(colour = "Month",
       x = "Lead time (days)")+
  ggsave(filename = here::here("out", "fore", "fig", "month_rank_cor.png"),
         width = 7,
         height = 4,
         units = "in",
         dpi = 1020
  )

shell.exec(here::here("out", "fore", "fig", "month_rank_cor.png"))





outlscat %>%
  mutate(month = month(for_date)) %>%
  # select(c( day_step, model)) %>%
  # spread(set, risk_si) %>%
  group_by(stna, model, day_step) %>%
  summarise(
    rank_cor = cor(obs_cat,fore_cat, method = "spearman")
  ) %>%
  ungroup() %>%
  reshape2::melt(
    .,
    id.vars = c("stna", "model",  "day_step"),
    # measure.vars = models,
    variable.name = "ind",
    value.name = "skill",
    factorsAsStrings  = FALSE
  ) %>%
  # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>%
  ggplot() +
  geom_smooth(aes(day_step, skill, color = factor(model)),
              span = 0.8,
              se = FALSE,
              size = .7
  ) +
  theme_article()+
  theme(
    text = element_text(size=12),
    legend.position = c(.82, .22),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.key.width = unit(1, "cm")
  )+
  # theme(legend.position = "right") +
  facet_wrap( ~factor(stna) , scales = "free") +
  scale_x_continuous(breaks = seq(1, 10, 1), labels = seq(1, 10, 1)) +
  scale_y_continuous(limits = c(0,1))+
  labs(colour = "Model:",
       x = "Lead time (days)")+
  ggsave(filename = here::here("out", "fore", "fig", "stna_model_rank_cor.png"),
         width = 7,
         height = 4,
         units = "in",
         dpi = 1020
  )

shell.exec(here::here("out", "fore", "fig", "stna_model_rank_cor.png"))





outlscat %>%
  filter(model!= "IR") %>% 
  mutate(month = month(for_date)) %>%
  # select(c( day_step, model)) %>%
  # spread(set, risk_si) %>%
  group_by(stna, day_step) %>%
  summarise(
    rank_cor = cor( obs_cat, fore_cat, method = "spearman")
  ) %>%
  ungroup() %>%
  reshape2::melt(
    .,
    id.vars = c("stna",  "day_step"),
    # measure.vars = models,
    variable.name = "ind",
    value.name = "skill",
    factorsAsStrings  = FALSE
  ) %>%
  # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>%
  ggplot() +
  geom_smooth(aes(day_step, skill, color = factor(stna)),
              span = 0.8,
              se = FALSE,
              size = .7
  ) +
  theme_article()+
  theme(
    text = element_text(size=12),
    legend.position = c(.82, .72),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.key.width = unit(1, "cm")
  )+
  # theme(legend.position = "right") +
  # facet_wrap( ~factor(stna) , scales = "free") +
  scale_x_continuous(breaks = seq(1, 10, 1), labels = seq(1, 10, 1)) +
  scale_y_continuous(limits = c(0,1))+
  # scale_color_manual(values = my_pair_lab)+
  labs(colour = "Stna:",
       title = "Agreement of forecasted and observed risk across models per station (no IR)",
       x = "Lead time (days)")+
  ggsave(filename = here::here("out", "fore", "fig", "stna_rank_cor.png"),
         width = 8,
         height = 4,
         units = "in",
         dpi = 1020
  )

shell.exec(here::here("out", "fore", "fig", "stna_rank_cor.png"))




outlscat %>%
  # select(c( day_step, model)) %>%
  # spread(set, risk_si) %>%
  group_by(obs_cat, model, day_step) %>%
  summarise(#mse = mean(abs(fore - obs)),
    rank_cor = cor(obs, fore, method = "spearman")) %>%
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
  ggplot() +
  geom_smooth(aes(day_step, skill, color = model),
              span = 1,
              se = FALSE) +
  theme_article() +
  theme(legend.position = "right") +
  facet_grid(obs_cat ~ ind, scales = "free") +
  scale_x_continuous(breaks = seq(1, 10, 1), labels = seq(1, 10, 1)) +
  labs(colour = "Model",
       title = "Validation of forecasted risk",
       x = "Lead time (days)")



#test per different rank
# outlscat %>%
#   # mutate(obs_cat  = obs )
#   split(., .$obs) %>%
#   lapply(., function(x)
#     x %>%
#       group_by(model, day_step) %>%
#       summarise(obs_cat = unique(obs),
#                 # cor = cor(obs,fore, method = "spearman")
#                 cor = summary(lm(obs ~ fore))$r.squared) %>%
#       ungroup()) %>%
#   bind_rows() %>%
#   
#   # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>%
#   ggplot() +
#   geom_line(aes(day_step, cor, color = model)) +
#   theme_bw() +
#   theme(legend.position = "right") +
#   facet_wrap( ~ obs_cat, ncol = 5) +
#   scale_x_continuous(breaks = seq(1, 10, 1), labels = seq(1, 10, 1)) +
#   labs(colour = "Model",
#        title = "Validation of forecasted risk",
#        x = "Lead time (days)")
# 



#################################################################
#Values above .8 ae cut off


# dff[1,"fore"] <- 1
outlrange <-
  split(dff, dff$model) %>%
  lapply(., function(fundf) {
    #get the thresholds for specific model
    wdf <-
      warntdf[warntdf$model == unique(fundf$model),]
    fundf[, c("obs")] <-
      sapply(fundf[, c("obs")], function(x) {
        ifelse(x < wdf[wdf$tpr == .8, "risk"], x, wdf[wdf$tpr == .8, "risk"])
      })

    fundf
  }) %>%
  bind_rows()

outlrange %>%
  # mutate(
  #   obs = ifelse(model == "Rmi", obs * 60, obs),
  #   fore = ifelse(model == "Rmi", fore * 60, fore),
  #   obs = ifelse(model == "R", obs * 80, obs),
  #   fore = ifelse(model == "R", fore * 80, fore),
  #   # obs = ifelse(model == "MIR", obs*2.5, obs),
  #   # fore = ifelse(model == "MIR", fore*2.5, fore),
  #   obs = ifelse(model == "IR", obs * 2.5, obs),
  #   fore = ifelse(model == "IR", fore * 2.5, fore)
  # ) %>%
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
  group_by(model, day_step) %>%
  summarise(
    rmse = sqrt(mean(fore - obs) ^ 2),
    mse = mean(abs(fore - obs)),
    cor = summary(lm(obs ~ fore))$r.squared
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
  ggplot() +
  geom_line(aes(day_step, skill, color = model)) +
  theme_article() +
  theme(legend.position = "right") +
  facet_wrap(~ ind, ncol = 1, scales = "free") +
  scale_x_continuous(breaks = seq(1, 10, 1), labels = seq(1, 10, 1)) +
  labs(colour = "Model",
       title = "Validation of forecasted risk",
       x = "Lead time (days)")


#test per different rank
outlrange %>%
  # mutate(obs_cat  = obs )
  split(., .$obs) %>%
  lapply(., function(x)
    x %>%
      group_by(model, day_step) %>%
      summarise(obs_cat = unique(obs),
                # cor = cor(obs,fore, method = "spearman")
                cor = summary(lm(obs ~ fore))$r.squared) %>%
      ungroup()) %>%
  bind_rows() %>%
  
  # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>%
  ggplot() +
  geom_line(aes(day_step, cor, color = model)) +
  theme_bw() +
  theme(legend.position = "right") +
  facet_wrap(~ obs_cat, ncol = 5) +
  scale_x_continuous(breaks = seq(1, 10, 1), labels = seq(1, 10, 1)) +
  labs(colour = "Model",
       title = "Validation of forecasted risk",
       x = "Lead time (days)")






# dff[1,"fore"] <- 15

#test per different rank
outlscat %>%
  # mutate(obs_cat  = obs )
  split(., .$obs_cat) %>%
  lapply(., function(x)
    x %>%
      group_by(model, day_step) %>%
      summarise(obs_cat = unique(obs_cat),
                # cor = cor(obs,fore, method = "spearman")
                cor = summary(lm(obs ~ fore))$r.squared) %>%
      ungroup()) %>%
  bind_rows() %>%
  
  # mutate(day_step = factor(day_step, levels = seq(1,10,1))) %>%
  ggplot() +
  geom_line(aes(day_step, cor, color = model)) +
  theme_bw() +
  theme(legend.position = "right") +
  facet_wrap(~ obs_cat, ncol = 5) +
  scale_x_continuous(breaks = seq(1, 10, 1), labels = seq(1, 10, 1)) +
  labs(colour = "Model",
       title = "Validation of forecasted risk",
       x = "Lead time (days)")

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



############################################
#Uncertainty introduced by forecasted data
############################################

#Load the  evaluation sata for the calibrated model
load(file = here::here("out", "calib", "eval_longer.Rdata"))
head(eval_longer)
ev <-
  eval_longer %>%
  select(model, spec, sens)
# Load the ccc estimations
load(file = here::here("out", "fore", "fore_model_out_data_frame.Rdata"))

foreeval <-
  dff %>%
  split(., list(dff$model, dff$day_step)) %>%
  lapply(., function(tmp) {
    tmp.ccc <- epiR::epi.ccc(
      tmp$obs,
      tmp$fore,
      ci = "z-transform",
      conf.level = 0.95,
      rep.measure = FALSE
    )$rho.c[, 1]
    
    x <- data.frame(
      ccc = tmp.ccc,
      model = unique(tmp$model),
      day_step = unique(tmp$day_step)
    )
    x
  }) %>%
  bind_rows() 
# %>%
#   spread(day_step, ccc)

ev %>%
  split(., ev$model) %>%
  lapply(., function(xy) {
    # split(ev, ev$model)[[3]] -> xy #test
    xy$day_step <- 0
    
    #Calculate the uncertainty with daily step
    funls <- list()
    for (i in 1:10) {
      funxy <- xy
      funxy$day_step <- i
      funxy$sens <-
        funxy$sens * foreeval[foreeval$model == unique(xy$model) &
                                foreeval$day_step == i, "ccc"]
      funls[[i]] <- funxy
    }
    bind_rows(xy,funls)
    
    
  }) %>% 
  bind_rows() %>% 
  ggplot()+
  geom_line(aes(spec, sens, color = factor(day_step)))+
  scale_color_grey(start=0.4, end=1) +
  theme_article()+
  facet_wrap(~model, ncol=1)+
  ggsave(filename = here::here("out", "fore", "fig", "rescaled_sens_per_model.png"),
         width = 5, 
         height = 8,
         units = "in",
         dpi = 1020)

shell.exec(here::here("out", "fore", "fig", "rescaled_sens_per_model.png"))







x <- xy$spec
y <- foreeval[foreeval$model == unique(xy$model) , "ccc"  ]
z <- xy$sens

library("plot3D")

scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"))

library(plotly)

plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers")


# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
# scatter plot with regression plane
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "wt", ylab = "disp", zlab = "mpg",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "mtcars")






















