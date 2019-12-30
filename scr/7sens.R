###################################################################################
# Sensitivity 
###################################################################################
###################################################################################
# Source data
###################################################################################
source(here::here("scr","lib",  "pkg.R"))
library("readxl")
source(here::here("scr", "lib", "funs.R"))


load(file = here::here("dat", "outbreak&weather.Rdata"))
load(file= here::here("dat", "treatment_no_estim.Rdata"))

###################################################################################
# Funs
###################################################################################
source(here::here("scr", "model", "run.R"))
source(here::here("scr", "lib", "IrishRulesModelSensitive.R"))


source(here::here("scr", "lib", "DiagFuns.R"))
load( file = here::here("out", "default", "warning_thresholds.Rdata"))


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


par_set <- 
  readxl::read_xlsx( 
    here::here("scr", "model", "par_eval", "par_eval.xlsx"))[,"met_set"] %>% 
  unlist() %>% as.character()

done <- 
  list.files(here::here("out", "eval", "out")) %>% 
  str_replace(".Rdata","")




###################################################################################
# Run model
###################################################################################
starttime <- Sys.time()


if(length(done)== 0){
  par_set_run <- par_set
}else{
  par_set_run <- par_set[par_set %in%done]
}

for (i in par_set_run){
  #  i <-  par_set[55]
  # x <- lss[[1]]
  # run_type <- "model"
  
  cl <- makeCluster(detectCores())
  clusterExport(cl, c("BlightR",
                      "IrishRulesModel", 
                      "RunModel",
                      "TPPFun", "ControlFreqFun", 
                      "ExtractCol",
                      "i"))
  
  clusterEvalQ(cl, library("tidyverse", quietly = TRUE, verbose = FALSE))
  
  
  out_ls <-
    # testing 
    # pblapply(lss[1:2], function(x)
    pblapply(lss, function(x)
      RunModel(x,
               model_parameters = i,
               run_type = "model") , cl = cl)
  # testing 
  # out_ls[[1]] %>% view()
  
  out_trt <- pblapply(wth_ls, function(x)
    RunModel(x,
             model_parameters = i,
             run_type = "wth"),
    cl = cl)
  
  res_lss <- list(out_ls, out_trt)
  save(res_lss, file = here::here("out","eval", "out", paste0(i,".Rdata")))
  
  
  
  gc()
  
  print(paste0(i,": ",  time_length(Sys.time() - starttime, unit = "minutes")))
  Sys.sleep(10)
  
  
}


print(paste0(i,": ",  round(time_length(Sys.time() - starttime, unit = "hours"), 3)))

# source(here::here("scr", "lib", "GitCommit.R" ))






###############################################################
#Diagnostic performance
###############################################################

source(here::here("scr","lib",  "pkg.R"))

source(here::here("scr", "lib", "funs.R"))
source(here::here("scr", "lib", "DiagFuns.R"))





par_set <- 
  readxl::read_xlsx( 
    here::here("scr", "model", "par_eval", "par_eval.xlsx"))[,"met_set"] %>% 
  unlist() %>% as.character()

done <- 
  list.files(here::here("out", "eval", "out")) %>% 
  str_replace(".Rdata","")




if(length(done)== 0){
 stop("No evaluation outputs...check!")
}else{
  par_set_run <- par_set[par_set %in%done]
}


starttime <- Sys.time()

# ls <- list()
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library("tidyverse", quietly = TRUE, verbose = FALSE))


#Initiate the list containing all of the diagnostic data frames
lss <- list()

for (i in par_set_run){
  #  i <-  par_set_run[2]
  # i <-  "ShapeSpor4l" 
  # x <- lss[[1]]
  # run_type <- "model"
  
  
  
  
  res_lss <- 
    get(load( file = here::here("out","eval", "out", paste0(i,".Rdata"))))
  
  
  trt_df <- do.call("rbind", res_lss[2][[1]])
  
  Cutoffs <- function(x){
    quantile(drop_na(x[x>0,])%>% unlist(), probs = seq(0, 1, 0.04)) 
  }
  
  warn_t_df <- 
    data.frame(warn_thresh = 1:26,
               risk_si =  Cutoffs(trt_df[ , "risk_si"]),
               risk_mi = Cutoffs(trt_df[ , "risk_mi"]),
               risk = Cutoffs(trt_df[ , "risk"]),
               ir_risk = seq.int(1,26,1),
               defir_risk = seq.int(1,26,1)
    )
  #Set the range ow cutoff points
  warning_thresholds <- warn_t_df$warn_thresh
  
  duration_of_season <- nrow(unique(res_lss[2][[1]][[1]]["doy"]))
  min_prot_dur <-  7
  max_trt <- 
    round(duration_of_season  / min_prot_dur,3)
  
  
  min_prot_dur_e  = c(7:14)
  max_trt_e <-
    round(duration_of_season  / min_prot_dur_e,3)
  
  
  red_df <- 
    data.frame(min_prot_dur_e  = min_prot_dur_e,
               max_trt_e = max_trt_e,
               reduction = round(max_trt_e/ max_trt,3))
  
  
  
  rm(Cutoffs,trt_df,duration_of_season)
  
  ###################################################
  #Evaluation
  ###################################################
  clusterExport(cl, c("res_lss",
                      "warn_t_df",
                      "warning_thresholds",
                      "ControlFreqFun", 
                      "TPPFun"))
  
  tpp_ev_ls <-
    pbapply::pblapply(warning_thresholds, function(x) {
      TPPFun(x, res_lss[1][[1]])
    },
    cl = cl) %>% bind_rows()
  
  
  trt_ev_ls <-
    pbapply::pblapply(res_lss[2][[1]], function(dta) {
      lapply(warning_thresholds, function(cutoff) {
        ControlFreqFun(cutoff,
                       warn_t_df,
                       data = dta,
                       no_cal = max_trt,
                       min_prot_dur = 7)
      }) %>% bind_rows()
    },
    cl = cl) 
  Sys.sleep(10)
  
  #################################################################
  #Calculate the partial AUC
  #################################################################
  #  
  eval_long <- EvalTable(tpp_ev_ls, trt_ev_ls)
  
  eval_long$model <- 
    gsub("risk_", "R",  eval_long$model) %>% 
    gsub("risk", "R",  .) %>% 
    gsub("ir_R", "MIR",  .) %>% 
    gsub("defMIR", "IR",  .) 
  
  eval_longer <- EvalCutoff(eval_long, 
                            cutoffs =  c(0.8, 0.85, 0.9))
  

  
  

  # rm(tpp_ev_ls, trt_ev_ls, eval_lss)
  
  eval_long$model <- 
    factor(eval_long$model, levels = c(  "R", "Rsi","Rmi"))
  

    
  fin <- DiagPerformance(eval_longer = eval_longer,
                         pAUCcutoff = .8,
                         no_of_outbreaks = 362)
  
  
  

    
  fin$eval <- i
  
  lss[[i]] <- fin
  save(fin, file = here::here("out","eval", "diag_fin", paste0(i,".Rdata")))

  #################################################################
  #Diag plots
  #################################################################
  
  #Produce labs that will contain the diagnostic information
  finlab <- 
    fin %>% 
    arrange(desc(pAUC)) %>% 
    mutate(pAUC = round(pAUC, 3)) %>% 
    mutate(AUC = round(AUC, 3)) %>% 
    select(-c(out_miss, AUC)) %>% 
    select(model, pAUC, maxTPR) %>% 
    unite( "lab" ,2:3, sep= "; ") %>% 
    mutate(lab = paste0("(", lab, "%)"))
  
  eval_longerdf <- 
    left_join(eval_longer, finlab, by = "model") %>% 
    mutate(modellab = paste(model, lab)) %>% 
    mutate(modellab = factor(modellab))
  
  #Set  color scheme
  my_pair_lab <- c(unikn::seecol(pal_unikn_pair)[c(1,7,9,15)], "#696969")
  names(my_pair_lab) <- unique(eval_longer$modellab)
  
  
  eval_longerdf %>% 
    ggplot(aes(spec, sens, color = fct_rev(modellab))) +
    geom_rect(mapping = aes( xmin =0, xmax=1, ymin = .8, ymax=1), fill = "lightgray", color = "white",alpha = .1) +
    geom_hline(
      yintercept = seq(0 , 1, 0.1),
      size = 0.3,
      color = "gray",
      linetype = "dotted"
    )+
    geom_vline(
      xintercept = red_df$reduction,
      size = 0.35,
      color = "gray",
      linetype = "solid",
      alpha= .3
    )+
    geom_hline(
      yintercept = c(.95, .9, .85, .8),
      linetype = "dashed",
      size = .3,
      alpha = .3
    ) +
    geom_point(size = .5,show.legend=FALSE) +
    geom_line( size = .6) +
    scale_x_continuous(
      limits = c(0, 1),
      expand = c(0, 0),
      breaks = red_df$reduction,
      labels = paste0(red_df$reduction, "\n", red_df$min_prot_dur_e),
      name = "Treatment reduction rate (TRR) and the corresponding average fixed treatment schedule",
      position = "top"
    ) +
    scale_color_manual(values = my_pair_lab)+
    labs(color = "Model (pAUC; maxTPR):")+
    theme_article() +
    theme(
      text = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size=12),
      legend.position = c(.81, .31),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.key.width = unit(1, "cm")
    ) +
    guides(color = guide_legend(
      nrow = 5,
      byrow = TRUE,
      title.position = "top",
      # colour = guide_legend(override.aes = list(alpha = 1, size = 1))
    ))+
    scale_y_continuous(breaks = seq(0, 1, 0.05),
                       limits = c(0, 1),
                       name = "Proportion of predicted outbreaks (TPR)")+
    ggsave(filename = here::here("out", "eval", "graphs", paste0(i,".png")), 
           width = 8.5, height = 6.5, units = "in", dpi=820)
  

  
    
  
  gc()
  Sys.sleep(5)
  rm( p1, p2, eval_long )
  print(paste0(i,": ",  time_length(Sys.time() - starttime, unit = "minutes")))
  
  
}



print(paste0(i,": ",  round(time_length(Sys.time() - starttime, unit = "hours"), 3)))

# source(here::here("scr", "lib", "GitCommit.R" ))


bind_rows(lss) %>% 
  save(here::here("out", "eval", "final_diag.Rdata"))
  
  



###############################################################
###############################################################
#Compare the diag. pefomance
###############################################################

source(here::here("scr","lib",  "pkg.R"))
library("readxl")
source(here::here("scr", "lib", "funs.R"))


evaldf <-
  get(load(file = here::here("out", "eval", "final_diag.Rdata")))

evaldf <- evaldf[evaldf$eval != "ShapeSpor4l",]

evaldf[evaldf$eval == "ShapeSpor4l",]
evaldf[evaldf$lev == 4,]
ls[98]

evaldf <- 
  evaldf %>% 
  bind_rows() %>% 
  dplyr:: filter(eval !="default") %>% 
  group_by(eval) %>%
  mutate(
    lev = ifelse(str_detect(eval,"-"), 
                 substring(eval, nchar(eval)-2,nchar(eval)-1), 
                 substring(eval, nchar(eval)-1,nchar(eval))),
    lev = as.numeric(gsub("l", "", lev)),
    maxTPR = as.numeric(gsub(" %", "", maxTPR)),
    stage = ifelse(str_detect(eval,"Spor"), "Sporulation", 
                   ifelse(str_detect(eval,"Inf"), "Infection",
                          ifelse(str_detect(eval,"B"), "Mortality","time"))),
    stage = factor(stage, levels = c("Infection","Sporulation","Mortality", "time")),
    par = ifelse(str_detect(eval,"-"), substring(eval, 1,nchar(eval)-3), substring(eval, 1, nchar(eval)-2))
  )

unique(evaldf$par)  
unique(evaldf$stage)  

#Set  color scheme
my_pair <- c(unikn::seecol(pal_unikn_pair)[c(1,7,9)])

names(my_pair) <- levels(evaldf$model)




ggplot(evaldf) +
  labs(color = "Model:",
       xlab = "Level") +
  theme_article() +
  geom_vline(
    xintercept = seq(-6 , 3, 1),
    size = 0.24,
    color = "gray"
  ) +
  geom_vline(
    xintercept = 0,
    size = 0.6,
    color = "gray"
  ) +
  geom_smooth(
    aes(
      x = lev,
      pAUC ,
      color = model,
      group = model
    ),
    se = FALSE,
    span = .5,
    method = 'loess',
    size = .8
  ) +
  labs(color = "Model:",
       xlab = "Level")+
  # geom_line(aes(x = lev, pAUC , color = model, group = model),size = .8)+
  facet_wrap(stage ~ par) +
  scale_color_manual(values = my_pair) +
  theme(legend.position = "top") +
  ggsave(
    filename = here::here("out", "eval", "diag_graph", "model_eval.png"),
    width = 8.5,
    height = 9,
    units = "in"
  )
shell.exec(here::here("out", "eval", "diag_graph", "model_eval.png"))




ggplot(evaldf)+
  labs(color = "Model:",
       xlab = "Level") +
  theme_article() +
  geom_vline(
    xintercept = seq(-6 , 3, 1),
    size = 0.24,
    color = "gray"
  ) +
  geom_vline(
    xintercept = 0,
    size = 0.6,
    color = "gray"
  ) +
  geom_smooth(
    aes(
      x = lev,
      maxTPR ,
      color = model,
      group = model
    ),
    se = FALSE,
    span = .5,
    method = 'loess',
    size = .8
  ) +
  facet_wrap(stage~par)+
  scale_color_manual(values = my_pair) +
  theme_article()+
  labs(color = "Model:",
       xlab = "Level")+
  theme(legend.position = "top")+
  ggsave(filename = here::here("out", "eval", "diag_graph", "maxTPR.png"),
         width = 9, height = 8, units = "in")

shell.exec(here::here("out", "eval", "diag_graph", "maxTPR.png"))




















