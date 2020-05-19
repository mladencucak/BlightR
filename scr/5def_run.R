
###################################################################################
# Source data
###################################################################################
source(here::here("scr","lib",  "pkg.R"))
source(here::here("scr", "lib", "funs.R"))


load(file = here::here("dat", "outbreak&weather.Rdata"))
load(file= here::here("dat", "treatment_no_estim.Rdata"))


###################################################################################
# Model run function
###################################################################################
source(here::here("scr", "model", "run.R")) #Load the BlightR mdoel
source(here::here("scr", "lib", "IrishRulesModelSensitive.R")) #Loard the Irish rules model


#Wrapper function used to detemine which models to run
RunModel <- function(x, 
                     ir_run = FALSE, 
                     ir_def_run = FALSE, 
                     model_parameters = "default", 
                     run_type=run_type ) {
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
###################################################################################
# Run the models
###################################################################################


starttime <- Sys.time()
cores <- ifelse(detectCores() > 1, detectCores()-1, 1) 
cl <- makeCluster(cores)
clusterExport(cl, c("BlightR","IrishRulesModel", "lss", "RunModel", "ExtractCol"))


out_ls <-
  pblapply(lss, function(x)  
    RunModel(x, run_type = "model",ir_run = TRUE,ir_def_run = TRUE) , 
    cl = cl)
trt_ls <-  
  pblapply(wth_ls, function(x)  
    RunModel(x, run_type = "wth",ir_run = TRUE,ir_def_run = TRUE), 
    cl = cl)



#add ID to each outbreak
# out_ls <- 
#   lapply(seq_along(out_ls), function(x) add_column(out_ls[[x]], id = x, .before = "doy"))



default_res_ls <- list(out_ls, trt_ls)
names(default_res_ls) <- c("sens", ("spec"))

save(default_res_ls, file = here::here("out","default", "model_outputs.Rdata"))

rm( out_ls, trt_ls)



###################################################################################
#Calculate the cutoff points - decision thresholds 
###################################################################################

load( file = here::here("out", "default", "model_outputs.Rdata"))

trt_df <- do.call("rbind", default_res_ls[2][[1]])

#A function to calculate the cutoff points 
Cutoffs <- function(x){
  quantile(
    x[x > 0]%>% unlist() , 
    probs = seq(0, 1, 0.04),
    na.rm = TRUE
    )
}


warn_t_df <- 
  data.frame(warn_thresh = 1:26,
             risk_si =  Cutoffs(trt_df[ , "risk_si"]),
             risk_mi = Cutoffs(trt_df[ , "risk_mi"]),
             risk = Cutoffs(trt_df[ , "risk"]),
             ir_risk = seq.int(1,26,1),
             defir_risk = seq.int(1,26,1)
  )
#Set the range of cutoff points
warning_thresholds <- warn_t_df$warn_thresh

save(warn_t_df, file = here::here("out", "default", "warning_thresholds.Rdata"))

rm(Cutoffs,trt_df)

###################################################################################
#Calculate the number of treatments
###################################################################################
# load( file = here::here("out", "default", "warning_thresholds.Rdata"))


duration_of_season <- nrow(unique(default_res_ls[2][[1]][[1]]["doy"]))
min_prot_dur <-  7
max_trt <- 
  round(duration_of_season  / min_prot_dur,2)


min_prot_dur_e  = c(7:15)
max_trt_e <-
  round(duration_of_season  / min_prot_dur_e,2)


red_df <- 
data.frame(min_prot_dur_e  = min_prot_dur_e,
           max_trt_e = max_trt_e,
           reduction = round(max_trt_e/ max_trt,2))
rm(duration_of_season)


save(red_df, file = here::here("out", "default", "avg_reduction_def.Rdata"))


###################################################################################
#Calculate the number of predicted outbreaks and treatments
###################################################################################


load( file = here::here("out", "default", "avg_reduction_def.Rdata"))
load( file= here::here("tmp", "outbreaks&wth&outputs.Rdata"))
source(here::here("scr", "lib", "DiagFuns.R"))


cores <- ifelse(detectCores() > 1, detectCores()-1, 1) 
cl <- makeCluster(cores)

clusterExport(cl, c("default_res_ls", "TPPFun", "ControlFreqFun", 
                    "warn_t_df", "warning_thresholds",
                    "min_prot_dur",
                    "max_trt"))
clusterEvalQ(cl, library("tidyverse", quietly = TRUE, verbose = FALSE))

#Calculate the number of predicted outbreaks for each waof the cutoff points
begin <-  Sys.time()
tpp_ev_ls <-
  pbapply::pblapply(warning_thresholds, function(x) {
    TPPFun(x, default_res_ls[1][[1]])
  },
  cl = cl) %>% bind_rows()



trt_ev_ls <-
  pbapply::pblapply(default_res_ls[2][[1]], function(data) {
    lapply(warning_thresholds, function(cutoff) {
      ControlFreqFun(cutoff,
                     warn_t_df,
                     data,
                     no_cal = max_trt,
                     min_prot_dur = min_prot_dur)
    }) %>% bind_rows()
  },
  cl = cl) 



Sys.time() - begin;rm(begin)
stopCluster(cl)

#Save outputs of the diagnostic performance at 26 utoff points

default_eval_lss <- list(tpp_ev_ls, trt_ev_ls)

save(default_eval_lss, file = here::here("out", "default", "model_eval.Rdata"))


###################################################################################
#Visualise the outputs of threatment lists
###################################################################################
source(here::here("scr","lib",  "pkg.R"))
load( file = here::here("out", "default", "model_eval.Rdata"))
source(here::here("scr", "lib", "DiagFuns.R"))
load( file = here::here("out", "default", "avg_reduction_def.Rdata"))

tpp_ev_ls <-  default_eval_lss[[1]]
trt_ev_ls <-  default_eval_lss[[2]]



lapply(default_res_ls[2][[1]], function(x){
  x <- separate(x, id, into = c("stna", "year"), sep = "_")
  data.frame(risk = sum(x$risk,na.rm = T  ),
             stna = unique(x$stna))
}) %>% 
  bind_rows() %>% 
  ggplot()+
  geom_boxplot(aes(stna, risk))+
  coord_flip()



lapply(trt_ev_ls, function(x){
  x <- separate(x, id, into = c("stna", "year"), sep = "_")
  
 bind_rows (data.frame(risk = x[x$warning_thres == 13,"risk_si_trt"] %>% unlist(),
             stna = unique(x$stna),
             model = "Rsi"),
  data.frame(risk = x[x$warning_thres == 14,"risk_mi_trt"] %>% unlist(),
             stna = unique(x$stna),
             model = "Rmi"),
  data.frame(risk = x[x$warning_thres == 11,"risk_trt"] %>% unlist(),
             stna = unique(x$stna),
             model = "R"),
  data.frame(risk = x[x$warning_thres == 2,"ir_risk_trt"] %>% unlist(),
             stna = unique(x$stna),
             model = "MIR"),
  data.frame(risk = x[x$warning_thres == 2,"ir_risk_trt"] %>% unlist(),
             stna = unique(x$stna),
             model = "IR"))
}) %>% 
  bind_rows() %>% 
  ggplot(aes( reorder(stna, risk, FUN = median), risk))+
  geom_boxplot()+
  facet_wrap(~model, nrow = 1)+
  coord_flip()+
  labs(title = "Number of treatments with the risk model at 90% accuracy.")

lapply(trt_ev_ls, function(x){
  x <- separate(x, id, into = c("stna", "year"), sep = "_")
  
  bind_rows (data.frame(risk = x[x$warning_thres == 13,"risk_si_trt"] %>% unlist(),
                        stna = unique(x$stna),
                        model = "Rsi"),
             data.frame(risk = x[x$warning_thres == 14,"risk_mi_trt"] %>% unlist(),
                        stna = unique(x$stna),
                        model = "Rmi"),
             data.frame(risk = x[x$warning_thres == 11,"risk_trt"] %>% unlist(),
                        stna = unique(x$stna),
                        model = "R"),
             data.frame(risk = x[x$warning_thres == 6,"ir_risk_trt"] %>% unlist(),
                        stna = unique(x$stna),
                        model = "MIR"),
             data.frame(risk = x[x$warning_thres == 2,"ir_risk_trt"] %>% unlist(),
                        stna = unique(x$stna),
                        model = "IR"))
}) %>% 
  bind_rows() %>% 
  ggplot()+
  geom_histogram(aes(risk), bins = 60)+
  facet_wrap(~model)







#################################################################
#Calculate the partial AUC
#################################################################

# source(here::here("scr","lib",  "pkg.R"))
# load( file = here::here("out", "default", "model_eval.Rdata"))
load(file = here::here("out", "default", "avg_reduction_def.Rdata"))

# the number of treatments 
max_trt <- red_df[ red_df$min_prot_dur_e==7 ,"max_trt_e"]

source(here::here("scr", "lib", "DiagFuns.R"))
# tpp_ev_ls <-  default_eval_lss[[1]]
# trt_ev_ls <-  default_eval_lss[[2]]
#  
eval_long <- EvalTable(tpp_ev_ls, trt_ev_ls)

eval_long$model <- 
  gsub("risk_", "R",  eval_long$model) %>% 
  gsub("risk", "R",  .) %>% 
  gsub("ir_R", "MIR",  .) %>% 
  gsub("defMIR", "IR",  .) 

eval_longer <- EvalCutoff(eval_long, 
                          cutoffs =  c(0.8, 0.85, 0.9))

fin <- DiagPerformance(eval_longer = eval_longer,
                       pAUCcutoff = .8,
                       no_of_outbreaks = 362)

fin$model <- 
  factor(fin$model, levels = c(  "R", "Rsi","Rmi", "IR","MIR" ))

fin %>% 
  arrange(model) %>% 
  rename(`False negative` = out_miss,
         Model = model,
         `Maximum TPR (%)` = maxTPR)%>%
  mutate(pAUC = round(pAUC, 3)) %>% 
  mutate(AUC = round(AUC, 3)) %>% 
  write_csv(here::here("out" ,"default", "Diag perf default.csv" ))


# shell.exec(here::here("out" ,"default", "Diag perf default.csv" ))

checkdf <- 
eval_longer %>% filter(sens == .85) %>% select(model, spec)

# lapply(trt_ev_ls, function(x){
#   x <- separate(x, id, into = c("stna", "year"), sep = "_")
#   
#   bind_rows (data.frame(risk = x[x$warning_thres == checkdf[checkdf$model<= "Rsi", "spec"],"risk_si_trt"] %>% unlist(),
#                         stna = unique(x$stna),
#                         model = "Rsi"),
#              data.frame(risk = x[x$warning_thres == checkdf[checkdf$model<= "Rmi", "spec"],"risk_mi_trt"] %>% unlist(),
#                         stna = unique(x$stna),
#                         model = "Rmi"),
#              data.frame(risk = x[x$warning_thres == checkdf[checkdf$model<= "R", "spec"],"risk_trt"] %>% unlist(),
#                         stna = unique(x$stna),
#                         model = "R"),
#              data.frame(risk = x[x$warning_thres == checkdf[checkdf$model<= "MIR", "spec"],"ir_risk_trt"] %>% unlist(),
#                         stna = unique(x$stna),
#                         model = "MIR"),
#              data.frame(risk = x[x$warning_thres == checkdf[checkdf$model<= "IR", "spec"],"ir_risk_trt"] %>% unlist(),
#                         stna = unique(x$stna),
#                         model = "IR"))
# })
#################################################################
#Diag plots
#################################################################


#MAke legend labels that will contain the diagnostic information
finlab <- 
  fin %>% 
  # arrange(desc(pAUC)) %>% 
  mutate(pAUC = round(pAUC, 3)) %>% 
  mutate(AUC = round(AUC, 3)) %>% 
  select(-c(out_miss, AUC)) %>% 
  select(model, pAUC, maxTPR) %>% 
  arrange(model)
  unite( "lab" ,2:3, sep= "; ") %>% 
  mutate(lab = paste0("(", lab, "%)"))

eval_longerdf <- 
  
  mutate(eval_longer, model= factor(model, levels = c(  "R", "Rsi","Rmi", "IR","MIR" ))) %>% 
  left_join(., finlab, by = "model") %>% 
  mutate(modellab = paste(model, lab)) %>% 
  mutate(modellab = factor(modellab)) %>% 
  arrange(model)



#Set  color scheme
my_pair_lab <- c(unikn::seecol(pal_unikn_pair)[c(1,7,9)], "darkgray", "black")
names(my_pair_lab) <- unique(eval_longerdf$modellab)


p <- 

  ggplot() +
  geom_rect(mapping = aes( xmin =0, xmax=1, ymin = .8, ymax=1), 
            fill = "#D7DBDD", color = "white",alpha = .1) +
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
  geom_point(aes(spec, sens, color = fct_rev(modellab)), data = eval_longerdf,
             size = .5,show.legend=FALSE, alpha = .9) +
  geom_line(aes(spec, sens, color = fct_rev(modellab)), data = eval_longerdf,
            size = .6, alpha = .9) +
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
    panel.grid.minor = element_blank(),
    legend.position = c(.16, .64),
    legend.text = element_text(size = 13.3),
    legend.title = element_text(size = 13.3),
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
                     name = "Proportion of predicted outbreaks (TPR)")


g <- 
  p+
    theme(
      legend.position = "none",
      text = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.background = element_rect(colour = "black", fill = "white", size = .3)
    )+
  coord_cartesian(ylim = c(0.8, 1), xlim = c(0.58, 1)) 


# Create inset plots
lb <- .57 #left border of the inset plot
rb <- .97 #right border of the inset plot
ub <- 0.48 #upper border of the inset plot

polydata <- data.frame(x = c(lb, 1.0, 1.0, lb, lb),
                       y = c(.79, .79, 1.0, 1.0, .79))
linedata <- data.frame(x = c(lb, .4 , 1.0, rb , lb, .4, 1, rb),
                       y = c(.79, .0 , .99,ub, 1  , ub , .8, 0),
                       id = c("a","a", "b", "b" , "c", "c", "d", "d"))

p +
  geom_path(data = polydata,
            aes(x, y),
            alpha = .5 ,
            size = .4) +
  geom_line(
    data = linedata,
    aes(x, y, group = id),
    linetype = "dashed",
    alpha = .5,
    size = .4
  ) +
  annotation_custom(
    grob = ggplotGrob(g),
    xmin = .4,
    xmax = rb,
    ymin = -.02,
    ymax = ub
  )+
  ggsave(filename = here::here("out", "default", "model_eval_ribbon.png"), 
         width = 8.5, height = 6.5, units = "in", 
         dpi=820)

  shell.exec(here::here("out", "default", "model_eval_ribbon.png"))
  # shell.exec(here::here("out", "default"))
  


