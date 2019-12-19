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


###################################################################################
# Run model
###################################################################################
starttime <- Sys.time()


for (i in par_set[93:length(par_set)]){
  #  i <-  par_set[55]
  # x <- lss[[1]]
  # run_type <- "model"


cl <- makeCluster(detectCores()-1)
clusterExport(cl, c("BlightR","IrishRulesModel", "RunModel",
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

Sys.sleep(10)

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

rm(Cutoffs,trt_df,out_ls, out_trt,duration_of_season)

###################################################
#Evaluation
###################################################

clusterExport(cl, c("res_lss",
                    "warn_t_df",
                    "warning_thresholds",
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
                     min_prot_dur = 5)
    }) %>% bind_rows()
  },
  cl = cl) 

# Get the evaluation data in long format
eval_long <- EvalTable(tpp_ev_ls, trt_ev_ls)

Sys.sleep(10)

###############################################################
#Save diag plots
###############################################################

eval_long$model <- 
  gsub("risk_", "R",  eval_long$model) %>% 
  gsub("risk", "R",  .) %>% 
  gsub("ir_R", "MIR",  .) %>% 
  gsub("defMIR", "IR",  .) 

eval_long$model <- 
  factor(eval_long$model, levels = c(  "R", "Rsi","Rmi", "IR","MIR" ))

#Set  color scheme

my_pair <- seecol(pal_unikn_pair)[c(1,7,9,15,16)]
names(my_pair) <- levels(eval_long$model)




pp <- 
  eval_long %>%
  ggplot(aes(spec, sens, color = model)) +
  geom_hline(
    yintercept = seq(0 , 1, 0.1),
    size = 0.1,
    color = "gray",
    linetype = "dotted"
  )+
  geom_vline(
    xintercept = red_df$reduction,
    size = 0.1,
    color = "gray",
    linetype = "solid",
    alpha= .5
  )+
  geom_point(size = .5) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1),
                     expand = c(0, 0),
                     breaks = seq(0, 1, 0.1),
                     name = "Proportion of predicted outbreaks") +
  scale_x_continuous(
    limits = c(0, 1),
    expand = c(0, 0),
    breaks = red_df$reduction,
    labels = red_df$min_prot_dur_e,
    name = "Proportion of treatment reduction from the 7-day application schedule"
  ) +
  scale_color_manual(values = my_pair)+
  labs(color = "Model:")+
  theme_bw() +
  theme(
    text = element_text(size = 10.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p1 <- 
  pp+
  geom_hline(yintercept = c(.9,.8), 
             linetype = "dashed",
             size = .2)

p2 <-
  pp +
  geom_hline(
    yintercept = c(.95, .9, .85),
    linetype = "dashed",
    size = .1,
    alpha = .5
  ) +
  coord_cartesian(ylim = c(0.8, 1), xlim = c(0.6, 1)) +
  scale_y_continuous(
    
    breaks = seq(0.8, 1, 0.05),
    name = "Proportion of predicted outbreaks"
  ) 




ggsave(filename = here::here("out", "eval", "graphs", paste0(i,".png")),  plot=p1)
ggsave(filename = here::here("out", "eval", "graphs", paste0(i, "_zoom",".png")),  plot=p2)


eval_lss <- list(tpp_ev_ls, trt_ev_ls)
save(eval_lss, file = here::here("out","eval", "eval", paste0(i,".Rdata")))
rm(tpp_ev_ls, trt_ev_ls, eval_lss)


rm(cl, p1, p2, eval_long )
print(paste0(i,": ",  time_length(Sys.time() - starttime, unit = "minutes")))
Sys.sleep(10)
}


print(paste0(i,": ",  round(time_length(Sys.time() - starttime, unit = "hours"), 3)))

# source(here::here("scr", "lib", "GitCommit.R" ))

