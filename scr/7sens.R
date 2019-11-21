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


par_set <- readxl::read_xlsx( here::here("scr", "model", "par_eval", "par_eval.xlsx"))[,"met_set"] %>% unlist() %>% as.character()


###################################################################################
# Run model
###################################################################################
starttime <- Sys.time()


for (i in par_set){
  #  i <-  par_set[2]
  # x <- lss[[1]]
  # run_type <- "model"
  
cl <- makeCluster(detectCores())
clusterExport(cl, c("BlightR","IrishRulesModel", "RunModel",
                    "TPPFun", "ControlFreqFun", 
                    "warn_t_df", "warning_thresholds",
                    "max_trt", 
                    "ExtractCol",
                    "i"))

clusterEvalQ(cl, library("tidyverse", quietly = TRUE, verbose = FALSE))

  
out_ls <- 
  pblapply(lss, function(x)  RunModel(x, 
                                           model_parameters = i,
                                           run_type = "model"
                                           ) , cl = cl)

out_trt <- pblapply(wth_ls, function(x)  RunModel(x,
                                                       model_parameters = i, 
                                                       run_type = "wth"),
                    cl = cl)

res_lss <- list(out_ls, out_trt)
rm(out_ls, out_trt)
save(res_lss, file = here::here("out","eval", "out", paste0(i,".Rdata")))


clusterExport(cl, c("res_lss"))
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

###############################################################
#Save diag plots
###############################################################
p1 <- 
  ggplot(eval_long)+
  geom_point(aes(spec, sens, color = model))+
  geom_line(aes(spec, sens, color = model))+
  scale_y_continuous(limits = c(0, 1),
                     expand = c(0, 0),
                     breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                     name = "Sensitivity") +
  scale_x_continuous(limits = c(0, 1),
                     expand = c(0, 0),
                     breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                     name = "Proportion of treatment reduction from 5-day calendar")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  theme_bw() +
  theme(
    text = element_text(size = 10.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


p2 <- 
  p1+
  coord_cartesian(ylim=c(0.8,1), xlim = c(0.7,1))
ggsave(filename = here::here("out", "eval", "graphs", paste0(i,".png")),  plot=p1)
ggsave(filename = here::here("out", "eval", "graphs", paste0(i, "_zoom",".png")),  plot=p2)


eval_lss <- list(tpp_ev_ls, trt_ev_ls)
save(eval_lss, file = here::here("out","eval", "eval", paste0(i,".Rdata")))
rm(tpp_ev_ls, trt_ev_ls, eval_lss)


rm(cl, p1, p2, eval_long )
print(paste0(i,": ",  time_length(Sys.time() - starttime, unit = "minutes")))

}


print(paste0(i,": ",  time_length(Sys.time() - starttime, unit = "minutes")))

