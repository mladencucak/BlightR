
###################################################################################
# Source data
###################################################################################
source(here::here("scr","lib",  "pkg.R"))
source(here::here("scr", "lib", "funs.R"))


load(file = here::here("dat", "outbreak&weather.Rdata"))
load(file= here::here("dat", "treatment_no_estim.Rdata"))


###################################################################################
# Funs
###################################################################################
source(here::here("scr", "model", "run.R"))
source(here::here("scr", "lib", "IrishRulesModelSensitive.R"))



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
###################################################################################
# Run
###################################################################################

library("pbapply")
library("parallel")
starttime <- Sys.time()
cores <- ifelse(detectCores() > 1, detectCores()-1, 1) 
cl <- makeCluster(cores)
clusterExport(cl, c("BlightR","IrishRulesModel", "lss", "RunModel", "ExtractCol"))


out_ls <- pblapply(lss, function(x)  RunModel(x, run_type = "model",ir_run = TRUE,ir_def_run = TRUE) , cl = cl)
out_trt <-  pblapply(wth_ls, function(x)  RunModel(x, run_type = "wth",ir_run = TRUE,ir_def_run = TRUE), cl = cl)


# out_ls <- list()
# out_trt <- list()
# for(i in seq_along(lss)){
#   out_ls[[i]] <- BlightR(lss[[i]])
#   out_ls[[i]]$ir <- IrishRulesModel(lss[[i]], temporal_res = "daily")
#   print(i)
# }
# for(i in seq_along(wth_ls)){
#   out_trt[[i]] <- BlightR(wth_ls[[i]])
#   out_trt[[i]]$ir <- IrishRulesModel(wth_ls[[i]], temporal_res = "daily", replace_na = TRUE)
#   print(i)
# }

#add ID to each outbreak
# out_ls <- 
#   lapply(seq_along(out_ls), function(x) add_column(out_ls[[x]], id = x, .before = "doy"))



default_res_ls <- list(out_ls, out_trt)

save(default_res_ls, file = here::here("out","default", "model_outputs.Rdata"))

rm( out_ls, out_trt)

################################
#Calculate the cutoff points - decision thresholds 
################################

load( file = here::here("out", "default", "model_outputs.Rdata"))
out_trt_df <- do.call("rbind", default_res_ls[2][[1]])

Cutoffs <- function(x){
  quantile(drop_na(x[x>0,])%>% unlist(), probs =seq(0, 1, 0.04)) 
}

warn_t_df <- 
  data.frame(warn_thresh = 1:26,
             risk_si =  Cutoffs(out_trt_df[ , "risk_si"]),
             risk_mi = Cutoffs(out_trt_df[ , "risk_mi"]),
             risk = Cutoffs(out_trt_df[ , "risk"]),
             cumul_risk_si = Cutoffs(out_trt_df[ , "cumul_risk_si"]),
             cumul_risk_mi = Cutoffs(out_trt_df[ , "cumul_risk_mi"]),
             cumul_risk = Cutoffs(out_trt_df[ , "cumul_risk"]),
             ir_risk = seq.int(1,26,1),
             defir_risk = seq.int(1,26,1)
  )
#Set the range ow cutoff points
warning_thresholds <- warn_t_df$warn_thresh

save(warn_t_df, file = here::here("out", "default", "warning_thresholds.Rdata"))

rm(Cutoffs,out_trt_df)

################################
#Calculate the number of treatments
################################
# TODO possibly reduce to 6 days because the average between 7/5 days could be there
duration_of_season <- nrow(unique(default_res_ls[2][[1]][[1]]["doy"]))
max_trt <- 
  round(duration_of_season  / min_prot_dur,1)
min_prot_dur  = c(5:7)

data.frame(min_prot_dur  = min_prot_dur,
           max_trt = max_trt)
rm(duration_of_season)

################################
#Calculate the number of treatments
################################


# load( file= here::here("tmp", "outbreaks&wth&outputs.Rdata"))


source(here::here("scr", "lib", "DiagFuns.R"))

cores <- ifelse(detectCores() > 1, detectCores()-1, 1) 
cl <- makeCluster(cores)

clusterExport(cl, c("default_res_ls", "TPPFun", "ControlFreqFun", 
                    "warn_t_df", "warning_thresholds",
                    "min_prot_dur",
                    "max_trt"))
clusterEvalQ(cl, library("tidyverse", quietly = TRUE, verbose = FALSE))


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

#############################################
#Visualise the outputs of threatment lists
#############################################
lapply(default_res_ls[2][[1]], function(x){
  x <- separate(x, id, into = c("stna", "year"), sep = "_")
  data.frame(risk = sum(x$cumul_risk,na.rm = T  ),
             stna = unique(x$stna))
}) %>% 
  bind_rows() %>% 
  ggplot()+
  geom_boxplot(aes(stna, risk))+
  coord_flip()



lapply(trt_ev_ls, function(x){
  x <- separate(x, id, into = c("stna", "year"), sep = "_")
  
  data.frame(risk = x[x$warning_thres == 12,"risk_trt"] %>% unlist(),
             stna = unique(x$stna))
}) %>% 
  bind_rows() %>% 
  ggplot(aes( reorder(stna, risk, FUN = median), risk))+
  geom_boxplot()+
  coord_flip()+
  labs(title = "Number of treatments with the risk model at 90% accuracy.")




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

p1 <- 
  left_join(trt_ev_long, tpp_ev_long, by = c("warning_thres", "model" )) %>% 
  ggplot()+
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


p3 <- 
  ggplot(eval_long,aes(spec, sens, color = model, label = warning_thres))+
  geom_point( colour = "black",size = 0.5)+
  
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
  ggrepel::geom_text_repel(size = 4)+
  theme_bw() +
  theme(
    text = element_text(size = 10.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  
  facet_wrap(~model, ncol = 4)+
  
  theme(legend.position = "top")

p2 <- 
  p1+
  coord_cartesian(ylim=c(0.8,1), xlim = c(0.7,1))
ggsave(filename = here::here("out", "default", "model_eval.png"),  plot=p1)
ggsave(filename = here::here("out", "default", "model_eval_crop.png"),  plot=p2)
ggsave(filename = here::here("out", "default", "model_eval_facets.png"),  plot=p3)