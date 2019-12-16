
###################################################################################
# Source data
###################################################################################
if("here"%in% installed.packages()==FALSE){install.packages("here");require("here")}
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
trt_ls <-  pblapply(wth_ls, function(x)  RunModel(x, run_type = "wth",ir_run = TRUE,ir_def_run = TRUE), cl = cl)


# out_ls <- list()
# trt_ls <- list()
# for(i in seq_along(lss)){
#   out_ls[[i]] <- BlightR(lss[[i]])
#   out_ls[[i]]$ir <- IrishRulesModel(lss[[i]], temporal_res = "daily")
#   print(i)
# }
# for(i in seq_along(wth_ls)){
#   trt_ls[[i]] <- BlightR(wth_ls[[i]])
#   trt_ls[[i]]$ir <- IrishRulesModel(wth_ls[[i]], temporal_res = "daily", replace_na = TRUE)
#   print(i)
# }

#add ID to each outbreak
# out_ls <- 
#   lapply(seq_along(out_ls), function(x) add_column(out_ls[[x]], id = x, .before = "doy"))



default_res_ls <- list(out_ls, trt_ls)
names(default_res_ls) <- c("sens", ("spec"))

save(default_res_ls, file = here::here("out","default", "model_outputs.Rdata"))

rm( out_ls, trt_ls)

################################
# 
################################


################################
#Calculate the cutoff points - decision thresholds 
################################

load( file = here::here("out", "default", "model_outputs.Rdata"))
trt_df <- do.call("rbind", default_res_ls[2][[1]])

Cutoffs <- function(x){
  quantile(drop_na(x[x>0,])%>% unlist(), probs =seq(0, 1, 0.04)) 
}

warn_t_df <- 
  data.frame(warn_thresh = 1:26,
             risk_si =  Cutoffs(trt_df[ , "risk_si"]),
             risk_mi = Cutoffs(trt_df[ , "risk_mi"]),
             risk = Cutoffs(trt_df[ , "risk"]),
             cumul_risk_si = Cutoffs(trt_df[ , "cumul_risk_si"]),
             cumul_risk_mi = Cutoffs(trt_df[ , "cumul_risk_mi"]),
             cumul_risk = Cutoffs(trt_df[ , "cumul_risk"]),
             ir_risk = seq.int(1,26,1),
             defir_risk = seq.int(1,26,1)
  )
#Set the range ow cutoff points
warning_thresholds <- warn_t_df$warn_thresh

save(warn_t_df, file = here::here("out", "default", "warning_thresholds.Rdata"))

rm(Cutoffs,trt_df)

################################
#Calculate the number of treatments
################################


duration_of_season <- nrow(unique(default_res_ls[2][[1]][[1]]["doy"]))
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
rm(duration_of_season)

####################################################
#Calculate the number of predicted outbreaks and treatments
####################################################


# load( file= here::here("tmp", "outbreaks&wth&outputs.Rdata"))


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


#############################################
#Visualise the outputs of threatment lists
#############################################
source(here::here("scr","lib",  "pkg.R"))
load( file = here::here("out", "default", "model_eval.Rdata"))

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
  
  data.frame(
    risk = x[x$warning_thres == 12,"risk_trt"] %>% unlist(),
             stna = unique(x$stna))
}) %>% 
  bind_rows() %>% 
  ggplot(aes( reorder(stna, risk, FUN = median), risk))+
  geom_boxplot()+
  coord_flip()+ 
  labs(title = "Number of treatments with the risk model at 90% accuracy.")


#for each model, find a 90% risk trhreshold

tpp_ev_ls <-  default_eval_lss[[1]]
trt_ev_ls <-  default_eval_lss[[2]]

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

#################################################################
#Diag plots
#################################################################


eval_long$model <- 
  gsub("risk_", "R",  eval_long$model) %>% 
    gsub("risk", "R",  .) %>% 
    
    gsub("cumul_R", "cR",  .) %>% 
    gsub("ir_R", "MIR",  .) %>% 
  gsub("defMIR", "IR",  .) 
  
eval_long$model <- 
  factor(eval_long$model, levels = c(  "R", "cR", "Rsi", "cRsi","Rmi", "cRmi", "IR","MIR" ))

# eval_long$col <-  my_pair
#Set  color scheme
library('unikn') 
my_pair <- seecol(pal_unikn_pair, n = 8)
names(my_pair) <- levels(eval_long$model)

eval_long%>%
  # arrange(spec) %>%
  # mutate(model = factor(model,levels =as.character(levels(eval_long$model)))) %>% 

  ggplot(aes(x =spec, y =sens, color =model)) +
  # geom_point(size = .5) +
  geom_line() + 
  scale_color_manual(values = my_pair)
scale_colour_identity()

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
  

  
  
 p3 <- 
  ggplot(eval_long,aes(spec, sens, color = model, label = warning_thres))+
  geom_hline(
    yintercept = seq(0 , 1, 0.1),
    size = 0.1,
    color = "gray",
    linetype = "dotted"
  )+
  geom_hline(yintercept = c(.9,.8), 
             linetype = "dashed",
             size = .2) +
  geom_vline(
    xintercept = red_df$reduction,
    size = 0.1,
    color = "gray",
    linetype = "solid",
    alpha= .5
  )+
  
  geom_point( colour = "black",size = 0.5)+
  
  geom_line(aes(spec, sens, color = model))+
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
    
  ggrepel::geom_text_repel(size = 2)+
  theme_bw() +
  theme(
    text = element_text(size = 10.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  
  facet_wrap(~model, ncol = 4)+
  
  theme(legend.position = "top")

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
   
ggsave(filename = here::here("out", "default", "model_eval.png"),  plot=p1,
       width = 7, height = 4, units = "in")
ggsave(filename = here::here("out", "default", "model_eval_crop.png"),  plot=p2,
       width = 7, height = 4, units = "in")
ggsave(filename = here::here("out", "default", "model_eval_facets.png"),  plot=p3,
       width = 7, height = 5, units = "in")

shell.exec(here::here("out", "default"))

#################################################################
#Calculate the partial ROC
#################################################################


eval_longer <- 
split(eval_long, eval_long$model) %>% 
  
lapply(., function(fundf){

  for(prop_tpp in c(0.8, 0.85, 0.9)){
  
 if(fundf$sens[1] >prop_tpp){
   # find the two nearest warning thresholds to the accepted decision threshold
   closest_high <- 
     fundf$sens[fundf$sens>prop_tpp] %>% tail(1)
   
   closest_low <-
     fundf$sens[which(fundf$sens<prop_tpp)][1]
   
   dff <- fundf[fundf$sens >= closest_low& fundf$sens <= closest_high,]
   spec <- dff$spec %>% unlist()
   sens <- dff$sens %>% unlist
   
   value <- 
     predict(lm(spec ~ sens ), data.frame(sens = prop_tpp))
   
   which(fundf$sens<prop_tpp)[1]
   fundf <- 
   add_row(fundf, 
           warning_thres = prop_tpp,
           model = unique(fundf$model),
           spec = value, 
           sens = prop_tpp,
           .before = which(fundf$sens<prop_tpp)[1])
 }else{
   fundf <- 
   add_row(fundf, 
           warning_thres = prop_tpp,
           model = unique(fundf$model),
           spec = NA, 
           sens = prop_tpp,
           .before = 1)
 }
  

}
  return(fundf)
  
}
) %>% 
  bind_rows()


eval_longer %>% 
  group_by(model) %>% 
  filter(warning_thres %in% c( 0.9, 0.85, 0.8)) %>% 
  mutate(spec = round(spec, 3)*100) %>% 
  dplyr::arrange(warning_thres) %>% 
  dplyr::select(-c(warning_thres)) %>% 
  
  # group_by(model) %>% 
  # dplyr::arrange(desc(model)) %>% 
  spread(model, spec) %>% 
  write_csv(here::here("out" ,"default", "Diag perf default.csv" ))
  spread(model, spec)


eval_long$warning_thres


eval_longer %>% 
  group_by(model) %>% 
  filter(sens>= 0.8) %>% 
  mutate(spec = round(spec, 3)*100) %>% 
  # ggplot(aes(spec,sens, color = model))+
  # geom_line()+
  # geom_point(size = .5)
  # facet_wrap(~model)%>%
    ggplot(.,aes(spec, sens, color = model)) +
    geom_hline(
      yintercept = seq(0 , 1, 0.1),
      size = 0.1,
      color = "gray",
      linetype = "dotted"
    )+
    geom_hline(yintercept = c(.9,.8), 
               linetype = "dashed",
               size = .2) +
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
                       breaks = seq(0.8, 1, 0.1),
                       name = "Proportion of predicted outbreaks") +
    scale_x_continuous(
      limits = c(0, 1),
      expand = c(0, 0),
      breaks = red_df$reduction,
      labels = red_df$min_prot_dur_e,
      name = "Proportion of treatment reduction from the 7-day application schedule"
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 10.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  




