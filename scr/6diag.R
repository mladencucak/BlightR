source(here::here("scr","lib",  "pkg.R"))

load( file = here::here("out", "default", "model_outputs.Rdata"))

#Get min and max model outputs and calculate the sensitivity for each threshold 
out_trt <- default_res_ls[2][[1]]

#Calculate the reduction in number of treatmetns
  out_trt_df <- do.call("rbind", default_res_ls[2][[1]])


# 
sum_df <- 
out_trt_df[, grepl( "risk" , names(out_trt_df))|
         grepl( "ir" , names(out_trt_df))|
         grepl( "id" , names(out_trt_df))] %>% 
  drop_na() %>% 
  separate(id, into = c("stna", "year"),  sep = "_") %>%
  select(-c(year)) %>% 
  group_by(stna) %>% 
  summarise_all(.,list(max = ~ sum(.))) %>% 
  ungroup() %>% 
  reshape2::melt(., id.vars = "stna", 
                 variable.name = "model", 
                 value.name = "risk" )

ggplot(sum_df)+
  geom_col(aes(stna, log(risk), fill = model), position = "dodge")


Summary <- function(x) {
  x <- x[x>0]
  data.frame(
    min = min(x),
    lowq = quantile(x)["25%"],
    meanq = quantile(x)["50%"],
    median = median(x),
    mean = mean(x),
    highq = quantile(x)["75%"],
    max = max(x)
  )
}

sum_df %>% 
  drop_na() %>% 
  group_by(model) %>% 
  do(Summary(.$risk)) %>% 
  gather("model", "risk") %>% 
  ggplot()+
  geom_point(aes(model, risk))

sum_df %>% 
  drop_na() %>% 
  ggplot()+
  geom_histogram(aes(risk))+
  facet_wrap(~model)



# Set automated cutoff creation if needed
# for(i in seq(models)){
#   warn_t_df[ , i] <- 
#   y <- models[i]
#   diag[[i]] <- 
#     cbind(tpp_ev[,y], trt_ev[,paste0(y,"_trt")], tpp_ev[, "warning_thres"])
# }


# cutoff <- 3
# data <-  default_res_ls[2][[1]][[5]]
# min_prot_dur = 5
# max_prot_dur = NULL
# data <- default_res_ls[2][[1]][[9]]
# data[, models]
# fun_df[, models[7]]
# sum_df
# model_output <- fun_df[, models[1]]%>% unlist() %>% as.numeric()
# model_output <- fun_df[, models[2]]%>% unlist() %>% as.numeric()
# model_output <- fun_df[, models[3]]%>% unlist() %>% as.numeric()
# model_output <- fun_df[, models[4]]%>% unlist() %>% as.numeric()
# model_output <-  c(0,1,0,0,0,0,0,0,1,NA,NA,0,0,0,0,0,0,1,1,1,0)
# y
# fun_df[, models] %>%
#   lapply(., function(x)
#     TreatmentWindowOne(x, min_prot_dur)) %>%
#   bind_cols() %>% view()
# 
# ControlFreqFun(cutoff,
#                warn_t_df,
#                default_res_ls[2][[1]][[9]],
#                no_cal = max_trt,
#                min_prot_dur = 5)





source(here::here("scr", "lib", "pkg.R"))
load( file = here::here("out", "default", "model_outputs.Rdata"))

source(here::here("scr", "lib", "DiagFuns.R"))


begin <- Sys.time()
#Detect the number of cores and set it to total minus 1, if there are multiple cores, to avoid overload
cores <- ifelse(detectCores() > 1, detectCores() - 1, 1)
cl <- makeCluster(cores)
clusterExport(cl, c("default_res_ls", "TPPFun", "ControlFreqFun", 
                    "warn_t_df", "warning_thresholds",
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
                     min_prot_dur = 5)
    }) %>% bind_rows()
  },
  cl = cl) 


Sys.time() - begin;rm(begin)
stopCluster(cl)

default_eval_lss <- list(tpp_ev_ls, trt_ev_ls)

save(default_eval_lss, file = here::here("out", "default", "model_eval.Rdata"))


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
   ggrepel::geom_text_repel(size = 1)+
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

  
diag <- list()
p_ls <- list()

for(i in seq(models)){
  y <- models[i]
  diag[[i]] <- 
    cbind(tpp_ev[,y], trt_ev[,paste0(y,"_trt")], tpp_ev[, "warning_thres"])
  
  p  <- 
   ggplot(diag[[i]])+
    geom_line(aes(unlist(diag[[i]][2]),unlist(diag[[i]][1]), color = "red"))+
    scale_y_continuous( limits = c(0,1))+
    geom_line(aes(unlist(diag[[i]][2]),0.9), linetype = "dashed")+
    geom_line(aes(max_trt,seq(0,1,0.04 )), linetype = "dashed")+
    annotate("text", x =10, y = 0.7,
               label = paste(models[i]))+
    theme(legend.position = "none")+
    labs(
      # colour = "Model performance",
      x = "Reduction of number of treatments",
      y = "Sensitivity/True positive rate")
   p_ls[[i]] <-  p
   
   rm(p)
}

cowplot::plot_grid(plotlist = p_ls[5])




  
  

