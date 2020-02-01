# load the forecast data

source(here::here("scr","lib",  "pkg.R"))
# library("readxl")
source(here::here("scr", "lib", "funs.R"))

load( file =here::here("out", "fore", "fore_model_out.Rdata"))


out_df <- 
out_ls %>% 
  bind_rows() %>% 
  select(-id) %>% 
  unite("id", for_date, stna) %>% 
  group_by(id) %>% 
  mutate(day_step = doy - min(doy-1)) %>% 
  ungroup()



out_ls <- split(out_df, out_df$id)
head(out_ls[[1]])
unique(out_ls[[1]]$set)
length(out_ls)

fun_df <-  out_ls[[1]]
model <-  colnames(fun_df[, grepl("risk" , names(fun_df))])[1]

# dd <- 
fun_df %>% 
  select(c(doy, set,id,short_date, model)) %>% 
  spread(set, risk_si)

dd <- 
out_df %>% 
  select(c(doy, set,id,short_date, day_step, model)) %>% 
  spread(set, risk_si)  

out_df %>% 
  select(c(doy, set,id,short_date, day_step, model)) %>% 
  spread(set, risk_si) %>% 
  group_by(day_step) %>% 
  summarise( rmse = sqrt(mean(fore - obs)^2),
             mse = mean(abs(fore - obs))) %>% 
  ungroup() 

####################################################################################
# Evaluation of the binary prediction
####################################################################################

# load the evaluation data from calibrated model
# Add the uncertainty around the threshold ?? Taht woudl make it interesting?? 

load( file = here::here("out", "default", "model_eval.Rdata"))
load( file = here::here("out", "default", "warning_thresholds.Rdata"))

#decide on what is minimum accepted proportion of true predictions
prop_tpp <- 0.9
#pick the model
model <-  colnames(fun_df[, grepl("risk" , names(fun_df))])[1]

# find two nearest warning thresholds to the accepted decision threshold
closest_high <- sum(default_eval_lss[[1]][model]>prop_tpp)
closest_low <-which(default_eval_lss[[1]][model]<prop_tpp)[1]

tpp <- 
default_eval_lss[[1]][model][closest_high:closest_low,] %>% unlist()

# Find the risk estimate for these two thresholds
risk <- 
warn_t_df[default_eval_lss[[1]]["warning_thres"][closest_high:closest_low,] %>% unlist(),model] 
#estimate the risk for point prop_tpp
risk_thresh <- 
predict(lm(risk ~ tpp), data.frame(tpp = prop_tpp))


dd <- 
  out_df %>% 
  select(c(day_step, set,id, model)) 
  

dd[, model] <- ifelse(dd[, model]>risk_thresh, 1, 0) %>% unlist() %>% as.numeric()


dd%>% 
  spread(set, risk_si) %>%
  reshape2::melt( .,
    id.vars = c("day_step", "id", "obs"),
    variable.name = "model",
    value.name = "pred",
    factorsAsStrings  = FALSE
  ) %>% 
  group_by(day_step,  model, obs) %>% 
  summarise(count = n()) %>% 
  mutate(prop = round(prop.table(count),2)) 


dd[dd[model]==1,]


dd %>%
  group_by(day_step,  set) %>% 
  summarise(count = n()) %>% 
  mutate(prop = round(prop.table(count),2)) 
  

write.csv(out_df[out_df$id == out_df$id[1],], here::here("tmp", "test.csv"))





