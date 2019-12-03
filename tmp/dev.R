


###################################################################################
# Check results
###################################################################################
out_ls <- default_res_ls[1][[1]]


# dff <- 
# out_ls[[1]] 

# color of the back ground baed on the mont//year
PlotFun <- function(dff, plot_all = "all"){
  if(plot_all == "red"){dff <-  select(dff,-c(spor, inf, surv_prob))}
  id <- unite(dff, "id", year,month, variety, comments, stna, dist)[["id"]][1]
  dff %>%
    select(-c(2:11)) %>% 
    mutate(spor_cond = ifelse(spor_cond == "yes", max(dff[, colnames(dff)[grep("risk", colnames(dff))]]), 0)) %>% 
    reshape2::melt(., id.vars = "doy", 
                   variable.name = "model", 
                   value.name = "risk",
                   factorsAsStrings  = FALSE) %>% 
    mutate(risk = ifelse(risk>20, 25, risk)) %>% 
    ggplot(aes(doy,risk, group = model, fill = model))+
    geom_vline(xintercept = c(min(dff$doy)+4), alpha = 0.5)+
    geom_bar(stat="identity",position=position_dodge2(padding = 0.25))+
    scale_fill_brewer(palette="Paired")+
    scale_x_continuous(breaks = min(dff$doy):max(dff$doy))+
    scale_y_continuous(limits = c(0,20))+
    theme_bw()+
    theme(legend.position = "top")+
    labs(subtitle = id, x = "", y = "")
  
}

all_plots =lapply(out_ls, PlotFun)
pdf(here::here("tmp", "test_plot", "test_def_run.pdf"))
all_plots
dev.off()

all_plots =lapply(out_ls[1:2], function(x) PlotFun(x, plot_all = "red"))
pdf(here::here("tmp", "test_plot", "test_def_run_reduced.pdf"), width = 10, height = 5)
all_plots
dev.off()

"2004_6_Kerrs Pink_1−5 isolated plants_Killowen_1.97"

#Calculate the max per model per event
for(i in seq_along(out_ls)){
  out_ls[[i]]$id <- names(out_ls)[i]
}


max_out <- 
out_ls %>% 
  bind_rows() %>% 
# unite(., "id", year,month, variety, comments, stna, dist) %>% 
  select(-c(spor, inf, surv_prob,doy, short_date, lat, long, dbo)) %>% 
  group_by(id) %>% 
  summarise_all(max)


apply(max_out, 2, any( . == 0))

max_out[max_out$risk %in% 0, ]

fun_df <-  out_ls[["2003-07-03//Dundrod//Clough/ Ballymeana, Antrim, UK//25.36"]] 

pbapply::pblapply(warning_thresholds, function(x) {
  TPPFun(x, out_ls[["2003-07-03//Dundrod//Clough/ Ballymeana, Antrim, UK//25.36"]] )
}) %>% bind_rows()


l <- 
lapply(data, function(fun_df) {
  
  # default_res_ls[1][[1]][[1]] -> fun_df
  models <-  colnames(fun_df[, grepl("risk" , names(fun_df))])
  
  for (i in models) {
    #if more or equal to the given warning threshold assign 1
    fun_df[, i] <-
      sapply(fun_df[, i],  function(x) {
        ifelse(x >= warn_t_df[cutoff, i], 1, 0)
      })
    rm(i)
  }
  fun_dff <- summarise_all(fun_df[, models], sum)
  fun_dff[1,] <- ifelse(fun_dff[1,] > 1, 1, 0) #if the threshold is reached change to one
  fun_dff$warning_thres <-  cutoff
  return(fun_dff)
})  

cond <- sapply(l, function(x) x$risk == 0)
l[cond]

  # bind_rows() %>% 
  add_column(., id  = names(out_ls), .before = "risk_si") %>% 
  filter(risk == 0) %>% 
  select("id") %>% 
  unlist()


out_ls[ids] %>% 
  bind_rows() %>% 
  view()




# TODO add station name to the 

out_df <- 
  out_ls %>% 
  bind_rows() %>% 
  group_by(id) %>% 
  summarise( year = unique(year),
             variety = unique(variety),
             month = unique(month(short_date))[2],
             dist  = unique(dists),
             # dist  = unique(dists),
             # stna = unique(stna), 
             sum_day_risk = sum(day_risk, na.rm= TRUE),
             max_day_risk = max(day_risk, na.rm= TRUE),
             sum_day_risk = sum(risk, na.rm= TRUE),
             max_day_risk = max(risk, na.rm= TRUE),
             surv_prob = min(surv_prob, na.rm= TRUE),
             inf_sol = sum(inf_sol, na.rm= TRUE),
             risk_si = max(risk_si, na.rm= TRUE),
             risk_mi = sum(risk_mi, na.rm= TRUE),
             cumul_risk = max(cumul_risk, na.rm= TRUE)
  )

daily <- melt(out_df, id.vars =c( "short_date", "id"), measure.vars = c("irish_rules", "mod_rules"))

n_row <- unique(out_df$year) %>% length()
out_df$id <- factor(out_df$id)



ggplot(out_df) +
  geom_col(aes(x = id, y = ifelse(max_day_risk > 100, 100, max_day_risk), fill = factor(variety))) +
  facet_wrap( ~ year, nrow = n_row, scales = "free",strip.position = "right") 

ggplot(out_df) +
  geom_col(aes(x = id, y = ifelse(max_day_risk > 100, 100, max_day_risk))) +
  facet_wrap( ~ year, nrow = n_row, scales = "free",strip.position = "right") +
  theme(axis.text.x=element_blank())

ggplot(out_df) +
  geom_col(aes(x = id, y = ifelse(risk_si > 100, 100, risk_si))) +
  facet_wrap( ~ year, nrow = n_row, scales = "free",strip.position = "right") +
  theme(axis.text.x=element_blank())

ggplot(out_df) +
  geom_col(aes(x = id, y = ifelse(cumul_risk > 30, 30, cumul_risk))) +
  facet_wrap( ~ year, nrow = n_row, scales = "free",strip.position = "right") +
  theme(axis.text.x=element_blank())


titl <- "Risk moratality*infection"
hist(out_df$risk_mi, breaks = 50)
ggplot(out_df) +
  geom_col(aes(x = id, y = risk_mi), width = 0.5) +
  facet_wrap( ~ year, nrow = n_row, scales = "free",strip.position = "right")+
  scale_y_continuous(limits = c(0, max(out_df$risk_mi)))+
  labs(title = titl)

ggplot(out_df) +
  geom_col(aes(x = id, y = ifelse(risk_mi > 10, 10, risk_mi))) +
  facet_wrap( ~ year, nrow = n_row, scales = "free",strip.position = "right")+
  scale_y_continuous(limits = c(0, max(out_df$risk_mi)))+
  labs(title = titl)


titl <- "Risk sporulation*infection"
hist(out_df$risk_si, breaks = 20)
ggplot(out_df) +
  geom_col(aes(x = id, y = risk_si, color = variety), width = 0.5) +
  facet_wrap( ~ year, nrow = n_row, scales = "free",strip.position = "right")+
  labs(title = titl)
ggplot(out_df) +
  geom_col(aes(x = id, y = ifelse(risk_si > 10, 10, risk_si))) +
  facet_wrap( ~ year, nrow = n_row, scales = "free",strip.position = "right")+
  scale_y_continuous(limits = c(0, max(out_df$risk_mi)))+
  labs(title = titl)



out_df[out_df$day_risk==0,"id"] %>% unlist() %>% as.numeric()

lss[out_df[out_df$day_risk==0,"id"] %>% unlist() %>% as.numeric()]
outl_ls[out_df[out_df$day_risk==0,"id"] %>% unlist() %>% as.numeric()]

tt[[1]] ->x
x

#Visualise the outputs

#Join information aout location, distance of weather station, year, variety...
dists <- 
  sapply(names(lss), function(x)str_split(x, pattern = "//")) %>% sapply(., "[[",4) %>% as.vector() %>% as.numeric()








