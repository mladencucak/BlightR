
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

names(default_res_ls) <- c("sens", ("spec"))
save(default_res_ls,ile = here::here("out","default", "model_outputs.Rdata"))

rm( out_ls, out_trt)

