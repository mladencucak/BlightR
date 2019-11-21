
load( file= here::here("dat", "weather_infilled.Rdata"))

sol_df <-  read_csv(here::here("dat", "sol_ls.csv"))
str(sol_df)
sol_df$hour <-  12
mean(is.na(sol_df$sol_nasa))

#Some of missing values are recorede as -99 and we shall remove them 
sol_df <-  sol_df[sol_df$sol_nasa >= 0, ]

#Join the data. The daily values will be added to 12th hour
#This does not make difference because the model works total amount of daily radiation
weather$idd <- as.character(weather$short_date)
weather$id <- 
  unite(weather, col = "id", c("idd", "short_date"))


unite()
wth_ls <- split(weather,weather$id )

MoveSolar <- function(x){
  if (all(is.na(x$sol_rad))) {
    x$sol_rad_i <- 2
    x$sol_rad <- 0
    x$sol_rad[13] <- sum(x$sol_nasa,na.rm = TRUE)
  }else{x}
  return(x)
}
 wth_lss <- 
  lapply(wth_ls, MoveSolar)

wth_ls <- bind_rows(wth_ls)

head(weather,20)