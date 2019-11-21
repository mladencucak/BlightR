########################################################
# General purpose functions
########################################################
# data <- fun_df
# column <- "lon"
ExtractCol <- function(data, column){
  #'Function to recognise different versions of column names (title case, capitalised and small letters)
  #' @param data Object of class data frame to be evaluated
  #' @param column The string to match in column names.  
  #' 
  if(!is.data.frame(data)){stop("The data is not of class data.frame!")}
  
  if(length(names(data)[str_detect(names(data), fixed(column, ignore_case=TRUE))])>0){
  col_name <- names(data)[str_detect(names(data), fixed(column, ignore_case=TRUE))]
  } else{
    stop("Variable doesnot exist in the data set.")
  }
  if (length(col_name)>1){
    col_name <-
    col_name [ column   == col_name]
  }
  if (column == "rh"){
    col_name <- "rhum"
  }
  return(col_name)
  #' Example
  #' df <- data.frame(aVerage= c(1,2,3 ), other_col = c(2:4))
  #' aVerage <- df[[DetectCol(df, "ver")]]
  #' all(aVerage==df$aVerage)
}
