require(stringr)

if(!is.na(Sys.getenv("RSTUDIO", unset = NA))){
  
  user_receive <- readline(prompt = cat("The following WSC stations are available for calibration:", stations.included, "\n",
                                      "Please enter station numbers to include in calibration (separated by commas)..."))

  ## Create a character vector of the stations to be included in calibration
  calibration.stations <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")
  
  print(paste(c("You entered...", calibration.stations)))
  

  
} else { 

if(!is.na(Sys.getenv("SHELL", unset = NA))){
  
  cat("The following WSC stations are available for calibration:", stations.included, "\n",
      "Please enter station numbers to include in calibration (separated by commas)...")
  user_receive <- readLines(con = "stdin", 1)
  
  ## Create a character vector of the stations to be included in calibration
  calibration.stations <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")
  
  cat("You entered...", calibration.stations, "\n")
  
  } 
}


# ## Check to see if there are snow pillows and/snow courses included in the current model run.
# 
# if(exists("snow.courses.included")){
# 
#     snow.courses.included <- snow.courses.included
# 
# } else{
#   
#   snow.courses.included <- "None"
#   
# }
# 
# if(exists("snow.pillows.included")){
#   
#   snow.pillows.included <- snow.pillows.included
#   
# } else {
#   
#   
#   snow.pillows.included <- "None"
# }
# 
# 
# max.len <- max(length(stations.included), length(snow.courses.included), length(snow.pillows.included))
# 
# 
# stations.included <- c(stations.included, rep(NA, max.len - length(stations.included)))
# snow.courses.included <- c(snow.courses.included, rep(NA, max.len - length(snow.courses.included)))
# snow.pillows.included<- c(snow.pillows.included, rep(NA, max.len - length(snow.pillows.included)))
# 
# 
# data.frame(WSC_Stations = stations.included,
#            Snow_Courses = snow.courses.included,
#            Snow_Pillows = snow.pillows.included
#            )


