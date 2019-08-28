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

