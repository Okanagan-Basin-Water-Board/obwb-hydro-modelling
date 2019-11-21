require(stringr)

## For now, only include naturalized streamflow datasets as targets for calibration (and not for validation) - this does not respect the calibration/validation weights.
if(validate.model == FALSE){

  ## Append naturalized flow datasets to WSC stations included as possible calibration targets
  stations.included <- c(stations.included, paste(include.watersheds, "_Nat_Q", sep = ""))

  }

if(length(stations.included) > 1){

  if(!is.na(Sys.getenv("RSTUDIO", unset = NA))){
    
    ## Determine which WSC stations should be included in the calibration

    user_receive <- readline(prompt = cat("The following WSC stations are available for calibration:", stations.included, "\n",
                                        "Please enter station numbers to include in calibration (separated by commas)..."))

  
    ## Create a character vector of the stations to be included in calibration
    calibration.stations <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")

    ## Print out your entry
    # cat("You entered...", calibration.stations, "\n)

    ## Determine the corresponding weighting for each station
    user_receive <- readline(prompt = cat("The following stations will be included in the calibration:", calibration.stations, "\n",
                                          "Please enter the corresponding weighting value for each station (separated by commas)..."))

    calibration.station.weights <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")



    if(sum(as.numeric(calibration.station.weights)) == 1){
      cat("WSC stations will be included as follows:", calibration.stations, calibration.station.weights, "\n")
    } else {
      user_receive <- readline(prompt = cat("Station weights must sum to 1. Please re-enter weight for the following WSC stations (separated by commas):", calibration.stations))


      calibration.station.weights <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")

      if(sum(as.numeric(calibration.station.weights)) != 1){
        cat("Station weights still do not sum to 1. Please re-start the execution process.")

      }

    }
    
    
  } else { 
  
    if(!is.na(Sys.getenv("SHELL", unset = NA))){
      
      cat("The following WSC stations are available for calibration:", stations.included, "\n",
          "Please enter station numbers to include in calibration (separated by commas)...")
      user_receive <- readLines(con = "stdin", 1)
      
      ## Create a character vector of the stations to be included in calibration
      calibration.stations <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")
      
      # cat("You entered...", calibration.stations, "\n")
      
      ## Determine the corresponding weighting for each station
      cat("The following stations will be included in the calibration:", calibration.stations, "\n",
          "Please enter the corresponding weighting value for each station (separated by commas)...")
      
      user_receive <- readLines(con = "stdin", 1)
      
      calibration.station.weights <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")
      
      if(sum(as.numeric(calibration.station.weights)) == 1){
        cat("WSC stations will be included as follows:", calibration.stations, calibration.station.weights, "\n")
      } else {
        
        cat("Station weights must sum to 1. Please re-enter weight for the following WSC stations (separated by commas):", calibration.stations)
        
        user_receive <- readLines(con = "stdin", 1)
        
        calibration.station.weights <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")
        
        if(sum(as.numeric(calibration.station.weights)) != 1){
          cat("Station weights still do not sum to 1. Please re-start the execution process.")
          
        }
        
      }
    
    } 
  }

} else {


  calibration.stations <- stations.included

  calibration.station.weights <- 1


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


