require(stringr)


ost.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/OST-Template.csv")

available.response.vars <- as.character(ost.template[ost.template$VARIABLE == "Response", "DEFINITION"])


## For now, only include naturalized streamflow datasets as targets for calibration (and not for validation) - this does not respect the calibration/validation weights.
if(validate.model == FALSE){

  ## Append naturalized flow datasets to WSC stations included as possible calibration targets (with the exception of Vernon Creek since no dataset is available here).
  stations.included <- c(stations.included, paste(include.watersheds, "_Nat_Q", sep = ""))
  
  ## If custom calibration targets are available, add them to the stations.included object
  if(exists("custom.calibration.targets")){
  
  stations.included <- c(stations.included, custom.calibration.targets)
  
  }
  ## If Vernon is included, write an extra target as the outlet of Kal Lake.
  if("Vernon" %in% include.watersheds){
    stations.included <- c(stations.included, "Vernon_Kal_Nat_Q")
    
  }
  
}



if(length(stations.included) > 1){

  if(!is.na(Sys.getenv("RSTUDIO", unset = NA))){
    
    ## Determine which WSC stations should be included in the calibration

    user_receive <- readline(prompt = cat("The following WSC stations are available for calibration:", paste(stations.included, collapse = " ; "), "\n",
                                        "Please enter station numbers to include in calibration (separated by commas)..."))

  
    ## Create a character vector of the stations to be included in calibration
    calibration.stations <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")

    ## Print out your entry
    # cat("You entered...", calibration.stations, "\n)

    ## Determine the corresponding weighting for each station
    user_receive <- readline(prompt = cat("The following stations will be included in the calibration:", paste(calibration.stations, collapse = " ; "), "\n",
                                          "Please enter the corresponding weighting value for each station (separated by commas)..."))

    calibration.station.weights <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")

    
    ## Determine which Response Variable to include
    user_receive <- readline(prompt = cat("The following Response Variables are available to be optimized through calibration:", paste(available.response.vars, collapse = " ; "), "\n",
                                          "Please enter the corresponding weighting value for each Response Variable (separated by commas)..."))
    
    response.variable.weights <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")

    # response.variables <- matrix(c(available.response.vars, response.variable.weights), nrow = length(available.response.vars))
    
    # response.variables <- as.matrix(response.variables[response.variables[,2] != 0, ], byrow = FALSE)
    

    if(sum(as.numeric(calibration.station.weights)) == 1 & sum(as.numeric(response.variable.weights)) == 1
       & length(calibration.stations) == length(calibration.station.weights)
       & length(available.response.vars) == length(response.variable.weights)){
      cat("WSC stations will be included as follows:", calibration.stations, calibration.station.weights, "\n",
          "Response Variables will be included as follows:", available.response.vars, response.variable.weights, "\n")
    } else {
      stop("Either Station Weights and/or Response Variable weights do not sum to 1, or the length of the weights provided is incorrect. Please restart the calibration process.")
    }
    
    
  } else { 
  
    if(!is.na(Sys.getenv("SHELL", unset = NA))){
      
      cat("The following WSC stations are available for calibration:", paste(stations.included, collapse = " ; "), "\n",
          "Please enter station numbers to include in calibration (separated by commas)...")
      user_receive <- readLines(con = "stdin", 1)
      
      ## Create a character vector of the stations to be included in calibration
      calibration.stations <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")
      
      # cat("You entered...", calibration.stations, "\n")
      
      ## Determine the corresponding weighting for each station
      cat("The following stations will be included in the calibration:", paste(calibration.stations, collapse = " ; "), "\n",
          "Please enter the corresponding weighting value for each station (separated by commas)...")
      
      user_receive <- readLines(con = "stdin", 1)
      
      calibration.station.weights <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")
      
      
      
      ## Determine which Response Variable to include
     cat("The following Response Variables are available to be optimized through calibration:", paste(available.response.vars, " ; "), "\n",
          "Please enter the corresponding weighting value for each Response Variable (separated by commas)...")
      
      response.variable.weights <- str_trim(strsplit(user_receive, ",")[[1]], side = "both")
      
      # response.variables <- matrix(c(available.response.vars, response.variable.weights), nrow = length(available.response.vars))
      
      # response.variables <- response.variables[response.variables[,2] != 0, ]
      
      
      if(sum(as.numeric(calibration.station.weights)) == 1 & sum(as.numeric(response.variable.weights)) == 1
         & length(calibration.stations) == length(calibration.station.weights)
         & length(available.response.vars) == length(response.variable.weights)){
        cat("WSC stations will be included as follows:", calibration.stations, calibration.station.weights, "\n",
            "Response Variables will be included as follows:", available.response.vars, response.variable.weights, "\n")
      } else {
        stop("Either Station Weights and/or Response Variable weights do not sum to 1, or the length of the weights provided is incorrect. Please restart the calibration process.")
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


