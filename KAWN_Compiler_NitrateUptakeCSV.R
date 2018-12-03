# Michelle C. Kelly
# Last modified: 27 November 2018
#
# This is a script that will calculate autotrophic
# nitrate uptake (Ua-NO3) based on Heffernan and 
# Cohen 2010


#### Load in data ################################
# These CSV files can be downloaded from the 
# StreamPULSE database
eric_data <- 
  read.csv("./Data/StreamPULSE_DownloadedFrom/KS_KANSASREASTLAWRENCE_sensorData.csv",
           header = TRUE)
steve_data <- 
  read.csv("./Data/StreamPULSE_DownloadedFrom/KS_KANSASRFALLLEAF_sensorData.csv", 
           header = TRUE)
desoto_data <- 
  read.csv("./Data/StreamPULSE_DownloadedFrom/KS_KANSASR_sensorData.csv", 
           header = TRUE)
##################################################


#### Prep data for calculations ##################
# Function to setup the data for uptake calculations
# Input: Sensor data frame
# Output: Sensor data frame with added columns for
#         local time, data flagged "Bad Data" removed

prepper <- function(sensor_data){
  # Format datetime column correctly
  sensor_data$DateTime_UTC <- 
    lubridate::ymd_hms(sensor_data$DateTime_UTC)
  # Add column for local time
  sensor_data$DateTime_CST <- 
    lubridate::with_tz(sensor_data$DateTime_UTC, 
                       tzone = "America/Chicago")
  #
  # Remove data that's been flagged as "Bad Data"
  sensor_data$value[sensor_data$flagtype %in% c("Bad Data")] <- NA
  # Remove flagtype and flagcomment columns
  sensor_data$flagtype <- NULL
  sensor_data$flagcomment <- NULL
  # Reshape dataframe from long to wide format
  sensor_data <- tidyr::spread(sensor_data, 
                               key = variable, value = value)
  # Create new dataframe with only data needed for uptake calculation
  sensor_uptake <- sensor_data[c("DateTime_UTC", "DateTime_CST",
                                 "Discharge_m3s", "Nitrate_mgL")]
  # Interpolate gaps in data
  sensor_uptake <- imputeTS::na.interpolation(sensor_uptake)
  # Return new dataframe
  return(sensor_uptake)
}

# Run function
eric_uptake <- prepper(eric_data)
steve_uptake <- prepper(steve_data)
desoto_uptake <- prepper(desoto_data)
##################################################


#### Specify equation used to calculate uptake ###
# If UptakeEqn == 1, calculate uptake using the 
# extrapolated diel method (Heffernan and Cohen 
# 2010, Eqn. 1)
# If UptakeEqn == 2, calculate uptake using the 
# integrated diel method (Heffernan and Cohen 
# 2010, Eqn. 2)
UptakeEqn <- 2
##################################################

#prepped_df <- eric_uptake
#### Calculate uptake using Equation 1 ###########
if (UptakeEqn == 1) {

  # Function to calculate uptake
  # Input: the dataframe spit out by prepper()
  # Output: Calculated NO3 Uptake
  
  HC_Eqn1 <- function(prepped_df){
    
    # Take hourly average nitrate concentration
    avg_df <- 
      prepped_df %>%
      dplyr::group_by(date = lubridate::date(DateTime_CST),
                      hour = lubridate::hour(DateTime_CST)) %>%
      dplyr::summarise(DateTime_CST = unique(floor_date(DateTime_CST, 
                                                        unit = "hours")),
                       Mean.Discharge_m3s = mean(Discharge_m3s),
                       Mean.Nitrate_mgL = mean(Nitrate_mgL))
    
    # Count number of days since start
    avg_df$day <- numeric(nrow(avg_df))
    
    for (i in 1:nrow(avg_df)){
      avg_df$day[i] <- 
        as.numeric(avg_df$date[i] - avg_df$date[1])
    }
    
    # Find maximum nitrate concentration each (local time) day
    avg_df <- 
      avg_df %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(MaxNitrate = max(Mean.Nitrate_mgL))
    
    # Use a static value for river bed area (m^2)
    # Average discharge at desoto = 61.3 m3/s
    # USGS field measuremnents
    # 2018-03-01    Q = 1410 ft3/s      Channel area = 811 ft2
    #                 = 39.93 m3/s                   = 75.34 m2
    # 2018-04-24    Q = 2330 ft3/s      Channel area = 1400 ft2
    #                 = 65.97 m3/s                   = 130.06 m2
    # Approximate river bed area as 130 m2 (for now)
    A_m2 <- 130
    
    # Calculate water velocity [m/s]
    avg_df$v_ms <- avg_df$Mean.Discharge_m3s / A_m2
    
    # Calculate difference betweeen preceeding day's NitrateMax and Nitrate at 
    # time t
    avg_df$DiffNitrate <- numeric(nrow(avg_df))
    
    # Grab the row number of the start of day 1
    start <- which(avg_df$day == 1)[1]
    
    # Loop from day 1 to end
    for(i in start:nrow(avg_df)){
      # Get previous day: current day - 1
      prevday <- avg_df$day[i] - 1
      # Get MaxNitrate during prevday
      MaxNitrate.prev <- unique(avg_df$MaxNitrate[avg_df$day == prevday])
      # Get current Nitrate
      Nitrate <- avg_df$Mean.Nitrate_mgL[i]
      # Calculate absolute value of difference
      avg_df$DiffNitrate[i] <- MaxNitrate.prev - Nitrate
      # Iterate loop
      i <- i + 1
    }
    
    # Sum the differences from t = 0 to t = 24 for each day
    avg_df <- avg_df %>%
      group_by(day) %>%
      mutate(SumDiffNitrate_mgL = sum(DiffNitrate))
    
    # Multiply SumDiff and velocity, and correct for units
    # Q [m3/s] / A [m2] = v [m/s]
    # v [m/s] * [NO3] [mg-N/L] = U [m*mg-N / s*L]
    # U [m*mg-N / s*L] * [1000 L / 1 m3] * 86400 [s / day] = U [mg-N / m2*day]
    avg_df$UaNO3_mgNm2day <- 
      avg_df$SumDiffNitrate_mgL * avg_df$v_ms * 1000 * 86400
    
    # I don't know if this calculation is totally correct. The numbers seem
    # super high.
    #
    # Rename columns
    names(avg_df)[names(avg_df) == "MaxNitrate"] <- "DailyMaxNitrate_mgNL"
    names(avg_df)[names(avg_df) == "v_ms"] <- "Velocity_ms"
    names(avg_df)[names(avg_df) == "DiffNitrate"] <- "DiffNitrate_mgNL"
    avg_df$date <- NULL
    avg_df$hour <- NULL
    avg_df$day <- NULL
    # Return dataframe
    return(avg_df)
  }
  
  # Execute function
  eric_uptake_Eqn1 <- HC_Eqn1(eric_uptake)
  steve_uptake_Eqn1 <- HC_Eqn1(steve_uptake)
  desoto_uptake_Eqn1 <- HC_Eqn1(desoto_uptake)
  
  # Merge calculations into a single dataframe and save to CSV file
  Uptake_Eqn1 <- dplyr::full_join(eric_uptake_Eqn1, steve_uptake_Eqn1, 
                                  by = "DateTime_CST", 
                                  suffix = c(".eric", ".steve"))
  
  Uptake_Eqn1 <- dplyr::full_join(Uptake_Eqn1, desoto_uptake_Eqn1, 
                                  by = "DateTime_CST")
  
  # Cleanup
  names(Uptake_Eqn1)[names(Uptake_Eqn1) == "Mean.Discharge_m3s"] <- 
    "Mean.Discharge_m3s.desoto"
  names(Uptake_Eqn1)[names(Uptake_Eqn1) == "Mean.Nitrate_mgL"] <- 
    "Mean.Nitrate_mgL.desoto"
  names(Uptake_Eqn1)[names(Uptake_Eqn1) == "DailyMaxNitrate_mgNL"] <- 
    "DailyMaxNitrate_mgNL.desoto"
  names(Uptake_Eqn1)[names(Uptake_Eqn1) == "Velocity_ms"] <- 
    "Velocity_ms.desoto"
  names(Uptake_Eqn1)[names(Uptake_Eqn1) == "DiffNitrate_mgNL"] <- 
    "DiffNitrate_mgNL.desoto"
  names(Uptake_Eqn1)[names(Uptake_Eqn1) == "SumDiffNitrate_mgL"] <- 
    "SumDiffNitrate_mgNL.desoto"
  names(Uptake_Eqn1)[names(Uptake_Eqn1) == "UaNO3_mgNm2day"] <- 
    "UaNO3_mgNm2day.desoto"
  
  # Add column to denote during waste release or after
  # During: dates prior to 1 April 2018
  # After: dates post 1 April 2018
  Uptake_Eqn1$ReleaseStatus <- "During"
  Uptake_Eqn1$ReleaseStatus[Uptake_Eqn1$DateTime_CST >= 
                              ymd_hms("2018-04-01 00:00:00", 
                                      tz = "America/Chicago")] <- "After"
  
  # Save dataframe
  write.csv(Uptake_Eqn1, file = "./Outputs/NitrateUptakeResults_Eqn1.csv", 
            row.names = FALSE)
  
}
##################################################


#### Calculate uptake using Equation 2 ###########
if (UptakeEqn == 2){
  # Calculate NO3 uptake using the integrated diel method 
  # (Heffernan and Cohen 2010, Eqn. 2)
  
  # Differences between Eqn. 1 and 2:
  #   - Eqn 2 takes an hourly average of obervations (less
  #     sensitive to variations in Nitrate)
  
  # Input: the dataframe spit out by prepper()
  # Output: Calculated NO3 Uptake
  
  HC_Eqn2 <- function(prepped_df){
    #
    # Take an hourly average Nitrate concentration
    prepped_df <- 
      prepped_df %>%
      dplyr::group_by(DateTime_CST = 
                        lubridate::floor_date(DateTime_CST, unit = "hour")) %>%
      dplyr::summarise(Mean.Discharge_m3s = mean(Discharge_m3s, na.rm = TRUE),
                       Mean.Nitrate_mgL = mean(Nitrate_mgL, na.rm = TRUE))
    #
    # Add date index vector
    # (Based on CST local time)
    # Extract date into new column
    prepped_df$date <- 
      lubridate::date(prepped_df$DateTime_CST)
    # Count number of days since start
    prepped_df$day <- numeric(length = nrow(prepped_df))
    for (i in 1:nrow(prepped_df)){
      prepped_df$day[i] <- 
        as.numeric(prepped_df$date[i] - prepped_df$date[1])
    }
    # Get hour of day, as fraction of 24
    prepped_df$hour <- lubridate::hour(prepped_df$DateTime_CST) / 24
    #
    # Find maximum nitrate concentration each 
    # (local time) day
    prepped_df <- prepped_df %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(MaxNitrate = max(Mean.Nitrate_mgL, na.rm = TRUE))
    #
    # Use a static value for river bed area (m^2)
    # Average discharge at desoto = 61.3 m3/s
    # USGS field measuremnents
    # 2018-03-01    Q = 1410 ft3/s      Channel area = 811 ft2
    #                 = 39.93 m3/s                   = 75.34 m2
    # 2018-04-24    Q = 2330 ft3/s      Channel area = 1400 ft2
    #                 = 65.97 m3/s                   = 130.06 m2
    # Approximate river bed area as 130 m2 (for now)
    A_m2 <- 130
    #
    # Calculate water velocity [m/s]
    prepped_df$v_ms <- prepped_df$Mean.Discharge_m3s/A_m2
    # 
    # Run loop to calculate uptake
    prepped_df$DiffNitrate_mgL <- NA
    # Grab the row number of the start of day 1
    start <- which(prepped_df$day == 1)[1]
    for (i in start:nrow(prepped_df)){
      # Get previous day: current day - 1
      prevday <- prepped_df$day[i] - 1
      # Get MaxNitrate during prevday
      MaxNitrate.prev <- unique(prepped_df$MaxNitrate[prepped_df$day == 
                                                        prevday])
      # Get MaxNitrate during current day
      MaxNitrate <- unique(prepped_df$MaxNitrate[prepped_df$day == 
                                                   prepped_df$day[i]])
      # Get Nitrate at current hour
      Nitrate <- prepped_df$Mean.Nitrate_mgL[i]
      # Calculate DiffNitrate for each hour
      prepped_df$DiffNitrate_mgL[i] <- MaxNitrate.prev * 
                                             (1 - prepped_df$hour[i]) + 
                                             MaxNitrate * prepped_df$hour[i] - 
                                             Nitrate
    }
    #
    # Sum the differences from t = 0 to t = 24 for each day
    results <- prepped_df %>%
      dplyr::group_by(day) %>%
      dplyr::summarise(date = date[1],
                       Mean.Discharge_m3s = mean(Mean.Discharge_m3s, 
                                                 na.rm = TRUE),
                       Mean.Nitrate_mgL = mean(Mean.Nitrate_mgL, na.rm = TRUE),
                       Mean.v_ms = mean(v_ms, na.rm = TRUE), 
                       SumDiffNitrate = sum(DiffNitrate_mgL))
    # Multiply SumDiff by velocity
    # Unit check:
    # [mg / L] * [m / s] = [mg*m / L*s]
    # [mg*m / L*s] * 1000 [L / m3] * 86400 [s / day] * 1 [g] / 1000 [mg]
    results$UaNO3_gNm2day <- 
      results$SumDiffNitrate * results$Mean.v_ms * 86400
    #
    # Clean up dataframe
    names(results)[names(results) == "date"] <- "Date"
    names(results)[names(results) == "Mean.v_ms"] <- "Mean.velocity_ms"
    results$SumDiffNitrate <- NULL
    results$day <- NULL
    # Return dataframe
    return(results)
  }
  
  # Execute function
  eric_uptake_Eqn2 <- HC_Eqn2(eric_uptake)
  steve_uptake_Eqn2 <- HC_Eqn2(steve_uptake)
  desoto_uptake_Eqn2 <- HC_Eqn2(desoto_uptake)
  
  # Merge calculations into a single dataframe and save to CSV file
  Uptake_Eqn2 <- dplyr::full_join(eric_uptake_Eqn2, steve_uptake_Eqn2, 
                                  by = "Date", 
                                  suffix = c(".eric", ".steve"))
  
  Uptake_Eqn2 <- dplyr::full_join(Uptake_Eqn2, desoto_uptake_Eqn2, 
                                  by = "Date")
  
  # Cleanup
  names(Uptake_Eqn2)[names(Uptake_Eqn2) == "Mean.Discharge_m3s"] <- 
    "Mean.Discharge_m3s.desoto"
  names(Uptake_Eqn2)[names(Uptake_Eqn2) == "Mean.Nitrate_mgL"] <- 
    "Mean.Nitrate_mgL.desoto"
  names(Uptake_Eqn2)[names(Uptake_Eqn2) == "DailyMaxNitrate_mgNL"] <- 
    "DailyMaxNitrate_mgNL.desoto"
  names(Uptake_Eqn2)[names(Uptake_Eqn2) == "Mean.velocity_ms"] <- 
    "Mean.velocity_ms.desoto"
  names(Uptake_Eqn2)[names(Uptake_Eqn2) == "DiffNitrate_mgNL"] <- 
    "DiffNitrate_mgNL.desoto"
  names(Uptake_Eqn2)[names(Uptake_Eqn2) == "SumDiffNitrate_mgL"] <- 
    "SumDiffNitrate_mgNL.desoto"
  names(Uptake_Eqn2)[names(Uptake_Eqn2) == "UaNO3_gNm2day"] <- 
    "UaNO3_gNm2day.desoto"
  
  # Add column to denote during waste release or after
  # During: dates prior to 1 April 2018
  # After: dates post 1 April 2018
  Uptake_Eqn2$ReleaseStatus <- "During"
  Uptake_Eqn2$ReleaseStatus[Uptake_Eqn2$Date >= ymd("2018-04-01")] <- "After"
  
  # Save dataframe
  write.csv(Uptake_Eqn2, file = "./Outputs/NitrateUptakeResults_Eqn2.csv", 
            row.names = FALSE)
}

##################################################
