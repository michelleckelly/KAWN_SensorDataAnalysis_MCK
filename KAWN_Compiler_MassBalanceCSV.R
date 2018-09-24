# this is a script that will load in discharge, water chemistry, and 
# sensor data and output a formatted csv file with calculated
# N loads to the river. it is called inside KAWN_Sensors_MassBalance.Rmd
########################################################################

# load in discharge data

# time period: 1 Jan 18 - 15 Jun 18
# load in USGS discharge data, units are [m^3 s^-1]
discharge <- read.csv(file = "./Data/SensorData_CompiledFiles/KAWN_USGSDischarge_dailymeans.csv")

# because of the water balance exercise, desoto data is negative, change the sign
discharge$Q5.desoto <- -1 * discharge$Q5.desoto
# ditch the difference column (used in the water balance exercise)
discharge$difference <- NULL

# rename columns to be more meaningful
names(discharge) <- c("date", "Q.lawrence_m3s", "Q.wakarusa_m3s", 
                      "Q.stranger_m3s", "Q.desoto_m3s", "Q.farmland_m3s")
# make sure date is correct format
discharge$date <- lubridate::mdy(discharge$date)

# after 03-30, farmland stops pumping, change Q to 0
discharge$Q.farmland_m3s[discharge$date >= mdy("03-30-2018")] <- 0

# convert [m^3 s^-1] to [L s^-1]
m3s_Ls <- function(x){
  x*1000
}

discharge$Q.lawrence_Ls <- m3s_Ls(discharge$Q.lawrence_m3s)
discharge$Q.wakarusa_Ls <- m3s_Ls(discharge$Q.wakarusa_m3s)
discharge$Q.stranger_Ls <- m3s_Ls(discharge$Q.stranger_m3s)
discharge$Q.desoto_Ls <- m3s_Ls(discharge$Q.desoto_m3s)
discharge$Q.farmland_Ls <- m3s_Ls(discharge$Q.farmland_m3s)

discharge$Q.lawrence_m3s <- NULL
discharge$Q.wakarusa_m3s <- NULL
discharge$Q.stranger_m3s <- NULL
discharge$Q.desoto_m3s <- NULL
discharge$Q.farmland_m3s <- NULL

# load in and format the nitrogen data

# Farmland NO3+NO2, NH3 --------------------------------------------------
# load in data provided by city
farmland <- read.csv(file = "./Data/SensorData_CompiledFiles/FarmlandDailyReport_CityofLawrence_MCK.csv")
farmland <- data.frame(date = lubridate::mdy(farmland$Date),
                       Nitrate.farmland_mgNL = farmland$OutflowWier_Farmland001A1_NitrateNitrite_mgN.L,
                       Ammonia.farmland_mgNL = farmland$OutflowWier_Farmland001A1_Ammonia_mgN.L)

# take data from only 1-Jan onwards
farmland <- farmland[farmland$date >= lubridate::mdy("1-1-2018"),]
# linearlly interpolate missing concentration values
# the point of this is to get an estimation of concentrations on weekends
# that pumping was done but samples were not taken. On dates that no pumping
# was done, a Q = 0 will be multiplied by these concentrations and they won't
# go into the N balance
farmland$Nitrate.farmland_mgNL <- imputeTS::na.interpolation(farmland$Nitrate.farmland_mgNL)
farmland$Ammonia.farmland_mgNL <- imputeTS::na.interpolation(farmland$Ammonia.farmland_mgNL)

# Bowersock NO3+NO2 -------------------------------------------------------
bowersock <- read.csv(file = "./Data/SensorData_CompiledFiles/KAWN_SensorData_Bowersock.csv")
# take average for each day
bowersock$dateTime <- lubridate::mdy_hm(bowersock$dateTime)
bowersock <- bowersock %>%
  dplyr::group_by(date = lubridate::date(dateTime)) %>%
  dplyr::summarise(Nitrate.bowersock_mgNL = mean(nitrateNitrite, na.rm = TRUE))

# Desoto, Steve, and Eric NO3+NO2 -----------------------------------------
# load in sensor data
desoto <- read.csv(file = "./Data/SensorData_StreamPULSE_Downloaded/KS_KANSASR_sensorData.csv")
steve <- read.csv(file = "./Data/SensorData_StreamPULSE_Downloaded/KS_KANSASRFALLLEAF_sensorData.csv")
eric <- read.csv(file = "./Data/SensorData_StreamPULSE_Downloaded/KS_KANSASREASTLAWRENCE_sensorData.csv")

# summarize sensor data: take daily averages
dailyavg <- function(sensor_data){
  # if sensor data has a "bad data" flag, set value to NA
  sensor_data$value[sensor_data$flagtype %in% c("Bad Data")] <- NA
  # reshape sensor data from long format to wide format
  sensor_data <- tidyr::spread(sensor_data, key = variable, value = value)
  # reformat datetime
  sensor_data$DateTime_UTC <- lubridate::mdy_hm(sensor_data$DateTime_UTC)
  # change time zone from UTC to central
  sensor_data$dateTime <- lubridate::with_tz(sensor_data$DateTime_UTC, tzone = "America/Chicago")
  # sort data file and find daily averages
  sensor_data <- sensor_data %>%
    dplyr::group_by(date = lubridate::date(dateTime)) %>% 
    dplyr::summarise(Nitrate_mgL.mean = mean(Nitrate_mgL, na.rm = TRUE))
  return(sensor_data)
}

desoto <- dailyavg(desoto)
steve <- dailyavg(steve)
eric <- dailyavg(eric)
names(desoto)[2] <- "Nitrate.desoto_mgNL"
names(steve)[2] <- "Nitrate.steve_mgNL"
names(eric)[2] <- "Nitrate.eric_mgNL"

# Desoto, Steve, Eric, Bowersock NH3 and NH4 -------------------------------------
NH3_NH4 <- read.csv(file = "./Data/SensorData_CompiledFiles/KAWN_Float_NH3_NH4.csv")
# for future reference: NH3_NH4 also has data for Eudora and WWTP sites

# fix date format
NH3_NH4$date <- lubridate::mdy(NH3_NH4$date)
# seperate out each site from NH3_NH4 and merge with site dataframe
desoto <- dplyr::full_join(desoto, NH3_NH4 %>% dplyr::filter(site == "Desoto"))
desoto$site <- NULL
colnames(desoto)[3] <- c("Ammonium.desoto_mgNL")

steve <- dplyr::full_join(steve, NH3_NH4 %>% dplyr::filter(site == "Steve"))
steve$site <- NULL
colnames(steve)[3] <- c("Ammonium.steve_mgNL")

eric <- dplyr::full_join(eric, NH3_NH4 %>% dplyr::filter(site == "Eric"))
eric$site <- NULL
colnames(eric)[3] <- c("Ammonium.eric_mgNL")

bowersock <- dplyr::full_join(bowersock, NH3_NH4 %>% dplyr::filter(site == "Bowersock"))
bowersock$site <- NULL
colnames(bowersock)[3] <- c("Ammonium.bowersock_mgNL")

# merge discharge, farmland, desoto, eric, steve, bowersock
massBal <- merge(discharge, farmland, by = "date", all = TRUE)
massBal <- merge(massBal, bowersock, by = "date", all = TRUE)
massBal <- merge(massBal, desoto, by = "date", all = TRUE)
massBal <- merge(massBal, steve, by = "date", all = TRUE)
massBal <- merge(massBal, eric, by = "date", all = TRUE)

# bowersock record has three days of missing nitrate data
# use imputeTS::na.interpolate() to fill these via linear
# interpolation
# interpolate to fill 2 days worth of missing nitrate data, so we can match this up
# with float sample
massBal$Nitrate.bowersock_mgNL[massBal$date >= ymd("2018-02-14") & 
                                 massBal$date <= ymd("2018-02-18")] <-
  imputeTS::na.interpolation(massBal$Nitrate.bowersock_mgNL
                             [massBal$date >= ymd("2018-02-14") & 
                                 massBal$date <= ymd("2018-02-18")])
# set NA's to zero
# if NA's aren't set to zero, the division step in the
# mass load calculations won't work
#massBal[is.na(massBal)] <- 0

# mass load     m [mg s-1] = Q [L s-1] * [NO3 as N] [mg L-1]
# conversion    [kg day-1] = [mg s-1] / 1000 [g/mg] / 1000 [kg/g] 
#                                 * 60 [s/min] * 60 [min/hr] * 24 [hr/day]

# nitrate load calculations
massBal$NitrateLoad.farmland_kgNday <- 
  massBal$Q.farmland_Ls * massBal$Nitrate.farmland_mgNL / 1000 / 1000 * 60 * 60 * 24
massBal$NitrateLoad.bowersock_kgNday <- 
  massBal$Q.lawrence_Ls * massBal$Nitrate.bowersock_mgNL / 1000 / 1000 * 60 * 60 * 24
massBal$NitrateLoad.desoto_kgNday <- 
  massBal$Q.desoto_Ls * massBal$Nitrate.desoto_mgNL / 1000 / 1000 * 60 * 60 * 24
massBal$NitrateLoad.steve_kgNday <- 
  massBal$Q.lawrence_Ls * massBal$Nitrate.steve_mgNL / 1000 / 1000 * 60 * 60 * 24
massBal$NitrateLoad.eric_kgNday <- 
  massBal$Q.lawrence_Ls * massBal$Nitrate.eric_mgNL / 1000 / 1000 * 60 * 60 * 24

# ammonium load calculations
massBal$AmmoniumLoad.bowersock_kgNday <- 
  massBal$Q.lawrence_Ls * massBal$Ammonium.bowersock_mgNL / 1000 / 1000 * 60 * 60 * 24
massBal$AmmoniumLoad.desoto_kgNday <- 
  massBal$Q.desoto_Ls * massBal$Ammonium.desoto_mgNL / 1000 / 1000 * 60 * 60 * 24
massBal$AmmoniumLoad.steve_kgNday <- 
  massBal$Q.lawrence_Ls * massBal$Ammonium.steve_mgNL / 1000 / 1000 * 60 * 60 * 24
massBal$AmmoniumLoad.eric_kgNday <- 
  massBal$Q.lawrence_Ls * massBal$Ammonium.eric_mgNL / 1000 / 1000 * 60 * 60 * 24

# DIN = nitrate + nitrite + ammonium
# calculate DIN by extrapolating values of NH4, then adding extrapolated
# NH4 readings to NO3+NO2 readings
massBal$DINLoad.bowersock_kgNday <- 
  massBal$NitrateLoad.bowersock_kgNday + 
  imputeTS::na.interpolation(massBal$AmmoniumLoad.bowersock_kgNday)
massBal$DINLoad.desoto_kgNday <- 
  massBal$NitrateLoad.desoto_kgNday + 
  imputeTS::na.interpolation(massBal$AmmoniumLoad.desoto_kgNday)
massBal$DINLoad.steve_kgNday <- 
  massBal$NitrateLoad.steve_kgNday + 
  imputeTS::na.interpolation(massBal$AmmoniumLoad.steve_kgNday)
massBal$DINLoad.eric_kgNday <- 
  massBal$NitrateLoad.eric_kgNday + 
  imputeTS::na.interpolation(massBal$AmmoniumLoad.eric_kgNday)

# ammonia load calculations
massBal$AmmoniaLoad.farmland_kgNday <- 
  massBal$Q.farmland_Ls * massBal$Ammonia.farmland_mgNL / 1000 / 1000 * 60 * 60 * 24

# trunkate the data at May 1
massBal <- massBal[massBal$date <= "2018-05-01",]

# export massBal dataframe
write.csv(massBal, file = "./Outputs/NMassBalance.csv", row.names = FALSE)