
#### COMPUTING ECEs DURING DEVELOPMENTAL PERIODS ####

#---------- REQUIRED LIBRARIES ----------------

library(dplyr)
library(tidyr)

# ---------- PREPARE DATA ----------------

#Read in climate data and source data (great tits with natal information)


clim <- read.csv("/ENTERYOURDIRECTORY/tempdata_1965to2024.csv", na.strings=c("", "NA"))
rainfalldata <- read.csv("/ENTERYOURDIRECTORY/raindata_1965to2024.csv", na.strings=c("", "NA"))

gtchicks <- read.csv("/ENTERYOURDIRECTORY/wytham_gretichicks_natalinfo.csv", na.strings=c("", "NA"))

# ---------- LINKING CLIMATE DATA TO NATAL DATA USING JULIAN DATES ----------------

gtchicks2<-gtchicks %>% 
  mutate(hd=april_birthhatchdate+90) %>% #to convert april day to january day
  mutate(hd2=formatC(hd, flag=0, width=3)) %>% #to get day into ddd format
  mutate(year_hd2=paste(birthyear, hd2, sep="")) %>% #to create the yyyyddd code for date
  mutate(observation = 1:n())

#yyyyddd codes will be used for linking climate data to developmental periods

clim2 <- clim %>% 
  mutate(
    tmean = (max + min) / 2,  # Daily mean temperature
    trange = max - min        # Daily temperature range
  ) %>% 
  group_by(year) %>% 
  mutate(julday = row_number()) %>% 
  ungroup() %>% 
  mutate(
    julday2 = formatC(julday, flag = "0", width = 3),
    year_jul = paste(year, julday2, sep = "") #to get yyyyddd code for date
  )

rain2 <- rainfalldata %>% 
  group_by(year) %>% 
  mutate(julday = row_number()) %>% 
  ungroup() %>% 
  mutate(
    julday2 = formatC(julday, flag = "0", width = 3),
    year_jul = paste(year, julday2, sep = "") #to get yyyyddd code for date
  )

# ---------- COMPUTING ECEs (using 5% and 1% thresholds) ----------------

# 5% threshold 
clim3_5<-clim2 %>% 
  group_by(month) %>% 
  mutate(meant_month=mean(tmean)) %>% #this is getting a monthly mean temp
  ungroup() %>% 
  mutate(difmean=tmean-meant_month) %>% #deviation of daily mean temp from monthly mean temp
  mutate(q95=quantile(difmean, 0.95)) %>% #thresholds for ece from the distribution of deviations across full study period
  mutate(q05=quantile(difmean, 0.05)) %>% 
  mutate(hot_ece = ifelse(difmean >= q95, 1, 0)) %>% #above 95 is hot ece
  mutate(cold_ece = ifelse(difmean <= q05, 1, 0)) #below 5 is cold ece

# 1% threshold
clim3_1 <- clim2 %>% 
  group_by(month) %>% 
  mutate(meant_month = mean(tmean)) %>%
  ungroup() %>%
  mutate(difmean =tmean-meant_month) %>%
  mutate(q99 = quantile(difmean, 0.99)) %>%
  mutate(q01 = quantile(difmean, 0.01)) %>%
  mutate(hot_ece = ifelse(difmean >= q99, 1, 0)) %>%
  mutate(cold_ece = ifelse(difmean <= q01, 1, 0))

# 5% threshold
rain3_5 <- rain2 %>% 
  group_by(month) %>% 
  mutate(meanrain_month = mean(rainfall)) %>%
  ungroup() %>%
  mutate(dif = rainfall - meanrain_month) %>%
  mutate(q95 = quantile(dif, 0.95)) %>%
  mutate(rain_ece = ifelse(dif >= q95, 1, 0))

# 1% threshold
rain3_1 <- rain2 %>% 
  group_by(month) %>% 
  mutate(meanrain_month = mean(rainfall)) %>%
  ungroup() %>%
  mutate(dif = rainfall - meanrain_month) %>%
  mutate(q99 = quantile(dif, 0.99)) %>%
  mutate(rain_ece = ifelse(dif >= q99, 1, 0))

# ---------- FUNCTIONS FOR ECES DURING SPECIFIC DEVELOPMENTAL PERIODS ----------------

tempfun <- function(df1, df2) {
  results_list <- vector("list", nrow(df1))  # Pre-allocate a list to store results
  
  for (i in 1:nrow(df1)) {
    # Define the start and end of each period
    start1 <- df1$year_hd2[i]
    end1 <- start1 + 7 #hatchling period (hatch day to 7 dph)
    start2 <- end1 + 1 
    end2 <- start2 + 7 #nestling period (8 dph to 15 dph)
    start3 <- end2 + 1
    end3 <- start3 + 6 # 16dph to 22 dph (fledging) -- not used for analysis
    broodid <- df1$broodid[i]
    
    # Calculate metrics for the first period
    period1_data <- df2[df2$year_jul >= start1 & df2$year_jul <= end1, ]
    average_temperature1 <- mean(period1_data$tmean, na.rm = TRUE)
    average_temprange1 <- mean(period1_data$trange, na.rm = TRUE)
    sd_tmean1 <- sd(period1_data$tmean, na.rm = TRUE)
    cv_tmean1 <- (sd_tmean1 / average_temperature1) * 100
    
    num_days1 <- end1 - start1 + 1
    num_days_hot_ece1 <- sum(period1_data$hot_ece, na.rm = TRUE)  
    num_days_cold_ece1 <- sum(period1_data$cold_ece, na.rm = TRUE)
    presence_hot_ece1 <- ifelse(num_days_hot_ece1 > 0, "Yes", "No")
    presence_cold_ece1 <- ifelse(num_days_cold_ece1 > 0, "Yes", "No")
    
    # Calculate metrics for the second period
    period2_data <- df2[df2$year_jul >= start2 & df2$year_jul <= end2, ]
    average_temperature2 <- mean(period2_data$tmean, na.rm = TRUE)
    average_temprange2 <- mean(period2_data$trange, na.rm = TRUE)
    sd_tmean2 <- sd(period2_data$tmean, na.rm = TRUE)
    cv_tmean2 <- (sd_tmean2 / average_temperature2) * 100
    
    num_days2 <- end2 - start2 + 1
    num_days_hot_ece2 <- sum(period2_data$hot_ece, na.rm = TRUE)
    num_days_cold_ece2 <- sum(period2_data$cold_ece, na.rm = TRUE)
    presence_hot_ece2 <- ifelse(num_days_hot_ece2 > 0, "Yes", "No")
    presence_cold_ece2 <- ifelse(num_days_cold_ece2 > 0, "Yes", "No")
    
    # Calculate metrics for the third period
    period3_data <- df2[df2$year_jul >= start3 & df2$year_jul <= end3, ]
    average_temperature3 <- mean(period3_data$tmean, na.rm = TRUE)
    average_temprange3 <- mean(period3_data$trange, na.rm = TRUE)
    sd_tmean3 <- sd(period3_data$tmean, na.rm = TRUE)
    cv_tmean3 <- (sd_tmean3 / average_temperature3) * 100
    
    num_days3 <- end3 - start3 + 1
    num_days_hot_ece3 <- sum(period3_data$hot_ece, na.rm = TRUE)
    num_days_cold_ece3 <- sum(period3_data$cold_ece, na.rm = TRUE)
    presence_hot_ece3 <- ifelse(num_days_hot_ece3 > 0, "Yes", "No")
    presence_cold_ece3 <- ifelse(num_days_cold_ece3 > 0, "Yes", "No")
    
    # Store the results in the list
    results_list[[i]] <- data.frame(
      broodid = broodid,
      average_temperature_per1 = average_temperature1,
      average_temprange_per1 = average_temprange1,
      sd_temp_per1 = sd_tmean1,
      cv_meantemp_per1 = cv_tmean1,
      Num_Days_per1 = num_days1,
      num_days_hot_ece1 = num_days_hot_ece1,
      num_days_cold_ece1 = num_days_cold_ece1,
      presence_hot_ece1 = presence_hot_ece1,
      presence_cold_ece1 = presence_cold_ece1,
      
      average_temperature_per2 = average_temperature2,
      average_temprange_per2 = average_temprange2,
      sd_temp_per2 = sd_tmean2,
      cv_meantemp_per2 = cv_tmean2,
      Num_Days_per2 = num_days2,
      num_days_hot_ece2 = num_days_hot_ece2,
      num_days_cold_ece2 = num_days_cold_ece2,
      presence_hot_ece2 = presence_hot_ece2,
      presence_cold_ece2 = presence_cold_ece2,
      
      average_temperature_per3 = average_temperature3,
      average_temprange_per3 = average_temprange3,
      sd_temp_per3 = sd_tmean3,
      cv_meantemp_per3 = cv_tmean3,
      Num_Days_per3 = num_days3,
      num_days_hot_ece3 = num_days_hot_ece3,
      num_days_cold_ece3 = num_days_cold_ece3,
      presence_hot_ece3 = presence_hot_ece3,
      presence_cold_ece3 = presence_cold_ece3
    )
  }
  
  # Combine all results into a single dataframe
  result <- do.call(rbind, results_list)
  
  return(result)
}

#same function for rain 

rainfun <- function(df1, df2) {
  results_list <- vector("list", nrow(df1))  # Pre-allocate a list to store results
  
  for (i in 1:nrow(df1)) {
    # Define the start and end of each period
    start1 <- df1$year_hd2[i]
    end1 <- start1 + 7
    start2 <- end1 + 1
    end2 <- start2 + 7
    start3 <- end2 + 1
    end3 <- start3 + 6
    broodid <- df1$broodid[i]
    
    # Calculate metrics for the first period
    period1_data <- df2[df2$year_jul >= start1 & df2$year_jul <= end1, ]
    average_rainfall1 <- mean(period1_data$rainfall, na.rm = TRUE)
    num_days_rain_ece1 <- sum(period1_data$rain_ece, na.rm = TRUE)
    presence_rain_ece1 <- ifelse(num_days_rain_ece1 > 0, "Yes", "No")
    
    # Calculate metrics for the second period
    period2_data <- df2[df2$year_jul >= start2 & df2$year_jul <= end2, ]
    average_rainfall2 <- mean(period2_data$rainfall, na.rm = TRUE)
    num_days_rain_ece2 <- sum(period2_data$rain_ece, na.rm = TRUE)
    presence_rain_ece2 <- ifelse(num_days_rain_ece2 > 0, "Yes", "No")
    
    # Calculate metrics for the third period
    period3_data <- df2[df2$year_jul >= start3 & df2$year_jul <= end3, ]
    average_rainfall3 <- mean(period3_data$rainfall, na.rm = TRUE)
    num_days_rain_ece3 <- sum(period3_data$rain_ece, na.rm = TRUE)
    presence_rain_ece3 <- ifelse(num_days_rain_ece3 > 0, "Yes", "No")
    
    # Store the results in the list
    results_list[[i]] <- data.frame(
      broodid = broodid,
      average_rainfall_per1 = average_rainfall1,
      num_days_rain_ece1 = num_days_rain_ece1,
      presence_rain_ece1 = presence_rain_ece1,
      
      average_rainfall_per2 = average_rainfall2,
      num_days_rain_ece2 = num_days_rain_ece2,
      presence_rain_ece2 = presence_rain_ece2,
      
      average_rainfall_per3 = average_rainfall3,
      num_days_rain_ece3 = num_days_rain_ece3,
      presence_rain_ece3 = presence_rain_ece3
    )
  }
  
  # Combine all results into a single dataframe
  result <- do.call(rbind, results_list)
  
  return(result)
}

# ---------- APPLYING FUNCTIONS ----------------

gtchicks2$year_hd2<-as.numeric(gtchicks2$year_hd2)
gtchicks2_uniquepnum <- gtchicks2 %>% distinct(broodid, .keep_all = TRUE) #13209 obs

# For temperature 5%
hotchicks_5 <- as.data.frame(tempfun(gtchicks2_uniquepnum, clim3_5))

# For temperature 1%
hotchicks_1 <- as.data.frame(tempfun(gtchicks2_uniquepnum, clim3_1))

# For rainfall 5%
rainchicks_5 <- as.data.frame(rainfun(gtchicks2_uniquepnum, rain3_5))

# For rainfall 1%
rainchicks_1 <- as.data.frame(rainfun(gtchicks2_uniquepnum, rain3_1))

hotchicksfull_5 <- left_join(gtchicks2, hotchicks_5, by="broodid")
hotchicksfull_1 <- left_join(gtchicks2, hotchicks_1, by="broodid")
rainchicksfull_5 <- left_join(gtchicks2, rainchicks_5, by="broodid")
rainchicksfull_1 <- left_join(gtchicks2, rainchicks_1, by="broodid")

#for a combined dataset with both 5% and 1% events and selecting only hatchling and nestling period metrics for analysis

analysis_data <- gtchicks2 %>%
  left_join(
    hotchicks_5 %>% select(
      broodid, num_days_hot_ece1, num_days_cold_ece1, num_days_hot_ece2, num_days_cold_ece2, average_temperature_per1, average_temperature_per2, average_temprange_per1, average_temprange_per2
    ), by = "broodid"
  ) %>%
  rename(
    hot_ece1_5 = num_days_hot_ece1,
    cold_ece1_5 = num_days_cold_ece1,
    hot_ece2_5 = num_days_hot_ece2,
    cold_ece2_5 = num_days_cold_ece2
  ) %>%
  left_join(
    hotchicks_1 %>% select(
      broodid, num_days_hot_ece1, num_days_cold_ece1, num_days_hot_ece2, num_days_cold_ece2
    ), by = "broodid"
  ) %>%
  rename(
    hot_ece1_1 = num_days_hot_ece1,
    cold_ece1_1 = num_days_cold_ece1,
    hot_ece2_1 = num_days_hot_ece2,
    cold_ece2_1 = num_days_cold_ece2
  ) %>%
  left_join(
    rainchicks_5 %>% select(
      broodid, num_days_rain_ece1, num_days_rain_ece2, average_rainfall_per1, average_rainfall_per2
    ), by = "broodid"
  ) %>%
  rename(
    rain_ece1_5 = num_days_rain_ece1,
    rain_ece2_5 = num_days_rain_ece2
  ) %>%
  left_join(
    rainchicks_1 %>% select(
      broodid, num_days_rain_ece1, num_days_rain_ece2
    ), by = "broodid"
  ) %>%
  rename(
    rain_ece1_1 = num_days_rain_ece1,
    rain_ece2_1 = num_days_rain_ece2
  )

write.csv(analysis_data,
          file = file.path("/ENTERYOURDIRECTORY/chickweight_ECEdata_5perc1perc.csv"), row.names = F
)

#### chickweight_ECEdata_5perc1perc.csv is used for models ####



