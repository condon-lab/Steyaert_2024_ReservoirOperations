# Jen Steyaert
# Data Paper 2 Whole Code
# 10-11-2021


library(readr)
library(tidyverse)
library(lubridate)
library(reshape)
library(ggplot2)
library(Hmisc)
library(zoo)
library(dplyr)
library(gridExtra)
library(RColorBrewer)

# Directories needed
attributes_resops <- "~/Desktop/ResOpsUS/attributes"
other_data_directory <- "~/Desktop/github/ResOpsUS_Analysis/data/other_data"
ts_all_directory <- "~/Desktop/ResOpsUS/time_series_all"
daily_data_save <- "~/Desktop/github/ResOpsUS_Analysis/data/HUC_FF/daily_ff"
monthly_data_save <- "~/Desktop/github/ResOpsUS_Analysis/data/HUC_FF/monthly_ff"
yearly_data_save <- "~/Desktop/github/ResOpsUS_Analysis/data/HUC_FF/yearly_ff"

# 1.  Read in GRanD matched to HUC2
# This contains the dam attributes from GRanD that have been joined to the HUC2 boundaries 

setwd(other_data_directory) # may need to change depending on where you saved this
HUC2 <- read_csv(file = "HUC2_CONUS_Res.csv")
HUC2$YEAR_CREATED[HUC2$YEAR_CREATED<1] <- NA

# Read in SPI averages by HUC2 starting in 1950 and going to 2012. Each HUC is a column. 
# SPI data is downloaded from NCAR at https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi
# SPI values were then spatially aggregated by HUC2 in QGIS

SPI <- read.csv(file="SPI_Water_Year_Avg_HUC2.csv", stringsAsFactors = FALSE)

# Read in Reservoir Attributes from ResOpsUS
setwd(attributes_resops)
attributes<- read.csv(file = "reservoir_attributes.csv", stringsAsFactors = FALSE)

# merge HUC2 with the reservoir attribute data
huc2_res <- merge(attributes, HUC2[, c(1,13,29,49)], by = "DAM_ID")

huc2_list <- c(3,5,6,7,8,9,10,11,12,14,15,16,17,18) # list of the HUC2's that have more than 50% storage covered in the dataset


# list of dams where the storage is greater than the storage capacity in GRanD
max_cap_wrong <- c(625,609,595,223,179,178,141,137,132,131,113,111,405,305,299,450,442,601,597,567,549,542,541,502,492,473,470,
                   469,451,423,1163,1119,780,784,893,898,952,956,962,978,481,415,393,373,362,1125,758,1896,481,415,393,373,362,1125,758,1896,
                   1238,310,311,182,185,648,7308,1863,7313,1801,1781,1774,1770,1777,1762,1756,1752,1910,1804, 375,377,378,
                   172,386,133,382,374,885,1005,1063,413,367,361,350,345,1081,1095, 605, 507, 976)

### Big For loop that Calculates the Fraction Filled and storage covered by HUC #####
for (m in 1:length(huc2_list)){
  
  huc2 <- huc2_list[m] # pull out the huc2 number
  print(huc2)
  all_dams <- HUC2 %>% filter(join_HUC2 ==huc2) # all the dams that exist in GRanD for this region
  filtered_res_att <- huc2_res %>% filter(join_HUC2_db == huc2)# filter out all the dams that are in this huc2
  dam_ids <- filtered_res_att$DAM_ID # all the dam ID's we have data for
  
  
###### 1. Combine all storage records into one file (column names = dam name, row names = date) #####
  max_cap_matrix <- matrix(data = NA, nrow = length(dam_ids), ncol = 2)
  
  for (l in 1:length(dam_ids)) { # loop through the dam_ids to pull all the data together
    dam <- dam_ids[l]
    file_name <- paste0("ResOpsUS_",dam, ".csv")
    setwd(ts_all_directory)
    res_data <- read.csv(file = file_name, stringsAsFactors = FALSE)

    
    # 2. Go through and make sure we have the right maximum capacity for all dams in the dataset
    # Make sure the Max does not go above this or below 0

      # Replace maximum values with maximum storage capacity
      max_cap_index <- which(dam == huc2_res$DAM_ID)
      max_cap <- huc2_res[max_cap_index, 10]
      if(is.na(max_cap) == TRUE| dam %in% max_cap_wrong ){
        max_cap = max(res_data$storage,na.rm =T)
      }
      if (dam == 137 | dam ==133 | dam ==1896 | dam == 1910 | dam == 1081){
        res_data_dam <- res_data %>% filter(date > 1999) %>% filter(date < 2011)
        max_cap = max(res_data_dam$storage,na.rm =T)
      }
      if (dam == 952  | dam == 179 ){
        res_data_dam <- res_data %>% filter(date > 1999) 
        max_cap = max(res_data_dam$storage,na.rm =T)
      }
      if (dam == 784 ){
        res_data_dam <- res_data %>% filter(date > 1989) %>% filter(date < 1996)
        max_cap = max(res_data_dam$storage,na.rm =T)
      }
      if (dam == 1119| dam == 223 ){
        res_data_dam <- res_data %>% filter(date > 2009) 
        max(res_data_dam$storage,na.rm =T)
      }
      if (dam == 7311 | dam == 7317  | dam == 7306 | dam == 7214 | dam == 7318 ){ # for the list of dams where Max capacity is 0
        max_cap_index <- which(dam == all_dams$DAM_ID)
        max_cap <- all_dams[max_cap_index, 12]
      }
      if(max_cap == 0){
        max_cap_index <- which(dam == all_dams$DAM_ID)
        max_cap <- all_dams[max_cap_index, 12] 
      }
      res_data$storage[res_data$storage > max_cap] <- max_cap
      
      # Replace negative values with NA
      res_data$storage[res_data$storage < 0] <- NA
      max_cap_matrix[l,1] <- dam
      max_cap_matrix[l,2] <- as.numeric(max_cap)
    
    colnames(res_data) <- c("date", paste0(dam_ids[l], "_value"), "inflow", "outflow", "elevation", "evaporation")
    res_data$date <- as.Date(res_data$date)
    res_data[,2] <- as.numeric(res_data[,2])
    
    if(l ==1){
      all_huc2 <- res_data[,1:2]
     
    }else{
      all_huc2 <- merge(all_huc2, res_data[,1:2], by = "date", all= T)
      
    }# (l ==1)
  } # for (l in 1:length(dam_ids))
  
 max_cap_df <- data.frame("dam_id" = max_cap_matrix[,1], "max_cap" = max_cap_matrix[,2]) 
  print("Finished Maximum Capacity Matrix")
 
####### 2. Make Storage Cap Not NA for Dams We Have Data For (ie Fill in Gaps in GRanD) #######
  
  ## set up data frame to hold storage capacity for GRanD and ResOpsUS in order to create Storage Covered over time
  all_years1 <- unique(sort(all_dams$YEAR_CREATED))
  all_years <- seq(all_years1[1], 2021, by =1)
  storage_covered <- data.frame(all_years)
  storage_covered$ResOpsUS<- NA
  storage_covered$storage_cap_GRanD <- NA
  
  
  for (p in 1:nrow(all_dams)){
    dam_id <- as.numeric(all_dams[p, 1])
    col_index <- which(grepl(dam_id, colnames(all_huc2)) == TRUE)
    dam_value = which(dam_id == max_cap_df$dam_id)
    if(dam_id %in% dam_ids){
      test_max_cap <- max_cap_df[dam_value,2]
      if(test_max_cap != all_dams[p,13] && is.na(test_max_cap) == F && test_max_cap != "-Inf"){ # if the test value does not equal the maximum capacity value
        all_dams[p,13] <- test_max_cap
      }
      if( is.na(test_max_cap) == T && all_dams[p,13] ==0 | test_max_cap == "Inf"){
        all_dams[p,13] <- all_dams[p,12]
    }
    }
  }
   print("Finished Storage Capacity")
   
 
   filtered_res_att <- huc2_res %>% filter(join_HUC2_db == huc2)# filter out all the dams that are in this huc2

   ###### 3. Linearly interpolate data to fill in gaps#######
    interpolated <- all_huc2 %>% 
    mutate_at(vars(matches("_value")), na.approx, na.rm=FALSE) %>%  # mutate at all the variables that have _value in their name, use na.approximate function and don't remove NA's
    as.data.frame() # make the output a data frame
  
  ###### 4. Pull out all start Dates for the Dams#####
  start_date_matrix<- as.data.frame(dam_ids)
  start_date_matrix$start <- NA
  start_date_matrix$end_date <- NA
  
  for (j in 1:length(dam_ids)){
    dam_id <- dam_ids[j] # pull out first dam_id
    start_year <- filtered_res_att[j,11] # pull out the built year
    column_index <- which(grepl(dam_id, colnames(all_huc2)) == TRUE)
    
    # Find the starting date
    counter =1
    while (is.na(all_huc2[counter,column_index]) == TRUE && counter <= nrow(all_huc2)){
      counter = counter +1 # counter will tell us what the row number is
    }
    
    date_start <- (all_huc2[counter, 1])
    start_date_matrix[j,2] <- year(date_start)
    inverse_counter =nrow(all_huc2)
    
    # Find the end date for the data
    while (is.na(all_huc2[inverse_counter,column_index]) == TRUE && inverse_counter >0){
      inverse_counter = inverse_counter -1 # counter will tell us what the row number is
    }
    
    date_end <- (all_huc2[inverse_counter, 1])
    if (length(date_end) ==0){
      start_date_matrix[j,3] <- NA
    }else{
      start_date_matrix[j,3] <- year(date_end) # add end date to the dates matrix
    }
  } # for (j in 1:length(dam_ids)
    
   
  colnames(start_date_matrix) <- c("DAM_ID", "start", "end_date")
  start_date_matrix <- merge(start_date_matrix, all_dams[,c(1,13)], by ="DAM_ID")
  years_using1 <- unique(sort(start_date_matrix$start))
  years_using <-seq(years_using1[1],2021, by =1)
  
  ###### 5. Fix Dates so that the Start Date in Data is NEVER Before the Start Date in GRanD ######
  for (p in 1:length(start_date_matrix$DAM_ID)){
    dam_id <- start_date_matrix[p,1]
    itemp <- which(dam_id == all_dams$DAM_ID)
    year_GRanD <- all_dams[itemp, 29]
    year_data <- start_date_matrix[p,2]
    if (is.na(year_data) == TRUE){ # if there is no storage data then skip this one because we will have to trust GRanD's build date
      next
    }
    if (year_data < year_GRanD| is.na(year_GRanD) == TRUE){
      all_dams[itemp, 29] <- year_data
    }
  }
   print("Finished Dates")
  
  #### 6. Loop through and calculate the total storage cap for each dam in the HUC2 in GRanD  ##### 
  for (k in 1:length(all_years)){
    year_1 <- all_years[k]
    stor_cap_index <- which( as.numeric(all_dams$YEAR_CREATED)== year_1 )
    previous <- k-1
    
    if(length(stor_cap_index) ==0){ # if no year exists please use the previous storage capacity
      storage_cap = storage_covered[previous,3]
    }
    values <- c()
    if (length(stor_cap_index) ==1){ # if this is equal to one then the storage cap is just that number
      storage_cap <- as.numeric(all_dams[stor_cap_index,13])
      
    }
    if(length(stor_cap_index)>1){ # if you have more than one matching year in this region , loop through to calculate
      for (p in 1:length(stor_cap_index)){
        index <- stor_cap_index[p]
        storage_cap <- as.numeric(all_dams[index,13])
        values <- append(values,storage_cap)
      }#  for (p in 1:length(stor_cap_index))
      storage_cap <- sum(values, na.rm =TRUE)
    }#if (length(stor_cap_index) ==1)

    # Code to iteratively add the dam data in as we move forward
    if (k ==1){
      storage_covered[k,3] <- storage_cap
    }#(k ==1){

    if (k >1 && length(stor_cap_index) >0){
      storage_covered[k,3] <- sum(storage_cap, as.numeric(storage_covered[previous,3]), na.rm = TRUE)
    }
    if (k>1 && length(stor_cap_index) == 0){
      storage_covered[k,3] <- storage_cap
    }
  }

  print("Storage Capacity Graph Done")
  ### need to make table with col names = stor_1, stor_cap_1, ..... stor_n, stor_cap_n and rows date
  
  stor_capacity_daily <- matrix(data = NA, nrow= nrow(all_huc2), ncol= ncol(all_huc2))
  stor_capacity_daily <- as.data.frame(stor_capacity_daily)
  colnames(stor_capacity_daily) <- c("date", dam_ids)
  stor_capacity_daily[,1] <- all_huc2[,1]
  col_name_storcap <- colnames(stor_capacity_daily)
  
  for(l in 2:ncol(stor_capacity_daily)){ # loop through dam ids and give index for adding to storage capacity table
    for(o in 1:nrow(stor_capacity_daily)){ # loop through rows to  addstorage capacity 
      if(is.na(interpolated[o,l]) == TRUE){ # if storage is negative then, ignore it 
        next
      }else{# if storage exist then pull storage capacity from GRanD
        dam <- col_name_storcap[l] 
        dam_index <- which(as.numeric(dam) == all_dams$DAM_ID)
        stor_cap <- all_dams[dam_index, 13] # pull out storage capacity
        stor_capacity_daily[o,l] <- stor_cap
      }
    }
  }
##### 7. Loop through and Calculate the Storage Capacity in ResOpsUS ####

  for (k in 1:length(years_using)){
    year <- years_using[k]
    stor_cap_daily_filter <- stor_capacity_daily %>% filter(year(date) == year)
    
   stor_cap_year_col <- colMeans(stor_cap_daily_filter[,2:ncol(stor_cap_daily_filter)], na.rm = T)
   storage_capacity <- sum(stor_cap_year_col, na.rm = T)
   
   # find year index for storage_covered
   year_index <- which(storage_covered$all_years == year)
  storage_covered[year_index,2] <- storage_capacity # plug storage capacity into the data frame

  }#(k in 1:length(all_years))
  
  

###### 8. Calculate the Water Year Averages ######
  year_list <- unique(year(interpolated$date))
  storage_year <- matrix(data = NA, ncol = 2, nrow = length(year_list))
  storage_year[,2] <- year_list
  
  # Aggregate Data by Water Year
  for (o in 2:length(year_list)){
    current_year <- year_list[o]
    past_year <- current_year -1
    
    start_index <- which(year(interpolated$date) ==past_year & month(interpolated$date) == 10 & day (interpolated$date) ==1)
    if (length(start_index) == 0){
      next
    }
    if (length(start_index) ==0){
      next
    }
    last_index <- which(year(interpolated$date) ==current_year & month(interpolated$date) == 9 & day(interpolated$date) == 30)
    if (length(last_index) ==0){
      next
    }
    water_year_matrix <- interpolated[start_index:last_index,]
    
    water_year_matrix$date <- NA
    stor_averages <- colMeans(water_year_matrix, na.rm = TRUE)
    average <- sum(stor_averages, na.rm = TRUE)
    storage_year[o,1] <- average
  }
 
  
  print("Made Water Year Averages")
 storage_year_graphing <- as.data.frame(storage_year, stringsAsFactors = FALSE)
 colnames(storage_year_graphing) <- c("value", "all_years")
 
 
#### 9. Calculate Fraction filled  ####
 normalized_matrix <- merge(storage_year_graphing, storage_covered, by = "all_years", all = TRUE )
 normalized_matrix$ff <- normalized_matrix$value/normalized_matrix$ResOpsUS

 
###### 10. Pull out SPI  by HUC2 #####
 drought_index <- huc2+1
 SPI_huc <- SPI[,c(1,drought_index)]
 colnames(SPI_huc) <- c("all_years", "SPI")
 
 ###### 11. Add in Drought Periods to See Trends ######
 normalized_final <- merge(normalized_matrix, SPI_huc, by = "all_years", all = TRUE)
 
 
 ###### 12. MAKE DAILY AVERAGES CSV ###########
final_daily <-rowSums(interpolated[,2:ncol(interpolated)], na.rm=TRUE) *NA^!rowSums(!is.na(interpolated[,2:ncol(interpolated)]))
final_daily_matrix <- cbind(as.character(all_huc2$date), final_daily)
colnames(final_daily_matrix) <- c("date", "storage")
all_years <- year(as.Date(final_daily_matrix[,1]))
months<- month(final_daily_matrix[,1])
final_daily_matrix <- cbind(final_daily_matrix, all_years, months)
year_mon <- paste0(final_daily_matrix[,3], "-", final_daily_matrix[,4])
final_daily_matrix <- cbind(final_daily_matrix, year_mon)
final_daily_matrix <- as.data.frame(final_daily_matrix, stringsAsFactors = F)
final_daily_matrix$storage <- as.numeric(final_daily_matrix$storage)
final_daily_matrix$date <- as.Date(final_daily_matrix$date)


# get storage capacity chart in a similar format
row_sum_stor_cap <- rowSums(stor_capacity_daily[,2:ncol(stor_capacity_daily)],na.rm= T)
row_sum_stor_cap[row_sum_stor_cap =="0"] <- NA
capacity_matrix <- data.frame("date" =stor_capacity_daily$date, "ResOpsUS" =row_sum_stor_cap)

final_daily_print <- merge(final_daily_matrix, capacity_matrix, by = "date")


final_daily_print$fraction<- as.numeric(final_daily_print$storage)/as.numeric(final_daily_print$ResOpsUS)


### 13. Create Monthly Dataset ######

month_data2 <- final_daily_print %>% group_by(year_mon) %>% 
   summarise(storage = mean(as.numeric(storage), na.rm = T),
             ResOpsUS = mean(as.numeric(ResOpsUS))) %>% 
   as.data.frame()
  month_data2$all_years <- year(as.Date(paste0(month_data2$year_mon,"-01")))
  month_data2$normalized <- month_data2$storage/month_data2$ResOpsUS
 

  ### 14. Write Out all the data ####

  setwd(yearly_data_save)
  data_file <- paste0("HUC", huc2, "averages.csv")
  write.csv(normalized_final, file = data_file, row.names = F )
  
  setwd(monthly_data_save)
 data_file2 <- paste0("HUC", huc2, "monthly_averages.csv")
 write.csv(month_data2, file = data_file2, row.names = F )
 
 setwd(daily_data_save)
 data_file3 <- paste0("HUC", huc2, "daily_averages.csv")
 write.csv(final_daily_print, file = data_file3, row.names = FALSE)
}#for (l in 1:length(huc2_list))

