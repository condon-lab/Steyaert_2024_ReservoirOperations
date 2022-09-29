# Drought Sensitivity
# Calculate Time to Recovery for Storage and SSI Timeseries


#Jen Steyaert
# Drought Sensistivity
# 3-17-2021

# libraries
library(tidyverse)
library(stats)
library(gridExtra)
library(ggplot2)
library(lubridate)
library(reshape)
library(trend)
library(zyp)
library(RColorBrewer)

## Create List of HUCS We Can Use Data For
greater_50_list <- c(3,5,6,8,9,10,11,12,14,15,16,18)
setwd("~/Desktop/Paper_2/Streamflow_Storage_Corr/")
periods <- read.csv(file= "Drought_Periods_HUC2.csv", stringsAsFactors = F)


# Loop Through and Analyze the 12 HUCS

for (u in 1:length(greater_50_list)){
  huc2 = greater_50_list[u]
  hucs = greater_50_list[u]
  
  # 1. Read in the daily file for the huc2 and the SSI files
  setwd("~/Desktop/Paper_2/HUC2 Water Year and Monthly Averages /final_data")
  data_file <- paste0("HUC", huc2, "monthly_averages.csv")
  data_monthly <- read.csv( file = data_file, stringsAsFactors = FALSE)
  monthly_filter <- data_monthly %>% filter(all_years < 2020) %>% filter(all_years>=1979)
  monthly_filter$date <- paste0(monthly_filter$year_mon, "-01")
  monthly_filter$date <- as.Date(monthly_filter$date)
  monthly_filter <- monthly_filter[order(monthly_filter$date),]
  
  setwd("~/Desktop/Paper_2/HUC2 Water Year and Monthly Averages /final_data")
  daily_file <-  paste0("HUC", huc2, "daily_averages.csv")
  daily_file <- read.csv(file = daily_file, stringsAsFactors = F)
  daily_filter <- daily_file %>% filter(year(date) >1979)
  
  # Calculate Monthly Averages to Compare SSI and Storage Recovery value to 
  monthly_averages <- matrix(data = NA, nrow = 12, ncol = 2)
  for(o in 1:12){
    monthly_averages[o,1] <- o
    average <- monthly_filter %>% filter(month(date) == o) %>% summarise(mean = median(normalized, na.rm = T))
    monthly_averages[o,2] <- as.numeric(average)
   
  }
  monthly_averages <- as.data.frame(monthly_averages, stringsAsFactors = F)
  colnames(monthly_averages) <- c("month", "average_stor")
  
  monthly_filter$month <- month(monthly_filter$date)
  monthly_filtered_anom <- merge(monthly_filter, monthly_averages, by = "month")
  monthly_filtered_anom$anomaly <- monthly_filtered_anom$normalized - monthly_filtered_anom$average_stor
  monthly_filtered_anom$year <- year(monthly_filtered_anom$date)
  monthly_filter_final <-  monthly_filtered_anom[with(monthly_filtered_anom, order(year, month)), ]  
  
  setwd("~/Desktop/Paper_2/HUC2 Water Year and Monthly Averages /final_data")
  year_file <- paste0("HUC", huc2, "averages.csv")
  year_data <- read.csv(file = year_file, stringsAsFactors = FALSE)
  
  setwd("~/Desktop/CONUS_INFO/HUC2_ref_gages/HUC2_SSI")
  if(huc2 <10 ){
    huc2 <- paste0(0, huc2)
    file_name <- paste0(huc2, "_avg_ssi.csv")
    ssi <- read.csv(file = file_name, stringsAsFactors = F)
    colnames(ssi) <- c("date", "average_ssi")
    ssi$date <- as.Date(ssi$date)
  }
  if(huc2 >10 ){
    file_name <- paste0(huc2, "_avg_ssi.csv")
    ssi <- read.csv(file = file_name, stringsAsFactors = F)
    colnames(ssi) <- c("date", "average_ssi")
    ssi$date <- as.Date(ssi$date)
  }
  if (huc2 == 10){
    huc1 <- "10L"
    ssi1 <- read.csv(file = paste0(huc1, "_avg_ssi.csv"), stringsAsFactors = F)
    huc2 <- "10U"
    ssi2 <- read.csv(file = paste0(huc2, "_avg_ssi.csv"), stringsAsFactors = F)
    ssi_all <- merge(ssi1, ssi2, by = "Date")
    means <- rowMeans(ssi_all[,2:3])
    ssi <- data.frame("date" = as.Date(ssi1$Date),
                      "average_ssi" = means, stringsAsFactors = F)
  }
  
 
  ssi_ff <- merge(ssi, monthly_filter_final[,c(7,9)], by = "date")
  colnames(ssi_ff) <- c("date", "average_ssi", "normalized")
  
 ggplot(data = monthly_filter_final)+
   #geom_line(mapping = aes(x = date, y = normalized))+
   geom_line(mapping = aes(x = date, y = anomaly))
  ##### Mkae 5 year rolling average for SSI and make 1 month rolling average for storage
  
  rolling_avg <- as.data.frame(ssi_ff) %>%
    dplyr::arrange(date) %>% 
    dplyr::mutate(five_year = zoo::rollmean(average_ssi, k = 60, fill = NA),
                  thirty_day = zoo::rollmean(normalized, k = 24, fill = NA))
  
  peak<- c(max(rolling_avg$five_year, na.rm=T))
  peakcoord <- which(rolling_avg$five_year == peak)
  min_value <- min(rolling_avg$five_year,na.rm = T)
  
  rolling_average_plot <- ggplot(data = rolling_avg)+
    geom_line(mapping = aes(x = date, y = thirty_day, color = "Storage"))+
    geom_point(mapping = aes(x = date, y = thirty_day, color = "Storage"))+
    geom_line(mapping = aes(x = date, y = five_year, color = "SSI"))+
    geom_point(mapping = aes(x = date, y = five_year, color = "SSI"))+
    geom_hline(yintercept = median(rolling_avg$five_year, na.rm = T), linetype = "dashed", color = "#67001F")+
    scale_color_manual(name = "Legend", labels = c( "SSI", "Storage"), 
                       values = c("#CE1256", "black" ))+
    xlab("Date")+
    ylab("Normalized Value")+
    theme_classic()+
    theme(axis.text.y = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),axis.title.x= element_text(face = "bold"),
          axis.text.x = element_text(size=9,face = "bold"), strip.text = element_text(face = "bold"),
          strip.background = element_blank())
  
  
  
  setwd("~/Desktop/Paper_2/Streamflow_Storage_Corr/")
  ggsave(filename = paste0(huc2, "Rolling Average.png"), rolling_average_plot)
  
  #2. Pull out Metrics for Each Drought Period
  
  # Metrics = minimum storage, storage at drought period start date,   minimum SSI, 
  #           SSI at start date
  

  periods_filtered <- periods %>% filter(periods$huc2 == greater_50_list[u])
  
  recovery_matrix <- matrix(data = NA, nrow = nrow(periods_filtered) , ncol = 13)
  recovery_matrix <- as.data.frame(recovery_matrix, stringsAsFactors = F)
  
  for(p in 1:nrow(periods_filtered)){
    
    ### First pull out the drought period dates to filter storage and SSI timeseries
    
    start_date <- periods_filtered[p,2]
    end_date <- periods_filtered[p,3]
    start_itemp <- which(rolling_avg$date == start_date)
    end_itemp <- which(rolling_avg$date == end_date)
    
    data2 <- rolling_avg[start_itemp:end_itemp,]
    median_stor <- median(data2$thirty_day, na.rm = T)
    min_stor <- min(data2$thirty_day, na.rm = T)
    median_ssi <- median(data2$five_year, na.rm = T)
    min_ssi <- min(data2$five_year, na.rm = T)
    
    recovery_matrix[p,1] <- min_stor
    
    
    recovery_matrix[p,5] <- min_ssi
    recovery_matrix[p,6] <- data2[1,2] # start ssi
    
    
    ##### Analyze the storage timeseries first 
    
    val = 1 # initialize the start index
    stor_start = data2[val,5]
    recovery_matrix[p,2] = stor_start
    
    while(is.na(stor_start) == T){
      val = val +1
      stor_start = data2[val,5]
    }
    
    recovery_matrix[p,2] <- stor_start
    value = which(min_stor == rolling_avg$thirty_day)
    storage = rolling_avg[value, 3]
    index = value
    month = 0
    while(storage < stor_start && is.na(storage) == F){
      index = index+1
      print(index)
      storage = round(rolling_avg[index,5],3)
      month = month +1 # add one to counter to count the years til normal
      if(index > nrow(rolling_avg)){
        #oct_time = nrow(oct_all)
        recovery_matrix[p,4] = ("storage did not recover")
        break
      }
    }
    recovery_matrix[p,3] <- month
    recovery_matrix[p,9] <- as.character(rolling_avg[index,1]) # get the date storage recovers
    recovery_matrix[p,13] <- rolling_avg[index,5] # pull storage at recovery
    
    
    #Analyze the SSI timeseries over the same drought period
    
    val2 = 1 # initialize the SSI index
    ssi_start = data2[val2,4]
    while(is.na(ssi_start) == T){
      val2 = val2 +1
      ssi_start = data2[val2,4]
    }
  
    value = which(min_ssi == rolling_avg$five_year)
    ssi = rolling_avg[value, 4]
    index2 = value
    month_ssi = 0
    
    while(ssi< ssi_start && is.na(ssi) == F){
      index2 = index2+1
      print(index2)
      ssi= round(rolling_avg[index2,2],3)
      month_ssi = month_ssi +1 # add one to counter to count the years til normal
      if(index2 > nrow(rolling_avg)){
        #oct_time = nrow(oct_all)
        recovery_matrix[p,8] = ("SSI did not recover")
        break
      }
    }
    recovery_matrix[p,7] <- month_ssi
    recovery_matrix[p,10] <- as.character(rolling_avg[index2,1])
    recovery_matrix[p,12] <- rolling_avg[index2,2] # pull ssi at recovery
    recovery_matrix[p,11] <- p
  }   
  
  # Rename Columns of Recovery Matrix
  colnames(recovery_matrix) <- c("min_stor", "start_stor", "stor_recov", 
                              "break_stor", "min_ssi", "start_ssi", "ssi_recov", "break_ssi",  
                              "date_stor_recov", "date_ssi_recov", "drought_num",  "final_ssi","final_stor")
  
  # Add Hucs to SSI Recovery Matrix
  recovery_matrix$huc2 <- hucs
  
  if(u ==1){
    all_regions = recovery_matrix
  }else{
    all_regions <- rbind(all_regions,recovery_matrix)
  }
  
  
  # Calculate Storage Anomoly (Storage Value at Recovery - Monthly Storage Average)
  anomoly_matrix <- matrix(data = NA, nrow = nrow(recovery_matrix), ncol = 6)
  anomoly_matrix <- as.data.frame(anomoly_matrix)
  for ( h in 1:nrow(recovery_matrix)){
    storage_val <- as.numeric(recovery_matrix[h,13])
    storage_date <- recovery_matrix[h,9]
    anomoly_matrix[h,1] <- storage_val
    
    
    # find matching month if storage recovered
    if(is.na(storage_date) == F ){
      mon <- month(as.Date(storage_date))
      index <- which(monthly_averages$month == mon)
      anomoly_matrix[h,3] <- monthly_averages[index,2]
      anomoly_matrix[h,2] <- storage_date
    }
    
    ssi_val <- recovery_matrix[h,12]
    ssi_date <- recovery_matrix[h,10]
    anomoly_matrix[h,4] <- ssi_val
    
    
    if(is.na(ssi_date) == F){
      mon2 <- month(as.Date(ssi_date))
      index2 <- which(monthly_averages$month == mon2)
      anomoly_matrix[h,6] <- monthly_averages[index2,2]
      anomoly_matrix[h,5] <- ssi_date
    }
  }
  colnames(anomoly_matrix) <- c("storage_val", "storage_date", "stor_average", "ssi_val", "ssi_date", "ssi_avg")
  
  # Calculate Storage Anomalies and put them all in the same matrix
  anomoly_matrix$storage_anom <- anomoly_matrix$storage_val - anomoly_matrix$stor_average
  anomoly_matrix$ssi_anom <- anomoly_matrix$ssi_val - anomoly_matrix$ssi_avg
  anomoly_matrix$huc2 <- greater_50_list[u]
  
  if(u ==1){
    all_anomolies = anomoly_matrix
  }else{
    all_anomolies <- rbind(all_anomolies,anomoly_matrix)
  }
  
}

# Final Graphs for the Paper

# Create Graphing Data frame
graphing_all <- data.frame("stor_recov" = all_regions[,3], "ssi_recov" = all_regions[,7], 
                           "huc2" = as.numeric(all_regions[,14]),"storage_recov_date" = all_regions[,9],
                             stringsAsFactors = F)
graphing_all<- graphing_all %>% filter(as.Date(storage_recov_date)< "2018-12-01")
graphing_all$stor_recov[graphing_all$stor_recov > 100] = 50

# Set Colors, basin names, aridity order, and make sure all are in the full graphing matrix
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols)

basin_names <- c("South Atlantic", "Ohio", "Tennessee", "Lower Mississippi", 
                 "Sourris Red Rainy", "Missouri",
                 "Arkansas White Red", "Texas Gulf", "Upper Colorado", "Lower Colorado", "Great Basin", 
                 "California")
aridity_order <- c("Tennessee", "Lower Mississippi", "Ohio","South Atlantic","Sourris Red Rainy", 
                   "Arkansas White Red",
                   "Texas Gulf","Missouri","California","Upper Colorado", "Great Basin","Lower Colorado") 

all_names <- cbind(greater_50_list,basin_names)

graphing_all$name <- NA
graphing_all$shape <- NA
for(h in 1:nrow(graphing_all)){
  huc <- as.character(graphing_all[h,3])
  index <- which(huc == as.numeric(all_names[,1]))
  graphing_all[h,5] <- all_names[index,2]
  
  if(graphing_all[h,1]>=50){
    graphing_all[h,6] <- 1
  }else{
    graphing_all[h,6] <- 0
  }
  
}

#Create  Figure 8b with correct colors, yellow one to one line, shapes and axis

ggplot(data = graphing_all)+
  geom_abline(slope = 1, linetype = "dashed", color = "yellow")+
  geom_point(mapping = aes(y = stor_recov, x = ssi_recov, shape = factor(shape),
                           color = factor(name, levels = aridity_order)), 
             size = 3)+
  xlim(0,30)+
  ylim(0,50)+
  scale_color_manual(values = mycolors, name = "Region")+
  scale_shape_discrete(name = "Recovery Longer than 8 Years", labels = c( "No", "Yes"))+
  ggtitle("Recovery for all")+
  xlab("SSI Recovery Time (Months)")+
  ylab("Storage Recovery Time (Months)")+
  theme_dark()+
  theme(axis.text = element_text(size=16, face="bold"), 
        strip.text = element_text(face="bold"),axis.title.y = element_text(face = "bold"),
        axis.title.x= element_text(face = "bold"),
        strip.background = element_blank())





# Create Final data frame consisting of storage recovery times, ssi recovery times, and HUC2 to create
# Figure 8 a in QGIS


final_data <- data.frame("stor_recov" = all_regions[,3], "ssi_recov" = all_regions[,7], 
                                         "huc2" = all_regions[,14], stringsAsFactors = F)

final_data[final_data$stor_recov > 200,] <- NA  # removes the two values that are way too high (greater than the amount of time we are looking at)
final <- final_data %>%
  group_by(huc2) %>%
  summarise_at(vars(stor_recov, ssi_recov), list(name = mean))
final$ratio <- final$stor_recov_name/final$ssi_recov_name # Calculate the storage ratio
setwd("~/Desktop/Paper_2/HUC2 Water Year and Monthly Averages /AGU PLOTS/") # Select directory to save the data
write.csv(final, file = "SSI_All.csv")


