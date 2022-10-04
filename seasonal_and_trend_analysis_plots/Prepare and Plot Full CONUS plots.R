# Analysis for Entire CONUS
# Jen Steyaert
# 11-10-2021

library(lubridate)
library(tidyverse)
library(reshape)
library(gridExtra)
library(trend)
library(zyp)

# directories needed (MAY NEED TO BE CHANGED)
daily_data_save <- "~/Desktop/github/ResOpsUS_Analysis/data/daily_ff"
monthly_data_save <- "~/Desktop/github/ResOpsUS_Analysis/data/monthly_ff"
spi_directory <- "~/Desktop/Datasets/github/ResOpsUS_Analysis/other_data/"


# read in all the HUC2 files and get the ts of the conus values
huc2_list <- seq(1,18, by =1)

# Read in Data
setwd(daily_data_save)
daily_files <- list.files(pattern = "daily_averages")

setwd(monthly_data_save)
monthly_files <- list.files(pattern = "monthly_averages")

for (l in 1:length(daily_files)){
  file_name <- daily_files[l]
  setwd(daily_data_save)
  daily_data <- read.csv(file = file_name, stringsAsFactors = FALSE)
  filtered_data <- daily_data %>% filter(all_years >= 1980) %>% filter(all_years <2020)
  if (l ==1){
    final_daily <- filtered_data[,c(1,2,6)]
  }else{
    final_daily <- merge(final_daily, filtered_data[,c(1,2,6)], by = "date", all = T)
  }
}

daily_dates <- final_daily$date
daily_values <- as.data.frame(final_daily[,c(seq(2,28, by =2))], stringsAsFactors = F)
storage_cap <- as.data.frame(final_daily[,c(seq(3,29, by =2))], stringsAsFactors = F)
daily_averages <- rowSums(daily_values, na.rm = T)
storage_sums <- rowSums(storage_cap, na.rm = T)

# combine all into a dataframe again
# FF here is the total storage/ total storage capacity per huc, not on a reservoir by reservoir basis
total_daily <- data.frame("date" = daily_dates,  "average_storage" = daily_averages, "storage_cap" =storage_sums)
total_daily$fraction_filled <- total_daily$average_storage/total_daily$storage_cap

#### Monthly loop #####
for (l in 1:length(monthly_files)){
  file_name <- monthly_files[l]
  setwd(monthly_data_save)
  monthly_data <- read.csv(file = file_name, stringsAsFactors = FALSE)
  filtered_data <- monthly_data %>% filter(all_years >= 1980) %>% filter(all_years <2020)
  if (l ==1){
    final_monthly <- filtered_data[,c(1,2,3)]
  }else{
    final_monthly <- merge(final_monthly, filtered_data[,c(1,2,3)], by = "year_mon", all = T)
  }
}

# calculate averages for monthly data 

monthly_dates <- final_monthly$year_mon
monthly_values <- as.data.frame(final_monthly[,c(seq(2,28, by =2))], stringsAsFactors = F)
storage_cap_mon <- as.data.frame(final_monthly[,c(seq(3,29, by =2))], stringsAsFactors = F)
monthly_averages <- rowSums(monthly_values, na.rm = T)
storage_sums_mon <- rowSums(storage_cap_mon, na.rm = T)

# combine all into a dataframe again
total_monthly <- data.frame("date" = monthly_dates,  "average_storage" = monthly_averages, "storage_cap" =storage_sums_mon)
total_monthly$fraction_filled <- total_monthly$average_storage/total_monthly$storage_cap
total_monthly$date <- paste0(total_monthly$date, "-01")


### Calculate Yearly FF ######
all_years <- seq(1980,2019, by =1)
var_df <-matrix(data = NA, nrow = length(all_years), ncol =2)
for (p in 2:length(all_years)){
  current_year <- all_years[p]
  past_year <- current_year -1
  start_index <- which(year(total_monthly$date) == past_year & month(total_monthly$date) == 10)
  end_index <- which(year(total_monthly$date) == current_year & month(total_monthly$date) == 9)
  data_filter <- total_monthly[start_index:end_index,]
  
  # calculate and add values to variance df
  variance <- var(data_filter$fraction_filled)
  var_df[p,1] <- current_year
  var_df[p,2] <- variance
  
  average <- colMeans(data_filter[,2:4])
  if(p==2){
    water_year_data <- append(current_year, average)
  }else{
    values <- append(current_year, average)
    water_year_data<- rbind(water_year_data,values)
  }
}


##### Plot Figure 5 panel B  ######
# Plotting FF, FF variance, and linear trend over time

linear_values <- total_monthly %>% filter(month(date) ==10)
linear_values$year <- year(linear_values$date)


# Pull out the coefficients that we need for 
coefficients <- zyp.sen(fraction_filled~year,linear_values) # y~x

ts_all <- ts(linear_values$fraction_filled, start=1980, end=2019, frequency=1) # create ts object with yearly values
sen_slope <- sens.slope(ts_all, conf.level = 0.95)
linear_values$predicted<- linear_values$year*sen_slope$estimates+ coefficients$coefficients[1]


# get variance ready to plot
var_df <- as.data.frame(var_df, stringsAsFactors = F)
colnames(var_df) <- c("water_year", "variance")

###### Add in SPI Values from NCAR dataset ######
setwd(spi_directory)
SPI <- read.csv( file = "SPI_Water_Year_Avg_HUC2.csv", stringsAsFactors = FALSE)

SPI_filtered <- SPI %>% filter(X >=1980)

SPI_dates <- SPI_filtered$X
SPI_filtered$X <- NA
spi_averages <- rowMeans(SPI_filtered, na.rm = T)
spi_final <- data.frame("water_year" = SPI_dates, "Average_SPI" = spi_averages)
drought_lines <- data.frame("xmin" = c(1980, 1987, 1993, 1999, 2005), "xmax" = c(1983, 1989, 1995, 2003, 2009),
                            "color" = c(1,2,3,2,3))

colnames(var_df) <- c("year", "variance")
plot_1 <- merge(linear_values[,c(4,5,6)], var_df, by = "year")
plot_1$variance <- plot_1$variance*100
graphing_test <- melt(plot_1, id.vars = "year")

# Plot for Figure 5 Panel B
ff_over_time  <- ggplot()+
  geom_rect(data = drought_lines, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 0.9, 
                                      fill = factor(color, level = c(3,1,2))), alpha = 0.4)+
  scale_fill_manual(name = "Drought Severity", labels = c("0 to -0.1","-0.1 to 0.2","Less than -0.3"), values= c("#FCBBA1","#FB6A4A","#A50F15"))+
  geom_point(data = plot_1,mapping = aes(x = year, y = fraction_filled),size = 2)+
  geom_line(data = graphing_test, mapping = aes(x = year, y =  value, color = variable, linetype = variable), size = 1.5)+
  geom_point(data = plot_1, mapping = aes(x= year, y = variance), color = "#4D004B", size = 2)+
  scale_y_continuous(
    name = "Fraction Filled",
    sec.axis = sec_axis(~./100, name = "Variance")
  ) +
  scale_linetype_manual(values = c(1,1,2), name = "Legend", labels = c("Linear Fit","Start of Water Year",  "Variance"))+
  scale_color_manual(name = "Legend", labels = c("Linear Fit","Start of Water Year",  "Variance"), 
                     values =c( "black","#8C6BB1", "#4D004B" ))+
  
  xlab("Water Year")+
  ylab("Fraction Filled")+
  theme_classic()+
  theme(legend.key.size=unit(3,"lines"))+
  theme (axis.title.y = element_text(face = "bold"),
         axis.title.x= element_text(face = "bold"), axis.text.y = element_text(face = "bold"), 
         axis.text.x = element_text(face = "bold"))



##### Prepare and Plot Figure 5 panel A ####
# Storage capacity in ResOpsUS and GRanD over time

huc2_list <- c(3,5,6,7,8,9,10,11,12,14,15,16,17,18)

for (p in 1:length(huc2_list)){
  setwd("~/Desktop/Paper_2/HUC2 Water Year and Monthly Averages /final_data")
  file_name <- paste0("HUC",huc2_list[p], "averages.csv")
  data <- read.csv(file= file_name)
  data_filter <- data %>%  filter(all_years<2020)
  if (p ==1){
    final_data_GRanD <- data_filter[,c(1,4)]
    final_data_ResOpsUS <- data_filter[,c(1,3)]
  }else{
    final_data_GRanD <- merge(final_data_GRanD, data_filter[,c(1,4)], by = "all_years")
    final_data_ResOpsUS <- merge(final_data_ResOpsUS, data_filter[,c(1,3)], by = "all_years")
  }
}

all_years <- final_data_GRanD$all_years
graphing_storcapG <- final_data_GRanD[,2:ncol(final_data_GRanD)]
storcap_sumsG <- rowSums(graphing_storcapG)

graphing_storcapR <- final_data_ResOpsUS[,2:ncol(final_data_ResOpsUS)]
storcap_sumsR <- rowSums(graphing_storcapR)

final_graphing <- as.data.frame(cbind(all_years, storcap_sumsG, storcap_sumsR), stringsAsFactors = FALSE)  
colnames(final_graphing) <- c("all_years", "GRanD", "ResOpsUS")
storcap_graphing <- melt(final_graphing, id.vars = "all_years")


resopsus_grand <- ggplot(storcap_graphing, mapping = aes(x = all_years, y = value,group = variable)) +
  geom_line(aes(linetype = variable))+
  ylab("Storage Capacity (MCM)")+
  scale_linetype_manual("Data Source", values = c( 1, 2))+

  xlab("Year")+
  theme_classic()  +
  theme(legend.position= "none",axis.title.y = element_text(face = "bold"),
   axis.title.x= element_text(face = "bold"), axis.text.y = element_text(face = "bold"), 
   axis.text.x = element_text(face = "bold"))


# Make final facet plot for the paper 
grid.arrange(resopsus_grand,  ff_over_time, ncol =2, nrow = 2)


