# Trend Analysis
# Jen Steyaert
# 11-17-2021
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stats)
library(reshape)
library(gridExtra)
library(RColorBrewer)
library(trend)
library(zyp)
library(scales)

basin_names <- c("d. South Atlantic", "c. Ohio", "a. Tennessee", "b. Lower Mississippi", 
                 "e. Sourris Red Rainy", "h. Missouri",
                 "f. Arkansas White Red", "g. Texas Gulf", "j. Upper Colorado", "l. Lower Colorado", "k. Great Basin", 
                 "i. California")
greater_50_list <- c(3,5,6,8,9,10,11,12,14,15,16,18)

month_slope_matrix <- matrix(data = NA, nrow = length(greater_50_list), ncol = 13)
month_slope_matrix[,13] <- greater_50_list

month_p_value_matrix2 <- matrix(data = NA, nrow = length(greater_50_list), ncol = 13)
month_p_value_matrix2[,13] <- greater_50_list


month_avg_range <- matrix(data = NA, nrow = 12, ncol = 12)
month_avg_range<- as.data.frame(month_avg_range)
colnames(month_avg_range) <- basin_names 
month_avg_range$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                            "Nov", "Dec")

month_max_range <- matrix(data = NA, nrow = 12, ncol = 12)
month_max_range<- as.data.frame(month_max_range)
colnames(month_max_range) <- basin_names 
month_max_range$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                           "Nov", "Dec")

month_min_range <- matrix(data = NA, nrow = 12, ncol = 12)
month_min_range<- as.data.frame(month_min_range)
colnames(month_min_range) <- basin_names 
month_min_range$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                           "Nov", "Dec")

# 1. Set up everything for storage trends
month_slope_storage <- matrix(data = NA, nrow = length(greater_50_list), ncol = 13)
month_slope_storage[,13] <- greater_50_list

month_p_value_storage <- matrix(data = NA, nrow = length(greater_50_list), ncol = 13)
month_p_value_storage[,13] <- greater_50_list


month_med_stor <- matrix(data = NA, nrow = 12, ncol = 12)
month_med_stor<- as.data.frame(month_med_stor )
colnames(month_med_stor ) <- basin_names 
month_med_stor $month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                           "Nov", "Dec")

month_max_stor <- matrix(data = NA, nrow = 12, ncol = 12)
month_max_stor<- as.data.frame(month_max_stor)
colnames(month_max_stor) <- basin_names 
month_max_stor$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                           "Nov", "Dec")

month_min_stor <- matrix(data = NA, nrow = 12, ncol = 12)
month_min_stor<- as.data.frame(month_min_stor)
colnames(month_min_stor) <- basin_names 
month_min_stor$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                           "Nov", "Dec")

for (p in 1:length(greater_50_list)){
  huc2 <- greater_50_list[p]
  
  setwd("~/Desktop/Paper_2/HUC2 Water Year and Monthly Averages /final_data")
  daily_file <-  paste0("HUC", huc2, "daily_averages.csv")
  daily_file <- read.csv(file = daily_file, stringsAsFactors = F)
  
  # 2.  Min and Max analysis #####
  # Put minimum, maximum, and median fraction filled values all in one data frame
  daily_filtered <- daily_file %>% filter(all_years >1979) %>% filter(all_years<2020)
  
  for (l in unique(daily_filtered$all_years)){
    print(l)
    min_max <- daily_filtered %>% filter(all_years ==l) %>% 
      group_by(months) %>% 
      summarise(
        max_month = max(fraction, na.rm = T),
        min_month = min(fraction, na.rm = T),
        med_month = median(fraction, na.rm = T)
      ) %>% 
      ungroup()
    min_max$year <- l
    if(l == 1980){
      final_data <- min_max
    }else{
      final_data <- rbind(final_data, min_max)
    }
  }
  
  final_data$date <- as.Date(paste0(final_data$year,"-", final_data$months,"-01"))
  final_data$diff <- final_data$max_month-final_data$min_month
  
  # 3. Calculate Sens Slopes for the Whole Period for both the storage and range trends
  
  for (o in 1:12){
    # Analyze Range Trend
    final_data_filtered <- final_data %>% filter(months == o)
    avg_range <- median(final_data_filtered$diff,na.rm = T)
    max_range <- max(final_data_filtered$diff, na.rm = T)
    min_range <- min(final_data_filtered$diff, na.rm = T)
    
    month_avg_range[o,p] <- avg_range
    month_min_range[o,p] <-min_range
    month_max_range[o,p] <- max_range
    
    coefficients_mon <- zyp.sen(diff~year,final_data_filtered) # y~x
    
    ts_month <- ts(final_data_filtered$diff, start=1980, end=2019, frequency=1) # create ts object with yearly values
    sen_slope_month <- sens.slope(ts_month, conf.level = 0.95)
    final_data_filtered$predicted<- final_data_filtered$year*sen_slope_month$estimates+ coefficients_mon$coefficients[1]
    
    month_slope_matrix[p,o] <- sen_slope_month$estimates
    month_p_value_matrix2[p,o] <-sen_slope_month$p.value
    
    # Analyze Storage Trend
   
    coefficients_stor <- zyp.sen(med_month~year,final_data_filtered) # y~x
    
    ts_stor <- ts(final_data_filtered$med_month, start=1980, end=2019, frequency=1) # create ts object with yearly values
    sen_slope_stor <- sens.slope(ts_stor, conf.level = 0.95)
    final_data_filtered$predicted_stor<- final_data_filtered$year*sen_slope_stor$estimates+ coefficients_stor$coefficients[1]
    
    month_slope_storage[p,o] <- sen_slope_stor$estimates
    month_p_value_storage[p,o] <-sen_slope_stor$p.value
  }
}

#### 4. Graph the slopes for Fraction Filled and Range Trends (Figure 3 and Figure 7 )
# Monthly Slopes Plots
aridity_order <- c("a. Tennessee", "b. Lower Mississippi", "c. Ohio","d. South Atlantic","e. Sourris Red Rainy", 
                   "f. Arkansas White Red",
                   "g. Texas Gulf","h. Missouri","i. California","j. Upper Colorado", "k. Great Basin","l. Lower Colorado") 



colnames(month_slope_matrix) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                  "Nov", "Dec", "huc2")

colnames(month_p_value_matrix2) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                     "Nov", "Dec", "huc2")
# Plot for Figure 7
month_p_value_matrix2 <- as.data.frame(month_p_value_matrix2)
month_p_value_matrix2$huc2 <- basin_names
all_p_value<-melt(month_p_value_matrix2, id.vars= "huc2")


month_slope_matrix <- as.data.frame(month_slope_matrix)
month_slope_matrix$huc2 <- basin_names
all_month<-melt(month_slope_matrix, id.vars= "huc2")

final_graphing <- cbind(all_month, all_p_value[,3])
colnames(final_graphing) <- c("huc2", "month", "slope", "pvalue")

final_graphing <- final_graphing %>%  mutate(pos = slope >= 0)


  
ggplot(data = final_graphing, mapping = aes(x = factor(month), y = slope, fill = pos, alpha = pvalue <0.1 ))+
  geom_bar( stat = "identity", position= position_dodge(), color = "black")+
  facet_wrap( .~huc2, scales= "free", ncol = 3)+
  geom_hline(yintercept = 0)+
  scale_fill_manual(name = "Positive Slope", labels = c("Negative", "Positive"), values = c("#AE017E", "#1D91C0"))+
  scale_alpha_discrete( name = "Statistically Significant", labels = c("No", "Yes"),range = c(0.3, 1), )+
  scale_y_continuous(labels = comma)+
  ylab("Sen Slope")+
  xlab("Month")+
  #ggtitle("Monthly Linear Trend Slope for Range")+
  theme_classic(base_size = 9)+ 
  theme(axis.text.y = element_text(face = "bold"), title =  element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),axis.title.x= element_text(face = "bold"),
        axis.text.x = element_text( angle = 90,face = "bold"), strip.text = element_text(face = "bold"),
        strip.background = element_blank())




# Plot for Figure 3

melt_avg <- melt(month_avg_range, id.vars= "month")
melted_max <- melt(month_max_range, id.vars= "month")
melted_min <- melt(month_min_range, id.vars= "month")

melted_bounds <- cbind(melted_max, melted_min[,3])
colnames(melted_bounds) <- c("month", "variable", "max", "min")
  



ggplot()+
  geom_ribbon(data = na.omit(melted_bounds), aes(x = factor(month, levels = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                      "Nov", "Dec")), ymin = min, ymax = max, 
                                        group = variable), fill ="#D4B9DA", alpha = 0.7)+
  geom_vline(xintercept = "Jun", linetype = 2)+
  geom_line(data = na.omit(melt_avg), mapping = aes(x = factor(month, levels = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                           "Nov", "Dec")),y = value), group =1)+
  geom_point(data = na.omit(melt_avg), mapping = aes(x = factor(month, levels = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                            "Nov", "Dec")),y = value), size =1)+
  facet_wrap(~factor(variable, levels = aridity_order))+ #issue is here with the variable name
  
  xlab("Month")+
  ylab("Fraction Filled Range")+
  theme_grey()+
  theme(axis.text.x = element_text(size=9, angle=90, face="bold"), 
        strip.text = element_text(face="bold"),axis.title.y = element_text(face = "bold"),
        axis.title.x= element_text(face = "bold"),
        strip.background = element_blank())





