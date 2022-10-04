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

# directories that may need to change
monthly_data_save <- "~/Desktop/github/ResOpsUS_Analysis/data/monthly_ff"
slope_save <- "~/Desktop/github/ResOpsUS_Analysis/data/"
other_data_directory <- "~/Desktop/github/ResOpsUS_Analysis/data/other_data"

# 1.Set up and Initialize all the necessary variables for plotting 
aridity_order <- c("a. Tennessee", "b. Lower Mississippi", "c. Ohio","d. South Atlantic","e. Sourris Red Rainy", 
                   "f. Arkansas White Red",
                   "g. Texas Gulf","h. Missouri","i. California","j. Upper Colorado", "k. Great Basin","l. Lower Colorado") 

basin_names <- c("d. South Atlantic", "c. Ohio", "a. Tennessee", "b. Lower Mississippi", 
                 "e. Sourris Red Rainy", "h. Missouri",
                 "f. Arkansas White Red", "g. Texas Gulf", "j. Upper Colorado", "l. Lower Colorado", "k. Great Basin", 
                 "i. California")

greater_50_list <- c(3,5,6,8,9,10,11,12,14,15,16,18)

r_squared_matrix <- matrix(data = NA, nrow = length(greater_50_list), ncol = 4)
r_squared_matrix[,1] <- greater_50_list

oct_all_data <- matrix(data = NA, nrow = 40, ncol = length(greater_50_list))
oct_all_predicted <-matrix(data = NA, nrow = 40, ncol = length(greater_50_list))

sen_slope_data <- matrix(data = NA, nrow = 12, ncol = 4)

# 2. Loop through and calculate the Sens slope for each region
for (p in 1:length(greater_50_list)){
  huc2 <- greater_50_list[p]
  # We used the the monthly averages to calculate the trend as there are some instances where an October 1st value is missing
  setwd(monthly_data_save)
  monthly_file <-  paste0("HUC", huc2, "monthly_averages.csv")
  monthly_data <- read.csv(file=monthly_file, stringsAsFactors = F)
  
 ### Clean up of the monthly files to make sure they are in the correct format
 
  monthly_data$date <- paste0(monthly_data$year_mon,"-01")
  monthly_data$date <-as.Date(monthly_data$date, format = "%Y-%m-%d")
  monthly_file_filter <- monthly_data %>% filter(all_years >=1980) %>% filter(all_years <2020)
  median_stor <- median(monthly_file_filter$normalized)
  monthly_file_filter$year_mon <- paste0(monthly_file_filter$year_mon, "-01")
  monthly_file_filter$month <- seq(0, nrow(monthly_file_filter)-1, by =1)
  
  # Pull out October data --> should be daily data
  oct_wy <-  monthly_data %>% filter(all_years >=1980) %>% filter(all_years <2020)%>% 
    filter(month(date) ==10) 
 
   #### 3. Calculate Sen Slopes #######
  
 
  coefficients <- zyp.sen(normalized~all_years,oct_wy) # y~x
  
  ts_oct <- ts(oct_wy$normalized, start=1980, end=2019, frequency=1) # create ts object with yearly values
  sen_slope <- sens.slope(ts_oct, conf.level = 0.95)
  oct_wy$predicted<- oct_wy$all_year*sen_slope$estimates+ coefficients$coefficients[1]
  
  sen_slope_data[p,1]<- huc2
  sen_slope_data[p,2] <- sen_slope$estimates
  sen_slope_data[p,3] <- sen_slope$p.value
  sen_slope_data[p,4] <- median_stor
  
  oct_all_data[,p] <-oct_wy$normalized
  oct_all_predicted[,p] <- oct_wy$predicted
}

colnames(sen_slope_data) <- c("HUC2", "slope", "pvalue", "median_stor")

### 4. Save the sens slopes to make QGIS map of Storage trends (Figure 6 b)

graphing_slopes<- cbind(basin_names, sen_slope_data)

setwd(slope_save)
write.csv(graphing_slopes,"All_slopes.csv", row.names = F)

## 5. Create Figure 6 a ###
# Plot of storage trends over time for each of the 12 basins

colnames(oct_all_data) <- basin_names
colnames(oct_all_predicted) <- basin_names

#create dataframe
oct_all_data <- as.data.frame(oct_all_data, stringsAsFactors = F)
oct_all_predicted <- as.data.frame(oct_all_predicted, stringsAsFactors = F)

oct_all_data$water_year <- seq(1980,2019, by =1)
oct_all_predicted$water_year <- seq(1980,2019, by =1)


# Drought Periods are pulled from SPI water year values
# Rectangles are plotted for water years that have SPI values less thant -0.3
# SPI values come from NCAR https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi

setwd(other_data_directory)
all_drought <- read.csv(file ="Drought_less_than_0.3.csv", stringsAsFactors = F)

melted_oct_all <- melt(oct_all_data, id.vars ="water_year")
melted_oct_predicted <- melt(oct_all_predicted, id.vars ="water_year")



ggplot()+
  geom_rect(data = all_drought, mapping = aes(xmin=xmin, xmax=xmax, ymin=0, ymax=1.1, fill = "Drought Periods"), alpha = 0.5)+
  geom_line(data = melted_oct_all,aes(x=water_year,y=value, color = "Start of Water Year Values"))+
  geom_point(data = melted_oct_all,aes(x=water_year,y=value, color = "Start of Water Year Values"), size =1)+
  geom_line(data=melted_oct_predicted,aes(x=water_year,y=value, color = "Linear Fit"), size = 1.25 )+
  facet_wrap(~factor(variable, levels = aridity_order), scales = "free_x")+ #issue is here with the variable name
  scale_color_manual(name = "Legend", labels = c(  "Linear Fit", "Start of Water Year Values"), 
                     values = c( "#8C6BB1","black" ))+
  scale_fill_manual(name = "Legend", label = "Drought Periods", values =  "#969696")+
  xlab("Water Year")+
  ylab("Fraction Filled")+
  theme_classic()+
  theme(axis.text.y = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),axis.title.x= element_text(face = "bold"),
        axis.text.x = element_text(size=9, angle = 90,face = "bold"), strip.text = element_text(face = "bold"),
        strip.background = element_blank())


