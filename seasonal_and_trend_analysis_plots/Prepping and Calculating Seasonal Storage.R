# Jen Steyaert
# Prep Data and Graph Seasonal Fraction Filled Medians
# 6-12-2022

# List of libraries
library(lubridate)
library(gridExtra)
library(reshape)

# Directories that may need to change
daily_data_save <- "~/Desktop/github/ResOpsUS_Analysis/data/HUC_FF/daily_ff"

# 1. Set Up that involves HUC2 list, ardity order for panels, basin names and matrices

greater_50_list <- c(3,5,6,8,9,10,11,12,14,15,16,18)# 
aridity_order <- c("a. Tennessee", "b. Lower Mississippi", "c. Ohio","d. South Atlantic","e. Sourris Red Rainy", 
                   "f. Arkansas White Red",
                   "g. Texas Gulf","h. Missouri","i. California","j. Upper Colorado", "k. Great Basin","l. Lower Colorado") 

basin_names <- c("d. South Atlantic", "c. Ohio", "a. Tennessee", "b. Lower Mississippi", 
                 "e. Sourris Red Rainy", "h. Missouri",
                 "f. Arkansas White Red", "g. Texas Gulf", "j. Upper Colorado", "l. Lower Colorado", "k. Great Basin", 
                 "i. California")

# Set Up all the Matrices that are needed to gather plotting data
month_matrix_med <- matrix(data = NA, nrow = 12, ncol = 12)
month_matrix_med <- as.data.frame(month_matrix_med)
colnames(month_matrix_med) <- basin_names 
month_matrix_med$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                            "Nov", "Dec")

month_matrix_max <- matrix(data = NA, nrow = 12, ncol = 12)
month_matrix_max <- as.data.frame(month_matrix_max)
colnames(month_matrix_max) <- basin_names 
month_matrix_max$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                            "Nov", "Dec")

month_matrix_min <- matrix(data = NA, nrow = 12, ncol = 12)
month_matrix_min <- as.data.frame(month_matrix_min)
colnames(month_matrix_min) <- basin_names 
month_matrix_min$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                            "Nov", "Dec")

# 2. For loop to calculate the monthly median fraction filled, as well as maximum and minimums
  for (p in 1:length(greater_50_list)){
    
    huc2 <- greater_50_list[p]
    
    # read in the daily files
    setwd(daily_data_save)
    
    data_file <- paste0("HUC", huc2, "daily_averages.csv")
    daily_file <- read.csv( file = data_file, stringsAsFactors = FALSE)
    
    daily_file <- daily_file %>% filter(all_years >=1980 & all_years < 2020) 
    
    # Calculate Max, Min and Variance for Monthly Data
    for (m in 1:12){
      median_all <- c()
      max_all <- c()
      min_all <- c()
      data_filter <- daily_file %>% 
        filter(month(daily_file$date)==m) 
      
      median <- median(data_filter$fraction, na.rm = TRUE)
      max_ff <- max(data_filter$fraction, na.rm = TRUE)
      min_ff <- min(data_filter$fraction, na.rm = TRUE)
    
    month_matrix_med[m,p]<- median
    month_matrix_max[m,p]<- max_ff
    month_matrix_min[m,p]<- min_ff
  } # end looping through all regions
} # end of for loop

### Plot Figure 2 Panel A ###
# Graph median ff with maximum and minimum bounds
  melted_med <- melt(month_matrix_med, id.vars = "month")
  melted_max <- melt(month_matrix_max, id.vars = "month")
  melted_min <- melt(month_matrix_min, id.vars = "month")
  
  melted_bounds <- cbind(melted_max, melted_min[,3])
  colnames(melted_bounds) <- c("month", "variable", "max", "min")
  
  
  ggplot()+
    geom_ribbon(data = melted_bounds, aes(x = factor(month, levels = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                        "Nov", "Dec")), ymin = min, ymax = max, 
                                          group = variable), fill ="#D4B9DA", alpha = 0.7)+
    geom_vline(xintercept = "Jun", linetype = 2)+
    geom_line(data = melted_med, mapping = aes(x = factor(month, levels = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                               "Nov", "Dec")),y = value), group =1)+
    geom_point(data = melted_med, mapping = aes(x = factor(month, levels = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                             "Nov", "Dec")),y = value), size =1)+
    facet_wrap(~factor(variable, levels = aridity_order))+ #issue is here with the variable name
    
    xlab("Month")+
    ylab("Fraction Filled")+
    theme_grey()+
    theme(axis.text.x = element_text(size=9, angle=90, face="bold"), 
          strip.text = element_text(face="bold"),axis.title.y = element_text(face = "bold"),
          axis.title.x= element_text(face = "bold"),
          strip.background = element_blank())
  
  
  
  
 