# Variance Calculations for Paper
# Jen Steyaert
# Variance by Water Year and by Month
# 11-1-2021

library(lubridate)
library(gridExtra)
library(ggplot2)
library(reshape)

# Directories that you may need to change
daily_data_save <- "~/Desktop/github/ResOpsUS_Analysis/data/HUC_FF/daily_ff"

# Set Up and Initialize all the lists that we have
greater_50_list <- c(3,5,6,8,9,10,11,12,14,15,16,18)# Use this one first because the most complete records
basin_names <- c("d. South Atlantic", "c. Ohio", "a. Tennessee", "b. Lower Mississippi", 
                 "e. Sourris Red Rainy", "h. Missouri",
                 "f. Arkansas White Red", "g. Texas Gulf", "j. Upper Colorado", "l. Lower Colorado", "k. Great Basin", 
                 "i. California")
final_variance_bp <- matrix(data = NA, nrow = 480, ncol = 13)
final_variance_bp <- as.data.frame(final_variance_bp)
colnames(final_variance_bp) <- c(greater_50_list, "date")

final_variance_bp[,13]  <- seq(as.Date("1/1/1980", format = "%m/%d/%Y"), as.Date("12/1/2019", format = "%m/%d/%Y"), 
                                    by = "month")
# 2. Loop through and calculate the variance
for (m in 1:length(greater_50_list)){
  huc2 <- greater_50_list[m]
  
  # read in the yearly and datily data files
  setwd(daily_data_save)
  data_file <- paste0("HUC", huc2, "daily_averages.csv")
  daily_file <- read.csv( file = data_file, stringsAsFactors = FALSE)
  
  daily_file <- daily_file %>% filter(all_years >=1980 & all_years < 2020) 
  all_years2 <- unique(daily_file$all_years)

  #### 3. Loop Through Pull Out Monthly Data, Calculate Variance and Then Plot ########
  daily_file$year_mon<- as.Date(paste( daily_file$year_mon, "-01-01", sep=""))
  variance_matrix <- matrix(data = NA, ncol =13, nrow = 40)
  counter =1
  for (j in 1:12){
    filtered_month <- daily_file %>%  filter(month(year_mon) == j) # will need to fix if remove row names
    
    var_all <- c()
    for (l in 1980:2019){
      year_filter <- filtered_month %>% filter(year(filtered_month$year_mon) == l)

      variance <- var(year_filter$fraction, na.rm = TRUE)

      var_all <- c(var_all, variance)
    }
    variance_matrix[,counter] <- c(var_all)
    counter = counter+1
  }
  
  colnames(variance_matrix) <- c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                  "Nov","Dec", "all_years")
  
  variance_matrix <- as.data.frame ( variance_matrix, stringsAsFactors = FALSE)
  
  # 4. Remove outliers by ensuring that the values are within the +/- IQR*1.5
  for(g in 1:12){
    Q = quantile(variance_matrix[,g])
    iqr = IQR(variance_matrix[,g])
    up <-  Q[2]+1.5*iqr # Upper Range
    low<- Q[1]-1.5*iqr # Lower Range

    trial <- variance_matrix[,g]
    trial[trial<low] <- NA
    trial[trial>up] <- NA
    variance_matrix[,g] <- trial
  }
  test <- t(variance_matrix)
  test <- as.data.frame(test[1:12,], stringsAsFactors = F)
  colnames(test) <- seq(1980,2019, by =1)
  test$month <-c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                  "Nov","Dec")
  test2 <- melt(test, id.vars = "month")
  test2$variable <- basin_names[m]
  
  if(m==1){
    final_graphing <- test2
  }
  if(m>1){
    final_graphing <- rbind(final_graphing, test2)
  }
} 

#### 5. Plot Figure 4 ####
#Variance Boxplots

aridity_order <- c("a. Tennessee", "b. Lower Mississippi", "c. Ohio","d. South Atlantic","e. Sourris Red Rainy", 
                   "f. Arkansas White Red",
                   "g. Texas Gulf","h. Missouri","i. California","j. Upper Colorado", "k. Great Basin","l. Lower Colorado") 

ggplot(data = final_graphing, 
                           mapping = aes(x = factor(month, levels = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                       "Nov","Dec")), y = as.numeric(value), group = month) )+
    geom_boxplot(outlier.shape = NA)+
    facet_wrap(~factor(variable, levels = aridity_order), scales = "free")+
    ylab("Variance")+
    scale_fill_brewer( name = "Month", palette = "Paired")+
    xlab("Month")+
    theme_bw()+
    theme(axis.text.y = element_text(face = "bold", size =8), 
          axis.text.x = element_text(size=8, angle=90, face = "bold"), strip.text = element_text(face="bold"),
          axis.title.y = element_text(face = "bold"),
          axis.title.x= element_text(face = "bold"),
          strip.background = element_blank())

 
