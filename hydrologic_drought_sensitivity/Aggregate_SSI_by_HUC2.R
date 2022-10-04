#Jen Steyaert
#Pull out specific characteristics and gages
#7-20-2020


#Read in Gages
library("tweedie")
library("dplyr")

directory <- "~/Desktop/github/ResOpsUS_Analysis/data/SSI_HUC" # Where you want to save data
ssi_directory <- "~/Desktop/CONUS_INFO/Gages_HUC4/SSI_Drainage_Area/" # Where you saved the reference gage data in Calculate_Standardized_Streamflow_code

# Read in the reference gages from the GagesII dataset hosted by the United States Geological Survey that have
# Over 70% of their record covered from 1980 - 2019

setwd("~/Desktop/github/ResOpsUS_Analysis/data/other_data") 
gages <- reference <- read.csv(file = "HUC2_ref_gage_70%_covered.csv", stringsAsFactors = FALSE) #

setwd(ssi_directory) 
all_SSI <- list.files(pattern = "_SSI_average_ref_HUC2.csv")
ssi_list <- gsub("_SSI_average_ref_HUC2.csv", "", all_SSI) # rename the directory name at the beginning

#create list of unique HUC 4 values
HUC2_list <- unique(gages$HUC2, rm.na = TRUE) #Should pull from all the HUC2s

for (i in HUC2_list){ #loop through HUC4's to find the right gage
  
if(is.na(i) == TRUE){
  next
}
  filtered <- gages %>% filter(HUC2 == i)   #filter the gages by the HUC
  siteno <- filtered$Station.ID
  
  for (g in 1:length(siteno)){ #loop through site numbers
    print(g) #print where in loop you are
    siteNo1 <- as.character(siteno[g]) #will need to add another zero in front for the bigger loop
    print(siteNo1) #print site number you are working with
    if(is.na(siteNo1) == TRUE){ #if there is no site number
      next
    } #if is.na == TRUE
    if(is.na(siteNo1) == FALSE){
      if (nchar(siteNo1) <8){ #if you are missing the zero in front, fill it in
        siteNo1 <- paste0("0", siteNo1)
      }
    }
    
    
    file_index <- which(siteNo1 == ssi_list)# find index for the file
    if(length(file_index) ==0){
      next
    }
    setwd(ssi_directory)
    ssi_data <- read.csv(file = all_SSI[file_index],stringsAsFactors = F)
    
    # reorganize the data so it is in monthly with three columns (year, month, value)
    total_rows <- nrow(ssi_data)
    
    for(p in 1:total_rows){ # doing a for loop because I can't think about a way to automate
      row_values <- ssi_data[p,]
      year <- row_values$Year
      month_data <- t(row_values[2:13])
      if(p ==1){
        three_col_data <- data.frame("Year" = year, "Month" = seq(1,12, by =1), "SSI" = month_data[,1], 
                                     stringsAsFactors = F)
      }# if(p ==1)
      if(p>1){
        month_data_values <- cbind(year, seq(1,12,by =1), month_data)
        colnames(month_data_values) <- c("Year", "Month", "SSI")
        three_col_data <- rbind(three_col_data, month_data_values)
      }#if(p>1){
      
    } # (p in 1:length(total_rows)
    three_col_data$SSI[grepl("number of zeros", three_col_data$SSI)] <- NA
    three_col_data$SSI <- as.numeric(three_col_data$SSI)
  three_col_data$date<- as.character(paste(three_col_data$Year, "-", three_col_data$Month, "-01"))
  three_col_data$date <- as.Date(three_col_data$date, format = "%Y - %m - %d")
  if (g ==1){
    final_values <- three_col_data[,3:4]
  }else{
    final_values <- merge(final_values, three_col_data[,3:4], by = "date", all = T)
  }
} # for g in siteno list

  # sum all the normalized values into one data frame for each HUC2
  huc2_values <- rowMeans(final_values[,2:ncol(final_values)], na.rm = T)
  huc2_ssi_mean <- data.frame("Date" = final_values$date, "Average_SSI" = huc2_values)
  
  setwd(directory)
  file_name <- paste0(i, "_avg_ssi.csv")
  write.csv(file = file_name, huc2_ssi_mean, row.names = F)
}# i in HUC2_list



