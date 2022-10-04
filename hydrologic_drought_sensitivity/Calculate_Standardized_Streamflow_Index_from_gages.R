#Jen Steyaert
#Calculate SSI from streamgages
#7-20-2020

#######
# Loop through the data and gather the timeseries data from USGS
######
library(dataRetrieval)
library(lubridate)
library(stats)
library(MASS)
library(fitdistrplus)
library(xts)
library(lmomco)
library(evd)
library(actuar)
library(tidyverse)


#####
#Reference Gages
#####


# Read in the file with the USGS reference gages for each HUC2 region
setwd("~/Desktop/github/ResOpsUS_Analysis/data/other_data") #where the file of streamgages lives
reference <- read.csv(file = "HUC2_ref_gage_70%_covered.csv", stringsAsFactors = FALSE)

# Create matrix and list of all the gages
compare <-matrix(data = NA, nrow = nrow(reference), ncol =  5) #make a matrix to compare everything
siteno <- reference$Station.ID #list of all the site numbers that we need

# Where you want to save the reference gage data
directory <- "~/Desktop/CONUS_INFO/Gages_HUC4/SSI_Drainage_Area/" # Change this for your directory 

i=1 #counter for the comparison matrix
for (g in 1:length(siteno)){ #loop through site numbers
  print(g) #print where in loop you are
  if (is.na(g) == TRUE){
    next
  }
  siteNo1 <- as.character(siteno[g]) #will need to add another zero in front for the bigger loop
  print(siteNo1) #print site number you are working with
  if(is.na(siteNo1) == TRUE){ #if there is no site number
    next
  }
  if(is.na(siteNo1) == FALSE){
    if (nchar(siteNo1) <8){ #if you are missing the zero in front, fill it in
      siteNo1 <- paste0("0", siteNo1)
    }
  pCode <- "00060" #discharge code from NWIS
  start.date <- "1970-01-01" #start date for when you want to start calculating
  end.date <- "2019-12-31" #end date to stop, I kept this as a annual year but you could do water year too
  data2 <- readNWISdv(siteNumbers = siteNo1, #this section reads in the streamgage inflow
                   parameterCd = pCode,
                   startDate = start.date,
                   endDate = end.date)
  
  data2 <- renameNWISColumns(data2) #this renames the columns to values that make sense
  print ("read in data")
  
  #####
  # Loop through and pull out data by year
  #####

  data2[,3] <- as.Date(data2[,3]) #make the date column dates
  time_series <- data.frame("DATE" = data2[,3], "FLOW" = data2[,4]) #pull out just the date and the discharge because this is what we care about
  order <- xts(time_series, time_series$DATE) #order the dates
  monthly_date2<- apply.monthly(order, mean, na.rm = T) #order the dates by month
  all_dates <- as.yearmon(time_series[,1]) #pull all the yearmon dates into a vector (yearmon = 1994JAN)
  dates <- unique(all_dates) #find all the unique months and years

  organized_data <- data.frame("DATE" = dates, "AVG_FLOW" = monthly_date2[,2]) #create a matrix with the organized data (ie all the January)
  years <- unique(year(organized_data[,1])) #find all the unique years of the month (ie 1950, 1951....2019)
   print("pulled out years")
  
   #######
  # Make matrix with all the values
  ######

  matrix_values <- matrix(data = NA, nrow= length(years), ncol = 1) #create matrix to hold the data for each month
  matrix_values[,1] = c(years) #put all the years in as a row
  matrix_values <- data.frame("DATE" = matrix_values[,1])
  
  #Reformat the data to pull out the data of the organized data based on the month
  Jan_col <- organized_data[format(organized_data[,1], "%B") == "January",] 
  Feb_col <-organized_data[format(organized_data[,1], "%B") == "February",]
  Mar_col <- organized_data[format(organized_data[,1], "%B") == "March",]
  Apr_col <- organized_data[format(organized_data[,1], "%B") == "April",]
  May_col <- organized_data[format(organized_data[,1], "%B") == "May",]
  Jun_col <- organized_data[format(organized_data[,1], "%B") == "June",]
  Jul_col <- organized_data[format(organized_data[,1], "%B") == "July",]
  Aug_col <- organized_data[format(organized_data[,1], "%B") == "August",]
  Sept_col <- organized_data[format(organized_data[,1], "%B") == "September",]
  Oct_col <- organized_data[format(organized_data[,1], "%B") == "October",]
  Nov_col <- organized_data[format(organized_data[,1], "%B") == "November",]
  Dec_col <-organized_data[format(organized_data[,1], "%B") == "December",]

  #####
  #Change the date of each column to just be a year so we can put it into the bigger matrix
  ####
  
  Jan_col[,1] <- year(Jan_col[,1])
  Feb_col[,1] <- year(Feb_col[,1])
  Mar_col[,1] <- year(Mar_col[,1])
  Apr_col[,1] <- year(Apr_col[,1])
  May_col[,1] <- year(May_col[,1])
  Jun_col[,1] <- year(Jun_col[,1])
  Jul_col[,1] <- year(Jul_col[,1])
  Aug_col[,1] <- year(Aug_col[,1])
  Sept_col[,1] <- year(Sept_col[,1])
  Oct_col[,1] <- year(Oct_col[,1])
  Nov_col[,1] <- year(Nov_col[,1])
  Dec_col[,1] <- year(Dec_col[,1])
  print("added_years")

  #####
  #Merge all the flow data into the matrix_values dataframe based on the organization done previously
  # columns start in January and go to December
  #####
  
  matrix_values <- merge(matrix_values, Jan_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Feb_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Mar_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Apr_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, May_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Jun_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Jul_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Aug_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Sept_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Oct_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Nov_col, by = "DATE", all = TRUE)
  matrix_values <- merge(matrix_values, Dec_col, by = "DATE", all = TRUE)
  
  #make this into a dataframe that we can write out/ know what columns are what
  matrix_values <- data.frame("Year" = matrix_values[,1], "Jan" = matrix_values[,2], "Feb" = matrix_values[,3],
                         "Mar" = matrix_values[,4], "Apr"= matrix_values[,5], "May" = matrix_values[,6],
                         "June"= matrix_values[,7], "July" = matrix_values[,8], "Aug" = matrix_values[,9],
                         "Sep" = matrix_values[,10], "Oct"= matrix_values[,11], "Nov"= matrix_values[,12],
                         "Dec" = matrix_values[,13])
  
  #create matrix to hold the SSI values, should have one value per flow
  SSI_matrix <- matrix(data = NA, nrow = nrow(matrix_values), ncol =ncol(matrix_values))
  SSI_matrix[,1]<- c(years) #add all the years to the first column
  print("finished oragnizing big average matrix")

  ####
  # Start SSI calculation based on Serrano et al 2012
  ####
  
  # These are all constants from the paper
  C0 = 2.515517
  C1 = 0.802853
  C2 = 0.010328
  d1 = 1.432788
  d2 = 0.189269
  d3 = 0.001308
  
  for (m in colnames(matrix_values)){ #loop through all the columns and calculate SSi
    if (m == "Year"){ #skip the year column
      next
    }
    #print(m)
    cols = which(colnames(matrix_values) == m) #find out which column equals the month we are
    flow  <- as.numeric(matrix_values[,cols], na.rm = T) #pull out all the flow values
  
    #####
    # Fill in the 0's with a super small number
    ####
  
    nums <- which(flow == 0) #ind all the zero values in the flow. This is important because it causes errors if you have too many
    s=0 # initialize counter
    if( length(nums) != 0){
      for ( n in nums){ #loop through those values and count how many
        #flow[n] = 1*10^-10 #fill with small number anything smaller errors when I get to that one month that has all zeros
        s = s+1 #counter
        #print("filled in 0's")
      }
    }
    if(s > 35 ){ #if you have more than half of your values == 0, then we skip calculating SSI and you probably need a new streamgage
      mes <- paste0 (s, " number of zeros")
      SSI_matrix[,cols] <- c(mes)
      next
    }
  
    #####
    #Replace nan with NA
    ####
    
    nan <- which(is.na(flow)) # find instances of na values
    if(length(nan) != 0){
      flow <-flow[!is.na(flow)] # loop through and replace nan with NA. we can work with NA but not nan
    }
    
    if(length(flow) <4){
      next
    }
    #Beginning of the Statistical Analysis to create z scores for the flow
    
    histogram <- hist(flow) # create histogram of the flow from the month we are
    plotdist(flow, histo = TRUE, demp = TRUE) #plot that histogram with the CDF, and PDF functions
    
    #flow[flow <0] <- 1*10^-10 # log log cannot handle negatives
    if (length(which(is.na(flow)) > 0.5*length(flow))){
      next
    }
    
    ####
    # Get all the different statistical indices
    ####
    
    values <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) #values for the quantiles. I chose intervals of 10%
    quantiles <- quantile(flow, values,na.rm = T) #find the quantiles for the flow
    test <- unique(quantiles)
    if (length(test) >1){
      lmom_data <- lmoms(quantiles) #find the lmoment ratio of the quantiles
      t3_data <- lmom_data$ratios[3] #this is the tau 3 l moment ratio
      compare[i,2] <- t3_data #put the tau 3 lmoment into the comparison matrix
      t4_data <- lmom_data$ratios[4] #this is the tau 4 lmoment ratio
      compare[i,3] <- t4_data #put tau 4 into the comparison
    }
    
    ####
    #Fit Distribution to the Data
    ####
    
    fit_gev <- fgev(flow, std.err = FALSE) #fit the gev distribution to the data. GEV has limitations with extreme values, but is a good general metric
    compare[i,1] <- siteNo1 #add the site number into the comparison matrix
    quantile_gev <- qgev(values, loc = fit_gev$estimate[1], scale = fit_gev$estimate[2], shape = fit_gev$estimate[3]) #find the quantiles of the gev
    lmom_gev <- lmoms(quantile_gev) #find hte l moments of the gev
    compare[i,4] <- lmom_gev$ratios[3] #put tau 3 into the comparison
    compare[i,5] <- lmom_gev$ratio[4] # ptu tau 4 into the comparison
      
    
    
    if (g == 286| g == 1010| g== 1008 | g == 1004 | g== 1006 |g == 923 |g == 737|g == 984| g == 994 |g == 991| g == 981 | g == 990|g == 974 |g == 929|g == 979 |g == 949| g == 879|g == 943| g == 918 | g == 919| g == 898| g == 899| g == 896| g == 897 | g == 861| g == 893 |g == 891| g == 889 |g == 880| g == 887| g == 842| g == 870| g == 844 | g == 762 |g == 771 |g == 738 | g == 743 |g == 572 | g==576| g == 590 |g == 650 | g == 603|g == 645| g== 625 |g == 638 |g ==637| g == 641){ # mle can't estimate params so i skipped
      #next
      print(g)
    }else{
  
      ###
      # Calculate SSI for the index using the one that fit the best
      ###
      
      mean_flow <- mean(flow, na.rm = T) #find the mean of the monthly flow
      std_flow <- sd(flow, na.rm =T) #find the standard deviation of the monthly flow
      for (l in 1:length(flow)){
        if(is.na(flow[l]) == TRUE){ #if the flow is NA skip. Na also causes errors
          SSI_matrix[l,cols] <- NA
          next()
        }

        itemp = which(flow[l] == matrix_values[,cols]) #match the flow to the right row
        
        prob = pgev(as.numeric(matrix_values[l, cols]), loc = fit_gev$estimate[1], scale = fit_gev$estimate[2], shape = fit_gev$estimate[3]) #find the quantiles of the gev
        P <- 1-prob #this gives probility of exceedence which we use to calculate the SSI
        
        if (is.na(P) == TRUE){
          next
        }
        if(P <= 0.5){
          W = sqrt(-2*log10(P)) #equation from Serrano et al 2012
        }
        if (P>0.5){
          P2 = 1-P
          W = sqrt(-2*log10(P2)) #equation from Serrano et al 2012
        }
        
        SSI = -1*(W- ((C0+C1*W+C2*W^2)/(1+d1*W+d2*W^2+d3*W^3))) #SSI index calculation from Serrano et al 2012
        SSI_matrix[l,cols] <- SSI #put that SSI value into the matrix
        
        ggplot(data = as.data.frame(SSI_matrix), mapping = aes(x = V13))+
          geom_histogram()
      }
    }
  }

  #####
  # Write out SSI matrix for each Gage with all the monthly values
  ####
  setwd(directory)
  ID <- paste0(siteNo1, "_SSI_average_ref_HUC2.csv" )
  final <- data.frame("Year" = SSI_matrix[,1], "Jan" = SSI_matrix[,2], "Feb" = SSI_matrix[,3],
                      "Mar" = SSI_matrix[,4], "Apr"= SSI_matrix[,5], "May" = SSI_matrix[,6],
                      "June"= SSI_matrix[,7], "July" = SSI_matrix[,8], "Aug" = SSI_matrix[,9],
                      "Sep" = SSI_matrix[,10], "Oct"= SSI_matrix[,11], "Nov"= SSI_matrix[,12],
                      "Dec" = SSI_matrix[,13])
  write.csv(final, ID, row.names = F, col.names = F)
  #print("Wrote SSi matrix")
  
  i = i+1 #counter for the comparison matrix
  }
}

