# ResOpsUS_Analysis 
*Developed by Jennie C. Steyaert and Laura E. Condon to analyze historical reservoir data from ResOpsUS (https://doi.org/10.5281/zenodo.5367383)*

## Description
This respository contains all the codes necessary to replicate the analysis in Steyaert & Condon, 2022. Each code contains numbered steps that explain what the chunk of code is doing. Some codes, such as that necessary to create Standardized Streamflow Indices (SSI values) contains more detailed notes on what exactly each line does and where specific values are coming from.

## Citation
[INSERT WHEN I HAVE IT]

## This GitHub Respository contains three folders based on each section of the paper and the corresponding data:
1. Data
2. Fraction_filled_calculations
3. Seasonal_and_trend_analysis_plots
4. Hydrologic_drought_sensitivity 

### 1. Data 
*The data folder contains three subfolders.* 

The first is titled "Fraction_Filled" which contains a file for each dam that has a four column data table with date, storage in MCM, GRanD storage capacity, and fraction. For each dam the storage capacity stays constant, but the storage and fraction columns may be missing data depending on if the storage value at that date existed in ResOpsUS.

The second is titled "HUC_FF." This folder contains one file for each of the 18 regions in the Contiguous United States that contains the fraction filled timeseries. 

The third is titled "SSI_HUC" and contains the average SSI timeseries for each of the 18 regions in the US.

### 2. Fraction filled calculations
This folder contains the codes to calculate the fraction filled time series for each dam and for each HUC. The daily fraction filled code compiles all the data for each dam and then the Full_HUC2_Analysis recomplies that data on the regional level.

### 3. Seasonal and trend analysis plots
This folder contains the codes necessary to calculate the metrics used to compare across regions. Specifically, there are codes that calculate and plot the seasonal medians, ranges and variances as well as codes that calculate the Sens slopes and trend analysis. Each of these metrics have their own codes, however, the seasonal range and range trend as to not have repetative codes. 

### 4. Hydrologic drought sensitivity
This folder contains the codes necessary to determine if a streamgage is a reference gage or not using the GagesII dataset, the calculation code for SSI based on Serrano & Vincente 2012, and the drought analysis code. As with the previous folders, the analysis and graphing are grouped together.

