################################################################################
# Description
############################################
# [PURPOSE]
# Preliminary weekly visualizations of Longitudinal Disease and Weather Data from 2000-2019.
# This script is extended for time series analysis using B-splines and Generalized Additive Models (GAMs).
# For Github viewers, this script is not reproducable due to confidentiality and dataset claims. Therefore I have also removed the analysis component
# I have uploaded this script just to demonstrate an example workflow of the data processing stage. 
#
# [AUTHOR]
# Samuel McEwan, QIMR Berghofer
#
# [DATE]
# 30.10.2020
#
# [DEPENDENCIES]
# Mostly our Typical Toolkit of packages. But c("gam", "tseries", "ggfortify", "e1071") may need to be installed.
# Running line 35 will report what packages need to be installed if any
#
# [INPUT]
# diseasedata_07_20_eb.xlsx
# Weather_EHF.xlsx

# [OUTPUT]
# Weekly_Plots/Weekly_[var].tiff"

# [OTHER]
# Set working directory to [confidential folder name] before running the script.


################################################################################
# Setup the Session
############################################
packages <- c("openxlsx", "magrittr", "dplyr", "lubridate", "stringr", "splines", "gam", "dlnm", "tseries", "ggfortify", "e1071", "epiDisplay", "lattice", "lmtest", "moments", "ggplot") # "haven", "tidyverse" 
packages[!packages %in% row.names(installed.packages())] # Checks required packages are installed. Should report "character(0)". Otherwise any listed packages need to be installed. 
invisible(lapply(packages, library, character.only = TRUE))

rm(list = ls)

source("Utilities.R")

################################################################################
# Load the data, drop duplicates and format ready for analysis
############################################

#Read-in the data
data <- read.xlsx("diseasedata_07_20_eb.xlsx", sheet = "Data") %>% filter(!duplicated(NOTF_ID)) 
weather_dat <- read.xlsx("Weather_EHF.xlsx", sheet = "Sheet1")
names(data) <- str_to_title(names(data))  # This is just preference. Changes column names of form "COL_NAME" to form "Col_name" which is easier on the eyes/nicer to code

# Format dates
data$Onset_date <- convertToDate(data$Onset_date)
weather_dat$Onset_date <- convertToDate(weather_dat$Date)

# Set 999 values as missing
lapply(c("Precipitation", "MaxTemp", "MinTemp", "Humidity0900", "Humidity1500"), FUN = function(x){weather_dat[weather_dat[,x] == 999, x] <<- NA} )

# Collapse Salmonella Category and Collapse Shigella Category
unique(data$Disease_name)
data$Disease_name[grep("SALMON", data$Disease_name)] <- "Salmonella"
data$Disease_name[grep("SHIGEL", data$Disease_name)] <- "Shigella"
unique(data$Disease_name)

# Create a Date_to_Week reference dataframe. The codebook is created by first generating a sequence of dates 
# starting on the last Monday prior to 2000, and the first Sunday after 2020 
codebook <- data.frame(week = NA, Onset_date = seq(from = ymd("1999-12-27"), to = ymd("2020-01-05"), by = "days"), day_name = NA)
codebook$day_name <- weekdays(codebook$Onset_date)
for(i in 1:ceiling(nrow(codebook)/7)){
  codebook$week[(7*i-6):(7*i)] <- paste("Week", i)
}
codebook[1:21,] # Perfect, the weeks are starting on Monday and ending on Sunday


################################################################################
# Add week information to Weather and Disease datasets and collapse by week. 
# Then merge the two collapsed datasets. This seems like an odd workflow but
# duplication issues arise if the datasets are not collapsed independently
############################################

# Add week information to Weather data and collapse by week
collapsed_weather <- merge(weather_dat, codebook, by = "Onset_date") %>% group_by(week) %>% summarize(
  start_week = min_with_na(Onset_date),  # Extracts the first dates for the first day of each week. Valid due to no missigness for the variable Onset_date.
  Weekly_Precipitation = sum_with_na(Precipitation),
  MaxTemp = max_with_na(MaxTemp),
  MinTemp = min_with_na(MinTemp),
  Mean_Humidity0900 = mean(Humidity0900, na.rm = T),
  Mean_Humidity1500 = mean(Humidity1500, na.rm = T),
  Mean_ehfl = mean(ehfl, na.rm = T),
  Mean_EHF_Severity = mean(EHF_Severity, na.rm = T),
  HeatWave = sum_with_na(HeatWave == "HW"),
  EHF_SeverityCat = paste0(unique(EHF_SeverityCat), collapse = "|")
) %>% as.data.frame

head(collapsed_weather)

# Recode the HeatWave variable, and extract the most severe EHF_SeverityCat value for each week. 
collapsed_weather$HeatWave[which(collapsed_weather$HeatWave > 0)] <- "Yes"
collapsed_weather$HeatWave[which(collapsed_weather$HeatWave == "0")] <- "No"
collapsed_weather$EHF_SeverityCat[grep("severe|extreme", collapsed_weather$EHF_SeverityCat)] <- "severe or extreme"
collapsed_weather$EHF_SeverityCat[grep("low", collapsed_weather$EHF_SeverityCat)] <- "low"

# Remove first and last week due to weather data not being availabe for the start of week 1 (previous year) and absent for the last week (next year)
nrow(collapsed_weather)
collapsed_weather <- collapsed_weather[!collapsed_weather$week %in% c("Week 1", "Week 1045"),]
nrow(collapsed_weather)

# Add week information to Disease data and collapse by week
collapsed_disease <- merge(data, codebook, by = "Onset_date") %>% group_by(week) %>% summarize(
  Weekly_Campy_count = sum(Disease_name == "CAMPYLOBACTER ENTERITIS"),  ###### NAs treated as 0
  Weekly_Salmonella_count = sum(Disease_name == "Salmonella"),          
  Weekly_Crypto_count = sum(Disease_name == "CRYPTOSPORIDIOSIS"),       
  Weekly_STEC_count = sum(Disease_name == "E COLI, SHIGA-LIKE TOXIN (SLTEC, VTEC, EHEC)"),
  Weekly_Listeria_count = sum(Disease_name == "LISTERIOSIS"),  
  Weekly_Yersinia_count = sum(Disease_name == "YERSINIOSIS"),
  Weekly_Shigella_count = sum(Disease_name == "Shigella"),  
  Weekly_All_count = sum(Disease_name != "CRYPTOSPORIDIOSIS")           
) %>% as.data.frame

head(collapsed_disease)

# Merge Collapsed Disease and Weather data
merged <- merge(collapsed_disease, collapsed_weather, by = "week")

# Month variable to look at seasonal trends
merged$month <- month(merged$start_week)  # Note that a week can overlap two months. For simplicity, here the month variable is assigned according to the first day of the week (Monday) 

# sort the variable order so that modelling can be easily completed in bulk runs
colnames(merged)
merged <- merged[order(merged$start_week), c(1,10, 20, 2:9, 11:19)]
colnames(merged)


###########################################################################
## Visualisation of the data
#########################################

library("epiDisplay") ## shifted above

des(merged)
summ(merged)
table(merged$HeatWave) ## Good numbers to examine
table(merged$EHF_SeverityCat) 

## Descriptive statistics (variance and sample size)
sapply(paste0("Weekly_", c("All", "Crypto", "Salmonella", "Campy"), "_count"), function(x){
  c(var = var(merged[,x], na.rm = T), n = sum(merged[,x] != 0))})

# This suggests we need to explore a negative binomial model for all?  Consider a zero-inflated negative binomial for crypto and there seems like an excess of zeros

# Correlations
cor(merged[,4:18], use = "pairwise.complete.obs")

## Salmonella has a relationship with Campo and crypto
## Salmonella, Campo and Crypto have a relationships with Min/Max Temp and ehfl/EHF severity

# Histogram for weather data
lapply(c("Weekly_Precipitation", "MaxTemp", "MinTemp", "Mean_Humidity0900", "Mean_Humidity1500", "Mean_ehfl", "Mean_EHF_Severity"), function(x){merged[,x] %>% histogram(., main = paste("Distribution of", x, "Using Weekly Data"), xlab = x)})

# Sharpio wilks test
sapply(c("Weekly_Precipitation", "MaxTemp", "MinTemp", "Mean_Humidity0900", "Mean_Humidity1500", "Mean_ehfl", "Mean_EHF_Severity"), function(x){
  c(skewness = skewness(merged[,x], na.rm = T),  shapiro.test = shapiro.test(merged[,x])$p.value)
}) %>% round(4)

## Plots

# Define a function that saves all raw longitudinal plots as .tiff files in a folder named "raw_plots" located in the current working directory
tiff_plot_week <- function(var){
  oldpar <- par(no.readonly=TRUE)
  tiff(filename = paste0("Weekly_Plots/Weekly", var, ".tiff"), height = 12, width = 20, units = 'cm', compression = "lzw", res = 300)
  plot(merged$start_week, merged[, var],
       xlab="", ylab = var, t="b",  lty=1, xaxt="n")
  title(xlab = "Date", line = 4)
  axis.Date(1, at=seq(as.Date("2000-01-01"), as.Date("2019-01-01"), by = "6 mon"), format="%m-%Y", las=2)
  abline(v=seq(as.Date("2000-01-01"), as.Date("2019-01-01"), by = "12 mon"),col=grey(0.6),lty=2)
  par(oldpar)
  dev.off()
}

# Generate and save all plots in a file named "raw_plots" in the local directory
lapply(names(merged)[4:18], tiff_plot_week)

