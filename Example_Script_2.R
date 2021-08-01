######################################################################
# Alkema Analyses Syntax - Sam McEwan
# Working Directory: L:\Lab_Stats_Unit\Louise\Sam and Louise
# Date Last Modified: 2/12/2020

# This syntax generates infectivity and gametocyte dynamics results based on 
# the alkema dataset "CHMI-trans2_dataset_DRYAD.xlsx." 

# Key results include:
# -	Times to patency, max and various gametocyte thresholds
# -	Maximum gametocytaemia value overall and within specified time intervals
# -	Profile of Male and Female Gamecytaemia depicted via Spaghetti plots
# - Slope estimates to characterize the natural decay of gametocytaemia over time
# -	Comparison of infectivity rates between types of feedings assays and time of feeding assay
# - Reapeated Measures Correlations between Male and Female Gametocytaemiass

# These results are reported in the word doc currently named "Syntax Alkema ReadMe_LM_SM.docx"

# The code is partitioned into chunks corresponding to each labeled table/figure.
# Please read the descriptions of each code segment prior to execution

######################

# Load in a toolkit of useful packages. Can check which packages need installing by running packages[!packages %in% row.names(installed.packages())] 
packages <- c("xlsx", "exact2x2", "Rmisc", "ggplot2", "magrittr", "dplyr", "tidyr", "foreign", "stringr", "reshape2")
invisible(lapply(packages, library, character.only = TRUE))

rm(list = ls())

# Read in the Alkema Data. Filter IBSM data only (cohort 2)
d <- read.xlsx("CHMI-trans2_dataset_DRYAD.xlsx", sheetIndex = 1) %>% filter(Cohort == 2)
d$Treatment <- mapvalues(d$Treatment, from = c(1,2), to = c("PHP", "SP"))
d[,grep("Asex_", names(d))] <- apply(d[,grep("Asex_", names(d))], MARGIN = 2, function(x){as.numeric(as.character(x))})


######################################################################
# Table 1 - Times to Patency, 300 gams, 400 gams

# Notes:
# - Times to various thresholds need to be derived from the dataset.
# - Due to the wide data format, this is non-trivial and therefore the function time_to_thresh() is created
#   The function first extracts the relevant longitudinal data by searching the columnspace
#   for named variables matching a particular biomarker, and then produces row-wise time to thresh summaries
#   for each individual
# - The function IQR_table assists in reporting time to patency. The function outputs Median (IQR) in character format as a single element, instead of a vector
# - I tend to use the function Sapply() whenever I can. Sapply is similar to looping, but with the advantage of automatically
#   ouputting tables stratified in row/column direction based on the iterator
# - The use of "%>%" is known as piping. Piping aids in an easy development process and readibility. 
#   Eg -   round(mean(rnorm(100)),3) is the same as rnorm(100) %>% mean %>% round
#   Note that for functions which require multiple arguments a decimal '.' is required to reference the 
#   output of the previous step in the pipeline
#   Eg - rnorm(100) %>% mean(.) round(.) gives the same results as above but use of '.' is not necessary
#        grep(paste0("CCp4_[0-9]"), names(d)) %>% d[,.] uses the '.' to reference the names of columns identified by the grep().
# - Individuals that reach the gametocyte threshold are distinguishable to individuals that did not reach the threshold by
#   a non NA time to thresh. 
#   Therefore times_to_thresh(x,y) %>% na.omit %>% {100*length(.)/12} reports the percentage of individuals that reached the gametocyte threshold

######################
times_to_thresh <- function(biomarker, thresh){
  apply(grep(paste0(biomarker, "_[0-9]"), names(d)) %>% d[,.], MARGIN = 1, FUN = function(x){
    ifelse(all(x < thresh), yes = NA, 
           no = which(x > thresh) %>% min(na.rm = T) %>% names(x)[.] %>% gsub(".*_", "", .))
  }) %>% as.numeric
}

IQR_table <- function(x, n){
  q <- quantile(x, na.rm = T) %>% round(n)
  paste0(q["50%"], " (", q["25%"], ", ", q["75%"], ")")
}

sapply(c("CCp4", "PfMGET"), function(x){
  sapply(c(6, 300, 400), function(y){
    times_to_thresh(x,y) %>% na.omit %>% {100*length(.)/12} %>% round(0) %>% c(
      times_to_thresh(x,y) %>% IQR_table(3)) 
  })
}) %>% t %>% 'colnames<-'(paste0(c("(%)Reached_", "Time_to_"), rep(c(6,300,400), each = 2)))


######################################################################
# Table 2 - Maximum Gametocytaemia and time to Max

# Notes:
# - Times to max gametocytaemia needs to be derived from the dataset
# - The first block of code identifies time to max via first calculating the maximum 
#   gametocytaemia for each individual, relating that to it's respective column name (eg - "CCp4_21")
#   and then extracting the day (21) by removing character text prior to the "_"
# - The function BT_CI_table assists in reporting maximum gametocytaemia.  
#   The function outputs Back Transformed 95% Confidence Intervals as mean (CI) rounded to 'n' decimal places

######################
sapply(c("CCp4", "PfMGET"), FUN = function(biomarker){
  apply(grep(paste0(biomarker, "_[0-9]"), names(d)) %>% d[,.], MARGIN = 1, FUN = function(x){
    which.max(x) %>% names(x)[.] %>% gsub(".*_", "", .)
  }) %>% as.numeric %>% IQR_table(., 3)
}) 

BT_CI_table <- function(x, n){
  conf <- 10^CI(log(x[!is.na(x)], base = 10)) %>% round(n) 
  paste0(conf[2], " (", conf[3], ", ", conf[1], ")")
}

sapply(c("PeakCCP4", "PeakPfMget"), FUN = function(peak){
  c(Max_Gam_IQR = IQR_table(d[,peak], 0),
    Max_Gam_CI  = BT_CI_table(d[,peak], 0))
}) 


######################################################################
# Table 3 - Maximum Gam values <20, 20-22, >22, 19-23 days PI

# Notes:
# - The function max_int is first generated to identify the maximium gametocytaemia over a specified time interval. It does this by 
#   first identifying timepoints for which gametocytaemia was measured, storing in the object named 'testdays'
#   The function then identifies the timepoints between day 'a' and 'b' (inclusive) and then extracts the relevant data
#   The row-wise maximum is identified for each individual and then summarized across all individuals, reporting 
#   a back transformed log10 Confidence interval
# - Note that max_int is inclusive of a and b. Therefore  max_int(biomarker, 19.9, 22.1) is the same as max_int(biomarker, 20, 22)

######################
max_int <- function(biomarker, a, b){
  testdays <- grep(paste0(biomarker, "_[0-9]"), names(d)) %>% names(d)[.] %>% gsub(".*_", "", .) %>% as.numeric
  between(testdays, a, b) %>% testdays[.] %>% paste0(biomarker, "_", .) %>% d[,.] %>% as.data.frame %>% 
    apply(., MARGIN = 1, FUN = function(x) max(x, na.rm = T) ) %>% BT_CI_table(., 0)
}

sapply(c("CCp4", "PfMGET"), function(biomarker){
  c(lt20    = max_int(biomarker, -Inf, 19.9),
    d20to22 = max_int(biomarker, 20, 22),
    gt22    = max_int(biomarker, 22.1, Inf),
    d19to23 = max_int(biomarker, 19, 23),
    overall = max_int(biomarker, -Inf, Inf)
  )
}) %>% t


######################################################################
# Figure 1 - Gametocyte Dynamics Plot

# Notes:
# - Firstly, the wide data frame is pivoted to a long format for easy graphing. 
#   Gametocyte values are mutated such that all values below LLOR are set to 1. 
#   This is important primarily due to reducing the difference in order between the maximum and minimum log10(gam value)
#   Consequent plots will be more appealing due to starting at 1 on the log scale (as opposed to axes such as 0.01, 0.1, 1, 10, 100, 1000)
# - The melt() function collapses the CCp4, PfMGET and Asex variables into one long variable with an accompanying label column.
#   The perk of this long format is there is no need to 'overlay' several plots, because CCp4, PfMGET and Asex are viewed as the same variable,
#   just coloured and grouped differently using ggplot's aes() calls.
# - A median trendline based on point-wise median estimates is created using stat_summary

######################
plot_data <- d %>% select(StudyCode, starts_with(c("Asex_", "PfMGET", "CCp4"))&!contains("min")) %>% 
  pivot_longer(contains(c("Asex_", "PfMGET_", "CCp4")),
               names_to = c(".value", "day"),
               names_sep = "_"
               ) %>% 
  as.data.frame %>% 
  mutate(day = as.numeric(day), StudyCode = as.factor(StudyCode),
         PfMGET = ifelse(PfMGET > 6, PfMGET, 1), 
         CCp4 = ifelse(CCp4 > 6, CCp4, 1)
         ) %>% 
  melt(id.vars = c("StudyCode", "day")) %>% filter(!is.na(value))

ggplot(plot_data %>% filter(variable != "Asex"), aes(x = day, y = value, color = variable, group = paste0(StudyCode, variable)))+#geom_point(size = 0.5)
  geom_line(size = 0.01, alpha = 0.3) + scale_y_log10(limits = c(0.7, 4000)) + xlim(0,36) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"), 
        axis.title = element_text(size = 9), 
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8)) + labs(x = "Day", y = "Gametocyte Density (gams/ml)") +
  ggtitle("Alkema Gametocytaemia Over Time")+
  scale_color_manual(values=c("CCp4" = "orange", "PfMGET"="blue"))+
  stat_summary(fun = median, geom= 'line', size = 2, aes(group = variable), alpha = 0.9) + 
  stat_summary(fun = median, geom= 'point', size = 2, aes(group = variable)) 

######################################################################
# Table 4 - Median [IQR] for Gam Densities at each time point

# Notes:
# - Simple computation and tabulation of median (IQR) of Male and Female Gams at each timepoint
# - An ifelse statement needed to be used due to timepoints when Female Gametocytaemias were tested but not Male

######################
sapply(c("CCp4", "PfMGET"), FUN = function(biomarker){
  sapply(paste0(biomarker, "_", c(21, 22, 23, 24, 27, 29, 31, 34, 36)), FUN = function(var){
    ifelse(var %in% names(d), IQR_table(d[,var], 0), NA)
  }) 
}) %>% 'rownames<-'(gsub(".*_", "Day_", rownames(.)))


######################################################################
# Tables 5 and 6 - Slopes over various intervals between Day 21-36

# Notes:
# - For easier computation of the slopes and analysis of longitudinal trends, the data is first pivoted from wide 
#   to long data format using the pivot_longer() function. 
#   Note that only a subset of relevant variables used for analyses are stored in this pivoted long format dataset.
# - Slopes are calculated and reported on the log10 scale
#   The decay rate is summarized by median (IQR) overall and also for each treatment group (SP, PHP)
# - The output table is quite large. Therefore I write it as an xlsx file to copy and paste into word. (Will need to become more familiar with RMarkdown)

######################
long_data <- d %>% select(StudyCode, Treatment, starts_with(c("CCp4", "PfMGET"))&!contains("min")) %>% 
  pivot_longer(contains(c("CCp4_", "PfMGET_")), 
               names_to = c(".value", "day"),
               names_sep = "_"
  ) %>% as.data.frame %>% filter(day >= 21) %>% 
  mutate(day = day %>% as.character %>% as.numeric,
         log10_ccp4   = log(CCp4, base = 10),
         log10_pfmget = log(PfMGET, base = 10))


Slopes <- sapply(d$StudyCode, FUN = function(id){
  c(StudyCode = id,
    treat = long_data[long_data$StudyCode == id, "Treatment"][1],
    f_21to29_slope = coef(lm(log10_ccp4 ~ day, data = long_data %>% filter(StudyCode == id, between(day, 21,29))))[2] %>% round(3),
    f_21to36_slope = coef(lm(log10_ccp4 ~ day, data = long_data %>% filter(StudyCode == id, between(day, 21,36))))[2] %>% round(3),
    f_24to29_slope = coef(lm(log10_ccp4 ~ day, data = long_data %>% filter(StudyCode == id, between(day, 24,29))))[2] %>% round(3),
    f_24to31_slope = coef(lm(log10_ccp4 ~ day, data = long_data %>% filter(StudyCode == id, between(day, 24,31))))[2] %>% round(3),
    m_21to29_slope = coef(lm(log10_pfmget ~ day, data = long_data %>% filter(StudyCode == id, between(day, 21,29))))[2] %>% round(3),
    m_24to29_slope = coef(lm(log10_pfmget ~ day, data = long_data %>% filter(StudyCode == id, between(day, 24,29))))[2] %>% round(3)
  ) 
}) %>% t %>% as.data.frame %>% 'colnames<-'(gsub(".day", "", names(.)))

Slopes %>% select(contains("slope")) %>% apply(MARGIN = 2, FUN = function(x){IQR_table(as.numeric(as.character(x)),3)})
Slopes %>% group_by(treat) %>% summarize_at(vars(contains("slope")), function(x){IQR_table(as.numeric(as.character(x)), 3)})

merged <- merge(long_data, Slopes) %>% mutate_at(vars(contains(c("slope", "StudyCode"))), function(x){ifelse(duplicated(x), "", x %>% as.character %>% as.numeric)})
head(merged)
output <- merged[c(1:5,9:14)]; output

wb <- createWorkbook(type = "xlsx")
sheet <- createSheet(wb, sheetName = "Slope Data")
addDataFrame(output, sheet, row.names = FALSE)
setColumnWidth(sheet, 1:ncol(output), colWidth = 11) 

saveWorkbook(wb, "Slope Data.xlsx")


######################################################################
# Table 6 - Infectivity

# Notes:
# - Comparison between the three types of feeeding assays, stratified by day/timepoint, but not by treatment group
# - To avoid coding the linkage between assay 1,2,3 and days 21, 24, 29, tables stratified by Day were filled in by hand based on these results

######################
lapply(c("MACS", "Skin", "DMFA"), function(x){
  sapply(paste0(x, 1:3), function(y){
    c(Trans    = sum(d[,y] > 0, na.rm = T), 
      No_Trans = sum(d[,y] == 0, na.rm = T),
      Pc       = round(100*sum(d[,y] > 0, na.rm = T)/12, 0)
    )
  }) %>% t
}) %>% do.call(rbind, .)


######################################################################
# Table 7 - Infectivity

# Notes:
# - Comparison between the three types of feeeding assays, stratified by treatment group and day/timepoint
# - Note that given the decay rate is negligible and that assay and treatment are on Day 21 (..potentially earlier for PHP but no effect on transmission..),
#   an additional table should pool treatment arms at Day 21, and only consider the PHP arm for timepoints 24 and 29.

######################
lapply(c("PHP", "SP"), function(z){
  lapply(c("MACS", "Skin", "DMFA"), function(x){
    sapply(paste0(x, 1:3), function(y){
      c(Trans    = sum(d[d$Treatment == z,y] > 0, na.rm = T), 
        No_Trans = sum(d[d$Treatment == z,y] == 0, na.rm = T),
        Pc       = round(100*sum(d[d$Treatment == z,y] > 0, na.rm = T)/12, 0)
      )
    }) %>% t
  }) %>% do.call(rbind, .)
}) %>% do.call(cbind, .) %>% 'colnames<-'(paste0(rep(c("PHP.", "SP."),each = 3), rep(c("Trans", "No_Trans", "Pc"), 2)))



######################################################################
# Table 8 - Infectivity by Gametocytaemia at day 21

# Notes:
# - Comparison of infectivity rates for individuals that did/did not reach a threshold of 300 gametocytes for both Male and Female gams (specifically at day 21)
# - Pools together both the PHP and SP group, assuming that the feeding assay on Day 21 was prior to treatment
# - The filter !is.na(CCp4) and !is.na(PfMGET) is used to ensure logic such as PfMGET > 300 does not return TRUE when PfMGET is NA (R would do this by default)
# - Skin1 reports the proportion of infected mosquitoes from Skin Feeding Assay at Day 21. Note that despite adding a wide data format variable (d$Skin1) to a long data
#   format (long_data), the IDs and Transmission Rates may not necessarily match up. However both data formats are sorted  by ID so there is no issue here. 
# - table(....)[2:1, 2:1] reorders the rows/columns of the contingency table due to preference. The table was originally sorted with both negative at top left corner 
#   I prefer both positive at top left corner of table

######################
Day21_data <- long_data %>% filter(day == 21, !is.na(CCp4), !is.na(PfMGET)) %>% mutate(Thresh_300 = ifelse(CCp4 > 300 & PfMGET > 300, "Reached Thresh", "Did Not Reach Thresh"), SkinTrans = ifelse(d$Skin1 > 0, "Trans", "No Trans"))
tab <- table(Day21_data$Thresh_300, Day21_data$SkinTrans)[2:1, 2:1]; tab

tab %>% mcnemar.exact


######################################################################
# Table 8 - Repeated Measures Correlation between Gametocytaemia and Parasitaemia at same timepoint

# Notes:
# - Gametocytaemia values are required to be greater than the LLOR. This is important because measurement error becomes much more significant 
#   for low gametocyte values, especially when a log transformation is requried. 
#   (ie - The difference between 0.1 and 1 gametocytes is treated the same as the difference between 100 and 1000 gametocytes)
# - 


######################



######################################################################
# Table 9 - Repeated Measures Correlation between Gametocytaemia and Parasitaemia at timepoint 10 days out of phase

# Notes:
# - 
# - 


######################
rmcorr_data <- d %>% select(StudyCode, starts_with(c("Asex_", "PfMGET", "CCp4"))&!contains("min")) %>% pivot_longer(contains(c("Asex_", "PfMGET_", "CCp4")),
                                                                                                                             names_to = c(".value", "day"),
                                                                                                                             names_sep = "_"
) %>% as.data.frame %>% mutate(day = as.numeric(day), StudyCode = as.factor(StudyCode))

library(rmcorr)
rmcorr(participant = StudyCode, measure1 = log(CCp4, base = 10), measure2 = log(PfMGET, base = 10), dataset = rmcorr_data %>% filter(CCp4 > 6, PfMGET > 6))
rmcorr(participant = StudyCode, measure1 = log(Asex, base = 10), measure2 = log(CCp4, base = 10), dataset = rmcorr_data %>% filter(CCp4 > 6, day >= 18))
rmcorr(participant = StudyCode, measure1 = log(Asex, base = 10), measure2 = log(PfMGET, base = 10), dataset = rmcorr_data %>% filter(PfMGET > 6, day >= 18))
rmcorr(participant = StudyCode, measure1 = log(Asex, base = 10), measure2 = log(CCp4 + PfMGET, base = 10), dataset = rmcorr_data %>% filter(CCp4+PfMGET > 12, day >=18))


######################################################################
# Figures - Pairwise Correlation Plots

# Notes 
# - Variables used for pairwise correlation analyses are plotted against each other on the log10 scale
# - Each pairwise plot is first temporarily saved and then collectively generated via lapply(list(p1,p2,p3,p4), add_trendline)
# - To save repeated redundant code, add_trendline() is a function that overlays the log10 regression trendline in
#   addition to improving the formatting of the ggplots.
# - The same data filters are applied as when computing the respective repeated measures correlation coefficients
#   ie- >6 gams for plots involving PfMGET/CCp4, and >=18 days for plots involving 18s
# - Additional plots of Gametocytes vs Parasitaemia Level 10 days earlier may provide greater insight

######################

add_trendline <- function(p){p + geom_smooth(data = ggplot_build(p)$data[[1]], aes(x=10^x, y=10^y), method = 'lm') +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), axis.title = element_text(size = 11), axis.text = element_text(size = 8))
}

# CCp4 vs PfMGET
p1 = ggplot(rmcorr_data %>% filter(CCp4 > 6, PfMGET > 6), aes(x = PfMGET, y = CCp4)) + geom_point(size = 1.25) + 
  ggtitle("Log10_CCp4 vs Log10_PfMGET") + scale_y_log10() + scale_x_log10() 

# CCp4 vs Asex
p2 = ggplot(rmcorr_data %>% filter(CCp4 > 6, day >= 18), aes(x = Asex, y = CCp4)) + geom_point(size = 1.25) + 
  ggtitle("Log10_CCp4 vs Log10_Asex") + scale_y_log10() + scale_x_log10() 

# PfMGET vs Asex
p3 = ggplot(rmcorr_data %>% filter(PfMGET > 6, day >= 18), aes(x = Asex, y = PfMGET)) + geom_point(size = 1.25) + 
  ggtitle("Log10_PfMGET vs Log10_Asex") + scale_y_log10() + scale_x_log10() 

# CCp4 + PfMGET vs Asex
p4 = ggplot(rmcorr_data %>% filter(CCp4 + PfMGET > 12, day >= 18), aes(x = Asex, y = CCp4+PfMGET)) + geom_point(size = 1.25) + 
  ggtitle("Log10(CCp4+PfMGET) vs Log10_Asex") + scale_y_log10() + scale_x_log10() 

# Generate Plots
lapply(list(p1,p2,p3,p4), add_trendline)


######################################################################
# Figures - Overlapping CCp4, PfMGET and 18s Longitudinal Plots

# Notes 
# - Sources the same processsed long dataframe used earlier for Figure 1.
#   A brief reminder: the original wide format datafile is pivoted to long 
#   format and gaametocyte values below LLOR are set to 1.


######################

ggplot(plot_data, aes(x = day, y = value, color = variable, group = paste0(StudyCode, variable)))+geom_line(size = 2.5, alpha = 0.06)+ geom_point(size = 0.5)+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.title = element_text(size = 11), 
        axis.text = element_text(size = 8)) + labs(x = "Day", y = "Gametocyte Density (gams/ml)") +
  scale_y_log10(limits = c(0.7, 25000)) + xlim(0,36) + ggtitle("Gametocytaemia and Parasitaemia Over Time")+
  scale_color_manual(values=c("Asex"="#666666", "CCp4" = "orange", "PfMGET"="blue"))+
  # scale_linetype_manual(values=c("Asex"="dashed","CCp4"="solid","PfMGET"="solid"))+
  stat_summary(fun = median, geom= 'line', size = 2.25, aes(group = variable), alpha = 0.9) + 
  stat_summary(fun = median, geom= 'point', size = 2.6, aes(group = variable)) 
