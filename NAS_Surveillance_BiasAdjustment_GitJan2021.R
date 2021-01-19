# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# NAS Surveillance Bias Adjustment
# K. Labgold
#
# Created: July 22, 2019
# Last Update: January 19, 2020
#
# Description: Sample Code for Multidimensional Bias Analysis of NAS surveillance
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(dplyr)
library(rio)


nas1 <- import("T:/epiprojs/Howards/Survelliance/www_app/shiny_test.xlsx") %>%
            filter(YEAR == 2016) %>%
            mutate(nonNAS_n = hosp_n - NAS_n)

####---- 1. Manuscript Example Tables: PPV 80%, NPV 99.95% ----####

## 1.1 Table of Observed Hospital Counts ----

#Creates a table of hosp from input above  
hospital <- round(matrix(c(nas1$NAS_n, 
                           nas1$nonNAS_n,
                           nas1$hosp_n), 
                         ncol=1, byrow=TRUE), 1)

colnames(hospital) <- c("2016 Hospital Discharge Data, New Jersey")
rownames(hospital) <- c("ICD Diagnosis of NAS", "No ICD Diagnosis of NAS", "Total Births")

hospital

## 1.2 Validation Table ----
PPV_input <- 80
NPV_input <- 99.95

nas1$a_v <- nas1$tot_ICD*(PPV_input/100)
nas1$b_v <- nas1$tot_ICD - nas1$a_v
nas1$d_v <- nas1$tot_nonicd*(NPV_input/100)
nas1$c_v <- nas1$tot_nonicd - nas1$d_v

nas1$emr_NAS <- nas1$a_v + nas1$c_v
nas1$emr_nonNAS <- nas1$b_v + nas1$d_v
nas1$emr_tot <- nas1$emr_NAS + nas1$emr_nonNAS

validity <- round(matrix(c(nas1$a_v, nas1$b_v, nas1$tot_ICD, 
                           nas1$c_v, nas1$d_v, nas1$tot_nonicd,
                           nas1$emr_NAS, nas1$emr_nonNAS, nas1$hosp_n), ncol=3, 
                         byrow=TRUE), 1)

colnames(validity) <- c("Expected EMR Diagnosis of NAS", "Expected No EMR Diagnosis of NAS", "Total")
rownames(validity) <- c("ICD Diagnosis of NAS", "No ICD Diagnosis NAS", "Total")

validity


## 1.3 Calculated Parameters ----
PPV_calc <- paste0(round((nas1$a_v / (nas1$a_v + nas1$b_v))*100, 2),"%")
NPV_calc <- paste0(round((nas1$d_v / (nas1$c_v + nas1$d_v))*100, 2), "%")
Se_calc <- paste0(round((nas1$a_v / (nas1$a_v + nas1$c_v))*100, 2), "%")
Sp_calc <- paste0(round((nas1$d_v / (nas1$b_v + nas1$d_v))*100, 2), "%")
prev_obs <- paste0(round((nas1$tot_ICD/nas1$hosp_n)*100, 2), "%")
prev_calc <- paste0(round((nas1$emr_NAS/nas1$hosp_n)*100, 2), "%")

validity_values <- matrix(c(PPV_calc, NPV_calc, Se_calc, Sp_calc, prev_obs, prev_calc),
                          ncol = 1, byrow = TRUE)
colnames(validity_values) <- "Calculated Values"
rownames(validity_values) <- c("Input PPV", "Input NPV", "Calculated Se", "Calculated Sp", 
                               "Observed Surveillance Prevalence", "Calculated EMR Prevalence")

validity_values


## 1.4 Expected EMR Counts ----

#Table of EMR expected counts
emr <- round(matrix(c(nas1$emr_NAS, 
                      nas1$emr_nonNAS,
                      nas1$hosp_n),ncol=1,byrow=TRUE), 1)
colnames(emr) <- c(paste("2010 Hypothetical EMR under Hospital Discharge PPV", PPV_input, "% and NPV", NPV_input, "%"))
rownames(emr) <- c("Expected Counts EMR Diagnosis of NAS", "Expected Counts No EMR Diagnosis of NAS", "Total Births")

emr


####---- 2. PPV and NPV over Time ----####

## 2.1 Write Functions --

change_fun <- function(PPV1, NPV1, PPV2, NPV2) { 

  # PPV/NPV Set 1
  
  expected_count_set1 <- round(((PPV1/100)*nas1$NAS_n) + ((1-(NPV1/100))*nas1$nonNAS_n), 1)
  
  rate_set1 <- round((expected_count_set1/nas1$hosp_n)*100, 2)
  
  expected_non_set1 <- round(nas1$hosp_n - expected_count_set1, 1)
  
  case_diff_set1 <- round(nas1$NAS_n - expected_count_set1, 1)
  
  # PPV/NPV Set 2
  expected_count_set2 <- round(((PPV2/100)*nas1$NAS_n) + ((1-(NPV2/100))*nas1$nonNAS_n), 1)
  
  rate_set2 <- round((expected_count_set2/nas1$hosp_n)*100, 2)
  
  expected_non_set2 <- round(nas1$hosp_n - expected_count_set2, 1)
  
  case_diff_set2 <- round(nas1$NAS_n - expected_count_set2, 1)
  

  # Return output---
  return(list(rate_set1, expected_count_set1, expected_non_set1, case_diff_set1,
                rate_set2, expected_count_set2, expected_non_set2, case_diff_set2))

}
  



## 2.2 Fill Table --

tblchange <- as.data.frame(matrix(ncol = 9, nrow = 2)) # Empty Data Frame

names(tblchange) <- c("Observed NAS Prevalence", "Observed NAS Cases", "Observed NAS Non-Cases",
                      "PPV", "NPV",
                      "Misclassification-Adjusted NAS Prevalence",
                      "Expected EMR NAS Cases", "Expected EMR Non-Cases",
                      "Expected Case Difference")


tblchange[1, "Observed NAS Prevalence"] <- round((nas1$NAS_n / nas1$hosp_n)*100, 2) 
tblchange[2, "Observed NAS Prevalence"] <- round((nas1$NAS_n / nas1$hosp_n)*100, 2)

tblchange[1, "Observed NAS Cases"] <- nas1$NAS_n
tblchange[2, "Observed NAS Cases"] <- nas1$NAS_n

tblchange[1, "Observed NAS Non-Cases"] <- nas1$nonNAS_n
tblchange[2, "Observed NAS Non-Cases"] <- nas1$nonNAS_n

tblchange[1, "PPV"] <- 80
tblchange[2, "PPV"] <- 82.5

tblchange[1, "NPV"] <- 99.5
tblchange[2, "NPV"] <- 99.3

tblchange[1, "Misclassification-Adjusted NAS Prevalence"] <- change_fun(80, 99.95, 82.5, 99.93)[1]
tblchange[2, "Misclassification-Adjusted NAS Prevalence"] <- change_fun(80, 99.95, 82.5, 99.93)[5]

tblchange[1, "Expected EMR NAS Cases"] <- change_fun(80, 99.95, 82.5, 99.93)[2]
tblchange[2, "Expected EMR NAS Cases"] <- change_fun(80, 99.95, 82.5, 99.93)[6]

tblchange[1, "Expected EMR Non-Cases"] <- change_fun(80, 99.95, 82.5, 99.93)[3]
tblchange[2, "Expected EMR Non-Cases"] <- change_fun(80, 99.95, 82.5, 99.93)[7]

tblchange[1, "Expected Case Difference"] <- change_fun(80, 99.95, 82.5, 99.93)[4]
tblchange[2, "Expected Case Difference"] <- change_fun(80, 99.95, 82.5, 99.93)[8]



tblchange
