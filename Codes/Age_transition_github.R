library(lubridate)
library(stats)
library(readxl)
library(readr)
library(dplyr)

# load the data
setwd("S:/NDL/2_CYP_MH/")
sample_data = read.csv(file = "NWL_MH_PLD_Referrals_Seen.csv") 

# assign ServiceGroup titles to A - adult, C - CYA -children and young adults groups
derive_service_group = function(val){
  if (val != '111') { out = 'O' }
  if (val == 'Adult') { out = 'A' }
  if (val == 'Tier 2') { out = 'C' }
  if (val == 'Tier 3') { out = 'C' }
  if (val == 'Child & Family') { out = 'C' }  
  if (val == 'CAMHS') { out = 'C' }  
  if (val == 'Psychotherapy and personality disorder service Perinatal Services') { out = 'A' }  
  if (val == 'Recovery houses') { out = 'A' }  
  if (val == 'CID community services') { out = 'A' }  
  if (val == 'London CAMHS and CAMHS Inpatients') { out = 'C' }  
  if (val == 'London Learning Disabilities and Inpatients') { out = 'A' }  
  if (val == 'London SPA') { out = 'A' }  
  if (val == 'London Inpatients') { out = 'A' }  
  if (val == 'London Offender Care') { out = 'A' }  
  if (val == 'Older Adult') { out = 'A' }  
  if (val == 'Health & Justice Services') { out = 'A' }  
  if (val == 'Adults') { out = 'A' }  
  if (val == 'Horton Haven') { out = 'A' }  
  
  return (out)
}
sample_data$SG_out <- mapply(derive_service_group, sample_data$ServiceGroup)

# assign ServiceDescription titles to A - adult, C - CYA -children and young adults groups, O -other

derive_desc_group = function(val){
  if (val != '111') { out = 'O' }
  if (val == 'CAMHS Ealing') { out = 'C' }
  if (val == 'CAMHS H&F') { out = 'C' }
  if (val == 'CAMHS Hounslow') { out = 'C' }
  if (val == 'Community Fup Appointment - CAMHS') { out = 'C' }  
  if (val == 'Outpatient First Appointment - CAMHS') { out = 'C' }  
  if (val == 'Outpatient Fup Appointment - CAMHS') { out = 'C' }  
  if (val == 'Community First Appointment - CAMHS') { out = 'C' }  
  
  if (val == 'Community - 1ST CAMHS') { out = 'C' }  
  if (val == 'Community - FUP CAMHS') { out = 'C' }  
  if (val == 'Gfrd & Pvale 0 18 SAFE') { out = 'C' }  
  
  
  if (val == 'Community - 1ST Adults') { out = 'A' }  
  if (val == 'Community - FUP Adults') { out = 'A' }  
  if (val == 'Outpatients - 1ST Adults') { out = 'A' }  
  if (val == 'CAMHS and developmental services') { out = 'C' }  
  
  if (val == 'Adults - Outpatient Attendances') { out = 'A' }  
  if (val == 'Day Care - Adult') { out = 'A' }  
  if (val == 'CAMHS - Outpatient Attendances') { out = 'C' }  
  
  if (val == 'CAMHS - Community Contacts') { out = 'C' }
  if (val == 'Child and Adolescent Eating Disorder Services - Outpatient Attendances') { out = 'C' }
  if (val == 'Child and Adolescent Eating Disorder Services - Community Contacts') { out = 'C' }
  if (val == 'Specialist Perinatal Mental Health Services - Community Contacts') { out = 'A' }
  if (val == 'CAMHS - Community FUP') { out = 'C' }
  if (val == 'CAMHS - Outpatient FUP') { out = 'C' }
  if (val == 'CAMHS - Community 1ST') { out = 'C' }
  if (val == 'CAMHS - Outpatient 1ST') { out = 'C' }
  
  return (out)
  
}
sample_data$SD_out <- mapply(derive_desc_group, sample_data$ServiceDescription)





# subset useful columns
sd = sample_data[, c("Patient_DerivedKey", "ReferralDate", "DerivedAge_Derived", "ServiceGroup", "ServiceDescription",
                     "SG_out", "SD_out", "combined", "team_derived")]
sd$ReferralDate = as.Date(sd$ReferralDate, format = "%d/%m/%Y")

# remove transitions containing Other teams
sd_ca = sd[sd$team_derived != 'O',]
# order the dataframe
sd_ca = sd_ca[order(sd_ca[, 1], sd_ca[, 2]),]
# do additional data preparations
sd_ca$year <-year(sd_ca$ReferralDate)
sd_ca$DerivedAge_Derived <- as.numeric(sd_ca$DerivedAge_Derived)



### Create s summary table with transitions
#  empty summary table
summ_table <- data.frame(stringsAsFactors=FALSE) 
summ_table = data.frame(matrix(NA, nrow = 0, ncol = 7))
colnames(summ_table) <-  c("id", "transition_yes", 'Year_transition',  "Age_Transition", "initial_team", 'last_team', 'Referral_date')

# filling summary table
for (i in unique(sd_ca$Patient_DerivedKey)){

  temp_sbp = 0
  temp_sbp = subset(sd_ca, Patient_DerivedKey == i)
  temp_sbp$lag = lag(temp_sbp$team_derived)
  temp_sbp$lag[1] = temp_sbp$team_derived[1]
  # create a summary one line table
  t_new <- data.frame(stringsAsFactors=FALSE) 
  t_new = data.frame(matrix(NA, nrow = 0, ncol = 7))
  colnames(t_new) <-  c("id", "transition_yes", 'Year_transition',  "Age_Transition", "initial_team", 'last_team', 'Referral_date')
  
  # fill the one line summary table
  # patient ID
  t_new[1,1] = temp_sbp[1, 'Patient_DerivedKey']
  # first team
  t_new[1,5] = temp_sbp[1,]$team_derived
  # last team
  t_new[1,6] = temp_sbp[nrow(temp_sbp),]$team_derived

  # transition from CAMHS to Adult happened
  tr = temp_sbp[(temp_sbp$team_derived == 'A') & (temp_sbp$lag == 'C'),]
  if ( nrow(tr) == 0 ) {
      t_new[1,2] = 0
      t_new[1,3] = temp_sbp[nrow(temp_sbp),]$year
      t_new[1,4] = temp_sbp[nrow(temp_sbp),]$DerivedAge_Derived
      t_new[1,7] = temp_sbp[nrow(temp_sbp),]$ReferralDate
      t_new[1,7] = as.Date(t_new[1,7], origin = '1970-01-01')
      }
  if ( nrow(tr) > 0 ) {
    
      t_new[1,2] = 1
      t_new[1,3] = tr[1,]$year
      t_new[1,4] = tr[1,]$DerivedAge_Derived
      t_new[1,7] = tr[1,]$ReferralDate
      t_new[1,7] = as.Date(t_new[1,7], origin = '1970-01-01')
  }
  
  # add the derived row
  summ_table = rbind(summ_table, t_new)
}
# fix the date in the summary table
summ_table$Referral_date =  as.Date(summ_table$Referral_date, origin = '1970-01-01')
#head(summ_table[summ_table$transition_yes==1,])
colnames(summ_table)[1] = 'PatientKey'


### Filtering summary table and analysis
# table with all transitions where the last team is an adult MH team (transitions from CY to adult MH services)
summ_table_t2 = summ_table[(summ_table$transition_yes==1) & (summ_table$last_team == 'A'),]

# remove patients with missing Referral_date     
summ_table_t2_2 = summ_table_t2[(is.na(summ_table_t2$Referral_date) != TRUE),]          #  2 missing

# find patients with missing Age_Transition
miss_age_tran = summ_table_t2_2[(is.na(summ_table_t2_2$Age_Transition) == TRUE),]     # 134 missing

# Deriving missing age of transition based on other records
summ_table_t2_2f = summ_table_t2_2
for (i in unique(miss_age_tran$PatientKey)){
  if (is.na(summ_table_t2_2f[summ_table_t2_2f$PatientKey == i,]$Age_Transition) == TRUE){
    temp_sbp = subset(sd_ca, Patient_DerivedKey == i)
    temp_sbp$lag = lag(temp_sbp$team_derived)
    temp_sbp$lag[1] = temp_sbp$team_derived[1]
    
    temp_sbp_1 = temp_sbp[((is.na(temp_sbp$DerivedAge_Derived) != TRUE)),]
    c_ref = temp_sbp_1$ReferralDate[1]
    c_age = temp_sbp_1$DerivedAge_Derived[1]
    temp_sbp_2 = temp_sbp[((is.na(temp_sbp$DerivedAge_Derived) == TRUE)) & (temp_sbp$team_derived == 'A') & (temp_sbp$lag =='C'),]
    t_ref = temp_sbp_2$ReferralDate
    diff = round(as.numeric((t_ref - c_ref))/365,0)
    t_age = c_age + diff
    summ_table_t2_2f[summ_table_t2_2f$PatientKey == i,]$Age_Transition = t_age
  }
}

# check if missing data imputation worked
summ_table_t2_3f = summ_table_t2_2f[(is.na(summ_table_t2_2f$Age_Transition) != TRUE),]     # 4 missing (all ages are missing)
# summ_table_t2_2f[(is.na(summ_table_t2_2f$Age_Transition) == TRUE),]


# setting covid category 
summ_table_t2_3 = summ_table_t2_3f
summ_table_t2_3$covid = ''
for (i in 1:nrow(summ_table_t2_3)){
  if (summ_table_t2_3$Referral_date[i] < '2020-03-01') {summ_table_t2_3$covid[i] = 'pre_covid'}
  if (summ_table_t2_3$Referral_date[i] > '2020-02-28') {summ_table_t2_3$covid[i] = 'covid'}
}


# getting medians 
median(summ_table_t2_3$Age_Transition)
median(summ_table_t2_3[summ_table_t2_3$covid == 'pre_covid',]$Age_Transition)
median(summ_table_t2_3[summ_table_t2_3$covid == 'covid',]$Age_Transition)

# Is there significant difference between pre-covid and covid
wilcox.test(Age_Transition ~ covid, summ_table_t2_3, exact = FALSE)

# Plotting histograms
hist(summ_table_t2$Age_Transition, breaks = 20)

h1 = hist(summ_table_t2_3$Age_Transition, breaks = 20)

h2 = hist(summ_table_t2_3[summ_table_t2_3$covid == 'pre_covid',]$Age_Transition, breaks = 15)

h3 = hist(summ_table_t2_3[summ_table_t2_3$covid == 'covid',]$Age_Transition, breaks = 15)


c1 = rgb(140, 216, 230, max = 255, alpha = 80, names = "lt.blue")
c2 = rgb(255, 220, 203, max = 255, alpha = 80, names = "lt.blue")

plot(h2, col = c1)
plot(h3, col = c2, add = TRUE)
legend(x = 'topright', legend = c('pre_covid','covid'), col = c(c1,c2), lwd = 8, cex = 0.8)



