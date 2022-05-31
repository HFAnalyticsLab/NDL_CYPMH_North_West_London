#Libraries and additional functions
library(readxl)
library(dplyr)
library(tidyverse)
library(data.table)
library(readr)
library(ggplot2)
library(magrittr)
library(lubridate)

options(scipen = 999)

`%nin%` <- Negate(`%in%`)
suppress_values <- function(x,minval=7,lab="<5"){
  return(ifelse(x<=minval,lab,as.character(x)))
}

#Read in data
mhdata <- data.frame(fread("processed_data/MH_patient_and_events_data.csv"))
mhdata <- mhdata %>% filter(Age>0 & Age < 26) %>% distinct()
#mhact <- fread("raw_data/Old files/NDL Phase 2 Mental Health Activity 20210916.csv")
#mhact <- fread("processed_data/BU/MH_activity_data.csv", nrows = 100000)
#mhact <- fread("raw_data/MH PLD Referrals Seen.csv")
cyp <- data.frame(fread("processed_data/All_cyp_cohort.csv"))
cyp <- cyp %>% filter(Age>0)
#icd10 <- read_excel("NDL_Sat2Codes.xlsx","ICD10")

mhdata$Setting2 <- mhdata$Setting
mhdata$Setting[mhdata$Setting %in% c("GP Event with Read Code",
                                     "GP prescription with MH drug")] <- "GP Event with Read Code or GP prescription with MH drug"
#Additional fixes
set_up_data <- function(data){
  
  if(any(data$Event_Date == "")){
    message("Cant process Event_Date column")
  } else { 
    data$Event_Date <- as.Date(data$Event_Date)
    data$year <- year(data$Event_Date)
    data$year_month <- zoo::as.yearmon(data$Event_Date)
    data$covid_time <- ifelse(data$Event_Date>=as.Date("2020-03-01"),"during","pre")
    data$covid_time <- factor(data$covid_time, levels = c("pre","during"))
  }
  
  data$codes <- paste(data$Code_One,data$Code_Two,data$Code_Three,data$Code_Four,data$Code_five,sep = ",")
  

  #Set factors
  data$AgeBracket <- factor(data$AgeBracket, levels = c("0-4","5-11", "12-17","18-21", "22-25", "26-30"))
  data$IMD_Quintile <- factor(data$IMD_Quintile, levels = c("1","2","3","4","5","Unknown"))
  
  
  return(data)
}

cyp <- set_up_data(cyp)
mhdata <- set_up_data(mhdata)
mhdata <- filter(mhdata, mhdata$Event_Date < as.Date("2021-10-01"))

#Analisis goals
# 1 - Description of CYP population accessing MH services in NWL####

##Figure 1 cases by setting####
fig1 <- mhdata %>% 
  group_by(PatientKey,Setting, year) %>%
  filter(row_number() ==1) %>%
  mutate(month = case_when(year == 2021 ~ 9,
                           year == 2015 ~ 8,
                           T ~ 12)) %>%
  dplyr::group_by(year,Setting) %>%
  summarise(n=n(),
            months = mean(month),
            prop= n()/mean(month)) %>%
  mutate(n = suppress_values(n),
         prop = ifelse(n=="<5",NA,prop))
    
ggplot(fig1) +
  geom_bar(aes(x=year,y=prop,fill=Setting), stat='identity',position='dodge')

fwrite(fig1,"Outputs/draft/O1/Figure1data.csv")

###Figure 1 cases by setting additional requeste by HF####
fig1 <- mhdata %>% 
  group_by(PatientKey,Setting2, year) %>%
  filter(row_number() ==1) %>%
  mutate(month = case_when(year == 2021 ~ 9,
                           year == 2015 ~ 8,
                           T ~ 12)) %>%
  dplyr::group_by(year,Setting2) %>%
  summarise(n=n(),
            months = mean(month),
            prop= n()/mean(month)) %>%
  mutate(n = suppress_values(n),
         prop = ifelse(n=="<5",NA,prop))

ggplot(fig1) +
  geom_bar(aes(x=year,y=prop,fill=Setting2), stat='identity',position='dodge')

fwrite(fig1,"Outputs/draft/O1/Figure1data_HF_request.csv")

##Figure 2 cases by ethnicity####

fig2base <- cyp %>% distinct(PatientKey,EthnicCategory) %>%
  group_by(EthnicCategory) %>%
  summarise(total=n())

fig2 <- mhdata %>%
  mutate(month = month(year_month)) %>%
  filter(month %in% 1:9) %>%
  group_by(year,PatientKey) %>%
  filter(row_number() == 1) %>%
  group_by(EthnicCategory,year) %>% 
  summarise(n=n()) 

fig2 <- left_join(fig2,fig2base,by="EthnicCategory") 
fig2$prop = fig2$n/fig2$total

fig2 <- fig2 %>%
  mutate(n = suppress_values(n),
         prop = ifelse(n=="<5",NA,prop))

ggplot(fig2) + 
  geom_line(aes(x=year,y=prop,color=EthnicCategory))

fwrite(fig2,"Outputs/draft/O1/fig2V1.csv")

##Figure 3 split by gender####

fig3base <- cyp %>% distinct(PatientKey,Gender) %>%
  group_by(Gender) %>%
  summarise(total=n())

fig3 <- mhdata %>% distinct(PatientKey,Setting,year,.keep_all = T) %>%
  group_by(Setting,Gender,year) %>%
  summarise(n=n()) 

fig3 <- left_join(fig3,fig3base,by="Gender") 
fig3$prop = fig3$n/fig3$total

fig3 <- fig3 %>%
  mutate(n = suppress_values(n),
         prop = ifelse(n=="<5",NA,prop))

ggplot(fig3) + 
  geom_boxplot(aes(x=Setting,y=prop,fill=Gender))

fwrite(fig3,"Outputs/draft/O1/fig3.csv")

##Figure 4 age group####
fig4base <- cyp %>% distinct(PatientKey,AgeGroup) %>%
  group_by(AgeGroup) %>%
  summarise(total=n())

fig4 <- mhdata %>% distinct(year,Setting,PatientKey, .keep_all = T) %>%
  mutate(month = month(year_month)) %>%
  group_by(AgeGroup,Setting,year) %>% summarise(n=n())

fig4 <- left_join(fig4,fig4base,by="AgeGroup") 
fig4$prop = fig4$n/fig4$total

fig4 <- fig4%>%
  mutate(n = suppress_values(n),
         prop = ifelse(n=="<5",NA,prop))

ggplot(fig4) + 
  geom_boxplot(aes(x=Setting,y=prop,fill=AgeGroup))

fwrite(fig4,"Outputs/draft/O1/fig4V1.csv")

#Additional table####

addtab <- mhdata %>% filter(year==2021)

addage <- addtab %>% group_by(PatientKey) %>%
  filter(row_number() == 1) %>%
  count(Age)
fwrite(addage,"Outputs/draft/additional/CountsAge.csv")

addgen <- addtab %>% group_by(PatientKey) %>%
  filter(row_number() == 1) %>%
  count(Gender)
fwrite(addgen,"Outputs/draft/additional/CountsGender.csv")

addimd <- addtab %>% group_by(PatientKey) %>%
  filter(row_number() == 1) %>%
  count(IMD_Quintile)
fwrite(addimd,"Outputs/draft/additional/CountsIMD.csv")

addeth <- addtab %>% group_by(PatientKey) %>%
  filter(row_number() == 1) %>%
  count(EthnicCategory)
fwrite(addeth,"Outputs/draft/additional/CountsEthnicity.csv")

addccg <- addtab %>% group_by(PatientKey) %>%
  filter(row_number() == 1) %>%
  count(CCG_Name)
fwrite(addccg,"Outputs/draft/additional/CountsCCG.csv")

addsett <- mhdata %>%
  filter(year == 2021) %>%
  group_by(PatientKey,Setting) %>%
  filter(row_number() == 1) %>%
  count(Setting)
fwrite(addsett,"Outputs/draft/additional/CountsSetting.csv")

addbreak <- mhdata %>%
  filter(year == 2021) %>%
  group_by(PatientKey,Setting) %>%
  filter(row_number() == 1) %>%
  count(Setting,AgeBracket) %>%
  mutate(n = suppress_values(n))
fwrite(addbreak,"Outputs/draft/additional/CountsSettingAge.csv")


##Odds ratio analysis IGNORE####
cyp$mhevent <- ifelse(is.na(cyp$MHEventPresent),F,T)

cyp$mh_during_covid <- ifelse(cyp$Event_Date == "","2000-01-01",cyp$Event_Date)
cyp$mh_during_covid <- as.Date(cyp$mh_during_covid)>=as.Date("2020-03-01")

cyp$mh_prior_covid <- ifelse(cyp$Event_Date == "","2000-01-01",cyp$Event_Date)
cyp$mh_prior_covid <- between(as.Date(cyp$mh_prior_covid),as.Date("2015-01-01"),as.Date("2020-03-01"))


mh_or_analysis <- function(data, target_var,category,mhdata=mhdata,type="previous"){
  
  if(type %nin% c("previous","new")){
    stop("Specify either previous or new")
  }
  
  data$target_var = data[,target_var]
  #Filter out data
  tdat <- data %>% filter(target_var == category)
  
  if(type == "previous"){
    tdat <- tdat %>% group_by(PatientKey) %>%
      summarise(mh_during_covid = any(mh_during_covid),
                mh_prior_covid = any(mh_prior_covid))
  }
  
  if(type == "new"){
    tdat <- tdat %>% group_by(PatientKey) %>%
      summarise(mh_during_covid = any(mh_during_covid),
                mh_prior_covid = !any(mh_prior_covid))
  }
  
  
  #Make model
  testmodel <- glm(mh_during_covid ~ mh_prior_covid ,data=tdat)
  model_data <- testmodel %>% broom::tidy() 
  
  #Add CI and coeffs
  glmmod_all_confint <- confint(testmodel)
  glmmod_all_confint <- data.frame(glmmod_all_confint)
  model_data$lwr_95ci <- exp(glmmod_all_confint$X2.5..)
  model_data$upr_95ci <- exp(glmmod_all_confint$X97.5..)
  model_data$coeff <- exp(model_data$estimate)
  
  model_data$target_var = target_var
  model_data$category = category
  model_data$pre_total  = sum(mhdata$covid_time == "pre")
  model_data$pre_mh = sum(tdat$PatientKey %in% mhdata$PatientKey[mhdata$covid_time == "pre"])
  model_data$dur_total = sum(mhdata$covid_time == "during")
  model_data$dur_mh = sum(tdat$PatientKey %in% mhdata$PatientKey[mhdata$covid_time == "during"])
  model_data$type = type
  
  return(model_data)

}
mh_or_analysis_full <- function(data, target_var,category,mhdata=mhdata){
  p1 <- mh_or_analysis(data = data, target_var = target_var, category = category,mhdata = mhdata,type = 'previous')
  p2 <- mh_or_analysis(data = data, target_var = target_var, category = category,mhdata = mhdata,type = 'new')
  return(bind_rows(p1,p2))
  
}


ors_age <- lapply(unique(cyp$AgeBracket),
                  function(x) mh_or_analysis_full(data = cyp, mhdata = mhdata,
                                             target_var = "AgeBracket",category = x)) %>%
  bind_rows() %>% filter(term!="(Intercept)")
fwrite(ors_age,"Outputs/draft/O1/O1_ors_age.csv")

ors_gender <- lapply(unique(cyp$Gender),
                     function(x) mh_or_analysis_full(data = cyp,  mhdata = mhdata,
                                                target_var = "Gender",category = x)) %>%
  bind_rows() %>% filter(term!="(Intercept)")
fwrite(ors_gender,"Outputs/draft/O1/O1_ors_gendr.csv")

ors_IMD <- lapply(unique(cyp$IMD_Quintile),
                     function(x) mh_or_analysis_full(data = cyp, mhdata = mhdata,
                                                target_var = "IMD_Quintile",category = x)) %>%
  bind_rows() %>% filter(term!="(Intercept)")
fwrite(ors_IMD,"Outputs/draft/O1/O1_ors_IMD.csv")

ors_ccg <- lapply(unique(cyp$ccg_name),
                     function(x) mh_or_analysis_full(data = cyp,  mhdata = mhdata,
                                                target_var = "ccg_name",category = x)) %>%
  bind_rows() %>% filter(term!="(Intercept)")
fwrite(ors_ccg,"Outputs/draft/O1/O1_ors_ccg.csv")

ors_ethni <- lapply(unique(cyp$EthnicCategory),
                     function(x) mh_or_analysis_full(data = cyp, mhdata = mhdata,
                                                target_var = "EthnicCategory",category = x)) %>%
  bind_rows() %>% filter(term!="(Intercept)")
fwrite(ors_ethni,"Outputs/draft/O1/O1_ors_ethni.csv")

##ORS plot line####
p1 <- mhdata %>% 
  group_by(year_month,Gender) %>% summarise(n=n()) %>%
  filter(Gender != "Unknown") %>%
  ggplot() + 
  geom_line(aes(x=year_month,y=n,color=Gender),stat='identity', size=1.2) +
  geom_vline(xintercept = 2020.2, color='red',linetype=2) +
  labs(x="Date",y="Number of events") +
  theme_bw() +
  theme(legend.position = 'bottom')

p2 <- mhdata %>% 
  group_by(year_month,AgeBracket) %>% summarise(n=n()) %>%
  filter(AgeBracket != "0-4") %>%
  ggplot() + 
  geom_line(aes(x=year_month,y=n,color=AgeBracket),stat='identity', size=1.2) +
  scale_color_brewer(palette = 'Spectral')+
  labs(x="Date",y="Number of events", color = 'Age band') +
  geom_vline(xintercept = 2020.2, color='red',linetype=2) +
  theme_bw() +
  theme(legend.position = 'bottom')

p3 <- mhdata %>% 
  group_by(year_month,EthnicCategory) %>% summarise(n=n()) %>%
  ggplot() + 
  geom_line(aes(x=year_month,y=n,color=EthnicCategory),stat='identity', size=1.2) +
  geom_vline(xintercept = 2020.2, color='red',linetype=2) +
  scale_color_brewer(palette = "Accent")+
  labs(x="Date",y="Number of events", color = 'Ethnicity') +
  theme_bw() +
  theme(legend.position = 'bottom')

p4 <- mhdata %>% 
  group_by(year_month,ccg_name) %>% summarise(n=n()) %>%
  filter(ccg_name!="NULL") %>%
  ggplot() + 
  geom_line(aes(x=year_month,y=n,color=ccg_name),stat='identity', size=1.2) +
  geom_vline(xintercept = 2020.2, color='red',linetype=2) +
  scale_color_brewer(palette = "Set2")+
  labs(x="Date",y="Number of events", color = 'CCG') +
  theme_bw() +
  theme(legend.position = 'bottom') + 
  guides(color = guide_legend(nrow = 3))

p5 <- mhdata %>% 
  group_by(year_month,IMD_Quintile) %>% summarise(n=n()) %>%
  ggplot() + 
  geom_line(aes(x=year_month,y=n,color=IMD_Quintile),stat='identity', size=1.2) +
  geom_vline(xintercept = 2020.2, color='red',linetype=2) +
  scale_color_brewer(palette = "RdYlBu")+
  labs(x="Date",y="Number of events", color = 'IMD quintile') +
  theme_bw() +
  theme(legend.position = 'bottom')


gridExtra::grid.arrange(p1,p2,p3,p5,p4,
                        layout_matrix = rbind(c(1,2,3),
                                              c(4,5,NA)))



p1 <- mhdata %>% 
  group_by(year_month,Gender) %>% summarise(n=n())

p2 <- mhdata %>% 
  group_by(year_month,AgeBracket) %>% summarise(n=n()) 

p3 <- mhdata %>% 
  group_by(year_month,EthnicCategory) %>% summarise(n=n())

p4 <- mhdata %>% 
  group_by(year_month,CCG_Name) %>% summarise(n=n()) 

p5 <- mhdata %>% 
  group_by(year_month,IMD_Quintile) %>% summarise(n=n()) 

fdata <- bind_rows(p1,p2,p3,p4,p5) %>% mutate(n = suppress_values(n))
fwrite(fdata,"Outputs/draft/O2/mh_events_counts.csv")

# 2 - Access to MH services####
# Incusion: all CYP with MH codes
# Timeline Feb 2015 - Feb 2020 (pre-covid), Mar 2020 - Jun 2020 (during)


###Additional: plot without demographics

obj2base <- mhdata %>% 
  group_by(year_month,covid_time) %>% 
  summarise(n=n(),
            unique_patients = length(unique(PatientKey))) %>%
  mutate(datex = as.Date(paste0("01 ",year_month), format = "%d %B %Y")) %>%
  ungroup() %>% 
  arrange(datex) %>% 
  mutate(daten = 1:n(),
         Year = year(datex),
         Month = month(datex),
         Pandemic = as.numeric(covid_time) - 1,
         RegPtList = nrow(mhdata),
         unique_patients) %>% 
  select(Year,
         Month,
         MonthYear = year_month,
         TimePeriod = daten,
         Pandemic,
         diag = n,
         RegPtList,
         unique_patients)

fwrite(obj2base,"Outputs/draft/O2/baselinedata.csv")

time_series_analysis_data <- function(data = mhdata,target_var = "AgeBracket", category = NULL){
  data$target <- data[,target_var]
  if(!is.null(category)){
    data <- data %>% filter(target == category)
  }
  
  totals <- nrow(data)
  
  #Make dataset
  data <- data %>% 
    group_by(year_month,covid_time) %>% 
    summarise(n=n(),
              unique_patients = length(unique(PatientKey))) %>%
    mutate(datex = as.Date(paste0("01 ",year_month), format = "%d %B %Y")) %>%
    ungroup() %>% 
    arrange(datex) %>% 
    mutate(daten = 1:n(),
           target_variable = target_var,
           subcat = category,
           Year = year(datex),
           Month = month(datex),
           Pandemic = as.numeric(covid_time) - 1,
           RegPtList = totals,
           unique_patients) %>% 
    select(Year,
           Month,
           subcat,
           MonthYear = year_month,
           TimePeriod = daten,
           Pandemic,
           diag = n,
           RegPtList,
           unique_patients)
  
  return(data)
}
time_series_analysis_model <- function(data){
  model <- glm(diag ~ offset(log(RegPtList)) + Pandemic + TimePeriod, family=poisson, data = data)
  return(model)
}
time_series_analysis_plot <- function(data,model){
  
  data_predict_overall <- data.frame(RegPtList=mean(data$RegPtList), 
                            Pandemic=data$Pandemic,
                            TimePeriod = data$TimePeriod,
                            Month= data$Month)
  
  overall_preds <- predict(model, type = "response", data_predict_overall,se.fit = T)
  
  overall_preds_data <- data.frame(pred= overall_preds$fit,
                                   upr95ci = overall_preds$fit + (2*overall_preds$se.fit),
                                   lwr95ci = overall_preds$fit - (2*overall_preds$se.fit),
                                   n = 1:length(overall_preds$fit))
  
  data_predict_covid <- data.frame(RegPtList=mean(data$RegPtList), 
                                   Pandemic= 0,
                                   TimePeriod = data$TimePeriod,
                                   Month= data$Month)
  
  covid_preds <- predict(model,data_predict_covid,type="response",se.fit = T)
  
  covid_preds_data <- data.frame(pred= covid_preds$fit,
                                 upr95ci = covid_preds$fit + (2*covid_preds$se.fit),
                                 lwr95ci = covid_preds$fit - (2*covid_preds$se.fit),
                                 n = 1:length(covid_preds$fit))
  
  
  p <- ggplot() + 
    #Original data
    geom_line(data = overall_preds_data, mapping = aes(x=n,y=pred,color = "p1",group=1), size= 1.5) +
    #geom_ribbon(data = overall_preds_data, mapping = aes(x=n, y=pred, ymin=lwr95ci,ymax=upr95ci), fill="grey50")
    #Predictions for model without COVID data
    geom_line(data = covid_preds_data, mapping = aes(x=n, y=pred, color = "p2"), size = 1.3) +  
    #geom_ribbon(data = covid_preds_data, mapping = aes(x=n, y=pred, ymin=lwr95ci,ymax=upr95ci), fill="grey50")
    #Predictions from model of what's seen for COVID
    geom_point(data = data, mapping = aes(x=TimePeriod, y=diag, color = "p3"), size = 1.3) + 
    scale_color_manual(breaks = c("p1","p2","p3"), 
                       values = c("cornflowerblue","firebrick","orange"),
                       labels = c("Original data", "Values expected without COVID-19","Values seen due to COVID-19")) + 
    labs(x= "Date", y= "Number of events", color = "") +
    theme_bw() + 
    theme(legend.position = "bottom")

  return(p)
}
run_ts_analysis <- function(mhdata,target_variable,tag){
  
  output_list<-list()
  categories <- unique(mhdata[,target_variable])
  all_data <- data_frame()
  
  for(categ in categories){
    message(paste0("Currently calculating ",target_variable,": ",categ))
    # data_n <-  paste0("O2_ts_",tag,"_",categ,"_data")
    # model_n <- paste0("O2_ts_",tag,"_",categ,"_model")
    # plot_n <-  paste0("O2_ts_",tag,"_",categ,"_plot")
    
    data_n <-  paste0("data")
    model_n <- paste0("model")
    plot_n <-  paste0("plot")
    
    mdata <- time_series_analysis_data(data = mhdata, target_var = target_variable, category = categ)
    mmodel <- time_series_analysis_model(mdata)
    mplot <- time_series_analysis_plot(data = mdata, model = mmodel)
    
    outs <- list(mdata,mmodel,mplot)
    names(outs) <- c(data_n,model_n,plot_n)
    
    output_list[[categ]] <- outs
    
  }
  
  #names(output_list) <- categories
  return(output_list)
}

#Age
O2_ts_age <- run_ts_analysis(mhdata = mhdata, target_variable = "AgeBracket",tag = "Age")
O2_ts_age_data <- bind_rows(lapply(names(O2_ts_age), function(x) O2_ts_age[[x]]$data)) %>% 
  mutate(diag = suppress_values(diag))
fwrite(O2_ts_age_data,"Outputs/draft/O2/time_series_age.csv")
#IMD
O2_ts_imd <- run_ts_analysis(mhdata = mhdata, target_variable = "IMD_Quintile",tag = "IMD")
O2_ts_imd_data <- bind_rows(lapply(names(O2_ts_imd), function(x) O2_ts_imd[[x]]$data)) %>% 
  mutate(diag = suppress_values(diag))
fwrite(O2_ts_imd_data,"Outputs/draft/O2/time_series_imd.csv")
#Sex
O2_ts_sex <- run_ts_analysis(mhdata = mhdata, target_variable = "Gender", tag = "gender")
O2_ts_sex_data <- bind_rows(lapply(names(O2_ts_sex), function(x) O2_ts_sex[[x]]$data)) %>% 
  mutate(diag = suppress_values(diag))
fwrite(O2_ts_sex_data,"Outputs/draft/O2/time_series_sex.csv")
#Ethnicity
O2_ts_eth <- run_ts_analysis(mhdata = mhdata, target_variable = "EthnicCategory", tag = "ethinicity")
O2_ts_eth_data <- bind_rows(lapply(names(O2_ts_eth), function(x) O2_ts_eth[[x]]$data)) %>% 
  mutate(diag = suppress_values(diag))
fwrite(O2_ts_eth_data,"Outputs/draft/O2/time_series_eth.csv")
#Geography
O2_ts_ccg <- run_ts_analysis(mhdata = mhdata, target_variable = "CCG_Name",tag = "CCG")
O2_ts_ccg_data <- bind_rows(lapply(names(O2_ts_ccg), function(x) O2_ts_ccg[[x]]$data)) %>% 
  mutate(diag = suppress_values(diag))
fwrite(O2_ts_ccg_data,"Outputs/draft/O2/time_series_ccg.csv")
#Setting
O2_ts_set <- run_ts_analysis(mhdata = mhdata, target_variable = "Setting",tag = "setting")
O2_ts_set_data <- bind_rows(lapply(names(O2_ts_set), function(x) O2_ts_set[[x]]$data)) %>% 
  mutate(diag = suppress_values(diag))
fwrite(O2_ts_set_data,"Outputs/draft/O2/time_series_set.csv")


##Extra analysis - are people post pandemic new or have they been seen before?####
keys_prepand <- unique(mhdata$PatientKey[mhdata$covid_time=="pre"])

first_contacts <- mhdata %>%
  mutate(seen_before = case_when(covid_time == "pre"  ~ "pre-pandemic",
                                covid_time == "during" & PatientKey %in% keys_prepand ~ "Seen previously",
                                covid_time == "during" & PatientKey %nin% keys_prepand ~ "New patient"
                                )) %>%
  group_by(year_month) %>%
  mutate(tot = n(),
         year_month = zoo::as.Date.yearmon(year_month)) %>%
  group_by(year_month,seen_before) %>%
  summarise(count = n(),
            prop = n()/mean(tot))

ggplot(first_contacts) + 
  geom_line(aes(x=year_month,y=prop,color=seen_before))
fwrite(first_contacts,"Outputs/draft/O2/first_contacts.csv")

#By month
un_months <- unique(mhdata$year_month)
prop_per_month <- data.frame(n = 1:length(un_months),
                             date = NA,
                             prop_new = NA,
                             prop_old = NA)

for(i in 1:length(un_months)){
  mnth <- un_months[i]
  ids_month <- unique(mhdata$PatientKey[mhdata$year_month == mnth])
  ids_prev <- unique(mhdata$PatientKey[mhdata$year_month < mnth])
  
  prp <- sum(ids_month %in% ids_prev) / length(ids_month)
  
  prop_per_month$date[i] <- mnth
  prop_per_month$prop_old[i] <- sum(ids_month %in% ids_prev) / length(ids_month)
  prop_per_month$prop_new[i] <- 1 - prp
}
rm(mnth,ids_month,ids_prev,prp,i)

prop_per_month <- prop_per_month %>% pivot_longer(cols = c("prop_new","prop_old"),
                                                  names_to = "type",
                                                  values_to = "value")


ggplot(prop_per_month) + 
  geom_area(aes(x=date,y=value,fill=type))

fwrite(prop_per_month,"Outputs/draft/O2/first_contact_per_month.csv")



 # 3 - MH admissions####

admis <- mhdata %>% filter(grepl(pattern = paste(icd10$Code,collapse = "|")
                                 ,x = codes))

admis <- mhdata %>% filter(grepl(pattern = "admission",x = Setting, ignore.case = T))

admis_test <- admis %>% group_by(Setting, year) %>% summarise(n=n())

admis %>% group_by(year) %>% distinct() %>% summarise(n=length(PatientKey))
admis %>% group_by(year) %>% summarise(n=length(PatientKey))

# #Age
# O3_ts_age <- run_ts_analysis(mhdata = admis, target_variable = "AgeBracket",tag = "Age")
# O3_ts_age_data <- bind_rows(lapply(names(O3_ts_age), function(x) O3_ts_age[[x]]$data)) %>% 
#   mutate(diag = suppress_values(diag))
# fwrite(O3_ts_age_data,"Outputs/draft/O3/time_series_age.csv")
# #IMD
# O3_ts_imd <- run_ts_analysis(mhdata = admis, target_variable = "IMD_Quintile",tag = "IMD")
# O3_ts_imd_data <- bind_rows(lapply(names(O3_ts_imd), function(x) O3_ts_imd[[x]]$data)) %>% 
#   mutate(diag = suppress_values(diag))
# fwrite(O3_ts_imd_data,"Outputs/draft/O3/time_series_imd.csv")
# #Sex
# O3_ts_sex <- run_ts_analysis(mhdata = admis, target_variable = "Gender", tag = "gender")
# O3_ts_sex_data <- bind_rows(lapply(names(O3_ts_sex), function(x) O3_ts_sex[[x]]$data)) %>% 
#   mutate(diag = suppress_values(diag))
# fwrite(O3_ts_sex_data,"Outputs/draft/O3/time_series_sex.csv")
# #Ethnicity
# O3_ts_eth <- run_ts_analysis(mhdata = admis, target_variable = "EthnicCategory", tag = "ethinicity")
# O3_ts_eth_data <- bind_rows(lapply(names(O3_ts_eth), function(x) O3_ts_eth[[x]]$data)) %>% 
#   mutate(diag = suppress_values(diag))
# fwrite(O3_ts_eth_data,"Outputs/draft/O3/time_series_eth.csv")
# #Geography
# O3_ts_ccg <- run_ts_analysis(mhdata = mhdata, target_variable = "ccg_name",tag = "CCG")
# O3_ts_ccg_data <- bind_rows(lapply(names(O3_ts_ccg), function(x) O3_ts_ccg[[x]]$data)) %>% 
#   mutate(diag = suppress_values(diag))
# fwrite(O3_ts_ccg_data,"Outputs/draft/O3/time_series_ccg.csv")

##Extra analysis - are people post pandemic new or have they been seen before?####
keys_prepand <- unique(admis$PatientKey[admis$covid_time=="pre"])

first_contacts <- admis %>%
  mutate(seen_before = case_when(covid_time == "pre"  ~ "pre-pandemic",
                                 covid_time == "during" & PatientKey %in% keys_prepand ~ "Seen previously",
                                 covid_time == "during" & PatientKey %nin% keys_prepand ~ "New patient"
  )) %>%
  group_by(year_month) %>%
  mutate(tot = n(),
         year_month = zoo::as.Date.yearmon(year_month)) %>%
  group_by(year_month,seen_before) %>%
  summarise(count = n(),
            prop = n()/mean(tot))

ggplot(first_contacts) + 
  geom_line(aes(x=year_month,y=prop,color=seen_before))
fwrite(first_contacts,"Outputs/draft/O3/first_contacts.csv")

#By month
un_months <- unique(admis$year_month)
prop_per_month <- data.frame(n = 1:length(un_months),
                             date = NA,
                             prop_new = NA,
                             prop_old = NA)

for(i in 1:length(un_months)){
  mnth <- un_months[i]
  ids_month <- unique(admis$PatientKey[admis$year_month == mnth])
  ids_prev <- unique(admis$PatientKey[admis$year_month < mnth])
  
  prp <- sum(ids_month %in% ids_prev) / length(ids_month)
  
  prop_per_month$date[i] <- mnth
  prop_per_month$prop_old[i] <- sum(ids_month %in% ids_prev) / length(ids_month)
  prop_per_month$prop_new[i] <- 1 - prp
}
rm(mnth,ids_month,ids_prev,prp,i)

prop_per_month <- prop_per_month %>% pivot_longer(cols = c("prop_new","prop_old"),
                                                  names_to = "type",
                                                  values_to = "value")


ggplot(prop_per_month) + 
  geom_area(aes(x=date,y=value,fill=type))

fwrite(prop_per_month,"Outputs/draft/O3/first_contact_per_month.csv")

admis_total <- admis %>% group_by(year) %>% summarise(n=n())

o3plot <- admis %>%
  group_by(Gender, year_month) %>%
  summarise(tot = n()) %>%
  mutate(tot = suppress_values(tot)) %>%
  mutate(aster = ifelse(tot == "<5","*",NA))

o3plot$tot <- ifelse(o3plot$tot == "<5", 0,as.numeric(o3plot$tot))
o3plot <- subset(o3plot, o3plot$Gender != "Unknown")

ggplot(o3plot) +
  geom_bar(aes(x=year_month, y=tot, fill = Gender), stat="identity", position = 'dodge') + 
  geom_smooth(aes(x=year_month, y=tot, color = Gender)) +
  geom_text(aes(x=year_month, y=tot, fill = Gender, label = aster), hjust = 0.5) + 
  labs(caption = "* data suppressed due to low numbers",
       x = "Time", y = "Number of patients") +
  theme_bw() + 
  theme(legend.position = 'bottom') 

fwrite(o3plot,"Outputs/draft/O3/o3_gender_split.csv")
fwrite(admis_total,"Outputs/draft/O3/o3_total_admissions.csv")

#

# 4 - time between services ####

mhactiv <- mhact %>%
   select(Patient_DerivedKey,
          DerivedAge_Derived,
          Activity_Month,
          ReferralDate,
          ContactDate,
          ContactType,
          AppointmentType,
          ServiceDescription,
          ServiceGroup)

mhactiv$ReferralDate <- as.Date(mhactiv$ReferralDate)
mhactiv$Activity_Month <- as.Date(mhactiv$Activity_Month)
mhactiv$ContactDate <- as.Date(mhactiv$ContactDate)
mhactiv$ReferralMonth <- zoo::as.yearmon(mhactiv$ReferralDate)


mhactiv$Event_type <- case_when(grepl(pattern = "1ST|First", mhactiv$ServiceDescription, ignore.case = T) ~ "First",
                                grepl(pattern = "fup", mhactiv$ServiceDescription, ignore.case = T) ~ "Follow-up",
                                grepl(pattern = "Specialist", mhactiv$ServiceDescription, ignore.case = T) ~ "Specialist",
                                T ~ "Other"
                                )

mhactiv$ServiceGroup_type <- case_when(grepl(pattern = "adult", mhactiv$ServiceGroup, ignore.case = T) |
                                         grepl(pattern = "adult", mhactiv$ServiceDescription, ignore.case = T)   ~ "Adult",
                                       grepl(pattern = "camhs|child", mhactiv$ServiceGroup, ignore.case = T) |
                                         grepl(pattern = "camhs|child", mhactiv$ServiceDescription, ignore.case = T) ~ "CAMHS",
                                       T ~ "Other"
)

#Check number of referrals
number_of_refs <- mhactiv %>%
  mutate(app_cat = case_when(AppointmentType %in% c("Community") ~ "Community",
                          AppointmentType %in% c("CPA Review", "Review") ~ "Case review",
                          AppointmentType %in% c("Day case") ~ "Day case",
                          AppointmentType %in% c("Generic Appointment",
                                                 "Mental Health Assessment",
                                                 "Standard",
                                                 "Treatment",
                                                 "Assessment & Treatment",
                                                 "Consultation") ~ "Assessment & Treatment",
                          AppointmentType %in% c("First Appointment",
                                                 "Initial Consultation",
                                                 "New Patient Assessment") ~ "Initial Consultation or assessment",
                          AppointmentType %in% c("Follow-up Consultation",
                                                 "Follow up appointment",
                                                 "Follow up appointment - 48 hours",
                                                 "Follow up appointment - 7 days",
                                                 "Follow up assessment") ~ "Follow up",
                          AppointmentType %in% c("NULL","Unknown") ~ "Unknown",
                          AppointmentType %in% c("Outpatient") ~ "Outpatient",
                          AppointmentType %in% c("Telephone Contact") ~ "Telephone Contact",
                          AppointmentType %in% c("Unplanned Contact") ~ "Unplanned Contact"
                          )) %>%
  group_by(ReferralMonth,app_cat) %>%
  summarise(n = n()) %>%
  mutate(Ref_month = as.Date(paste0("01 ",ReferralMonth),format = "%d %B %Y")) %>%
  filter(Ref_month > as.Date("2013-01-01"))

ggplot(number_of_refs) + 
  geom_bar(aes(x=Ref_month, y= n,fill = app_cat),stat='identity')

fwrite(number_of_refs %>% mutate(n = suppress_values(n)),"Outputs/draft/O4/Number_of_refs.csv")



#Check average length of referral

referral_time <- mhactiv %>%
  group_by(Patient_DerivedKey,ReferralDate) %>%
  summarise(duration = max(ContactDate) - min(ContactDate),
            ref_month = unique(ReferralMonth),
            Ref_month_date = unique(as.Date(paste0("01 ",unique(ReferralMonth)),format = "%d %B %Y")),
            year = unique(substr(ReferralDate,1,4)))

referral_time$duration<-as.numeric(referral_time$duration)

referral_time2 <- referral_time %>% ungroup() %>%
  select(-Patient_DerivedKey,ReferralDate) %>%
  filter(!is.na(year)) %>%
  filter(between(year,2010,2021)) %>%
  group_by(year) %>%
  summarise(n=n(),
            mean_dur = mean(duration,na.rm = T),
            medial_dur = median(duration, na.rm = T))

ggplot(referral_time2) + 
  geom_boxplot(aes(x=year,y=duration))

fwrite(referral_time2,"Outputs/draft/O4/average_ref_time.csv")

#Check average time from child to adult
child_to_adult <- mhactiv %>% 
  group_by(Patient_DerivedKey) %>%
  filter(grepl("Adult",ServiceGroup_type) & Event_type != "Specialist") %>%
  filter(DerivedAge_Derived<18)
  
child_to_adult <- mhactiv %>% filter(Patient_DerivedKey %in% child_to_adult$Patient_DerivedKey)

child_to_adult <- child_to_adult %>%
  group_by(Patient_DerivedKey,ReferralDate) %>%
  mutate(adult = any(ServiceGroup_type %in% "Adult"),
         child = any(ServiceGroup_type %in% "CAMHS")) %>%
  filter(adult & child)

child_to_adult <- child_to_adult %>%
  group_by(Patient_DerivedKey,ReferralDate) %>%
  arrange(ContactDate) %>%
  mutate(prev_case = paste0(lag(ServiceGroup_type),"-",ServiceGroup_type),
         prev_date = lag(ContactDate)) %>%
  mutate(time_between = as.numeric(ContactDate - prev_date))

time_change <- child_to_adult %>%
  select(ReferralMonth,prev_case,time_between) %>%
  mutate(Ref_month = as.Date(paste0("01 ",ReferralMonth),format = "%d %B %Y"),
         year = substr(Ref_month,1,4)) 

time_change$year <- factor(time_change$year,2012:2021)

ggplot(time_change) +
  geom_boxplot(aes(x=year,y=time_between,color=prev_case)) +
  scale_x_discrete(drop=F)

fwrite(time_change,"Outputs/draft/O4/cta_times.csv")




