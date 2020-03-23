library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(plyr)
library(plotly)
options(digits=10)

####################################################################################################################

setwd("~/OneDrive/Desktop/Capstone Project/Natality 2016")



nat2016 <- read.csv('data/nat2016.csv')
glimpse(nat2016)

nat2018 <- read.csv('./data/nat2018.csv')
glimpse(nat2018)


####################################################################################################################

plot_ly(APGAR16, x = ~birth_weight_gm, y =~APGAR_score_5min_average, type = 'box', name = "APGAR 5") %>% 
  add_trace(y = ~APGAR_score_5min_average, name = 'Birth Weight') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(APGAR16, x = ~APGAR_score_10min, y =~birth_weight_gm_average, type = 'box', name = "APGAR 10") %>% 
  add_trace(y = ~birth_weight_gm_average, name = 'Birth Weight') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

########################## APGAR SCORE ##########################



# APGAR DataFrame
APGAR16 <- nat2016 %>% 
  group_by(APGAR_score_5min,APGAR_score_10min,
           combined_gestation_wk,birth_weight_gm) %>% 
  summarise_at(vars(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                    mothers_hispanic_origin2,fathers_race,fathers_hispanic_origin2,n_prenatal_visits,
                    mothers_marital_status,mothers_bmi,last_norm_menses_mo),
               list(average = mean))

test1 <- nat2016 %>% 
  group_by(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                    mothers_hispanic_origin2,fathers_race,fathers_hispanic_origin2,n_prenatal_visits,
                    mothers_marital_status,mothers_bmi,wic) %>% 
  summarise_at(APGAR_score_5min,APGAR_score_10min,
               combined_gestation_wk,birth_weight_gm)





plot_ly(test1, x = ~APGAR_score_5min, y = ~mothers_education_average, type = 'bar', name = "mothers_education_average") %>%
  add_trace(y = ~fathers_education_average, name = 'Fathers Education') %>%
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(test1, x = ~APGAR_score_5min, y = ~mothers_education_average, type = 'bar', name = "mothers_education_average") %>%
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>%
  layout(yaxis = list(title = 'Average'), barmode = 'group')

plot_ly(APGAR16, x = ~APGAR_score_5min, y = ~mothers_education_average, type = 'bar', name = "APGAR 5") %>%
  add_trace(y = ~fathers_education_average, name = 'Fathers Education') %>%
  layout(yaxis = list(title = 'Average'), barmode = 'group')


#APGAR Mothers Education
APGAR5_MEDU <- plot_ly(APGAR16, x = ~APGAR_score_5min, y = ~mothers_education_average, type = 'bar', name = "APGAR 5") %>%
  add_trace(y = ~fathers_education_average, name = 'Fathers Education') %>%
  layout(yaxis = list(title = 'Average'), barmode = 'group')
APGAR5_MEDU


APGAR10_MEDU <- plot_ly(APGAR16, x = ~APGAR_score_10min, y = ~mothers_education_average, type = 'bar', name = "APGAR 10") %>%
  add_trace(y = ~mothers_education_average, name = 'Mothers Education') %>%
  layout(yaxis = list(title = 'Average'), barmode = 'group')
APGAR10_MEDU  
  

#APGAR Mothers Age
APGAR5_MAGE <- plot_ly(APGAR16, x = ~APGAR_score_5min, y = ~mothers_age_average, type = 'bar', name = "Mother's Age") %>% 
  add_trace(y = ~fathers_age_average, name = 'Fathers Age') %>%
  layout(yaxis = list(title = 'Average'), barmode = 'group')
APGAR5_MAGE
  

APGAR10_MAGE <- plot_ly(APGAR16, x = ~APGAR_score_10min, y = ~mothers_age_average, type = 'bar', name = "Mother's Age") %>% 
  add_trace(y = ~fathers_age_average, name = 'Fathers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')
APGAR10_MAGE


#APGAR Mothers Prenatal Visits
APGAR5_MPRE <- plot_ly(APGAR16, x = ~APGAR_score_5min, y = ~n_prenatal_visits_average, type = 'bar', name = "APGAR 5") %>% 
  add_trace(y = ~n_prenatal_visits_average, name = 'Number of Prenatal Visits') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


APGAR10_MPRE <- plot_ly(APGAR16, x = ~APGAR_score_10min, y = ~n_prenatal_visits_average, type = 'bar', name = "APGAR 10") %>% 
  add_trace(y = ~n_prenatal_visits_average, name = 'Number of Prenatal Visits') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


#APGAR Gestation Period
plot_ly(test1, x = ~APGAR_score_5min, y =~combined_gestation_wk_average, type = 'box', name = "APGAR 5") %>% 
  add_trace(y = ~combined_gestation_wk_average, name = 'Gestation Weeks') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(test1, x = ~APGAR_score_10min, y =~combined_gestation_wk_average, type = 'box', name = "APGAR 10") %>% 
  add_trace(y = ~combined_gestation_wk_average, name = 'Gestation Weeks') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


#APGAR Mother's BMI
plot_ly(APGAR16, x = ~APGAR_score_5min, y = ~mothers_bmi_average, type = 'bar', name = "APGAR 5") %>% 
  add_trace(y = ~mothers_bmi_average, name = 'BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(APGAR16, x = ~APGAR_score_10min,y = ~mothers_bmi_average, type = 'box', name = 'APGAR 10') %>% 
  add_trace(y = ~wic_average, name = 'BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group') 


#APGAR Marital Status
plot_ly(APGAR16, x = ~APGAR_score_5min, y = ~mothers_marital_status_average, type = 'bar', name = "APGAR 5") %>% 
  add_trace(y = ~mothers_marital_status_average, name = 'Marital Status') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(APGAR16, x = ~APGAR_score_10min, y = ~mothers_marital_status_average, type = 'bar', name = "APGAR 10") %>% 
  add_trace(y = ~mothers_marital_status_average, name = 'Marital Status') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


#APGAR Mothers Race
plot_ly(APGAR16, x = ~APGAR_score_10min, y = ~mothers_race_average, type = 'bar', name = "APGAR 10") %>% 
  add_trace(y = ~fathers_race_average, name = 'Fathers Race') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')






# Mothers BMI

infant_risk <-  nat2016 %>% 
  group_by(mothers_bmi,pre_preg_diab,gest_diab,pre_preg_hypten,gest_hypten,
                    hypten_ecl,prev_preterm_birth,infertility_treatment,fertil_enhance,asst_repro_tech,
                    n_prev_cesar,no_risk_reported) %>% 
  summarise_at(vars(birth_weight_gm, 
                    sex_of_infant),
               list(average = mean))

infant_risk2 <- nat2016 %>% 
  group_by(sex_of_infant) %>% 
  summarise_at(vars(mothers_bmi,birth_weight_gm,pre_preg_diab,gest_diab,pre_preg_hypten,gest_hypten,
                    hypten_ecl,prev_preterm_birth,infertility_treatment,fertil_enhance,asst_repro_tech,
                    n_prev_cesar,no_risk_reported),
               list(average = mean))





# Mothers BMI vs Infant Weight

plot_ly(infant_risk, x = ~mothers_bmi, y = ~birth_weight_gm_average, type = 'bar', name = 'Mothers BMI') %>% 
  add_trace(y = ~birth_weight_gm_average, name = 'Birth Weight') %>%
  layout(yaxis = list(title = 'Average'), barmode = 'group')



# Infant weight based on Mothers BMI
plot_ly(infant_risk2, x = ~sex_of_infant, y = ~birth_weight_gm_average, type = 'bar', name = 'Birth Weight') %>% 
  add_trace(y = ~mothers_bmi_average, name = 'Mothers BMI') %>%
  layout(yaxis = list(title = 'Average'), barmode = 'group')


########################## Gestation Period ##########################



# Gestation vs. Mothers age
plot_ly(APGAR16, x = ~combined_gestation_wk, y = ~mothers_age_average, type = 'bar', name = "Mother's Age") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

#Gestation vs. Mothers Education
plot_ly(APGAR16, x = ~combined_gestation_wk, y = ~mothers_education_average, type = 'bar', name = "Gestation Period") %>% 
  add_trace(y = ~mothers_education_average, name = 'Mothers Education') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Gestation vs. Last Normal Menst Cycle
plot_ly(APGAR16, x = ~combined_gestation_wk, y = ~last_norm_menses_mo_average, type = 'bar', name = "ast Normal Menstreul Cycle") %>% 
  add_trace(y = ~last_norm_menses_mo_average, name = 'Last Normal Menstreul Cycle') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')



########################## Abnormal Conditions ##########################

abnormal <- nat2016 %>% 
  group_by(mothers_bmi,pre_preg_diab,gest_diab,pre_preg_hypten,gest_hypten,
           hypten_ecl,prev_preterm_birth,infertility_treatment,fertil_enhance,asst_repro_tech,
           n_prev_cesar,no_risk_reported,mothers_age,induced_labor) %>% 
  summarise_at(vars(admit_NICU, 
                    assist_vent_after6),
               list(average = mean))

abnormal2 <- nat2016 %>% 
  group_by(admit_NICU,surfactant,antibiotics_for_newborn,seizures,assist_vent_immed,assist_vent_after6) %>% 
  summarise_at(vars(mothers_bmi,mothers_age),
               list(average = mean))


# Mother's BMI vs. Admit to NICU
plot_ly(abnormal2, x = ~admit_NICU, y = ~mothers_bmi_average, type = 'bar', name = "Mothers BMI") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's Age vs. Admit to NICU
plot_ly(abnormal2, x = ~admit_NICU, y = ~mothers_age_average, type = 'bar', name = "Mothers Age") %>% 
  add_trace(y = ~mothers_bmi_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's Age vs. Surfactant
plot_ly(abnormal2, x = ~surfactant, y = ~mothers_age_average, type = 'bar', name = "Surfactant") %>% 
  add_trace(y = ~mothers_bmi_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's Age vs. Surfactant
plot_ly(abnormal2, x = ~surfactant, y = ~mothers_age_average, type = 'bar', name = "Mothers Age") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's BMI vs. Antibiotics
plot_ly(abnormal2, x = ~antibiotics_for_newborn, y = ~mothers_bmi_average, type = 'bar', name = "antibiotics_for_newborn") %>% 
  add_trace(y = ~mothers_bmi_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's AGE vs. Antibiotics
plot_ly(abnormal2, x = ~antibiotics_for_newborn, y = ~mothers_age_average, type = 'bar', name = "antibiotics_for_newborn") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's AGE vs. assist_vent_immed
plot_ly(abnormal2, x = ~assist_vent_immed, y = ~mothers_age_average, type = 'bar', name = "assist_vent_immed") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers AGE') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's BMI vs. assist_vent_after6
plot_ly(abnormal2, x = ~assist_vent_after6, y = ~mothers_bmi_average, type = 'bar', name = "assist_vent_after6") %>% 
  add_trace(y = ~mothers_bmi_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's AGE vs. assist_vent_immed
plot_ly(abnormal2, x = ~assist_vent_immed, y = ~mothers_age_average, type = 'bar', name = "assist_vent_immed") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

# Mother's BMI vs. assist_vent_after6
plot_ly(abnormal2, x = ~assist_vent_after6, y = ~mothers_bmi_average, type = 'bar', name = "assist_vent_after6") %>% 
  add_trace(y = ~mothers_bmi_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


# Mother's AGE vs. seizures
plot_ly(abnormal2, x = ~seizures, y = ~mothers_age_average, type = 'bar', name = "seizures") %>% 
  add_trace(y = ~mothers_bmi_average, name = 'Mothers BMI') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')

########################## Anomalies ##########################

anomalies <- nat2016 %>% 
  group_by(anencephaly,meningo_spina_bif,cyn_cong_heart_disease,
           cong_diaph_hernia,omphalocele,gastroschisis,limb_reduc_defect,
           cleft_lip_or_palate,cleft_palate_only,down_syndr,suspect_chromo_disorder,
           hypospadias,no_cong_anamolies_checked) %>% 
  summarise_at(vars(mothers_bmi,mothers_age),
               list(average = mean))

plot_ly(anomalies, x = ~anencephaly, y = ~mothers_bmi_average, type = 'bar', name = "seizures") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~meningo_spina_bif, y = ~mothers_bmi_average, type = 'bar', name = "meningo_spina_bif") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~cyn_cong_heart_disease, y = ~mothers_bmi_average, type = 'bar', name = "cyn_cong_heart_disease") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~cong_diaph_hernia, y = ~mothers_bmi_average, type = 'bar', name = "cong_diaph_hernia") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~omphalocele, y = ~mothers_bmi_average, type = 'bar', name = "omphalocele") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~gastroschisis, y = ~mothers_bmi_average, type = 'bar', name = "gastroschisis") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~limb_reduc_defect, y = ~mothers_bmi_average, type = 'bar', name = "limb_reduc_defect") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~cleft_lip_or_palate, y = ~mothers_bmi_average, type = 'bar', name = "cleft_lip_or_palate") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~cleft_palate_only, y = ~mothers_bmi_average, type = 'bar', name = "cleft_palate_only") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~down_syndr, y = ~mothers_bmi_average, type = 'bar', name = "down_syndr") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~suspect_chromo_disorder, y = ~mothers_bmi_average, type = 'bar', name = "suspect_chromo_disorder") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~hypospadias, y = ~mothers_bmi_average, type = 'bar', name = "hypospadias") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(anomalies, x = ~no_cong_anamolies_checked, y = ~mothers_bmi_average, type = 'bar', name = "no_cong_anamolies_checked") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')




########################## Infant ##########################

infant_after <- nat2016 %>% 
  group_by(infant_living_at_report,infant_breastfed_at_discharge,last_norm_menses_mo) %>% 
  summarise_at(vars(mothers_bmi,mothers_age,mothers_race,mothers_education,fathers_education),
               list(average = mean))



plot_ly(infant_after, x = ~infant_living_at_report, y = ~mothers_bmi_average, type = 'bar', name = "mothers_bmi_average") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')


plot_ly(infant_after, x = ~infant_living_at_report, y = ~mothers_bmi_average, type = 'bar', name = "mothers_bmi_average") %>% 
  add_trace(y = ~mothers_age_average, name = 'Mothers Age') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')



plot_ly(infant_after, x = ~infant_breastfed_at_discharge, y = ~mothers_education_average, type = 'box', name = "mothers_education_average") %>% 
  add_trace(y = ~mothers_bmi_average, name = 'mothers_bmi_average') %>% 
  layout(yaxis = list(title = 'Average'), barmode = 'group')




nat2016 %>% 
  group_by(infant_breastfed_at_discharge) %>% 
  summarise_at(vars(mother),
               list(average = n)) 

















########################## NICU ##########################




# average apgar10min score by mothers_education,mothers_age,mothers_race

nat2016 %>% 
  group_by(mothers_education,mothers_age,mothers_race) %>% 
  summarise_at(vars(APGAR_score_10min),
               list(average = mean))



# average gestation period by mothers_education,mothers_age,mothers_race
nat2016 %>% 
  group_by(mothers_education,mothers_age,mothers_race) %>% 
  summarise_at(vars(combined_gestation_wk),
               list(average = mean))


# # admitted to NICU by race
nat2016 %>% 
  group_by(mothers_race) %>% 
  select(admit_NICU) %>% 
  count() %>% View()



# # breastfed at discharge by race
nat2016 %>% 
  group_by(mothers_race,mothers_education) %>% 
  filter(infant_breastfed_at_discharge == 'Y') %>% 
  select(infant_breastfed_at_discharge) %>% 
  count()

nat2016 %>% 
  group_by(mothers_race,mothers_education) %>% 
  filter(infant_breastfed_at_discharge == 'N') %>% 
  select(infant_breastfed_at_discharge) %>% 
  count()


# # admitted to NICU by race
nat2016 %>% 
  group_by(mothers_race,mothers_education) %>% 
  filter(admit_NICU == 'Y') %>% 
  select(admit_NICU) %>% 
  count()

nat2016 %>% 
  group_by(mothers_race) %>% 
  filter(admit_NICU == 'N') %>% 
  select(admit_NICU)


# birth_weight_gm','assist_vent_immed','assist_vent_after6','admit_NICU','antibiotics_for_newborn'

nat2016 %>% 
  group_by(assist_vent_immed,assist_vent_after6,surfactant,antibiotics_for_newborn) %>% 
  select(sex_of_infant) %>% 
  count()


nat2016 %>% 
  group_by(assist_vent_immed,assist_vent_after6,surfactant,antibiotics_for_newborn) %>% 
  select(mothers_age) %>% 
  
  
  
# CONGENITAL ANOMALIES OF THE NEWBORN COLUMNS
  
  
  
  nat2016 %>% group_by(
    anencephaly,
    meningo_spina_bif,
    cyn_cong_heart_disease,
    
    cong_diaph_hernia,
    omphalocele,
    gastroschisis,
    
    limb_reduc_defect,
    cleft_lip_or_palate,
    cleft_palate_only,
    
    down_syndr,
    suspect_chromo_disorder,
    hypospadias,
    no_cong_anamolies_checked
  ) %>%
  summarise_at(vars(infant_living_at_report),
               list(count = mean)) %>% View()



nat2016 %>% group_by(anencephaly,
                     meningo_spina_bif,
                     cyn_cong_heart_disease,
                     ) %>% 
  summarise_at(vars(birth_weight_gm,APGAR_score_10min,APGAR_score_5min),
               list(count = mean))


  
# ABNORMAL CONDITIONS OF THE NEWBORN

nat2016 %>% 
  group_by(assist_vent_immed,assist_vent_after6,admit_NICU,surfactant,antibiotics_for_newborn,seizures) %>%
  select(mothers_race,mothers_age) %>% 
  count() %>% View()
  

# # admitted to NICU by race and age
nicu <- nat2016 %>% 
  group_by(mothers_race,mothers_age,mothers_education,mothers_hispanic_origin2) %>% 
  select(admit_NICU) %>% 
  count()









ggplot(nicu,aes(x = nicu$mothers_race, y = nicu$n)) + geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  xlab("Mother's Race") + ylab("NICU Number")


ggplot(nicu,aes(x = nicu$mothers_age, y = nicu$n)) + geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  xlab("Mother's Age") + ylab("NICU Number")


ggplot(nicu,aes(x = nicu$mothers_education, y = nicu$n)) + geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  xlab("Mother's Education") + ylab("NICU Number")

ggplot(nicu,aes(x = nicu$mothers_hispanic_origin2, y = nicu$n)) + geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  xlab("Mother's Hispanic Origin") + ylab("NICU Number")





nat2018 %>%
  group_by(delivery_payment_source,
           attendant_at_birth, fetal_present_at_birth, final_delivery_method) %>%
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean)) %>% 
  View()











#######################

# nat_2016.groupby(["APGAR_score_10min", 
#                   "APGAR_score_5min", "combined_gestation_wk", 
#                   "infant_living_at_report"])[["fathers_education", "mothers_education", "mothers_age",
#                                                "fathers_age"]].mean()


nat18 %>% 
  group_by(delivery_payment_source,attendant_at_birth,fetal_present_at_birth,final_delivery_method) %>% 
  summarise_at(vars(fathers_education,mothers_education,mothers_age,fathers_age),
               list(average = mean)) %>% 
  View()




nat2016 %>% 
  group_by(mothers_education) %>% 
  summarise_at(vars(APGAR_score_5min),
               list(average = mean)) 

#This code above I think is much better for the APGAR score for 5 minutes, because it gives an average of the scores for 5 minutes.


nat2016 %>% 
  group_by(infant_breastfed_at_discharge) %>% 
  summarise_at(vars(mothers_education,mothers_age,mothers_race),
               list(average = mean)) 

#I think that this works better, because the education and ages are numeric compared to the breast fed question which is a Yes or No question. 

####################################################################################################################



# average apgar5min score by mothers_education,mothers_age,mothers_race

nat18 %>% 
  group_by(mothers_age,mothers_race) %>% 
  summarise_at(vars(APGAR_score_5min),
               list(average = mean))


# average apgar10min score by mothers_education,mothers_age,mothers_race

nat18 %>% 
  group_by(mothers_education,mothers_age,mothers_race) %>% 
  summarise_at(vars(APGAR_score_10min),
               list(average = mean))




# average gestation period by mothers_education,mothers_age,mothers_race
nat18 %>% 
  group_by(mothers_education,mothers_age,mothers_race) %>% 
  summarise_at(vars(combined_gestation_wk),
               list(average = mean))


# # admitted to NICU by race
nat18 %>% 
  group_by(mothers_race) %>% 
  select(admit_NICU) %>% 
  count() %>% View()



# # breastfed at discharge by race
nat18 %>% 
  group_by(mothers_race,mothers_education) %>% 
  filter(infant_breastfed_at_discharge == 'Y') %>% 
  select(infant_breastfed_at_discharge) %>% 
  count()

nat18 %>% 
  group_by(mothers_race,mothers_education) %>% 
  filter(infant_breastfed_at_discharge == 'N') %>% 
  select(infant_breastfed_at_discharge) %>% 
  count()


# # admitted to NICU by race
nat18 %>% 
  group_by(mothers_race,mothers_education) %>% 
  filter(admit_NICU == 'Y') %>% 
  select(admit_NICU) %>% 
  count()

nat18 %>% 
  group_by(mothers_race) %>% 
  filter(admit_NICU == 'N') %>% 
  select(admit_NICU)


# birth_weight_gm','assist_vent_immed','assist_vent_after6','admit_NICU','antibiotics_for_newborn'

nat18 %>% 
  group_by(assist_vent_immed,assist_vent_after6,surfactant,antibiotics_for_newborn) %>% 
  select(sex_of_infant) %>% 
  count()


nat18 %>% 
  group_by(assist_vent_immed,assist_vent_after6,surfactant,antibiotics_for_newborn) %>% 
  select(mothers_age) %>% 
  
  
  
  # CONGENITAL ANOMALIES OF THE NEWBORN COLUMNS
  
  nat18 %>% group_by(anencephaly,meningo_spina_bif,cyn_cong_heart_disease,
                     cong_diaph_hernia,omphalocele,gastroschisis,limb_reduc_defect,
                     cleft_lip_or_palate,cleft_palate_only,down_syndr,suspect_chromo_disorder
                     ,hypospadias,no_cong_anamolies_checked
  ) %>% 
  select(mothers_race) %>%
  distinct()

#This code is not working for me 




# ABNORMAL CONDITIONS OF THE NEWBORN

nat18 %>% 
  group_by(assist_vent_immed,assist_vent_after6,admit_NICU,surfactant,antibiotics_for_newborn,seizures) %>%
  select(mothers_race,mothers_age) %>% 
  count() %>% View()


# # admitted to NICU by race and age
nicu <- nat18 %>% 
  group_by(mothers_race,mothers_age,mothers_education,mothers_hispanic_origin2) %>% 
  select(admit_NICU) %>% 
  count()



ggplot(nicu,aes(x = nicu$mothers_race, y = nicu$n)) + geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  xlab("Mother's Race") + ylab("NICU Number")


ggplot(nicu,aes(x = nicu$mothers_age, y = nicu$n)) + geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  xlab("Mother's Age") + ylab("NICU Number")


ggplot(nicu,aes(x = nicu$mothers_education, y = nicu$n)) + geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  xlab("Mother's Education") + ylab("NICU Number")

ggplot(nicu,aes(x = nicu$mothers_hispanic_origin2, y = nicu$n)) + geom_bar(stat = "identity", fill = "blue", alpha = 0.8) +
  xlab("Mother's Hispanic Origin") + ylab("NICU Number")






















































































































