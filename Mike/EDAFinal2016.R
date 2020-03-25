library(shiny)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(grid)
library(gridExtra)
library(plotly)
options(digits=10)

####################################################################################################################

setwd("~/OneDrive/Desktop/Capstone Project/Natality 2016")



nat2016 <- read.csv('data/nat2016.csv')
nat2017 <- read.csv('data/nat2017.csv')
nat2018 <- read.csv('data/nat2018.csv')

natall <- rbind(nat2016,nat2017,nat2018)

glimpse(nat2016)
glimpse(nat2017)
glimpse(nat2018)
glimpse(natall)

write.csv(natall,'natall.csv')

####################################################  APGAR Data  ###############################################
APGAR <- natall %>%
  group_by(APGAR_score_5min,APGAR_score_10min,mothers_marital_status) %>%
  summarise_at(vars(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                    mothers_hispanic_origin2,fathers_race,fathers_hispanic_origin2,n_prenatal_visits,
                    mothers_bmi,last_norm_menses_mo,combined_gestation_wk,birth_weight_gm),
               list(average = mean))


####################################################  Infant Data  ###############################################
infant_after <- natall %>%
  group_by(infant_living_at_report,infant_breastfed_at_discharge,last_norm_menses_mo) %>%
  summarise_at(vars(mothers_bmi,mothers_age,mothers_race,mothers_education,fathers_education),
               list(average = mean))



####################################################  Abnormal Data  ###############################################
abnormal <- natall %>%
  group_by(admit_NICU,surfactant,antibiotics_for_newborn,seizures,assist_vent_immed,assist_vent_after6) %>%
  summarise_at(vars(mothers_bmi,mothers_age),
               list(average = mean))



####################################################  Anomalies Data  ###############################################
anomalies <- natall %>%
  group_by(assist_vent_immed,anencephaly,meningo_spina_bif,cyn_cong_heart_disease,
           cong_diaph_hernia,omphalocele,gastroschisis,limb_reduc_defect,
           cleft_lip_or_palate,cleft_palate_only,down_syndr,suspect_chromo_disorder,
           hypospadias,no_cong_anamolies_checked) %>%
  summarise_at(vars(mothers_bmi,mothers_age),
               list(average = mean))



####################################################  Gestation Data  ###############################################
gestation <- natall %>%
  group_by(combined_gestation_wk) %>%
  summarise_at(vars(mothers_bmi),
               list(average = mean))


####################################################  Race Data  ###############################################
race <- natall %>%
  group_by(mothers_hispanic_origin2,mothers_race,infant_living_at_report) %>%
  summarise_at(vars(APGAR_score_5min,APGAR_score_10min),
               list(average = mean))



####################################################  Infant Living at Report  ##############################################################################################
# Infant Living at Report vs. Mother's BMI
living_bmi <- plot_ly(infant_after, x = ~infant_living_at_report, y = ~mothers_bmi_average, type = 'box', name = "mothers_bmi_average") %>%
  layout(title = "",
         barmode = 'group',
         xaxis = list(title = "Infant Living at Report"),
         yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
living_bmi



####################################################  Infant Breastfed  ###############################################
#Infant Breastfed vs. Mothers Education
breastfed_edu <- plot_ly(infant_after, x = ~infant_breastfed_at_discharge, y = ~mothers_education_average, type = 'box', name = "mothers_education_average") %>%
  layout(title = "",
         barmode = 'group',
         xaxis = list(title = "Infant Breasfed at Discharge"),
         yaxis = list(title = "Average Mother's Education"))
breastfed_edu



####################################################  Infant NICU  ###############################################
# Mother's BMI vs. Admit to NICU
NICU_bmi <- plot_ly(abnormal, x = ~admit_NICU, y = ~mothers_bmi_average, type = 'box', name = "mothers_bmi_average") %>%
  layout(title = "",
         barmode = 'group',
         xaxis = list(title = "Mother Admitted into NICU"),
         yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
NICU_bmi




####################################################  Gestation Period  ###############################################
# Gestation vs. BMI
gestation_bmi <- plot_ly(gestation, x = ~combined_gestation_wk, y = ~average, type = 'scatter', name = "last Normal Menstreul Cycle") %>%
  layout(title = "",
         barmode = 'group',
         xaxis = list(title = "Gestation Period (weeks)"),
         yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
gestation_bmi




####################################################  APGAR Scores  ###############################################
#APGAR Score 5 min vs. Mothers BMI
apgar5_bmi <- plot_ly(APGAR, x = ~APGAR_score_5min, y = ~mothers_bmi_average, type = 'box', name = "APGAR 5") %>%
  layout(title = "",
         barmode = 'group',
         xaxis = list(title = "APGAR 5 Min Scores"),
         yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
apgar5_bmi




#APGAR Score 10 min vs. Mothers BMI
apgar10_bmi <- plot_ly(APGAR, x = ~APGAR_score_10min, y = ~mothers_bmi_average, type = 'box', name = "APGAR 10") %>%
  layout(title = "",
         barmode = 'group',
         xaxis = list(title = "APGAR 10 Min Scores"),
         yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
apgar10_bmi


#Graph 6
output$"APGAR 10 Min Scoress vs. Average Mother's BMI" =rendorPlot({
  gestation_bmi
})





