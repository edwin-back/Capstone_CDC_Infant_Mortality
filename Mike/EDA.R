library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
options(digits=10)


nat2016 <- read.csv('data/nat2016.csv')
glimpse(nat2016)





####################################################################################################################
nat_2016.groupby(["APGAR_score_10min", 
                  "APGAR_score_5min", "combined_gestation_wk", 
                  "infant_living_at_report"])[["fathers_education", "mothers_education", "mothers_age",
                                               "fathers_age"]].mean()


nat2016 %>% 
  group_by(delivery_payment_source,attendant_at_birth,fetal_present_at_birth,final_delivery_method) %>% 
  summarise_at(vars(fathers_education,mothers_education,mothers_age,fathers_age),
               list(average = mean)) %>% 
  View()



nat2016 %>% 
  select(mothers_education) %>% 
  distinct() %>%
  group_by(format(round(mean(nat2016$APGAR_score_5min),3), nsmall = 3)) %>% 
  arrange(mothers_education)



nat2016 %>% 
  group_by(mothers_education,mothers_age,mothers_race) %>% 
  summarise_at(vars(infant_breastfed_at_discharge),
               list(average = mean))

####################################################################################################################




# average apgar5min score by mothers_education,mothers_age,mothers_race

nat2016 %>% 
  group_by(mothers_education,mothers_age,mothers_race) %>% 
  summarise_at(vars(APGAR_score_5min),
               list(average = mean))


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

nat2016 %>% group_by(anencephaly,meningo_spina_bif,cyn_cong_heart_disease,
                     cong_diaph_hernia,omphalocele,gastroschisis,limb_reduc_defect,
                     cleft_lip_or_palate,cleft_palate_only,down_syndr,suspect_chromo_disorder
                     ,hypospadias,no_cong_anamolies_checked
                     ) %>% 
  select(mothers_race) %>% 
  distinct()
  
  
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






