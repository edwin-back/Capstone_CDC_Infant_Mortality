library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
options(digits=10)


View(nat_quart_2018)

nat18 <- read.csv('./nat_quart_2018.csv')
View(nat18)
glimpse(nat18)

#Delivery method, Attendant present, position of infant coming out, and final delivery method all in realtion to the parents age, and education (averaged)

nat18 %>% 
  group_by(delivery_payment_source,attendant_at_birth,fetal_present_at_birth,final_delivery_method) %>% 
  summarise_at(vars(fathers_education,mothers_education,mothers_age,fathers_age),
               list(average = mean))


#Delivery method, Attendant present, position of infant coming out, and final delivery method all in realtion to the mothers average bmi, pre pregnancy pounds, and delivery pounds


nat18 %>% 
  group_by(delivery_payment_source, 
           attendant_at_birth, fetal_present_at_birth, final_delivery_method) %>% 
  summarise_at(vars(mothers_bmi,delivery_lbs,pre_preg_lbs),
               list(average = mean))






###################


##gestation and fertil enhancments made to the mother in relation to parents age and education, part A. (averaged)

nat18 %>% 
  group_by(pre_preg_diab, pre_preg_diab, gest_diab, pre_preg_hypten, gest_hypten, 
           hypten_ecl, prev_preterm_birth) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))


#gestation and fertil enhancments made to the mother in relation to parents age and education, part B. (averaged)


nat18 %>% 
  group_by(infertility_treatment, fertil_enhance, asst_repro_tech) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))





#################

#gestation and fertil enhancments made to the mother in relation to mothers bmi and pre/post birth weight, part A. (averaged)

nat18 %>% 
  group_by(pre_preg_diab, pre_preg_diab, gest_diab, pre_preg_hypten, gest_hypten, 
           hypten_ecl, prev_preterm_birth) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))

#gestation and fertil enhancments made to the mother in relation to mothers bmi and pre/post birth weight, part B. (averaged)

nat18 %>% 
  group_by(infertility_treatment, fertil_enhance, asst_repro_tech) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))




###############

#Infections listed during pregnancy in relation to the parents age and education. (averaged)

nat18 %>% 
  group_by(gonorrhea, syphilis, chlamydia, hepB, hepC) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Infections listed during pregnancy in relation to the mothers bmi, and pre/post pregnancy. (averaged)

nat18 %>% 
  group_by(gonorrhea, syphilis, chlamydia, hepB, hepC) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))






##################3

#Average level of infections reported on a regular basis in relation to the parents age and education.

nat18 %>% 
  group_by(no_infection_reported) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Average level of infections reported on a regular basis in relation to the mothers bmi and pre/post preganancy weight.

nat18 %>% 
  group_by(no_infection_reported) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))





##################3

#Reported medication used during infants birth in relation to the average parents education and age.

nat18 %>% 
  group_by(success_ext_cep, fail_ext_cep, induced_labor, aug_labor,
                       steriods, antibiotics, chorioamnionitis, anesthesia) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Reported medication used during infants birth in relation to the mothers average bmi, pre preganancy and delivery weight.

nat18 %>% 
  group_by(success_ext_cep, fail_ext_cep, induced_labor, aug_labor,
           steriods, antibiotics, chorioamnionitis, anesthesia) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))



##################3

#Type of fetal present at birth in relation to the average age, education of the parents; bmi, and weight of the mother.


nat18 %>% 
  group_by(fetal_present_at_birth) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))


#Type of fetal present at birth in relation to the average age, education of the parents; bmi, and weight of the mother.

nat18 %>% 
  group_by(fetal_present_at_birth) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))




#################3


#Reported delivery method used at birth in relation to the average age, education of the parents; bmi, and weight of the mother.

nat18 %>% 
  group_by(final_delivery_method) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))


nat18 %>% 
  group_by(final_delivery_method) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))




##################

#Number of prenatal visits made in relation to the parents average education and age.

nat18 %>% 
  group_by(n_prenatal_visits) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Number of prenatal visits made in relation to the mothers bmi, pre pregnancy weight and delivery weight.

nat18 %>% 
  group_by(n_prenatal_visits) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))



######################

#Paternity acknowlegment in realtion to the parents age and education. (mean)

nat18 %>% 
  group_by(paternity_acknow) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Paternity acknowlegment in realtion to the mothers bmi, pre pregnancy weight and delivery weight. (mean)

nat18 %>% 
  group_by(paternity_acknow) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))




########################

#Mothers reported hispanic origin in relation to their average age and education, as well as the fathers.

nat18 %>% 
  group_by(mothers_hispanic_origin2) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Mothers reported hispanic origin in relation to their average bmi, pre pregnancy weight and delivery weight.

nat18 %>% 
  group_by(mothers_hispanic_origin2) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))






#######################

#Fathers reported hispanic origin in relation to their average age and education, as well as the fathers.

nat18 %>% 
  group_by(fathers_hispanic_origin2) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Fathers reported hispanic origin in relation to their average bmi, pre pregnancy weight and delivery weight.

nat18 %>% 
  group_by(fathers_hispanic_origin2) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))






####################

#Under father, the average level of infants breatfed at discharge, these numbers were transformed to prove an 
#interesting point with the correlation between parent af the breast feeding occurring.

father_breast %>% 
  group_by(fathers_education) %>% 
  summarise_at(vars(infant_breastfed_at_discharge),
               list(average = mean))

#Under mother, the average level of infants breatfed at discharge, these numbers were transformed to prove an 
#interesting point with the correlation between parent af the breast feeding occurring.

mother_breast %>% 
  group_by(mothers_education) %>% 
  summarise_at(vars(infant_breastfed_at_discharge),
               list(average = mean))






#####################

#Mothers use of tobacco in all 3 trimesters on average.

A= nat18 %>% 
  group_by(mothers_education) %>% 
  summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
               list(average = mean))

#Mothers use of tobacco in all 3 trimesters (sum)

nat18 %>% 
  group_by(mothers_age) %>% 
  summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
               list(average = mean))

#Mothers use of tobacco in all 3 trimesters on average.

nat18 %>% 
  group_by(fathers_education) %>% 
  summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
               list(average = mean))


#Fathers use of tobacco in all 3 trimesters (sum)

nat18 %>% 
  group_by(fathers_age) %>% 
  summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
               list(average = mean))







#######################

#Average parental education, births and APGAR score shown in every state depending on whether or not WIC was used.


WIC_table= WIC_cleaned %>% 
  group_by(State_of_Residence, WIC) %>% 
  summarise_at(vars(Mothers_Education_Code, Fathers_Education_Code, Births, Ten_Minute_APGAR_Score),
               list(average = mean))


View(WIC_table)

#load this CSV file into the R tablet 








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




nat18 %>% 
  group_by(mothers_education) %>% 
  summarise_at(vars(APGAR_score_5min),
               list(average = mean)) 

#This code above I think is much better for the APGAR score for 5 minutes, because it gives an average of the scores for 5 minutes.


nat18 %>% 
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






















































































































