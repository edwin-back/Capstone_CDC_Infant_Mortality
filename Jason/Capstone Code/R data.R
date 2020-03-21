library(shiny)
library(plotly)
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

Delivery_Education18= nat18 %>% 
  group_by(delivery_payment_source,attendant_at_birth) %>% 
  summarise_at(vars(fathers_education,mothers_education,mothers_age,fathers_age),
               list(average = mean))


Del_Edu18= read.csv('./Delivery_Education18.csv')


View(Delivery_Education18)


del_ed <- plot_ly(Del_Edu18, x = ~delivery_payment_source, y = ~fathers_education_average, type = 'bar', name = 'Fathers Education')
del_ed <- del_ed %>% add_trace(y = ~mothers_education_average, name = 'Mothers Education')
del_ed <- del_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
del_ed

del_age = plot_ly(Delivery_Education18, x = ~delivery_payment_source, y = ~fathers_age_average, type = 'bar', name = 'Fathers Age')
del_age = del_age %>% add_trace(y = ~mothers_age_average, name = 'Mothers Age')
del_age = del_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
del_age

att_ed <- plot_ly(Delivery_Education18, x = ~attendant_at_birth, y = ~fathers_education_average, type = 'bar', name = 'Fathers Education')
att_ed <- att_ed %>% add_trace(y = ~mothers_education_average, name = 'Mothers Education')
att_ed <- att_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
att_ed

att_age <- plot_ly(Delivery_Education18, x = ~attendant_at_birth, y = ~fathers_age_average, type = 'bar', name = 'Fathers Age')
att_age <- att_age %>% add_trace(y = ~mothers_age_average, name = 'Mothers Age')
att_age <- att_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
att_age

#Modify these columns 

#Grouped bar plot 

#Delivery method, Attendant present, position of infant coming out, and final delivery method all in realtion to the mothers average bmi, pre pregnancy pounds, and delivery pounds


Delivery_history18= nat18 %>% 
  group_by(delivery_payment_source, 
           attendant_at_birth) %>% 
  summarise_at(vars(mothers_bmi,delivery_lbs,pre_preg_lbs),
               list(average = mean))


Del_hist18= read.csv('./Delivery_history18.csv')

View(Delivery_history18)

del_bmi <- plot_ly(Del_hist18, x = ~delivery_payment_source, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
del_bmi <- del_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
del_bmi <- del_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Preg Weight')
del_bmi <- del_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
del_bmi

att_bmi <- plot_ly(Delivery_history18, x = ~attendant_at_birth, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
att_bmi <- att_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
att_bmi <- att_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Preg Weight')
att_bmi <- att_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
att_bmi

#Modify these columns 

#Grouped Barplot 



###################


##gestation and fertil enhancments made to the mother in relation to parents age and education, part A. (averaged)

gestation_education18= nat18 %>% 
  group_by(pre_preg_diab, gest_diab, pre_preg_hypten, gest_hypten, 
           hypten_ecl, prev_preterm_birth) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Education

pre_diab_ed <- plot_ly(gestation_education18, x = ~pre_preg_diab, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
pre_diab_ed <- pre_diab_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
pre_diab_ed <- pre_diab_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pre_diab_ed

gest_diab_ed <- plot_ly(gestation_education18, x = ~gest_diab, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
gest_diab_ed <- gest_diab_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
gest_diab_ed <- gest_diab_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gest_diab_ed

pre_hypten_ed <- plot_ly(gestation_education18, x = ~pre_preg_hypten, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
pre_hypten_ed <- pre_hypten_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
pre_hypten_ed <- pre_hypten_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pre_hypten_ed

gest_hypten_ed <- plot_ly(gestation_education18, x = ~gest_hypten, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
gest_hypten_ed <- pre_hypten_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
gest_hypten_ed <- pre_hypten_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gest_hypten_ed

hypten_ecl_ed <- plot_ly(gestation_education18, x = ~hypten_ecl, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
hypten_ecl_ed <- hypten_ecl_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
hypten_ecl_ed <- hypten_ecl_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hypten_ecl_ed

prev_preterm_birth_ed <- plot_ly(gestation_education18, x = ~prev_preterm_birth, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
prev_preterm_birth_ed <- prev_preterm_birth_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
prev_preterm_birth_ed <- prev_preterm_birth_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
prev_preterm_birth_ed

#Age

pre_diab_age <- plot_ly(gestation_education18, x = ~pre_preg_diab, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
pre_diab_age <- pre_diab_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
pre_diab_age <- pre_diab_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pre_diab_age

gest_diab_age <- plot_ly(gestation_education18, x = ~gest_diab, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
gest_diab_age <- gest_diab_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
gest_diab_age <- gest_diab_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gest_diab_age

pre_hypten_age <- plot_ly(gestation_education18, x = ~pre_preg_hypten, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
pre_hypten_age <- pre_hypten_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
pre_hypten_age <- pre_hypten_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pre_hypten_age

gest_hypten_age <- plot_ly(gestation_education18, x = ~gest_hypten, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
gest_hypten_age <- pre_hypten_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
gest_hypten_age <- pre_hypten_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gest_hypten_age

hypten_ecl_age <- plot_ly(gestation_education18, x = ~hypten_ecl, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
hypten_ecl_age <- hypten_ecl_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
hypten_ecl_age <- hypten_ecl_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hypten_ecl_age

prev_preterm_birth_age <- plot_ly(gestation_education18, x = ~prev_preterm_birth, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
prev_preterm_birth_age <- prev_preterm_birth_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
prev_preterm_birth_age <- prev_preterm_birth_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
prev_preterm_birth_age


#Grouped Barplot 

#gestation and fertil enhancments made to the mother in relation to parents age and education, part B. (averaged)


infertility_education18= nat18 %>% 
  group_by(infertility_treatment, fertil_enhance, asst_repro_tech) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Education

infertility_treatment_ed <- plot_ly(infertility_education18, x = ~infertility_treatment, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
infertility_treatment_ed <- infertility_treatment_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
infertility_treatment_ed <- infertility_treatment_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
infertility_treatment_ed

fertil_enhance_ed <- plot_ly(infertility_education18, x = ~fertil_enhance, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
fertil_enhance_ed <- fertil_enhance_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
fertil_enhance_ed <- fertil_enhance_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fertil_enhance_ed

asst_repro_tech_ed <- plot_ly(infertility_education18, x = ~asst_repro_tech, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
asst_repro_tech_ed <- asst_repro_tech_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
asst_repro_tech_ed <- asst_repro_tech_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
asst_repro_tech_ed


#Age

infertility_treatment_age <- plot_ly(infertility_education18, x = ~infertility_treatment, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
infertility_treatment_age <- infertility_treatment_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
infertility_treatment_age <- infertility_treatment_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
infertility_treatment_age

fertil_enhance_age <- plot_ly(infertility_education18, x = ~fertil_enhance, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
fertil_enhance_age <- fertil_enhance_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
fertil_enhance_age <- fertil_enhance_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fertil_enhance_age

asst_repro_tech_age <- plot_ly(infertility_education18, x = ~asst_repro_tech, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
asst_repro_tech_age <- asst_repro_tech_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
asst_repro_tech_age <- asst_repro_tech_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
asst_repro_tech_age



#Grouped barplot 


#################

#gestation and fertil enhancments made to the mother in relation to mothers bmi and pre/post birth weight, part A. (averaged)

gestation_history18= nat18 %>% 
  group_by(pre_preg_diab, gest_diab, pre_preg_hypten, gest_hypten, 
           hypten_ecl, prev_preterm_birth) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))


pre_diab_bmi <- plot_ly(gestation_history18, x = ~pre_preg_diab, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
pre_diab_bmi <- pre_diab_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
pre_diab_bmi <- pre_diab_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
pre_diab_bmi <- pre_diab_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pre_diab_bmi

gest_diab_bmi <- plot_ly(gestation_history18, x = ~gest_diab, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
gest_diab_bmi <- gest_diab_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
gest_diab_bmi <- gest_diab_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
gest_diab_bmi <- gest_diab_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gest_diab_bmi

pre_hypten_bmi <- plot_ly(gestation_history18, x = ~pre_preg_hypten, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
pre_hypten_bmi <- pre_hypten_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
pre_hypten_bmi <- pre_hypten_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Delivery Weight')
pre_hypten_bmi <- pre_hypten_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pre_hypten_bmi

gest_hypten_bmi <- plot_ly(gestation_history18, x = ~gest_hypten, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
gest_hypten_bmi <- gest_hypten_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
gest_hypten_bmi <- gest_hypten_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Delivery Weight')
gest_hypten_bmi <- gest_hypten_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gest_hypten_bmi

hypten_ecl_bmi <- plot_ly(gestation_history18, x = ~hypten_ecl, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
hypten_ecl_bmi <- hypten_ecl_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
hypten_ecl_bmi <- hypten_ecl_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Delivery Weight')
hypten_ecl_bmi <- hypten_ecl_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hypten_ecl_bmi

prev_preterm_birth_bmi <- plot_ly(gestation_history18, x = ~prev_preterm_birth, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
prev_preterm_birth_bmi <- prev_preterm_birth_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
prev_preterm_birth_bmi <- prev_preterm_birth_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Delivery Weight')
prev_preterm_birth_bmi <- prev_preterm_birth_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
prev_preterm_birth_bmi



#Grouped barplot 

#gestation and fertil enhancments made to the mother in relation to mothers bmi and pre/post birth weight, part B. (averaged)

infertility_history18= nat18 %>% 
  group_by(infertility_treatment, fertil_enhance, asst_repro_tech) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))


infertility_treatment_bmi <- plot_ly(infertility_history18, x = ~infertility_treatment, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
infertility_treatment_bmi <- infertility_treatment_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
infertility_treatment_bmi <- infertility_treatment_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
infertility_treatment_bmi <- infertility_treatment_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
infertility_treatment_bmi

fertil_enhance_bmi <- plot_ly(infertility_history18, x = ~fertil_enhance, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
fertil_enhance_bmi <- fertil_enhance_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
fertil_enhance_bmi <- fertil_enhance_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
fertil_enhance_bmi <- fertil_enhance_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fertil_enhance_bmi

asst_repro_tech_bmi <- plot_ly(infertility_history18, x = ~asst_repro_tech, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
asst_repro_tech_bmi <- asst_repro_tech_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
asst_repro_tech_bmi <- asst_repro_tech_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Delivery Weight')
asst_repro_tech_bmi <- asst_repro_tech_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
asst_repro_tech_bmi



###############

#Grouped barplot 

#Infections listed during pregnancy in relation to the parents age and education. (averaged)

infections_education= nat18 %>% 
  group_by(gonorrhea, syphilis, chlamydia, hepB, hepC) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Infections listed during pregnancy in relation to the mothers bmi, and pre/post pregnancy. (averaged)


#Grouped barplot 

#Education

gonorrhea_ed <- plot_ly(infections_education, x = ~gonorrhea, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
gonorrhea_ed <- gonorrhea_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
gonorrhea_ed <- gonorrhea_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gonorrhea_ed

syphilis_ed <- plot_ly(infections_education, x = ~syphilis, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
syphilis_ed <- syphilis_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
syphilis_ed <- syphilis_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
syphilis_ed

chlamydia_ed <- plot_ly(infections_education, x = ~chlamydia, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
chlamydia_ed <- chlamydia_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
chlamydia_ed <- chlamydia_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
chlamydia_ed

hepB_ed <- plot_ly(infections_education, x = ~hepB, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
hepB_ed <- hepB_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
hepB_ed <- hepB_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hepB_ed

hepC_ed <- plot_ly(infections_education, x = ~hepC, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
hepC_ed <- hepC_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
hepC_ed <- hepC_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hepC_ed

#Age

gonorrhea_age <- plot_ly(infections_education, x = ~gonorrhea, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
gonorrhea_age <- gonorrhea_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
gonorrhea_age <- gonorrhea_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gonorrhea_age

syphilis_age <- plot_ly(infections_education, x = ~syphilis, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
syphilis_age <- syphilis_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
syphilis_age <- syphilis_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
syphilis_age

chlamydia_age <- plot_ly(infections_education, x = ~chlamydia, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
chlamydia_age <- chlamydia_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
chlamydia_age <- chlamydia_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
chlamydia_age

hepB_age <- plot_ly(infections_education, x = ~hepB, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
hepB_age <- hepB_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
hepB_age <- hepB_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hepB_age

hepC_age <- plot_ly(infections_education, x = ~hepC, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
hepC_age <- hepC_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
hepC_age <- hepC_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hepC_age


#####

infections_history18= nat18 %>% 
  group_by(gonorrhea, syphilis, chlamydia, hepB, hepC) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))


gonorrhea_bmi <- plot_ly(infections_history18, x = ~gonorrhea, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
gonorrhea_bmi <- gonorrhea_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
gonorrhea_bmi <- gonorrhea_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
gonorrhea_bmi <- gonorrhea_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
gonorrhea_bmi

syphilis_bmi <- plot_ly(infections_history18, x = ~syphilis, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
syphilis_bmi <- syphilis_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
syphilis_bmi <- syphilis_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
syphilis_bmi <- syphilis_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
syphilis_bmi

chlamydia_bmi <- plot_ly(infections_history18, x = ~chlamydia, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
chlamydia_bmi <- chlamydia_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
chlamydia_bmi <- chlamydia_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Delivery Weight')
chlamydia_bmi <- chlamydia_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
chlamydia_bmi


hepB_bmi <- plot_ly(infections_history18, x = ~hepB, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
hepB_bmi <- hepB_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
hepB_bmi <- hepB_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
hepB_bmi <- hepB_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hepB_bmi

hepC_bmi <- plot_ly(infections_history18, x = ~hepC, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
hepC_bmi <- hepC_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
hepC_bmi <- hepC_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
hepC_bmi <- hepC_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hepC_bmi




##################3

#Average level of infections reported on a regular basis in relation to the parents age and education.

#Grouped barplot 

no_infection_education18= nat18 %>% 
  group_by(no_infection_reported) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

no_inf_edu18= read.csv('./no_infection_education18.csv')


View(no_infection_education18)

#Average level of infections reported on a regular basis in relation to the mothers bmi and pre/post preganancy weight.

#Grouped barplot 

no_infection_reported_ed <- plot_ly(no_inf_edu18, x = ~no_infection_reported, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
no_infection_reported_ed <- no_infection_reported_ed %>% add_trace(y = ~fathers_age_average, name = 'Fathers Education')
no_infection_reported_ed <- no_infection_reported_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
no_infection_reported_ed

no_infection_reported_age <- plot_ly(no_infection_education18, x = ~no_infection_reported, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
no_infection_reported_age <- no_infection_reported_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
no_infection_reported_age <- no_infection_reported_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
no_infection_reported_age

#Adjust these columns 



no_infection_history18= nat18 %>% 
  group_by(no_infection_reported) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))


no_inf_age18= read.csv('./no_infection_history18.csv')


View(no_infection_history18)


no_infection_bmi <- plot_ly(no_inf_age18, x = ~no_infection_reported, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
no_infection_bmi <- no_infection_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
no_infection_bmi <- no_infection_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
no_infection_bmi <- no_infection_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
no_infection_bmi


##################3

#Reported medication used during infants birth in relation to the average parents education and age.


#Grouped barplot 

drug_education18= nat18 %>% 
  group_by(success_ext_cep, fail_ext_cep, induced_labor, aug_labor,
                       steriods, antibiotics, chorioamnionitis, anesthesia) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Reported medication used during infants birth in relation to the mothers average bmi, pre preganancy and delivery weight.

#Grouped barplot 


success_ed <- plot_ly(drug_education18, x = ~success_ext_cep, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
success_ed <- success_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
success_ed <- success_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
success_ed

fail_ed <- plot_ly(drug_education18, x = ~fail_ext_cep, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
fail_ed <- fail_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
fail_ed <- fail_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fail_ed

induced_ed <- plot_ly(drug_education18, x = ~induced_labor, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
induced_ed <- induced_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
induced_ed <- induced_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
induced_ed

aug_ed <- plot_ly(drug_education18, x = ~aug_labor, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
aug_ed <- aug_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
aug_ed <- aug_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
aug_ed

steroids_ed <- plot_ly(drug_education18, x = ~steriods, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
steroids_ed <- steroids_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
steroids_ed <- steroids_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
steroids_ed

antibiotics_ed <- plot_ly(drug_education18, x = ~antibiotics, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
antibiotics_ed <- antibiotics_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
antibiotics_ed <- antibiotics_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
antibiotics_ed

chorioamnionitis_ed <- plot_ly(drug_education18, x = ~chorioamnionitis, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
chorioamnionitis_ed <- chorioamnionitis_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
chorioamnionitis_ed <- chorioamnionitis_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
chorioamnionitis_ed

anesthesia_ed <- plot_ly(drug_education18, x = ~anesthesia, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
anesthesia_ed <- anesthesia_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
anesthesia_ed <- anesthesia_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
anesthesia_ed


#Age

success_age <- plot_ly(drug_education18, x = ~success_ext_cep, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
success_age <- success_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
success_age <- success_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
success_age

fail_age <- plot_ly(drug_education18, x = ~fail_ext_cep, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
fail_age <- fail_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
fail_age <- fail_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fail_age

induced_age <- plot_ly(drug_education18, x = ~induced_labor, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
induced_age <- induced_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
induced_age <- induced_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
induced_age

aug_age <- plot_ly(drug_education18, x = ~aug_labor, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
aug_age <- aug_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
aug_age <- aug_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
aug_age

steroids_age <- plot_ly(drug_education18, x = ~steriods, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
steroids_age <- steroids_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
steroids_age <- steroids_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
steroids_age

antibiotics_age <- plot_ly(drug_education18, x = ~antibiotics, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
antibiotics_age <- antibiotics_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
antibiotics_age <- antibiotics_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
antibiotics_age

chorioamnionitis_age <- plot_ly(drug_education18, x = ~chorioamnionitis, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
chorioamnionitis_age <- chorioamnionitis_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
chorioamnionitis_age <- chorioamnionitis_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
chorioamnionitis_age

anesthesia_age <- plot_ly(drug_education18, x = ~anesthesia, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
anesthesia_age <- anesthesia_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
anesthesia_age <- anesthesia_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
anesthesia_age




drug_history18= nat18 %>% 
  group_by(success_ext_cep, fail_ext_cep, induced_labor, aug_labor,
           steriods, antibiotics, chorioamnionitis, anesthesia) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))


success_bmi <- plot_ly(drug_history18, x = ~success_ext_cep, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
success_bmi <- success_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
success_bmi <- success_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
success_bmi <- success_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
success_bmi

fail_bmi <- plot_ly(drug_history18, x = ~fail_ext_cep, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
fail_bmi <- fail_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
fail_bmi <- fail_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
fail_bmi <- fail_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fail_bmi

induced_bmi <- plot_ly(drug_history18, x = ~induced_labor, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
induced_bmi <- induced_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
induced_bmi <- induced_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
induced_bmi <- induced_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
induced_bmi

aug_bmi <- plot_ly(drug_history18, x = ~aug_labor, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
aug_bmi <- aug_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
aug_bmi <- aug_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
aug_bmi <- aug_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
aug_bmi

steroids_bmi <- plot_ly(drug_history18, x = ~steriods, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
steroids_bmi <- steroids_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
steroids_bmi <- steroids_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
steroids_bmi <- steroids_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
steroids_bmi

antibiotics_bmi <- plot_ly(drug_history18, x = ~antibiotics, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
antibiotics_bmi <- antibiotics_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
antibiotics_bmi <- antibiotics_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
antibiotics_bmi <- antibiotics_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
antibiotics_bmi

chorioamnionitis_bmi <- plot_ly(drug_history18, x = ~chorioamnionitis, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
chorioamnionitis_bmi <- chorioamnionitis_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
chorioamnionitis_bmi <- chorioamnionitis_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
chorioamnionitis_bmi <- chorioamnionitis_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
chorioamnionitis_bmi

anesthesia_bmi <- plot_ly(drug_history18, x = ~anesthesia, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
anesthesia_bmi <- anesthesia_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
anesthesia_bmi <- anesthesia_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
anesthesia_bmi <- anesthesia_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
anesthesia_bmi



##################3

#Type of fetal present at birth in relation to the average age, education of the parents; bmi, and weight of the mother.

#Grouped barplot 


fetal_history18= nat18 %>% 
  group_by(fetal_present_at_birth) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))

fetal_bmi18= read.csv('./fetal_history18.csv')


#Type of fetal present at birth in relation to the average age, education of the parents; bmi, and weight of the mother.

#Grouped barplot 

View(fetal_history18)

#Adjust this column 


fetal_present_bmi <- plot_ly(fetal_bmi18, x = ~fetal_present_at_birth, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
fetal_present_bmi <- fetal_present_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
fetal_present_bmi <- fetal_present_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
fetal_present_bmi <- fetal_present_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fetal_present_bmi



fetal_education18= nat18 %>% 
  group_by(fetal_present_at_birth) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

fetal_edu18= read.csv('./fetal_education18.csv')

#Education

fetal_present_ed <- plot_ly(fetal_edu18, x = ~fetal_present_at_birth, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
fetal_present_ed <- fetal_present_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
fetal_present_ed <- fetal_present_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fetal_present_ed

#Age

fetal_present_age <- plot_ly(fetal_edu18, x = ~fetal_present_at_birth, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
fetal_present_age <- fetal_present_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
fetal_present_age <- fetal_present_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fetal_present_age

#################3


#Reported delivery method used at birth in relation to the average age, education of the parents; bmi, and weight of the mother.

#Grouped barplot 

final_del_education18= nat18 %>% 
  group_by(final_delivery_method) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

final_del_edu18= read.csv('./final_del_education18.csv')



View(final_del_education18)

#Grouped barplot 


#Adjust these plots



#Education

final_del_ed <- plot_ly(final_del_edu18, x = ~final_delivery_method, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
final_del_ed <- final_del_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
final_del_ed <- final_del_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
final_del_ed



#Age

final_del_age <- plot_ly(final_del_edu18, x = ~final_delivery_method, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
final_del_age <- final_del_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
final_del_age <- final_del_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
final_del_age



final_delivery_bmi18= nat18 %>% 
  group_by(final_delivery_method) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))

final_del_bmi18= read.csv('./final_delivery_bmi18.csv')



final_del_bmi <- plot_ly(final_del_bmi18, x = ~final_delivery_method, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
final_del_bmi <- final_del_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
final_del_bmi <- final_del_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
final_del_bmi <- final_del_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
final_del_bmi




##################

#Number of prenatal visits made in relation to the parents average education and age.

#Grouped barplot 

prenatal_education18= nat18 %>% 
  group_by(n_prenatal_visits) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

View(prenatal_education18)

#Number of prenatal visits made in relation to the mothers bmi, pre pregnancy weight and delivery weight.

#Grouped barplot 

#Education

prenatal_ed_mother= ggplot(prenatal_education18, aes(x=n_prenatal_visits, y=mothers_education_average, group=1)) +
  geom_line()+
  geom_point()

prenatal_ed_mother

prenatal_ed_father= ggplot(prenatal_education18, aes(x=n_prenatal_visits, y=fathers_education_average, group=1)) +
  geom_line()+
  geom_point()


#Age

prenatal_age_mother= ggplot(prenatal_education18, aes(x=n_prenatal_visits, y=mothers_age_average, group=1)) +
  geom_line()+
  geom_point()

prenatal_age_mother

prenatal_age_father= ggplot(prenatal_education18, aes(x=n_prenatal_visits, y=fathers_age_average, group=1)) +
  geom_line()+
  geom_point()

prenatal_age_father

#Line Graph



prenatal_history18= nat18 %>% 
  group_by(n_prenatal_visits) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))

View(prenatal_history18)


prenatal_bmi= ggplot(prenatal_history18, aes(x=n_prenatal_visits, y=mothers_bmi_average, group=1)) +
  geom_line()+
  geom_point()

prenatal_bmi

prenatal_pre_preg= ggplot(prenatal_history18, aes(x=n_prenatal_visits, y=pre_preg_lbs_average, group=1)) +
  geom_line()+
  geom_point()

prenatal_ed_mother

prenatal_delivery= ggplot(prenatal_history18, aes(x=n_prenatal_visits, y=delivery_lbs_average, group=1)) +
  geom_line()+
  geom_point()

prenatal_delivery


#Line Graph



######################

#Paternity acknowlegment in realtion to the parents age and education. (mean)

#Grouped barplot 

paternity_education18= nat18 %>% 
  group_by(paternity_acknow) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Paternity acknowlegment in realtion to the mothers bmi, pre pregnancy weight and delivery weight. (mean)

#Grouped barplot 


#Education

pat_ed <- plot_ly(paternity_education18, x = ~paternity_acknow, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
pat_ed <- pat_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
pat_ed <- pat_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pat_ed

#Age

pat_age <- plot_ly(paternity_education18, x = ~paternity_acknow, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
pat_age <- pat_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
pat_age <- pat_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pat_age



paternity_history18= nat18 %>% 
  group_by(paternity_acknow) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))


pat_bmi <- plot_ly(paternity_history18, x = ~paternity_acknow, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
pat_bmi <- pat_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
pat_bmi <- pat_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
pat_bmi <- pat_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
pat_bmi


########################

#Mothers reported hispanic origin in relation to their average age and education, as well as the fathers.

#Grouped barplot 

mhispanic_education18= nat18 %>% 
  group_by(mothers_hispanic_origin2) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Mothers reported hispanic origin in relation to their average bmi, pre pregnancy weight and delivery weight.

#Grouped barplot 

#Education

hisp_ed <- plot_ly(mhispanic_education18, x = ~mothers_hispanic_origin2, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
hisp_ed <- hisp_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
hisp_ed <- hisp_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hisp_ed

#Age

hisp_age <- plot_ly(mhispanic_education18, x = ~mothers_hispanic_origin2, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
hisp_age <- hisp_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
hisp_age <- hisp_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hisp_age




mhispanic_history18= nat18 %>% 
  group_by(mothers_hispanic_origin2) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))


hisp_bmi <- plot_ly(mhispanic_history18, x = ~mothers_hispanic_origin2, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
hisp_bmi <- hisp_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
hisp_bmi <- hisp_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
hisp_bmi <- hisp_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
hisp_bmi




#######################

#Fathers reported hispanic origin in relation to their average age and education, as well as the fathers.

#Grouped barplot 

fhispanic_education18= nat18 %>% 
  group_by(fathers_hispanic_origin2) %>% 
  summarise_at(vars(fathers_education, mothers_education, mothers_age, fathers_age),
               list(average = mean))

#Fathers reported hispanic origin in relation to their average bmi, pre pregnancy weight and delivery weight.

#Grouped barplot 

#Education

fhisp_ed <- plot_ly(fhispanic_education18, x = ~fathers_hispanic_origin2, y = ~mothers_education_average, type = 'bar', name = 'Mothers Education')
fhisp_ed <- fhisp_ed %>% add_trace(y = ~fathers_education_average, name = 'Fathers Education')
fhisp_ed <- fhisp_ed %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fhisp_ed

#Age

fhisp_age <- plot_ly(fhispanic_education18, x = ~fathers_hispanic_origin2, y = ~mothers_age_average, type = 'bar', name = 'Mothers Age')
fhisp_age <- fhisp_age %>% add_trace(y = ~fathers_age_average, name = 'Fathers Age')
fhisp_age <- fhisp_age %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fhisp_age




fhispanic_history18= nat18 %>% 
  group_by(fathers_hispanic_origin2) %>% 
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))

fhisp_bmi <- plot_ly(fhispanic_history18, x = ~fathers_hispanic_origin2, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
fhisp_bmi <- fhisp_bmi %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
fhisp_bmi <- fhisp_bmi %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
fhisp_bmi <- fhisp_bmi %>% layout(yaxis = list(title = 'Average'), barmode = 'group')
fhisp_bmi





####################

#Under father, the average level of infants breatfed at discharge, these numbers were transformed to prove an 
#interesting point with the correlation between parent af the breast feeding occurring.

#Line plot


#FATHER BREAST FED

View(father_breast)

father_b= ggplot(father_breast, aes(x=fathers_education, y=infant_breastfed_at_discharge, group=1)) +
  geom_line()+
  geom_point()
father_b

#Under mother, the average level of infants breatfed at discharge, these numbers were transformed to prove an 
#interesting point with the correlation between parent af the breast feeding occurring.


#Line plot

View(mother_breast)

mother_b= ggplot(mother_breast, aes(x=mothers_education, y=infant_breastfed_at_discharge, group=1)) +
  geom_line()+
  geom_point()

mother_b




#####################

#Mothers use of tobacco in all 3 trimesters on average.


mtobacco_education= nat18 %>% 
  group_by(mothers_education) %>% 
  summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
               list(average = mean))

#Mothers use of tobacco in all 3 trimesters (sum)


mtobacco_ed1= ggplot(mtobacco_education, aes(x=mothers_education, y=cigs_tri1_average, group=1)) +
  geom_line()+
  geom_point()

mtobacco_ed1

mtobacco_ed2= ggplot(mtobacco_education, aes(x=mothers_education, y=cigs_tri2_average, group=1)) +
  geom_line()+
  geom_point()

mtobacco_ed2

mtobacco_ed3= ggplot(mtobacco_education, aes(x=mothers_education, y=cigs_tri3_average, group=1)) +
  geom_line()+
  geom_point()

mtobacco_ed3


##################

mtobacco_age= nat18 %>% 
  group_by(mothers_age) %>% 
  summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
               list(average = mean))


#Mothers use of tobacco in all 3 trimesters on average.


mtobacco_age1= ggplot(mtobacco_age, aes(x=mothers_age, y=cigs_tri2_average, group=1)) +
  geom_line()+
  geom_point()

mtobacco_age1

mtobacco_ed2= ggplot(mtobacco_age, aes(x=mothers_age, y=cigs_tri2_average, group=1)) +
  geom_line()+
  geom_point()

mtobacco_age2

mtobacco_age3= ggplot(mtobacco_age, aes(x=mothers_age, y=cigs_tri3_average, group=1)) +
  geom_line()+
  geom_point()

mtobacco_age3



ftobacco_education= nat18 %>% 
  group_by(fathers_education) %>% 
  summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
               list(average = mean))


#Fathers use of tobacco in all 3 trimesters (sum)

ftobacco_ed1= ggplot(ftobacco_education, aes(x=fathers_education, y=cigs_tri1_average, group=1)) +
  geom_line()+
  geom_point()

ftobacco_ed1

ftobacco_ed2= ggplot(ftobacco_education, aes(x=fathers_education, y=cigs_tri2_average, group=1)) +
  geom_line()+
  geom_point()

ftobacco_ed2

ftobacco_ed3= ggplot(ftobacco_education, aes(x=fathers_education, y=cigs_tri3_average, group=1)) +
  geom_line()+
  geom_point()

ftobacco_ed3



ftobacco_age= nat18 %>% 
  group_by(fathers_age) %>% 
  summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
               list(average = mean))


ftobacco_age1= ggplot(ftobacco_age, aes(x=fathers_age, y=cigs_tri1_average, group=1)) +
  geom_line()+
  geom_point()

ftobacco_age1

ftobacco_age2= ggplot(ftobacco_age, aes(x=fathers_age, y=cigs_tri2_average, group=1)) +
  geom_line()+
  geom_point()

ftobacco_age2

ftobacco_age3= ggplot(ftobacco_age, aes(x=fathers_age, y=cigs_tri3_average, group=1)) +
  geom_line()+
  geom_point()

ftobacco_age3



#######################

#Average parental education, births and APGAR score shown in every state depending on whether or not WIC was used.


#A US map to show the difference in different regions 


WIC <- read.csv('./WIC_cleaned.csv')

WIC_table= WIC %>% 
  group_by(State_of_Residence, WIC) %>% 
  summarise_at(vars(Mothers_Education_Code, Fathers_Education_Code, Births, Ten_Minute_APGAR_Score),
               list(average = mean))


View(WIC)

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






















































































































