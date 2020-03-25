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
glimpse(nat2018)

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





p1 = function() {natall %>%
    group_by(infant_living_at_report,infant_breastfed_at_discharge,last_norm_menses_mo) %>%
    summarise_at(vars(mothers_bmi,mothers_age,mothers_race,mothers_education,fathers_education),
                 list(average = mean))


}
ouput$living_bmi <- renderPlotly(
  plot_ly(p1(), x = ~infant_living_at_report, y = ~mothers_bmi_average, type = 'box', name = "mothers_bmi_average") %>%
    layout(title = "",
           barmode = 'group',
           xaxis = list(title = "Infant Living at Report"),
           yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
)

p2 = function() {natall %>%
    group_by(infant_living_at_report,infant_breastfed_at_discharge,last_norm_menses_mo) %>%
    summarise_at(vars(mothers_bmi,mothers_age,mothers_race,mothers_education,fathers_education),
                 list(average = mean))


}
ouput$breastfed_edu <- renderPlotly(
  plot_ly(p2(), x = ~infant_breastfed_at_discharge, y = ~mothers_education_average, type = 'box', name = "mothers_education_average") %>%
    layout(title = "",
           barmode = 'group',
           xaxis = list(title = "Infant Breasfed at Discharge"),
           yaxis = list(title = "Average Mother's Education"))
)

p3 = function() {natall %>%
    group_by(admit_NICU,surfactant,antibiotics_for_newborn,seizures,assist_vent_immed,assist_vent_after6) %>%
    summarise_at(vars(mothers_bmi,mothers_age),
                 list(average = mean))


}
ouput$breastfed_edu <- renderPlotly(
  plot_ly(p3(), x = ~admit_NICU, y = ~mothers_bmi_average, type = 'box', name = "mothers_bmi_average") %>%
    layout(title = "",
           barmode = 'group',
           xaxis = list(title = "Mother Admitted into NICU"),
           yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
)

p4 = function() {natall %>%
    group_by(combined_gestation_wk) %>%
    summarise_at(vars(mothers_bmi),
                 list(average = mean))


}
ouput$gestation_bmi <- renderPlotly(
  plot_ly(p4(), x = ~combined_gestation_wk, y = ~average, type = 'scatter', name = "last Normal Menstreul Cycle") %>%
    layout(title = "",
           barmode = 'group',
           xaxis = list(title = "Gestation Period (weeks)"),
           yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
)

p5 = function() {natall %>%
    group_by(APGAR_score_5min,APGAR_score_10min,mothers_marital_status) %>%
    summarise_at(vars(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                      mothers_hispanic_origin2,fathers_race,fathers_hispanic_origin2,n_prenatal_visits,
                      mothers_bmi,last_norm_menses_mo,combined_gestation_wk,birth_weight_gm),
                 list(average = mean))


}
ouput$apgar5_bmi <- renderPlotly(
  plot_ly(p5(), x = ~APGAR_score_5min, y = ~mothers_bmi_average, type = 'box', name = "APGAR 5") %>%
    layout(title = "",
           barmode = 'group',
           xaxis = list(title = "APGAR 5 Min Scores"),
           yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))

)



p6 = function() {natall %>%
    group_by(APGAR_score_5min,APGAR_score_10min,mothers_marital_status) %>%
    summarise_at(vars(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                      mothers_hispanic_origin2,fathers_race,fathers_hispanic_origin2,n_prenatal_visits,
                      mothers_bmi,last_norm_menses_mo,combined_gestation_wk,birth_weight_gm),
                 list(average = mean))


}
ouput$apgar10_bmi <- renderPlotly(
  apgar10_bmi <- plot_ly(p6(), x = ~APGAR_score_10min, y = ~mothers_bmi_average, type = 'box', name = "APGAR 10") %>%
    layout(title = "",
           barmode = 'group',
           xaxis = list(title = "APGAR 10 Min Scores"),
           yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))

)

##################### JASONS #######################################

final_delivery_bmi18= nat18 %>%
  group_by(final_delivery_method) %>%
  summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
               list(average = mean))



prenatal_age_father18= nat18 %>%
  group_by(fathers_age) %>%
  summarise_at(vars(n_prenatal_visits, mothers_education, mothers_age, fathers_education),
               list(average = mean))


prenatal_age_mother18= nat18 %>%
  group_by(mothers_age) %>%
  summarise_at(vars(n_prenatal_visits, mothers_education, fathers_education, fathers_age),
               list(average = mean))



p7 = function() {nat18 %>%
    group_by(final_delivery_method) %>%
    summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
                 list(average = mean))

  # final_del_bmi18= read.csv('./final_delivery_bmi18.csv')


}
ouput$final_del_bmi <- renderPlotly(
  plot_ly(p7(), x = ~final_delivery_method, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI') %>%
    add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight') %>%
    add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight') %>%
    layout(title = "Final Delivery Method in relation to the average BMI/Weight of the mother",
           barmode = 'group',
           xaxis = list(title = "Final Delivery Options"),
           yaxis = list(title = "Average BMI/Weight"))
)




p8 = function() {natall %>%
    group_by(mothers_age) %>%
    summarise_at(vars(n_prenatal_visits, mothers_education, fathers_education, fathers_age),
                 list(average = mean))


}
ouput$prenatal_age_mother <- renderPlot(
  ggplot(p8(), aes(x=mothers_age, y=n_prenatal_visits_average, group=1)) +
    geom_line(linetype="dashed", color="blue", size=0.2)+
    labs(title ="Average Prenetal Visits in Relation to the Age of the Mother")+
    xlab("Mothers Age")+
    ylab("Average Prenatal Visits")+
    geom_point()
)

p9 = function() {natall %>%
    group_by(fathers_age) %>%
    summarise_at(vars(n_prenatal_visits, mothers_education, mothers_age, fathers_education),
                 list(average = mean))


}
ouput$prenatal_age_father <- renderPlot(
  ggplot(p9(), aes(x=fathers_age, y=n_prenatal_visits_average, group=1)) +
    geom_line(linetype="dashed", color="blue", size=0.2)+
    labs(title ="Average Prenetal Visits in Relation to the Age of the Father")+
    xlab("Fathers Age")+
    ylab("Average Prenatal Visits")+
    geom_point()
)

}










