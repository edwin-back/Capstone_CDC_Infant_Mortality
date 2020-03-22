library(shiny)
library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)



del_ed <- del_ed %>% layout(title = "Delivery Payment Option Based on Average Parental Education",
                      barmode = 'group',
                      xaxis = list(title = "Delivery Payment Options"),
                      yaxis = list(title = "Average Education Level"))

del_ed


del_age <- del_age %>% layout(title = "Delivery Payment Option Based on Average Parental Age",
       barmode = 'group',
       xaxis = list(title = "Delivery Payment Options"),
       yaxis = list(title = "Average Age"))

del_age



gonorrhea_age <- gonorrhea_age %>% layout(title = "Infection with Gonorrhea in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Report on Gonorrhea; No; Yes; Unknown"),
                              yaxis = list(title = "Average Age"))

gonorrhea_age


syphilis_age <- syphilis_age %>% layout(title = "Infection with Syphilis in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on Syphilis; No; Yes; Unknown"),
                              yaxis = list(title = "Average Age"))

syphilis_age


chlamydia_age <- chlamydia_age %>% layout(title = "Infection with Chlamydia in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on Chlamydia; No; Yes; Unknown"),
                              yaxis = list(title = "Average Age"))
chlamydia_age


hepB_age <- hepB_age %>% layout(title = "Infection with HepB in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on HepB; No; Yes; Unknown"),
                              yaxis = list(title = "Average Age"))

hepB_age



hepC_age <- hepC_age %>% layout(title = "Infection with HepC in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on HepC; No; Yes; Unknown"),
                              yaxis = list(title = "Average Age"))
hepC_age


gonorrhea_bmi <- gonorrhea_bmi %>% layout(title = "Infection with Gonorrhea in realtion to the Average age of the parents",
                                          barmode = 'group',
                                          xaxis = list(title = "Mothers report on Gonorrhea; No; Yes; Unknown"),
                                          yaxis = list(title = "Average BMI/Weight"))

gonorrhea_bmi

syphilis_bmi <- syphilis_bmi %>% layout(title = "Infection with Syphilis in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on Syphilis; No; Yes; Unknown"),
                              yaxis = list(title = "Average BMI/Weight"))
syphilis_bmi


chlamydia_bmi <- chlamydia_bmi %>% layout(title = "Infection with Chlamydia in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on Chlamydia; No; Yes; Unknown"),
                              yaxis = list(title = "Average BMI/Weight"))
chlamydia_bmi


hepB_bmi <- hepB_bmi %>% layout(title = "Infection with HepB in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on HepB; No; Yes; Unknown"),
                              yaxis = list(title = "Average BMI/Weight"))
hepB_bmi


hepC_bmi <- hepC_bmi %>% layout(title = "Infection with HepC in realtion to the Average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on HepC; No; Yes; Unknown"),
                              yaxis = list(title = "Average BMI/Weight"))
hepC_bmi

# no_infection_reported_ed
# 
# no_infection_reported_age
# 
# no_infection_bmi

# success_ed
# 
# fail_ed

# induced_ed

# aug_ed

# steroids_ed

# antibiotics_ed

# chorioamnionitis_ed

# anesthesia_ed

# success_age
 
# fail_age

# induced_age

# aug_age

steroids_age <- steroids_age %>% layout(title = "Use of Steroids medication in relation to parents age",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on using Steroids; No; Yes; Unknown"),
                              yaxis = list(title = "Average Age"))
steroids_age

antibiotics_age <- antibiotics_age %>% layout(title = "Use of Antobiotics medication in relation to parents age",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on using Antibiotics; No; Yes; Unknown"),
                              yaxis = list(title = "Average Age"))
antibiotics_age

# chorioamnionitis_age

anesthesia_age <- anesthesia_age %>% layout(title = "Use of Anesthesia medication in relation to parents age",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on using Anesthesia; No; Yes; Unknown"),
                              yaxis = list(title = "Average Age"))
anesthesia_age
 
# success_bmi
 
# fail_bmi

# induced_bmi

# aug_bmi

steroids_bmi <- steroids_bmi %>% layout(title = "Use of Steroids medication in relation to mothers BMI/Weight",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on using Steroids; No; Yes; Unknown"),
                              yaxis = list(title = "Average BMI/Weight"))
steroids_bmi

antibiotics_bmi <- antibiotics_bmi %>% layout(title = "Use of Antibiotics medication in relation to mothers BMI/Weight",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on using Antibiotics; No; Yes; Unknown"),
                              yaxis = list(title = "Average BMI/Weight"))
antibiotics_bmi

# chorioamnionitis_bmi

anesthesia_bmi <- anesthesia_bmi %>% layout(title = "Use of Anesthesia medication in relation to mothers BMI/Weight",
                              barmode = 'group',
                              xaxis = list(title = "Mothers report on using Anesthesia; No; Yes; Unknown"),
                              yaxis = list(title = "Average BMI/Weight"))

anesthesia_bmi



# final_del_ed

final_del_age <- final_del_age %>% layout(title = "Final Delivery Method in relation to the average age of the parents",
                              barmode = 'group',
                              xaxis = list(title = "Final Delivery Options"),
                              yaxis = list(title = "Average Age"))
final_del_age


final_del_bmi <- final_del_bmi %>% layout(title = "Final Delivery Method in relation to the average BMI/Weight of the mother",
                              barmode = 'group',
                              xaxis = list(title = "Final Delivery Options"),
                              yaxis = list(title = "Average BMI/Weight"))
final_del_bmi

# prenatal_ed_mother

# prenatal_ed_father

prenatal_age_mother= ggplot(prenatal_age_mother18, aes(x=mothers_age, y=n_prenatal_visits_average, group=1)) +
  geom_line(linetype="dashed", color="blue", size=0.2)+
  labs(title ="Average Prenetal Visits in Relation to the Age of the Mother")+
  xlab("Mothers Age")+
  ylab("Average Prenatal Visits")+
  geom_point()

prenatal_age_mother



prenatal_age_father= ggplot(prenatal_age_father18, aes(x=fathers_age, y=n_prenatal_visits_average, group=1)) +
  geom_line(linetype="dashed", color="blue", size=0.2)+
  labs(title ="Average Prenetal Visits in Relation to the Age of the Father")+
  xlab("Fathers Age")+
  ylab("Average Prenatal Visits")+
  geom_point()


prenatal_age_father

# prenatal_bmi #has outliers
# 
# prenatal_pre_preg #has outlier 
# 
# prenatal_delivery #has outlier

# pat_ed

pat_age <- pat_age %>% layout(title = "Paternity Acknowledgment in relation to the average of the parents age",
                              barmode = 'group',
                              xaxis = list(title = "Paternity Acknowledgment; No, Yes, Unknown, Not Reported"),
                              yaxis = list(title = "Average Age"))
pat_age

pat_bmi <- pat_bmi %>% layout(title = "Paternity Acknowledgment in relation to the average BMI/Weight of the Mother",
                              barmode = 'group',
                              xaxis = list(title = "Paternity Acknowledgment; No, Yes, Unknown, Not Reported"),
                              yaxis = list(title = "Average BMI/Weight"))
pat_bmi



# hisp_ed

hisp_age <- hisp_age %>% layout(title = "Hispanic Origin in relation to the Average age of the Parents",
                              barmode = 'group',
                              xaxis = list(title = "Hispanic Origin"),
                              yaxis = list(title = "Average Age"))
hisp_age



hisp_bmi <- hisp_bmi %>% layout(title = "Hispanic Origin in relation to the Average BMI/Weight of the Mother",
                              barmode = 'group',
                              xaxis = list(title = "Hispanic Origin"),
                              yaxis = list(title = "Average BMI/Weight"))
hisp_bmi

# fhisp_ed

# fhisp_age

# fhisp_bmi

# mtobacco_ed1
# 
# mtobacco_ed2
# 
# mtobacco_ed3



mtobacco_age1= ggplot(mtobacco_age, aes(x=mothers_age, y=cigs_tri1_average, group=1)) +
  geom_line(linetype="dashed", color="blue", size=0.2)+
  labs(title ="Average Tobacco Use in the 1st Trimester in Relation to the Age of the Mother")+
  xlab("Mothers Age")+
  ylab("Average Tobacco Use in Trimester 1")+
  geom_point()

mtobacco_age1


mtobacco_age2= ggplot(mtobacco_age, aes(x=mothers_age, y=cigs_tri2_average, group=1)) +
  geom_line(linetype="dashed", color="blue", size=0.2)+
  labs(title ="Average Tobacco Use in the 2nd Trimester in Relation to the Age of the Mother")+
  xlab("Mothers Age")+
  ylab("Average Tobacco Use in Trimester 2")+
  geom_point()

mtobacco_age2


mtobacco_age3= ggplot(mtobacco_age, aes(x=mothers_age, y=cigs_tri3_average, group=1)) +
  geom_line(linetype="dashed", color="blue", size=0.2)+
  labs(title ="Average Tobacco Use in the 3rd Trimester in Relation to the Age of the Mother")+
  xlab("Mothers Age")+
  ylab("Average Tobacco Use in Trimester 3")+
  geom_point()

mtobacco_age3

# ftobacco_ed1
# 
# ftobacco_ed2
# 
# ftobacco_ed3

ftobacco_age1= ggplot(ftobacco_age, aes(x=fathers_age, y=cigs_tri1_average, group=1)) +
  geom_line(linetype="dashed", color="blue", size=0.2)+
  labs(title ="Average Tobacco Use in the 1st Trimester in Relation to the Age of the father")+
  xlab("Fathers Age")+
  ylab("Average Tobacco Use in Trimester 1")+
  geom_point()

ftobacco_age1


ftobacco_age2= ggplot(ftobacco_age, aes(x=fathers_age, y=cigs_tri2_average, group=1)) +
  geom_line(linetype="dashed", color="blue", size=0.2)+
  labs(title ="Average Tobacco Use in the 2nd Trimester in Relation to the Age of the father")+
  xlab("Fathers Age")+
  ylab("Average Tobacco Use in Trimester 2")+
  geom_point()

ftobacco_age2


ftobacco_age3= ggplot(ftobacco_age, aes(x=fathers_age, y=cigs_tri3_average, group=1)) +
  geom_line(linetype="dashed", color="blue", size=0.2)+
  labs(title ="Average Tobacco Use in the 3rd Trimester in Relation to the Age of the father")+
  xlab("Fathers Age")+
  ylab("Average Tobacco Use in Trimester 3")+
  geom_point()

ftobacco_age3









