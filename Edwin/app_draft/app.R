# Load packages#######################################################################
######################################################################################
######################################################################################
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(stats)
library(ggplot2)
library(plotly)
library(bootstraplib)
library(rsconnect)

#############################             IMPORT OF RAW DATA    ######################
######################################################################################
######################################################################################

# Load data
# df_mort = read.csv('mort17_dead.csv') # dead infants; 5570 rows (a quarter of 22,280)
# df_mort = df_mort %>% select(everything(), -X)
# df_live = read.csv('mort17_live.csv') # live infants; 966,196 rows (a quarter of 3.8 million)
# df_live = df_live %>% select(everything(), -X)
# 
# natall_small = read.csv('natall_small.csv', stringsAsFactors = F)
# final_del_bmi18 = read.csv('final_delivery_bmi18.csv')
# 
# risk_factors = read.csv('health_risks.csv') %>% select(everything(), -X)
# infections = read.csv('health_infections.csv') %>% select(everything(), -X)

#############################                 PREPROCESSING      #####################
######################################################################################
######################################################################################

# Set global app themes, colors, fonts, etc.
bs_theme_new(bootswatch = "minty")
bs_theme_base_colors(bg = "#FDFBFB")
bs_theme_add_variables(
  "spacer" = "1.5rem",
  "font-size-base" = "1.1rem"
)

### Uncomment when deploying the Shiny App
### rsconnect::setAccountInfo(name='mikelim91',
                          #token='E4388F626B940A3844945D6982FC351A',
                          #secret='3El4/BUHlwayvJl/rvgvCyhWxv+DXTYt7jrRsruV')

ui <- shinyUI(fluidPage(bootstrap(),  # minty https://bootswatch.com/
                # Main navbar
                navbarPage(title = "CDC Infant Natality & Mortality Analysis", inverse = TRUE, collapsible=T, icon("plus-square"),
                           tabPanel("Get Started", # Outer Navbar - Get Started
                                   fluidRow(
                                     column(4,
                                            div(class = "jumbotron bg-success",
                                                h1(class="text-center", "Our Mission"), br(),
                                                p(class= "text-justify text-dark", "To open the eyes of expecting mothers about key health & life-style choices they can 
                                                  make, leading up to and during their pregnancy, to potentially reduce the chances of their 
                                                  infant dying prematurely. Whether it is weight, exercise, mental 
                                                  health, or physical strain, we aim to ensure that you receive the best data-driven
                                                  information possible so that your future child is born healthy, happy, 
                                                  and strong."), br(),
                                                tags$blockquote(class = "blockquote text-left", '"Health and wellness is what keeps us selfless."',
                                                                tags$footer(class = "blockquote-footer", "IPAP"))
                                            )
                                     ),
                                     column(4,
                                            div(class = "jumbotron bg-warning",
                                                h1(class="text-center", "Infant Natality"), br(),
                                                p(class="text-justify text-dark", "The CDC provides a wealth of data and information regarding infant natality 
                                                  derived from U.S. birth certificates."),
                                                p(class= "text-justify text-dark", "The CDC Natality online databases report counts of live births occurring within 
                                                  the United States to U.S. citizens and legal residents. Each birth is described by a variety of 
                                                  demographic characteristics, such as state and county of residence, mother's 
                                                  race, and mother's age, and health and medical items, such as tobacco use, 
                                                  method of delivery, and congenital anomalies.")
                                            )
                                     ),
                                     column(4,
                                            div(class = "jumbotron bg-info",
                                                h1(class="text-center", "Infant Mortality"), br(),
                                                p(class= "text-justify text-dark", "The CDC Mortality online databases report counts and rates for deaths of children 
                                                  under 1 year of age, occurring within the United States to U.S. residents. 
                                                  Information from death certificates has been linked to corresponding birth 
                                                  certificates. The mortality data is described by county of mother's residence, 
                                                  child's age, underlying cause of death, gender, birth weight, birth plurality, 
                                                  birth order, gestational age at birth, period of prenatal care, maternal race 
                                                  and ethnicity, maternal age, maternal education and marital status. The data 
                                                  is produced by the National Center for Health Statistics.")
                                            )
                                     )
                           )
                                                
                                                # tabPanel("Deciphering Results", br(),
                                                #          fluidRow(
                                                #            column(4,
                                                #                   div(class = "jumbotron bg-secondary",
                                                #                       h1("Mother's Age Range Decoded"),
                                                #                       hr(),
                                                #                       h5("Age Range Code ", em("1"), ": Under 15 years old"),
                                                #                       h5("Age Range Code ", em("2"), ": 15 to 19 years old"),
                                                #                       h5("Age Range Code ", em("3"), ": 20 to 24 years old"),
                                                #                       h5("Age Range Code ", em("4"), ": 25 to 29 years old"),
                                                #                       h5("Age Range Code ", em("5"), ": 30 to 34 years old"),
                                                #                       h5("Age Range Code ", em("6"), ": 35 to 39 years old"),
                                                #                       h5("Age Range Code ", em("7"), ": 40 to 44 years old"),
                                                #                       h5("Age Range Code ", em("8"), ": 45 to 49 years old"),
                                                #                       h5("Age Range Code ", em("9"), ": 50 to 54 years old")
                                                #                   )
                                                #            ),
                                                #            column(4,
                                                #                   div(class = "jumbotron bg-warning",
                                                #                       h1("Mother's Education Decoded"),
                                                #                       hr(),
                                                #                       h5("Education Code ", em("1"), ": 8th Grade or Less"),
                                                #                       h5("Education Code ", em("2"), ": Some High School - No Diploma"),
                                                #                       h5("Education Code ", em("3"), ": High School Diploma or GED"),
                                                #                       h5("Education Code ", em("4"), ": Some College - No Degree"),
                                                #                       h5("Education Code ", em("5"), ": Associate's"),
                                                #                       h5("Education Code ", em("6"), ": Bachelor's"),
                                                #                       h5("Education Code ", em("7"), ": Master's"),
                                                #                       h5("Education Code ", em("8"), ": Doctorate's")
                                                #                   )
                                                #            ),
                                                #            column(4,
                                                #                   div(class = "jumbotron bg-info",
                                                #                       h1("Mother's Race Decoded"),
                                                #                       hr(),
                                                #                       h5("Race Code ", em("1"), ": White"),
                                                #                       h5("Race Code ", em("2"), ": Black"),
                                                #                       h5("Race Code ", em("3"), ": American Indian Alaskan Native"),
                                                #                       h5("Race Code ", em("4"), ": Asian"),
                                                #                       h5("Race Code ", em("5"), ": Native Hawaiian or Pacific Islander"),
                                                #                       h5("Race Code ", em("6"), ": More Than One Race (Mixed)")
                                                #                   )
                                                #            )
                                                #          )
                                                # )
                           ),
                           # Dropdown Navbar - Explore Data
                           navbarMenu("General Statistics",
                                      tabPanel("Demographics", br(),# Basic Stats navbar
                                               tabsetPanel(type = "pills",
                                                           tabPanel("Age", br(), br(),
                                                                    h3("Distribution of Infant Death Rates Based on Mother's Age"), hr(),
                                                                    plotlyOutput("basic_age_plot")
                                                           ),
                                                           tabPanel("Education", br(), br(),
                                                                    h3("Distribution of Infant Death Rates Based on Mother's Education"), hr(),
                                                                    plotlyOutput("basic_edu_plot")
                                                           ),
                                                           tabPanel("Race", br(), br(),
                                                                    h3("Distribution of Infant Death Rates Based on Mother's Race"), hr(),
                                                                    plotlyOutput("basic_race_plot")
                                                           ),
                                                           tabPanel("Enter Your Info & View Results", br(), br(),
                                                                    # Sidebar panel for inputs: mother's age, mother's education, mother's race
                                                                    sidebarLayout(
                                                                      sidebarPanel(br(),
                                                                        h5(class = "text-center", "Select Options to See Results"), hr(),
                                                                        selectizeInput("mothers_age_Input", label="Current Age Range", choices=c("Select Option",
                                                                                                                                           'Under 15 years old',
                                                                                                                                           '15 to 19 years old',
                                                                                                                                           '20 to 24 years old',
                                                                                                                                           '25 to 29 years old',
                                                                                                                                           '30 to 34 years old',
                                                                                                                                           '35 to 39 years old',
                                                                                                                                           '40 to 44 years old',
                                                                                                                                           '45 to 49 years old',
                                                                                                                                           '50 to 54 years old'), selected=F), br(),
                                                                        selectizeInput("mothers_edu_Input", label="Highest Education", choices=c("Select Option",
                                                                                                                                                 '8th Grade or Less',
                                                                                                                                                 'Some High School - No Diploma',
                                                                                                                                                 'High School Diploma',
                                                                                                                                                 'Some College - No Degree',
                                                                                                                                                 "Associate's",
                                                                                                                                                 "Bachelor's",
                                                                                                                                                 "Master's",
                                                                                                                                                 "Doctorate's"), selected=F), br(),
                                                                        selectizeInput("mothers_race_Input", label="Race", choices=c("Select Option",
                                                                                                                                     'White',
                                                                                                                                     'Black',
                                                                                                                                     'American Indian Alaskan Native',
                                                                                                                                     'Asian',
                                                                                                                                     'Native Hawaiian or Pacific Islander',
                                                                                                                                     'More Than One (Mixed)'), selected=F), br()
                                                                      ),
                                                                      # Main panel for displaying outputs
                                                                      mainPanel(
                                                                        h2("Statistics Based on 2017 CDC Data"), hr(),
                                                                        fluidRow(
                                                                          column(4,
                                                                                 h4("Total Infant Deaths"),
                                                                                 h5("In the U.S."),
                                                                                 valueBoxOutput("deaths_demo_box")
                                                                          ),
                                                                          column(4,
                                                                                 h4("National Infant"),
                                                                                 h5("Death Rate"),
                                                                                 valueBoxOutput("nat_deathrate_demo_box"),
                                                                                 h6("Deaths Per 1,000 Births"),
                                                                          ),
                                                                          column(4,
                                                                                 h4(class= "text-secondary", "Infant Death Rate"),
                                                                                 h5(class= "text-info", "For Your Demographic"),
                                                                                 valueBoxOutput("deathrate_demo_box"),
                                                                                 h6("Deaths Per 1,000 Births"),
                                                                          )
                                                                        ), hr(),
                                                                        fluidRow(
                                                                          column(4,
                                                                                 h4("Total Live Births"),
                                                                                 h5("In the U.S."),
                                                                                 valueBoxOutput("births_demo_box"),
                                                                          ),
                                                                          column(4,
                                                                                 h4("National Infant"),
                                                                                 h5("Birth Rate"),
                                                                                 valueBoxOutput("birthrate_demo_box"),
                                                                                 h6("Births Per 1,000 People")
                                                                          ),
                                                                          column(4,
                                                                                 h4("General"),
                                                                                 h5("Fertility Rate"),
                                                                                 valueBoxOutput("fertrate_demo_box"),
                                                                                 h6("Births Per 1,000 Women"),
                                                                                 h6("Ages 15 to 44 Years Old")
                                                                          )
                                                                        ), hr(),
                                                                      )
                                                                    )
                                                           )
                                               )
                                      ),
                                      # Explore - Health Stats
                                      tabPanel("Physical Health", br(),
                                               tabsetPanel(type="pills",
                                                           tabPanel(
                                                             "Body Mass Index (BMI)", br(), br(),
                                                             h3("Distribution of Infant Death Rates Based on Mother's BMI"), hr(),
                                                             plotlyOutput("health_bmi_plot")
                                                           ),
                                                           tabPanel(
                                                             "Infections Present", br(), br(),
                                                             h3("Infant Death Rates of Mothers with Infections"), hr(),
                                                             plotlyOutput("health_infections_plot")
                                                           ),
                                                           tabPanel(
                                                             "Diagnosed Risk Factors", br(), br(),
                                                             h3("Infant Death Rates of Mothers with Diagnosed Risk Factors"), hr(),
                                                             plotlyOutput("health_risks_plot")
                                                           ),
                                                           tabPanel(
                                                             "Enter Your Info & View Results", br(), br(),
                                                             sidebarLayout(
                                                               sidebarPanel(br(),
                                                                            h5(class = "text-center", "Select Options to See Results"), hr(),
                                                                            selectizeInput("mothers_bmi_Input", label="Estimated BMI Range:", choices=c("Select Option",
                                                                                                                                            'Underweight: < 18.5',
                                                                                                                                            'Normal: 18.5 to 24.9',
                                                                                                                                            'Overweight: 25.0 to 29.9',
                                                                                                                                            'Obese Level 1: 30.0 to 34.9',
                                                                                                                                            'Obese Level 2: 35.0 to 39.9',
                                                                                                                                            'Extremely Obese: 40.0 or more'), selected=F), br(),
                                                                 selectizeInput("mothers_infections_Input", label="Current Infections:", choices=c("Select Option",
                                                                                                                                                   "None",
                                                                                                                                                   "Gonorrhea",
                                                                                                                                                   "Syphillis",
                                                                                                                                                   "Chlamydia",
                                                                                                                                                   "Hepatitis B",
                                                                                                                                                   "Hepatitis C"), selected=F), br(),
                                                                 selectizeInput("mothers_risks_Input", label="Diagnosed Risk Factors:", choices=c("Select Option",
                                                                                                                                                  "None",
                                                                                                                                                  "Pre-pregnancy Diabetes",
                                                                                                                                                  "Gestational Diabetes",
                                                                                                                                                  "Pre-pregnancy Hypertension",
                                                                                                                                                  "Gestational Hypertension",
                                                                                                                                                  "Hypertension Ecalmpsia",
                                                                                                                                                  "Previous Preterm Births"), selected=F)
                                                               ),
                                                               mainPanel(
                                                                 h2("Statistics Based on 2017 CDC Data"), hr(),
                                                                 fluidRow(
                                                                   column(4,
                                                                          h4("Total Infant Deaths"),
                                                                          h5("In the U.S."),
                                                                          valueBoxOutput("deaths_health_box")
                                                                   ),
                                                                   column(4,
                                                                          h4("National Infant"),
                                                                          h5("Death Rate"),
                                                                          valueBoxOutput("nat_deathrate_health_box"),
                                                                          h6("Deaths Per 1,000 Births")
                                                                   ),
                                                                   column(4,
                                                                          h4(class= "text-secondary", "Infant Death Rate"),
                                                                          h5(class= "text-info", "For Your Health"),
                                                                          valueBoxOutput("deathrate_health_box"),
                                                                          h6("Deaths Per 1,000 Births")
                                                                   )
                                                                 ), hr(),
                                                                 fluidRow(
                                                                   column(4,
                                                                          h4("Total Live Births"),
                                                                          h5("In the U.S."),
                                                                          valueBoxOutput("births_health_box")
                                                                   ),
                                                                   column(4,
                                                                          h4("National Infant"),
                                                                          h5("Birth Rate"),
                                                                          valueBoxOutput("birthrate_health_box"),
                                                                          h6("Births Per 1,000 People")
                                                                   ),
                                                                   column(4,
                                                                          h4("Total Fertility"),
                                                                          h5("Rate"),
                                                                          valueBoxOutput("fertrate_health_box"),
                                                                          h6("Births Per 1,000 Women"),
                                                                          h6("Ages 15 to 44 Years Old")
                                                                   )
                                                                 ), hr()
                                                               )
                                                             )
                                                           )
                                               )
                                      ),
                                      tabPanel("Miscellaneous",
                                               tabsetPanel(type = "pills",
                                                           tabPanel(
                                                             "Prenatal Visits", br(),
                                                             h3("Average Prenetal Visits Based on Mother's Age"), br(),
                                                             plotOutput("bmi_prenat_mother"), hr(),
                                                             p("This plot represents the characteristics that are shared between 
                                                               the age of the mother in regards to their consistency in keeping 
                                                               up with prenatal visits for the child."),
                                                             p("There appears to be strong linearity with mothers, with a gradual 
                                                               increase the older they get.  The best number of prenatal visits 
                                                               also appears be between 12 to 14."),
                                                             p("An interesting rise also occurs around the time the mother reaches 
                                                               50, with an average of around 16 prenatal visits occurring then.")
                                                           ),
                                                           tabPanel(
                                                             "NICU Admittance Rate", br(),
                                                             h3("Average BMI of Mothers with Infants Admitted to NICU"), br(),
                                                             plotlyOutput("bmi_nicu")
                                                           )
                                               )
                                      )
                           ),
                           # Improving Natality
                           navbarMenu("Health & Lifestyle",
                                      tabPanel("Weight",
                                               tabsetPanel(type="tabs",
                                                           tabPanel(
                                                             "Premature Development", br(),
                                                             h3("Average BMI of Mothers with Premature Infants"), hr(),
                                                             plotlyOutput("bmi_preme")
                                                           ),
                                                           tabPanel( # convert to columns
                                                             "First 5 Minutes of Life", br(),
                                                             h3("Average BMI of Mothers with Higher 5-Min APGAR Scores"), hr(),
                                                             plotlyOutput("bmi_5min"), br(), br(),
                                                             p("APGAR is short for Appearance, Pulse, Grimace, Activity, and Respiration"),
                                                             p("A 5-minute APGAR score is an overall rating of an infant's health given by the doctor within the first five minutes of being born.")
                                                           ),
                                                           tabPanel(
                                                             "First 10 Minutes of Life", br(),
                                                             h3("Average BMI of Mothers with Higher 10-Min APGAR Scores"), hr(),
                                                             plotlyOutput("bmi_10min"), br(), br(),
                                                             p("APGAR is short for Appearance, Pulse, Grimace, Activity, and Respiration"),
                                                             p("A 10-minute APGAR score is an overall rating of an infant's health given by the doctor within the first ten minutes of being born.")
                                                           ),
                                                           tabPanel(
                                                             "Infant Survival", br(),
                                                             h3("Average BMI of Mothers with Infants Living at Time of Report"), hr(),
                                                             plotlyOutput("bmi_living")
                                                           ),
                                                           tabPanel(
                                                             "Final Delivery Method", br(),
                                                             h3("Final Delivery Method Based on Average BMI of Mother"), hr(),
                                                             plotlyOutput("bmi_final_del"), hr(),
                                                             p("Along the x-axis, the numbers define the delivery procedure that has 
                                                               taken place for the child, they are as followed:"),
                                                             p(class = "text-secondary", "1 = Spontaneous, 2 = Forceps, 3 = Vacuum, 4 = Cesarean, 5 = Unknown or not stated"),
                                                             p("This plot represents the characteristics that are shared between 
                                                               the health of the mother regarding their total BMI during pregnancy, 
                                                               their weight prior to pregnancy, and their weight while going into labor."),
                                                             p("It is interesting to note that not states/unknown is marked as the highest 
                                                               for delivery. It is also interesting to note the consistent trends that exist 
                                                               for the mothers BMI, and weight before/during pregnancy does not change for 
                                                               any of the 5 categories by that much.")
                                                           )
                                               )
                                      ),
                                      tabPanel("Smoking",
                                               tabsetPanel(type="tabs",
                                                           tabPanel(
                                                             "Pre Pregnancy", br(),
                                                             h3("Infant Death Rate of Smoking vs Non-Smoking Mothers"), hr(),
                                                             plotlyOutput("smoke_pre_preg_plot")
                                                           ),
                                                           tabPanel(
                                                             "First Trimester", br(),
                                                             h3("Average Daily Cigarettes Smoked During 1st Trimester"), hr(),
                                                             plotOutput("first_tri_age"), br(),
                                                             plotlyOutput("smoke_death1")
                                                           ),
                                                           tabPanel(
                                                             "Second Trimester", br(),
                                                             h3("Average Daily Cigarettes Smoked During 2nd Trimester"), hr(),
                                                             plotOutput("second_tri_age"), br(),
                                                             plotlyOutput("smoke_death2")
                                                           ),
                                                           tabPanel(
                                                             "Third Trimester", br(),
                                                             h3("Average Daily Cigarettes Smoked During 3rd Trimester"), hr(),
                                                             plotOutput("third_tri_age"), br(),
                                                             plotlyOutput("smoke_death3")
                                                           )
                                               )
                                      )
                           ),
                           tabPanel("Meet The IPAP Team", br(),
                                    fluidRow(
                                      column(3,
                                             div(class="card mb-3",
                                                 div(class="card-header text-white bg-primary", ""),
                                                 div(class="card-body",
                                                     h3(class="card-title", "Edwin Back"),
                                                     a(href="https://www.linkedin.com/in/edwin-back/", "LinkedIn"), br(),
                                                     a(href="https://github.com/edwin-back/", "Github"), br(),
                                                     a(href="https://nycdatascience.com/blog/author/edwin-back/", "Project Blog Posts"), br(),
                                                     a(href="back.edwin@gmail.com", "back.edwin@gmail.com"), br(), hr(),
                                                     h4(class="card-title", "Bio"),
                                                     p(class="card-text", "Edwin Back is a Data Analyst with a strong quantitative foundation and several years of experience in scientific analysis and consulting. His collection of technical skills include Python, R, and SQL with an emphasis in data analysis and visualization techniques. He is an efficient communicator and can articulate complex concepts to a diverse audience.")
                                                 )
                                             )
                                      ),
                                      column(3,
                                             div(class="card mb-3",
                                                 div(class="card-header text-white bg-info", ""),
                                                 div(class="card-body",
                                                     h3(class="card-title", "Jason Hoffmeier"),
                                                     a(href="https://www.linkedin.com/in/jason-hoffmeier/", "LinkedIn"), br(),
                                                     a(href="https://github.com/jhoffme1/", "Github"), br(),
                                                     a(href="https://nycdatascience.com/blog/author/jason-hoffmeier/", "Project Blog Posts"), br(),
                                                     a(href="jhoffmeier1@gmail.com", "jhoffmeier@gmail.com"), br(), hr(),
                                                     h4(class="card-title", "Bio"),
                                                     p(class="card-text", "Jason Hoffmeier is a NYC Data Science fellow that currently resides in New York City. He has a Masters Degree in Systems Engineering from SUNY Binghamton, and has recently earned his Lean Six Sigma Black Belt for quality improvement and statistical analysis also through SUNY Binghamton. He believes Data Science is the way of the future and looks forward to using it for his future work and career.")
                                                 )
                                             )
                                      ),
                                      column(3,
                                             div(class="card mb-3", 
                                                 div(class="card-header text-white bg-danger", ""),
                                                 div(class="card-body",
                                                     h3(class="card-title", "Baptiste Mokas"),
                                                     a(href="https://www.linkedin.com/in/baptiste-mokas-8574ab108/", "LinkedIn"), br(),
                                                     a(href="https://www.github.com/baptiste-mokas/", "Github"), br(),
                                                     a(href="https://www.nycdatascience.com/blog/author/baptiste-mokas/", "Project Blog Posts"), br(),
                                                     a(href="baptiste.mokas@gmail.com", "baptiste.mokas@gmail.com"), br(), hr(),
                                                     h4(class="card-title", "Bio"),
                                                     p(class="card-text", "Baptiste Mokas is a PhD Candidate based in Paris, France with a concentration in Machine Learning Engineering. ")
                                                 )
                                             )
                                      ),
                                      column(3,
                                             div(class="card mb-3",
                                                 div(class="card-header text-white bg-warning", ""),
                                                 div(class="card-body",
                                                     h3(class="card-title", "Michael Lim"),
                                                     a(href="https://www.linkedin.com/in/mikelim91/", "LinkedIn"), br(),
                                                     a(href="https://github.com/mikelim91/", "Github"), br(),
                                                     a(href="https://nycdatascience.com/blog/author/mike-lim/", "Project Blog Posts"), br(),
                                                     a(href="mikelim91@gmail.com", "mikelim91@gmail.com"), br(), hr(),
                                                     h4(class="card-title", "Bio"),
                                                     p(class="card-text", "Mike Lim is a Data Analyst with a background in industrial systems engineering. He graduated from Rutgers University in 2013.")
                                                 )
                                             )
                                      )
                                    )
                           )
                           # # Model Predictions
                           # navbarMenu("Our Predictions",
                           #            tabPanel("Models",
                           #                     tabsetPanel(type="pills",
                           #                                 tabPanel("Model Description #1", br()), 
                           #                                 tabPanel("Model Description #2", br()),
                           #                                 tabPanel("Model Description #3", br())
                           #                     )
                           #            ),
                           #            tabPanel("Predictions",
                           #                     tabsetPanel(type="pills",
                           #                                 tabPanel("Prediction #1", br()),
                           #                                 tabPanel("Prediction #2", br()),
                           #                                 tabPanel("Prediction #3", br())
                           #                     )
                           #                     
                           #            )
                           # ),
                           # navbarMenu("More",
                           #            tabPanel("CDC Source Data",
                           #                     tabsetPanel(type="tabs",
                           #                                 tabPanel("Infant Natality 2016-2018", br()), 
                           #                                 tabPanel("Infant Mortality 2017", br()),
                           #                     )
                           #            )
                           # )
                )
))          

#### SERVER ####                
# Define server function
server <- shinyServer(function(input, output, session) {
  options(shiny.autoreload = TRUE)
  # Basic Stats -> Mother's Age
  basic_age <- function() {
    alive <- df_live %>%
      group_by(mothers_age_recode) %>% 
      summarise(live_count = n())
    
    dead <- df_mort %>% 
      group_by(mothers_age_recode) %>% 
      summarise(death_count = n())
    
    mom_age = c('Under 15 yrs old', '15 to 19 yrs old', '20 to 24 yrs old', '25 to 29 yrs old', '30 to 34 yrs old', '35 to 39 yrs old', '40 to 44 yrs old', '45 to 49 yrs old', '50 to 54 yrs old')
    death_rate = dead$death_count / ((alive$live_count + dead$death_count)/1000) # Infant Mortality Rate per 1000 births
    
    data = data.frame(mom_age, death_rate)
    data$mom_age = factor(data$mom_age, levels = data$mom_age)
    data$death_rate = round(data$death_rate, 1)
    # data$mom_age = factor(data$mom_age, levels=data$mom_age[order(data$death_rate, decreasing = T)])
    data
  }
  
  output$basic_age_plot <- renderPlotly({
    f = list(family = "Verdana", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "Mother's Age Range", titlefont = f, tickfont = t)
    ylab = list(title = "Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)

    fig <- plot_ly(basic_age(), x = ~mom_age, y = ~death_rate, type='bar',
                   text = basic_age()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(128,125,186)", line = list(color = "rgb(84,39,143)", width = 3)))
    fig %>% layout(xaxis = xlab, yaxis = ylab)
  })
  
  # Basic Stats -> Mother's Education
  basic_edu <- reactive({
    alive <- df_live %>%
      filter(mothers_education != 9) %>% 
      group_by(mothers_education) %>% 
      summarise(live_count = n())
    
    dead <- df_mort %>% 
      filter(mothers_education != 9) %>%
      group_by(mothers_education) %>% 
      summarise(death_count = n())
    
    mom_edu = c('8th Grade or Less', 'Some High School (No Diploma)', 'High School Grad or GED', 'Some College (No Degree)', "Associate's Degree", "Bachelor's Degree", "Master's Degree", "Doctorate Degree")
    death_rate = dead$death_count / ((alive$live_count + dead$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    data = data.frame(mom_edu, death_rate)
    data$mom_edu = factor(data$mom_edu, levels=data$mom_edu)
    data$death_rate = round(data$death_rate, 1)
    data
  })
  
  output$basic_edu_plot <- renderPlotly({
    f = list(family = "Verdana", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "Mother's Education", titlefont = f, tickfont = t)
    ylab = list(title = "Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(basic_edu(), x = ~mom_edu, y = ~death_rate, type='bar',
                   text = basic_edu()$death_rate, textposition = 'auto',
                   marker = list(color = "rgb(251,106,74)", line = list(color = "rgb(203,24,29)", width = 3)))
    fig %>% layout(xaxis = xlab, yaxis = ylab)
  })
  
  # Explore Data -> Basic Stats -> Mother's Race
  basic_race <- reactive({
    alive <- df_live %>%
      group_by(mothers_race) %>% 
      summarise(live_count = n())
    
    dead <- df_mort %>% 
      group_by(mothers_race) %>% 
      summarise(death_count = n()) 
    
    mom_race = c('White', 'Black', 'American Indian Alaskan Native', 'Asian', 'Native Hawaiian or Pacific Islander', 'More Than One')
    death_rate = dead$death_count / ((alive$live_count + dead$death_count)/1000) # Infant Mortality Rate per 1000 births
    
    data = data.frame(mom_race, death_rate, stringsAsFactors = F)
    data$death_rate = round(data$death_rate, 1)
    data$mom_race = factor(data$mom_race, levels=data$mom_race[order(data$death_rate, decreasing = T)])
    data
  })
  
  output$basic_race_plot <- renderPlotly({
    f = list(family = "Verdana", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "Mother's Race", titlefont = f, tickfont = t)
    ylab = list(title = "Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(basic_race(), x = ~as.factor(mom_race), y = ~death_rate, type='bar',
                   text = basic_race()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(107,174,214)", line = list(color = "rgb(33,113,181)", width = 3)))
    fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)
    fig
  })
  
  # Customized Results
  
  demo_custom <- reactive({
    if ((input$mothers_age_Input != "Select Option") & (input$mothers_edu_Input != "Select Option") & (input$mothers_race_Input != "Select Option")) {
      mothers_age_Input = switch(input$mothers_age_Input,
                                 "Under 15 years old" = 1,
                                 "15 to 19 years old" = 2,
                                 "20 to 24 years old" = 3,
                                 "25 to 29 years old" = 4,
                                 "30 to 34 years old" = 5,
                                 "35 to 39 years old" = 6,
                                 "40 to 44 years old" = 7,
                                 "45 to 50 years old" = 8,
                                 "50 to 54 years old" = 9)
      mothers_edu_Input = switch(input$mothers_edu_Input,
                                 '8th Grade or Less' = 1,
                                 'Some High School - No Diploma' = 2,
                                 'High School Diploma' = 3,
                                 'Some College - No Degree' = 4,
                                 "Associate's" = 5,
                                 "Bachelor's" = 6,
                                 "Master's" = 7,
                                 "Doctorate's" = 8)
      mothers_race_Input = switch(input$mothers_race_Input,
                                  'White' = 1,
                                  'Black' = 2,
                                  'American Indian Alaskan Native' = 3,
                                  'Asian' = 4,
                                  'Native Hawaiian or Pacific Islander' = 5,
                                  'More Than One (Mixed)' = 6)
      alive <- df_live %>%
        filter(mothers_age_recode == mothers_age_Input, mothers_education == mothers_edu_Input, mothers_race == mothers_race_Input) %>% 
        group_by(mothers_age_recode, mothers_education, mothers_race) %>%
        summarise(live_count = n()) 
      
      dead <- df_mort %>%
        filter(mothers_age_recode == mothers_age_Input, mothers_education == mothers_edu_Input, mothers_race == mothers_race_Input) %>% 
        group_by(mothers_age_recode, mothers_education, mothers_race) %>%
        summarise(death_count = n())
      
      death_rate = round(dead$death_count / ((alive$live_count + dead$death_count)/1000), 1) # Infant Mortality Rate per 1000 births
      death_rate
    } else {
      death_rate = 5.8 # Infant Mortality Rate per 1000 births
      death_rate
    }
  })

  output$births_demo_box <- renderValueBox({
    valueBox(
      "",
      value = 3791712
    )
  })
  output$birthrate_demo_box <- renderValueBox({
    valueBox(
      "",
      value = 12.1
    )
  })
  output$fertrate_demo_box <- renderValueBox({
    valueBox(
      "",
      value = 60.2
    )
  })
  
  output$deaths_demo_box <- renderValueBox({
    valueBox(
      "",
      value = 22280
    )
  })
  output$nat_deathrate_demo_box <- renderValueBox({
    valueBox(
      "",
      value = 5.8
    )
  })
  output$deathrate_demo_box <- renderValueBox({
    valueBox(
      "",
      value = demo_custom()
    )
  })
  
  # Physical Health -> Body Mass Index (BMI)
  health_bmi <- reactive({
    alive <- df_live %>%
      filter(mothers_bmi != 9) %>% 
      group_by(mothers_bmi) %>% 
      summarise(live_count = n())
    
    dead <- df_mort %>% 
      filter(mothers_bmi != 9) %>%
      group_by(mothers_bmi) %>% 
      summarise(death_count = n())
    
    mom_bmi = c('Underweight', 'Normal', 'Overweight', 'Obese Level 1', 'Obese Level 2', 'Extremely Obese')
    death_rate = dead$death_count / ((alive$live_count + dead$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    data = data.frame(mom_bmi, death_rate)
    data$death_rate = round(data$death_rate, 1)
    data
  })
  
  output$health_bmi_plot <- renderPlotly({
    f = list(family = "Arial", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "Mother's Body Mass Index (BMI)", titlefont = f, tickfont = t)
    ylab = list(title = "Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(health_bmi(), x = ~mom_bmi, y = ~death_rate, type='bar',
                   text = health_bmi()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(107,174,214)", line = list(color = "rgb(33,113,181)", width = 3)))
    fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)
    fig
  })
  
  # Physical Health -> Infections Present
  health_infections <- reactive({
    infect = c("Gonorrhea", "Syphilis", "Chlamydia", "Hepatitis B", "Hepatitis C")
    infect_yes = infections %>% filter(mom_risk == 'Yes') %>% .$death_rate
    infect_no = infections %>% filter(mom_risk == 'No') %>% .$death_rate
    
    data = data.frame(infect, infect_yes, infect_no)
    data
  })
  
  output$health_infections_plot <- renderPlotly({
    f = list(family = "Arial", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "Most Commonly Present Infections", titlefont = f, tickfont = t)
    ylab = list(title = "Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(health_infections(), x = ~infect, y = ~infect_yes, type = 'bar', name = 'Infection Present',
                   text = health_infections()$infect_yes, textposition = 'auto')
    fig <- fig %>% add_trace(y = ~infect_no, name = 'Infection Absent', text = health_infections()$infect_no, textposition = 'auto')
    fig <- fig %>% layout(xaxis = xlab, yaxis = ylab, barmode = 'group')
    fig
  })
  
  # Physical Health -> Diagnosed Risk Factors
  health_risks <- reactive({
    risk = c("Pre-Pregnancy Diabetes", "Gestational Diabetes", "Pre-Pregnancy Hypertension", "Gestational Hypertension", "Hyptertension Eclampsia", "Previous Preterm Birth")
    risk_yes = risk_factors %>% filter(mom_infect == 'Yes') %>% .$death_rate
    risk_no = risk_factors %>% filter(mom_infect == 'No') %>% .$death_rate
    
    data = data.frame(risk, risk_yes, risk_no)
    data$risk_yes = round(data$risk_yes, 1)
    data$risk_no = round(data$risk_no, 1)
    data
  })
  
  output$health_risks_plot <- renderPlotly({
    f = list(family = "Arial", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "Most Commonly Diagnosed Risk Factors", titlefont = f, tickfont = t)
    ylab = list(title = "Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(health_risks(), x = ~risk, y = ~risk_yes, type = 'bar', name = 'Risk Present',
                   text = health_risks()$risk_yes, textposition = 'auto')
    fig <- fig %>% add_trace(y = ~risk_no, name = 'Risk Absent', text = health_risks()$risk_no, textposition = 'auto')
    fig <- fig %>% layout(xaxis = xlab, yaxis = ylab, barmode = 'group')
    fig
  })
  
  
  # Physical Health -> Custom Results
  health_custom <- reactive({
    if ((input$mothers_bmi_Input != "Select Option") & (input$mothers_infections_Input != "Select Option") & (input$mothers_risks_Input != "Select Option")) {
      mothers_bmi_Input = switch(input$mothers_bmi_Input,
                                 'Underweight: < 18.5' = 1,
                                 'Normal: 18.5 to 24.9' = 2,
                                 'Overweight: 25.0 to 29.9' = 3,
                                 'Obese Level 1: 30.0 to 34.9' = 4,
                                 'Obese Level 2: 35.0 to 39.9' = 5,
                                 'Extremely Obese: 40.0 or more' = 6)
      mothers_infections_Input = switch(input$mothers_infections_Input,
                                        "Gonorrhea" = "gonorrhea",
                                        "Syphilis" = "syphilis",
                                        "Chlamydia" = "chlamydia",
                                        "Hepatitis B" = "hepB",
                                        "Hepatitis C" = "hepC")
      mothers_infections_Input = as.factor(mothers_infections_Input)
      
      mothers_risks_Input = switch(input$mothers_risks_Input,
                                   "Pre-pregnancy Diabetes" = "pre_preg_diab",
                                   "Gestational Diabetes" = "gest_diab",
                                   "Pre-pregnancy Hypertension" = "pre_preg_hypten",
                                   "Gestational Hypertension" = "gest_hypten",
                                   "Hypertension Ecalmpsia" = "hypten_ecl",
                                   "Previous Preterm Births" = "prev_preterm_birth")
      mothers_risks_Input = as.factor(mothers_risks_Input)
      
      if ((input$mothers_infections_Input == 'None') & (input$mothers_risks_Input == 'None')) {
        alive <- df_live %>%
          filter(mothers_bmi == mothers_bmi_Input,
                 gonorrhea == 'N', syphilis == 'N', chlamydia == 'N', hepB == 'N', hepC == 'N',
                 pre_preg_diab == 'N', gest_diab == 'N', pre_preg_hypten == 'N', gest_hypten == 'N', hypten_ecl == 'N', prev_preterm_birth == 'N') %>% 
          group_by(mothers_bmi) %>% 
          summarise(live_count = n()) 
        
        dead <- df_mort %>% 
          filter(mothers_bmi == mothers_bmi_Input,
                 gonorrhea == 'N', syphilis == 'N', chlamydia == 'N', hepB == 'N', hepC == 'N',
                 pre_preg_diab == 'N', gest_diab == 'N', pre_preg_hypten == 'N', gest_hypten == 'N', hypten_ecl == 'N', prev_preterm_birth == 'N') %>% 
          group_by(mothers_bmi) %>% 
          summarise(death_count = n())
        
        death_rate = round(dead$death_count / ((alive$live_count + dead$death_count)/1000), 1) # Infant Mortality Rate per 1000 births
        death_rate
      }
      else if ((input$mothers_infections_Input != 'None') & (input$mothers_risks_Input == 'None')) {
        alive <- df_live %>%
          filter(mothers_bmi == mothers_bmi_Input,
                 mothers_infections_Input == 'Y',
                 pre_preg_diab == 'N', gest_diab == 'N', pre_preg_hypten == 'N', gest_hypten == 'N', hypten_ecl == 'N', prev_preterm_birth == 'N') %>% 
          group_by(mothers_bmi) %>% 
          summarise(live_count = n()) 
        
        dead <- df_mort %>% 
          filter(mothers_bmi == mothers_bmi_Input,
                 mothers_infections_Input == 'Y', 
                 pre_preg_diab == 'N', gest_diab == 'N', pre_preg_hypten == 'N', gest_hypten == 'N', hypten_ecl == 'N', prev_preterm_birth == 'N') %>% 
          group_by(mothers_bmi) %>%
          summarise(death_count = n())
        
        death_rate = round(dead$death_count / ((alive$live_count + dead$death_count)/1000), 1) # Infant Mortality Rate per 1000 births
        death_rate
      }
      
      
    } else {
      death_rate = 5.8 # Infant Mortality Rate per 1000 births
      death_rate
    }
  })
  
  output$births_health_box <- renderValueBox({
    valueBox(
      "",
      value = 3791712
    )
  })
  output$birthrate_health_box <- renderValueBox({
    valueBox(
      "",
      value = 12.1
    )
  })
  output$fertrate_health_box <- renderValueBox({
    valueBox(
      "",
      value = 60.2
    )
  })
  
  output$deaths_health_box <- renderValueBox({
    valueBox(
      "",
      value = 22280
    )
  })
  output$nat_deathrate_health_box <- renderValueBox({
    valueBox(
      "",
      value = 5.8
    )
  })
  output$deathrate_health_box <- renderValueBox({
    valueBox(
      "",
      value = health_custom()
    )
  })
  
  
  ### Health & Lifestyle ###
  # Weight/BMI -> Infant Survival (infant living at report)
  p1 = function() {
    natall_small %>%
      filter(infant_living_at_report != 'U') %>% 
      group_by(infant_living_at_report, infant_breastfed_at_discharge) %>%
      summarise_at(vars(mothers_bmi,mothers_age,mothers_race,mothers_education,fathers_education),
                   list(average = mean))
  }
  
  output$bmi_living <- renderPlotly({
    plot_ly(p1(), x = ~infant_living_at_report, y = ~mothers_bmi_average, type = 'box', name = "mothers_bmi_average") %>%
      layout(barmode = 'group',
             xaxis = list(title = "Infant Living at Report"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
  })
  
  # Weight/BMI -> NICU Admittance
  p3 = reactive({
    natall_small %>%
      filter(admit_NICU != 'U') %>% 
      group_by(admit_NICU,surfactant,antibiotics_for_newborn,seizures,assist_vent_immed,assist_vent_after6) %>%
      summarise_at(vars(mothers_bmi, mothers_age),
                   list(average = mean))
  })
  
  output$bmi_nicu <- renderPlotly({
    plot_ly(p3(), x = ~admit_NICU, y = ~mothers_bmi_average, type = 'box', name = "mothers_bmi_average") %>%
      layout(barmode = 'group',
             xaxis = list(title = "Mother Admitted into NICU"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
  })
  
  # Weight/BMI -> Premature Babies (fetal dev)
  p4 = function() {
    natall_small %>%
      filter(combined_gestation_wk != 99) %>% 
      group_by(combined_gestation_wk) %>%
      summarise_at(vars(mothers_bmi),
                   list(average = mean))
  }
  output$bmi_preme <- renderPlotly({
    plot_ly(p4(), x = ~combined_gestation_wk, name = "Mothers BMI",
            line = list(color = 'rgb(0, 0, 0)', width = 1)) %>%
      add_trace(y = ~average, name = "Fetal Development at Birth", mode='lines+markers',dash='dash') %>% 
      layout(xaxis = list(title = "Gestation Period (weeks)"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)")) %>%
      add_trace(
        x = c(38,39,40),
        y = c(28.98222,28.84011,28.43967),
        marker = list(
          color = 'rgba(17, 157, 255,0)',
          size = 20,
          line = list(
            color = 'rgb(164, 198, 57)',
            width = 2
          )
        ),
        showlegend = F
      ) %>% 
      add_trace(
        x = c(17:37),
        y = c(40.61111,36.69706,37.74408,38.77598,37.78296,36.665474,35.59231,34.54177,
              33.42062,32.2899,32.606,32.73843,31.86384,31.6043,31.14695,31.25102,31.12922,
              30.59386,30.24277,29.87827,29.57763),
        marker = list(
          color = 'rgba(17, 157, 255,0)',
          size = 20,
          line = list(
            color = 'rgb(230, 18, 49)',
            width = 2
          )
        ),
        showlegend = F
      )
  })
  
  # Weight/BMI -> APGAR 5 min
  p5 = function() {
    natall_small %>%
      filter(APGAR_score_5min != 99) %>% 
      group_by(APGAR_score_5min, mothers_marital_status) %>%
      summarise_at(vars(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                        mothers_bmi, combined_gestation_wk),
                   list(average = mean))
  }
  
  output$bmi_5min <- renderPlotly({
    plot_ly(p5(), x = ~APGAR_score_5min, y = ~mothers_bmi_average, type = 'box', name = "APGAR 5") %>%
      layout(barmode = 'group',
             xaxis = list(title = "APGAR 5 Min Scores"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
  })
  
  # Weight/BMI -> APGAR 10 min
  p6 = function() {
    natall_small %>%
      filter(APGAR_score_10min != 88, APGAR_score_10min != 99) %>% 
      group_by(APGAR_score_10min,mothers_marital_status) %>%
      summarise_at(vars(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                        mothers_bmi, combined_gestation_wk),
                   list(average = mean))
  }
  output$bmi_10min <- renderPlotly({
    apgar10_bmi <- plot_ly(p6(), x = ~APGAR_score_10min, y = ~mothers_bmi_average, type = 'box', name = "APGAR 10") %>%
      layout(barmode = 'group',
             xaxis = list(title = "APGAR 10 Min Scores"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
  })
  
  # Weight/BMI -> Final delivery method
  output$bmi_final_del <- renderPlotly({
    fig <- plot_ly(final_del_bmi18, x = ~final_delivery_method, type = 'bar', y = ~mothers_bmi_average, name = "Mother's BMI", text = round(final_del_bmi18$mothers_bmi_average), textposition = 'auto')
    fig <- fig %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight', text = round(final_del_bmi18$pre_preg_lbs_average), textposition = 'auto')
    fig <- fig %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight', text = round(final_del_bmi18$delivery_lbs_average), textposition = 'auto')
    fig <- fig %>% layout(barmode = 'group',
                          xaxis = list(title = "Final Delivery Method Used"),
                          yaxis = list(title = "Average Values"))
    fig
  })
  
  # Weight/BMI -> Prenatal Visits - Mother's Age
  prenatal_age_mother = function() {
    data = natall_small %>% 
      group_by(mothers_age) %>% 
      summarise_at(vars(n_prenatal_visits, mothers_education, fathers_education, fathers_age),
                   list(average = mean))
    data
  }
  
  output$bmi_prenat_mother <- renderPlot({
    fig = ggplot(prenatal_age_mother(), aes(x = mothers_age, y = n_prenatal_visits_average, group = 1)) +
      geom_line(linetype="dashed", color="blue", size=0.2) +
      labs(title = NULL, x = "Mother's Age", y = "Average Number of Prenatal Visits Made") +
      geom_point()
    fig
  })
  
  # Smoking -> Pre-Pregnancy Tobacco Use
  smoke_pre_preg <- reactive({
    # Non Smokers
    alive_nonsmoker <- df_live %>%
      filter(cigs_before_preg != 99, cigs_before_preg == 0) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_nonsmoker <- df_mort %>% 
      filter(cigs_before_preg != 99, cigs_before_preg == 0) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_nonsmoker = dead_nonsmoker$death_count / ((alive_nonsmoker$live_count + dead_nonsmoker$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    # Smokers
    alive_smoker <- df_live %>%
      filter(cigs_before_preg != 99, cigs_before_preg > 0) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_smoker <- df_mort %>% 
      filter(cigs_before_preg != 99, cigs_before_preg > 0) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_smoker = dead_smoker$death_count / ((alive_smoker$live_count + dead_smoker$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    mom_tobacco = c('Smokers', 'Non Smokers')
    death_rate = c(death_rate_smoker, death_rate_nonsmoker)
    data = data.frame(mom_tobacco, death_rate)
    data$death_rate = round(data$death_rate, 1)
    data # CIA World Factbook has the U.S. infant mortality rate at 5.80 in 2017
  })
  
  output$smoke_pre_preg_plot <- renderPlotly({
    f = list(family = "Arial", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "", titlefont = f, tickfont = t)
    ylab = list(title = "Number of Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(smoke_pre_preg(), y = ~mom_tobacco, x = ~death_rate, type='bar', orientation = 'h',
                   text = smoke_pre_preg()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(251,106,74)", line = list(color = "rgb(203,24,29)", width = 3)))
    fig <- fig %>% layout(xaxis = ylab, yaxis = xlab)
    fig
  })
  
  # Smoking - trimesters
  mtobacco_age = reactive({
    natall_small %>% 
      filter(mothers_age > 14) %>% 
      group_by(mothers_age) %>% 
      summarise_at(vars(cigs_tri1, cigs_tri2, cigs_tri3),
                   list(average = mean))
  })

  # Smoking -> 1st trimester Tobacco Use
  output$first_tri_age = renderPlot({
    mtobacco_age1 = ggplot(mtobacco_age(), aes(x=mothers_age, y=cigs_tri1_average, group=1)) +
      geom_line(linetype="dashed", color="red", size=0.7)+
      labs(title ="Average Tobacco Use in the 1st Trimester in Relation to the Age of the Mother")+
      xlab("Mothers Age")+
      ylab("Average Tobacco Use in Trimester 1") +
      geom_point(size=3)
    
    mtobacco_age1
  })
  
  # Smoking -> 2nd trimester Tobacco Use
  output$second_tri_age = renderPlot({
    mtobacco_age2 = ggplot(mtobacco_age(), aes(x=mothers_age, y=cigs_tri2_average, group=1)) +
      geom_line(linetype="dashed", color="red", size=0.7)+
      labs(title ="Average Tobacco Use in the 2nd Trimester in Relation to the Age of the Mother")+
      xlab("Mothers Age")+
      ylab("Average Tobacco Use in Trimester 2")+
      geom_point(size=3)
    
    mtobacco_age2
  })

  # Smoking -> 3rd trimester Tobacco Use
  output$third_tri_age = renderPlot({
    mtobacco_age3= ggplot(mtobacco_age(), aes(x=mothers_age, y=cigs_tri3_average, group=1)) +
      geom_line(linetype="dashed", color="red", size=0.7)+
      labs(title ="Average Tobacco Use in the 3rd Trimester in Relation to the Age of the Mother")+
      xlab("Mothers Age")+
      ylab("Average Tobacco Use in Trimester 3")+
      geom_point(size=3)
    
    mtobacco_age3
  })
  
  # Tobacco Use and NICU Admittance
  smoke_1st_tri <- reactive({
    # Non Smokers
    alive_nonsmoker <- df_live %>%
      filter(cigs_tri1 == 0) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_nonsmoker <- df_mort %>% 
      filter(cigs_tri1 == 0) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_nonsmoker = dead_nonsmoker$death_count / ((alive_nonsmoker$live_count + dead_nonsmoker$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    alive_smoker1less <- df_live %>%
      filter((cigs_tri1 > 0), (cigs_tri1 <= 20), (cigs_tri2 == 0), (cigs_tri3 == 0)) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_smoker1less <- df_mort %>% 
      filter((cigs_tri1 > 0) & (cigs_tri1 <= 20), (cigs_tri2 == 0), (cigs_tri3 == 0)) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_smoker1less = dead_smoker1less$death_count / ((alive_smoker1less$live_count + dead_smoker1less$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    alive_smoker1more <- df_live %>%
      filter((cigs_tri1 > 20), (cigs_tri2 == 0), (cigs_tri3 == 0)) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_smoker1more <- df_mort %>% 
      filter((cigs_tri1 > 20), (cigs_tri2 == 0), (cigs_tri3 == 0)) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_smoker1more = dead_smoker1more$death_count / ((alive_smoker1more$live_count + dead_smoker1more$death_count) / 1000)
    
    mom_tobacco = c('Non Smokers', 'Smokers 1 Pack or Less', 'Smokers 1 Pack or More')
    death_rate = c(death_rate_nonsmoker, death_rate_smoker1less, death_rate_smoker1more)
    data = data.frame(mom_tobacco, death_rate)
    data$death_rate = round(data$death_rate, 1)
    data # CIA World Factbook has the U.S. infant mortality rate at 5.80 in 2017
  })
  
  output$smoke_death1 <- renderPlotly({
    f = list(family = "Arial", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "", titlefont = f, tickfont = t)
    ylab = list(title = "Number of Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(smoke_1st_tri(), y = ~mom_tobacco, x = ~death_rate, type='bar', orientation = 'h',
                   text = smoke_1st_tri()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(251,106,74)", line = list(color = "rgb(203,24,29)", width = 3)))
    fig <- fig %>% layout(xaxis = ylab, yaxis = xlab)
    fig
  })
  
  smoke_2nd_tri <- reactive({
    # Non Smokers
    alive_nonsmoker <- df_live %>%
      filter(cigs_tri1 == 0, cigs_tri2 == 0) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_nonsmoker <- df_mort %>% 
      filter(cigs_tri1 == 0, cigs_tri2 == 0) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_nonsmoker = dead_nonsmoker$death_count / ((alive_nonsmoker$live_count + dead_nonsmoker$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    # Smokers
    alive_smoker1less <- df_live %>%
      filter((cigs_tri2 > 0), (cigs_tri2 <= 20), (cigs_tri1 > 0)) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_smoker1less <- df_mort %>% 
      filter((cigs_tri2 > 0), (cigs_tri2 <= 20), (cigs_tri1 > 0)) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_smoker1less = dead_smoker1less$death_count / ((alive_smoker1less$live_count + dead_smoker1less$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    alive_smoker1more <- df_live %>%
      filter((cigs_tri2 > 20), (cigs_tri1 > 0)) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_smoker1more <- df_mort %>% 
      filter((cigs_tri2 > 20), (cigs_tri1 > 0)) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_smoker1more = dead_smoker1more$death_count / ((alive_smoker1more$live_count + dead_smoker1more$death_count) / 1000)
    
    mom_tobacco = c('Non Smokers', 'Smokers 1 Pack or Less', 'Smokers 1 Pack or More')
    death_rate = c(death_rate_nonsmoker, death_rate_smoker1less, death_rate_smoker1more)
    data = data.frame(mom_tobacco, death_rate)
    data$death_rate = round(data$death_rate, 1)
    data # CIA World Factbook has the U.S. infant mortality rate at 5.80 in 2017
  })
  
  output$smoke_death2 <- renderPlotly({
    f = list(family = "Arial", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "", titlefont = f, tickfont = t)
    ylab = list(title = "Number of Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(smoke_2nd_tri(), y = ~mom_tobacco, x = ~death_rate, type='bar', orientation = 'h',
                   text = smoke_2nd_tri()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(251,106,74)", line = list(color = "rgb(203,24,29)", width = 3)))
    fig <- fig %>% layout(xaxis = ylab, yaxis = xlab)
    fig
  })
  
  smoke_3rd_tri <- reactive({
    # Non Smokers
    alive_nonsmoker <- df_live %>%
      filter(cigs_tri1 == 0, cigs_tri2 == 0, cigs_tri3 == 0) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_nonsmoker <- df_mort %>% 
      filter(cigs_tri1 == 0, cigs_tri2 == 0, cigs_tri3 == 0) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_nonsmoker = dead_nonsmoker$death_count / ((alive_nonsmoker$live_count + dead_nonsmoker$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    # Smokers
    alive_smoker1less <- df_live %>%
      filter((cigs_tri3 > 0), (cigs_tri3 <= 20), (cigs_tri1 > 0), (cigs_tri2 > 0)) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_smoker1less <- df_mort %>% 
      filter((cigs_tri3 > 0), (cigs_tri3 <= 20), (cigs_tri1 > 0), (cigs_tri2 > 0)) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_smoker1less = dead_smoker1less$death_count / ((alive_smoker1less$live_count + dead_smoker1less$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    alive_smoker1more <- df_live %>%
      filter(cigs_tri3 > 20, (cigs_tri1 > 0), (cigs_tri2 > 0)) %>% # filter out unknown or not stated
      summarise(live_count = n())
    
    dead_smoker1more <- df_mort %>% 
      filter(cigs_tri3 > 20, (cigs_tri1 > 0), (cigs_tri2 > 0)) %>% # filter out unknown or not stated
      summarise(death_count = n())
    
    death_rate_smoker1more = dead_smoker1more$death_count / ((alive_smoker1more$live_count + dead_smoker1more$death_count) / 1000)
    
    mom_tobacco = c('Non Smokers', 'Smokers 1 Pack or Less', 'Smokers 1 Pack or More')
    death_rate = c(death_rate_nonsmoker, death_rate_smoker1less, death_rate_smoker1more)
    data = data.frame(mom_tobacco, death_rate)
    data$death_rate = round(data$death_rate, 1)
    data # CIA World Factbook has the U.S. infant mortality rate at 5.80 in 2017
  })
  
  output$smoke_death3 <- renderPlotly({
    f = list(family = "Arial", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "", titlefont = f, tickfont = t)
    ylab = list(title = "Number of Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(smoke_3rd_tri(), y = ~mom_tobacco, x = ~death_rate, type='bar', orientation = 'h',
                   text = smoke_3rd_tri()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(251,106,74)", line = list(color = "rgb(203,24,29)", width = 3)))
    fig <- fig %>% layout(xaxis = ylab, yaxis = xlab)
    fig
  })
})
    
# Create Shiny object
shinyApp(ui = ui, server = server)
