# Load packages#######################################################################
######################################################################################
######################################################################################
library(shiny)
library(plyr)
library(dplyr)
library(zoo)
library(DT)
library(stats)
library(base)
library(ggplot2)
library(bootstraplib)
library(plotly)
library(htmltools)
library(htmlwidgets)
library(httpuv)

#############################             IMPORT OF RAW DATA    ######################
######################################################################################
######################################################################################

# Change this to your own working directory where you keep your large datasets
setwd('~/Desktop/NYCDSA/Capstone/data/Linked_BD_17')

# Load Linked Mortality 2017 Data: RUN ONLY FIRST TIME, comment out after to save computational speed
#df_mort = read.csv('mort17_dead.csv') # dead infants; 5570 rows (a quarter of 22,280)
#df_mort = df_mort %>% select(everything(), -X)
#
#df_live = read.csv('mort17_live.csv') # live infants; 966,196 rows (a quarter of 3.8 million)
#df_live = df_live %>% select(everything(), -X)

setwd('~/Desktop/NYCDSA/Capstone/data')
#natall <- read.csv('natality_all.csv', stringsAsFactors = F)

#final_del_bmi18 = read.csv('./Natality 18 Jason/Jason_plots/final_delivery_bmi18.csv')
#nat18 <- read.csv('./nat_quart_2018.csv')

#############################                 PREPROCESSING      #####################
######################################################################################
######################################################################################

# Change working directory back to app directory
setwd('~/Desktop/NYCDSA/Capstone/capstone-CDC-infants/Edwin/app_draft/')

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
                div(class="position-relative align-center",
                    titlePanel("CDC Infant Natality & Mortality Analysis")
                ),
                
                # Main navbar
                navbarPage(icon("plus-square"), collapsible=T,
                           tabPanel("Meet The Team",
                                    fluidRow(
                                      column(3,
                                             div(class="card mb-3",
                                                 div(class="card-header text-white bg-primary", "Edwin Back Picture"),
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
                                                 div(class="card-header text-white bg-secondary", "Baptiste Mokas Picture"),
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
                                                 div(class="card-header text-white bg-warning", "Michael Lim Picture"),
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
                                      ),
                                      column(3,
                                             div(class="card mb-3",
                                                 div(class="card-header text-white bg-info", "Jason Hoffmeier Picture"),
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
                                      )
                                    )
                           ),
                           tabPanel("Get Started", # Outer Navbar - Get Started
                                    tabsetPanel(type="tabs",
                                                tabPanel("Description", br(),
                                                         fluidRow(
                                                           column(6,
                                                                  div(class = "jumbotron bg-info",
                                                                      h1("CDC Infant Natality & Mortality"), br(),
                                                                      p("The CDC provides a wealth of data and information regarding infant natality and mortality from U.S. certified births.")
                                                                  )
                                                           ),
                                                           column(6,
                                                                  div(class = "jumbotron bg-success",
                                                                      h1("Our Mission"), br(),
                                                                      p("To educate expecting mothers about key health & life-style choices they can make that may reduce infant mortality")
                                                                  )
                                                           )
                                                         )
                                                ),
                                                tabPanel("Deciphering Results", br(),
                                                         fluidRow(
                                                           column(4,
                                                                  div(class = "jumbotron bg-secondary",
                                                                      h1("Mother's Age Range Decoded"),
                                                                      hr(),
                                                                      h5("Age Range Code ", em("1"), ": Under 15 years old"),
                                                                      h5("Age Range Code ", em("2"), ": 15 to 19 years old"),
                                                                      h5("Age Range Code ", em("3"), ": 20 to 24 years old"),
                                                                      h5("Age Range Code ", em("4"), ": 25 to 29 years old"),
                                                                      h5("Age Range Code ", em("5"), ": 30 to 34 years old"),
                                                                      h5("Age Range Code ", em("6"), ": 35 to 39 years old"),
                                                                      h5("Age Range Code ", em("7"), ": 40 to 44 years old"),
                                                                      h5("Age Range Code ", em("8"), ": 45 to 49 years old"),
                                                                      h5("Age Range Code ", em("9"), ": 50 to 54 years old")
                                                                  )
                                                           ),
                                                           column(4,
                                                                  div(class = "jumbotron bg-warning",
                                                                      h1("Mother's Education Decoded"),
                                                                      hr(),
                                                                      h5("Education Code ", em("1"), ": 8th Grade or Less"),
                                                                      h5("Education Code ", em("2"), ": Some High School - No Diploma"),
                                                                      h5("Education Code ", em("3"), ": High School Diploma or GED"),
                                                                      h5("Education Code ", em("4"), ": Some College - No Degree"),
                                                                      h5("Education Code ", em("5"), ": Associate's"),
                                                                      h5("Education Code ", em("6"), ": Bachelor's"),
                                                                      h5("Education Code ", em("7"), ": Master's"),
                                                                      h5("Education Code ", em("8"), ": Doctorate's")
                                                                  )
                                                           ),
                                                           column(4,
                                                                  div(class = "jumbotron bg-info",
                                                                      h1("Mother's Race Decoded"),
                                                                      hr(),
                                                                      h5("Race Code ", em("1"), ": White"),
                                                                      h5("Race Code ", em("2"), ": Black"),
                                                                      h5("Race Code ", em("3"), ": American Indian Alaskan Native"),
                                                                      h5("Race Code ", em("4"), ": Asian"),
                                                                      h5("Race Code ", em("5"), ": Native Hawaiian or Pacific Islander"),
                                                                      h5("Race Code ", em("6"), ": More Than One Race (Mixed)")
                                                                  )
                                                           )
                                                         )
                                                )
                                    )
                           ),
                           # Dropdown Navbar - Explore Data
                           navbarMenu("Explore Statistics",
                                      tabPanel("Demographics", # Basic Stats navbar
                                               tabsetPanel(type = "pills",
                                                           tabPanel("Age", br(), br(),
                                                                    h3("Infant Death Rates Based on Mother's Age"), hr(),
                                                                    plotlyOutput("basic_age_plot")
                                                           ),
                                                           tabPanel("Education", br(), br(),
                                                                    h3("Infant Death Rates Based on Mother's Education"), hr(),
                                                                    plotlyOutput("basic_edu_plot")
                                                           ),
                                                           tabPanel("Race", br(), br(),
                                                                    h3("Infant Death Rates Based on Mother's Race"), hr(),
                                                                    plotlyOutput("basic_race_plot")
                                                           ),
                                                           tabPanel("Enter Your Info & View Results", br(),
                                                                    # Sidebar panel for inputs: mother's age, mother's education, mother's race
                                                                    sidebarLayout(
                                                                      sidebarPanel(
                                                                        selectizeInput("mothers_age_Input", label="Current Age", choices=c("Select Option",
                                                                                                                                           'Under 15 years old' = 1,
                                                                                                                                           '15 to 19 years old' = 2,
                                                                                                                                           '20 to 24 years old' = 3,
                                                                                                                                           '25 to 29 years old' = 4,
                                                                                                                                           '30 to 34 years old' = 5,
                                                                                                                                           '35 to 39 years old' = 6,
                                                                                                                                           '40 to 44 years old' = 7,
                                                                                                                                           '45 to 49 years old' = 8,
                                                                                                                                           '50 to 54 years old' = 9), selected=F),
                                                                        selectizeInput("mothers_edu_Input", label="Highest Education", choices=c("Select Option",
                                                                                                                                                 '8th Grade or Less' = 1,
                                                                                                                                                 'Some High School - No Diploma' = 2,
                                                                                                                                                 'High School Diploma' = 3,
                                                                                                                                                 'Some College - No Degree' = 4,
                                                                                                                                                 "Associate's" = 5,
                                                                                                                                                 "Bachelor's" = 6,
                                                                                                                                                 "Master's" = 7,
                                                                                                                                                 "Doctorate's" = 8), selected=F),
                                                                        selectizeInput("mothers_race_Input", label="Race", choices=c("Select Options",
                                                                                                                                     'White' = 1,
                                                                                                                                     'Black' = 2,
                                                                                                                                     'American Indian Alaskan Native' = 3,
                                                                                                                                     'Asian' = 4,
                                                                                                                                     'Native Hawaiian or Pacific Islander' = 5,
                                                                                                                                     'More Than One (Mixed)' = 6), selected=F)
                                                                      ),
                                                                      # Main panel for displaying outputs
                                                                      mainPanel(
                                                                        h3("Custom Results"),
                                                                        plotlyOutput("basic_plot")
                                                                      )
                                                                    )
                                                           )
                                               )
                                      ),
                                      # Explore - Health Stats
                                      tabPanel("Physical Health",
                                               tabsetPanel(type="pills",
                                                           tabPanel(
                                                             "Body Mass Index (Overall Health)", br(),
                                                             h3("Infant Death Rate Based on Mother's BMI"), br(),
                                                             plotlyOutput("health_bmi_plot")
                                                           ),
                                                           tabPanel(
                                                             "Pre Pregnancy Tobacco Use", br(),
                                                             h3("Infant Death Rate Based on Mother's Pre Pregnancy Tobacco Use"), br(),
                                                             plotlyOutput("health_tobacco_plot")
                                                           ),
                                                           tabPanel(
                                                             "Infections", br(),
                                                             h3("Infant Death Rate of Mothers with Infections"), br(),
                                                             plotlyOutput("health_infections_plot")
                                                           ),
                                                           tabPanel(
                                                             "Pre-Existing Conditions", br(),
                                                             h3("Infant Death Rate of Mothers with Pre-Existing Conditions"), br(),
                                                             plotlyOutput("health_conditions_plot")
                                                           ),
                                                           tabPanel(
                                                             "Enter Your Info & View Results", br(), br(),
                                                             sidebarLayout(
                                                               sidebarPanel(
                                                                 selectizeInput("mothers_bmi_Input", label="Estimated BMI Range", choices=c("Select Option",
                                                                                                                                            'Underweight: < 18.5' = 1,
                                                                                                                                            'Normal: 18.5 to 24.9' = 2,
                                                                                                                                            'Overweight: 25.0 to 29.9' = 3,
                                                                                                                                            'Obese Level 1: 30.0 to 34.9' = 4,
                                                                                                                                            'Obese Level 2: 35.0 to 39.9' = 5,
                                                                                                                                            'Extremely Obese: 40.0 or more' = 6), selected=F),
                                                                 selectizeInput("mothers_tobacco_Input", label="Tobacco Use (Pre-Pregnancy)", choices=c("Select Option",
                                                                                                                                                        'Yes' = 1,
                                                                                                                                                        'No' = 2), selected=F),
                                                                 selectizeInput("mothers_infections_Input", label="Present Infections", choices=c("Select Option",
                                                                                                                                                  "None",
                                                                                                                                                  "Gonorrhea" = "gonorrhea",
                                                                                                                                                  "Syphillis" = "syphillis",
                                                                                                                                                  "Chlamydia" = "chlamydia",
                                                                                                                                                  "Hepatitis B" = "hepB",
                                                                                                                                                  "Hepatitis C" = "hepC"), selected=F)
                                                               ),
                                                               mainPanel(
                                                                 h3("Custom Results"), br(),
                                                                 plotlyOutput("health_plot")
                                                               )
                                                             )
                                                           )
                                               )
                                      )
                           ),
                           # Improving Natality
                           navbarMenu("Improve Natality",
                                      tabPanel(
                                        "Fetal Development", br(),
                                        h3("Average BMI of Mothers with Premature Infants"), br(),
                                        plotlyOutput("bmi_preme")
                                      ),
                                      tabPanel( # convert to columns
                                        "First 5 Minutes of Life", br(),
                                        h3("Average BMI of Mothers with Higher 5-Min APGAR Scores"),
                                        plotlyOutput("bmi_5min"), br(),
                                        p("APGAR stands for Appearance, Pulse, Grimace, Activity, and Respiration. It is an overall rating of an infant's health right after being born."),
                                      ),
                                      tabPanel(
                                        "First 10 Minutes of Life",
                                        br(),
                                        h3("Average BMI of Mothers with Higher 10-Min APGAR Scores"),
                                        br(),
                                        plotlyOutput("bmi_10min")
                                      ),
                                      tabPanel(
                                        "NICU Admittance Rate", br(),
                                        h3("Average BMI of Mothers with Infants Admitted to NICU"), br(),
                                        plotlyOutput("bmi_nicu")
                                      ),
                                      tabPanel(
                                        "Infant Survival", br(),
                                        h3("Average BMI of Mothers with Infants Living at Time of Report"), br(),
                                        plotlyOutput("bmi_living")
                                      ),
                                      tabPanel(
                                        "Prenatal Visits", br(),
                                        h3("Average Prenetal Visits Based on Mother's Age"), br(),
                                        plotOutput("bmi_prenat_mother")
                                      ),
                                      tabPanel(
                                        "Final Delivery Method", br(),
                                        h3("Final Delivery Method Based on Average BMI of Mother"), br(),
                                        plotlyOutput("bmi_final_del")
                                      )
                           ),
                           # Model Predictions
                           navbarMenu("Our Predictions",
                                      tabPanel("Models",
                                               tabsetPanel(type="pills",
                                                           tabPanel("Model Description #1", br()), 
                                                           tabPanel("Model Description #2", br()),
                                                           tabPanel("Model Description #3", br())
                                               )
                                      ),
                                      tabPanel("Predictions",
                                               tabsetPanel(type="pills",
                                                           tabPanel("Prediction #1", br()),
                                                           tabPanel("Prediction #2", br()),
                                                           tabPanel("Prediction #3", br())
                                               )
                                               
                                      )
                           ),
                           navbarMenu("More",
                                      tabPanel("CDC Source Data",
                                               tabsetPanel(type="tabs",
                                                           tabPanel("Infant Natality 2018", br()), 
                                                           tabPanel("Infant Natality 2017", br()),
                                                           tabPanel("Infant Natality 2016", br()),
                                                           tabPanel("Linked Infant Births/Deaths 2017", br()),
                                                           tabPanel("Linked Infant Births/Deaths 2016", br())
                                               )
                                      ),
                                      tabPanel("More Item #2",
                                               tabsetPanel(type="tabs",
                                                           tabPanel("Placeholder #1", br()),
                                                           tabPanel("Placeholder #2", br()),
                                                           tabPanel("Placeholder #3", br())
                                               )
                                               
                                      )
                           )
                )
))          

#### SERVER ####                
# Define server function
server <- shinyServer(function(input, output, session) {
  options(shiny.autoreload = TRUE)
  # Explore Data -> Basic Stats -> Mother's Age
  basic_age <- function() {
    alive <- df_live %>%
      group_by(mothers_age_recode) %>% 
      summarise(live_count = n())
    
    dead <- df_mort %>% 
      group_by(mothers_age_recode) %>% 
      summarise(death_count = n())
    
    mom_age = c('Under 15 yrs old', '15 to 19 yrs old', '20 to 24 yrs old', '25 to 29 yrs old', '30 to 34 yrs old', '35 to 39 yrs old', '40 to 44 yrs old', '45 to 49 yrs old', '50 to 54 yrs old')
    death_rate = dead$death_count / ((alive$live_count + dead$death_count)/1000) # Infant Mortality Rate per 1000 births
    
    data = data.frame(mom_age, death_rate, stringsAsFactors = F)
    data$death_rate = round(data$death_rate, 1)
    data$mom_age = factor(data$mom_age, levels=data$mom_age[order(data$death_rate, decreasing = T)])
    data
  }
  
  output$basic_age_plot <- renderPlotly({
    f = list(family = "Verdana", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "Mother's Age Range", titlefont = f, tickfont = t)
    ylab = list(title = "Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)

    fig <- plot_ly(basic_age(), x = ~mom_age, y = ~death_rate, type='bar',
                   text = p()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(128,125,186)", line = list(color = "rgb(84,39,143)", width = 3)))
    fig %>% layout(xaxis = xlab, yaxis = ylab)
  })
  
  # Explore Data -> Basic Stats -> Mother's Education
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
    
    data = data.frame(mom_edu, death_rate, stringsAsFactors = F)
    data$death_rate = round(data$death_rate, 1)
    data$mom_edu = factor(data$mom_edu, levels=data$mom_edu[order(data$death_rate, decreasing = T)])
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
  
  # # Basic Stats -> Customized Results
  # basic_age_edu_race <- reactive({
  #   alive <- df_live %>%
  #     group_by(mothers_age_recode, mothers_education, mothers_race) %>%
  #     summarise(live_count = n())
  #   
  #   dead <- df_mort %>%
  #     group_by(mothers_age_recode, mothers_education, mothers_race) %>%
  #     summarise(death_count = n())
  #   
  #   death_rate = dead$death_count / ((alive$live_count + dead$death_count)/1000) # Infant Mortality Rate per 1000 births
  # 
  #   # fix this part and below plot to output a value box of some useful information
  #   dead %>%
  #     group_by(mothers_age_recode, mothers_education, mothers_race) %>%
  #     filter(mothers_age_recode == input$mothers_age_Input,
  #            mothers_education == input$mothers_edu_Input,
  #              mothers_race == input$mothers_race_Input) %>%
  #     summarise(death_count = n(), percent_deaths = death_count/sum(death_count))
  # })
  # 
  # output$basic_plot <- renderPlotly({
  #   basic_age_edu_race() %>%
  #     ggplot(aes(x = as.factor(mothers_race), y = death_count)) + geom_bar(stat = "identity", color="blue", fill="white") +
  #     labs(x = "Your Selected Demographic Code", y = "Infant Deaths in 2017")
  # })
  
  # Health Stats -> BMI (Overall Health)
  health_bmi <- reactive({
    alive <- df_live %>%
      filter(mothers_bmi != 9) %>% 
      group_by(mothers_bmi) %>% 
      summarise(live_count = n())
    
    dead <- df_mort %>% 
      filter(mothers_bmi != 9) %>% 
      group_by(mothers_bmi) %>% 
      summarise(death_count = n())
    
    mom_bmi = c('Underweight: Less than 18.5', 'Normal: 18.5 to 24.9', 'Overweight: 25.0 to 29.9', 'Obese Level 1: 30.0 to 34.9', 'Obese Level 2: 35.0 to 39.9', 'Extremely Obese: Greater than 40.0')
    death_rate = dead$death_count / ((alive$live_count + dead$death_count) / 1000) # Infant Mortality Rate per 1000 births
    
    data = data.frame(mom_bmi, death_rate, stringsAsFactors = F)
    data$death_rate = round(data$death_rate, 1)
    data$mom_bmi = factor(data$mom_bmi, levels=data$mom_bmi[order(data$death_rate, decreasing = T)])
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
  
  # Health Stats -> Pre Pregancy Tobacco Use
  health_tobacco <- reactive({
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
  
  output$health_tobacco_plot <- renderPlotly({
    f = list(family = "Arial", size = 18)
    t = list(family = "Arial, sans-serif", size = 16)
    xlab = list(title = "Mother's Pre Pregnancy Tobacco Use", titlefont = f, tickfont = t)
    ylab = list(title = "Number of Infant Deaths Per 1,000 Births", titlefont = f, tickfont = t)
    
    fig <- plot_ly(health_tobacco(), y = ~mom_tobacco, x = ~death_rate, type='bar', orientation = 'h',
                   text = health_tobacco()$death_rate, textposition = 'auto',
                   marker = list(color="rgb(251,106,74)", line = list(color = "rgb(203,24,29)", width = 3)))
    fig <- fig %>% layout(xaxis = ylab)
    fig
  })
  
  # Health Stats -> Infections
  
  
  # Health Stats -> Pre-Existing Conditions
  
  
  # Improve Natality -> Infant Survival (combined gestation)
  p1 = function() {
    natall %>%
      filter(infant_living_at_report != 'U') %>% 
      group_by(infant_living_at_report,infant_breastfed_at_discharge) %>%
      summarise_at(vars(mothers_bmi,mothers_age,mothers_race,mothers_education,fathers_education),
                   list(average = mean))
  }
  
  output$bmi_living <- renderPlotly({
    plot_ly(p1(), x = ~infant_living_at_report, y = ~mothers_bmi_average, type = 'box', name = "mothers_bmi_average") %>%
      layout(barmode = 'group',
             xaxis = list(title = "Infant Living at Report"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
  })
  
  # Improve Natality -> NICU
  p3 = reactive({
    natall %>%
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
  
  # Improve Natality -> Gestation (fetal dev)
  p4 = function() {
    natall %>%
      filter(combined_gestation_wk != 99) %>% 
      group_by(combined_gestation_wk) %>%
      summarise_at(vars(mothers_bmi),
                   list(average = mean))
  }
  
  output$bmi_preme <- renderPlotly({
    plot_ly(p4(), x = ~combined_gestation_wk, name = "last Normal Menstreul Cycle") %>%
      add_trace(y = ~average, name = "Fetal Development at Birth", mode='lines+markers', type = "scatter") %>% 
      layout(xaxis = list(title = "Gestation Period (weeks)"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
  })
  
  # Improve Natality -> APGAR 5 min
  p5 = function() {
    natall %>%
      filter(APGAR_score_5min != 99) %>% 
      group_by(APGAR_score_5min, mothers_marital_status) %>%
      summarise_at(vars(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                        mothers_hispanic_origin2,fathers_race,fathers_hispanic_origin2,n_prenatal_visits,
                        mothers_bmi,last_norm_menses_mo,combined_gestation_wk,birth_weight_gm),
                   list(average = mean))
  }
  
  output$bmi_5min <- renderPlotly({
    plot_ly(p5(), x = ~APGAR_score_5min, y = ~mothers_bmi_average, type = 'box', name = "APGAR 5") %>%
      layout(barmode = 'group',
             xaxis = list(title = "APGAR 5 Min Scores"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
  })
  
  # Improve Natality -> APGAR 5 min
  p6 = function() {
    natall %>%
      filter(APGAR_score_10min != 88, APGAR_score_10min != 99) %>% 
      group_by(APGAR_score_10min,mothers_marital_status) %>%
      summarise_at(vars(mothers_education,mothers_age,fathers_education,fathers_age,mothers_race,
                        mothers_hispanic_origin2,fathers_race,fathers_hispanic_origin2,n_prenatal_visits,
                        mothers_bmi,last_norm_menses_mo,combined_gestation_wk,birth_weight_gm),
                   list(average = mean))
  }
  output$bmi_10min <- renderPlotly({
    apgar10_bmi <- plot_ly(p6(), x = ~APGAR_score_10min, y = ~mothers_bmi_average, type = 'box', name = "APGAR 10") %>%
      layout(barmode = 'group',
             xaxis = list(title = "APGAR 10 Min Scores"),
             yaxis = list(title = "Average Mother's Body Mass Index (BMI)"))
  })
  
  # Improve Natality -> Final delivery method
  
  # final_delivery_bmi18 = nat18 %>% 
  #   group_by(final_delivery_method) %>% 
  #   summarise_at(vars(mothers_bmi, pre_preg_lbs, delivery_lbs),
  #                list(average = mean))
  
  output$bmi_final_del <- renderPlotly({
    fig <- plot_ly(final_del_bmi18, x = ~final_delivery_method, y = ~mothers_bmi_average, type = 'bar', name = 'Mothers BMI')
    fig <- fig %>% add_trace(y = ~pre_preg_lbs_average, name = 'Pre Pregnancy Weight')
    fig <- fig %>% add_trace(y = ~delivery_lbs_average, name = 'Delivery Weight')
    fig <- fig %>% layout(barmode = 'group',
                          xaxis = list(title = "Final Delivery Method Used"),
                          yaxis = list(title = "Average BMI"))
    fig
  })
  
  # Improve Natality -> Prenatal Mother
  prenatal_age_mother = function() {
    data = nat18 %>% 
      group_by(mothers_age) %>% 
      summarise_at(vars(n_prenatal_visits, mothers_education, fathers_education, fathers_age),
                   list(average = mean))
    data
  }
  
  output$bmi_prenat_mother <- renderPlot({
    fig = ggplot(prenatal_age_mother(), aes(x=mothers_age, y=n_prenatal_visits_average, group=1)) +
      geom_line(linetype="dashed", color="blue", size=0.2)+
      labs(title ="Average Prenetal Visits in Relation to the Age of the Mother")+
      xlab("Mothers Age")+
      ylab("Average Prenatal Visits")+
      geom_point()
    fig
  })
  
  # # Improve Natality -> Prenatal Father
  # prenatal_age_father = function() {
  #   data = nat18 %>% 
  #     group_by(fathers_age) %>% 
  #     summarise_at(vars(n_prenatal_visits, mothers_education, mothers_age, fathers_education),
  #                  list(average = mean))
  #   data
  # }
  # 
  # output$bmi_prenat_father <- renderPlot({
  #   fig = ggplot(prenatal_age_father(), aes(x=fathers_age, y=n_prenatal_visits_average, group=1)) +
  #     geom_line(linetype="dashed", color="blue", size=0.2)+
  #     labs(title ="Average Prenetal Visits in Relation to the Age of the Father")+
  #     xlab("Fathers Age")+
  #     ylab("Average Prenatal Visits")+
  #     geom_point()
  #   fig
  # })
})
    
# Create Shiny object
shinyApp(ui = ui, server = server)
