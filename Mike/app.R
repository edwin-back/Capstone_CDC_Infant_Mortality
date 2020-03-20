# Load packages#######################################################################
######################################################################################
######################################################################################
library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(readr)

library(data.table)
library(splitstackshape)
library(zoo)
library(DT)
library(stats)
library(base)
library(ExPanDaR)
library(ggplot2)
library(rsconnect)
#############################             IMPORT OF RAW DATA    ######################
######################################################################################
######################################################################################
#LOCAL Play????
#setwd("/Users/baptistasmokas/Desktop/Shiny_Project/shiny_app")

#############################                 PREPROCESSING      #####################
######################################################################################
######################################################################################

###### FOR THE APP WE NEED TO ADD A NEW ROW INSIDE OUR DATAFRAME   ###################
# Load data

# trend_description <- read_csv("data/trend_description.csv")
# Select whether to overlay smooth trend line

rsconnect::setAccountInfo(name='mikelim91',
                          token='E4388F626B940A3844945D6982FC351A',
                          secret='3El4/BUHlwayvJl/rvgvCyhWxv+DXTYt7jrRsruV')


ui <- fluidPage(theme = shinytheme("sandstone"),   #slate
                titlePanel("CAPSTONE PROJECT : Improving Infant Natality & Mortality Rates", "Baptiste Mokas - Shiny App"),
                navbarPage("-",
                           
                           tabPanel("Product",
                                    img(src='nycdsa.png', height="5%", align = "right"),
                                    mainPanel(
                                      HTML(paste("Michael Lim, Jason Hoffmeier, Edwin Back, Baptiste Mokas", "- A NYCDSA PROJECT", '<br/>', "2020", '<br/>', "_", '<br/>')),
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Product Description",
                                                           HTML(
                                                             paste(
                                                             '<br/>',
                                                             "Target Audience:",'<br/>',
                                                             "Mothers",'<br/>',
                                                             '<br/>',
                                                             "Objective:",'<br/>',
                                                             "Improve Natality & Reduce Mortality based on various health & life-style choices ",'<br/>'
                                                                )
                                                             )),
                                                  
                                                  
                                                  tabPanel("Mother's General Information",
                                                           HTML(
                                                             paste(
                                                               '<br/>',
                                                               "Input Field",
                                                               '<br/>',
                                                               '<br/>',
                                                               "Education Level",
                                                               '<br/>',
                                                               "Age",
                                                               '<br/>',
                                                               "Race"
                                                             )
                                                           )),
                                                  
                                                  tabPanel("Mother's Personal Health Information",
                                                           HTML(
                                                             paste(
                                                               '<br/>',
                                                               "Input Field",
                                                               '<br/>',
                                                               '<br/>',
                                                               "Health",
                                                               '<br/>',
                                                               "Smoker/Non-Smoker",
                                                               '<br/>',
                                                               "Disease/No Disease",
                                                               '<br/>',
                                                               "Height",
                                                               '<br/>',
                                                               "Weight",
                                                               '<br/>',
                                                               "BMI if known"
                                                             )
                                                           )),
                                                  
                                                  tabPanel("Advice & Results",
                                                           HTML(
                                                             paste(
                                                               '<br/>',
                                                               "Insights",
                                                               '<br/>',
                                                               '<br/>',
                                                               "Model",
                                                               '<br/>',
                                                               "Clusters",
                                                               '<br/>',
                                                               "Logistic Regression",
                                                               '<br/>',
                                                               "BoxPlots",
                                                               '<br/>',
                                                               "distribution plots",
                                                               '<br/>',
                                                               '<br/>',
                                                               "Random Facts",
                                                               '<br/>',
                                                               "Facts that pertain to person within specific subpopulation/cluster"
                                                               
                                                             )
                                                           ))
                                      )
                                    )
                                    
                                    
                           ), 
                           
                           
                           tabPanel("RAW DATA", 
                                    mainPanel(
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Tab test", 
                                                           HTML(paste(
                                                             '<br/>',
                                                             
                                                             "test 1?",'<br/>',
                                                             "test 2?",'<br/>',
                                                             "test 3 ?",'<br/>',
                                                             '<br/>',
                                                             "test 4?",'<br/>',
                                                             "test5 ?",'<br/>',
                                                             '<br/>',
                                                             "test6 ?"
                                                           )
                                                           )
                                                          ),
                                                  tabPanel("Tab test",
                                                           HTML(paste(
                                                             '<br/>',
                                                             
                                                             "test 1?",'<br/>',
                                                             "test 2?",'<br/>',
                                                             "test 3 ?",'<br/>',
                                                             '<br/>',
                                                             "test 4?",'<br/>',
                                                             "test5 ?",'<br/>',
                                                             '<br/>',
                                                             "test6 ?"
                                                           )
                                                           )),
                                                  tabPanel("Tab test", 
                                                           tabsetPanel(type = "tabs", 
                                                                       
                                                                       tabPanel("Tab test"
                                                                                ),
                                                                       
                                                                       tabPanel("Tab test")
                                                                       ) 
                                                           
                                                          ),
                                                  
                                                  tabPanel("tab",
                                                          tabsetPanel(type = "tabs",
                                                                       tabPanel("test2", 
                                                                                HTML(paste(
                                                                                  '<br/>',
                                                                                  
                                                                                  "test 1?",'<br/>',
                                                                                  "test 2?",'<br/>',
                                                                                  "test 3 ?",'<br/>',
                                                                                  '<br/>',
                                                                                  "test 4?",'<br/>',
                                                                                  "test5 ?",'<br/>',
                                                                                  '<br/>',
                                                                                  "test6 ?"
                                                                                )
                                                                                )),
                                                                      tabPanel("test 6", 
                                                                               HTML(paste(
                                                                                 '<br/>',
                                                                                 
                                                                                 "test 1?",'<br/>',
                                                                                 "test 2?",'<br/>',
                                                                                 "test 3 ?",'<br/>',
                                                                                 '<br/>',
                                                                                 "test 4?",'<br/>',
                                                                                 "test5 ?",'<br/>',
                                                                                 '<br/>',
                                                                                 "test6 ?"
                                                                               )
                                                                               )),
                                                                      tabPanel("test8", 
                                                                               HTML(paste(
                                                                                 '<br/>',
                                                                                 
                                                                                 "test 1?",'<br/>',
                                                                                 "test 2?",'<br/>',
                                                                                 "test 3 ?",'<br/>',
                                                                                 '<br/>',
                                                                                 "test 4?",'<br/>',
                                                                                 "test5 ?",'<br/>',
                                                                                 '<br/>',
                                                                                 "test6 ?"
                                                                               )
                                                                               )),
                                                                      tabPanel("Ttestr."),
                                                                      tabPanel(".."),
                                                                      tabPanel("test")
                                                        )
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  )
                                            )
                                    )
                                    ),
                                    
                           
                           
                           tabPanel("METHODOLOGY", 
                                    HTML(paste(
                                      '<br/>',
                                      
                                      "test 1?",'<br/>',
                                      "test 2?",'<br/>',
                                      "test 3 ?",'<br/>',
                                      '<br/>',
                                      "test 4?",'<br/>',
                                      "test5 ?",'<br/>',
                                      '<br/>',
                                      "test6 ?"
                                    )
                                    )),
                           
                           
                           
                           
                           tabPanel("EXPLORATORY DATA ANALYSIS",  
                                    plotOutput(outputId = "lineplot3", height = "630px")),
                                    
                           
                                                          
                                                          
                                                
                           tabPanel("MODELING 1",
                                    HTML(paste(
                                      '<br/>',
                                      
                                      "test 1?",'<br/>',
                                      "test 2?",'<br/>',
                                      "test 3 ?",'<br/>',
                                      '<br/>',
                                      "test 4?",'<br/>',
                                      "test5 ?",'<br/>',
                                      '<br/>',
                                      "test6 ?"
                                    )
                                    )
                  ),
                  tabPanel("MODELING 2", 
                           
                           HTML(paste(
                             '<br/>',
                             
                             "test 1?",'<br/>',
                             "test 2?",'<br/>',
                             "test 3 ?",'<br/>',
                             '<br/>',
                             "test 4?",'<br/>',
                             "test5 ?",'<br/>',
                             '<br/>',
                             "test6 ?"
                           )
                           )),
                tabPanel("PERSPECTIVE ON THE DATASET", 
                         HTML(paste(
                           '<br/>',
                           
                           "test 1?",'<br/>',
                           "test 2?",'<br/>',
                           "test 3 ?",'<br/>',
                           '<br/>',
                           "test 4?",'<br/>',
                           "test5 ?",'<br/>',
                           '<br/>',
                           "test6 ?"
                         )
                         )
                         ),
                
                tabPanel("CONCLUSION", 
                         HTML(paste(
                           '<br/>',
                           
                           "test 1?",'<br/>',
                           "test 2?",'<br/>',
                           "test 3 ?",'<br/>',
                           '<br/>',
                           "test 4?",'<br/>',
                           "test5 ?",'<br/>',
                           '<br/>',
                           "test6 ?"
                         )
                         )
                ),
                
                
                tabPanel("About / GitHub",
                         "Baptiste Mokas",
                         
                         
                         HTML(paste('<br/>',
                                    h4("baptiste.mokas@gmail.com"),'<br/>',
                                    "This application is a project conducted during the NYC DATASCIENCE ACADEMY bootcamp in 2020",
                                    h2("Thank you")
                                    
                                    ))
                        
                         
                         )
            )
)          
                
                
# Define server function
server <- function(input, output) {
  

  

  # Create scatterplot object the plotOutput function is expecting


  

  
  url <- a("Google Homepage", href="https://www.google.com/")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })

}

# Create Shiny object
shinyApp(ui = ui, server = server)

