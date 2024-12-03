#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Navbar page
  navbarPage("Digoxin Trial Data",
             
             #PAGE 1
             tabPanel("Participant Information",
                      sidebarLayout(
                        
                        #sidebar - input control, select demographic variable
                        sidebarPanel(
                          selectInput("dem_var", "Choose a variable to view its distribution:", c("a", "b", "c"))
                        ), 
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                          
                          
                        )
                        ###
                        
                      )
             ),
             
             
             #PAGE 2         
             tabPanel("Baseline Variables",
                      sidebarLayout(
                        
                        #sidebar - input control, select a baseline measurement to compare across groups
                        sidebarPanel(
                          selectInput("base_var", "Choose a variable to compare baseline values:", c("a", "b", "c"))
                        ), 
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                          
                          
                        )
                        ###
                        
                      )
             ),
             
             #PAGE3
             tabPanel("Mortality",
                      sidebarLayout(
                        
                        #sidebar - input control, select a variable to see its effect on mortality
                        sidebarPanel(
                          selectInput("base_var", "Choose a variable to compare baseline values:", names(dig.df))
                        ),
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                          
                          
                        )
                        ###
                        
                      )
             )
             
             
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #load in data
  dig.df <- read.csv("DIG.csv")
  
  #select variables
  dig.df <- dig.df %>%
    select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)
  
  #set variable classes
  dig.df$TRTMT <- as.factor(dig.df$TRTMT)
  dig.df$SEX <- as.factor(dig.df$SEX)
  dig.df$HYPERTEN <- as.factor(dig.df$HYPERTEN)
  dig.df$CVD <- as.factor(dig.df$CVD)
  dig.df$WHF <- as.factor(dig.df$WHF)
  dig.df$DIG <- as.factor(dig.df$DIG)
  dig.df$HOSP <- as.factor(dig.df$HOSP)
  dig.df$DEATH <- as.factor(dig.df$DEATH)
  
  #rename factor levels
  levels(dig.df$SEX) <- c("Male", "Female")
  levels(dig.df$TRTMT) <- c("Placebo", "Drug")
  levels(dig.df$HYPERTEN) <- c("No", "Yes") # History of Hypertension
  levels(dig.df$CVD) <- c("No", "Yes") # Hospitalisation due to Cardiovascular Disease
  levels(dig.df$WHF) <- c("No", "Yes") # Hospitalisation due to Worsening Heart Failure
  levels(dig.df$DIG) <- c("No", "Yes") # Hospitalisation due to Digoxin Toxicity
  levels(dig.df$HOSP) <- c("No", "Yes") # Hospitalisation (any)
  levels(dig.df$DEATH) <- c("Alive", "Dead") # Vital status of patient
}

# Run the application 
shinyApp(ui = ui, server = server)
