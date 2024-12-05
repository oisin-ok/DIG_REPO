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
library(table1)
library(plotly)

# Variable Lists

# Categorical Variables
baseline_vars <- c("TRTMT", "AGE", "SEX", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP")
cat_vars <- c("TRTMT", "SEX", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "DEATH")
cont_vars <- c("AGE", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HOSPDAYS", "DEATHDAY")




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Navbar page
  navbarPage("Digoxin Trial Data",
             
             #PAGE 1
             tabPanel("Participant Information",
                      sidebarLayout(
                        
                        #sidebar - input control, select demographic variable
                        sidebarPanel(
                          selectInput("dem_var", "Choose a variable to view its distribution:", choices = c("a", "b", "c"))
                        ), 
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                                  tabsetPanel(
                                              tabPanel("Summary Statistics"
                                                        
                                                        
                                                        
                                                      ), # tab1 close
                                              
                                              tabPanel("Summary Statistics"
                                                       
                                                       
                                                       
                                                      ) # tab2 close
                                              ) # tabset close
                                  ) # main close
                        ###
                        
                      ) # inner sidebar Layout close
             ), # navtab 1 close
             
             #PAGE 2
             tabPanel("Baseline Variables",
                      sidebarLayout(
                        
                        #sidebar - input control, select a variable to see its effect on mortality
                        sidebarPanel(
                          selectInput("base_var1", "Choose a variable to compare baseline values:", choices = baseline_vars, selected = "AGE"),
                          selectInput("base_var2", "Choose a variable to compare across:", choices = cat_vars)
                        ),
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                                  tabsetPanel(
                                              tabPanel("Summary Statistics",
                                                       uiOutput("table_baseline_compare")
                                                       
                                              ), # tab1 close
                                              
                                              tabPanel("Summary Statistics",
                                                       
                                                       plotOutput("boxplot_baseline_compare")
                                                       
                                              ) # tab2 close
                                  ) # tabset close
                          
                        ) # main close
                        ###
                        
                                    ) #  inner sidebar Layout close
                      ),# navtab close
             
             #PAGE 3         
             tabPanel("Mortality",
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
                        
                      ) # inner sidebar Layout close
             ) # nav_tab 3 close
             
             
             
             
            ) #navbar close
)# UI close

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

  
  # Baseline Variables Outputs
  reactive({
    
  })
  
  # BASELINE COMPARISONS - TAB 2  
  
  output$table_baseline_compare <- renderUI({
    var1 <- input$base_var1
    var2 <- input$base_var2
    
    formula <- as.formula(paste("~", input$base_var1, "|", input$base_var2))
    
    table1(formula, data = dig.df) # generate table1 with two input variables # maybe make this able to select multiple variables in rows later?
  })

  
  # Sort of works, needs to be labelled and made interactive with hover tooltips. Also don't forget to remove KLEVEL outlier.
  bvar_1 <- reactive({input$base_var1})
  bvar_2 <- reactive({input$base_var2})
  
  output$boxplot_baseline_compare <- renderPlot({
    dig.df %>%
      select(bvar_1(), bvar_2()) %>%
      na.omit() %>%
      ggplot(aes_string(x = bvar_2(), y = bvar_1(), fill = bvar_2())) +
      geom_boxplot(tooltip = c("x", "y", "text"))
  })
  
  output$plot1 <- renderPlot({ 
    static_compare_baseline <- dig.df %>%
      select()
      filter(species == input$species) %>%
      filter(sex == input$sex) %>%
      filter(body_mass_g >= input$bmass[1] & body_mass_g <= input$bmass[2]) %>%
      filter(year == input$year) %>%
      ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
      geom_point()
      
  })
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
