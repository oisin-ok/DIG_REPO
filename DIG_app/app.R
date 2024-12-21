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
library(DT)
library(survival)
library(survminer)

#Data Cleaning
#load in data
dig.df <- read.csv("DIG.csv")

#select variables
dig.df <- dig.df %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)

#calculate month variable
dig.df <- dig.df %>%
  mutate(Month = round(DEATHDAY/30))

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


#remove KLEVEL outlier
dig.df$KLEVEL[dig.df$KLEVEL > 100] <- NA


#mortality data frame
mort_month<-summary(survfit(Surv(Month, DEATH) ~ 1, data = dig.df))

mortality.df <- data.frame(Month = mort_month$time, # Extract month
                           Enrolled = mort_month$n.risk[,1], # Extract number enrolled at beginning of each month
                           Deaths = mort_month$n.event[,2], # Extract number of people who died (finished trial and died)
                           Cum_Prob_Death = mort_month$pstate[,2]) # Extract the cumulative probability of death at each month

mortality.df <- mutate(mortality.df, Monthly_Risk = Deaths/Enrolled)




# Variable Lists

# Categorical Variables
baseline_vars <- c("TRTMT", "AGE", "SEX", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP")
cat_vars <- c("TRTMT", "SEX", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "DEATH")
cont_vars <- c("AGE", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HOSPDAYS", "DEATHDAY")




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Navbar page
  navbarPage("Digoxin Trial Data",
             
             
             #PAGE 0
             tabPanel("Demographics",
                     
                        dashboardHeader(title = "DIG Trial Dashboard"),
                        dashboardSidebar(
                          sidebarMenu(
                            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                            menuItem("Demographics", tabName = "demographics", icon = icon("users")),
                            menuItem("Clinical Measures", tabName = "clinical", icon = icon("heartbeat")),
                            menuItem("Outcomes", tabName = "outcomes", icon = icon("bar-chart"))
                          )
                        ),
                        dashboardBody(
                          tabItems(
                            # Overview Tab
                            tabItem(tabName = "overview", 
                                    h2("Overview of DIG Trial Data"),
                                    fluidRow(
                                      box(title = "Dataset Overview", width = 12, status = "primary", solidHeader = TRUE,
                                          dataTableOutput("overviewTable"))
                                    )
                            ),
                            
                            # Demographics Tab
                            tabItem(tabName = "demographics",
                                    h2("Demographic Analysis"),
                                    h4("Use the filters to explore age and race distributions."),
                                    fluidRow(
                                      # Filter Options Box
                                      box(title = "Filter Options", width = 4, collapsible = TRUE, status = "info",
                                          sliderInput("ageRange", "Age Range:", 
                                                      min = min(DIG$AGE), max = max(DIG$AGE), 
                                                      value = c(min(DIG$AGE), max(DIG$AGE))),
                                          checkboxGroupInput("genderFilter", "Select Gender:", 
                                                             choices = levels(DIG$SEX), 
                                                             selected = levels(DIG$SEX)),
                                          checkboxGroupInput("raceFilter", "Select Race:", 
                                                             choices = levels(DIG$RACE), 
                                                             selected = levels(DIG$RACE))
                                      ),
                                      
                                      # Plots for Demographics
                                      box(title = "Age Distribution", width = 8, status = "primary", solidHeader = TRUE,
                                          plotlyOutput("agePlot", height = "400px")),
                                      box(title = "Race Distribution", width = 8, status = "primary", solidHeader = TRUE,
                                          plotlyOutput("racePlot", height = "400px"))
                                    )
                            ),
                            
                            # Clinical Measures Tab
                            tabItem(tabName = "clinical",
                                    h2("Clinical Measures"),
                                    h4("Analyze BMI distribution and blood pressure trends."),
                                    fluidRow(
                                      box(title = "BMI Distribution", width = 6, status = "primary", solidHeader = TRUE,
                                          plotOutput("bmiPlot", height = "400px")),
                                      box(title = "Blood Pressure Analysis", width = 6, status = "primary", solidHeader = TRUE,
                                          plotOutput("bpPlot", height = "400px"))
                                    )
                            ),
                            
                            # Outcomes Tab
                            tabItem(tabName = "outcomes",
                                    h2("Outcomes"),
                                    h4("Explore mortality by treatment, summary statistics, and survival analysis."),
                                    tabBox(
                                      title = "Outcomes Analysis",
                                      width = 12,
                                      tabPanel("Mortality by Treatment", plotOutput("deathPlot", height = "400px")),
                                      tabPanel("Summary Table", dataTableOutput("summaryTable")),
                                      tabPanel("Survival Analysis", plotOutput("survivalPlot", height = "400px"))
                                    )
                            )
                          )
                        
                      )
                      
             ), # navtab 0 close
             
             
             
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
                          selectInput("base_var2", "Choose a variable to compare across:", choices = cat_vars) # make this optional? so if nothing selected, single box plot summarising variable
                        ),
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                                  tabsetPanel(
                                              tabPanel("Summary Statistics",
                                                       uiOutput("table_baseline_compare")
                                                       
                                              ), # tab1 close
                                              
                                              tabPanel("Visualistion",
                                                       
                                                       plotlyOutput("boxplot_baseline_compare_plotly")
                                                       
                                              ) # tab2 close
                                  ) # tabset close
                          
                        ) # main close
                        ###
                        
                                    ) #  inner sidebar Layout close
                      ),# navtab close
             
             
             #PAGE 3        - mosaic plots between variables and split over groups
             tabPanel("Associations",
                      sidebarLayout(
                        
                        #sidebar - input control
                        sidebarPanel(
                          
                        ), 
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                          
                          
                        )
                        ###
                        
                      ) # inner sidebar Layout close
             ), # nav_tab 3 close
             
             
             
             #PAGE 4         
             tabPanel("Mortality",
                      sidebarLayout(
                        
                        #sidebar - input control, select a baseline measurement to compare across groups
                        sidebarPanel(
                          radioButtons("mortality_type", "Select mortality statistic to view:", c("Absolute_Deaths", "Cumulative_Risk", "Instantaneous Risk", "Kaplan_Meir"))
                        ), 
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                          plotOutput("km_plot")
                          
                        )
                        ###
                        
                      ) # inner sidebar Layout close
             ), # nav_tab 4 close
             
             
             #PAGE 5         
             tabPanel("Explore the Data",
                     
                      fluidPage(
                        titlePanel("Digitalis Investigation Group Trial"),
                      
                                fluidRow(
                                  column(3,
                                         radioButtons("trtmt_button",
                                                     "Treatment:",
                                                     c("Any", unique(as.character(dig.df$TRTMT))))
                                         ),
                                  column(3,
                                         radioButtons("sex_button",
                                                     "Sex:",
                                                     c("Any", unique(as.character(dig.df$SEX))))
                                         ),
                                  column(3,
                                         radioButtons("death_button",
                                                      "Status:",
                                                      c("All", unique(as.character(dig.df$DEATH))))
                                         ),
                                  column(3,
                                         checkboxGroupInput("hosp_buttons", "Select Hospitalisation Types",
                                                            choices = c("CVD", "WHF", "DIG"),
                                                            selected = NULL),
                                          )
                                          ), # fluid row close
                                # Create a new row for the table.
                                DT::dataTableOutput("table_dt") #need to make the units/variable names clearer, and address the binary classifier under hospitalisation types

                        #Maybe make one of those line plots for every variable we saw in the lectures to visualise the selection?
                        
                        
                                ) # fluidPage close
             
             
             
                      ) #nav_tab 5 close
            ) #navbar close

)# UI close


server <- function(input, output) {
  ####################### SERVER CODE TAB 0 ##################################################################################################
    
    # Overview: Display the dataset
    output$overviewTable <- renderDataTable({
      datatable(DIG, options = list(pageLength = 10))
    })
    
    # Demographics: Age Distribution
    output$agePlot <- renderPlotly({
      filtered_data <- DIG %>% 
        filter(AGE >= input$ageRange[1], AGE <= input$ageRange[2],
               SEX %in% input$genderFilter, RACE %in% input$raceFilter)
      
      validate(
        need(nrow(filtered_data) > 0, "No data available for the selected filters.")
      )
      
      # Create the ggplot object
      p <- ggplot(filtered_data, aes(x = AGE)) +
        geom_histogram(binwidth = 5, fill = "blue", color = "black") +
        labs(title = "Age Distribution", x = "Age", y = "Frequency") +
        theme_minimal()
      
      # Convert ggplot object to plotly
      ggplotly(p)
    })
    
    # Demographics: Race Distribution
    output$racePlot <- renderPlotly({
      filtered_data <- DIG %>% 
        filter(SEX %in% input$genderFilter, RACE %in% input$raceFilter)
      
      validate(
        need(nrow(filtered_data) > 0, "No data available for the selected filters.")
      )
      
      # Create the ggplot object
      p <- ggplot(filtered_data, aes(x = RACE, fill = RACE)) +
        geom_bar() +
        labs(title = "Race Distribution", x = "Race", y = "Count") +
        scale_fill_manual(values = c("blue", "green")) +
        theme_minimal()
      
      # Convert ggplot object to plotly
      ggplotly(p)
    })
    
    # Clinical Measures: BMI Distribution
    output$bmiPlot <- renderPlot({
      ggplot(DIG, aes(x = BMI)) +
        geom_histogram(binwidth = 1, fill = "orange", color = "black") +
        labs(title = "BMI Distribution", x = "BMI", y = "Frequency") +
        theme_minimal()
    })
    
    # Clinical Measures: Blood Pressure Analysis
    output$bpPlot <- renderPlot({
      filtered_data <- DIG %>% filter(!is.na(SYSBP) & !is.na(DIABP) & 
                                        is.finite(SYSBP) & is.finite(DIABP))
      
      ggplot(filtered_data, aes(x = SYSBP, y = DIABP)) +
        geom_point(color = "red", alpha = 0.6) +
        labs(title = "Systolic vs Diastolic Blood Pressure", 
             x = "Systolic BP", y = "Diastolic BP") +
        theme_minimal()
    })
    
    # Outcomes: Mortality by Treatment
    output$deathPlot <- renderPlot({
      ggplot(DIG, aes(x = TRTMT, fill = as.factor(DEATH))) +
        geom_bar(position = "dodge") +
        labs(title = "Mortality by Treatment", x = "Treatment", y = "Count") +
        scale_fill_manual(values = c("red", "green"), name = "Death (1=Yes, 0=No)") +
        theme_minimal()
    })
    
    # Outcomes: Summary Table
    output$summaryTable <- renderDataTable({
      DIG %>%
        group_by(TRTMT) %>%
        summarise(
          Total = n(),
          Deaths = sum(DEATH, na.rm = TRUE),
          DeathRate = round(mean(DEATH, na.rm = TRUE) * 100, 2)
        )
    })
    
    # Outcomes: Kaplan-Meier Survival Analysis
    output$survivalPlot <- renderPlot({
      surv_fit <- survfit(Surv(DEATHDAY, DEATH) ~ TRTMT, data = DIG)
      
      ggsurvplot(surv_fit, data = DIG, pval = TRUE, conf.int = TRUE,
                 legend.labs = c("Placebo", "Treatment"),
                 title = "Kaplan-Meier Survival Curve",
                 xlab = "Days", ylab = "Survival Probability")
    })
    
  
  
  ####################### SERVER CODE TAB 1 ##################################################################################################
  
  
  
  
  ####################### SERVER CODE TAB 2 ##################################################################################################
  
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

  

  # selected variables
  bvar_1 <- reactive({input$base_var1})
  bvar_2 <- reactive({input$base_var2})
  
  # box plots
  output$boxplot_baseline_compare_plotly <- renderPlotly({
    dig.df %>%
      select(bvar_1(), bvar_2()) %>%
      na.omit() %>%
      plot_ly(
        x = ~ .[[bvar_2()]],
        y = ~ .[[bvar_1()]],
        type = "box",
        color = ~ .[[bvar_2()]],
        colors = c("red", "blue")
      ) %>%
      layout(
        title = paste("Comparison of", bvar_1(), "across", bvar_2()),
        xaxis = list(title = bvar_2()),
        yaxis = list(title = bvar_1())
      )
    
  })
  
  ####################### SERVER CODE TAB 3 ##################################################################################################
  
  
  ####################### SERVER CODE TAB 4 ##################################################################################################
  
  #km_plot - not working
    output$km_plot <- renderPlot({
      
      
      km_fit <- survfit(Surv(Month, Deaths) ~ 1, data = mortality.df)
      
      ggsurvplot(km_fit, data = mortality.df, risk.table = TRUE)
      
      
    })
  
  
  
  
  ####################### SERVER CODE TAB 5 ##################################################################################################
  
  # Filter data based on selections
  output$table_dt <- DT::renderDataTable(DT::datatable({
    data <- dig.df
    if (input$trtmt_button != "Any") {
      data <- data[data$TRTMT == input$trtmt_button,]
    }
    if (input$sex_button != "Any") {
      data <- data[data$SEX == input$sex_button,]
    }
    if (input$death_button != "All") {
      data <- data[data$DEATH == input$death_button,]
    }
    
    
    
    hosp_columns <- c("CVD", "WHF", "DIG")
    data[hosp_columns] <- lapply(data[hosp_columns], function(x) ifelse(x == "Yes", 1, 0))
    
    
    if (!is.null(input$hosp_buttons) && length(input$hosp_buttons) > 0) { # if something is selected:
      data <- data[rowSums(data[, input$hosp_buttons, drop = FALSE]) == length(input$hosp_buttons), ] # filter for rows where sum of the selected columns == the number of selected columns 
    } else {
      data
    }
  },  rownames = FALSE,
      colnames = c("ID", "Treatment", "Age (Years)", "Sex", "BMI", "Serum Potassium (mmol/L)", "Serum Creatinine (mg/dL)",
                   "Diastolic BP (mmHg)", "Systolic BP (mmHg)", "Hypertension History", "Hospitalisation: Cardiovascular Disease (1=Yes)",
                   "Hospitalisation: Worsening Heart Failure (1=Yes)", "Hospitalisation: Digoxin Toxicity (1=Yes)", "Hospitalisation (Any)",
                   "Time till First Hospitalisation (Days)", "Status", "Time till Last Followup (Days)", "Time till Last Followup (Months)"),
      caption = "Source: Digitalis Investigation Group Trial" ))
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
