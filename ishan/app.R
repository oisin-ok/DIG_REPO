install.packages(c("shinydashboard", "shinyWidgets", "plotly", "dplyr", "shiny", "DT", "survival", "survminer"))
update.packages(ask = FALSE)

# Set working directory
setwd("C:\\R Assignment 5")

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(survival)
library(survminer)
library(plotly)

# Load the dataset
DIG <- read.csv("DIG.csv")
DIG <- DIG %>% filter(!is.na(AGE) & !is.na(DEATH) & is.finite(AGE))

# Convert categorical variables to factors
DIG$TRTMT <- factor(DIG$TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment"))
DIG$SEX <- factor(DIG$SEX, levels = c(1, 2), labels = c("Male", "Female"))
DIG$RACE <- factor(DIG$RACE, levels = c(1, 2), labels = c("White", "Nonwhite"))

# UI Section
ui <- dashboardPage(
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
)

# Server Section
server <- function(input, output) {
  
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
  
}

# Run the App
shinyApp(ui = ui, server = server)