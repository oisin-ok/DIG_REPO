library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(survival)
library(survminer)
library(dplyr)

# Load data
# Update with your specific dataset path if not already available

dig.df <- read.csv("DIG.csv")

# Preprocess data
dig.df <- dig.df %>%
  # Ensure DEATHDAY and DEATH columns exist and are numeric
  filter(!is.na(DEATHDAY), !is.na(DEATH)) %>%
  mutate(
    # Convert DEATHDAY to months (rounding to the nearest month)
    Month = round(as.numeric(DEATHDAY) / 30),
    
    # Convert categorical variables to factors with descriptive labels
    TRTMT = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Drug")),
    SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    RACE = factor(RACE, levels = c(1, 2), labels = c("White", "Other")),
    HYPERTEN = factor(HYPERTEN, levels = c(0, 1), labels = c("No", "Yes")),
    CVD = factor(CVD, levels = c(0, 1), labels = c("No", "Yes")),
    WHF = factor(WHF, levels = c(0, 1), labels = c("No", "Yes")),
    DIG = factor(DIG, levels = c(0, 1), labels = c("No", "Yes")),
    HOSP = factor(HOSP, levels = c(0, 1), labels = c("No", "Yes")),
    DEATH = factor(DEATH, levels = c(0, 1), labels = c("Alive", "Dead")),
    
    # Convert DEATH to numeric for survival analysis
    DEATH_NUM = ifelse(DEATH == "Dead", 1, 0)
  ) %>%
  # Select and ensure all continuous variables are numeric
  mutate(across(c(AGE, BMI, KLEVEL, CREAT, DIABP, SYSBP, HOSPDAYS, DEATHDAY), as.numeric))

# Define the continuous and categorical variables for reference
cont_vars <- c("AGE", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HOSPDAYS", "DEATHDAY")
cat_vars <- c("TRTMT", "SEX", "RACE", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "DEATH")
ui <- fluidPage(
  navbarPage("Digoxin Trial Data",
             tabPanel("Participant Information",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("cont_hist_var", "Select a Continuous Variable for Histogram:", 
                                      choices = cont_vars),
                          selectInput("binary_pie_var", "Select a Binary Variable for Pie Chart:", 
                                      choices = cat_vars),
                          selectInput("summary_var", "Select a Variable for Summary Statistics:", 
                                      choices = c(cont_vars, cat_vars))
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Histograms",
                                     plotlyOutput("cont_histograms")
                            ),
                            tabPanel("Pie Charts",
                                     plotlyOutput("binary_pie_charts")
                            ),
                            tabPanel("Summary Statistics",
                                     tableOutput("summary_stats_table"),
                                     plotlyOutput("summary_boxplot")
                            )
                          )
                        )
                      )
             ),
             tabPanel("Mortality",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Kaplan-Meier survival analysis based on mortality data.")
                        ),
                        mainPanel(
                          plotOutput("km_plot")
                        )
                      )
             )
  )
)

server <- function(input, output) {
  
  ####################### PARTICIPANT INFORMATION TAB ##############################
  
  ## Continuous Variable Histograms ##
  output$cont_histograms <- renderPlotly({
    req(input$cont_hist_var)  # Ensure a variable is selected
    
    plot_ly(
      data = dig.df,
      x = ~ get(input$cont_hist_var),
      type = "histogram",
      marker = list(color = "#1f77b4")
    ) %>%
      layout(
        title = paste("Histogram of", input$cont_hist_var),
        xaxis = list(title = input$cont_hist_var),
        yaxis = list(title = "Count")
      )
  })
  
  ## Binary Variable Pie Charts ##
  output$binary_pie_charts <- renderPlotly({
    req(input$binary_pie_var)  # Ensure a variable is selected
    
    # Use sym() and count the occurrences of each binary category
    pie_data <- dig.df %>%
      filter(!is.na(!!sym(input$binary_pie_var))) %>%
      count(!!sym(input$binary_pie_var))  # Ensuring proper grouping with sym
    
    plot_ly(
      data = pie_data,
      labels = ~ get(input$binary_pie_var),  # This should be fine with dynamic input
      values = ~ n,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = c("#ff7f0e", "#1f77b4"))
    ) %>%
      layout(
        title = paste("Distribution of", input$binary_pie_var)
      )
  })
  
  ## Summary Statistics ##
  
  # Table
  output$summary_stats_table <- renderTable({
    req(input$summary_var)  # Ensure a variable is selected
    
    summary_stats <- summary(dig.df[[input$summary_var]])  # Use na.omit for summary
    data.frame(Statistic = names(summary_stats), Value = as.vector(summary_stats))
  })
  
  # Boxplot
  output$summary_boxplot <- renderPlotly({
    req(input$summary_var)  # Ensure a variable is selected
    
    # Check if the variable is numeric (for valid boxplot)
    if (is.numeric(dig.df[[input$summary_var]])) {
      plot_ly(
        data = dig.df,
        y = ~ get(input$summary_var),
        type = "box",
        marker = list(color = "#1f77b4")
      ) %>%
        layout(
          title = paste("Boxplot of", input$summary_var),
          yaxis = list(title = input$summary_var)
        )
    } else {
      NULL  # Skip boxplot if the variable is not numeric
    }
  })
  
  ####################### MORTALITY TAB ##############################
  
  # Kaplan-Meier Plot
  # Check and preprocess the data
  output$km_plot <- renderPlot({
    # Ensure the necessary columns are available
    req(dig.df$DEATHDAY, dig.df$DEATH)
    
    # Filter out missing or invalid data
    dig.df <- dig.df[!is.na(dig.df$DEATHDAY) & !is.na(dig.df$DEATH), ]
    
    # Convert DEATHDAY to numeric and DEATH to binary (if needed)
    dig.df$DEATHDAY <- as.numeric(dig.df$DEATHDAY)
    dig.df$DEATH <- as.numeric(dig.df$DEATH)
    
    # Check if DEATHDAY and DEATH have valid data
    validate(
      need(all(dig.df$DEATH %in% c(0, 1)), "DEATH must contain only 0 or 1 values."),
      need(all(dig.df$DEATHDAY >= 0), "DEATHDAY must contain non-negative values.")
    )
    
    # Create the Surv object for Kaplan-Meier analysis
    surv_obj <- Surv(time = dig.df$DEATHDAY, event = dig.df$DEATH_NUM)
    
    # Fit the survival model
    km_fit <- survfit(surv_obj ~ 1, data = dig.df)
    
    # Kaplan-Meier plot
    ggsurvplot(
      km_fit,
      data = dig.df,
      risk.table = TRUE,
      xlab = "Days",
      ylab = "Survival Probability",
      title = "Kaplan-Meier Survival Curve"
    )
    
  })
  
  
}

shinyApp(ui = ui, server = server)
