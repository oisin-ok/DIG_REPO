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
library(ggplot2)

#Data Cleaning
#load in data
dig.df <- read.csv("DIG.csv")

#select variables
dig.df <- dig.df %>%
  select(ID, TRTMT, AGE, SEX, RACE, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)

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
dig.df$RACE <- as.factor(dig.df$RACE)

#rename factor levels
levels(dig.df$SEX) <- c("Male", "Female")
levels(dig.df$TRTMT) <- c("Placebo", "Drug")
levels(dig.df$HYPERTEN) <- c("No", "Yes") # History of Hypertension
levels(dig.df$CVD) <- c("No", "Yes") # Hospitalisation due to Cardiovascular Disease
levels(dig.df$WHF) <- c("No", "Yes") # Hospitalisation due to Worsening Heart Failure
levels(dig.df$DIG) <- c("No", "Yes") # Hospitalisation due to Digoxin Toxicity
levels(dig.df$HOSP) <- c("No", "Yes") # Hospitalisation (any)
levels(dig.df$DEATH) <- c("Alive", "Dead") # Vital status of patient
levels(dig.df$RACE) <- c("White", "Non-white") # Patient ethnicity

#remove KLEVEL outlier
dig.df$KLEVEL[dig.df$KLEVEL > 100] <- NA


# Variables Categories
every_var <- c("TRTMT", "SEX", "RACE", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "DEATH", "AGE", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HOSPDAYS", "DEATHDAY")
baseline_vars <- c("TRTMT", "AGE", "SEX", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP")
cat_vars <- c("TRTMT", "SEX", "RACE", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "DEATH")
cont_vars <- c("AGE", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HOSPDAYS", "DEATHDAY")


###################################################### SHINY #########################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Navbar page
  navbarPage("Digoxin Trial Data",
             
             #PAGE 1
                 tabPanel("Participant Information",
                      tabsetPanel(
                        tabPanel("Continuous Variables",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("cont_hist_var", "Select a Continuous Variable for Histogram:", choices = cont_vars),
                              h4("Variable Codebook"),
                              tableOutput("codebook_table_cont_1")
                            ),
                            
                            mainPanel(
                              
                                       plotlyOutput("cont_histograms"),
                                       htmlOutput("histo_caption"),
                                       div(style = "height: 40px;"), # divider for space
                                       plotlyOutput("summary_boxplot"),
                                       htmlOutput("one_box_caption"),
                                       div(style = "height: 40px;"), # divider for space
                                       tableOutput("summary_stats_table_cont")
                              )
                            )
                          ),
                        
                        
                        tabPanel("Categorical Variables",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("binary_pie_var", "Select a Binary Variable for Pie Chart:", choices = cat_vars),
                                     h4("Variable Codebook"),
                                     tableOutput("codebook_table_bin_3")
                                   ),
                                   mainPanel(
                                     plotlyOutput("binary_pie_charts"),
                                     htmlOutput("pie_caption"),
                                     div(style = "height: 40px;"), # divider for space
                                     tableOutput("summary_stats_table_cat")
                                   )
                                 )
                                 )
                        
                        )
             ), # navtab 1 close
             
             #PAGE 2
             tabPanel("Baseline Variables",
                tabsetPanel(
                  tabPanel("Mixed Compare (Boxplot)",
                      
                      
                      sidebarLayout(
                        
                        #sidebar - input control, select a variable to see its effect on mortality
                        sidebarPanel(
                          selectInput("base_var1", "Choose a continuous variable to compare baseline values:", choices = cont_vars, selected = "AGE"),
                          selectInput("base_var2", "Choose a condition to compare across:", choices = cat_vars), # make this optional? so if nothing selected, single box plot summarising variable
                          h4("Variable Codebook"),
                          tableOutput("codebook_ui_1")
                        ),
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                                   uiOutput("baseline_table_title_1"),
                                   
                                   plotlyOutput("boxplot_baseline_compare_plotly"),
                                   
                                   htmlOutput("box_caption"),
                                   
                                   uiOutput("table_baseline_compare_1")
                        ) # main close
                        ###
                        
                                    ) #  inner sidebar Layout close
                            ), #tab panel inner 1 close
                  
                  tabPanel("Continuous Compare (Scatter)",
                           
                           sidebarLayout(
                             
                             #sidebar - input control, select a variable to see its effect on mortality
                             sidebarPanel(
                               selectInput("scatter_var1", "Choose a continuous variable to compare baseline values (Y):", choices = cont_vars, selected = "AGE"),
                               selectInput("scatter_var2", "Choose a continuous variable to compare across (X):", choices = cont_vars, selected = "BMI"),
                               selectInput("scatter_colour", "Choose a variable for colouring points:", choices = c("None" = "", cat_vars)),
                               h4("Variable Codebook"),
                               tableOutput("codebook_ui_2")
                             ),
                             ###
                             
                             #main panel - outputs
                             mainPanel(
                               #visualisation
                               uiOutput("baseline_table_title_2"),
                               tableOutput("table_baseline_compare_2"),
                               plotlyOutput("scatter_baseline_compare_plotly"),
                               htmlOutput("scatter_caption"),
                               
                               #summary stats
                               h4("Summary Statistics for Selected Variables"),
                               tableOutput("summary_table")
                             ) # main close
                             ###
                             
                           )
                           ),
                  
                  
                  tabPanel("Association Compare (Stacked Bar)",
                           sidebarLayout(
                             
                             #sidebar - input control, select a variable to see its effect on mortality
                             sidebarPanel(
                               selectInput("mosaic_var1", "Choose a binary variable (X):", choices = cat_vars, selected = "TRTMT"),
                               selectInput("mosaic_var2", "Choose a binary variable (Colour):", choices = cat_vars, selected = "SEX"),
                               h4("Variable Codebook"),
                               tableOutput("codebook_table_bin_1")
                             ),
                             ###
                             
                             #main panel - outputs
                             mainPanel(
                               uiOutput("baseline_table_title_3"),
                               tableOutput("table_baseline_compare_3"),
                               plotlyOutput("mosaic_baseline_compare_plotly"),
                               htmlOutput("mosaic_caption")
                             ) # main close
                             ###
                             
                           )
                           )
                  
                  
                  
                  
                          ) #tabset panel close
                      ),# navtab close
             
             
             #PAGE 4         
             tabPanel("Mortality",
                      sidebarLayout(
                        
                        #sidebar - input control, select a baseline measurement to compare across groups
                        sidebarPanel(
                         selectInput("KM_split", "Select a stratification variable:",
                                     choices = c("None", "TRTMT", "SEX", "RACE", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP"),
                                     selected = "None"),
                         
                         # Checkbox to toggle censor points
                         checkboxInput("show_censor", "Show Censor Points", value = TRUE),
                         
                         # Checkbox to toggle grid background
                         checkboxInput("show_grid", "Show Grid", value = TRUE),
                         
                         #codebook
                         h4("Variable Codebook"),
                         tableOutput("codebook_table_bin_2")
                         
                        ), 
                        ###
                        
                        #main panel - outputs
                        mainPanel(
                          uiOutput("KM_title"),
                          plotOutput("KM_plot"),
                          htmlOutput("KM_caption")
                        )
                        ###
                        
                      ) # inner sidebar Layout close
             ), # nav_tab 4 close
             
             
             #PAGE 5         
             tabPanel("Explore the Data",
                tabsetPanel(
                  tabPanel("All Data",
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
                                DT::dataTableOutput("table_dt")
                        
                        
                                ) # fluidPage close
             
                          ),
                  
                  tabPanel("Parallel Coordinates",
                           
                           
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput("vars", 
                                                  label = "Select continuous variables to include:",
                                                  choices = cont_vars, 
                                                  selected = cont_vars[1:5]),  # default
                               
                               # select which variable will determine the colour of the lines
                               selectInput("colour_var", 
                                           label = "Select a variable for colouring:",
                                           choices = cat_vars, 
                                           selected = cat_vars[1]),  # default
                               
                               h4("Variable Codebook"),
                               tableOutput("codebook_ui_3")
                               
                             ),
                             
                             # Main panel
                             mainPanel(
                               titlePanel("Dynamic Parallel Coordinates Plot"),
                               plotlyOutput("parallel_plot"),
                               htmlOutput("parallel_plot_caption")
                             )
                           )
                           
                           
                          )
                        )
                      ), #nav_tab 5 close
             
             
            ) #navbar close

)# UI close


server <- function(input, output) {
  
  ####################### Misc ##################################################################################################
  
  #mini codebook
  output$codebook_table <- renderTable({
    data.frame(
      Variable = every_var,
      Description = c("Treatment", "Sex", "Race", "History of Hypertension", "Hospitalisation due to Cardiovascular Disease", 
                      "Hospitalisation due to Worsening Heart Faillure", "Hospitalisation due to Digoxin Toxicity", "Hospitalisation", 
                      "Vital Status of Patient", "Age (years)", "Body Mass Index", "Serum Potassium (mmol/L", "Serum Creatinine (mg/dL)", 
                      "Diastolic Blood Pressure", "Systolic Blood Pressure", "Days till First Hospitalisation", "Days till Last Followup/Death")
    )
    
  })
  
  #instances
  output$codebook_ui_1 <- renderUI({ # Baseline Variables > Mixed Compare
    tableOutput("codebook_table")
  })
  
  output$codebook_ui_2 <- renderUI({ # Baseline Variables > Continuous Compare
    tableOutput("codebook_table")
  })
  
  output$codebook_ui_3 <- renderUI({ # Explore the Data > Parallel coordinates
    tableOutput("codebook_table")
  })
  
  #mini codebook - binary vars only
  output$codebook_table_bin <- renderTable({
    data.frame(
      Variable = cat_vars,
      Description = c("Treatment", "Sex", "Race", "History of Hypertension", "Hospitalisation due to Cardiovascular Disease", 
                      "Hospitalisation due to Worsening Heart Faillure", "Hospitalisation due to Digoxin Toxicity", "Hospitalisation", 
                      "Vital Status of Patient")
    )
    
  })
  
  #instances
  output$codebook_table_bin_1 <- renderUI({ # Baseline Variables > Association Compare
    tableOutput("codebook_table_bin")
  })
  
  output$codebook_table_bin_2 <- renderUI({ # Mortality
    tableOutput("codebook_table_bin")
  })
  
  output$codebook_table_bin_3 <- renderUI({ # Demographics > Categorical
    tableOutput("codebook_table_bin")
  })
  
  
  #mini codebook - cont vars only
  output$codebook_table_cont <- renderTable({
    data.frame(
      Variable = cont_vars,
      Description = c("Age (years)", "Body Mass Index", "Serum Potassium (mmol/L", "Serum Creatinine (mg/dL)", 
                      "Diastolic Blood Pressure", "Systolic Blood Pressure", "Days till First Hospitalisation", "Days till Last Followup/Death")
    )
  })
  
  # instances
  output$codebook_table_cont_1 <- renderUI({ # not in use
    tableOutput("codebook_table_cont")
  })
  
  ####################### SERVER CODE TAB 1 ##################################################################################################
  
  ## Continuous Variable Histograms ##
  output$cont_histograms <- renderPlotly({
    req(input$cont_hist_var)  # Ensure a variable is selected
    
    plot_ly(
      data = dig.df,
      x = ~ get(input$cont_hist_var),
      type = "histogram",
      marker = list(color = "dodgerblue")
    ) %>%
      layout(
        title = paste("Histogram of", input$cont_hist_var),
        xaxis = list(title = input$cont_hist_var),
        yaxis = list(title = "Count")
      )
  })
  
  #caption
  output$histo_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      HTML("Histogram showing distribution of continuous baseline variables.
           <br>Source: Digitalis Investigation Group Trial")
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
      marker = list(colors = c("darkorange", "dodgerblue"))
    ) %>%
      layout(
        title = paste("Distribution of", input$binary_pie_var)
      )
  })
  
  #caption
  output$pie_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      HTML("Piechart showing proportions of categorical variable levels.
           <br>Source: Digitalis Investigation Group Trial")
    )
  })
  
  ## Summary Statistics ##
  
  # Table
  output$summary_stats_table_cont <- renderTable({
    req(input$cont_hist_var)  # Ensure a variable is selected
    
    summary_stats_cont <- summary(na.omit(dig.df[[input$cont_hist_var]]))  
    data.frame(Statistic = names(summary_stats_cont), Value = as.vector(summary_stats_cont))
  })
  
  # Table
  output$summary_stats_table_cat <- renderTable({
    req(input$binary_pie_var)  # Ensure a variable is selected
    
    summary_stats_cat <- summary(na.omit(dig.df[[input$binary_pie_var]]))  
    data.frame(Statistic = names(summary_stats_cat), Value = as.vector(summary_stats_cat))
  })
  
  # Boxplot
  output$summary_boxplot <- renderPlotly({
    req(input$cont_hist_var)  # Ensure a variable is selected
    
    # Check if the variable is numeric (for valid boxplot)
    if (is.numeric(dig.df[[input$cont_hist_var]])) {
      plot_ly(
        data = dig.df,
        y = ~ get(input$cont_hist_var),
        type = "box",
        marker = list(color = "dodgerblue")
      ) %>%
        layout(
          title = paste("Boxplot of", input$cont_hist_var),
          yaxis = list(title = input$cont_hist_var)
        )
    } else {
      NULL  # Skip boxplot if the variable is not numeric
    }
  })
  
  #caption
  output$one_box_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      HTML("Boxplot showing summary of distribution for the continuous variable.
           <br>Source: Digitalis Investigation Group Trial")
    )
  })
  
  
  ####################### SERVER CODE TAB 2 ##################################################################################################
  ####### SUBTAB 1 #######
  
  # selected variables
  bvar_1 <- reactive({input$base_var1})
  bvar_2 <- reactive({input$base_var2})
  
  # Table1 Dynamic Title
  output$baseline_table_title_1 <- renderUI({
    req(input$base_var1, input$base_var2)
    
    # Create a dynamic title
    title_text <- paste("Comparison of", bvar_1(), "by", bvar_2())
    
    # Render the title as an HTML element
    tags$h3(title_text, style = "margin-bottom: 20px;")
  })
  
  # Table1 as DT
  output$table_baseline_compare_1 <- renderUI({
    req(input$base_var1, input$base_var2)
    
    #remove NA values from base variables
    clean_data.df <- dig.df %>%
      filter(!is.na(.[[bvar_1()]]) & !is.na(.[[bvar_2()]]))
    
    formula <- as.formula(paste("~", bvar_1(), "|", bvar_2()))
    
    tbl <- table1(formula, data = clean_data.df) # generate table1 with two input variables
    
    tbl_df <- as.data.frame(tbl) #convert to dataframe to make it prettier
    
    DT::datatable(tbl_df, rownames = FALSE, options = list(paging = FALSE, dom = 't', autoWidth = TRUE)) %>%
      DT::formatStyle(columns = 1, fontWeight = "bold" # Make row names bold
      )
  })

  
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
        colors = c("darkorange", "dodgerblue")
      ) %>%
      layout(
        xaxis = list(title = bvar_2()),
        yaxis = list(title = bvar_1())
      )
  })
  
  #caption
  output$box_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      HTML("Boxplots showing summary statistics for two continuous variables between groups. Hover over plots for more information.
           <br>Source: Digitalis Investigation Group Trial")
    )
  })
  
  ####### SUBTAB 2 #######
  
  # selected variables
  svar_1 <- reactive({input$scatter_var1})
  svar_2 <- reactive({input$scatter_var2})
  
  # Table1 Dynamic Title
  output$baseline_table_title_2 <- renderUI({
    req(input$scatter_var1, input$scatter_var2)
    
    # Create a dynamic title
    title_text <- paste("Scatterplot of", svar_1(), "and", svar_2())
    
    # Render the title as an HTML element
    tags$h3(title_text, style = "margin-bottom: 20px;")
  })

  
  # scatter plot
  
  output$scatter_baseline_compare_plotly <- renderPlotly({
    req(input$scatter_var1, input$scatter_var2)  # Ensure variables are selected
    
    # Base selected columns
    selected_columns <- c(input$scatter_var1, input$scatter_var2)
    
    # Add colour variable if it's not "None"
    if (input$scatter_colour != "") {
      selected_columns <- c(selected_columns, input$scatter_colour)
    }
    
    scatter_data <- dig.df %>%
      select(all_of(selected_columns)) %>%
      na.omit()
    
    # Calculate the correlation coefficient
    correlation <- cor(scatter_data[[input$scatter_var1]], scatter_data[[input$scatter_var2]], use = "complete.obs")
    corr_text <- paste("Correlation: ", round(correlation, 2))
    
    # Check if a colour variable is selected
    if (input$scatter_colour == "") {
      # Default plot without colouring
      plot_ly(
        data = scatter_data,
        x = ~ get(input$scatter_var2),
        y = ~ get(input$scatter_var1),
        type = "scatter",
        mode = "markers",
        marker = list(color = "#1F77B4")  # Default uniform colour
      ) %>%
        layout(
          title = list( # correlation coef
            text = paste("Scatter Plot of", input$scatter_var1, "vs", input$scatter_var2, "<br>", corr_text),
            x = 0.5,  
            y = 0.95, 
            yanchor = 'top'
          ),
          margin = list(t = 100), # axes start at 0
          xaxis = list(
            title = input$scatter_var2,
            range = c(0, max(dig.df[[input$scatter_var2]], na.rm = TRUE))
          ),
          yaxis = list(
            title = input$scatter_var1,
            range = c(0, max(dig.df[[input$scatter_var1]], na.rm = TRUE))
          )
        )
    } else {
      # Plot with colouring by the selected binary variable
      plot_ly(
        data = scatter_data,
        x = ~ get(input$scatter_var2),
        y = ~ get(input$scatter_var1),
        color = ~ as.factor(get(input$scatter_colour)),  
        colors = c("dodgerblue", "darkorange"),
        type = "scatter",
        mode = "markers"
      ) %>%
        layout(
          title = list( # correlation coef
            text = paste("Scatter Plot of", input$scatter_var1, "vs", input$scatter_var2, "<br>", corr_text),
            x = 0.5,
            y = 0.95,
            yanchor = 'top'
          ),
          margin = list(t = 100), 
          xaxis = list( # both axes start at 0
            title = input$scatter_var2,
            range = c(0, max(dig.df[[input$scatter_var2]], na.rm = TRUE))
          ),
          yaxis = list(
            title = input$scatter_var1,
            range = c(0, max(dig.df[[input$scatter_var1]], na.rm = TRUE))
          )
        )
    }
  })
  
  #caption
  output$scatter_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      HTML("Scatterplot showing association between continuous variables including correlation coefficient.
           <br>Source: Digitalis Investigation Group Trial")
    )
  })
  
  
  # summary stats
  
  output$summary_table <- renderTable({
    
    # compute summary statistics for both variables
    summary_var1 <- summary(na.omit(dig.df[[svar_1()]]))
    summary_var2 <- summary(na.omit(dig.df[[svar_2()]]))
    
    # combine both summaries into one data frame
    combined_summary <- data.frame(
      Statistic = names(summary_var1),
      beep = as.vector(summary_var1),
      boop = as.vector(summary_var2)
    )
    
    # name columns accordingly
    colnames(combined_summary)[2] <- svar_1()
    colnames(combined_summary)[3] <- svar_2()
    
    # return combined summary as a table
    combined_summary
  })
  
  
  ####### SUBTAB 3 #######
  
  mvar_1 <- reactive({input$mosaic_var1})
  mvar_2 <- reactive({input$mosaic_var2})
  
  # dynamic title
  output$baseline_table_title_3 <- renderUI({
    req(input$mosaic_var1, input$mosaic_var2)
    
    # Create a dynamic title
    title_text <- paste("Association between", mvar_1(), "and", mvar_2())
    
    # Render the title as an HTML element
    tags$h3(title_text, style = "margin-bottom: 20px;")
  })
  
  
  #visualise
  output$mosaic_baseline_compare_plotly <- renderPlotly({
    
    contingency_table <- table(dig.df[[mvar_1()]], dig.df[[mvar_2()]])
    
    contingency.df <- as.data.frame(contingency_table) %>%
      group_by(Var1) %>%
      mutate(percentage = Freq / sum(Freq) * 100) %>%
      ungroup()
    
    plot_ly(
      data = contingency.df,
      x = ~Var1,  
      y = ~Freq,  # Frequency count of each category combination
      color = ~Var2,
      type = "bar",
      colors = c("darkorange", "dodgerblue"),
      text = ~paste("Frequency: ", Freq, "<br>Percentage: ", round(percentage, 2), "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = mvar_1()),  
        yaxis = list(title = "Frequency") 
      )
    
  })
  
  #caption
  output$mosaic_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      HTML("Stacked bar chart showing differences in proportions between binary variables in the dataset.
           <br>Source: Digitalis Investigation Group Trial")
    )
  })
  
  
  ####################### SERVER CODE TAB 3 ##################################################################################################
  
  
  ####################### SERVER CODE TAB 4 ##################################################################################################
  
  # Dynamic title
  output$KM_title <- renderUI({
    if (input$KM_split == "None") {
      title_text <- "Kaplan-Meier Survival Curve"
    } else {
      title_text <- paste("Kaplan-Meier Survival Curve by", input$KM_split)
    }
    
    # Render the title as an HTML element
    tags$h3(title_text, style = "margin-bottom: 20px;")
  })
  
  output$KM_plot <- renderPlot({
    km.df <- dig.df
    levels(km.df$DEATH) <- c(0, 1)
    km.df$DEATH <- as.numeric(km.df$DEATH)
    
    if (input$KM_split == "None") {
      # Single curve when "None" is selected
      fit <- survfit(Surv(Month, DEATH) ~ 1, data = km.df, conf.int = FALSE)
      
      plot(fit,
           xlab = "Time (Months)", ylab = "Survival Probability",
           col = "black", lwd = 2,
           mark.time = input$show_censor,
           conf.int = FALSE)  
      
      # Add grid lines to the plot if the checkbox is selected
      if (input$show_grid) {
        grid(col = "gray", lty = "dotted")
      }
      
    } else {
      # Stratified curve when a variable is selected
      km_formula <- as.formula(paste("Surv(Month, DEATH) ~", input$KM_split))
      
      # Fit Kaplan-Meier model
      fit <- survfit(km_formula, data = km.df, conf.int = FALSE)
      
      # Colours for the binary groups
      colors <- c("darkorange", "dodgerblue")
      
      # Plot the Kaplan-Meier survival curve
      plot(fit,
           xlab = "Time (Months)", ylab = "Survival Probability", 
           col = colors[1:length(levels(km.df[[input$KM_split]]))], lwd = 2, 
           mark.time = input$show_censor)  # Toggle censoring marks based on checkbox
      
      # Add grid lines to the plot if the checkbox is selected
      if (input$show_grid) {
        grid(col = "gray", lty = "dotted")
      }
      
      # Add a legend
      legend("topright", 
             legend = levels(km.df[[input$KM_split]]),  # Level names
             fill = colors[1:length(levels(km.df[[input$KM_split]]))],  # Colours for the boxes
             title = paste(input$KM_split),  # Title
             cex = 1.2, 
             border = "black")
    }
  })
  
  
  #caption
  
  output$KM_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      HTML("Kaplan-Meier survival curve, showing cumulative probability of survival for trial participants. Plus symbols (+) depict censorship events.
           <br>Source: Digitalis Investigation Group Trial")
    )
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
      colnames = c("ID", "Treatment", "Age (Years)", "Sex", "Race", "BMI", "Serum Potassium (mmol/L)", "Serum Creatinine (mg/dL)",
                   "Diastolic BP (mmHg)", "Systolic BP (mmHg)", "Hypertension History", "Hospitalisation: Cardiovascular Disease (1=Yes)",
                   "Hospitalisation: Worsening Heart Failure (1=Yes)", "Hospitalisation: Digoxin Toxicity (1=Yes)", "Hospitalisation (Any)",
                   "Time till First Hospitalisation (Days)", "Status", "Time till Last Followup (Days)", "Time till Last Followup (Months)"),
      caption = "Source: Digitalis Investigation Group Trial" ))
  
  
  # Parallel Coordinates Plot
  
  output$parallel_plot <- renderPlotly({
    
    # Filter out selected variables
    selected_data <- dig.df %>%
      select(all_of(input$vars)) %>%
      na.omit()
    
    # Check if variables are selected
    if (length(input$vars) == 0) {
      return(NULL)
    }
    
    # Select the categorical variable for coloring
    colour_var <- dig.df[[input$colour_var]]
    
    # Ensure the coloring variable is a factor (for discrete colors)
    colour_var <- as.factor(colour_var)
    
    # Parallel coordinates plot with discrete colors
    plot_ly(
      data = selected_data,
      type = "parcoords",
      line = list(
        color = as.numeric(colour_var),  # Convert factor to numeric for coloring
        colorscale = list(
          list(0, "dodgerblue"),  # Map level 1 to blue
          list(1, "darkorange")    # Map level 2 to red
        ),
        showscale = TRUE,  # Add a color legend
        colorbar = list(
          title = input$colour_var,  # Label for the colorbar
          tickvals = c(1, 2),  # Ensure both levels are shown
          ticktext = levels(colour_var)  # Display factor levels as tick labels
        )
      ),
      dimensions = lapply(names(selected_data), function(var) {
        
        # Dynamically compute the range for each variable
        var_min <- min(selected_data[[var]], na.rm = TRUE)
        var_max <- max(selected_data[[var]], na.rm = TRUE)
        
        # Return the settings for the dimension
        list(
          range = c(var_min, var_max),  # Min and max for each dimension's range
          label = var,                  # Dimension label
          values = selected_data[[var]] # Dimension values
        )
      })
    ) %>%
      layout(
        title = "Parallel Coordinates Plot",
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })
  
  
  
  #caption
  
  output$parallel_plot_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      HTML("Parallel coordinates plot showing the relationships between a selection of continuous variables in the dataset.
           <br> Source: Digitalis Investigation Group Trial")
    )
  })
    
  }

# Run the application 
shinyApp(ui = ui, server = server)
