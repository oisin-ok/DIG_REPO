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
levels(dig.df$RACE) <- c("White", "Other") # Patient ethnicity

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

# Variables Categories
every_var <- c("TRTMT", "SEX", "RACE", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "DEATH", "AGE", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HOSPDAYS", "DEATHDAY")
baseline_vars <- c("TRTMT", "AGE", "SEX", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP")
cat_vars <- c("TRTMT", "SEX", "RACE", "HYPERTEN", "CVD", "WHF", "DIG", "HOSP", "DEATH")
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
                                   uiOutput("table_baseline_compare_1"),
                                   plotlyOutput("boxplot_baseline_compare_plotly")
                        ) # main close
                        ###
                        
                                    ) #  inner sidebar Layout close
                            ), #tab panel inner 1 close
                  
                  tabPanel("Continuous Compare (Scatter)",
                           
                           sidebarLayout(
                             
                             #sidebar - input control, select a variable to see its effect on mortality
                             sidebarPanel(
                               selectInput("scatter_var1", "Choose a continuous variable to compare baseline values:", choices = cont_vars, selected = "AGE"),
                               selectInput("scatter_var2", "Choose a continuous variable to compare across:", choices = cont_vars, selected = "BMI"),
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
                               
                               #summary stats
                               h4("Summary Statistics for Selected Variables"),
                               tableOutput("summary_table")
                             ) # main close
                             ###
                             
                           )
                           ),
                  
                  
                  tabPanel("Association Compare (Mosaic)",
                           sidebarLayout(
                             
                             #sidebar - input control, select a variable to see its effect on mortality
                             sidebarPanel(
                               selectInput("mosaic_var1", "Choose a binary variable:", choices = cat_vars, selected = "TRTMT"),
                               selectInput("mosaic_var2", "Choose a binary variable:", choices = cat_vars, selected = "SEX"),
                               h4("Variable Codebook"),
                               tableOutput("codebook_table_bin_1")
                             ),
                             ###
                             
                             #main panel - outputs
                             mainPanel(
                               uiOutput("baseline_table_title_3"),
                               tableOutput("table_baseline_compare_3"),
                               plotlyOutput("mosaic_baseline_compare_plotly")
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
                               tableOutput("codebook_table_cont_1")
                               
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
  
  
  #mini codebook - cont vars only
  output$codebook_table_cont <- renderTable({
    data.frame(
      Variable = cont_vars,
      Description = c("Age (years)", "Body Mass Index", "Serum Potassium (mmol/L", "Serum Creatinine (mg/dL)", 
                      "Diastolic Blood Pressure", "Systolic Blood Pressure", "Days till First Hospitalisation", "Days till Last Followup/Death")
    )
  })
  
  
  output$codebook_table_cont_1 <- renderUI({ # Explore the Data > Parallel coordinates
    tableOutput("codebook_table_cont")
  })
  
  ####################### SERVER CODE TAB 1 ##################################################################################################
  
  
  
  
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
        colors = c("red", "blue")
      ) %>%
      layout(
        xaxis = list(title = bvar_2()),
        yaxis = list(title = bvar_1())
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
        colors = c("#1F77B4", "red4"),
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
      text = ~paste("Frequency: ", Freq, "<br>Percentage: ", round(percentage, 2), "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = mvar_1()),  
        yaxis = list(title = "Frequency") 
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
      colnames = c("ID", "Treatment", "Age (Years)", "Sex", "Race", "BMI", "Serum Potassium (mmol/L)", "Serum Creatinine (mg/dL)",
                   "Diastolic BP (mmHg)", "Systolic BP (mmHg)", "Hypertension History", "Hospitalisation: Cardiovascular Disease (1=Yes)",
                   "Hospitalisation: Worsening Heart Failure (1=Yes)", "Hospitalisation: Digoxin Toxicity (1=Yes)", "Hospitalisation (Any)",
                   "Time till First Hospitalisation (Days)", "Status", "Time till Last Followup (Days)", "Time till Last Followup (Months)"),
      caption = "Source: Digitalis Investigation Group Trial" ))
  
  
  # Parallel Coordinates Plot
  
  output$parallel_plot <- renderPlotly({
    
    # Filter out  selected variables
    selected_data <- dig.df %>%
      select(all_of(input$vars)) %>%
      na.omit()
    
    # Check if variables are selected
    if (length(input$vars) == 0) {
      return(NULL)
    }
    
    # Select the categorical variable for colouring
    colour_var <- dig.df[[input$colour_var]]
    
    # convert categorical variable to factor for colouring
    if (is.factor(colour_var)) {
      colour_var <- as.numeric(colour_var)
    }
    
    # parallel coordinates plot with manual ranges
    plot_ly(
      data = selected_data,
      type = "parcoords",
      line = list(color = colour_var, colorscale = "Viridis"),  # colouring
      dimensions = lapply(names(selected_data), function(var) {
        
        # Dynamically compute the range for each variable
        var_min <- min(selected_data[[var]], na.rm = TRUE)
        var_max <- max(selected_data[[var]], na.rm = TRUE)
        
        # Return the settings for the dimension
        list(
          range = c(var_min, var_max),  # min and max for each dimension's range
          label = var,                  # Dimension label
          values = selected_data[[var]] # Dimension values
        )
      })
    ) %>%
      layout(
        title = "Parallel Coordinates Plot",
        showlegend = FALSE
      )
  })
  
  #caption
  
  output$parallel_plot_caption <- renderUI({
    tags$div(
      style = "margin-top: 10px; font-style: italic; color: gray;",
      "Parallel coordinates plot showing the relationships between a selection of continuous variables in the dataset."
    )
  })
    
  }

# Run the application 
shinyApp(ui = ui, server = server)
