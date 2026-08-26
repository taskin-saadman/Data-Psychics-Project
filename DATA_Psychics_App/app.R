library(shiny)
library(tidyverse)
library(rstatix)
library(glue)

#setwd("C:/Users/user/Downloads")
#getwd()

# Read csv
df <- read_csv(
  "DATA2X02_data_cleaned.csv",
  show_col_types = FALSE
)

# Convert to factor
df <- df %>% mutate(
  across(
    c(
      "intl_dom_type",
      "type_2X02",
      "assessment_workflow",
      "workflow_grouped",
      "rent_y_n",
      "employment_y_n",
      "multitask_y_n"
    ),
    as.factor
  )
)

# Convert to numeric for ordinal factors
df <- df %>% mutate(
  across(
    c("grade_1X01", "grade_2X02"),
    ~ case_when(
      .x == "P" ~ 1,
      .x == "CR" ~ 2,
      .x == "D" ~ 3,
      .x == "HD" ~ 4
    )
  )
)

# Define numeric choices
numeric_choices <- c(
  "travel_to_campus_min",
  "rent_dollars",
  "employment_hours",
  "sleep_hours",
  "water_l"
)

# Define categorical nominal choices
nominal_choices <- c(
  "intl_dom_type",
  "type_2X02",
  "assessment_workflow",
  "workflow_grouped",
  "rent_y_n",
  "employment_y_n",
  "multitask_y_n"
)

# Define categorical ordinal choices
ordinal_choices <- c(
  "grade_1X01",
  "grade_2X02"
)


# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  
  titlePanel("DATA2902 Psychics"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # ===================================================
      # QUESTION 1 SIDEBAR
      # ===================================================
      conditionalPanel(
        condition = "input.selected_tab == 'question1'",
        
        h4("Question 1"),
        
        conditionalPanel(
          condition = "input.q1_tab == 'introduction'",
          
          p(
            "The variables for this analysis are fixed by the research question."
          ),
          
          tags$strong("Variables used:"),
          
          tags$ul(
            tags$li("Course enrolment"),
            tags$li("Paid employment status"),
            tags$li("Domestic/International student status")
          )
        ),
        
        conditionalPanel(
          condition = "input.q1_tab == 'independence'",
          
          tags$strong("Pearson Chi-square Test of Independence"),
          
          p(
            "Select two categorical variables to examine their association."
          ),
          
          selectInput(
            inputId = "q1_ind_x",
            label = "Categorical variable 1:",
            choices = nominal_choices,
            selected = "type_2X02"
          ),
          
          selectInput(
            inputId = "q1_ind_y",
            label = "Categorical variable 2:",
            choices = nominal_choices,
            selected = "employment_y_n"
          ),
          
          radioButtons(
            inputId = "q1_ind_display",
            label = "Display:",
            choices = c(
              "Observed counts",
              "Expected counts",
              "Row proportions"
            ),
            selected = "Observed counts"
          )
        ),
        
        conditionalPanel(
          condition = "input.q1_tab == 'homogeneity'",
          
          tags$strong("Chi-square Test of Homogeneity"),
          
          p(
            "Compares the distribution of employment status across courses."
          )
        ),
        
        conditionalPanel(
          condition = "input.q1_tab == 'cmh'",
          
          tags$strong("Cochran-Mantel-Haenszel Chi-square Test"),
          
          p(
            "Controls for Domestic/International student status."
          )
        )
      ),
      
      
      # ===================================================
      # QUESTION 2 SIDEBAR
      # ===================================================
      conditionalPanel(
        condition = "input.selected_tab == 'question2'",
        
        # ---------------------------------------------------
        # Module 1 sidebar options
        # ---------------------------------------------------
        conditionalPanel(
          condition = "input.module_tab == 'module1'",
          
          selectInput(
            inputId = "drop1_xvar",
            label = "Module 1 Test Options:",
            choices = nominal_choices,
            selected = "intl_dom_type"
          ),
          
          selectInput(
            inputId = "drop1_yvar",
            label = "Module 1 Test Options:",
            choices = numeric_choices,
            selected = "rent_dollars"
          )
        ),
        
        
        # ---------------------------------------------------
        # Module 2 sidebar options
        # ---------------------------------------------------
        conditionalPanel(
          condition = "input.module_tab == 'module2'",
          
          selectInput(
            inputId = "drop2_xvar",
            label = "Grouping Variables:",
            choices = nominal_choices[-3],
            selected = "intl_dom_type"
          ),
          
          radioButtons(
            inputId = "yvar_type",
            label = "Ordinal or Numerical Variables?",
            choices = c("Numerical", "Ordinal")
          ),
          
          conditionalPanel(
            condition = "input.yvar_type == 'Numerical'",
            
            selectInput(
              inputId = "drop2_yvar_num",
              label = "Numerical Variables:",
              choices = numeric_choices,
              selected = "travel_to_campus_min"
            )
          ),
          
          conditionalPanel(
            condition = "input.yvar_type == 'Ordinal'",
            
            selectInput(
              inputId = "drop2_yvar_ord",
              label = "Ordinal Variables:",
              choices = ordinal_choices,
              selected = "grade_1X01"
            )
          )
        ),
        
        
        # ---------------------------------------------------
        # Resampling Tests sidebar options
        # ---------------------------------------------------
        conditionalPanel(
          condition = "input.module_tab == 'resampling'",
          
          selectInput(
            inputId = "drop3_xvar",
            label = "Module 3 Test Options:",
            choices = nominal_choices,
            selected = "intl_dom_type"
          ),
          
          selectInput(
            inputId = "drop3_yvar",
            label = "Module 3 Test Options:",
            choices = numeric_choices,
            selected = "rent_dollars"
          )
        )
        
      )
    ),
    
    
    # =====================================================
    # MAIN PANEL
    # =====================================================
    mainPanel(
      
      # DEFINE RESEARCH QUESTION
      h4("Research Question"),
      p("The question we would like to answer is whether ", 
        "there may exist significant challenges that students ", 
        "face in their studies. This includes employment, travel, ", 
        "or gaps in study skills. Through performing relevant ", 
        "statistical tests on selected variables, relationships ", 
        "between these possible challenges and a student's grades ", 
        "may surface. By identifying these challenges, the ",
        "university may be able to provide earlier, more efficient ", 
        "and targeted assistance to students in need."), 
    

      # ---------------------------------------------------
      # TOP-LEVEL TABS
      # ---------------------------------------------------
      tabsetPanel(
        id = "selected_tab",
        
        
        # =================================================
        # QUESTION 1
        # =================================================
        tabPanel(
          title = "QUESTION 1",
          value = "question1",
          
          h3(
            "Employment, DATA2002/DATA2902 Enrolment, and Weekly Paid Work"
          ),
          
          tabsetPanel(
            id = "q1_tab",
            
            # ---------------------------------------------
            # Introduction
            # ---------------------------------------------
            tabPanel(
              title = "Introduction",
              value = "introduction",
              
              h4("Research Question"),
              
              p(
                "Is being in paid employment associated with enrolment ",
                "in DATA2002 versus DATA2902, and among employed students, ",
                "do DATA2902 students work fewer hours per week than DATA2002 ",
                "students, once the different mix of Domestic and International ",
                "students is taken into account?"
              ),
              
              h4("Analysis"),
              
              p(
                "This research question involves multiple outcomes and therefore ",
                "requires more than one statistical test."
              ),
              
              tags$ul(
                tags$li(
                  "A Pearson chi-square test of independence examines the ",
                  "unadjusted association between course enrolment and paid ",
                  "employment status."
                ),
                tags$li(
                  "A chi-square test of homogeneity provides the equivalent ",
                  "homogeneity interpretation of the same contingency table."
                ),
                tags$li(
                  "A Cochran-Mantel-Haenszel chi-square test examines the ",
                  "course-employment association while controlling for ",
                  "Domestic/International student status."
                ),
                tags$li(
                  "Weekly employment hours are numerical and therefore are ",
                  "not analysed using a chi-square test."
                )
              ),
              
              h4("Significance Level"),
              
              p(
                "All hypothesis tests use a significance level of α = 0.05."
              )
            ),
            
            
            # ---------------------------------------------
            # Chi-square independence
            # ---------------------------------------------
            tabPanel(
              title = "Chi-square Independence",
              value = "independence",
              
              h4("Pearson Chi-square Test of Independence"),
              
              p(
                "This test examines whether DATA2002/DATA2902 enrolment ",
                "is associated with paid employment status."
              ),
              
              h4("Contingency Table Display"),
              
              tableOutput("q1_ind_table"),
              
              verbatimTextOutput("q1_chi_independence")
            ),
            
            
            # ---------------------------------------------
            # Chi-square homogeneity
            # ---------------------------------------------
            tabPanel(
              title = "Chi-square Homogeneity",
              value = "homogeneity",
              
              h4("Chi-square Test of Homogeneity"),
              
              p(
                "This test examines whether the distribution of employment ",
                "status is homogeneous across DATA2002 and DATA2902."
              ),
              
              verbatimTextOutput("q1_chi_homogeneity")
            ),
            
            
            # ---------------------------------------------
            # CMH chi-square
            # ---------------------------------------------
            tabPanel(
              title = "CMH Chi-square",
              value = "cmh",
              
              h4("Cochran-Mantel-Haenszel Chi-square Test"),
              
              p(
                "This test examines the association between course enrolment ",
                "and employment status while controlling for ",
                "Domestic/International student status."
              ),
              
              verbatimTextOutput("q1_cmh")
            )
          )
        ),
        
        
        # =================================================
        # QUESTION 2
        # =================================================
        tabPanel(
          title = "QUESTION 2",
          value = "question2",
          
          h3("Question 2"),
          
          # ------------------------------------------------
          # Existing module tabs are nested inside Q2
          # ------------------------------------------------
          tabsetPanel(
            id = "module_tab",
            
            # ----------------------------------------------
            # Module 1
            # ----------------------------------------------
            tabPanel(
              title = "Module 1 Tests",
              value = "module1",
              
              textOutput("out1")
            ),
            
            
            # ----------------------------------------------
            # Module 2
            # ----------------------------------------------
            tabPanel(
              title = "Module 2 Tests",
              value = "module2",
              
              verbatimTextOutput("out2")
            ),
            
            
            # ----------------------------------------------
            # Resampling Tests
            # ----------------------------------------------
            tabPanel(
              title = "Resampling Tests",
              value = "resampling",
              
              textOutput("out3")
            )
          )
        ),
        
        
        # =================================================
        # QUESTION 3
        # =================================================
        tabPanel(
          title = "QUESTION 3",
          value = "question3",
          
          h3("Question 3"),
          
          p(
            "Question 3 analysis will be added in a later commit."
          )
        )
      )
    )
  )
)


# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  # -------------------------------------------------------
  # MODULE 1
  # -------------------------------------------------------
  output$out1 <- renderText({
    paste(
      "You selected:",
      input$drop1_xvar
    )
  })
  
  
  # -------------------------------------------------------
  # MODULE 2
  # -------------------------------------------------------
  output$out2 <- renderPrint({
    
    xvar <- input$drop2_xvar
    
    if (input$yvar_type == "Numerical") {
      
      yvar <- input$drop2_yvar_num
      
      # Build formula
      f <- as.formula(
        paste(yvar, "~", xvar)
      )
      
      # Conduct Levene's test
      p_equal_var <- levene_test(
        f,
        data = df
      ) %>%
        pull(p)
      
      if (p_equal_var < 0.05) {
        
        return(
          list(
            glue(
              "We will conduct a Welch's t-test,
              as the p-value for Levene's test = {p_equal_var}
              is less than the significance level of 0.05
              (heteroscedasticity)."
            ),
            
            df |>
              t.test(
                formula = f,
                data = _
              )
          )
        )
        
      } else {
        
        return(
          list(
            glue(
              "We will conduct a Student's t-test,
              as the p-value for Levene's test = {p_equal_var}
              is more than the significance level of 0.05
              (homoscedasticity). Sienna says hi"
            ),
            
            df |>
              t.test(
                formula = f,
                var.equal = TRUE,
                data = _
              )
          )
        )
      }
      
    } else {
      
      yvar <- input$drop2_yvar_ord
      
      # Build formula
      f <- as.formula(
        paste(yvar, "~", xvar)
      )
      
      return(
        list(
          glue(
            "We will conduct a Wilcoxon Rank-Sum test,
            as the chosen y-variables are ordinal (categorical)."
          ),
          
          wilcox.test(
            formula = f,
            data = df
          )
        )
      )
    }
  })
  
  
  # -------------------------------------------------------
  # RESAMPLING TESTS
  # -------------------------------------------------------
  output$out3 <- renderText({
    paste(
      "You selected:",
      input$drop3_xvar
    )
  })
  
  
  # -------------------------------------------------------
  # QUESTION 1 - INDEPENDENCE CONTINGENCY TABLE
  # -------------------------------------------------------
  q1_ind_table <- reactive({
    
    req(input$q1_ind_x, input$q1_ind_y)
    
    validate(
      need(
        input$q1_ind_x != input$q1_ind_y,
        "Please select two different categorical variables."
      )
    )
    
    table(
      df[[input$q1_ind_x]],
      df[[input$q1_ind_y]],
      dnn = c(input$q1_ind_x, input$q1_ind_y)
    )
  })
  
  
  # -------------------------------------------------------
  # QUESTION 1 - CHI-SQUARE INDEPENDENCE TEST
  # -------------------------------------------------------
  q1_ind_test <- reactive({
    
    chisq.test(
      q1_ind_table(),
      correct = FALSE
    )
  })
  
  
  output$q1_ind_table <- renderTable({
    
    req(input$q1_ind_display)
    
    if (input$q1_ind_display == "Observed counts") {
      
      q1_ind_table()
      
    } else if (input$q1_ind_display == "Expected counts") {
      
      round(
        q1_ind_test()$expected,
        2
      )
      
    } else {
      
      round(
        prop.table(
          q1_ind_table(),
          margin = 1
        ),
        3
      )
    }
    
  }, rownames = TRUE)
  
  
  output$q1_chi_independence <- renderPrint({
    q1_ind_test()
  })
  
  
  # -------------------------------------------------------
  # QUESTION 1 TEST PLACEHOLDERS
  # -------------------------------------------------------
  # Remaining chi-square tests will be implemented in
  # separate commits.
  
  output$q1_chi_homogeneity <- renderPrint({
    "Chi-square homogeneity test will be implemented in a separate commit."
  })
  
  
  output$q1_cmh <- renderPrint({
    "Cochran-Mantel-Haenszel test will be implemented in a separate commit."
  })
}


# =========================================================
# RUN APP
# =========================================================

shinyApp(
  ui = ui,
  server = server
)