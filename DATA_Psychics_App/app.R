library(shiny)
library(tidyverse)
library(rstatix)
library(glue)

df <- read_csv(
  "DATA2X02_data_cleaned.csv",
  show_col_types = FALSE
)

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

numeric_choices <- c(
  "travel_to_campus_min",
  "rent_dollars",
  "employment_hours",
  "sleep_hours",
  "water_l"
)

nominal_choices <- c(
  "intl_dom_type",
  "type_2X02",
  "assessment_workflow",
  "workflow_grouped",
  "rent_y_n",
  "employment_y_n",
  "multitask_y_n"
)

ordinal_choices <- c(
  "grade_1X01",
  "grade_2X02"
)

ui <- fluidPage(
  
  titlePanel("DATA2902 Psychics"),
  
  sidebarLayout(
    
    sidebarPanel(
      
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
      
      conditionalPanel(
        condition = "input.selected_tab == 'question2'",
        
        conditionalPanel(
          condition = "input.q2_tab == 'two_sample_q2'",
          
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
        )
      ),
      
      conditionalPanel(
        condition = "input.selected_tab == 'question3'",
        
        conditionalPanel(
          condition = "input.q3_tab == 'q3_perm_employment'",
          
          h4("Monte Carlo Permutation Options"),
          p("Permutation test for the difference in mean weekly working hours between DATA2002 and DATA2902 students."),
          
          numericInput(
            inputId = "q1_B",
            label = "Permutations:",
            value = 1000,
            min = 1,
            max = 100000
          ),
          
          actionButton(
            inputId = "run_q1_perm",
            label = "Run Permutation Test",
            class = "btn-primary"
          )
        ),
        
        conditionalPanel(
          condition = "input.q3_tab == 'q3_perm_multitask'",
          
          h4("Monte Carlo Permutation Options"),
          
          selectInput(
            inputId = "perm_group",
            label = "Compare Multitasking By:",
            choices = c(
              "Employment Status (working/not working)" = "employment_y_n",
              "Rent Status (rent/no rent)" = "rent_y_n"
            )
          ),
          
          numericInput(
            inputId = "B",
            label = "Permutations:",
            value = 1000,
            min = 1,
            max = 100000
          ),
          
          actionButton(
            inputId = "run_perm",
            label = "Run Permutation Test",
            class = "btn-primary"
          )
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "selected_tab",
        
        tabPanel(
          title = "INTRODUCTION",
          value = "intro",
          
          h2("Research Motivation"),
          p("Graphs go in here, possibly interactive as well"),
          p(
            "The question we would like to answer is whether ",
            "there may exist significant challenges that students ",
            "face in their studies. This includes employment, travel, ",
            "or gaps in study skills. Through performing relevant ",
            "statistical tests on selected variables, relationships ",
            "between these possible challenges and a student's grades ",
            "may surface. By identifying these challenges, the ",
            "university may be able to provide earlier, more efficient ",
            "and targeted assistance to students in need."
          ),
          
          h2("Data Description"),
          p("Basic description of data, justification on whether it's a random sample...etc."),
          
          h2("Limitations and Biases"),
          
          h2("Data Processing"),
          p(
            "The column names of the dataset were standardised to summarise the recorded value based on the questions asked alongside its unit of measurement. 
          All categorical variables were factorised to ensure proper representation in R, including students' grades for DATA1X01 and DATA2X02, which encoded as ordered factors to preserve their ordinal structure. 
          A Boolean employment variable was created to indicate whether a student had spent any time in employment during the past week, with values above zero coded as yes and zero coded as no."
          ),
          p(
            "The dataset was then subsetted to only include variables relevant to the research question on student lifestyle and academic factors, improving computational efficiency and reducing storage requirements.
            The Boolean rent and multitasking variables were reprocessed to ensure consistency and prevent misaligned records: rent values above zero were coded as yes, while zero values were coded as no. 
            For multitasking, inconsistencies were corrected where students claimed to multitask but provided no task entries, or conversely, provided entries but selected no."
          ),
          
          p(
            "Unrealistic numerical values were also filtered out, such as travelling for more than 4 hours daily to reach to campus, 
            spending more than 1.5 × 38 hours per week in employment (38 hours a week is the Fair Work Ombudsman (n.d.) limit for full-time work, 
            with extra allotted hours to allow reasonable flexibility for part-time work), or recording implausible sleep durations 
            such as more than 20 hours (as it substantially exceeds normal adult and most clinical hypersomnia ranges (citation needed)), or exactly zero hours a day."
          ),
          
          p(
            "Lastly, a new grouping column was created to classify assessment workflow patterns as as either consistent (gradually and immediately) or inconsistent (crammed and other), 
            primarily for two-sample mean comparison analyses."
          )
        ),
        
        tabPanel(
          title = "QUESTION 1",
          value = "question1",
          
          h3(
            "Employment, DATA2002/DATA2902 Enrolment, and Weekly Paid Work"
          ),
          
          tabsetPanel(
            id = "q1_tab",
            
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
              
              h4("Test Result"),
              
              verbatimTextOutput("q1_chi_independence"),
              
              h4("Assumption Check"),
              
              verbatimTextOutput("q1_ind_assumptions"),
              
              h4("Interpretation"),
              
              verbatimTextOutput("q1_ind_interpretation")
            ),
            
            tabPanel(
              title = "Chi-square Homogeneity",
              value = "homogeneity",
              
              h4("Chi-square Test of Homogeneity"),
              
              p(
                "This test examines whether the distribution of employment ",
                "status is homogeneous across DATA2002 and DATA2902."
              ),
              
              h4("Observed Counts"),
              
              tableOutput("q1_hom_table"),
              
              h4("Employment Proportions by Course"),
              
              tableOutput("q1_hom_proportions"),
              
              h4("Test Result"),
              
              verbatimTextOutput("q1_chi_homogeneity"),
              
              h4("Interpretation"),
              
              verbatimTextOutput("q1_hom_interpretation")
            ),
            
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
        
        tabPanel(
          title = "QUESTION 2",
          value = "question2",
          
          h3("Two Sample Tests for Student Academics & Lifestyle Differences"),
          
          #first tabset
          tabsetPanel(
            id = "q2_tab",
            
            tabPanel(
              title = "Introduction",
              value = "intro_q2",
              
              h4("Research Question"),
              p("This is where I'd put my research question... if I had one"),
              
              h4("Assumptions"),
              p("And this is where I elaborate on the assumptions for the three tests we did!")
            ),
            
            tabPanel(
              title = "Two Sample test",
              value = "two_sample_q2",
              
              verbatimTextOutput("out2")
            )
          )
        ),
        
        tabPanel(
          title = "QUESTION 3",
          value = "question3",
          
          h3("Question 3 (Monte Carlo Permutation Tests)"),
          
          tabsetPanel(
            id = "q3_tab",
            
            tabPanel(
              title = "Employment Hours",
              value = "q3_perm_employment",
              
              h4("Permutation Test: Employment Hours Difference"),
              p(
                "We use a Monte Carlo permutation test to evaluate if the difference in ",
                "mean weekly employment hours between DATA2002 and DATA2902 students is statistically significant."
              ),
              
              plotOutput("q1_perm_plot"),
              verbatimTextOutput("q1_perm_summary")
            ),
            
            tabPanel(
              title = "Multitasking",
              value = "q3_perm_multitask",
              
              h4("Permutation Test: Multitasking Proportion Shifts"),
              p(
                "We use a Monte Carlo permutation test to determine if the proportion of ",
                "students who multitask shifts significantly based on employment or rent status."
              ),
              
              plotOutput("perm_plot"),
              verbatimTextOutput("perm_summary")
            )
          )
        ),
        
        tabPanel(
          title = "APPENDIX",
          value = "appendix",
          
          h4("AI Usage Statement"),
          p("No/ALL (like every single one) artificial intelligence tools were used in the completion of this project."),
          
          h4("ISI Professional Ethics Statement"),
          p("Shared Value: Throughout our research process, we maintained respect for the privacy of the participants by fairly processing the data, ensuring truthfulness and integrity in  handling statistical results."),
          p("Ethical Principles: To explore our overall research question, we maintained confidence in our statistical analysis and its limitations, ensuring the methods used produced the best possible results."),
          
          h4("References"),
          p("Insert any references in APA 7th format that was used for this report, including coding references")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output$out2 <- renderPrint({
    
    xvar <- input$drop2_xvar
    
    if (input$yvar_type == "Numerical") {
      
      yvar <- input$drop2_yvar_num
      
      f <- as.formula(
        paste(yvar, "~", xvar)
      )
      
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
              (homoscedasticity)."
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
  
  perm_res <- eventReactive(input$run_perm, {
    req(df)
    
    validate(
      need(input$B >= 1 && input$B <= 100000, "Please enter a number of permutations between 1 and 100,000.")
    )
    
    B <- input$B
    group_var <- input$perm_group
    
    dat <- df %>% filter(!is.na(multitask_y_n), !is.na(!!sym(group_var)))
    g_levels <- levels(factor(dat[[group_var]]))
    
    if(length(g_levels) < 2) return(NULL)
    
    g1 <- g_levels[1]
    g2 <- g_levels[2]
    success_level <- if("Yes" %in% levels(dat$multitask_y_n)) "Yes" else levels(dat$multitask_y_n)[1]
    
    obs_p1 <- mean(dat$multitask_y_n[dat[[group_var]] == g1] == success_level, na.rm = TRUE)
    obs_p2 <- mean(dat$multitask_y_n[dat[[group_var]] == g2] == success_level, na.rm = TRUE)
    obs_diff <- obs_p1 - obs_p2
    
    diffs <- replicate(B, {
      shuffled_group <- sample(dat[[group_var]])
      p1 <- mean(dat$multitask_y_n[shuffled_group == g1] == success_level, na.rm = TRUE)
      p2 <- mean(dat$multitask_y_n[shuffled_group == g2] == success_level, na.rm = TRUE)
      p1 - p2
    })
    
    pval <- mean(abs(diffs) >= abs(obs_diff))
    
    list(dist = diffs, obs_diff = obs_diff, pval = pval, success = success_level, group = group_var, g1 = g1, g2 = g2)
  }, ignoreNULL = FALSE)
  
  output$perm_summary <- renderPrint({
    req(perm_res())
    res <- perm_res()
    
    cat("Monte Carlo Permutation Test for difference in proportions (Level: '", res$success, "')\n", sep="")
    cat("Grouping Variable: ", res$group, "\n")
    cat("Test Statistic (Observed Difference): Proportion(", res$g1, ") - Proportion(", res$g2, ")\n\n", sep="")
    
    cat("Observed Difference: ", round(res$obs_diff, 4), "\n")
    cat("Permutation P-value: ", format.pval(res$pval, eps = 1/input$B), "\n\n")
    
    if(res$pval < 0.05) {
      cat("Conclusion: At alpha = 0.05, we reject the null hypothesis. The proportion significantly shifts based on ", res$group, ".\n", sep="")
    } else {
      cat("Conclusion: At alpha = 0.05, we fail to reject the null hypothesis. We do not have sufficient evidence that the proportion significantly shifts based on ", res$group, ".\n", sep="")
    }
  })
  
  output$perm_plot <- renderPlot({
    req(perm_res())
    res <- perm_res()
    df_plot <- data.frame(val = res$dist)
    
    ggplot(df_plot, aes(x = val)) +
      geom_histogram(bins = 30, fill = "#4A90E2", color = "white") +
      geom_vline(xintercept = res$obs_diff, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = -res$obs_diff, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = paste("Null Distribution of Difference in Proportions\n(", res$g1, " vs ", res$g2, ")", sep=""),
        x = "Simulated Difference Under Null Hypothesis",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  q1_perm_res <- eventReactive(input$run_q1_perm, {
    req(df)
    
    validate(
      need(input$q1_B >= 1 && input$q1_B <= 100000, "Please enter a number of permutations between 1 and 100,000.")
    )
    
    B <- input$q1_B
    
    dat <- df %>% filter(!is.na(employment_hours), !is.na(type_2X02))
    
    obs_mean_2002 <- mean(dat$employment_hours[dat$type_2X02 == "DATA2002"], na.rm = TRUE)
    obs_mean_2902 <- mean(dat$employment_hours[dat$type_2X02 == "DATA2902"], na.rm = TRUE)
    obs_diff <- obs_mean_2002 - obs_mean_2902
    
    diffs <- replicate(B, {
      shuffled_type <- sample(dat$type_2X02)
      mean_2002 <- mean(dat$employment_hours[shuffled_type == "DATA2002"], na.rm = TRUE)
      mean_2902 <- mean(dat$employment_hours[shuffled_type == "DATA2902"], na.rm = TRUE)
      mean_2002 - mean_2902
    })
    
    pval <- mean(abs(diffs) >= abs(obs_diff))
    
    list(dist = diffs, obs_diff = obs_diff, pval = pval, g1 = "DATA2002", g2 = "DATA2902")
  }, ignoreNULL = FALSE)
  
  output$q1_perm_summary <- renderPrint({
    req(q1_perm_res())
    res <- q1_perm_res()
    
    cat("Monte Carlo Permutation Test for difference in mean weekly employment hours\n")
    cat("Test Statistic: Mean(", res$g1, ") - Mean(", res$g2, ")\n\n", sep="")
    
    cat("Observed Difference: ", round(res$obs_diff, 4), " hours\n")
    cat("Permutation P-value: ", format.pval(res$pval, eps = 1/input$q1_B), "\n\n")
    
    if(res$pval < 0.05) {
      cat("Conclusion: At alpha = 0.05, we reject the null hypothesis. There is a significant difference in working hours between the cohorts.\n")
    } else {
      cat("Conclusion: At alpha = 0.05, we fail to reject the null hypothesis. We do not have sufficient evidence to claim a difference in working hours between the cohorts.\n")
    }
  })
  
  output$q1_perm_plot <- renderPlot({
    req(q1_perm_res())
    res <- q1_perm_res()
    
    df_plot <- data.frame(val = res$dist)
    
    ggplot(df_plot, aes(x = val)) +
      geom_histogram(bins = 30, fill = "#F5A623", color = "white") +
      geom_vline(xintercept = res$obs_diff, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = -res$obs_diff, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = "Null Distribution of Difference in Mean Employment Hours\n(DATA2002 - DATA2902)",
        x = "Simulated Difference Under Null Hypothesis",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
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
  # QUESTION 1 - CHI-SQUARE ASSUMPTION CHECK
  # -------------------------------------------------------
  output$q1_ind_assumptions <- renderPrint({
    
    expected <- q1_ind_test()$expected
    
    total_cells <- length(expected)
    cells_below_5 <- sum(expected < 5)
    cells_below_1 <- sum(expected < 1)
    percent_below_5 <- (cells_below_5 / total_cells) * 100
    
    cat("Expected Cell Count Check\n\n")
    
    cat(
      "Cells with expected count below 5:",
      cells_below_5,
      "out of",
      total_cells,
      paste0("(", round(percent_below_5, 1), "%)"),
      "\n"
    )
    
    cat(
      "Cells with expected count below 1:",
      cells_below_1,
      "\n\n"
    )
    
    if (cells_below_1 == 0 && percent_below_5 <= 20) {
      
      cat(
        "Conclusion: The expected-count condition for the ",
        "chi-square approximation is satisfied."
      )
      
    } else {
      
      cat(
        "Conclusion: The expected-count condition may not be satisfied. ",
        "The Pearson chi-square approximation should therefore be ",
        "interpreted with caution."
      )
    }
  })
  
  
  # -------------------------------------------------------
  # QUESTION 1 - CHI-SQUARE INTERPRETATION
  # -------------------------------------------------------
  output$q1_ind_interpretation <- renderPrint({
    
    test <- q1_ind_test()
    
    p_value <- test$p.value
    
    cat(
      "Selected variables:",
      input$q1_ind_x,
      "and",
      input$q1_ind_y,
      "\n\n"
    )
    
    cat(
      "Chi-square statistic:",
      round(unname(test$statistic), 3),
      "\n"
    )
    
    cat(
      "Degrees of freedom:",
      unname(test$parameter),
      "\n"
    )
    
    cat(
      "P-value:",
      format.pval(p_value, digits = 4, eps = 0.0001),
      "\n\n"
    )
    
    if (p_value < 0.05) {
      
      cat(
        "Conclusion: At alpha = 0.05, we reject the null hypothesis. ",
        "There is statistically significant evidence of an association ",
        "between ",
        input$q1_ind_x,
        " and ",
        input$q1_ind_y,
        ".",
        sep = ""
      )
      
    } else {
      
      cat(
        "Conclusion: At alpha = 0.05, we fail to reject the null hypothesis. ",
        "There is insufficient evidence of an association between ",
        input$q1_ind_x,
        " and ",
        input$q1_ind_y,
        ".",
        sep = ""
      )
    }
  })
  
  
  # -------------------------------------------------------
  # QUESTION 1 - CHI-SQUARE HOMOGENEITY
  # -------------------------------------------------------
  q1_hom_table <- reactive({
    
    table(
      df$type_2X02,
      df$employment_y_n,
      dnn = c("Course enrolment", "Employment status")
    )
  })
  
  
  q1_hom_test <- reactive({
    
    chisq.test(
      q1_hom_table(),
      correct = FALSE
    )
  })
  
  
  output$q1_hom_table <- renderTable({
    q1_hom_table()
  }, rownames = TRUE)
  
  
  output$q1_hom_proportions <- renderTable({
    
    round(
      prop.table(
        q1_hom_table(),
        margin = 1
      ),
      3
    )
    
  }, rownames = TRUE)
  
  
  output$q1_chi_homogeneity <- renderPrint({
    q1_hom_test()
  })
  
  
  output$q1_hom_interpretation <- renderPrint({
    
    test <- q1_hom_test()
    p_value <- test$p.value
    
    cat(
      "Null hypothesis: The distribution of paid employment status ",
      "is the same for DATA2002 and DATA2902 students.\n\n"
    )
    
    cat(
      "Alternative hypothesis: The distribution of paid employment ",
      "status differs between DATA2002 and DATA2902 students.\n\n"
    )
    
    cat(
      "Chi-square statistic:",
      round(unname(test$statistic), 3),
      "\n"
    )
    
    cat(
      "Degrees of freedom:",
      unname(test$parameter),
      "\n"
    )
    
    cat(
      "P-value:",
      format.pval(p_value, digits = 4, eps = 0.0001),
      "\n\n"
    )
    
    if (p_value < 0.05) {
      
      cat(
        "Conclusion: At alpha = 0.05, we reject the null hypothesis. ",
        "There is statistically significant evidence that the distribution ",
        "of paid employment status is not homogeneous across DATA2002 ",
        "and DATA2902 students."
      )
      
    } else {
      
      cat(
        "Conclusion: At alpha = 0.05, we fail to reject the null hypothesis. ",
        "There is insufficient evidence that the distribution of paid ",
        "employment status differs between DATA2002 and DATA2902 students."
      )
    }
  })
  
  
  output$q1_cmh <- renderPrint({
    "Cochran-Mantel-Haenszel test will be implemented in a separate commit."
  })
}

shinyApp(
  ui = ui,
  server = server
)