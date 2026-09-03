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
  "Commute to Campus Time (mins)" = "travel_to_campus_min",
  "Weekly Rent (dollars)" = "rent_dollars",
  "Employment Hours (hours)" = "employment_hours",
  "Sleep Duration (hours)" = "sleep_hours",
  "Water Consumption (litres)" = "water_l"
)

nominal_choices <- c(
  "International/Domestic Student Status" = "intl_dom_type",
  "DATA2X02 Enrollment" = "type_2X02",
  "Assessment Workflow" = "assessment_workflow",
  "Grouped Workflow" = "workflow_grouped",
  "Rental Status (y/n)" = "rent_y_n",
  "Employment Status (y/n)" = "employment_y_n",
  "Multitasking Status (y/n)" = "multitask_y_n"
)

ordinal_choices <- c(
  "DATA1X01 Grade" = "grade_1X01",
  "Aspirational DATA2X02 Grade" = "grade_2X02"
)


hatpc_panel <- function(
  hypothesis,
  assumptions,
  test_statistic,
  p_value,
  conclusion
) {
  tags$div(
    class = "hatpc-panel",

    h4("HATPC Framework"),

    tags$div(
      class = "hatpc-step",
      tags$strong("H — Hypothesis"),
      hypothesis
    ),

    tags$div(
      class = "hatpc-step",
      tags$strong("A — Assumptions"),
      assumptions
    ),

    tags$div(
      class = "hatpc-step",
      tags$strong("T — Test Statistic"),
      test_statistic
    ),

    tags$div(
      class = "hatpc-step",
      tags$strong("P — P-value"),
      p_value
    ),

    tags$div(
      class = "hatpc-step",
      tags$strong("C — Conclusion"),
      conclusion
    )
  )
}

ui <- fluidPage(

  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style.css"
    )
  ),

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

          hatpc_panel(
            hypothesis = uiOutput("q1_ind_hatpc_hypothesis"),

            assumptions = tagList(
              p("Observations are independent: each row represents an individual student's survey response."),
              p("Both variables are categorical: course enrolment is DATA2002/DATA2902 and employment status is yes/no."),
              p("Expected cell counts must be sufficiently large for the chi-square approximation to be appropriate.")
            ),

            test_statistic = tagList(
              p("Pearson's χ² statistic compares the observed course-employment counts with the counts expected under independence."),
              uiOutput("q1_ind_hatpc_stat")
            ),

            p_value = uiOutput("q1_ind_hatpc_p"),

            conclusion = p(
              "If p < 0.05, reject H₀ and conclude that course enrolment and employment status are associated in this sample. Otherwise, fail to reject H₀."
            )
          ),

          tags$hr(),

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

          hatpc_panel(
            hypothesis = tagList(
              p("H₀: The distribution of paid employment status is the same for DATA2002 and DATA2902 students."),
              p("H₁: The distribution of paid employment status differs between the two courses."),
              p("This compares the proportion of employed and non-employed students across the two course groups.")
            ),

            assumptions = tagList(
              p("Observations are independent: each row represents an individual student's survey response."),
              p("Course enrolment defines the groups, while employment status is the categorical response being compared."),
              p("Expected cell counts must be sufficiently large for the chi-square approximation.")
            ),

            test_statistic = tagList(
              p("Pearson's χ² statistic measures how far the observed employment counts in each course differ from the counts expected if both courses had the same employment distribution."),
              uiOutput("q1_hom_hatpc_stat")
            ),

            p_value = uiOutput("q1_hom_hatpc_p"),

            conclusion = p(
              "If p < 0.05, reject H₀ and conclude that the employment distribution differs between DATA2002 and DATA2902 students. Otherwise, fail to reject H₀."
            )
          ),

          tags$hr(),

          tags$strong("Chi-square Test of Homogeneity"),

          p(
            "Compares the distribution of employment status across courses."
          )
        ),

        conditionalPanel(
          condition = "input.q1_tab == 'cmh'",

          hatpc_panel(
            hypothesis = tagList(
              p("H₀: After controlling for Domestic/International status, course enrolment and paid employment status are conditionally independent."),
              p("H₁: An association remains after controlling for Domestic/International status."),
              p("This checks whether the course-employment relationship remains after accounting for differences in student status.")
            ),

            assumptions = tagList(
              p("Observations are independent: each row represents an individual student's survey response."),
              p("Course enrolment and employment status are categorical, while Domestic/International status defines the strata."),
              p("The course-employment association should be reasonably comparable across the Domestic and International strata.")
            ),

            test_statistic = tagList(
              p("The Cochran-Mantel-Haenszel χ² statistic measures the association between course and employment after combining information across the Domestic and International strata."),
              uiOutput("q1_cmh_hatpc_stat")
            ),

            p_value = uiOutput("q1_cmh_hatpc_p"),

            conclusion = p(
              "If p < 0.05, reject H₀ and conclude that the association remains after controlling for Domestic/International status. Otherwise, fail to reject H₀."
            )
          ),

          tags$hr(),

          tags$strong("Cochran-Mantel-Haenszel Chi-square Test"),

          p(
            "Controls for Domestic/International student status."
          ),

          radioButtons(
            inputId = "q1_cmh_view",
            label = "View contingency table for:",
            choices = c(
              "Combined" = "Combined",
              levels(df$intl_dom_type)
            ),
            selected = "Combined"
          ),

          p(
            "The selected view is for exploration only. ",
            "The CMH test will use all Domestic/International strata together."
          )
        )
      ),

      
      #-----------
      # QUESTION 2
      #-----------
      conditionalPanel(
        condition = "input.selected_tab == 'question2'",

        #conditionalPanel(
        #  condition = "input.q2_tab == 'two_sample_q2'",

          h4("Two Sample Mean Test Options"),
          p("Please choose one grouping variable and one numerical/ordinal variable."),
          
          selectInput(
            inputId = "drop2_xvar",
            label = "Grouping Variables:",
            choices = nominal_choices[-3]
            #selected = "intl_dom_type"
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
              choices = numeric_choices
              #selected = "travel_to_campus_min"
            )
          ),

          conditionalPanel(
            condition = "input.yvar_type == 'Ordinal'",

            selectInput(
              inputId = "drop2_yvar_ord",
              label = "Ordinal Variables:",
              choices = ordinal_choices
              #selected = "grade_1X01"
            )
          )
        ),
      #),

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
          h3("Critique of Survey Questions"),
          p(
            "The survey also contained inconsistencies in how key variables were recorded, particularly in the use of both numerical and Boolean formats for some items but not others.
            For example, rent was assessed through a numerical amount and a separate yes/no question, whereas employment was captured solely through weekly hours. 
            This uneven structure resulted in contradictory entries that needed to be fixed later on \"no\" rent paired with a positive rent value, or vice versa. 
            Future surveys would benefit from standardising the response format to one numerical field with instructions to leave non‑applicable items at 0 would improve clarity and data reliability."
          ),
          
          p(
            "Additionally, students were allowed to select “Other” instead of the three provided workflow groups, but the survey did not include a space for them to describe what they actually do. 
            This makes “Other” responses ambiguous and limits their usefulness. A simple free‑text field would have helped capture more accurate and meaningful distinctions among students’ assessment habits."
          ),
          
          h3("Data Cleaning"),
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
            such as more than 20 hours (as it substantially exceeds normal adult and most clinical hypersomnia ranges (Bollu et al., 2018), or exactly zero hours a day.
            Although a subset of observations reported water consumption values far above the typical range, manual inspection of such records indicated no inconsistencies in the accompanying data.
            Given the plausibility of genuine high intake, such as due to medical or lifestyle factors, these observations were retained rather than treated as errors. 
            Similarly, entries reporting 0 litres of daily water consumption were retained, as it may reflect individuals who hydrate through other beverages and therefore did not report any “plain water” consumption."
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

              uiOutput("q1_ind_research_question"),

              h4("Contingency Table Display"),

              tableOutput("q1_ind_table"),

              h4("Test Result"),

              verbatimTextOutput("q1_chi_independence"),

              h4("Chi-square Distribution"),

              plotOutput("q1_ind_chisq_plot"),

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

              h4("Chi-square Distribution"),

              plotOutput("q1_hom_chisq_plot"),

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

              h4("Contingency Table by Student Status"),

              tableOutput("q1_cmh_view_table"),

              h4("Test Result"),

              verbatimTextOutput("q1_cmh"),

              h4("Chi-square Distribution"),

              plotOutput("q1_cmh_chisq_plot"),

              h4("Adjusted Interpretation"),

              verbatimTextOutput("q1_cmh_interpretation"),

              h4("Unadjusted vs Adjusted Comparison"),

              verbatimTextOutput("q1_cmh_comparison")
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
              p("Is there a difference in the means of student's", 
                textOutput("chosen_var", inline = TRUE),
              "when divided by", 
              textOutput("grouping_var", inline = TRUE),"?"),
              
              p("To evaluate the research question based on the selected variables, 
                our program detects which two sample test is most suitable following the assumptions:"),

              
              tags$strong("Variables used:"),
              
              tags$ul(
                tags$li("Course enrolment"),
                tags$li("Paid employment status"),
                tags$li("Domestic/International student status")
              )
            ),
            
              h5("Assumptions"),
              h5("Student Two Sample T-Test"),
              tags$ul(
              tags$li("Quantitative: data values collected are quantitative."),
              tags$li("Independence within and between groups: each row represents an individual student's survey response."),
              tags$li("Homogeneity of variance: population variances of both groups are equal, checked using Levene's test."),
              tags$li("Normality: Central Limit Theorem applies as n ≥ 30 for both groups of all categories.")),
              
              h5("Welch Two Sample T-Test"),
              p("Quantitative: data values collected are quantitative."),
              p("Independence within and between groups: each row represents an individual student's survey response."),
              p("Normality: Central Limit Theorem applies as n ≥ 30 for both groups of all categories."),     
              
              h5("Wilcoxon Rank-Sum Test"),
              p("Independence: groups are independent of one another."),
              p("Same Distribution: groups follow the same distribution, differing by shift."),
              
              p("If the user chose a numerical variable, then the program will run a two sample t-test. We check if the groups have equal variances via a Levene's test.
                If the p-value is small (< 0.05), we conduct a Student t-test; otherwise, we conduct a Welch t-test. For ordinal data, the Wilcoxon Rank-Sum test is conducted.")
              
            ),

            tabPanel(
              title = "Two Sample test",
              value = "two_sample_q2",

              uiOutput("out2")
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
          p("Insert any references in APA 7th format that was used for this report, including coding references"),
        
          p("Australian Government. (n.d.). Hours of work. Fair Work Ombudsman. Retrieved 2 September 2026, from https://www.fairwork.gov.au/employment-conditions/hours-of-work-breaks-and-rosters/hours-of-work"),
          p("Bollu, P. C., Manjamalai, S., Thakkar, M., & Sahota, P. (2018). Hypersomnia. Missouri Medicine, 115(1), 85–91.")
        )
      )
    )
  )
)

server <- function(input, output) {
  # -------------------------------------------------------
  # QUESTION 2 - INTRODUCTION
  # -------------------------------------------------------  
  output$grouping_var <- renderText({
    names(nominal_choices)[nominal_choices == input$drop2_xvar]
  })
  
  output$chosen_var <- renderText({
    #identify which raw name was selected based on the active radio button
    raw_val <- if (input$yvar_type == "Numerical") input$drop2_yvar_num else input$drop2_yvar_ord
      
    #combine lists to map backend name back to its display label
    all_choices <- c(numeric_choices, ordinal_choices)
    clean_label <- names(all_choices)[all_choices == raw_val]
      
    return(clean_label)
  })
  
  # -------------------------------------------------------
  # QUESTION 2 - TWO SAMPLE T-TEST
  # -------------------------------------------------------
  output$out2 <- renderUI({
    
    alpha <- 0.05
    
    # Unicode glyphs used in the write-up
    mu   <- "μ"   # μ
    sig  <- "σ"   # σ
    neq  <- "≠"   # ≠
    sim  <- "∼"   # ∼
    dots <- "…"   # …
    
    hypothesis <- glue("H₀: {mu}x = {mu}y H₁: {mu}x {neq} {mu}y")
    
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
      
      if (p_equal_var < alpha) {
        test_type <- "welch"
        test_name <- "Welch's t-test"
        tt <- df |>
          t.test(
            formula = f,
            data = _
          )
      } else {
        test_type <- "student"
        test_name <- "Student's t-test"
        tt <- df |>
          t.test(
            formula = f,
            var.equal = TRUE,
            data = _
          )
      }
      
      stat <- signif(unname(tt$statistic), 4)
      dof  <- signif(unname(tt$parameter), 4)
      pval <- signif(tt$p.value, 4)
      lev  <- signif(p_equal_var, 4)
      
      reject <- tt$p.value < alpha
      
      if (test_type == "student") {
        
        indep_clause <- if (reject) {
          "Xi and Yi are independent and have equal variance."
        } else {
          "Xi and Yi are independent."
        }
        
        variance_clause <- glue(
          "Using Levene's test, as {lev} is greater than 0.05, X and Y have similar ",
          "variances and the Student's t-test is used."
        )
        
      } else {
        
        indep_clause <- "Xi and Yi are independent and have equal variance."
        
        variance_clause <- glue(
          "Using Levene's test, as {lev} is less than 0.05, X and Y have different ",
          "variances and the Welch's t-test is used."
        )
      }
      
      assumptions <- glue(
        "X1, {dots} , Xnx are independent and identically distributed with the ",
        "distribution N ({mu}X, {sig}2), Y1, {dots} , Yny are independent and ",
        "identically distributed with the distribution N ({mu}Y , {sig}2), and ",
        "{indep_clause} {variance_clause}"
      )
      
      test_statistic <- glue(
        "Under H₀,  T{sim} t{dof}. The test statistic is {stat}."
      )
      
      conclusion <- if (reject) {
        "Since the p-value < 0.05, we reject H₀."
      } else {
        "Since the p-value > 0.05, we do not reject H₀."
      }
      
    } else {
      
      yvar <- input$drop2_yvar_ord
      
      f <- as.formula(
        paste(yvar, "~", xvar)
      )
      
      test_name <- "Wilcoxon Rank Sum Test"
      
      wt <- wilcox.test(
        formula = f,
        data = df
      )
      
      stat <- signif(unname(wt$statistic), 4)
      pval <- signif(wt$p.value, 4)
      
      reject <- wt$p.value < alpha
      
      assumptions <- "Xi and Yi are independent and follow the same distribution but differ by a shift."
      
      test_statistic <- glue(
        "Under H₀,  W follows the WRS(nX, nY ) distribution. ",
        "The test statistic is {stat}."
      )
      
      conclusion <- if (reject) {
        "Since the p-value < 0.05, we reject H₀."
      } else {
        "Since the p-value > 0.05, we do not reject H₀."
      }
    }
    
    tags$div(
      class = "hatpc-panel",
      
      h4(paste0("Test performed: ", test_name)),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Significance level"),
        p("alpha = 0.05")
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Hypothesis"),
        p(hypothesis)
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Assumptions"),
        p(assumptions)
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Test-statistic"),
        p(test_statistic)
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("p-value"),
        p(paste0("The p-value is ", pval, "."))
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Conclusion"),
        p(conclusion)
      )
    )
  })

  # -------------------------------------------------------
  # QUESTION 3 - RESAMPLING MONTE CARLO
  # -------------------------------------------------------
  
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

  q1_ind_variable_label <- function(variable) {

    labels <- c(
      "intl_dom_type" = "Domestic/International student status",
      "type_2X02" = "DATA2002/DATA2902 enrolment",
      "assessment_workflow" = "assessment workflow",
      "workflow_grouped" = "grouped assessment workflow",
      "rent_y_n" = "rent status",
      "employment_y_n" = "paid employment status",
      "multitask_y_n" = "multitasking status"
    )

    unname(labels[variable])
  }


  output$q1_ind_hatpc_hypothesis <- renderUI({

    req(input$q1_ind_x, input$q1_ind_y)

    x_label <- q1_ind_variable_label(input$q1_ind_x)
    y_label <- q1_ind_variable_label(input$q1_ind_y)

    tagList(
      p(
        paste0(
          "H₀: ",
          x_label,
          " and ",
          y_label,
          " are independent."
        )
      ),
      p(
        paste0(
          "H₁: ",
          x_label,
          " and ",
          y_label,
          " are associated."
        )
      ),
      p(
        paste0(
          "This tests whether ",
          x_label,
          " is associated with ",
          y_label,
          "."
        )
      )
    )
  })


  output$q1_ind_research_question <- renderUI({

    req(input$q1_ind_x, input$q1_ind_y)

    x_label <- q1_ind_variable_label(input$q1_ind_x)
    y_label <- q1_ind_variable_label(input$q1_ind_y)

    p(
      paste0(
        "This test examines whether ",
        x_label,
        " is associated with ",
        y_label,
        "."
      )
    )
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


  output$q1_ind_hatpc_stat <- renderUI({
    test <- q1_ind_test()

    p(
      paste0(
        "χ² = ",
        round(unname(test$statistic), 3)
      )
    )
  })


  output$q1_ind_hatpc_p <- renderUI({
    test <- q1_ind_test()

    p(
      paste0(
        "p = ",
        format.pval(
          test$p.value,
          digits = 4,
          eps = 0.0001
        )
      )
    )
  })


  output$q1_ind_chisq_plot <- renderPlot({

    test <- q1_ind_test()

    df_chi <- unname(test$parameter)
    stat <- unname(test$statistic)
    p_value <- test$p.value

    x_max <- max(
      qchisq(0.999, df = df_chi),
      stat * 1.15
    )

    x <- seq(
      0.01,
      x_max,
      length.out = 1000
    )

    y <- dchisq(
      x,
      df = df_chi
    )

    plot(
      x,
      y,
      type = "l",
      lwd = 2,
      xlab = expression(chi^2),
      ylab = "Density",
      main = paste(
        "Chi-square Distribution (df =",
        df_chi,
        ")"
      )
    )

    tail_x <- seq(
      stat,
      x_max,
      length.out = 500
    )

    tail_y <- dchisq(
      tail_x,
      df = df_chi
    )

    polygon(
      c(stat, tail_x, x_max),
      c(0, tail_y, 0),
      col = adjustcolor(
        "red",
        alpha.f = 0.25
      ),
      border = NA
    )

    lines(
      x,
      y,
      lwd = 2
    )

    abline(
      v = stat,
      col = "red",
      lty = 2,
      lwd = 2
    )

    legend(
      "topright",
      legend = c(
        paste0("Observed χ² = ", round(stat, 3)),
        paste0("Right-tail p = ", format.pval(p_value, digits = 4))
      ),
      lty = c(2, NA),
      lwd = c(2, NA),
      col = c("red", NA),
      bty = "n"
    )
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


  output$q1_hom_hatpc_stat <- renderUI({
    test <- q1_hom_test()

    p(
      paste0(
        "χ² = ",
        round(unname(test$statistic), 3)
      )
    )
  })


  output$q1_hom_hatpc_p <- renderUI({
    test <- q1_hom_test()

    p(
      paste0(
        "p = ",
        format.pval(
          test$p.value,
          digits = 4,
          eps = 0.0001
        )
      )
    )
  })


  output$q1_hom_chisq_plot <- renderPlot({

    test <- q1_hom_test()

    df_chi <- unname(test$parameter)
    stat <- unname(test$statistic)
    p_value <- test$p.value

    x_max <- max(
      qchisq(0.999, df = df_chi),
      stat * 1.15
    )

    x <- seq(
      0.01,
      x_max,
      length.out = 1000
    )

    y <- dchisq(
      x,
      df = df_chi
    )

    plot(
      x,
      y,
      type = "l",
      lwd = 2,
      xlab = expression(chi^2),
      ylab = "Density",
      main = paste(
        "Chi-square Distribution (df =",
        df_chi,
        ")"
      )
    )

    tail_x <- seq(
      stat,
      x_max,
      length.out = 500
    )

    tail_y <- dchisq(
      tail_x,
      df = df_chi
    )

    polygon(
      c(stat, tail_x, x_max),
      c(0, tail_y, 0),
      col = adjustcolor(
        "red",
        alpha.f = 0.25
      ),
      border = NA
    )

    lines(
      x,
      y,
      lwd = 2
    )

    abline(
      v = stat,
      col = "red",
      lty = 2,
      lwd = 2
    )

    legend(
      "topright",
      legend = c(
        paste0("Observed χ² = ", round(stat, 3)),
        paste0("Right-tail p = ", format.pval(p_value, digits = 4))
      ),
      lty = c(2, NA),
      lwd = c(2, NA),
      col = c("red", NA),
      bty = "n"
    )
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


  # -------------------------------------------------------
  # QUESTION 1 - CMH STRATUM VIEW
  # -------------------------------------------------------
  q1_cmh_view_table <- reactive({

    req(input$q1_cmh_view)

    if (input$q1_cmh_view == "Combined") {

      table(
        df$type_2X02,
        df$employment_y_n,
        dnn = c("Course enrolment", "Employment status")
      )

    } else {

      cmh_subset <- df %>%
        filter(intl_dom_type == input$q1_cmh_view)

      table(
        cmh_subset$type_2X02,
        cmh_subset$employment_y_n,
        dnn = c("Course enrolment", "Employment status")
      )
    }
  })


  output$q1_cmh_view_table <- renderTable({
    q1_cmh_view_table()
  }, rownames = TRUE)


  # -------------------------------------------------------
  # QUESTION 1 - CMH 3D CONTINGENCY TABLE
  # -------------------------------------------------------
  q1_cmh_table <- reactive({

    table(
      df$type_2X02,
      df$employment_y_n,
      df$intl_dom_type,
      dnn = c(
        "Course enrolment",
        "Employment status",
        "Domestic/International status"
      )
    )
  })


  # -------------------------------------------------------
  # QUESTION 1 - COCHRAN-MANTEL-HAENSZEL TEST
  # -------------------------------------------------------
  q1_cmh_test <- reactive({

    cmh_table <- q1_cmh_table()

    validate(
      need(
        length(dim(cmh_table)) == 3,
        "The CMH analysis requires a three-dimensional contingency table."
      ),
      need(
        dim(cmh_table)[1] == 2 &&
          dim(cmh_table)[2] == 2,
        "The CMH test requires two levels for course enrolment and two levels for employment status."
      ),
      need(
        dim(cmh_table)[3] >= 2,
        "The CMH test requires at least two Domestic/International strata."
      )
    )

    mantelhaen.test(
      cmh_table,
      correct = FALSE
    )
  })


  output$q1_cmh <- renderPrint({
    q1_cmh_test()
  })


  output$q1_cmh_hatpc_stat <- renderUI({
    test <- q1_cmh_test()

    p(
      paste0(
        "χ² = ",
        round(unname(test$statistic), 3)
      )
    )
  })


  output$q1_cmh_hatpc_p <- renderUI({
    test <- q1_cmh_test()

    p(
      paste0(
        "p = ",
        format.pval(
          test$p.value,
          digits = 4,
          eps = 0.0001
        )
      )
    )
  })


  output$q1_cmh_chisq_plot <- renderPlot({

    test <- q1_cmh_test()

    df_chi <- unname(test$parameter)
    stat <- unname(test$statistic)
    p_value <- test$p.value

    x_max <- max(
      qchisq(0.999, df = df_chi),
      stat * 1.15
    )

    x <- seq(
      0.01,
      x_max,
      length.out = 1000
    )

    y <- dchisq(
      x,
      df = df_chi
    )

    plot(
      x,
      y,
      type = "l",
      lwd = 2,
      xlab = expression(chi^2),
      ylab = "Density",
      main = paste(
        "CMH Chi-square Distribution (df =",
        df_chi,
        ")"
      )
    )

    tail_x <- seq(
      stat,
      x_max,
      length.out = 500
    )

    tail_y <- dchisq(
      tail_x,
      df = df_chi
    )

    polygon(
      c(stat, tail_x, x_max),
      c(0, tail_y, 0),
      col = adjustcolor(
        "red",
        alpha.f = 0.25
      ),
      border = NA
    )

    lines(
      x,
      y,
      lwd = 2
    )

    abline(
      v = stat,
      col = "red",
      lty = 2,
      lwd = 2
    )

    legend(
      "topright",
      legend = c(
        paste0("Observed χ² = ", round(stat, 3)),
        paste0("Right-tail p = ", format.pval(p_value, digits = 4))
      ),
      lty = c(2, NA),
      lwd = c(2, NA),
      col = c("red", NA),
      bty = "n"
    )
  })


  # -------------------------------------------------------
  # QUESTION 1 - CMH ADJUSTED INTERPRETATION
  # -------------------------------------------------------
  output$q1_cmh_interpretation <- renderPrint({

    test <- q1_cmh_test()
    p_value <- test$p.value

    cat(
      "Null hypothesis: After controlling for Domestic/International ",
      "student status, course enrolment and paid employment status ",
      "are conditionally independent.\n\n"
    )

    cat(
      "Alternative hypothesis: After controlling for Domestic/International ",
      "student status, course enrolment and paid employment status ",
      "remain associated.\n\n"
    )

    cat(
      "CMH chi-square statistic:",
      round(unname(test$statistic), 3),
      "\n"
    )

    cat(
      "Degrees of freedom:",
      unname(test$parameter),
      "\n"
    )

    cat(
      "Adjusted P-value:",
      format.pval(
        p_value,
        digits = 4,
        eps = 0.0001
      ),
      "\n"
    )

    if (!is.null(test$estimate)) {

      cat(
        "Common odds ratio estimate:",
        round(unname(test$estimate), 3),
        "\n"
      )
    }

    if (!is.null(test$conf.int)) {

      cat(
        "95% confidence interval for the common odds ratio:",
        paste0(
          "(",
          round(test$conf.int[1], 3),
          ", ",
          round(test$conf.int[2], 3),
          ")"
        ),
        "\n"
      )
    }

    cat("\n")

    if (p_value < 0.05) {

      cat(
        "Conclusion: At alpha = 0.05, we reject the null hypothesis. ",
        "There is statistically significant evidence of an association ",
        "between course enrolment and paid employment status even after ",
        "controlling for Domestic/International student status."
      )

    } else {

      cat(
        "Conclusion: At alpha = 0.05, we fail to reject the null hypothesis. ",
        "After controlling for Domestic/International student status, ",
        "there is insufficient evidence of an association between course ",
        "enrolment and paid employment status."
      )
    }
  })


  # -------------------------------------------------------
  # QUESTION 1 - UNADJUSTED VS ADJUSTED COMPARISON
  # -------------------------------------------------------
  output$q1_cmh_comparison <- renderPrint({

    unadjusted_test <- q1_hom_test()
    adjusted_test <- q1_cmh_test()

    unadjusted_p <- unadjusted_test$p.value
    adjusted_p <- adjusted_test$p.value

    cat("Unadjusted Pearson chi-square analysis\n")

    cat(
      "P-value:",
      format.pval(
        unadjusted_p,
        digits = 4,
        eps = 0.0001
      ),
      "\n\n"
    )

    cat(
      "CMH analysis adjusted for Domestic/International status\n"
    )

    cat(
      "P-value:",
      format.pval(
        adjusted_p,
        digits = 4,
        eps = 0.0001
      ),
      "\n\n"
    )

    if (
      unadjusted_p < 0.05 &&
      adjusted_p < 0.05
    ) {

      cat(
        "Comparison: Both the unadjusted and adjusted analyses are ",
        "statistically significant. The evidence of an association ",
        "between course enrolment and employment status remains after ",
        "controlling for Domestic/International student status."
      )

    } else if (
      unadjusted_p < 0.05 &&
      adjusted_p >= 0.05
    ) {

      cat(
        "Comparison: The unadjusted association is statistically ",
        "significant, but the adjusted CMH association is not. ",
        "This suggests that differences in the Domestic/International ",
        "student mix may help explain the unadjusted association."
      )

    } else if (
      unadjusted_p >= 0.05 &&
      adjusted_p < 0.05
    ) {

      cat(
        "Comparison: The unadjusted analysis is not statistically ",
        "significant, but the adjusted CMH analysis is. Controlling for ",
        "Domestic/International student status reveals evidence of an ",
        "association that was not apparent in the unadjusted analysis."
      )

    } else {

      cat(
        "Comparison: Neither the unadjusted nor the adjusted analysis ",
        "is statistically significant. There is insufficient evidence ",
        "of an association between course enrolment and employment status ",
        "before or after controlling for Domestic/International student status."
      )
    }
  })
}

shinyApp(
  ui = ui,
  server = server
)