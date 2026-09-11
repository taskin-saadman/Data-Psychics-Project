library(shiny)
library(tidyverse)
library(rstatix)
library(glue)
library(plotly)
library(bslib)

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

#global constants initialisation
# Unicode glyphs used in the write-up
mu   <- "μ"   # μ
sig  <- "σ"   # σ
neq  <- "≠"   # ≠
sim  <- "∼"   # ∼
dots <- "…"   # …
alp  <- "α"   # α

# Subscripted sample observations used in the assumptions
X1  <- "X<sub>1</sub>"
X2  <- "X<sub>2</sub>"
Xi  <- "X<sub>i</sub>"
Xnx <- "X<sub>n<sub>X</sub></sub>"
Y1  <- "Y<sub>1</sub>"
Y2  <- "Y<sub>2</sub>"
Yi  <- "Y<sub>i</sub>"
Yny <- "Y<sub>n<sub>Y</sub></sub>"

# Distribution parameters used in the assumptions
muX  <- glue("{mu}<sub>X</sub>")
muY  <- glue("{mu}<sub>Y</sub>")
sig2 <- glue("{sig}<sup>2</sup>")
nX   <- "n<sub>X</sub>"
nY   <- "n<sub>Y</sub>"

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

# General-purpose helper: wraps plain / captured console-style text output
# inside the animated hatpc-panel / hatpc-step card so that every test
# result across the app shares the same HATPC visual treatment.
hatpc_text_output <- function(title, text) {
  tags$div(
    class = "hatpc-panel",
    tags$div(
      class = "hatpc-step",
      tags$strong(title),
      tags$pre(text)
    )
  )
}

ui <- fluidPage(
  theme = bs_theme(version = 5),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style.css"
    )
  ),
  
  titlePanel("DATA2902 - Exploring Associations between Student Lifestyle and Academic Factors"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      conditionalPanel(
        condition = "input.selected_tab == 'intro'",
        
        h4("Group Members - Data Psychics (L91G07)"),
        tags$ul(
          tags$li("Ahnaf Tahmid Bhuiyan, 560268744"),
          tags$li("Fanxi Liu (Felix), 541011817"),
          tags$li("Nadhira Dania Nazaruddin, 550245287"),
          tags$li("Sienna Grasso, 540491131"),
          tags$li("Taskin Saadman, 541025803"),
          tags$li("Tuhin Ajay Biswas, 550540584")
        )
      ),
      
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
        
        h4("Two Sample Mean Test Options"),
        p("Please choose one grouping variable and one numerical/ordinal variable."),
        
        selectInput(
          inputId = "drop2_xvar",
          label = "Grouping Variables:",
          choices = nominal_choices[-3]
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
          )
        ),
        
        conditionalPanel(
          condition = "input.yvar_type == 'Ordinal'",
          
          selectInput(
            inputId = "drop2_yvar_ord",
            label = "Ordinal Variables:",
            choices = ordinal_choices
          )
        ),
        
        p("Please select which alternative hypothesis to perform."),
        radioButtons(
          inputId = "q2_alternative",
          label = "Alternative hypothesis:",
          choices = c(
            "Two-sided (≠)"     = "two.sided",
            "Greater than (>)"  = "greater",
            "Less than (<)"     = "less"
          ),
          selected = "two.sided"
        )
      ),
      
      conditionalPanel(
        condition = "input.selected_tab == 'question3'",
        
        conditionalPanel(
          condition = "input.q3_tab == 'intro_q3'",
          h4("Question 3"),
          p(
            "The variables for this analysis are fixed by the research question."
          ),
          
          tags$strong("Variables used:"),
          
          tags$ul(
            tags$li("Rental status"),
            tags$li("DATA1X01/DATA2X02 grade"),
            tags$li("Sleep hours"),
            tags$li("DATA2X02 enrolment")
        )
      ),
        
        conditionalPanel(
          condition = "input.q3_tab == 'q3_perm_rent'",
          
          h4("Monte Carlo simulation Options"),
          p("Permutation test for the difference in mean grade between students who pay rent and those who do not."),
          
          selectInput(
            inputId = "q3_grade_choice",
            label = "Grade Variable:",
            choices = c("DATA1X01" = "grade_1X01", "DATA2X02" = "grade_2X02")
          ),
          
          numericInput(
            inputId = "q3_B_rent",
            label = "Permutations:",
            value = 1000,
            min = 1,
            max = 100000
          ),
          
          actionButton(
            inputId = "run_perm_rent",
            label = "Run Permutation Test",
            class = "btn-primary"
          )
        ),
        
        conditionalPanel(
          condition = "input.q3_tab == 'q3_perm_sleep'",
          
          h4("Monte Carlo simulation Options"),
          p("Permutation test for the difference in mean weekly sleep hours between DATA2002 and DATA2902 students."),
          
          numericInput(
            inputId = "q3_B",
            label = "Permutations:",
            value = 1000,
            min = 1,
            max = 100000
          ),
          
          actionButton(
            inputId = "run_perm_sleep",
            label = "Run Permutation Test",
            class = "btn-primary"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.selected_tab == 'appendix'",
        p("Thank you for using our R shiny app!")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "selected_tab",
        
        
        #------------------
        # INTRODUCTION
        #------------------
        tabPanel(
          title = "INTRODUCTION",
          value = "intro",
          
          accordion(
            id = "intro_accordion",
            open = "intro_motivation", #keeps research motivation open by default
            
            accordion_panel(
              title = HTML("<h4 style='margin:0;'>Research Motivation</h4>"),
              id = "intro_motivation",
              
              p(
                "As university students, balancing study, paid work and personal wellbeing can be challenging. ",
                "This app explores how employment, rent, sleep and other lifestyle ",
                "factors relate to academic performance, and how academic pressures, in turn, ",
                "influence students' wellbeing and daily routines. "),
              p(
                "These findings may help student services identify areas in need of ",
                "more targeted support, allowing earlier intervention and better outcomes.")
            ),
            
            #data description
            accordion_panel(
              title = HTML("<h4 style='margin:0;'>Dataset and Context</h4>"),
              id = "intro_dataset",
              
              tags$ul(
                tags$li(
                  tags$strong("Source: "),
                  "A voluntary survey of University of Sydney DATA2002 and DATA2902 ",
                  "students in Semester 2, 2026, collected to inform syllabus design ",
                  "and student learning pathways. The resulting dataset supplied for this group project."
                ),
                tags$li(
                  tags$strong("Collection: "),
                  "Students were invited to participate during the first lecture ",
                  "and allocated workshop time."
                ),
                tags$li(
                  tags$strong("Cleaned dataset: "),
                  "673 deidentified responses across 23 variables."
                ),
                tags$li(
                  tags$strong("Main topics: "),
                  tags$ol(
                    tags$li(
                      tags$strong("Demographic and Lifestyle: "),
                      "Age (years), height (cm), domestic or international status, whether the student pays rent (and how much per week), ",
                      "hours of paid employment per week, hours of sleep per night, litres of water consumed per day, number of languages spoken fluently, ",
                      "and favourite music genre."
                    ),
                    tags$br(),
                    tags$li(
                      tags$strong("Academic Background and Study Behaviour: "),
                      "Unit enrolment (DATA2002 or DATA2902), grade achieved in the DATA1001/1901 prerequisite, ",
                      "grade hoped for in DATA2002/2902, typical approach to assessments (e.g. gradually, crammed, immediately), ",
                      "and commute time to campus in minutes."
                    ),
                    tags$br(),
                    tags$li(
                      tags$strong("Weekly learning choices: "),
                      "The learning pathway and lecture delivery mode the student chose that week, along with a free-text explanation of the factors behind each choice. ",
                      "It also included a boolean response on whether the student multi-tasks during lectures, what tasks they do, and why."
                    )
                  )
                )
              )
            ),
            
            #random sampling and potential biases
            accordion_panel(
              title = HTML("<h4 style='margin:0;'>Sampling and Potential Biases</h4>"),
              id = "intro_biases",
              
              p(
                tags$strong("This is not a random sample. "),
                "All enrolled DATA2X02 students were invited, but individuals chose ",
                "whether to respond. This was an attempted census of the cohort with ",
                "voluntary participation, rather than random selection."),
              tags$ul(
                tags$li(
                  tags$strong("Self-report bias: "),
                  "Students are trusted to report their own behaviours,",
                  "but their recall and perception of experience such as sleep duration or water consumption may be imprecise.",
                  "Because these values are not objectively measured, self-reported responses can be affected by",
                  "misremembering, rough estimation, or exaggeration, leading to inaccurate data."),
                tags$li(
                  tags$strong("Non-response bias: "),
                  "Students who did not respond may differ systematically from respondents. ",
                  "For example, students with heavier work commitments may have had less ",
                  "time to complete the survey, potentially underrepresenting their experiences.")
              ),
              p(
                "These are potential biases, not confirmed differences. Our results should ",
                "be interpreted solely within the surveyed cohort and cannot be assumed to ",
                "represent all University of Sydney students.",
                "It is also important to remember the associations found in this research do not establish cause and effect."
              )
            ),
            
            #survey improvements
            accordion_panel(
              title = HTML("<h4 style='margin:0;'>Survey Improvements</h4>"),
              id = "intro_survey",
              
              tags$ul(
                tags$li(
                  tags$strong("Rent status and amount: "),
                  "Separate yes/no and numerical questions allowed contradictory answers, ",
                  "such as selecting 'no' while reporting positive rent. ",
                  tags$strong("Improvement: "),
                  "Use one clearly labelled weekly rent field with instructions to enter ",
                  "0 when no rent is paid, keeping missing responses distinct from zero."
                ),
                tags$li(
                  tags$strong("Assessment workflow: "),
                  "The 'Other' option lacked a follow-up text field, making these ",
                  "responses ambiguous and difficult to classify. ",
                  tags$strong("Improvement: "),
                  "Add an 'Other: please specify' field so students can describe ",
                  "workflows not covered by the listed options."
                )
              )
            ),
            
            #data cleaning
            accordion_panel(
              title = HTML("<h4 style='margin:0;'>Data Cleaning</h4>"),
              id = "intro_cleaning",
              
              p("To process the overall dataset, various work was done to ensure accurate data representation and allow efficient statistical analysis:"),
              tags$ol(
                tags$li(
                  tags$strong("Standardised variables: "),
                  "Renamed columns for clarity and retained variables relevant to ",
                  "the research questions. Categorical variables were converted to ",
                  "factors; the app codes grades as P = 1, CR = 2, D = 3 and HD = 4 ",
                  "to preserve their ranking."
                ),
                tags$li(
                  tags$strong("Created employment status: "),
                  "Positive weekly employment hours were coded as 'yes' and zero ",
                  "hours as 'no'. This indicates whether a student worked that week; ",
                  "zero hours does not necessarily imply that they had no job."
                ),
                tags$li(
                  tags$strong("Reconciled inconsistent answers: "),
                  "Rent status was recoded from the reported amount: positive values ",
                  "became 'yes' and zero became 'no'. Conflicting multitasking responses ",
                  "were checked and reconciled against the activities students listed."
                ),
                tags$li(
                  tags$strong("Screened numerical responses: "),
                  "Applied project-defined filters for reported commute times above ",
                  "4 hours, employment above 57 hours per week, and sleep of zero ",
                  "or above 20 hours per day. These thresholds aimed to screen ",
                  "potentially unreliable responses, but may also exclude genuine ",
                  "extreme values."
                ),
                tags$li(
                  tags$strong("Reviewed unusual water intake: "),
                  "High values were retained after manual inspection found no ",
                  "inconsistencies in accompanying responses. Zero water intake was ",
                  "also retained because students may drink other beverages. ",
                  "Unusual values alone were not treated as errors."
                ),
                tags$li(
                  tags$strong("Grouped assessment workflows: "),
                  "'Gradually' and 'immediately' were grouped as 'consistent'; ",
                  "'crammed' and 'other' as 'inconsistent' for two-sample comparisons. ",
                  "This simplifies the analysis, although unspecified 'Other' responses ",
                  "may not accurately fit the 'inconsistent' category."
                )
              )
            )
          )
        ),
        
        #----------------
        # QUESTION 1
        #----------------
        
        tabPanel(
          title = "QUESTION 1",
          value = "question1",
          
          h3("Chi-Square tests for Employment, DATA2002/DATA2902 Enrolment, and Weekly Paid Work"),
          
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
              p("All hypothesis tests use a significance level of α = 0.05.")
            ),
            
            tabPanel(
              title = "Chi-square Independence",
              value = "independence",
              
              h4("Pearson Chi-square Test of Independence"),
              
              uiOutput("q1_ind_research_question"),
              
              h4("Contingency Table Display"),
              
              tableOutput("q1_ind_table"),
              
              h4("Test Result"),
              
              uiOutput("q1_chi_independence"),
              
              h4("Chi-square Distribution"),
              
              plotOutput("q1_ind_chisq_plot"),
              
              h4("Assumption Check"),
              
              uiOutput("q1_ind_assumptions"),
              
              h4("Interpretation"),
              
              uiOutput("q1_ind_interpretation")
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
              
              uiOutput("q1_cmh"),
              
              h4("Chi-square Distribution"),
              
              plotOutput("q1_cmh_chisq_plot"),
              
              h4("Adjusted Interpretation"),
              
              uiOutput("q1_cmh_interpretation")
            )
          )
        ),
        
        #---------------
        # QUESTION 2
        #---------------
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
              uiOutput("rq_q2"),
              
              p("To evaluate the research question based on the selected variables, 
                our program detects which two sample test is most suitable following the assumptions:"),
              
              h5("Student Two Sample T-Test"),
              tags$ul(
                tags$li("Quantitative: data values collected are quantitative."),
                tags$li("Independence within and between groups: each row represents an individual student's survey response."),
                tags$li("Homogeneity of variance: population variances of both groups are equal, checked using Levene's test."),
                tags$li("Normality: Central Limit Theorem applies as n ≥ 30 for both groups of all categories.")),
              
              h5("Welch Two Sample T-Test"),
              tags$ul(
                tags$li("Quantitative: data values collected are quantitative."),
                tags$li("Independence within and between groups: each row represents an individual student's survey response."),
                tags$li("Normality: Central Limit Theorem applies as n ≥ 30 for both groups of all categories.")),     
              
              h5("Wilcoxon Rank-Sum Test"),
              tags$ul(
                tags$li("Independence: groups are independent of one another."),
                tags$li("Same Distribution: groups follow the same distribution, differing by shift.")),
              
              p("If a numerical variable is chosen, then the program will run a two sample t-test. We check if the groups have equal variances via a Levene's test.
                If the p-value is small (< 0.05), we conduct a Student t-test; otherwise, we conduct a Welch t-test. 
                For ordinal data, the Wilcoxon Rank-Sum test is conducted."),
              
              p(HTML("<b> Note: </b>Please do not set \"Rental Status (y/n)\" as grouping variable and \"Weekly Rent (dollars)\" as
                numerical variable, or \"Employment Status (y/n)\" as grouping variable and
                \"Employment Hours (hours)\" as numerical variable. This is because the \"no\" group for
                both grouping variables correspond to a numerical value of zero in the corresponding
                numerical variable.")),
            ),
            
            #second tabset
            tabPanel(
              title = "Assumption Checking",
              value = "assumptions_q2",
              
              conditionalPanel(
                condition = "input.yvar_type == 'Numerical'",
                
                h4("Q-Q Plot: Visual check for normality"),
                p("The Q-Q plots below serve as an additional visual check if the data is normally distributed.
                  However, for the purposes of our analysis, as the sample is large, we assume Central Limit Theorem applies
                  for the means of both groups, ensuring the normality assumption holds."),
                plotOutput("q2_qqplot"),
                
                h4("Violin and box plots: Visual check for distribution and spread"),
                p("The violin and box plots show the shape of each group's distribution, as well as
                  the median, interquartile range and outliers. Comparing the widths of the two
                  violins gives a visual check of the equal variance assumption."),
                plotOutput("q2_violin_box"),
                
                h4("Levene's test of equal variance"),
                p("Levene's test gives a numerical value to the equality of variances between two
                  groups. To conduct it, it can be assumed that the variance of one group is independent to that
                  of the other group."),
                p("The null hypothesis states that the variances are equal across groups.
                  Hence, a small p-value of less than 0.05 indicates homogeneity of variance across groups, 
                  while a large p-value indicates significantly different variances, so the equal variance assumption is violated."),
                uiOutput("q2_levene")
              ),
              
              conditionalPanel(
                condition = "input.yvar_type == 'Ordinal'",
                
                p(tags$em("The Q-Q and violin/boxplot diagnostics are shown for numerical variables only.
                          The Wilcoxon Rank-Sum test used for ordinal variables does not assume normality."))
              )
            ),
            
            #third tabset
            tabPanel(
              title = "Two Sample test",
              value = "two_sample_q2",
              
              plotOutput("out2_graph"),
              uiOutput("out2_t_haptc")
              )
            )
          ),
        
        #--------------
        # QUESTION 3
        #--------------
        tabPanel(
          title = "QUESTION 3",
          value = "question3",
          
          h3("Monte Carlo Simulation Test for Mean Differences in Grades and Sleep Hours"),
          
          tabsetPanel(
            id = "q3_tab",
            
            tabPanel(
              title = "Introduction",
              value = "intro_q3",
              
              h4("Research Question"),
              p("Are DATA1X01 and DATA2X02 grades associated with rental status, ",
                "and is there a difference in mean sleep hours between students enrolled ",
                "in DATA2002 versus DATA2902, using Monte Carlo simulation?"),
              
              h4("Analysis"),
              p(
                "This research question involves conducting a Monte Carlo 
                permutation test, which is resampling‑based test that compares 
                the observed mean differences expected by pure random change when
                group labels are shuffled."),
              
              p("In the subsequent tabs, please choose your desired resampling size 
              for the Monte Carlo simulations, and (for the rental status tab) which 
                grade variable to use. The program will then conduct the 
                simulations and automatically update the results."),
            ),
            
            tabPanel(
              title = "Rental Status vs Grade",
              value = "q3_perm_rent",
              
              h4("Permutation Test: Grade by Rental Status"),
              p(
                "We use a Monte Carlo permutation test to determine if mean ",
                "grade differs significantly between students who pay rent and ",
                "those who do not. Use the sidebar dropdown to switch between ",
                "the DATA1X01 and DATA2X02 grade."
              ),
              
              plotOutput("q3_perm_plot_rent"),
              uiOutput("q3_perm_summary_rent")
            ),
            
            tabPanel(
              title = "Sleep Hours vs Enrolment",
              value = "q3_perm_sleep",
              
              h4("Permutation Test: Sleep Hours by DATA2002/DATA2902 Enrolment"),
              p(
                "We use a Monte Carlo permutation test to determine if mean ",
                "sleep hours differ significantly between DATA2002 and DATA2902 students."
              ),
              
              plotOutput("q3_perm_plot_sleep"),
              uiOutput("q3_perm_summary_sleep")
            )
          )
        ),
        
        tabPanel(
          title = "APPENDIX",
          value = "appendix",
          
          h4("AI Usage Statement"),
          p("Taskin - I used GPT 5.6 Sol by OpenAI for helping me with the coding part (tabs) and suggesting me statistical approaches for the chi-squared test tabs. All of the work was reviewed by me."),
          p("Felix - I used Opus 5 by Claude to help with formatting the written portion of the statistical tests, including symbols and sub/superscripts."),
          p("Nadh - I used Gemini to clarify the framework of the Monte Carlo resampling test, including how to apply a HAPTC framework onto it."),
          p("Tuhin - I used Sonnet 5 by Claude to assist with CSS bugs and Gemini Plus to help with resampling graphs"),
          
          h4("ISI Professional Ethics Statement"),
          p("Shared Value: Throughout our research process, we maintained respect for the privacy of the participants by fairly processing the data, ensuring truthfulness and integrity in  handling statistical results."),
          p("Ethical Principles: To explore our overall research question, we maintained confidence in our statistical analysis and its limitations, ensuring the methods used produced the best possible results."),
          
          h4("References"),
          p("Australian Government. (n.d.). Hours of work. Fair Work Ombudsman. Retrieved 2 September 2026, from https://www.fairwork.gov.au/employment-conditions/hours-of-work-breaks-and-rosters/hours-of-work"),
          p("Bollu, P. C., Manjamalai, S., Thakkar, M., & Sahota, P. (2018). Hypersomnia. Missouri Medicine, 115(1), 85–91."),
          p("https://stackoverflow.com/a/61276959, by lauren.marietta")
        )
      )
    )
  )
)


server <- function(input, output) {
  q2_xvar <- reactive(input$drop2_xvar)
  q2_yvar <- reactive(if (input$yvar_type == "Numerical") input$drop2_yvar_num else input$drop2_yvar_ord)
  
  # -------------------------------------------------------
  # QUESTION 2 - INTRODUCTION
  # -------------------------------------------------------  
  output$rq_q2 <- renderUI({
    #get the grouping variable
    raw_val <- q2_xvar()
    grouping_var <- names(nominal_choices)[nominal_choices == raw_val]
    
    #identify which raw name was selected based on the active radio button
    raw_val <- q2_yvar()
    
    #combine lists to map backend name back to its display label
    all_choices <- c(numeric_choices, ordinal_choices)
    clean_label <- names(all_choices)[all_choices == raw_val]
    
    p(paste0("Is there a difference in the means of student's ", clean_label, 
             " when divided by ", grouping_var, "?"))
  })
  
  # -------------------------------------------------------
  # QUESTION 2 - ASSUMPTIONS
  # -------------------------------------------------------  
  q2_plot_data <- reactive({
    req(input$yvar_type == "Numerical")
    
    xvar <- q2_xvar()
    yvar <- input$drop2_yvar_num
    
    d <- df[, c(xvar, yvar)]
    names(d) <- c("group", "value")
    
    d <- d[!is.na(d$group) & !is.na(d$value), ]
    d$group <- factor(d$group)
    
    validate(
      need(nrow(d) > 0, "No complete observations for the selected variables.")
    )
    
    d
  })
  
  #display labels for the axes/titles of the assumption plots
  q2_plot_labels <- reactive({
    list(
      group = names(nominal_choices)[nominal_choices == q2_xvar()],
      value = names(numeric_choices)[numeric_choices == input$drop2_yvar_num]
    )
  })
  
  output$q2_qqplot <- renderPlot({
    d <- q2_plot_data()
    labs_q2 <- q2_plot_labels()
    
    ggplot(d, aes(sample = value, colour = group)) +
      stat_qq() +
      stat_qq_line(colour = "black") +
      scale_colour_manual(values = c("#69b3a2", "#404080")) +
      facet_wrap(~ group) +
      labs(
        title = paste0("Normal Q-Q Plots of ", labs_q2$value, " by ", labs_q2$group),
        x = "Theoretical Quantiles",
        y = paste0("Sample Quantiles: ", labs_q2$value),
        colour = labs_q2$group
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$q2_levene <- renderUI({
    d <- q2_plot_data()
    
    lt <- levene_test(d, value ~ group)
    
    stat <- signif(unname(lt$statistic), 4)
    df1  <- unname(lt$df1)
    df2  <- unname(lt$df2)
    pval <- format.pval(lt$p, digits = 4, eps = 0.0001)
    
    tags$div(
      class = "hatpc-panel",
      tags$div(
        class = "hatpc-step",
        tags$strong("Levene's Test of Equal Variance"),
        p(HTML(glue("F<sub>{df1}, {df2}</sub> = {stat}, p = {pval}")))
      )
    )
  })
  
  output$q2_violin_box <- renderPlotly({
    d <- q2_plot_data()
    labs_q2 <- q2_plot_labels()
    
    p <- ggplot(d, aes(x = group, y = value, fill = group)) +
      geom_violin(width = 1.0, colour = "#e9ecef", alpha = 0.8) +
      geom_boxplot(width = 0.12, fill = "white", colour = "grey25", outlier.size = 1) +
      scale_fill_manual(values = c("#69b3a2", "#404080")) +
      labs(
        title = paste0("Distribution of ", labs_q2$value, " by ", labs_q2$group),
        x = labs_q2$group,
        y = labs_q2$value) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("text")) |> plotly::style(hoverinfo = "none", traces = 1:2)
  })
  
  
  # -------------------------------------------------------
  # QUESTION 2 - TWO SAMPLE T-TEST
  # -------------------------------------------------------
  output$out2_graph <- renderPlot({
    xvar <- q2_xvar()
    yvar <- q2_yvar()
    
    labs_q2 <- q2_plot_labels()
    
    df %>% ggplot(aes_string(x = yvar, fill = xvar)) +
      (if (input$yvar_type == "Numerical") geom_histogram(color="#e9ecef", position = 'identity')
       else geom_bar(color="#e9ecef", position = 'identity')) +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="",
           title = paste0("Distribution of ", labs_q2$value, " by ", labs_q2$group),
           x = labs_q2$group,
           y = labs_q2$value) + facet_wrap(~.data[[xvar]]) +
      theme(legend.position = "none") +
      theme_minimal() 
  })
  
  output$out2_t_haptc <- renderUI({
    alpha <- 0.05
    alt <- input$q2_alternative
    
    #evaluates what alt will be and changes the alt hyp accordingly
    h1_symbol <- switch(alt,
                        "two.sided" = "≠",
                        "greater"   = ">",
                        "less"      = "<")
    
    direction_phrase <- switch(alt,
                               "two.sided" = "significantly differs",
                               "greater"   = "is greater in the",
                               "less"      = "is lower in the")
    
    hypothesis <- glue("H₀: μx = μy. H₁: μx {h1_symbol} μy.")
    
    xvar <- input$drop2_xvar
    groups <- levels(df[[xvar]])
    grouping_var <- names(nominal_choices)[nominal_choices == xvar]
    
    #identify which raw name was selected based on the active radio button
    raw_val <- q2_yvar()
    
    #combine lists to map backend name back to its display label
    all_choices <- c(numeric_choices, ordinal_choices)
    clean_label <- names(all_choices)[all_choices == raw_val]

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
            alternative = alt,
            data = _
          )
      } else {
        test_type <- "student"
        test_name <- "Student's t-test"
        tt <- df |>
          t.test(
            formula = f,
            var.equal = TRUE,
            alternative = alt,
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
          glue("{Xi} and {Yi} are independent and have equal variance.")
        } else {
          glue("{Xi} and {Yi} are independent.")
        }
        
        variance_clause <- glue(
          "Using Levene's test, as p-value = {lev} is greater than 0.05, X and Y have similar ",
          "variances and the Student's t-test is used."
        )
        
      } else {
        
        indep_clause <- glue("{Xi} and {Yi} are independent and have equal variance.")
        
        variance_clause <- glue(
          "Using Levene's test, as p-value = {lev} is less than 0.05, X and Y have different ",
          "variances and the Welch's t-test is used."
        )
      }
      
      assumptions <- glue(
        "{X1}, {X2}, {dots} , {Xnx} are independent and identically distributed with the ",
        "distribution N ({muX}, {sig2}), {Y1}, {Y2}, {dots} , {Yny} are independent and ",
        "identically distributed with the distribution N ({muY}, {sig2}), and ",
        "{indep_clause} {variance_clause}"
      )
      
      test_statistic <- p(HTML(glue(
        "Under H₀,  T{sim} t<sub>{dof}</sub>. The test statistic is {stat}."
      )))
      
      conclusion <- if (reject) {
        p(HTML(glue(
          "Since the p-value < {alpha}, we reject H₀. There is evidence that the mean ",
          clean_label, " ", direction_phrase, " ",
          if (alt == "two.sided") glue("between {groups[1]} and {groups[2]} groups for {grouping_var}.")
          else glue("{groups[1]} group than the {groups[2]} group for {grouping_var}.")
        )))
      } else {
        p(HTML(glue(
          "Since the p-value > {alpha}, we do not reject H₀. ",
          "We do not have sufficient evidence to conclude that the mean ", clean_label, " ", direction_phrase, " ",
          if (alt == "two.sided") glue("between {groups[1]} and {groups[2]} groups for {grouping_var}.")
          else glue("{groups[1]} group compared to the {groups[2]} group for {grouping_var}.")
        )))
      }
      
    } else {
      
      yvar <- input$drop2_yvar_ord
      
      f <- as.formula(
        paste(yvar, "~", xvar)
      )
      
      test_name <- "Wilcoxon Rank Sum Test"
      
      wt <- wilcox.test(
        formula = f,
        alternative = alt,
        data = df
      )
      
      stat <- signif(unname(wt$statistic), 4)
      pval <- signif(wt$p.value, 4)
      
      reject <- wt$p.value < alpha
      
      assumptions <- glue("{Xi} and {Yi} are independent and follow the same distribution but differ by a shift.")
      
      test_statistic <- p(HTML(glue(
        "Under H₀,  W follows the WRS(n<sub>X</sub>, n<sub>Y</sub>) distribution. ",
        "The test statistic is {stat}."
      )))
      
      conclusion <- if (reject) {
        p(HTML(glue(
          "Since the p-value < {alpha}, we reject H₀. There is evidence that the mean ",
          clean_label, " ", direction_phrase, " ",
          if (alt == "two.sided") glue("between {groups[1]} and {groups[2]} groups for {grouping_var}.")
          else glue("{groups[1]} group than the {groups[2]} group for {grouping_var}.")
        )))
      } else {
        p(HTML(glue(
          "Since the p-value > {alpha}, we do not reject H₀. ",
          "We do not have sufficient evidence to conclude that the mean ", clean_label, " ", direction_phrase, " ",
          if (alt == "two.sided") glue("between {groups[1]} and {groups[2]} groups for {grouping_var}.")
          else glue("{groups[1]} group compared to the {groups[2]} group for {grouping_var}.")
        )))
      }
    }
    
    tags$div(
      class = "hatpc-panel",
      
      h4(paste0("Test performed: ", test_name)),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Significance level"),
        p(paste0(alp, " = ", alpha))
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Hypothesis"),
        p(paste0("Let x: ", groups[1], " group and y: ", groups[2]), " group."),
        p(hypothesis)
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Assumptions"),
        p(HTML(assumptions))
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
  # QUESTION 3 - RENTAL STATUS vs DATA1X01/DATA2X02 GRADES
  # -------------------------------------------------------
  run_rent_grade_perm <- function(grade_var, B) {
    req(df)
    
    dat <- df %>% filter(!is.na(.data[[grade_var]]), !is.na(rent_y_n))
    g_levels <- levels(factor(dat$rent_y_n))
    
    if(length(g_levels) < 2) return(NULL)
    
    g1 <- g_levels[1]
    g2 <- g_levels[2]
    
    obs_mean1 <- mean(dat[[grade_var]][dat$rent_y_n == g1], na.rm = TRUE)
    obs_mean2 <- mean(dat[[grade_var]][dat$rent_y_n == g2], na.rm = TRUE)
    obs_diff <- obs_mean1 - obs_mean2
    
    diffs <- replicate(B, {
      shuffled_group <- sample(dat$rent_y_n)
      m1 <- mean(dat[[grade_var]][shuffled_group == g1], na.rm = TRUE)
      m2 <- mean(dat[[grade_var]][shuffled_group == g2], na.rm = TRUE)
      m1 - m2
    })
    
    pval <- mean(abs(diffs) >= abs(obs_diff))
    
    list(dist = diffs, obs_diff = obs_diff, pval = pval, grade_var = grade_var, g1 = g1, g2 = g2)
  }
  
  render_rent_grade_summary <- function(res, grade_label, B) {
    
    # Unicode glyphs used in the write-up
    mu   <- "μ"   # μ
    neq  <- "≠"   # ≠
    dots <- "…"   # …
    
    hypothesis <- p(HTML(glue("H₀: {mu}<sub>x</sub> = {mu}<sub>y</sub> <br>
                              H₁: {mu}<sub>x</sub> {neq} {mu}<sub>y</sub>")))
    
    assumptions <- p(HTML(glue(
      "X<sub>1</sub>, {dots} , X<sub>n<sub>x</sub></sub> and Y<sub>1</sub>, {dots} , Y<sub>n<sub>y</sub></sub> are independent random samples. ",
      "Under the null hypothesis (H₀), both groups are exchangeable and follow, ",
      "the same underlying distribution (i.e., they follow identical shapes and variances.)")))
    
    test_statistic <- glue("Under H₀, the test statistic is the observed difference, ",  round(res$obs_diff, 4))
    
    conclusion <- if (res$pval < 0.05) 
    {(paste0("At α = 0.05, since p-value < 0.05, we reject the null hypothesis. There is a significant difference in mean ", grade_label, " grade between renters and non-renters.\n"))} 
    else {(paste0("At α = 0.05, since p-value > 0.05, we fail to reject the null hypothesis. We do not have sufficient evidence to claim a difference in mean ", grade_label, " grade between renters and non-renters.\n"))}
    
    tags$div(
      class = "hatpc-panel",
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Significance level"),
        p("α = 0.05")
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Hypothesis"),
        p(paste0("Let x: rent = ", res$g1, " group and y: rent = ", res$g2, " group."),
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
          p(paste0("The permutation p-value is ", format.pval(res$pval, eps = 1/B), ", 
                   calculated using ", B ," Monte Carlo resamples.")),
          
          
          tags$div(
            class = "hatpc-step",
            tags$strong("Conclusion"),
            p(conclusion))
        )
      )
    )
  }
  
  render_rent_grade_plot <- function(res, grade_label) {
    df_plot <- data.frame(val = res$dist)
    
    ggplot(df_plot, aes(x = val)) +
      geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
      geom_vline(xintercept = res$obs_diff, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = -res$obs_diff, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = paste0("Null Distribution of Difference in Mean ", grade_label, " Grade\n(Rent: ", res$g1, " vs ", res$g2, ")"),
        x = "Simulated Difference Under Null Hypothesis",
        y = "Frequency"
      ) +
      theme_minimal()
  }
  
  #-------------------------
  # Rental status vs Grade tab (DATA1X01 / DATA2X02 chosen via dropdown)
  #-------------------------
  perm_res_rent <- eventReactive(input$run_perm_rent, {
    validate(
      need(input$q3_B_rent >= 1 && input$q3_B_rent <= 100000, "Please enter a number of permutations between 1 and 100,000."),
      need(input$q3_grade_choice %in% c("grade_1X01", "grade_2X02"), "Please select a grade variable.")
    )
    
    run_rent_grade_perm(input$q3_grade_choice, input$q3_B_rent)
  }, ignoreNULL = FALSE)
  
  grade_var_label <- function(grade_var) {
    if (grade_var == "grade_1X01") "DATA1X01" else "DATA2X02"
  }
  
  output$q3_perm_summary_rent <- renderUI({
    req(perm_res_rent())
    res <- perm_res_rent()
    render_rent_grade_summary(res, grade_var_label(res$grade_var), input$q3_B_rent)
  })
  
  output$q3_perm_plot_rent <- renderPlot({
    req(perm_res_rent())
    res <- perm_res_rent()
    render_rent_grade_plot(res, grade_var_label(res$grade_var))
  })
  
  # -------------------------------------------------------
  # QUESTION 3 - DATA2002/DATA2902 ENROLMENT vs SLEEP HOURS
  # -------------------------------------------------------
  perm_res_sleep <- eventReactive(input$run_perm_sleep, {
    req(df)
    
    validate(
      need(input$q3_B >= 1 && input$q3_B <= 100000, "Please enter a number of permutations between 1 and 100,000.")
    )
    
    B <- input$q3_B
    
    dat <- df %>% filter(!is.na(sleep_hours), !is.na(type_2X02))
    
    obs_mean_2002 <- mean(dat$sleep_hours[dat$type_2X02 == "DATA2002"], na.rm = TRUE)
    obs_mean_2902 <- mean(dat$sleep_hours[dat$type_2X02 == "DATA2902"], na.rm = TRUE)
    obs_diff <- obs_mean_2002 - obs_mean_2902
    
    diffs <- replicate(B, {
      shuffled_type <- sample(dat$type_2X02)
      mean_2002 <- mean(dat$sleep_hours[shuffled_type == "DATA2002"], na.rm = TRUE)
      mean_2902 <- mean(dat$sleep_hours[shuffled_type == "DATA2902"], na.rm = TRUE)
      mean_2002 - mean_2902
    })
    
    pval <- mean(abs(diffs) >= abs(obs_diff))
    
    list(dist = diffs, obs_diff = obs_diff, pval = pval, g1 = "DATA2002", g2 = "DATA2902")
  }, ignoreNULL = FALSE)
  
  output$q3_perm_summary_sleep <- renderUI({
    req(perm_res_sleep())
    res <- perm_res_sleep()
    
    B <- input$q3_B
    
    # Unicode glyphs used in the write-up
    mu   <- "μ"   # μ
    neq  <- "≠"   # ≠
    dots <- "…"   # …
    
    hypothesis <- p(HTML(glue("H₀: {mu}<sub>x</sub> = {mu}<sub>y</sub> <br>
                              H₁: {mu}<sub>x</sub> {neq} {mu}<sub>y</sub>")))
    
    assumptions <- p(HTML(glue(
      "X<sub>1</sub>, {dots} , X<sub>n<sub>x</sub></sub> and Y<sub>1</sub>, {dots} , Y<sub>n<sub>y</sub></sub> are independent random samples. ",
      "Under the null hypothesis (H₀), both groups are exchangeable and follow, ",
      "the same underlying distribution (i.e., they follow identical shapes and variances.)")))
    
    test_statistic <- glue("Under H₀, the test statistic is the observed difference, ",  round(res$obs_diff, 4))
    
    conclusion <- if (res$pval < 0.05) 
    {("At α = 0.05, since p-value < 0.05, we reject the null hypothesis. There is a significant difference in mean sleep hours between the cohorts.\n")} 
    else {("At α = 0.05, since p-value > 0.05, we fail to reject the null hypothesis. We do not have sufficient evidence to claim a difference in mean sleep hours between the cohorts.\n")}
    
    tags$div(
      class = "hatpc-panel",
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Significance level"),
        p("α = 0.05")
      ),
      
      tags$div(
        class = "hatpc-step",
        tags$strong("Hypothesis"),
        p(paste0("Let x: ", res$g1, " group and y: ", res$g2, " group."),
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
          p(paste0("The permutation p-value is ", format.pval(res$pval, eps = 1/input$q3_B), 
                   ", calculated using ", B ," Monte Carlo resamples.")),
          
          
          tags$div(
            class = "hatpc-step",
            tags$strong("Conclusion"),
            p(conclusion))
        )
      )
    )
  })
  
  output$q3_perm_plot_sleep <- renderPlot({
    req(perm_res_sleep())
    res <- perm_res_sleep()
    
    df_plot <- data.frame(val = res$dist)
    
    ggplot(df_plot, aes(x = val)) +
      geom_histogram(bins = 30, fill = "#404080", color = "white") +
      geom_vline(xintercept = res$obs_diff, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = -res$obs_diff, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = "Null Distribution of Difference in Mean Sleep Hours\n(DATA2002 - DATA2902)",
        x = "Simulated Difference Under Null Hypothesis",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  
  #-------------------------------
  # QUESTION 1
  #-------------------------------
  
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
  
  output$q1_chi_independence <- renderUI({
    result_text <- paste(capture.output(print(q1_ind_test())), collapse = "\n")
    hatpc_text_output("Chi-square Test of Independence", result_text)
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
  output$q1_ind_assumptions <- renderUI({
    
    expected <- q1_ind_test()$expected
    
    total_cells <- length(expected)
    cells_below_5 <- sum(expected < 5)
    cells_below_1 <- sum(expected < 1)
    percent_below_5 <- (cells_below_5 / total_cells) * 100
    
    lines <- c(
      "Expected Cell Count Check",
      "",
      paste0(
        "Cells with expected count below 5: ",
        cells_below_5,
        " out of ",
        total_cells,
        " (", round(percent_below_5, 1), "%)"
      ),
      paste0(
        "Cells with expected count below 1: ",
        cells_below_1
      ),
      ""
    )
    
    conclusion_line <- if (cells_below_1 == 0 && percent_below_5 <= 20) {
      "Conclusion: The expected-count condition for the chi-square approximation is satisfied."
    } else {
      "Conclusion: The expected-count condition may not be satisfied. The Pearson chi-square approximation should therefore be interpreted with caution."
    }
    
    result_text <- paste(c(lines, conclusion_line), collapse = "\n")
    
    hatpc_text_output("Expected Cell Count Check", result_text)
  })
  
  
  # -------------------------------------------------------
  # QUESTION 1 - CHI-SQUARE INTERPRETATION
  # -------------------------------------------------------
  output$q1_ind_interpretation <- renderUI({
    
    test <- q1_ind_test()
    
    p_value <- test$p.value
    
    conclusion_line <- if (p_value < 0.05) {
      paste0(
        "Conclusion: At α = 0.05, we reject the null hypothesis. ",
        "There is statistically significant evidence of an association between ",
        input$q1_ind_x, " and ", input$q1_ind_y, "."
      )
    } else {
      paste0(
        "Conclusion: At α = 0.05, we fail to reject the null hypothesis. ",
        "There is insufficient evidence of an association between ",
        input$q1_ind_x, " and ", input$q1_ind_y, "."
      )
    }
    
    result_text <- paste(
      c(
        paste0("Selected variables: ", input$q1_ind_x, " and ", input$q1_ind_y),
        "",
        paste0("Chi-square statistic: ", round(unname(test$statistic), 3)),
        paste0("Degrees of freedom: ", unname(test$parameter)),
        paste0("P-value: ", format.pval(p_value, digits = 4, eps = 0.0001)),
        "",
        conclusion_line
      ),
      collapse = "\n"
    )
    
    hatpc_text_output("Interpretation", result_text)
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
    
    cmh_display <- as.data.frame(
      q1_cmh_view_table(),
      responseName = "Frequency"
    )
    
    names(cmh_display) <- c(
      "Course enrolment",
      "Employment status",
      "Frequency"
    )
    
    cmh_display %>%
      group_by(`Course enrolment`) %>%
      mutate(
        Proportion = round(Frequency / sum(Frequency), 3)
      ) %>%
      ungroup()
    
  }, rownames = FALSE)
  
  
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
  
  
  output$q1_cmh <- renderUI({
    result_text <- paste(capture.output(print(q1_cmh_test())), collapse = "\n")
    hatpc_text_output("Cochran-Mantel-Haenszel Chi-square Test", result_text)
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
  output$q1_cmh_interpretation <- renderUI({
    
    test <- q1_cmh_test()
    p_value <- test$p.value
    
    lines <- c(
      "Null hypothesis: After controlling for Domestic/International student status, course enrolment and paid employment status are conditionally independent.",
      "",
      "Alternative hypothesis: After controlling for Domestic/International student status, course enrolment and paid employment status remain associated.",
      "",
      paste0("CMH chi-square statistic: ", round(unname(test$statistic), 3)),
      paste0("Degrees of freedom: ", unname(test$parameter)),
      paste0("Adjusted P-value: ", format.pval(p_value, digits = 4, eps = 0.0001))
    )
    
    if (!is.null(test$estimate)) {
      lines <- c(lines, paste0("Common odds ratio estimate: ", round(unname(test$estimate), 3)))
    }
    
    if (!is.null(test$conf.int)) {
      lines <- c(
        lines,
        paste0(
          "95% confidence interval for the common odds ratio: (",
          round(test$conf.int[1], 3), ", ", round(test$conf.int[2], 3), ")"
        )
      )
    }
    
    conclusion_line <- if (p_value < 0.05) {
      paste0(
        "Conclusion: At α = 0.05, we reject the null hypothesis. There is statistically significant evidence of an ",
        "association between course enrolment and paid employment status even after controlling for Domestic/International student status."
      )
    } else {
      paste0(
        "Conclusion: At α = 0.05, we fail to reject the null hypothesis. After controlling for Domestic/International student status, ",
        "there is insufficient evidence of an association between course enrolment and paid employment status."
      )
    }
    
    result_text <- paste(c(lines, "", conclusion_line), collapse = "\n")
    
    hatpc_text_output("Adjusted Interpretation", result_text)
  })
  
  
  # -------------------------------------------------------
  # QUESTION 1 - UNADJUSTED VS ADJUSTED COMPARISON
  # -------------------------------------------------------
  output$q1_cmh_comparison <- renderUI({
    
    
    adjusted_test <- q1_cmh_test()
    
    unadjusted_p <- unadjusted_test$p.value
    adjusted_p <- adjusted_test$p.value
    
    comparison_line <- if (unadjusted_p < 0.05 && adjusted_p < 0.05) {
      paste0(
        "Comparison: Both the unadjusted and adjusted analyses are statistically significant. ",
        "The evidence of an association between course enrolment and employment status remains after ",
        "controlling for Domestic/International student status."
      )
    } else if (unadjusted_p < 0.05 && adjusted_p >= 0.05) {
      paste0(
        "Comparison: The unadjusted association is statistically significant, but the adjusted CMH association is not. ",
        "This suggests that differences in the Domestic/International student mix may help explain the unadjusted association."
      )
    } else if (unadjusted_p >= 0.05 && adjusted_p < 0.05) {
      paste0(
        "Comparison: The unadjusted analysis is not statistically significant, but the adjusted CMH analysis is. ",
        "Controlling for Domestic/International student status reveals evidence of an association that was not apparent in the unadjusted analysis."
      )
    } else {
      paste0(
        "Comparison: Neither the unadjusted nor the adjusted analysis is statistically significant. ",
        "There is insufficient evidence of an association between course enrolment and employment status before or after ",
        "controlling for Domestic/International student status."
      )
    }
    
    result_text <- paste(
      c(
        "Unadjusted Pearson chi-square analysis",
        paste0("P-value: ", format.pval(unadjusted_p, digits = 4, eps = 0.0001)),
        "",
        "CMH analysis adjusted for Domestic/International status",
        paste0("P-value: ", format.pval(adjusted_p, digits = 4, eps = 0.0001)),
        "",
        comparison_line
      ),
      collapse = "\n"
    )
    
  })
}

shinyApp(
  ui = ui,
  server = server
)