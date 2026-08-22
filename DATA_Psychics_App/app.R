library(shiny)
library(tidyverse)
library(car)

# Read csv
df <- read_csv("../DATA2X02_data_cleaned.csv")

# Convert to factor
df <- df %>%
  mutate(across(c("intl_dom_type",
                  "type_2X02",
                  "assessment_workflow",
                  "workflow_grouped",
                  "rent_y_n",
                  "employment_y_n",
                  "multitask_y_n"), as.factor))

df <- df %>%
  mutate(across(c("grade_1X01", "grade_2X02"), 
                ~ factor(.x, 
                         levels = c("P", "CR", "D", "HD"), 
                         ordered = TRUE)))


# Define numeric choices
numeric_choices <- c("travel_to_campus_min",
                     "rent_dollars", 
                     "employment_hours",
                     "sleep_hours", 
                     "water_l")

# Define categorical nominal choices
nominal_choices <- c("intl_dom_type",
                     "type_2X02",
                     "assessment_workflow",
                     "workflow_grouped", 
                     "rent_y_n", 
                     "employment_y_n", 
                     "multitask_y_n")

# Define categorical ordinal choices
ordinal_choices <- c("grade_1X01",
                     "grade_2X02")


# Define UI for app that performs tests
ui <- fluidPage(
  titlePanel("DATA2902 Psychics Tests"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown group for Tab 1
      conditionalPanel(
        condition = "input.selected_tab == 'tab1'",
        selectInput(inputId = "drop1_xvar", 
                    label = "Module 1 Test Options:", 
                    choices = nominal_choices,
                    selected = "intl_dom_type"),
        selectInput(inputId = "drop1_yvar", 
                    label = "Module 1 Test Options:", 
                    choices = numeric_choices,
                    selected = "rent_dollars")
      ),
      
      # Dropdown group for Tab 2
      conditionalPanel(
        condition = "input.selected_tab == 'tab2'",
        selectInput(inputId = "drop2_xvar", 
                    label = "Module 2 Test Options:", 
                    choices = nominal_choices,
                    selected = "intl_dom_type"),
        selectInput(inputId = "drop2_yvar", 
                    label = "Module 2 Test Options:", 
                    choices = ordinal_choices,
                    selected = "grade_1X01")
      ),
      
      # Dropdown group for Tab 3
      conditionalPanel(
        condition = "input.selected_tab == 'tab3'",
        selectInput(inputId = "drop3_xvar", 
                    label = "Module 3 Test Options:", 
                    choices = nominal_choices,
                    selected = "intl_dom_type"),
        selectInput(inputId = "drop3_yvar", 
                    label = "Module 3 Test Options:", 
                    choices = numeric_choices,
                    selected = "rent_dollars")
      )
    ),
    
    mainPanel(
      # The id connects this panel to the conditional panels above
      tabsetPanel(
        id = "selected_tab",
        tabPanel("Module 1 Tests", value = "tab1", textOutput("out1")),
        tabPanel("Module 2 Tests", value = "tab2", textOutput("out2")),
        tabPanel("Resampling Tests", value = "tab3", textOutput("out3"))
      )
    )
  )
)

server <- function(input, output) {
  output.out1 <- renderText({ paste("You selected:", input$drop1) })
  output.out2 <- renderText({ paste("You selected:", input$drop2) })
  output.out3 <- renderText({ paste("You selected:", input$drop3) })
}

shinyApp(ui = ui, server = server)



