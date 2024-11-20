library(shiny)
library(ggplot2)
library(dplyr)
library(knitr)
library(rmarkdown)

# Load the data (replace the path with the correct one when running locally)
traffic_data <- read.csv("Traffic_Tickets_Issued__Number_of_Tickets_by_Age__Gender__and_Violation_20241113.csv")

# Add Age Group column to the dataset
traffic_data <- traffic_data %>%
  mutate(Age_Group = case_when(
    Age.at.Violation >= 16 & Age.at.Violation <= 25 ~ "16-25",
    Age.at.Violation >= 26 & Age.at.Violation <= 35 ~ "26-35",
    Age.at.Violation >= 36 & Age.at.Violation <= 45 ~ "36-45",
    Age.at.Violation >= 46 & Age.at.Violation <= 55 ~ "46-55",
    Age.at.Violation >= 56 & Age.at.Violation <= 65 ~ "56-65",
    Age.at.Violation >= 66 & Age.at.Violation <= 75 ~ "66-75",
    Age.at.Violation >= 76 & Age.at.Violation <= 85 ~ "76-85",
    Age.at.Violation >= 86 & Age.at.Violation <= 95 ~ "86-95",
    TRUE ~ "Other"
  ))

# Define UI for the app
ui <- fluidPage(
  titlePanel("Traffic Tickets Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("age_group", "Select Age Group:", 
                  choices = c("All", unique(traffic_data$Age_Group)),
                  selected = "All"),
      selectInput("gender", "Select Gender:", 
                  choices = c("All", unique(traffic_data$Gender)),
                  selected = "All"),
      selectInput("violation", "Select Violation Type:", choices = c("All", unique(traffic_data$Violation.Description)),
                  selected = "All")
    ),
    
    mainPanel(
      plotOutput("ticketPlot"),
      plotOutput("scatterPlot"),
      plotOutput("topViolationsPlot"),
      dataTableOutput("ticketTable"),
      uiOutput("visualsOutput"),
      uiOutput("ageSubsetsOutput")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  # Reactive data based on user input
  filtered_data <- reactive({
    data <- traffic_data
    
    if (input$age_group != "All") {
      data <- data %>% filter(Age_Group == input$age_group)
    }
    
    if (input$gender != "All") {
      data <- data %>% filter(Gender == input$gender)
    }
    
    if (input$violation != "All") {
      data <- data %>% filter(Violation.Description == input$violation)
    }
    
    return(data)
  })
  
  # Plot output
  output$ticketPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Violation.Description, fill = Gender)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(title = "Number of Tickets by Violation Type",
           x = "Violation Type",
           y = "Number of Tickets",
           fill = "Gender") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Scatter plot of age range by total violation
  output$scatterPlot <- renderPlot({
    traffic_data_2 <- traffic_data %>%
      group_by(Age_Group) %>%
      summarise(tot_violations = n())
    
    ggplot(data = traffic_data_2, mapping = aes(x = Age_Group, y = tot_violations)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Scatter Plot of Age Range by Total Violation",
           x = "Age Range",
           y = "Total Violations")
  })
  
  # Top 5 types of violations by age range and gender
  output$topViolationsPlot <- renderPlot({
    data <- filtered_data()
    
    top_5_violations <- data %>%
      group_by(Violation.Description) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n = 5)
    
    filtered_top_data <- data %>%
      filter(Violation.Description %in% top_5_violations$Violation.Description) %>%
      group_by(Age_Group, Violation.Description) %>%
      summarise(count = n(), .groups = "drop")
    
    ggplot(filtered_top_data, aes(x = Age_Group, y = count, fill = Violation.Description)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Top 5 Violations by Age Range",
        x = "Age Range",
        y = "Number of Violations",
        fill = "Violation Type"
      ) +
      theme_minimal()
  })
  
  # Table output
  output$ticketTable <- renderDataTable({
    filtered_data()
  })
  
  # Render Visuals.Rmd
  output$visualsOutput <- renderUI({
    includeMarkdown("Visuals.Rmd")
  })
  
  # Render age_subsets.Rmd
  output$ageSubsetsOutput <- renderUI({
    includeMarkdown("age_subsets.Rmd")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
