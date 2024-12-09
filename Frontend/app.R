library(shiny)
library(ggplot2)
library(dplyr)
library(knitr)
library(markdown)
library(rmarkdown)
library(plotly)
library(tidyr)

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
# Top 5 violations data 
top_violations <- traffic_data %>%
  group_by(Violation.Description) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 5)

plot_data <- traffic_data %>%
  filter(Violation.Description %in% top_violations$Violation.Description) %>%
  group_by(Violation.Year, Age_Group) %>%
  summarise(total_violations = n(), .groups = "drop")

# Define UI
ui <- fluidPage(
  titlePanel("NYC Traffic Violations App"),
  tabsetPanel(
    # Tab 1: Plot 1
    tabPanel(
      "Plot 1",
      sidebarLayout(
        sidebarPanel(
          selectInput("age_group", "Select Age Group:", 
                      choices = c("All", unique(traffic_data$Age_Group)),
                      selected = "All"),
          selectInput("gender", "Select Gender:", 
                      choices = c("All", unique(traffic_data$Gender)),
                      selected = "All"),
          selectInput("violation", "Select Violation Type:", 
                      choices = c("All", unique(traffic_data$Violation.Description)),
                      selected = "All"),
          actionButton("show_report", "Show Detailed Report", class = "btn-primary mt-3")
        ),
        mainPanel(
          plotOutput("ticketPlot"),
          DT::DTOutput("ticketTable")
        )
      )
    ),
    # Tab 2: Plot 2
    tabPanel(
      "Plot 2",
      plotOutput("scatterPlot")
    ),
    # Tab 3: Plot 3
    tabPanel(
      "Plot 3",
      plotOutput("topViolationsPlot")
    ),
    # Tab 4: Traffic Violations Trends by Age Group (2020-2023)
    tabPanel(
      "Traffic Violations Trends by Age Group (2020-2023)",
      sidebarLayout(
        sidebarPanel(
          selectInput("age_group_interactive", "Select Age Group:", 
                      choices = c("All", levels(factor(unique(plot_data$Age_Group)))), 
                      selected = "All")
        ),
        mainPanel(
          plotlyOutput("interactivePlot"),
          HTML("<h1 style='font-size: 20px; margin-top: 20px;'><u>Insights</u></h1>"),
          HTML("<p style='font-size: 14px; margin-top: 20px;'>Overall, NYC traffic violations have increased across most age groups from 2020 to 2023. However, in 2020, all age groups except for 86-95 appear to have exactly 100 traffic violations within the top 5 violation categories. It is unclear whether this is due to potential data collection issues or if there was some sort of existing quota to fulfill during that year. Something to consider is that during the year 2020, law enforcement's focus may have been more on public safety rather than traffic violations due to the COVID-19 pandemic. However, we cannot conclude whether this fully explains the trend. Additionally, both the 16-25 and 26-35 age groups saw an increase in violations, which may suggest that younger drivers are more active on the roads in NYC. Since the 16-25 and 26-35 age groups had the highest number of violations in 2023, this could also indicate that they are more targeted demographics or that there are potentially fewer older drivers on the road. Finally another insight we found is that the 76-85 age group has shown the most consistent number of violations across the years, which could mean they are at lower risk of being issued a violation ticket.</p>")
        )
      )
    ),
 
    # Tab 5: Report
tabPanel(
  "Report",
  fluidRow(
    column(
      width = 12,
      h3("Traffic Violations Analysis Report"),
      p("Text"),
      p("Key findings include:"),
      tags$ul(
        tags$li("Text."),
        tags$li("Text."),
        tags$li("Text."),
        tags$li("Text.")
      ),
      p("Text.")
    )
  )
)
)
)

# Define server logic
server <- function(input, output) {
  
  # Tab 1: Plot 1
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
  
  output$ticketTable <- DT::renderDT({
    filtered_data()
  })
  
  # Tab 2: Plot 2
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
  
  # Tab 3: Plot 3
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
  
  # Tab 4: Traffic Violations Trends by Age Group (2020-2023)
  filtered_interactive_data <- reactive({
    if (input$age_group_interactive == "All") {
      plot_data  
    } else {
      plot_data %>% filter(Age_Group == input$age_group_interactive)
    }
  })
  
  output$interactivePlot <- renderPlotly({
    req(nrow(filtered_interactive_data()) > 0)
    
    p <- ggplot(filtered_interactive_data(), aes(x = Violation.Year, y = total_violations, color = Age_Group, group = Age_Group)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal(base_size = 14) +
      scale_x_continuous(breaks = seq(min(plot_data$Violation.Year), max(plot_data$Violation.Year), 1)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_color_brewer(palette = "Paired") +
      labs(
        title = "Traffic Violations Trends by Age Group",
        x = "Year",
        y = "Number of Violations",
        color = "Age Group"
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
