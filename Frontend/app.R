library(shiny)
library(ggplot2)
library(dplyr)
library(knitr)
library(markdown)
library(rmarkdown)
library(bslib)
library(plotly)

# Load the data (replace the path with the correct one when running locally)
traffic_data <- read.csv("Traffic_Tickets_Issued__Number_of_Tickets_by_Age__Gender__and_Violation_20241113.csv")

##NEW CODE
# Get all violations for detailed analysis
all_violations <- sort(unique(traffic_data$Violation.Description))


# Filter Gender column by "M" and "F"
traffic_data <- traffic_data[traffic_data$Gender %in% c("M", "F"), ]

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

# Get top 5 violations
top_5_violations <- traffic_data %>%
  count(Violation.Description) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5) %>%
  pull(Violation.Description)

# Prepare data for the time series plot
plot_data <- traffic_data %>%
  filter(Violation.Description %in% top_5_violations) %>%
  group_by(Violation.Year, Age_Group) %>%
  summarise(total_violations = n(), .groups = "drop")

# Define UI for the app
ui <- page_navbar(
  title = "Traffic Tickets Visualization",
  
  # Common controls in the sidebar
  sidebar = sidebar(
    selectInput("age_group", "Select Age Group:", 
                choices = c("All", unique(traffic_data$Age_Group)),
                selected = "All"),
    radioButtons("gender", "Select Gender:", 
                 choices = c("All", unique(traffic_data$Gender)),
                 selected = "All"),
    radioButtons("view_type", "Select View:", 
                 choices = c("Counts" = "count", "Proportions" = "proportion"),
                 selected = "count"),
    selectInput("violation", "Select Violation Type:", 
                choices = c("Top 5", top_5_violations),
                selected = "Top 5"),
    actionButton("show_report", "Show Detailed Report", class = "btn-primary mt-3")
  ),
  
  # Tab 1: Violation Overview
  nav_panel("Violation Overview",
            card(
              card_header("Top 5 Tickets by Violation Type and Gender"),
              plotOutput("ticketPlot")
            ),card(
              card_header("Insights"),
              div(
                style = "padding: 15px;",
                p("Across all demographics within the dataset, the five most common traffic violations include: speed in zone, uninspected motor vehicle, speed over 55 zone, weaving (moved from lane unsafely), and failing to stop at stop sign. Looking at age groups, the age group 26-35 had the most traffic violations overall. On the other hand, older drivers (76-85) had consistently low violation rates, which reflects either safer driving habits, less driving activity, or less focus from enforcement. Additionally, speeding violations were particularly higher among younger drivers (16-25 and 26-35), which might suggest that these age groups have riskier driving behaviors. Another interesting trend we found is that there was a high number of uninspected motor vehicle violations (approximately 655) which might indicate that there could be a widespread lack of awareness about inspection requirements.
                  We were able to find that males, females, and an unidentified gender group (X) all had speed in Zone as their top violation. This may suggest that speeding is a primary violation across all genders, which could indicate the need for better education on anti-speeding resources, campaigns, and better speeding enforcement policies or strategies. The second most common violation for males was uninspected motor vehicles, while for females, it was moved from lane unsafely/weaving. Overall, males had the highest overall traffic violations across all genders.")
              )
            )
  ),
  
  # Tab 2: Age Analysis
  nav_panel("Age Analysis",
            card(
              card_header("Age Distribution Analysis"),
              plotOutput("scatterPlot")
            ),card(
              card_header("Insights"),
              div(
                style = "padding: 15px;",
                p("This scatter plot reveals the relationship between age groups and total violations. The visualization helps identify which age groups are most frequently involved in traffic violations, showing potential patterns or trends across different age ranges in NYC.")
              )
            )
  ),
  
  # Tab 3: Top Violations by Age
  nav_panel("Top Violations by Age ",
            card(
              card_header("Top 5 Violations Analysis"),
              plotOutput("topViolationsPlot")
            ),
            card(
              card_header("Insights"),
              div(
                style = "padding: 15px;",
                p("Traffic Violation Trends by Age Group (2020-2023)
Overall, NYC traffic violations have increased across most age groups from 2020 to 2023. However, in 2020, all age groups except for 86-95 appear to have exactly 100 traffic violations within the top 5 violation categories. It is unclear whether this is due to potential data collection issues or if there was some sort of existing quota to fulfill during that year. Something to consider is that during the year 2020, law enforcement's focus may have been more on public safety rather than traffic violations due to the COVID-19 pandemic. However, we cannot conclude whether this fully explains the trend. Additionally, both the 16-25 and 26-35 age groups saw an increase in violations, which may suggest that younger drivers are more active on the roads in NYC. Since the 16-25 and 26-35 age groups had the highest number of violations in 2023, this could also indicate that they are more targeted demographics or that there are potentially fewer older drivers on the road. Finally another insight we found is that the 76-85 age group has shown the most consistent number of violations across the years, which could mean they are at lower risk of being issued a violation ticket.")
              )
            )
  ),
  
  
  # Tab 4: Time Series Analysis
  nav_panel("Time Series Analysis",
            card(
              card_header("Violations Trend Over Time"),
              selectInput("age_group_interactive", "Select Age Group for Trend:",
                          choices = c("All", unique(traffic_data$Age_Group)),
                          selected = "All"),
              plotlyOutput("interactivePlot", height = "600px")
            ),
            card(
              card_header("Key Insights"),
              div(
                style = "padding: 15px;",
                p("Overall, NYC traffic violations have increased across most age groups from 2020 to 2023. However, in 2020, all age groups except for 86-95 appear to have exactly 100 traffic violations within the top 5 violation categories. It is unclear whether this is due to potential data collection issues or if there was some sort of existing quota to fulfill during that year."),
                p("Something to consider is that during the year 2020, law enforcement's focus may have been more on public safety rather than traffic violations due to the COVID-19 pandemic. However, we cannot conclude whether this fully explains the trend."),
                p("Additionally, both the 16-25 and 26-35 age groups saw an increase in violations, which may suggest that younger drivers are more active on the roads in NYC. Since the 16-25 and 26-35 age groups had the highest number of violations in 2023, this could also indicate that they are more targeted demographics or that there are potentially fewer older drivers on the road."),
                p("Finally another insight we found is that the 76-85 age group has shown the most consistent number of violations across the years, which could mean they are at lower risk of being issued a violation ticket.")
              )
            )
  ),
  
  # Tab 5: Data and Reports
  nav_panel("Data & Reports",
            card(
              card_header("Insights"),
              div(
                style = "padding: 15px;",
                p("Our analysis of NYC traffic violations from 2020 to 2023 shows interesting trends and insights into road safety and enforcement patterns. Across all demographics within the dataset, the five most common traffic violations include: speed in zone, uninspected motor vehicle, speed over 55 zone, weaving (moved from lane unsafely), and failing to stop at stop sign. Looking at gender, we were able to find that across all years, males consistently received the most violations, which could reflect a combination of their driving patterns and enforcement practices that predominantly target males. Looking at age groups, the age group 26-35 had the most traffic violations overall. On the other hand, older drivers (76-85) had consistently low violation rates, which reflects either safer driving habits, less driving activity, or less focus from enforcement. Additionally, speeding violations were particularly higher among younger drivers (16-25 and 26-35), which might suggest that these age groups have riskier driving behaviors. Another interesting trend we found is that there was a high number of uninspected motor vehicle violations (approximately 655) which might indicate that there could be a widespread lack of awareness about inspection requirements. Now looking at yearly trends, traffic violations were lowest in 2021, which is most likely due to reduced driving activity and change in enforcement priorities during the COVID-19 pandemic. However, by 2023, violations peaked, with noticeable increases in younger age groups. This increase may possibly be driven by increased post-pandemic driving activity or improved enforcement efforts. The data suggests that there could be potential disparities in enforcement or targeting, as some demographics may see higher violation rates. To address these potential disparities, there should be increased awareness about inspection requirements and the risk of partaking in poor driving behaviors. This data may also help in enforcement strategies by shifting the focus on the top 5 traffic violations. Overall, analyzing this data is the first step to reducing violations, improving road safety, and better traffic enforcement in New York City.")
              )
            ),
            
  ),
  
  ##NEW CODE
  # Tab 6: Detailed Violation Analysis
  nav_panel("Detailed Violation Analysis",
            layout_columns(
              card(
                card_header("Select Violation"),
                selectInput("detailed_violation", "Choose Violation Type:",
                            choices = all_violations,
                            selected = all_violations[1])
              ),
              card(
                card_header("Distribution by Age Group"),
                plotOutput("violation_by_age")
              ),
              card(
                card_header("Distribution by Gender"),
                plotOutput("violation_by_gender")
              ),
              card(
                card_header("Age and Gender Heatmap"),
                plotOutput("violation_heatmap")
              ),
              card(
                card_header("Key Statistics"),
                textOutput("violation_stats")
              )
            )
  ),
  theme = bs_theme(version = 5)
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
    
    if (input$violation != "Top 5") {
      data <- data %>% filter(Violation.Description == input$violation)
    }
    
    # Filter for only top 5 violations
    data %>% filter(Violation.Description %in% top_5_violations)
  })
  
  # Plot output - modified to show only top 5 violations with value labels (added proportion)
  output$ticketPlot <- renderPlot({
    plot_data <- filtered_data() %>%
      count(Violation.Description, Gender) %>%
      group_by(Gender) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ungroup()
    
    if (input$view_type == "count") {
      ggplot(plot_data, aes(x = Violation.Description, y = n, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = n), position = position_dodge(0.9), vjust = -0.5) +
        labs(y = "Count", title = "Traffic Violations by Type and Gender")
    } else {
      ggplot(plot_data, aes(x = Violation.Description, y = percentage, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = paste0(round(percentage, 1), "%")),
                  position = position_dodge(0.9), vjust = -0.5) +
        labs(y = "Proportion (%)", title = "Proportion of Violations by Type and Gender")
    }
  })
  
  # Scatter plot of age range by total violation
  output$scatterPlot <- renderPlot({
    traffic_data_2 <- traffic_data %>%
      group_by(Age_Group) %>%
      summarise(tot_violations = n())
    
    ggplot(data = traffic_data_2, mapping = aes(x = Age_Group, y = tot_violations)) +
      geom_point(size = 3, color = "steelblue") +
      theme_minimal() +
      labs(title = "Scatter Plot of Age Range by Total Violation",
           x = "Age Range",
           y = "Total Violations") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top 5 types of violations by age range and gender with value labels
  output$topViolationsPlot <- renderPlot({
    filtered_top_data <- filtered_data() %>%
      group_by(Age_Group, Violation.Description) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(Age_Group) %>%
      mutate(percentage = count / sum(count) * 100)
    
    if (input$view_type == "count") {
      ggplot(filtered_top_data, aes(x = Age_Group, y = count, fill = Violation.Description)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = count), position = position_dodge(0.9), vjust = -0.5) +
        labs(y = "Count", title = "Top Violations by Age Group")
    } else {
      ggplot(filtered_top_data, aes(x = Age_Group, y = percentage, fill = Violation.Description)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                  position = position_dodge(0.9), vjust = -0.5) +
        labs(y = "Proportion (%)", title = "Proportion of Violations by Age Group")
    }
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
  
  # Opening R Markdown File
  observeEvent(input$show_report, {
    showModal(modalDialog(
      title = "Detailed Traffic Report/Analysis",
      includeMarkdown("frontEnd.Rmd"),
      size = "l",  
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Interactive Time Series Plot
  filtered_interactive_data <- reactive({
    if (input$age_group_interactive == "All") {
      plot_data  
    } else {
      plot_data %>% filter(Age_Group == input$age_group_interactive)
    }
  })
  
  output$interactivePlot <- renderPlotly({
    req(nrow(filtered_interactive_data()) > 0)
    
    p <- ggplot(filtered_interactive_data(), 
                aes(x = Violation.Year, y = total_violations, 
                    color = Age_Group, group = Age_Group)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal(base_size = 14) +
      scale_x_continuous(breaks = seq(min(plot_data$Violation.Year), 
                                      max(plot_data$Violation.Year), 1)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_color_brewer(palette = "Set2") +
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
  
  # Opening R Markdown File
  observeEvent(input$show_report, {
    showModal(modalDialog(
      title = "Detailed Traffic Report/Analysis",
      includeMarkdown("frontEnd.Rmd"),
      size = "l",  
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  ##NEW CODE
  # New reactive dataset for detailed violation analysis
  detailed_violation_data <- reactive({
    traffic_data %>%
      filter(Violation.Description == input$detailed_violation)
  })
  
  ##NEW CODE
  # Plot by age group
  output$violation_by_age <- renderPlot({
    detailed_violation_data() %>%
      count(Age_Group) %>%
      ggplot(aes(x = Age_Group, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = n), vjust = -0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Age Group", y = "Number of Violations",
           title = "Violation Distribution by Age Group")
  })
  
  ##NEW CODE
  # Plot by gender
  output$violation_by_gender <- renderPlot({
    detailed_violation_data() %>%
      count(Gender) %>%
      ggplot(aes(x = Gender, y = n, fill = Gender)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), vjust = -0.5) +
      theme_minimal() +
      labs(x = "Gender", y = "Number of Violations",
           title = "Violation Distribution by Gender")
  })
  
  ##NEW CODE
  # Heatmap of age and gender
  output$violation_heatmap <- renderPlot({
    detailed_violation_data() %>%
      count(Age_Group, Gender) %>%
      ggplot(aes(x = Age_Group, y = Gender, fill = n)) +
      geom_tile() +
      geom_text(aes(label = n), color = "white") +
      scale_fill_viridis_c() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Age Group", y = "Gender", 
           title = "Age and Gender Distribution Heatmap")
  })
  
  ##NEW CODE
  # Key statistics
  output$violation_stats <- renderText({
    data <- detailed_violation_data()
    total_violations <- nrow(data)
    most_common_age <- names(which.max(table(data$Age_Group)))
    most_common_gender <- names(which.max(table(data$Gender)))
    
    paste0(
      "Total violations: ", total_violations, "\n",
      "Most common age group: ", most_common_age, "\n",
      "Most common gender: ", most_common_gender
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)