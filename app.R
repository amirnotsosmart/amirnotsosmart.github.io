library(shiny)
library(ggplot2)
library(dplyr)  
library(tidyverse)

# Read in your dataset
goat_data <- read.csv("goat.csv")

ui <- fluidPage(
  titlePanel("Messi vs Ronaldo: A Comparative Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Choose Player", choices = unique(goat_data$Player)),
      # selectInput("season", "Choose Season", choices = unique(goat_data$Season)),
      # selectInput("competition", "Choose Competition", choices = unique(goat_data$Competition)),
      actionButton("go", "Generate Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Goals Over Seasons", plotOutput("timePlot")),
        tabPanel("Goals by Position", plotOutput("compPlot")),
        tabPanel("Goals by Type", plotOutput("typePlot")),
        tabPanel("Goal Heatmap", plotOutput("heatmapPlot"))
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$go, {
    # Update plots when the 'go' button is pressed

    
    output$timePlot <- renderPlot({
      df <- goat_data %>%
        filter(Player == input$player) %>%
        group_by(Season) %>%
        count()
      if (nrow(df) > 0) {
        ggplot(df, aes(x = Season, y = n, group = 1, color = as.factor(input$player))) +
          geom_line() +
          geom_point() +
          theme_minimal() +
          labs(title = "Total Goals Over Seasons", x = "Season", y = "Total Goals") +
          theme(axis.text.x = element_text(angle = 60))
      }
    })
    
    # Corrected Bar Plot Code
    output$compPlot <- renderPlot({
      df <- goat_data %>%
        filter(Player == input$player) %>%
        group_by(Playing_Position) %>%
        count()
      if (nrow(df) > 0) {
        ggplot(df, aes(x = Playing_Position, y = n, fill = factor(input$player))) +
          geom_bar(stat = "identity", position = position_dodge()) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Goals by Position", x = "Position", y = "Total Goals") +
          theme(axis.text = element_text(angle = 60)) 
      }
      
    })
    
    # Corrected Heatmap Code
    output$heatmapPlot <- renderPlot({
      filtered_data <- goat_data %>%
        filter(Player == input$player) %>%
        group_by(Minute) %>%
        summarise(TotalGoals = n()) %>%
        ungroup() %>%
        mutate(Minute = as.numeric(Minute)) %>%
        arrange(Minute)
      print(str(filtered_data))
      ggplot(filtered_data, aes(x = Minute, y = factor(1), fill = TotalGoals)) + # Assuming 'Minute' is numeric
        geom_tile() +
        scale_fill_gradient(low = "lightblue", high = "blue") +
        labs(title = "Goal Distribution by Match Minute", x = "Minute of Match", y = "") +
        theme(axis.text.x = element_text(angle = 90))
    })
    
    
    # Render a bar plot for goals by type
    output$typePlot <- renderPlot({
      filtered_data <- goat_data %>%
        filter(Player == input$player) %>%
        group_by(Type) %>%
        summarise(TotalGoals = n())  # Count the number of rows in each group
      
      # Plot the data
      ggplot(filtered_data, aes(x = Type, y = TotalGoals)) +
        geom_col() +
        theme_minimal() +
        labs(title = "Total Goals by Type", x = "Type of Goal", y = "Total Goals") +
        theme(axis.text = element_text(angle = 60)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 8))
    })
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)



