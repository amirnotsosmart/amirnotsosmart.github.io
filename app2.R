library(shiny)
library(ggplot2)
library(dplyr)  
library(tidyverse)

# Read in your dataset
goat_data <- read.csv("goat.csv")

ui <- fluidPage(
  titlePanel("Messi vs Ronaldo: By the Seasons"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Choose Player", choices = unique(goat_data$Player)),
      selectInput("season", "Choose Season", choices = c("All" = "All", unique(goat_data$Season))),
      selectInput("competition", "Choose Competition", choices = c("All" = "All", unique(goat_data$Competition))),
      actionButton("go", "Generate Analysis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Competition Comparison", plotOutput("competitionComparisonPlot")),
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$go, {
    # Update plots when the 'go' button is pressed
    
    output$competitionComparisonPlot <- renderPlot({
      df <- goatdebate %>%
        filter(Competition == input$competition) %>%
        group_by(Player) %>%
        summarise(TotalGoals = n(), TotalAssists = sum(as.numeric(Goal_assist), na.rm = TRUE))
      
      ggplot(df, aes(x = Player, y = TotalGoals, fill = Player)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        labs(title = "Competition Comparison", x = "Player", y = "Total Goals")
    })
    
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
