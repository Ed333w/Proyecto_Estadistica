library(shiny)
library(tidyverse)

# Define UI
ui <- fluidPage(
  titlePanel("Wage Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Seleciona el gemelo:", 
                  choices = c("HRWAGEL", "HRWAGEH"), selected = "HRWAGEL"),
      sliderInput("age_range", "filtrar por edad:",
                  min = 0, max = 100, value = c(0, 100), step = 1),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load data
  filtered_data <- reactive({
    req(file.exists("filtered_data.csv"))
    read.csv("filtered_data.csv", header = TRUE)
  })
  
  # Filter data based on user inputs
  filtered_data_reactive <- reactive({
    req(filtered_data())
    filtered_data() %>%
      filter(AGE >= input$age_range[1] & AGE <= input$age_range[2])
  })
  
  # Render the boxplot
  output$boxplot <- renderPlot({
    req(filtered_data_reactive())
    ggplot(filtered_data_reactive(), aes_string(x = NULL, y = input$variable)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(title = paste("Wage plot de", input$variable),
           x = NULL, y = input$variable)
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered <- filtered_data_reactive()
      write.csv(filtered, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)





