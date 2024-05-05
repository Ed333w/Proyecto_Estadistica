library(shiny)
library(tidyverse)


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


server <- function(input, output, session) {
  

  filtered_data <- reactive({
    req(file.exists("filtered_data.csv"))
    read.csv("filtered_data.csv", header = TRUE)
  })
  

  filtered_data_reactive <- reactive({
    req(filtered_data())
    filtered_data() %>%
      filter(AGE >= input$age_range[1] & AGE <= input$age_range[2])
  })
  

  output$boxplot <- renderPlot({
    req(filtered_data_reactive())
    ggplot(filtered_data_reactive(), aes_string(x = NULL, y = input$variable)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(title = paste("Wage plot de", input$variable),
           x = NULL, y = input$variable)
  })
  

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


shinyApp(ui = ui, server = server)





