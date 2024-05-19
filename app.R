# Define UI
ui <- navbarPage(
  title = "Analisis Salarial",
  theme = shinytheme("cyborg"), # Use a dark theme for better contrast
  
  tabPanel("Wage Plot",
           sidebarLayout(
             sidebarPanel(
               selectInput("variable", "Seleccione el Gemelo:", 
                           choices = c("HRWAGEL", "HRWAGEH"), selected = "HRWAGEL"),
               sliderInput("age_range", "Filtrar por Edad:",
                           min = 0, max = 100, value = c(0, 100), step = 1),
               downloadButton("downloadData", " Datos Filtrados", class = "btn-primary")
             ),
             mainPanel(
               plotOutput("boxplot")
             )
           )
  ),
  
  tabPanel("Datos en Crudo",
           fluidPage(
             h3("Analísis de la base de datos"),
             p("Usamos las herramientas de analsis de datos de tidyverse para
               hacer los filtros y la limpieza de datos:"),
             verbatimTextOutput("raw_output"),
             br(),
             h3("Datos Filtrados"),
             tableOutput("filtered_table")
           )
  ),
  
  tabPanel("Analisis Númerico",
           fluidPage(
             h3("Análisis Numérico"),
             p("En esta sección se proporciona un análisis numérico de los datos salariales.")
           )
  ),
  
  tags$style(HTML("
    body { background-color: #000; color: #fff; }
    .navbar { background-color: #1a1a1a; }
    .navbar-brand { color: #fff !important; }
    .navbar-nav > li > a { color: #fff !important; }
    .btn-primary { background-color: #1a1a1a; border-color: #1a1a1a; color: #fff; }
    .sidebarPanel, .mainPanel { background-color: #333; }
    h3, p, label, .control-label, .shiny-input-container { color: #fff; }
    .slider-animate-container, .irs-single, .irs-bar-edge, .irs-bar { background: #666; border-color: #666; }
    .irs-line { border: 1px solid #999; }
    .irs-grid-text, .irs-min, .irs-max { color: #999; }
  "))
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
    
    boxplot_color <- ifelse(input$variable == "HRWAGEL", "skyblue", "orange")
    
    ggplot(filtered_data_reactive(), aes_string(x = NULL, y = input$variable)) +
      geom_boxplot(fill = boxplot_color, color = "black", outlier.colour = "white") +
      labs(title = paste("Wage Plot of", input$variable),
           x = NULL, y = input$variable) +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "#000000", color = NA),
        panel.background = element_rect(fill = "#000000", color = NA),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_line(color = "#222222"),
        axis.text = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#ffffff", size = 20, face = "bold"),
        legend.background = element_rect(fill = "#000000"),
        legend.key = element_rect(fill = "#000000", color = NA),
        legend.text = element_text(color = "#ffffff")
      )
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
  
  output$raw_output <- renderPrint({
    raw <- read.table("twins.txt", header = TRUE, sep = ",")
    
    cat("Numero de registros (rows):", nrow(raw), "\n")
    cat("Numero de variables (columns):", ncol(raw), "\n")
    
    gem <- read.table("twins.txt", header = TRUE, sep = ",", dec = ".", na.strings = ".")
    
    # Define la filtración por variables
    filters <- c("DLHRWAGE", "AGE", "DEDUC1", "AGESQ", "HRWAGEH", "WHITEH", 
                 "MALEH", "EDUCH", "HRWAGEL", "WHITEL", "MALEL", "EDUCL", 
                 "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")
    
    # Limpieza de datos
    # Eliminar los datos que contengan valores vacíos
    filtered_data <- gem %>%
      filter(across(all_of(filters), ~(!is.na(.) & (nchar(as.character(.)) > 1 | . != "."))))
    
    cat("Número de registros con información completa:", nrow(filtered_data), "\n")
  })
  
  output$filtered_table <- renderTable({
    filtered_data <- gem %>%
      filter(across(all_of(filters), ~(!is.na(.) & (nchar(as.character(.)) > 1 | . != "."))))
    
    filtered_data
  })
  
  

}

# Run the application
shinyApp(ui = ui, server = server)





