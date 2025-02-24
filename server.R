#==============================================================================#
# Polarización Afectiva en América Latina
# Visualization of affective polarization between business leaders and unionists
# 
# server.R - Server file
#
# Author: Gabriel N. Camargo-Toledo
# Date Created: February 21, 2025
# Last Updated: February 21, 2025
#==============================================================================#

server <- function(input, output, session) {
  
  # Generate insights based on the country data
  get_insights <- function(country) {
    # Get data for the selected country
    country_data <- filter(data, Pais == country)
    
    # Calculate metrics with error checking
    tryCatch({
      # Views about the private sector
      bigbusi_private_data <- filter(country_data, 
                                  Grupo == "Grandes empresarios" & 
                                    Grupo2 == "Sector privado")
      bigbusi_informal_data <- filter(country_data, 
                                      Grupo == "Grandes empresarios" & 
                                        Grupo2 == "Emp. informales")
      bigbusi_students_data <- filter(country_data, 
                                      Grupo == "Grandes empresarios" & 
                                        Grupo2 == "Estudiantes")
      bigbusi_leaders_data <- filter(country_data, 
                                     Grupo == "Grandes empresarios" & 
                                       Grupo2 == "Líderes soc.")
      bigbusi_citizen_data <- filter(country_data, 
                                     Grupo == "Grandes empresarios" & 
                                       Grupo2 == "Ciudadanos")
      
      # View about the unions
      union_private_data <- filter(country_data, 
                                   Grupo == "Sindicalistas" & 
                                     Grupo2 == "Sector privado")
      union_students_data <- filter(country_data, 
                                    Grupo == "Sindicalistas" & 
                                      Grupo2 == "Estudiantes")
      union_informals_data <- filter(country_data, 
                                     Grupo == "Sindicalistas" & 
                                       Grupo2 == "Emp. informales")
      union_leaders_data <- filter(country_data, 
                                   Grupo == "Sindicalistas" & 
                                     Grupo2 == "Líderes soc.")
      union_citizen_data <- filter(country_data, 
                                   Grupo == "Sindicalistas" & 
                                     Grupo2 == "Ciudadanos")
      
      # Check if we have data for all groups
      if (nrow(bigbusi_private_data) == 0 || nrow(bigbusi_citizen_data) == 0 ||
          nrow(union_private_data) == 0 || nrow(union_citizen_data) == 0) {
        return(HTML("<p>No hay datos suficientes para generar interpretaciones para este país.</p>"))
      }
      
      # Calculate means
      bigbusi_private_view <- mean(bigbusi_private_data$mean, na.rm = TRUE)
      bigbusi_citizen_view <- mean(bigbusi_citizen_data$mean, na.rm = TRUE)
      union_private_view <- mean(union_private_data$mean, na.rm = TRUE)
      union_citizen_view <- mean(union_citizen_data$mean, na.rm = TRUE)
      
      # Calculate polarization
      bigbusi_polarization <- bigbusi_private_view - bigbusi_citizen_view
      union_polarization <- union_private_view - union_citizen_view
      avg_polarization <- mean(c(bigbusi_polarization, union_polarization))
      
      # Determine polarization level
      nivel_polarizacion <- case_when(
        avg_polarization > 40 ~ "Alto",
        avg_polarization > 20 ~ "Moderado",
        TRUE ~ "Bajo"
      )
      
      # Format insights
      insights <- HTML(paste0(
        "<div style='font-family: \"Source Sans Pro\", sans-serif; color: var(--awesome-gray);'>",
        "<p><strong style='color: var(--awesome-darkgray);'>Nivel de polarización:</strong> ", 
        nivel_polarizacion, "</p>",
        
        "<p><strong style='color: var(--awesome-darkgray);'>Hacia los grandes empresarios:</strong> Los miembros del sector privado ven a los grandes empresarios con un <strong style='color: var(--awesome-emerald);'>",
        round(bigbusi_self_view, 0), 
        "%</strong> de valoración positiva, mientras que los ciudadanos los ven <strong style='color: var(--awesome-skyblue);'>",
        round(bigbusi_citizen_view, 0), 
        "%</strong> (diferencia de ", 
        round(bigbusi_polarization, 0), " puntos).</p>",
        
        "<p><strong style='color: var(--awesome-darkgray);'Hacia los sindicalistas:</strong> El sector privado ve a los sindicalistas con un <strong style='color: var(--awesome-emerald);'>",
        round(union_private_view, 0),
        "%</strong> de valoración positiva, mientras que ciudadanos con un <strong style='color: var(--awesome-skyblue);'>",
        round(union_private_view, 0),
        "%</strong> (diferencia de ",
        round(union_polarization, 0), " puntos).</p>",
        
        "<p><strong style='color: var(--awesome-darkgray);'>Observación clave:</strong> ",
        if (abs(bigbusi_polarization - union_polarization) < 5) {
          "Ambos grupos muestran niveles similares de polarización afectiva."
        } else if (bigbusi_polarization > union_polarization) {
          "Hay mayor polarización afectiva hacia los grandes empresarios que hacia los sindicalistas."
        } else {
          "Hay mayor polarización afectiva hacia los sindicalistas que hacia los grandes empresarios."
        },
        "</p>",
        "</div>"
      ))
      
      return(insights)
      
    }, error = function(e) {
      return(HTML("<p style='font-family: \"Source Sans Pro\", sans-serif; color: var(--awesome-gray);'>Hubo un error al procesar los datos para la interpretación.</p>"))
    })
  }
  
  # Render the plot
  output$polarizationPlot <- renderPlotly({
    create_interactive_plot(input$Pais, data, input$vizType)
  })
  
  # Render insights
  output$insights <- renderUI({
    get_insights(input$Pais)
  })
  
  # Render data table
  output$dataTable <- renderDT({
    datatable(
      create_data_table(input$Pais),
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        searchHighlight = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({",
          "'background-color': 'var(--awesome-white)',", 
          "'color': 'var(--awesome-darkgray)',",
          "'font-family': '\"Roboto\", sans-serif'",
          "});",
          "}"
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      filter = 'top',
      extensions = 'Buttons'
    ) %>%
      formatRound(columns = 'Porcentaje positivo (%)', digits = 1)
  })
  
  # Download data handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("polarizacion-afectiva-", input$Pais, "-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(create_data_table(input$Pais), file, row.names = FALSE)
    }
  )
  
  # Download plot handler
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("grafica-polarizacion-", input$Pais, "-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      p <- create_interactive_plot(input$Pais, data, input$vizType)
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300)
    }
  )
}
