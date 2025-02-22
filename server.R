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
    country_data <- filter(data, Pais == country)
    
    # Calculate polarization metrics
    private_self_view <- mean(filter(country_data, Grupo == "Sector privado" & Grupo2 == "Empresarios formales")$mean)
    private_union_view <- mean(filter(country_data, Grupo == "Sector privado" & Grupo2 == "Sindicalistas")$mean)
    union_self_view <- mean(filter(country_data, Grupo == "Sindicalistas" & Grupo2 == "Sindicalistas")$mean) 
    union_private_view <- mean(filter(country_data, Grupo == "Sindicalistas" & Grupo2 == "Empresarios formales")$mean)
    
    private_polarization <- private_self_view - private_union_view
    union_polarization <- union_self_view - union_private_view
    
    # Format insights based on data
    insights <- HTML(paste0(
      "<p><strong>Nivel de polarización:</strong> ",
      if(mean(c(private_polarization, union_polarization)) > 40) "Alto" else 
        if(mean(c(private_polarization, union_polarization)) > 20) "Moderado" else "Bajo",
      "</p>",
      
      "<p><strong>Sector privado:</strong> Ve a los empresarios formales con un <strong>", round(private_self_view, 1), 
      "%</strong> de valoración positiva, mientras que a los sindicalistas con un <strong>", 
      round(private_union_view, 1), "%</strong> (diferencia de ", round(private_polarization, 1), " puntos).</p>",
      
      "<p><strong>Sindicalistas:</strong> Se ven a sí mismos con un <strong>", round(union_self_view, 1), 
      "%</strong> de valoración positiva, mientras que a los empresarios formales con un <strong>", 
      round(union_private_view, 1), "%</strong> (diferencia de ", round(union_polarization, 1), " puntos).</p>",
      
      "<p><strong>Observación clave:</strong> ", 
      if(private_polarization > union_polarization) 
        "El sector privado muestra mayor polarización afectiva que los sindicalistas." 
      else if(union_polarization > private_polarization) 
        "Los sindicalistas muestran mayor polarización afectiva que el sector privado."
      else 
        "Ambos grupos muestran niveles similares de polarización afectiva.",
      "</p>"
    ))
    
    return(insights)
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
          "$(this.api().table().header()).css({'background-color': '#424242', 'color': 'white'});",
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
      # Create static ggplot for download (higher quality than plotly export)
      p <- ggplot(filter(data, Pais == input$Pais), aes(x = Grupo2, y = mean)) +
        geom_point(size = 3, aes(colour = Pregunta)) +
        geom_segment(
          aes(x = Grupo2, xend = Grupo2, y = 0, yend = mean, colour = Pregunta),
          alpha = 0.85, linewidth = 1.2
        ) +
        geom_text(
          aes(label = paste0(round(mean, 1), "%")),
          nudge_y = 5, size = 3.5, color = "white", family = "Lato"
        ) +
        facet_grid(cols = vars(Pregunta), rows = vars(Grupo)) +
        labs(
          title = paste("Polarización afectiva entre empresarios y sindicalistas en", input$Pais),
          subtitle = "Porcentaje de respuesta de las dos opciones más positivas",
          x = "Grupo de la población evaluado",
          y = "Porcentaje de respuestas positivas (%)",
          caption = paste("Datos de", sample_sizes[[input$Pais]], "encuestas del proyecto Tejiendo Puentes")
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        viztheme +
        scale_colour_brewer(palette = "Set2") +
        ylim(0, 100)
      
      ggsave(file, plot = p, width = 10, height = 8, dpi = 300)
    }
  )
}
