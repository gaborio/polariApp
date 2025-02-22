#==============================================================================#
# Polarización Afectiva en América Latina
# Visualization of affective polarization between business leaders and unionists
# 
# ui.R - UI file
#
# Author: Gabriel N. Camargo-Toledo
# Date Created: February 21, 2025
# Last Updated: February 21, 2025
#==============================================================================#

ui <- fluidPage(
  theme = shinytheme("slate"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&family=Open+Sans:wght@400;600&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Open Sans', sans-serif;
        line-height: 1.6;
      }
      h1, h2, h3, h4 {
        font-family: 'Lato', sans-serif;
        font-weight: 700;
      }
      .well {
        background-color: #424242;
        border: 1px solid #555;
        box-shadow: 0 1px 5px rgba(0,0,0,0.2);
      }
      .nav-tabs > li.active > a {
        background-color: #424242 !important;
        color: white !important;
        font-weight: bold;
      }
      .chart-container {
        background-color: #333;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .form-control {
        background-color: #555;
        color: white;
        border: 1px solid #777;
      }
      .form-group label {
        color: #ddd;
        font-weight: 600;
      }
      .btn-primary {
        background-color: #5d8aa8;
        border-color: #4a7a98;
      }
      .btn-primary:hover {
        background-color: #4a7a98;
      }
      .footer {
        margin-top: 40px;
        padding-top: 15px;
        border-top: 1px solid #555;
        font-size: 12px;
        color: #aaa;
      }
      #key_insights {
        background-color: #444;
        padding: 15px;
        border-left: 4px solid #5d8aa8;
        margin: 20px 0;
      }
    "))
  ),
  
  # Application title with logo
  navbarPage(
    title = div(
      img(src = "https://sensata.io/images/sensata-logo.png", height = "30px", style = "margin-right: 10px;"),
      "Polarización Afectiva en América Latina"
    ),
    
    # First tab: Visualization
    tabPanel("Visualización",
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Controles", style = "border-bottom: 1px solid #555; padding-bottom: 10px;"),
                        
                        selectInput(
                          inputId = "Pais",
                          label = "Selecciona el país:",
                          choices = c("Argentina", "Brasil", "Colombia", "México"),
                          selected = "Colombia"
                        ),
                        
                        radioButtons(
                          inputId = "vizType",
                          label = "Tipo de visualización:",
                          choices = c(
                            "Estándar (Lollipop)" = "standard", 
                            "Comparativa (Barras)" = "difference"
                          ),
                          selected = "standard"
                        ),
                        
                        checkboxInput(
                          inputId = "showAnnotations",
                          label = "Mostrar interpretación",
                          value = TRUE
                        ),
                        
                        hr(),
                        
                        downloadButton("downloadData", "Descargar datos", 
                                       style = "width: 100%; margin-bottom: 10px;"),
                        downloadButton("downloadPlot", "Descargar gráfica", 
                                       style = "width: 100%;")
                      )
               ),
               
               column(9,
                      div(class = "chart-container",
                          plotlyOutput("polarizationPlot", height = "600px")
                      ),
                      
                      conditionalPanel(
                        condition = "input.showAnnotations",
                        div(id = "key_insights", 
                            h4("Interpretación clave:"),
                            uiOutput("insights"))
                      )
               )
             )
    ),
    
    # Second tab: Data Explorer
    tabPanel("Explorador de datos",
             fluidRow(
               column(12, 
                      h3("Datos de polarización afectiva"),
                      p("Esta tabla muestra los datos completos para el país seleccionado. Puedes ordenar y filtrar los datos."),
                      hr(),
                      DTOutput("dataTable")
               )
             )
    ),
    
    # Third tab: About
    tabPanel("Acerca del estudio",
             fluidRow(
               column(8, offset = 2,
                      h3("Sobre el Proyecto Tejiendo Puentes"),
                      p("Esta visualización utiliza datos del proyecto Tejiendo Puentes de Sensata UX Research, 
            que estudia la polarización afectiva entre diferentes grupos sociales en América Latina."),
                      
                      h4("Metodología"),
                      p("Los datos fueron recolectados a través de más de 10,000 encuestas virtuales en Argentina, 
            Brasil, Colombia y México. El estudio examina cómo los empresarios y sindicalistas se perciben 
            mutuamente en dimensiones como la confianza, la justicia y la disposición para la interacción social."),
                      
                      h4("Interpretación de los resultados"),
                      p("La polarización afectiva se presenta cuando un grupo ve a sus miembros de manera 
            positiva mientras percibe al otro grupo de manera negativa. Las diferencias grandes 
            en los porcentajes entre cómo un grupo se ve a sí mismo versus cómo ve al otro grupo 
            indican mayor polarización."),
                      
                      h4("Enlaces y recursos"),
                      tags$ul(
                        tags$li(tags$a(href = "https://sensata.io/es/tejiendo-puentes", "Página oficial del proyecto Tejiendo Puentes")),
                        tags$li(tags$a(href = "https://github.com/gaborio/polariApp", "Repositorio de código en GitHub"))
                      )
               )
             ),
             div(class = "footer",
                 fluidRow(
                   column(12, align = "center",
                          p("Desarrollado con Shiny por R Studio • Datos proporcionados por Sensata UX Research")
                   )
                 )
             )
    )
  )
)