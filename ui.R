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
  theme = shinytheme("flatly"), # Using flatly as base theme for clean look
  
  tags$head(
    tags$link(rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@400;600&family=Roboto:wght@400;700&display=swap"),
    tags$style(HTML("
      :root {
        --awesome-white: #FFFFFF;
        --awesome-black: #000000;
        --awesome-darkgray: #333333;
        --awesome-gray: #5D5D5D;
        --awesome-lightgray: #999999;
        --awesome-red: #DC3522;
        --awesome-skyblue: #0395DE;
        --awesome-emerald: #00A388;
      }
      
      body {
        font-family: 'Source Sans Pro', sans-serif;
        line-height: 1.6;
        color: var(--awesome-gray);
        background-color: var(--awesome-white);
      }
      
      h1, h2, h3, h4 {
        font-family: 'Roboto', sans-serif;
        font-weight: 700;
        color: var(--awesome-gray);
      }
      
      .well {
        background-color: var(--awesome-white);
        border: 1px solid var(--awesome-lightgray);
        box-shadow: 0 1px 5px rgba(0,0,0,0.1);
      }
      
      .nav-tabs > li.active > a {
        background-color: var(--awesome-white) !important;
        color: var(--awesome-red) !important;
        font-weight: bold;
        border-bottom: 2px solid var(--awesome-red);
      }
      
      .nav-tabs > li > a {
        color: var(--awesome-gray) !important;
      }
      
      .chart-container {
        background-color: var(--awesome-white);
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
        border: 1px solid var(--awesome-lightgray);
      }
      
      .form-control {
        background-color: var(--awesome-white);
        color: var(--awesome-gray);
        border: 1px solid var(--awesome-lightgray);
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      .form-group label {
        color: var(--awesome-darkgray);
        font-weight: 600;
        font-family: 'Roboto', sans-serif;
      }
      
      .btn-primary {
        background-color: var(--awesome-skyblue);
        border-color: var(--awesome-skyblue);
        font-family: 'Source Sans Pro', sans-serif;
        font-weight: 600;
      }
      
      .btn-primary:hover {
        background-color: var(--awesome-emerald);
        border-color: var(--awesome-emerald);
      }
      
      #key_insights {
        background-color: var(--awesome-white);
        padding: 15px;
        border-left: 4px solid var(--awesome-skyblue);
        margin: 20px 0;
        color: var(--awesome-gray);
      }
      
      .footer {
        margin-top: 40px;
        padding-top: 15px;
        border-top: 1px solid var(--awesome-lightgray);
        font-size: 12px;
        color: var(--awesome-gray);
        font-family: 'Source Sans Pro', sans-serif;
      }
      
      /* Custom styling for data table */
      .dataTables_wrapper {
        font-family: 'Source Sans Pro', sans-serif;
        color: var(--awesome-gray);
      }
      
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid var(--awesome-lightgray);
        border-radius: 4px;
      }
      
      .dataTables_wrapper .dataTables_length select {
        border: 1px solid var(--awesome-lightgray);
        border-radius: 4px;
      }
      
      table.dataTable thead th {
        background-color: var(--awesome-white);
        color: var(--awesome-darkgray);
        font-family: 'Roboto', sans-serif;
        font-weight: 700;
        border-bottom: 2px solid var(--awesome-lightgray);
      }
    "))
  ),
  
  # Application title
  navbarPage(
    title = div(
      style = "font-family: 'Roboto', sans-serif; color: var(--awesome-white); font-weight: 700;",
      "Polarización Afectiva en América Latina"
    ),
    
    # Rest of the UI structure remains the same, just updated with new styling
    tabPanel("Visualización",
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Controles", 
                           style = "border-bottom: 1px solid var(--awesome-lightgray); 
                                  padding-bottom: 10px;
                                  color: var(--awesome-darkgray);"),
                        
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
                        
                        hr(style = "border-color: var(--awesome-lightgray);"),
                        
                        downloadButton(
                          "downloadData", 
                          "Descargar datos", 
                          style = "width: 100%; margin-bottom: 10px;
                                 background-color: var(--awesome-skyblue);
                                 border-color: var(--awesome-skyblue);"
                        ),
                        downloadButton(
                          "downloadPlot", 
                          "Descargar gráfica", 
                          style = "width: 100%;
                                 background-color: var(--awesome-skyblue);
                                 border-color: var(--awesome-skyblue);"
                        )
                      )
               ),
               
               column(9,
                      div(class = "chart-container",
                          plotlyOutput("polarizationPlot", height = "600px")
                      ),
                      
                      conditionalPanel(
                        condition = "input.showAnnotations",
                        div(id = "key_insights", 
                            h4("Interpretación clave:", 
                               style = "color: var(--awesome-darkgray); 
                                      font-family: 'Roboto', sans-serif;"),
                            uiOutput("insights"))
                      )
               )
             )
    ),
    
    # Data Explorer tab
    tabPanel("Explorador de datos",
             fluidRow(
               column(12, 
                      h3("Datos de polarización afectiva",
                         style = "color: var(--awesome-darkgray);
                                font-family: 'Roboto', sans-serif;
                                margin-bottom: 20px;"),
                      p("Esta tabla muestra los datos completos para el país seleccionado. 
                        Puedes ordenar y filtrar los datos.",
                        style = "color: var(--awesome-gray);
                               font-family: 'Source Sans Pro', sans-serif;"),
                      hr(style = "border-color: var(--awesome-lightgray);"),
                      DTOutput("dataTable")
               )
             )
    ),
    
    # About tab
    tabPanel("Acerca del estudio",
             fluidRow(
               column(8, offset = 2,
                      h3("Sobre el Proyecto Tejiendo Puentes",
                         style = "color: var(--awesome-darkgray);
                                font-family: 'Roboto', sans-serif;"),
                      p("Esta visualización utiliza datos del proyecto Tejiendo Puentes de Sensata UX Research, 
                        que estudia la polarización afectiva entre diferentes grupos sociales en América Latina.",
                        style = "color: var(--awesome-gray);
                               font-family: 'Source Sans Pro', sans-serif;"),
                      
                      h4("Metodología",
                         style = "color: var(--awesome-darkgray);
                                font-family: 'Roboto', sans-serif;"),
                      p("Los datos fueron recolectados a través de más de 10,000 encuestas virtuales en Argentina, 
                        Brasil, Colombia y México. El estudio examina cómo los empresarios y sindicalistas se perciben 
                        mutuamente en dimensiones como la confianza, la justicia y la disposición para la interacción social.",
                        style = "color: var(--awesome-gray);
                               font-family: 'Source Sans Pro', sans-serif;"),
                      
                      h4("Interpretación de los resultados",
                         style = "color: var(--awesome-darkgray);
                                font-family: 'Roboto', sans-serif;"),
                      p("La polarización afectiva se presenta cuando un grupo ve a sus miembros de manera 
                        positiva mientras percibe al otro grupo de manera negativa. Las diferencias grandes 
                        en los porcentajes entre cómo un grupo se ve a sí mismo versus cómo ve al otro grupo 
                        indican mayor polarización.",
                        style = "color: var(--awesome-gray);
                               font-family: 'Source Sans Pro', sans-serif;"),
                      
                      h4("Enlaces y recursos",
                         style = "color: var(--awesome-darkgray);
                                font-family: 'Roboto', sans-serif;"),
                      tags$ul(
                        tags$li(
                          tags$a(
                            href = "https://sensata.io/es/tejiendo-puentes",
                            "Página oficial del proyecto Tejiendo Puentes",
                            style = "color: var(--awesome-skyblue);"
                          )
                        ),
                        tags$li(
                          tags$a(
                            href = "https://github.com/gaborio/polariApp",
                            "Repositorio de código en GitHub",
                            style = "color: var(--awesome-skyblue);"
                          )
                        ),
                        style = "color: var(--awesome-gray);
                               font-family: 'Source Sans Pro', sans-serif;"
                      )
               )
             ),
             div(class = "footer",
                 fluidRow(
                   column(12, align = "center",
                          p("Desarrollado con Shiny por R Studio • Datos proporcionados por Sensata UX Research",
                            style = "color: var(--awesome-gray);
                                   font-family: 'Source Sans Pro', sans-serif;")
                   )
                 )
             )
    )
  )
)