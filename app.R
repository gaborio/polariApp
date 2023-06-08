#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(sensataDataAnalysis)
font_add_google(name = "Open Sans", family = "Open Sans")
font_add_google(name = "News Cycle", family = "News Cycle")
font_add_google(name = "Lato", family = "Lato")
showtext_auto()



gabo_theme <- theme(
  text = element_text(family = "Open Sans"),
  axis.title = element_text(colour = "white"),
  axis.text.x = element_text(colour = "white"),
  axis.text.y = element_text(colour = "white"),
  legend.text = element_text(colour = "white"),
  legend.title = element_text(colour = "white"),
  plot.title = element_text(
    family = "Lato",
    colour = "white",
    lineheight = 1
  ),
  plot.caption = element_text(colour = "gray20"),
  plot.subtitle = element_text(family = "Lato", colour = "white"),
  panel.background = element_rect(linewidth = 0, fill = "gray90"),
  plot.background = element_rect(fill = "#333333"),
  panel.grid.major = element_line(
    linewidth = 0.5, linetype = "solid",
    colour = "gray85"
  ),
  legend.key = element_rect(fill = "white", colour = "white"),
  panel.grid.major.x = element_blank(),
  legend.position = "none"
)

empre_data <- readRDS("data/afectoEmpresarios.rds") |> droplevels()
empre_data$Grupo <- "Sector privado"
sindi_data <- readRDS("data/afectoSindicalistas.rds") |> droplevels()
sindi_data$Grupo <- "Sindicalistas"
data <- empre_data |> bind_rows(sindi_data)
data$Pregunta <- data$Pregunta |> recode(
  "Confia que cumplen normas de tránsito" = "Cumplen normas tránsito",
  "Confia que pagan salarios justos" = "Pagan salarios justos",
  "Estaría feliz de que sus hijos sean amigos" = "Feliz cercanía social"
)

data$Grupo2 <- data$Grupo2 |>
  recode(
    "Empresarios informales" = "Emp. informales",
    "Líderes sociales" = "Líderes soc.",
    "Otros ciudadanos" = "Ciudadanos"
  )

# Define a function that takes a country name and returns a plot
plot_by_country <- function(country, df) {
  ggplot(filter(data, Pais == country), aes(x = Grupo2, y = mean)) +
    geom_point(
      size = 1.5,
      aes(colour = Pregunta)
    ) +
    geom_segment(
      aes(
        x = Grupo2,
        xend = Grupo2,
        y = 0,
        yend = mean,
        colour = Pregunta
      ),
      alpha = 0.75,
      linewidth = 1
    ) +
    geom_text(
      aes(
        label = paste0(round(mean, 1), "%"),
        group = mean
      ),
      nudge_y = 5,
      size = 3.5,
      color = "gray20",
      family = "Lato"
    ) +
    facet_grid(cols = vars(Pregunta), rows = vars(Grupo)) +
    labs(
      title = paste("Polarización afectiva entre empresarios y sindicalistas en", country),
      subtitle = paste(strwrap("Porcentaje de respuesta de las dos opciones más positivas", width = 63), collapse = "\n"),
      x = "Grupo de la población",
      y = "%",
      caption = "Datos del proyecto Tejiendo Puentes de Sensata UX Research"
    ) +
    #  scale_x_discrete(labels = str_wrap(empre_data$Grupo2, width = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    gabo_theme +
    scale_colour_brewer(palette = "Dark2") +
    ylim(0, 100)
}

# Apply the function to each value of Pais and store the plots in a list
countries <- c("Argentina", "Brasil", "Colombia", "México")
empre_plot_list <- map(countries, plot_by_country) |>
  set_names(countries)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  # Add custom CSS rules
  tags$style(
    HTML("
        h1 {
        font-family: 'Lato', sans-serif;
        }

        label {
        font-family: 'Lato', sans-serif;
        }
        
        p {font-family: 'Lato';
        }")
  ),
  tags$head(tags$style("#text {font-family: 'Lato';}")),

  # Application title
  titlePanel("Polarización Afectiva en Cuatro países de América Latina"),
  

  # Create a grid layout with one row and two columns
  sidebarLayout(
    # sideBar with the input
    sidebarPanel( width = 3,
      selectInput(
        inputId = "Pais",
        label = "Selecciona el país",
        choices = c("Argentina" = "Argentina", "Brasil" = "Brasil", "Colombia" = "Colombia", "México" = "México")
      )
    ),
    # Third column with the plot and sample size
    mainPanel(width = 12,
    "¿Qué tanto los empresarios ven a los sindicalistas de una manera negativa y  a si mismos de manera positiva? ¿Qué tanto pasa lo contrario entre los sindicalistas? Hablamos de polarización afectiva cuando un grupo se ve a sí mismo de manera positiva y al otro de manera negativa. Esta herramienta te permite observar qué tanta polarización afectiva existe entre el Sector Privado y los Sindicalistas en Argentina, Brasil, Colombia y México. Para esto usé los datos recolectados por Sensata UX Research, en su proyecto Tejiendo Puentes.",
    tags$a(href="https://sensata.io/es/tejiendo-puentes", 
           "Ve a la página"), " del proyecto para tener más información de los datos (más de 10.000 encuestas virtuales), el proyecto y las preguntas.",
      plotOutput("plot"),
      textOutput("text"),
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    empre_plot_list[[input$Pais]]
  })
  output$text <- renderText({
    pais <- input$Pais
    if (pais == "Argentina") {
      "La gráfica de Argentina se basa en los datos de 3541 encuestas virtuales"
    } else if (pais == "Brasil") {
      "La gráfica de Brasil se basa en los datos de 2273 encuestas virtuales"
    } else if (pais == "Colombia") {
      "La gráfica de Brasil se basa en los datos de 2812 encuestas virtuales"
    } else if (pais == "México") {
      "La gráfica de México se basa en los datos de 2622 encuestas virtuales"
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
