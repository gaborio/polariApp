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
  axis.title = element_text(colour = "dimgray"),
  axis.text.x = element_text(colour = "dimgray"),
  axis.text.y = element_text(colour = "dimgray"),
  legend.text = element_text(colour = "dimgray"),
  legend.title = element_text(colour = "dimgray"),
  plot.title = element_text(
    family = "Lato",
    colour = "black",
    lineheight = 1
  ),
  plot.caption = element_text(colour = "dimgray"),
  plot.subtitle = element_text(family = "Lato"),
  panel.background = element_rect(linewidth = 0, fill = "white"),
  panel.grid.major = element_line(
    linewidth = 0.5, linetype = "solid",
    colour = "gray85"
  ),
  legend.key = element_rect(fill = "white", colour = "white"),
  panel.grid.major.x = element_blank(),
  legend.position = "none"
)

empre_data <- readRDS("data/afectoEmpresarios.rds") |> droplevels()
empre_data$Grupo <- "Empresarios"
sindi_data <- readRDS("data/afectoSindicalistas.rds") |> droplevels()
sindi_data$Grupo <- "Sindicalistas"
data <- empre_data |> bind_rows(sindi_data)
data$Pregunta <- data$Pregunta |> recode("Confia que cumplen normas de tránsito" = "Cumplen normas tránsito",
                                         "Confia que pagan salarios justos" = "Pagan salarios justos",
                                         "Estaría feliz de que sus hijos sean amigos" = "Feliz cercanía social")

data$Grupo2 <- data$Grupo2 |> 
  recode("Empresarios informales" = "Emp. informales",
         "Líderes sociales" = "Líderes soc.",
         "Otros ciudadanos" = "Ciudadanos")

# Define a function that takes a country name and returns a plot
plot_by_country <- function(country, df) {
  ggplot(filter(data, Pais == country), aes(x=Grupo2, y=mean)) +
    geom_point(size = 1.5,
               aes(colour = Pregunta)) +
    geom_segment(aes(x = Grupo2, 
                     xend = Grupo2, 
                     y = 0, 
                     yend = mean,
                     colour = Pregunta),
                 alpha = 0.75,
                 linewidth = 1) +
    geom_text(aes(label = paste0(round(mean,1),"%"),
                  group = mean),
              nudge_y = 5,
              size = 3.5,
              color = "gray40",
              family = "Open Sans") + 
    facet_grid(cols = vars(Pregunta), rows = vars(Grupo)) +
    labs(title = paste("Polarización afectiva entre empresarios y sindicalistas en", country),
         subtitle = paste(strwrap("Porcentaje de respuesta de las dos opciones más positivas", width = 63), collapse = "\n"),
         x = "Grupo de la población",
         y = "%",
         caption = "Datos del proyecto Tejiendo Puentes de Sensata UX Research") +
  #  scale_x_discrete(labels = str_wrap(empre_data$Grupo2, width = 10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    gabo_theme +
    scale_colour_brewer(palette = "Dark2") + ylim (0,100) 
}

# Apply the function to each value of Pais and store the plots in a list
countries <- c("Argentina", "Brasil", "Colombia", "México")
empre_plot_list <- map(countries, plot_by_country) |> 
  set_names(countries)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("sandstone"),

    # Application title
    titlePanel("Polarización Afectiva en Cuatro países de América Latina"),

    # Create a grid layout with one row and two columns
    fluidRow(
      # First column with the input
      column(width=12,
             selectInput(inputId="Pais",
                         label="Selecciona qué País te interesa",
                         choices=c("Argentina"="Argentina","Brasil"="Brasil","Colombia"="Colombia","México"="México"))
      ),
      # Second column with the plot
      column(width=12,
             plotOutput("plot")
      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot <- renderPlot({
    empre_plot_list[[input$Pais]]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
