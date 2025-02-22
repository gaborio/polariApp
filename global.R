#==============================================================================#
# Polarización afectiva en América Latina
# Visualization of affective polarization between business leaders, and civil society
# 
# GLOBAL.R - Package loading and data preparation
#
# Author: Gabriel N. Camargo-Toledo
# Date Created: June 08, 2023
# Last Updated: February 21, 2025
# 
# Description: This Shiny application visualizes data from Sensata UX Research's
# "Tejiendo Puentes" project, examining affective polarization between different
# social groups across Argentina, Brazil, Colombia, and Mexico.
#
# Data source: Sensata UX Research (https://sensata.io/es/tejiendo-puentes)
# Sample: Over 10,000 virtual surveys across four Latin American countries
#
# Dependencies:
#   - shiny, shinythemes, tidyverse, plotly, DT, showtext, shinyWidgets
#
# Contact: gabriel.n.c.t182@gmail.com, gn.camargo215@uniandes.edu.co
# Repository: https://github.com/gaborio/polariApp
#==============================================================================#

# Load required packages --------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(showtext)
library(DT)

# Load fonts ---------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font_add_google(name = "Lato", family = "Lato")
showtext_auto()

# Define the visualization theme -------
viztheme <- theme(
  text = element_text(family = "Open Sans"),
  axis.title = element_text(colour = "white", size = 11, face = "bold"),
  axis.text.x = element_text(colour = "white", size = 10),
  axis.text.y = element_text(colour = "white", size = 10),
  legend.text = element_text(colour = "white", size = 10),
  legend.title = element_text(colour = "white", size = 11, face = "bold"),
  plot.title = element_text(
    family = "Lato",
    colour = "white",
    size = 14,
    face = "bold",
    hjust = 0
  ),
  plot.caption = element_text(colour = "gray80", size = 8),
  plot.subtitle = element_text(family = "Lato", colour = "white", size = 11),
  panel.background = element_rect(linewidth = 0, fill = "#424242"),
  plot.background = element_rect(fill = "#333333"),
  panel.grid.major = element_line(
    linewidth = 0.5, linetype = "dotted",
    colour = "gray75"
  ),
  panel.grid.minor = element_blank(),
  legend.key = element_rect(fill = "transparent", colour = "transparent"),
  panel.grid.major.x = element_blank(),
  legend.position = "bottom",
  legend.background = element_rect(fill = "#333333", color = NA),
  legend.box.background = element_rect(fill = "#333333"),
  strip.background = element_rect(fill = "#555555"),
  strip.text = element_text(colour = "white", size = 11, face = "bold")
)

# Load and process data ----------
empre_data <- readRDS("data/afectoEmpresarios.rds") |> droplevels()
empre_data$Grupo <- "Sector privado"
sindi_data <- readRDS("data/afectoSindicalistas.rds") |> droplevels()
sindi_data$Grupo <- "Sindicalistas"
data <- empre_data |> bind_rows(sindi_data)

# Improved labels for better clarity ----
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

# Sample sizes by country ----
sample_sizes <- list(
  "Argentina" = 3541,
  "Brasil" = 2273,
  "Colombia" = 2812,
  "México" = 2622
)

# Helper function to create interactive plots ----
create_interactive_plot <- function(country, df, view_type = "standard") {
  filtered_data <- filter(data, Pais == country)
  
  if (view_type == "difference") {
    # Create a comparison dataset showing differences between groups
    base_plot <- ggplot(filtered_data, aes(x = Grupo2, y = mean, fill = Grupo)) +
      geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
      geom_text(
        aes(label = paste0(round(mean, 1), "%")),
        position = position_dodge(width = 0.7),
        vjust = -0.5,
        size = 3.5,
        color = "white",
        family = "Lato"
      ) +
      facet_wrap(~Pregunta, ncol = 3) +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title = paste("Comparación de percepciones entre empresarios y sindicalistas en", country),
        subtitle = "Porcentaje de respuestas positivas por grupo",
        x = "Grupo evaluado",
        y = "Porcentaje de respuestas positivas (%)",
        fill = "Grupo que responde",
        caption = paste("Datos de", sample_sizes[[country]], "encuestas del proyecto Tejiendo Puentes de Sensata UX Research")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      viztheme +
      ylim(0, 100)
    
  } else {
    # Original lollipop chart with improved styling
    base_plot <- ggplot(filtered_data, aes(x = Grupo2, y = mean)) +
      geom_point(
        size = 3,
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
        alpha = 0.85,
        linewidth = 1.2
      ) +
      geom_text(
        aes(
          label = paste0(round(mean, 1), "%")
        ),
        nudge_y = 5,
        size = 3.5,
        color = "white",
        family = "Lato"
      ) +
      facet_grid(cols = vars(Pregunta), rows = vars(Grupo)) +
      labs(
        title = paste("Polarización afectiva entre empresarios y sindicalistas en", country),
        subtitle = "Porcentaje de respuesta de las dos opciones más positivas",
        x = "Grupo de la población evaluado",
        y = "Porcentaje de respuestas positivas (%)",
        caption = paste("Datos de", sample_sizes[[country]], "encuestas del proyecto Tejiendo Puentes de Sensata UX Research")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      viztheme +
      scale_colour_brewer(palette = "Set2") +
      ylim(0, 100)
  }
  
  # Convert to interactive plotly
  interactive_plot <- ggplotly(base_plot, tooltip = c("x", "y", "colour", "group"))
  
  # Improve tooltips
  interactive_plot <- interactive_plot %>% 
    layout(
      hoverlabel = list(
        bgcolor = "#333333",
        bordercolor = "white",
        font = list(family = "Lato", size = 12, color = "white")
      ),
      legend = list(orientation = "h", y = -0.2)
    )
  
  return(interactive_plot)
}

# Create data table function for downloading ------
create_data_table <- function(country) {
  filtered_data <- filter(data, Pais == country) %>%
    select(Pais, Grupo, Grupo2, Pregunta, mean) %>%
    rename(
      "País" = Pais,
      "Grupo que responde" = Grupo,
      "Grupo evaluado" = Grupo2,
      "Pregunta" = Pregunta,
      "Porcentaje positivo (%)" = mean
    )
  
  return(filtered_data)
}