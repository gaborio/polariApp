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
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

# Define colors from awesome-cv --------
awesome_colors <- list(
  white = "#FFFFFF",
  black = "#000000",
  darkgray = "#333333",
  gray = "#5D5D5D",
  lightgray = "#999999",
  awesome_red = "#DC3522",
  awesome_skyblue = "#0395DE",
  awesome_emerald = "#00A388",
  awesome_pink = "#EF4089",
  awesome_orange = "#FF6138",
  awesome_nephritis = "#27AE60",
  awesome_concrete = "#95A5A6",
  awesome_darknight = "#131A28"
)

# Define the visualization theme -------
viztheme <- theme(
  text = element_text(family = "Source Sans Pro"),
  axis.title = element_text(
    colour = awesome_colors$gray, 
    size = 11, 
    face = "bold",
    family = "Roboto"
  ),
  axis.text.x = element_text(
    colour = awesome_colors$gray, 
    size = 10,
    family = "Source Sans Pro"
  ),
  axis.text.y = element_text(
    colour = awesome_colors$gray, 
    size = 10,
    family = "Source Sans Pro"
  ),
  legend.text = element_text(
    colour = awesome_colors$gray, 
    size = 10,
    family = "Source Sans Pro"
  ),
  legend.title = element_text(
    colour = awesome_colors$darkgray, 
    size = 11, 
    face = "bold",
    family = "Roboto"
  ),
  plot.title = element_text(
    family = "Roboto",
    colour = awesome_colors$darkgray,
    size = 14,
    face = "bold",
    hjust = 0
  ),
  plot.caption = element_text(
    colour = awesome_colors$lightgray, 
    size = 8,
    family = "Source Sans Pro"
  ),
  plot.subtitle = element_text(
    family = "Source Sans Pro", 
    colour = awesome_colors$gray, 
    size = 11
  ),
  panel.background = element_rect(
    linewidth = 0, 
    fill = awesome_colors$white
  ),
  plot.background = element_rect(
    fill = awesome_colors$white
  ),
  panel.grid.major = element_line(
    linewidth = 0.5, 
    linetype = "dotted",
    colour = awesome_colors$lightgray
  ),
  panel.grid.minor = element_blank(),
  legend.key = element_rect(
    fill = "transparent", 
    colour = "transparent"
  ),
  panel.grid.major.x = element_blank(),
  legend.position = "bottom",
  legend.background = element_rect(
    fill = awesome_colors$white, 
    color = NA
  ),
  legend.box.background = element_rect(
    fill = awesome_colors$white
  ),
  strip.background = element_rect(
    fill = awesome_colors$gray
  ),
  strip.text = element_text(
    colour = awesome_colors$white, 
    size = 11, 
    face = "bold",
    family = "Roboto"
  )
)

# Create custom color scales for plots
custom_color_scale <- scale_color_manual(
  values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
             "#0072B2", "#D55E00", "#CC79A7"))

custom_fill_scale <- scale_fill_manual(
  c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
    "#0072B2", "#D55E00", "#CC79A7")
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
  "Estaría feliz de que sus hijos sean amigos" = "Cercanía social"
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
    base_plot <- ggplot(filtered_data, aes(x = Grupo2, y = mean, fill = Grupo)) +
      geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
      geom_text(
        aes(label = paste0(round(mean, 1), "%")),
        position = position_dodge(width = 0.7),
        vjust = -0.5,
        size = 3.5,
        color = awesome_colors$gray,
        family = "Source Sans Pro"
      ) +
      facet_wrap(~Pregunta, ncol = 3) +
      custom_fill_scale +
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
    base_plot <- ggplot(filtered_data, aes(x = Grupo2, y = mean)) +
      geom_point(size = 3, aes(colour = Pregunta)) +
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
        aes(label = paste0(round(mean, 1), "%")),
        nudge_y = 5,
        size = 3.5,
        color = awesome_colors$gray,
        family = "Source Sans Pro"
      ) +
      facet_grid(cols = vars(Pregunta), rows = vars(Grupo)) +
      custom_color_scale +
      labs(
        title = paste("Polarización afectiva entre empresarios y sindicalistas en", country),
        subtitle = "Porcentaje de respuesta de las dos opciones más positivas",
        x = "Grupo de la población evaluado",
        y = "Porcentaje de respuestas positivas (%)",
        caption = paste("Datos de", sample_sizes[[country]], "encuestas del proyecto Tejiendo Puentes de Sensata UX Research")
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      viztheme +
      ylim(0, 100)
  }
  
  # Convert to interactive plotly with custom styling
  interactive_plot <- ggplotly(base_plot, tooltip = c("x", "y", "colour", "group"))
  
  interactive_plot <- interactive_plot %>% 
    layout(
      paper_bgcolor = awesome_colors$white,
      plot_bgcolor = awesome_colors$white,
      font = list(
        family = "Source Sans Pro",
        color = awesome_colors$gray
      ),
      hoverlabel = list(
        bgcolor = awesome_colors$white,
        bordercolor = awesome_colors$gray,
        font = list(
          family = "Source Sans Pro",
          size = 12,
          color = awesome_colors$gray
        )
      ),
      legend = list(
        orientation = "h",
        y = -0.2,
        bgcolor = awesome_colors$white,
        font = list(
          family = "Source Sans Pro",
          color = awesome_colors$gray
        )
      )
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