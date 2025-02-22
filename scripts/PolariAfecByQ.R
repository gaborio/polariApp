# MVP preguntas grandes de investigación polarización
# Surveyid: Port: 6HHtyywaRxX7Jptz83ggrM Empresarios: HlrCUgVnwRpMMJwHLli7F All: 4SsfOG3M1adYm3EueODWHU
# Created by: Gabriel N. Camargo-Toledo
# Created on: Jun/14/2022
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Jun/23/2022
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Pop!_OS 21.10 8gb Ram R4.1.2


# INIT --------------------------------------------------------------------
library(sensataDataAnalysis)
library(openxlsx)

font_add_google(name = "Open Sans", family = "Open Sans")
font_add_google(name = "News Cycle", family = "News Cycle")
font_add_google(name = "Montserrat", family = "montserrat")
showtext_auto()

# Paleta de colores
poPaleta <- c("#4B1D3F",
              "#A54050",
              "#FF6361",
              "#FFA600",
              "#7FB069",
              "#479672",
              "#0E7C7B")

poColors <- scale_colour_manual(values = poPaleta)
poFills <- scale_fill_manual(values = poPaleta)

poOrdPale <- c("#FFD700",
               "#EEB51B",
               "#DC9337",
               "#CB7252",
               "#B9506E",
               "#A82E89")

poOrdCol <- scale_colour_manual(values = poOrdPale)
poOrdFil <- scale_fill_manual(values = poOrdPale)

poOrdSPale <- c("#FFD700",
                "#EEB51B",
                "#DC9337",
                "#CB7252",
                "#B9506E",
                "#A82E89",
                "#C0C0C0")

poOrdSCol <- scale_colour_manual(values = poOrdSPale)
poOrdSFil <- scale_fill_manual(values = poOrdSPale)

theme_sensata <-   theme(text = element_text(family = "Open Sans", size = 22),
                         axis.title = element_text(colour = "dimgray", size = 20),
                         axis.text.x = element_text(angle = 0,
                                                    vjust = 0.5,
                                                    hjust = 0.5,
                                                    size = 18,
                                                    colour = "dimgray"),
                         axis.text.y = element_text(size = 18,
                                                    colour = "dimgray"),
                         legend.text = element_text(size = 16,
                                                    colour = "dimgray"),
                         legend.title = element_text(colour = "dimgray"),
                         plot.title = element_text(family = "Montserrat", 
                                                   colour = "black",
                                                   size = 26, 
                                                   lineheight=.5),
                         plot.caption = element_text(colour = "dimgray"),
                         plot.subtitle = element_text(family = "Montserrat",
                                                      size = 18),
                         panel.background = element_rect(size = 0, fill = "white"),
                         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                         colour = "gray85"),
                         panel.grid.major.x = element_blank(),
                         legend.key=element_rect(fill = "white", colour = "white"))
# Data --------------------------------------------------------------------
polData <- readRDS("data/final/polari_analisis.rds")

# create data by countries ------------------------------------------------
argData <- polData %>% filter(Pais == "Argentina")
braData <- polData %>% filter(Pais == "Brasil")
colData <- polData %>% filter(Pais == "Colombia")
mexData <- polData %>% filter(Pais == "México")
polData <- polData %>% filter(Pais != "Otro")

# ####### ---- Afecto empresarios ------------------------------------------
# q_POL_EMO_10 empresarios ------------------------------------------------
var2Graph <- "q_POL_EMO_10"
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
        labs(title = var_label(polData[[var2Graph]]),
             # subtitle = "Total",
             caption = "Sensata UX",
             fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Empresarios_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

# GrupoCorpo
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Empresarios_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")


# q_POL_PAF_10 empresarios ------------------------------------------------
var2Graph <- "q_POL_PAF_10"
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))


p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Empresarios_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

# GrupoCorpo
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Empresarios_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")


# q_POL_PAF_13 empresarios ------------------------------------------------
var2Graph <- "q_POL_PAF_13"
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Empresarios_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

# GrupoCorpo
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Empresarios_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")

# q_POL_PAF_01/02 empresarios exp confianza ------------------------------------------------
rm(var2Graph)
vars2Graph <- c("q_POL_PAF_01","q_POL_PAF_02")
graphData1Arg <- createGraphData(df = argData,
                                originVar = vars2Graph[1],
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphData1Arg$pais <- "Argentina"

graphData1Bra <- createGraphData(df = braData,
                                originVar = vars2Graph[1],
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphData1Bra$pais <- "Brasil"

graphData1Col <- createGraphData(df = colData,
                                originVar = vars2Graph[1],
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphData1Col$pais <- "Colombia"

graphData1Mex <- createGraphData(df = mexData,
                                originVar = vars2Graph[1],
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphData1Mex$pais <- "México"

graphData1 <- graphData1Arg %>% 
  bind_rows(graphData1Bra) %>%
  bind_rows(graphData1Col) %>%
  bind_rows(graphData1Mex)

graphData1$Pregunta <- "Paguen salarios justos"

graphData2Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Arg$pais <- "Argentina"

graphData2Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Bra$pais <- "Brasil"

graphData2Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Col$pais <- "Colombia"

graphData2Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Mex$pais <- "México"

graphData2 <- graphData2Arg %>% 
  bind_rows(graphData2Bra) %>%
  bind_rows(graphData2Col) %>%
  bind_rows(graphData2Mex)

graphData2$Pregunta <- "Cumplan normas de transito"

graphData <- graphData1 %>% 
  bind_rows(graphData2)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_grid(Pregunta ~ pais) + 
  labs(title = "Confianza que los empresarios:",
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/PAF_01_02__Empresarios_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

# GrupoCorpo
graphData1Arg <- createGraphData(df = argData,
                                originVar = vars2Graph[1],
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphData1Arg$pais <- "Argentina"

graphData1Bra <- createGraphData(df = braData,
                                originVar = vars2Graph[1],
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphData1Bra$pais <- "Brasil"

graphData1Col <- createGraphData(df = colData,
                                originVar = vars2Graph[1],
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphData1Col$pais <- "Colombia"

graphData1Mex <- createGraphData(df = mexData,
                                originVar = vars2Graph[1],
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphData1Mex$pais <- "México"

graphData1 <- graphData1Arg %>% 
  bind_rows(graphData1Bra) %>%
  bind_rows(graphData1Col) %>%
  bind_rows(graphData1Mex)

graphData1$Pregunta <- "Paguen salarios justos"

graphData2Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[2],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData2Arg$pais <- "Argentina"

graphData2Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[2],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData2Bra$pais <- "Brasil"

graphData2Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[2],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData2Col$pais <- "Colombia"

graphData2Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[2],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData2Mex$pais <- "México"

graphData2 <- graphData2Arg %>% 
  bind_rows(graphData2Bra) %>%
  bind_rows(graphData2Col) %>%
  bind_rows(graphData2Mex)

graphData2$Pregunta <- "Cumplan normas de transito"

graphData <- graphData1 %>% 
  bind_rows(graphData2)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_grid(pais~Pregunta) + 
  labs(title = "Confianza que los empresarios:",
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/PAF_01_02_Empresarios_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")

# q_POL_PAF_01/02 empresarios exp confianza desacuerdo ------------------------------------------------
rm(vars2Graph)

polData$q_POL_PAF_01_r <- polData$q_POL_PAF_01 %>% recode(
  .default = 0,
  "2" = 100,
  "1. En desacuerdo" = 100
)

graphData1 <- polData %>% summarySE("q_POL_PAF_01_r", 
                                   weightsVar = "ponde", 
                                   groupVars = c("Pais", "Grupo2"),
                                   na.rm = T)

graphData1$Pregunta <- "Paguen salarios justos"

polData$q_POL_PAF_02_r <- polData$q_POL_PAF_02 %>% recode(
  .default = 0,
  "2" = 100,
  "1. En desacuerdo" = 100
)

graphData2 <- polData %>% summarySE("q_POL_PAF_02_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "Grupo2"),
                                    na.rm = T)

graphData2$Pregunta <- "Cumplan las normas de tránsito"

graphData <- graphData1 %>% 
  bind_rows(graphData2)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))


p <- ggplot(graphData, aes(x=Grupo2, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais), 
                  size = 3)

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Desconfianza que los empresarios",
       subtitle = "% desacuerdo (1 o 2)",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1)) + ylim(20,80) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/PAF_01_02_100_Empresarios_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

#GrupoCorpo
graphData1 <- polData %>% summarySE("q_POL_PAF_01_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "subGrupoCorpo"),
                                    na.rm = T) %>%
                            filter(!is.na(subGrupoCorpo))

graphData1$Pregunta <- "Paguen salarios justos"

polData$q_POL_PAF_02_r <- polData$q_POL_PAF_02 %>% recode(
  .default = 0,
  "2" = 100,
  "1. En desacuerdo" = 100
)

graphData2 <- polData %>% summarySE("q_POL_PAF_02_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "subGrupoCorpo"),
                                    na.rm = T) %>%
                          filter(!is.na(subGrupoCorpo))

graphData2$Pregunta <- "Cumplan las normas de tránsito"

graphData <- graphData1 %>% 
  bind_rows(graphData2)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais), 
                size = 3)

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Desconfianza que los empresarios",
       subtitle = "% desacuerdo (1 o 2)",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1)) + ylim(10,70) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/PAF_01_02_100_Empresarios_SubGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")


# Todas las de afecto empresarios -----------------------------------------
# Creando variables para porcentaje aprobación
polData$q_POL_EMO_10_r <- polData$q_POL_EMO_10  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Positiva" = 100
)

polData$q_POL_PAF_10_r <- polData$q_POL_PAF_10  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Honestos" = 100
)

polData$q_POL_PAF_13_r <- polData$q_POL_PAF_13  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Solidarios" = 100
)

polData$q_POL_PAF_01_r <- polData$q_POL_PAF_01 %>% recode(
  .default = 0,
  "4" = 100,
  "5. De acuerdo" = 100
)

polData$q_POL_PAF_02_r <- polData$q_POL_PAF_02 %>% recode(
  .default = 0,
  "4" = 100,
  "5. De acuerdo" = 100
)

polData$q_POL_PAF_07_r <- polData$q_POL_PAF_07 %>% recode(
  .default = 0,
  "Smiling" = 100,
  "Very happy" = 100
)

#Grupo2
graphDataOpi <- polData %>% summarySE("q_POL_EMO_10_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
                            filter(!is.na(Grupo2))
graphDataOpi$Pregunta <- "Opinión positiva"

graphDataHon <- polData %>% summarySE("q_POL_PAF_10_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
                            filter(!is.na(Grupo2))
graphDataHon$Pregunta <- "Los considera honestos"

graphDataSol <- polData %>% summarySE("q_POL_PAF_13_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T)%>% 
                            filter(!is.na(Grupo2))
graphDataSol$Pregunta <- "Los considera solidarios"

graphDataSal <- polData %>% summarySE("q_POL_PAF_01_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T)%>% 
                            filter(!is.na(Grupo2))
graphDataSal$Pregunta <- "Confia que pagan salarios justos"

graphDataTra <- polData %>% summarySE("q_POL_PAF_02_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
                            filter(!is.na(Grupo2))
graphDataTra$Pregunta <- "Confia que cumplen normas de tránsito"

graphDataAmi <- polData %>% summarySE("q_POL_PAF_07_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
graphDataAmi$Pregunta <- "Estaría feliz de que sus hijos sean amigos"

graphData <- graphDataOpi %>% 
  bind_rows(graphDataHon) %>%
  bind_rows(graphDataSol) %>%
  bind_rows(graphDataSal) %>%
  bind_rows(graphDataTra) %>%
  bind_rows(graphDataAmi)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

p <- ggplot(graphData, aes(x=Grupo2, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais))

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Afecto a empresarios",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text(size = 12),
                        text = element_text(lineheight = 0.3))  + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/Afecto_Empresarios_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

#subGrupoCorpo
graphDataOpi <- polData %>% summarySE("q_POL_EMO_10_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataOpi$Pregunta <- "Opinión positiva"

graphDataHon <- polData %>% summarySE("q_POL_PAF_10_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataHon$Pregunta <- "Los considera honestos"

graphDataSol <- polData %>% summarySE("q_POL_PAF_13_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T)%>% 
  filter(!is.na(subGrupoCorpo))
graphDataSol$Pregunta <- "Los considera solidarios"

graphDataSal <- polData %>% summarySE("q_POL_PAF_01_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T)%>% 
  filter(!is.na(subGrupoCorpo))
graphDataSal$Pregunta <- "Confia que pagan salarios justos"

graphDataTra <- polData %>% summarySE("q_POL_PAF_02_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataTra$Pregunta <- "Confia que cumplen normas de tránsito"

graphDataAmi <- polData %>% summarySE("q_POL_PAF_07_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataAmi$Pregunta <- "Estaría feliz de que sus hijos sean amigos"

graphData <- graphDataOpi %>% 
  bind_rows(graphDataHon) %>%
  bind_rows(graphDataSol) %>%
  bind_rows(graphDataSal) %>%
  bind_rows(graphDataTra) %>%
  bind_rows(graphDataAmi)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais))

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Afecto a empresarios",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text(size = 12),
                        text = element_text(lineheight = 0.3))  + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/Afecto_Empresarios_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")

<<<<<<< HEAD
# ####### ---- Afecto sindicalistas ---------------------------------------
# q_POL_EMO_11 sindicalistas ------------------------------------------------
var2Graph <- "q_POL_EMO_11"
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Sindicalistas_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

# GrupoCorpo
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Sindicalistas_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")



# q_POL_PAF_11 sindicalistas ------------------------------------------------
var2Graph <- "q_POL_PAF_11"
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))


p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Sindicalistas_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

# GrupoCorpo
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Sindicalistas_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")



# q_POL_PAF_14 sindicalistas ------------------------------------------------
var2Graph <- "q_POL_PAF_14"
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "Grupo2",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))


p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Sindicalistas_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

# GrupoCorpo
graphDataArg <- createGraphData(df = argData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataArg$pais <- "Argentina"

graphDataBra <- createGraphData(df = braData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataBra$pais <- "Brasil"

graphDataCol <- createGraphData(df = colData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataCol$pais <- "Colombia"

graphDataMex <- createGraphData(df = mexData,
                                originVar = var2Graph,
                                groupVar = "subGrupoCorpo",
                                weightVar = "ponde")
graphDataMex$pais <- "México"

graphData <- graphDataArg %>% 
  bind_rows(graphDataBra) %>%
  bind_rows(graphDataCol) %>%
  bind_rows(graphDataMex)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_wrap("pais") + 
  labs(title = var_label(polData[[var2Graph]]),
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/", var2Graph, "_Sindicalistas_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")


# q_POL_PAF_03/04 sindicalistas exp confianza ------------------------------------------------
rm(var2Graph)
vars2Graph <- c("q_POL_PAF_03","q_POL_PAF_04")
graphData1Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[1],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData1Arg$pais <- "Argentina"

graphData1Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[1],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData1Bra$pais <- "Brasil"

graphData1Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[1],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData1Col$pais <- "Colombia"

graphData1Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[1],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData1Mex$pais <- "México"

graphData1 <- graphData1Arg %>% 
  bind_rows(graphData1Bra) %>%
  bind_rows(graphData1Col) %>%
  bind_rows(graphData1Mex)

graphData1$Pregunta <- "Cumplan normas de tránsito"

graphData2Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Arg$pais <- "Argentina"

graphData2Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Bra$pais <- "Brasil"

graphData2Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Col$pais <- "Colombia"

graphData2Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Mex$pais <- "México"

graphData2 <- graphData2Arg %>% 
  bind_rows(graphData2Bra) %>%
  bind_rows(graphData2Col) %>%
  bind_rows(graphData2Mex)

graphData2$Pregunta <- "Protesten pacíficamente"

graphData <- graphData1 %>% 
  bind_rows(graphData2)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_grid(Pregunta ~ pais) + 
  labs(title = "Confianza que los sindicalistas:",
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/PAF_03_04_Sindicatos_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

# GrupoCorpo
graphData1Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[1],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData1Arg$pais <- "Argentina"

graphData1Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[1],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData1Bra$pais <- "Brasil"

graphData1Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[1],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData1Col$pais <- "Colombia"

graphData1Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[1],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData1Mex$pais <- "México"

graphData1 <- graphData1Arg %>% 
  bind_rows(graphData1Bra) %>%
  bind_rows(graphData1Col) %>%
  bind_rows(graphData1Mex)

graphData1$Pregunta <- "Cumplan normas de tránsito"

graphData2Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[2],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData2Arg$pais <- "Argentina"

graphData2Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[2],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData2Bra$pais <- "Brasil"

graphData2Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[2],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData2Col$pais <- "Colombia"

graphData2Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[2],
                                 groupVar = "subGrupoCorpo",
                                 weightVar = "ponde")
graphData2Mex$pais <- "México"

graphData2 <- graphData2Arg %>% 
  bind_rows(graphData2Bra) %>%
  bind_rows(graphData2Col) %>%
  bind_rows(graphData2Mex)

graphData2$Pregunta <- "Protesten pacíficamente"

graphData <- graphData1 %>% 
  bind_rows(graphData2)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_grid(pais~Pregunta) + 
  labs(title = "Confianza que los sindicalistas:",
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/PAF_03_04_Sindicatos_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")


# q_POL_PAF_03/04 sindicalistas exp confianza desacuerdo ------------------------------------------------
rm(vars2Graph)

polData$q_POL_PAF_03_r <- polData$q_POL_PAF_03 %>% recode(
  .default = 0,
  "2" = 100,
  "1. En desacuerdo" = 100
)

graphData1 <- polData %>% summarySE("q_POL_PAF_03_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "Grupo2"),
                                    na.rm = T)

graphData1$Pregunta <- "Cumplan normas de tránsito"

polData$q_POL_PAF_04_r <- polData$q_POL_PAF_04 %>% recode(
  .default = 0,
  "2" = 100,
  "1. En desacuerdo" = 100
)

graphData2 <- polData %>% summarySE("q_POL_PAF_04_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "Grupo2"),
                                    na.rm = T)

graphData2$Pregunta <- "Protesten pacíficamente"

graphData <- graphData1 %>% 
  bind_rows(graphData2)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))


p <- ggplot(graphData, aes(x=Grupo2, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais), 
                size = 3)

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Desconfianza que los sindicalistas",
       subtitle = "% desacuerdo (1 o 2)",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1)) + 
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/PAF_03_04_100_Sindicalistas_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

#GrupoCorpo
graphData1 <- polData %>% summarySE("q_POL_PAF_03_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "subGrupoCorpo"),
                                    na.rm = T) %>%
  filter(!is.na(subGrupoCorpo))

graphData1$Pregunta <- "Cumplan normas de tránsito"

polData$q_POL_PAF_02_r <- polData$q_POL_PAF_02 %>% recode(
  .default = 0,
  "2" = 100,
  "1. En desacuerdo" = 100
)

graphData2 <- polData %>% summarySE("q_POL_PAF_04_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "subGrupoCorpo"),
                                    na.rm = T) %>%
  filter(!is.na(subGrupoCorpo))

graphData2$Pregunta <- "Protesten pacíficamente"

graphData <- graphData1 %>% 
  bind_rows(graphData2)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais), 
                size = 3)

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Desconfianza que los empresarios",
       subtitle = "% desacuerdo (1 o 2)",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1)) + 
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/PAF_03_04_100_Sindicalistas_SubGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")

# Todas las de afecto sindicalistas -----------------------------------------
=======

# Todas las de afecto sindicalistas -----------------------------------------

>>>>>>> a32109b9e465e96c453fb5749e6f4d6016682b4c
# Creando variables para porcentaje aprobación
polData$q_POL_EMO_11_r <- polData$q_POL_EMO_11  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Positiva" = 100
)

polData$q_POL_PAF_11_r <- polData$q_POL_PAF_11  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Honestos" = 100
)

polData$q_POL_PAF_14_r <- polData$q_POL_PAF_14  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Solidarios" = 100
)

polData$q_POL_PAF_03_r <- polData$q_POL_PAF_03 %>% recode(
  .default = 0,
  "4" = 100,
  "5. De acuerdo" = 100
)

polData$q_POL_PAF_04_r <- polData$q_POL_PAF_04 %>% recode(
  .default = 0,
  "4" = 100,
  "5. De acuerdo" = 100
)

polData$q_POL_PAF_08_r <- polData$q_POL_PAF_08 %>% recode(
  .default = 0,
  "Smiling" = 100,
  "Very happy" = 100
)

#Grupo2
graphDataOpi <- polData %>% summarySE("q_POL_EMO_11_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
graphDataOpi$Pregunta <- "Opinión positiva"

graphDataHon <- polData %>% summarySE("q_POL_PAF_11_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
graphDataHon$Pregunta <- "Los considera honestos"

graphDataSol <- polData %>% summarySE("q_POL_PAF_14_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T)%>% 
  filter(!is.na(Grupo2))
graphDataSol$Pregunta <- "Los considera solidarios"

graphDataSal <- polData %>% summarySE("q_POL_PAF_03_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T)%>% 
  filter(!is.na(Grupo2))
<<<<<<< HEAD
graphDataSal$Pregunta <- "Confia que cumplen normas de tránsito"
=======
graphDataSal$Pregunta <- "Confia que cumplen las normas de tránsito"
>>>>>>> a32109b9e465e96c453fb5749e6f4d6016682b4c

graphDataTra <- polData %>% summarySE("q_POL_PAF_04_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
<<<<<<< HEAD
graphDataTra$Pregunta <- "Confia que protesten pacíficamente"

graphDataAmi <- polData %>% summarySE("q_POL_PAF_08_r", 
=======
graphDataTra$Pregunta <- "Confia que prostestan pacificamente"

graphDataAmi <- polData %>% summarySE("q_POL_PAF_02_r", 
>>>>>>> a32109b9e465e96c453fb5749e6f4d6016682b4c
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
graphDataAmi$Pregunta <- "Estaría feliz de que sus hijos sean amigos"

graphData <- graphDataOpi %>% 
  bind_rows(graphDataHon) %>%
  bind_rows(graphDataSol) %>%
  bind_rows(graphDataSal) %>%
  bind_rows(graphDataTra) %>%
  bind_rows(graphDataAmi)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

<<<<<<< HEAD
p <- ggplot(graphData, aes(x=Grupo2, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais))

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Afecto a sindicalistas",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text(size = 12),
                        text = element_text(lineheight = 0.3))  + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/Afecto_Sindicalistas_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

#subGrupoCorpo
graphDataOpi <- polData %>% summarySE("q_POL_EMO_11_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataOpi$Pregunta <- "Opinión positiva"

graphDataHon <- polData %>% summarySE("q_POL_PAF_11_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataHon$Pregunta <- "Los considera honestos"

graphDataSol <- polData %>% summarySE("q_POL_PAF_14_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T)%>% 
  filter(!is.na(subGrupoCorpo))
graphDataSol$Pregunta <- "Los considera solidarios"

graphDataSal <- polData %>% summarySE("q_POL_PAF_03_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T)%>% 
  filter(!is.na(subGrupoCorpo))
graphDataSal$Pregunta <- "Confia que pagan salarios justos"

graphDataTra <- polData %>% summarySE("q_POL_PAF_04_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataTra$Pregunta <- "Confia que cumplen normas de tránsito"

graphDataAmi <- polData %>% summarySE("q_POL_PAF_08_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataAmi$Pregunta <- "Estaría feliz de que sus hijos sean amigos"

graphData <- graphDataOpi %>% 
  bind_rows(graphDataHon) %>%
  bind_rows(graphDataSol) %>%
  bind_rows(graphDataSal) %>%
  bind_rows(graphDataTra) %>%
  bind_rows(graphDataAmi)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais))

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Afecto a sindicalistas",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text(size = 12),
                        text = element_text(lineheight = 0.3))  + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/Afecto_Sindicalistas_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")

=======
graphDataCol <- graphData %>% filter(Pais=="Colombia")
>>>>>>> a32109b9e465e96c453fb5749e6f4d6016682b4c

# Afecto estudiantes --------------------------------------------------------
# Todas las de afecto estudiantes -----------------------------------------
# Creando variables para porcentaje aprobación
polData$q_POL_EMO_09_r <- polData$q_POL_EMO_09  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Positiva" = 100
)

polData$q_POL_PAF_12_r <- polData$q_POL_PAF_12  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Honestos" = 100
)

polData$q_POL_PAF_15_r <- polData$q_POL_PAF_15  %>% recode(
  .default = 0,
  "5" = 100,
  "6. Solidarios" = 100
)

polData$q_POL_PAF_05_r <- polData$q_POL_PAF_05 %>% recode(
  .default = 0,
  "4" = 100,
  "5. De acuerdo" = 100
)

polData$q_POL_PAF_06_r <- polData$q_POL_PAF_06 %>% recode(
  .default = 0,
  "4" = 100,
  "5. De acuerdo" = 100
)

polData$q_POL_PAF_09_r <- polData$q_POL_PAF_09 %>% recode(
  .default = 0,
  "Smiling" = 100,
  "Very happy" = 100
)

#Grupo2
graphDataOpi <- polData %>% summarySE("q_POL_EMO_09_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
graphDataOpi$Pregunta <- "Opinión positiva"

graphDataHon <- polData %>% summarySE("q_POL_PAF_12_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
graphDataHon$Pregunta <- "Los considera honestos"

graphDataSol <- polData %>% summarySE("q_POL_PAF_15_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T)%>% 
  filter(!is.na(Grupo2))
graphDataSol$Pregunta <- "Los considera solidarios"

graphDataSal <- polData %>% summarySE("q_POL_PAF_05_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T)%>% 
  filter(!is.na(Grupo2))
graphDataSal$Pregunta <- "Confia que cumplen normas de tránsito"

graphDataTra <- polData %>% summarySE("q_POL_PAF_06_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
graphDataTra$Pregunta <- "Confia que protesten pacíficamente"

graphDataAmi <- polData %>% summarySE("q_POL_PAF_09_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "Grupo2"),
                                      na.rm = T) %>% 
  filter(!is.na(Grupo2))
graphDataAmi$Pregunta <- "Estaría feliz de que sus hijos sean amigos"

graphData <- graphDataOpi %>% 
  bind_rows(graphDataHon) %>%
  bind_rows(graphDataSol) %>%
  bind_rows(graphDataSal) %>%
  bind_rows(graphDataTra) %>%
  bind_rows(graphDataAmi)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

p <- ggplot(graphData, aes(x=Grupo2, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais))

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Afecto a estudiantes",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text(size = 12),
                        text = element_text(lineheight = 0.3))  + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/Afecto_Estudiantes_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")

#subGrupoCorpo
graphDataOpi <- polData %>% summarySE("q_POL_EMO_09_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataOpi$Pregunta <- "Opinión positiva"

graphDataHon <- polData %>% summarySE("q_POL_PAF_12_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataHon$Pregunta <- "Los considera honestos"

graphDataSol <- polData %>% summarySE("q_POL_PAF_15_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T)%>% 
  filter(!is.na(subGrupoCorpo))
graphDataSol$Pregunta <- "Los considera solidarios"

graphDataSal <- polData %>% summarySE("q_POL_PAF_05_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T)%>% 
  filter(!is.na(subGrupoCorpo))
graphDataSal$Pregunta <- "Confia que pagan salarios justos"

graphDataTra <- polData %>% summarySE("q_POL_PAF_06_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataTra$Pregunta <- "Confia que cumplen normas de tránsito"

graphDataAmi <- polData %>% summarySE("q_POL_PAF_09_r", 
                                      weightsVar = "ponde", 
                                      groupVars = c("Pais", "subGrupoCorpo"),
                                      na.rm = T) %>% 
  filter(!is.na(subGrupoCorpo))
graphDataAmi$Pregunta <- "Estaría feliz de que sus hijos sean amigos"

graphData <- graphDataOpi %>% 
  bind_rows(graphDataHon) %>%
  bind_rows(graphDataSol) %>%
  bind_rows(graphDataSal) %>%
  bind_rows(graphDataTra) %>%
  bind_rows(graphDataAmi)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Rango alto",
                                                                         "Rango medio",
                                                                         "Rango bajo",
                                                                         "Otros ciudadanos",
                                                                         "Total"))

p <- ggplot(graphData, aes(x=subGrupoCorpo, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais))

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Afecto a estudiantes",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text(size = 12),
                        text = element_text(lineheight = 0.3))  + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/Afecto_Estudiantes_subGrupoCorpo.png"), device = "png", height = 10, width = 18, units = "cm")



# #####Distancia social --------------------------------------------------------
# Todas las opciones -------------
rm(var2Graph)
rm(vars2Graph)

vars2Graph <- c("q_POL_PAF_07","q_POL_PAF_08","q_POL_PAF_09")
graphData1Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[1],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData1Arg$pais <- "Argentina"

graphData1Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[1],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData1Bra$pais <- "Brasil"

graphData1Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[1],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData1Col$pais <- "Colombia"

graphData1Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[1],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData1Mex$pais <- "México"

graphData1 <- graphData1Arg %>% 
  bind_rows(graphData1Bra) %>%
  bind_rows(graphData1Col) %>%
  bind_rows(graphData1Mex)

graphData1$Pregunta <- "Amigo empresario"

graphData2Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Arg$pais <- "Argentina"

graphData2Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Bra$pais <- "Brasil"

graphData2Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Col$pais <- "Colombia"

graphData2Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[2],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData2Mex$pais <- "México"

graphData2 <- graphData2Arg %>% 
  bind_rows(graphData2Bra) %>%
  bind_rows(graphData2Col) %>%
  bind_rows(graphData2Mex)

graphData2$Pregunta <- "Amigo sindicalista"

graphData3Arg <- createGraphData(df = argData,
                                 originVar = vars2Graph[3],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData3Arg$pais <- "Argentina"

graphData3Bra <- createGraphData(df = braData,
                                 originVar = vars2Graph[3],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData3Bra$pais <- "Brasil"

graphData3Col <- createGraphData(df = colData,
                                 originVar = vars2Graph[3],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData3Col$pais <- "Colombia"

graphData3Mex <- createGraphData(df = mexData,
                                 originVar = vars2Graph[3],
                                 groupVar = "Grupo2",
                                 weightVar = "ponde")
graphData3Mex$pais <- "México"

graphData3 <- graphData3Arg %>% 
  bind_rows(graphData3Bra) %>%
  bind_rows(graphData3Col) %>%
  bind_rows(graphData3Mex)

graphData3$Pregunta <- "Amigo estudiante"

graphData <- graphData1 %>% 
  bind_rows(graphData2) %>%
  bind_rows(graphData3)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))

p <- ggplot(graphData, aes(x=Grupo2, y=Porcentaje))
g <- geom_col(aes(fill=Value),  width = 0.4)

p + g + facet_grid(Pregunta ~ pais) + 
  labs(title = "Cercanía social",
       # subtitle = "Total",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1, size = 12)) +
  scale_fill_manual(values = c(poPaleta))
ggsave(filename = paste0("Figures/polarizacionAfectiva/Cercania_Social_Grupo2.png"), device = "png", height = 10, width = 18, units = "cm")


# Solo sad/crying -----------------
rm(vars2Graph)

polData$q_POL_PAF_07_r <- polData$q_POL_PAF_07 %>% recode(
  .default = 0,
  "Crying" = 100,
  "Sad" = 100
)

graphData1 <- polData %>% summarySE("q_POL_PAF_07_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "Grupo2"),
                                    na.rm = T)

graphData1$Pregunta <- "Amigo empresario"

polData$q_POL_PAF_08_r <- polData$q_POL_PAF_08 %>% recode(
  .default = 0,
  "Crying" = 100,
  "Sad" = 100
)

graphData2 <- polData %>% summarySE("q_POL_PAF_08_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "Grupo2"),
                                    na.rm = T)

graphData2$Pregunta <- "Amigo sindicalista"

polData$q_POL_PAF_09_r <- polData$q_POL_PAF_09 %>% recode(
  .default = 0,
  "Crying" = 100,
  "Sad" = 100
)

graphData3 <- polData %>% summarySE("q_POL_PAF_09_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "Grupo2"),
                                    na.rm = T)

graphData3$Pregunta <- "Amigo estudiante"

graphData <- graphData1 %>% 
  bind_rows(graphData2) %>%
  bind_rows(graphData3)

graphData$Grupo2 <- graphData$Grupo2 %>% factor(levels = c("Sector privado",
                                                           "Empresarios informales",
                                                           "Estudiantes",
                                                           "Líderes sociales",
                                                           "Otros ciudadanos",
                                                           "Total"))


p <- ggplot(graphData, aes(x=Grupo2, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais), 
                size = 3)

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Cercanía social",
       subtitle = "% llorando o triste",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1)) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/Cercania_Social_Grupo2_Tristes.png"), device = "png", height = 10, width = 18, units = "cm")

#Rangos

polData$q_POL_PAF_07_r <- polData$q_POL_PAF_07 %>% recode(
  .default = 0,
  "Crying" = 100,
  "Sad" = 100
)

graphData1 <- polData %>% summarySE("q_POL_PAF_07_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "subGrupoCorpo"),
                                    na.rm = T) %>%
  filter(!is.na(subGrupoCorpo))

graphData1$Pregunta <- "Amigo empresario"

polData$q_POL_PAF_08_r <- polData$q_POL_PAF_08 %>% recode(
  .default = 0,
  "Crying" = 100,
  "Sad" = 100
)

graphData2 <- polData %>% summarySE("q_POL_PAF_08_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "subGrupoCorpo"),
                                    na.rm = T)  %>%
  filter(!is.na(subGrupoCorpo))

graphData2$Pregunta <- "Amigo sindicalista"

polData$q_POL_PAF_09_r <- polData$q_POL_PAF_09 %>% recode(
  .default = 0,
  "Crying" = 100,
  "Sad" = 100
)

graphData3 <- polData %>% summarySE("q_POL_PAF_09_r", 
                                    weightsVar = "ponde", 
                                    groupVars = c("Pais", "subGrupoCorpo"),
                                    na.rm = T)  %>%
  filter(!is.na(subGrupoCorpo))

graphData3$Pregunta <- "Amigo estudiante"

graphData <- graphData1 %>% 
  bind_rows(graphData2) %>%
  bind_rows(graphData3)

graphData$subGrupoCorpo <- graphData$subGrupoCorpo %>% factor(levels = c("Otros ciudadanos",
                                                                         "Rango bajo",
                                                                         "Rango medio",
                                                                         "Rango alto",
                                                                         "Total"))


p <- ggplot(graphData, aes(x=subGrupoCorpo, y=mean))
g <- geom_point(aes(colour=Pais, 
                    shape=Pais), 
                size = 3)

p + g + facet_wrap("Pregunta") +
  geom_line(aes(colour = Pais, group = Pais)) +
  labs(title = "Cercanía social",
       subtitle = "% llorando o triste",
       caption = "Sensata UX",
       fill = "Respuesta") +
  xlab("") + ylab("%") +
  theme_sensata + theme(axis.text.x = element_text( angle = 20, hjust = 1, vjust = 1)) +
  poColors

ggsave(filename = paste0("Figures/polarizacionAfectiva/Cercania_Social_SubGrupoCorpo_Tristes.png"), device = "png", height = 10, width = 18, units = "cm")
