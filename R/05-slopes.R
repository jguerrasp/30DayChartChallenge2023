# ---------------------------------------------------------------------------- #
#             Script #30DayChartChallenge 2023 - Día 5: slopes
#
#  Evolución de la percepción del aborto en Chile utilizando datos históricos 
#  de la Encuesta Bicentenario UC.
#
#  Datos se pueden encontrar en: https://encuestabicentenario.uc.cl/
#
# ---------------------------------------------------------------------------- #



# Librerías ---------------------------------------------------------------

library(tidyverse)
library(haven)
library(openxlsx)
library(CGPfunctions)
library(extrafont)


# Cargar datos ------------------------------------------------------------

df <- read.xlsx('./5-slopes/input/Datos_aborto_bicentenario.xlsx') %>% 
  tibble()



# Procesamiento de datos --------------------------------------------------

df <- df %>%
  mutate(prc = round(prc, 0),
         anio = as.character(anio))


# Función modificada para grafico slope -----------------------------------

## Función original pertenece a la librería "CGPfunctions"
## Documentación de la función original: 
##      https://cran.r-project.org/web/packages/CGPfunctions/CGPfunctions.pdf

newggslopegraph_mod <- function(dataframe,
                                Times,
                                Measurement,
                                Grouping,
                                Data.label = NULL,
                                Title = "No title given",
                                SubTitle = "No subtitle given",
                                Caption = "No caption given",
                                XTextSize = 12,
                                YTextSize = 3,
                                TitleTextSize = 14,
                                SubTitleTextSize = 10,
                                CaptionTextSize = 8,
                                TitleJustify = "left",
                                SubTitleJustify = "left",
                                CaptionJustify = "right",
                                LineThickness = 1,
                                LineColor = "ByGroup",
                                DataTextSize = 2.5,
                                DataTextColor = "black",
                                DataLabelPadding = 0.05,
                                DataLabelLineSize = 0,
                                DataLabelFillColor = "white",
                                WiderLabels = FALSE,
                                ReverseYAxis = FALSE,
                                ReverseXAxis = FALSE,
                                RemoveMissing = TRUE,
                                ThemeChoice = "bw") {
  
  # ---------------- theme selection ----------------------------
  
  if (ThemeChoice == "bw") {
    theme_set(theme_bw())
  } else if (ThemeChoice == "ipsum") {
    theme_set(hrbrthemes::theme_ipsum_rc())
  } else if (ThemeChoice == "econ") {
    theme_set(ggthemes::theme_economist()) ## background = "#d5e4eb"
    if (DataLabelFillColor == "white") {
      DataLabelFillColor <- "#d5e4eb"
    }
  } else if (ThemeChoice == "wsj") {
    theme_set(ggthemes::theme_wsj()) ## background = "#f8f2e4"
    if (DataLabelFillColor == "white") {
      DataLabelFillColor <- "#f8f2e4"
    }
    TitleTextSize <- TitleTextSize - 1
    SubTitleTextSize <- SubTitleTextSize + 1
  } else if (ThemeChoice == "gdocs") {
    theme_set(ggthemes::theme_gdocs())
  } else if (ThemeChoice == "tufte") {
    theme_set(ggthemes::theme_tufte())
  } else {
    theme_set(theme_bw())
  }
  
  # ---------------- ggplot setup work ----------------------------
  
  # Since ggplot2 objects are just regular R objects, put them in a list
  MySpecial <- list(
    # Format tweaks
    scale_x_discrete(position = "top",
                     expand = expansion(add = c(2, 2))), # move the x axis labels up top
    theme(legend.position = "none"), # Remove the legend
    theme(panel.border = element_blank()), # Remove the panel border
    theme(axis.title.y = element_blank()), # Remove just about everything from the y axis
    theme(axis.text.y = element_blank()),
    theme(panel.grid.major.y = element_blank()),
    theme(panel.grid.minor.y = element_blank()),
    theme(axis.title.x = element_blank()), # Remove a few things from the x axis
    theme(panel.grid.major.x = element_line(colour = "grey90")),
    theme(axis.text.x.bottom = element_text(size = XTextSize, 
                                            face = "bold", 
                                            family = 'Roboto Condensed')), # and increase font size
    theme(axis.ticks = element_blank()), # Remove x & y tick marks
    theme(plot.title = element_text(
      size = TitleTextSize,
      face = "bold",
      hjust = CGPfunctions:::justifyme(TitleJustify)
    )),
    theme(plot.subtitle = element_text(
      size = SubTitleTextSize,
      hjust = CGPfunctions:::justifyme(SubTitleJustify)
    )),
    theme(plot.caption = element_text(
      size = CaptionTextSize,
      hjust = CGPfunctions:::justifyme(CaptionJustify)
    ))
  )
  
  # ---------------- input checking ----------------------------
  
  # error checking and setup
  if (length(match.call()) <= 4) {
    stop("Not enough arguments passed requires a dataframe, plus at least three variables")
  }
  argList <- as.list(match.call()[-1])
  if (!hasArg(dataframe)) {
    stop("You didn't specify a dataframe to use", call. = FALSE)
  }
  
  NTimes <- deparse(substitute(Times)) # name of Times variable
  NMeasurement <- deparse(substitute(Measurement)) # name of Measurement variable
  NGrouping <- deparse(substitute(Grouping)) # name of Grouping variable
  
  if(is.null(argList$Data.label)) {
    NData.label <- deparse(substitute(Measurement))
    Data.label <- argList$Measurement
  } else {
    NData.label <- deparse(substitute(Data.label))
    #     Data.label <- argList$Data.label
  }
  
  Ndataframe <- argList$dataframe # name of dataframe
  if (!is(dataframe, "data.frame")) {
    stop(paste0("'", Ndataframe, "' does not appear to be a data frame"))
  }
  if (!NTimes %in% names(dataframe)) {
    stop(paste0("'", NTimes, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (anyNA(dataframe[[NTimes]])) {
    stop(paste0("'", NTimes, "' can not have missing data please remove those rows!"), call. = FALSE)
  }
  if (!NMeasurement %in% names(dataframe)) {
    stop(paste0("'", NMeasurement, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (!NGrouping %in% names(dataframe)) {
    stop(paste0("'", NGrouping, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (!NData.label %in% names(dataframe)) {
    stop(paste0("'", NData.label, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (anyNA(dataframe[[NGrouping]])) {
    stop(paste0("'", NGrouping, "' can not have missing data please remove those rows!"), call. = FALSE)
  }
  if (!class(dataframe[[NMeasurement]]) %in% c("integer", "numeric")) {
    stop(paste0("Sorry I need the measured variable '", NMeasurement, "' to be a number"), call. = FALSE)
  }
  if (!"ordered" %in% class(dataframe[[NTimes]])) { # keep checking
    if (!"character" %in% class(dataframe[[NTimes]])) { # keep checking
      if ("factor" %in% class(dataframe[[NTimes]])) { # impose order
        message(paste0("\nConverting '", NTimes, "' to an ordered factor\n"))
        dataframe[[NTimes]] <- factor(dataframe[[NTimes]], ordered = TRUE)
      } else {
        stop(paste0("Sorry I need the variable '", NTimes, "' to be of class character, factor or ordered"), call. = FALSE)
      }
    }
  }
  
  Times <- enquo(Times)
  Measurement <- enquo(Measurement)
  Grouping <- enquo(Grouping)
  Data.label <- enquo(Data.label)
  
  # ---------------- handle some special options ----------------------------
  
  if (ReverseXAxis) {
    dataframe[[NTimes]] <- forcats::fct_rev(dataframe[[NTimes]])
  }
  
  NumbOfLevels <- nlevels(factor(dataframe[[NTimes]]))
  if (WiderLabels) {
    MySpecial <- c(MySpecial, expand_limits(x = c(0, NumbOfLevels + 1)))
  }
  
  if (ReverseYAxis) {
    MySpecial <- c(MySpecial, scale_y_reverse())
  }
  
  if (length(LineColor) > 1) {
    if (length(LineColor) < length(unique(dataframe[[NGrouping]]))) {
      message(paste0("\nYou gave me ", length(LineColor), " colors I'm recycling colors because you have ", length(unique(dataframe[[NGrouping]])), " ", NGrouping, "s\n"))
      LineColor <- rep(LineColor, length.out = length(unique(dataframe[[NGrouping]])))
    }
    LineGeom <- list(geom_line(aes_(color = Grouping), 
                               size = LineThickness), 
                     scale_color_manual(values = LineColor))
  } else {
    if (LineColor == "ByGroup") {
      LineGeom <- list(geom_line(aes_(color = Grouping, alpha = 1), 
                                 size = LineThickness))
    } else {
      LineGeom <- list(geom_line(aes_(), 
                                 linewidth = LineThickness, 
                                 size = LineColor))
    }
  }
  
  # logic to sort out missing values if any
  if (anyNA(dataframe[[NMeasurement]])) { # are there any missing
    if (RemoveMissing) { # which way should we handle them
      dataframe <- dataframe %>%
        group_by(!!Grouping) %>%
        filter(!anyNA(!!Measurement)) %>%
        droplevels()
    } else {
      dataframe <- dataframe %>%
        filter(!is.na(!!Measurement))
    }
  }
  
  # ---------------- main ggplot routine ----------------------------
  
  dataframe %>%
    ggplot(aes_(group = Grouping, y = Measurement, x = Times)) +
    LineGeom +
    # left side y axis labels
    ggrepel::geom_text_repel(
      data = . %>% filter(!!Times == min(!!Times)),
      aes_(label = Grouping),
      hjust = "left",
      box.padding = 0.10,
      point.padding = 0.10,
      segment.color = "gray",
      segment.alpha = 0.6,
      fontface = "plain",
      family = 'Roboto Condensed',
      size = YTextSize,
      nudge_x = -1.8,
      direction = "y",
      force = .5,
      max.iter = 3000,
      colour = "white"
    ) +
    # right side y axis labels
    ggrepel::geom_text_repel(
      data = . %>% filter(!!Times == max(!!Times)),
      aes_(label = Grouping),
      hjust = "right",
      box.padding = 0.10,
      point.padding = 0.10,
      segment.color = "gray",
      segment.alpha = 0.6,
      fontface = "plain",
      family = 'Roboto Condensed',
      size = YTextSize,
      nudge_x = 1.8,
      direction = "y",
      force = .5,
      max.iter = 3000,
      colour = "white"
    ) +
    # data point labels
    geom_label(aes_string(label = NData.label),
               size = DataTextSize,
               # label.padding controls fill padding
               label.padding = unit(DataLabelPadding, "lines"),
               # label.size controls width of line around label box
               # 0 = no box line
               label.size = DataLabelLineSize,
               # color = text color of label
               color = DataTextColor,
               # fill background color for data label
               fill = DataLabelFillColor,
               family = 'Roboto Condensed'
    ) +
    MySpecial +
    labs(
      title = Title,
      subtitle = SubTitle,
      caption = Caption
    )
  
  # implicitly return plot object
} # end of function






# Grafico slopes ----------------------------------------------------------

gg <- newggslopegraph_mod(dataframe = df,
                          Times = anio,
                          Measurement = prc,
                          Grouping = escala,
                          LineThickness = 2,
                          DataTextSize = 7,
                          Caption = 'Fuente: Encuesta Bicentenario',
                          Title = "Evolución de la percepción del aborto en Chile",
                          TitleJustify = 'L',
                          TitleTextSize = 20,
                          SubTitle = "En su opinión, ¿Ud. cree que la mujer debería tener derecho a hacerse un aborto?",
                          SubTitleJustify = "L",
                          SubTitleTextSize = rel(0.8),
                          CaptionTextSize = rel(0.5),
                          YTextSize = rel(6.2),
                          DataLabelPadding = 0.07,
                          DataLabelFillColor = '#4C4B4D',
                          DataLabelLineSize = 0,
                          DataTextColor = "white",
                          LineColor = c("#ff920f", "#ffbb64", "#ffe3b8")) +
  theme(text = element_text(family = "Roboto",
                            colour = "white",
                            size = 30),
        plot.background = element_rect(fill = "#4C4B4D"),
        panel.background = element_rect(fill = "#4C4B4D"),
        panel.grid.major.x = element_line(linewidth = 0.6,
                                          colour = "#C8C8C8",
                                          linetype = "dotted"),
        panel.spacing.y = unit(3, 'pt'),
        plot.title = element_text(colour = "white", size = rel(2)),
        plot.subtitle = element_text(face = "italic", family = 'Roboto condensed',
                                     margin = margin(0,0,20,0)),
        axis.text.x = element_text(colour = "white", face = "bold", size = 25))


# Guardar gráfico ---------------------------------------------------------

# Funcion personalizada para guardar el grafico
gg_save_desuc <- function(gg_chart, name,
                          width = 22,
                          height = 13) {
  ggsave(filename = paste0('output/', name),
         plot = gg_chart,
         width = width,
         height = height,
         units = 'cm')
}

# Guardar png
gg_save_desuc(gg, 
              width = 48,
              height = 28,
              'day5_slopes_1.png')
)



















