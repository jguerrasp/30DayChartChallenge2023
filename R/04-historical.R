# Carga de paquetes --------

library(tidyverse)
library(readxl)
library(ggsci)
library(janitor)

# Definiciones -----------

theme_desuc <- list(theme_minimal(base_family = 'Roboto Condensed'),
                    theme(text = element_text(size = 30),
                          plot.title.position = 'plot',
                          legend.position = 'bottom',
                          legend.title=element_blank(),
                          legend.text = element_text(size = rel(.8)),
                          plot.caption = element_text(color = 'grey40', size = rel(0.5)),
                          plot.title = element_text(color = "#1A5276", face = "bold", size = rel(2), family = "Roboto Condensed"),
                          axis.title = element_blank(), 
                          axis.text = element_text(size = rel(.6)),
                          plot.background = element_rect(fill = '#D4E6F1', color = NA)))

# Guardar

gg_save_desuc <- function(gg_chart, name,
                          width = 22,
                          height = 13) {
  ggsave(filename = paste0('output/', name),
         plot = gg_chart,
         width = width,
         height = height,
         units = 'cm')
}


# Lectura base --------

df <- read_csv('input/marriage-rate-per-1000-inhabitants.csv') %>% clean_names() # descargada de: https://ourworldindata.org/marriages-and-divorces

# Procesamiento base -------

paises <- c('Chile','Australia','Bulgaria','United States','Japan')

df <- df %>% 
  filter(entity %in% paises) %>% 
  filter(year > 1950)

# Gráfico ----------

gg <- df %>% 
  ggplot(aes(x = year, y = crude_marriage_rate_per_1_000_inhabitants)) +
  geom_line(aes(color = entity), alpha = .4, size = 1.5) +
  geom_point(aes(color = entity), size = 4) + 
  scale_color_d3() +
  scale_y_continuous(breaks = seq(0, 15, by = 1),
                     limits = c(0,12)) + 
  scale_x_continuous(breaks = seq(1950, 2020, by = 10),
                     limits = c(1950,2020)) + 
  theme_desuc +
  labs(title = 'Cambios históricos de tasa\nde matrimonios 💕 por cada\n1000 habitantes',
       caption = 'Fuente: OWID, Naciones Unidas, Eurostat y otros')
  
gg_save_desuc(gg,
              width = 28,
              height = 25,
              'day4_historical_1.png')

gg_save_desuc(gg, 
              width = 50,
              height = 25,
              'day4_historical_2.png')
