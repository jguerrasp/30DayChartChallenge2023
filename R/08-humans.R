# Carga de paquetes --------

library(tidyverse)
library(readxl)
library(janitor)
library(ggsci)
library(countrycode)

# Definiciones -----------

theme_desuc <- list(theme_minimal(base_family = 'Roboto Condensed'),
                    theme(text = element_text(size = 30),
                          plot.title.position = 'plot',
                          legend.position = 'bottom',
                          legend.title=element_blank(),
                          legend.text = element_text(size = rel(.6)),
                          plot.caption = element_text(color = 'grey40', size = rel(0.5)),
                          plot.title = element_text(color = "#d86f00", face = "bold", size = rel(1.9), family = "Roboto Condensed"),
                          axis.title = element_text(size = rel(0.8)), 
                          axis.text.y = element_text(size = rel(.7)),
                          axis.title.x = element_blank(),
                          axis.text.x = element_text(size = rel(.7), angle = 90),
                          plot.background = element_rect(fill = '#efdede', color = NA)))

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

# Lectura base ---------

df <- read_csv('input/UN-population-projection-medium-variant.csv') %>% #obtenida en: https://ourworldindata.org/future-population-growth
  clean_names()

# Procesamiento base -------

df <- df %>% 
  filter(year > 2024) # filtramos para años en el "futuro"

continentes <- countrycode::codelist %>% #acá obtenemos y agregamos el dato de continente por país
  select(country.name.en, continent,region) %>% 
  rename(entity = country.name.en) 

df <- left_join(df,
                continentes,
                by = 'entity') %>% 
  filter(!(is.na(continent)))

rm(continentes)

df <- df %>% 
  group_by(continent,year) %>% 
  summarise(poblacion = sum(population_sex_all_age_all_variant_medium))

df <- df %>% 
  mutate(poblacion = poblacion/1000000) # lo pasamos a millones

# Gráfico -------

library(ggridges)

gg <- ggplot(df, aes(x=year, y=poblacion, fill=continent)) +
  geom_area(alpha = .7, size = .6, colour = "black") +
  scale_x_continuous(limits = c(2025,2100),
                     breaks = seq(2025, 2100, by = 5)) +
  scale_fill_locuszoom() + 
  labs(title = 'Proyección de distribución de\npoblación humana 🧒🏻👦🏻👩🏻🧑🏻🧑🏻‍🦳👨🏻‍🦳\nsegún continente',
       subtitle = 'Desde año 2025 al 2100',
       y = 'Población en millones',
       caption = 'HYDE (2017); Gapminder (2023); Naciones Unidas (2022)') + 
  theme_desuc

gg_save_desuc(gg,
              width = 28,
              height = 25,
              'day8_humans_1.png')

gg_save_desuc(gg, 
              width = 50,
              height = 25,
              'day8_humans_2.png')  
