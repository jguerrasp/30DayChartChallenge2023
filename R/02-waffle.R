# Carga de paquetes --------

library(tidyverse)
library(readxl)
library(janitor)
library(waffle)
library(ggsci)

# Definiciones -----------

theme_desuc <- list(theme_minimal(base_family = 'Roboto Condensed'),
                    theme(text = element_text(size = 30),
                          plot.title.position = 'plot',
                          legend.position = 'bottom',
                          legend.title=element_blank(),
                          legend.text = element_text(size = rel(.6)),
                          plot.caption = element_text(color = 'grey40', size = rel(0.5)),
                          plot.title = element_text(color = "#2471A3", face = "bold", size = rel(2.2), family = "Roboto Condensed"),
                          axis.title = element_text(size = rel(0.7)), 
                          axis.text.y = element_blank(),
                          axis.title.x = element_blank(),
                          axis.text.x = element_blank(),
                          plot.background = element_rect(fill = '#FCF3CF', color = NA)))

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

df <- read_csv('input/population-by-broad-age-group.csv') %>% 
  clean_names() # obtenida en el siguiente link: https://ourworldindata.org/age-structure

# Procesamiento base --------

df <- df %>% 
  filter(code == 'CHL') %>% 
  filter(year %in% c(1950, 1980, 2021)) %>% 
  select(-entity,-code)

var_names <- c('ano','65 y más','25 a 64','15 a 24','5 a 14','4 o menos')
colnames(df) <- var_names
df <- df %>% select(1,6,5,4,3,2)

df <- df %>% 
  pivot_longer(!ano, names_to = 'edad', values_to = 'pob')

df <- df %>% 
  group_by(ano) %>% 
  mutate(porc = round(pob/sum(pob)*100,0),
         porc = ifelse(ano==2021 & edad=='5 a 14', 12, porc)) #se aproxima hacia abajo para no dar suma sobre 100

gg <- df %>% 
  ggplot(aes(fill = factor(edad,orden), values = porc)) +
  geom_waffle(n_rows = 5, color = "#FCF3CF", size=1.125) +
  facet_wrap(~ano, ncol=1) +
  scale_fill_npg() +
  theme_desuc +
  labs(title = 'Cambios en composición\nsegún edad en Chile',
       caption = 'Fuente: Naciones Unidas, División de Población')

gg_save_desuc(gg, 
              width = 28,
              height = 25,
              'day2_waffle_1.png')

gg_save_desuc(gg, 
              width = 50,
              height = 25,
              'day2_waffle_2.png')
