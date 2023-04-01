# Carga de paquetes --------

library(tidyverse)
library(readxl)
library(janitor)

# Definiciones -----------

theme_desuc <- list(theme_minimal(base_family = 'Roboto Condensed'),
                    theme(text = element_text(size = 30),
                          plot.title.position = 'plot',
                          legend.position = 'top',
                          legend.title=element_blank(),
                          legend.text = element_text(size = rel(.9)),
                          plot.caption = element_text(color = 'grey40', size = rel(0.5)),
                          plot.title = element_text(color = "grey40", face = "bold", size = rel(2.2), family = "Roboto Condensed"),
                          plot.subtitle = element_text(color = 'grey40', size = rel(1.5)),
                          axis.title = element_text(size = rel(0.7)), 
                          axis.text.y = element_blank(),
                          axis.title.x = element_blank(),
                          axis.text.x = element_text(size = rel(.8)),
                          plot.background = element_rect(fill = '#EBDEF0', color = NA)))

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

# Lectura base ----------

df <- read.csv('input/share-of-people-who-say-they-are-happy.csv') %>% 
  clean_names() # obtenida en el siguiente link: https://ourworldindata.org/happiness-and-life-satisfaction

# Procesamiento base -------

df <- df %>% 
  filter(code == 'CHL') %>% 
  select(-code,-entity) %>% 
  rename(feliz = share_of_people_who_are_happy_world_value_survey_2014) %>% 
  mutate(no_feliz = feliz-100)

df <- df %>% 
  pivot_longer(!year, names_to = "categoria", values_to = "porc") %>% 
  mutate(porc = round(porc,0),
         year = as.character(year))

# Gráfico ----------

gg <- ggplot(df, aes(fill=categoria, y=porc, x=year)) + 
  geom_bar(position="stack", stat="identity", width = .6) +
  geom_text(aes(label=abs(porc), y = porc + 3.5 * sign(porc)),
            family = 'Roboto', size = 7) +
  scale_fill_manual(values = c('#45B39D', '#5D6D7E'),
                    labels = c('Feliz', 'No feliz')) + 
  labs(y = 'Porcentaje',
       title = 'Evolución de personas\nen Chile que señalan\nser felices',
       caption = 'Fuente: World Value Survey\nFeliz=Muy+Bastante feliz, No feliz=Poco+Nada feliz') +
  theme_desuc 

gg_save_desuc(gg, 
              width = 28,
              height = 25,
              'day1_part_to_whole_1.png')

gg_save_desuc(gg, 
              width = 50,
              height = 25,
              'day1_part_to_whole_2.png')
