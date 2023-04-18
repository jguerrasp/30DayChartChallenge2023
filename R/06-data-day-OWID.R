# Carga de paquetes --------

library(tidyverse)
library(readxl)
library(janitor)
library(scales)

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

# Lectura base

df <- readxl::read_xlsx('input/migration.xlsx') %>% 
  clean_names() # obtenida en el siguiente link: https://ourworldindata.org/migration

df <- df %>% 
  mutate(country2 = country)


# Gráfico inspirado en https://www.data-to-viz.com/caveat/spaghetti.html 

## Gráfico Instagram

gg <- df %>% 
  ggplot(aes(x = year, y = net_migration, color = country, group = country)) +
  geom_line(data = df %>% dplyr::select(-country), aes(group=country2), color="#A5D7E8", size=0.5, alpha=0.5) +
  geom_line(aes(color=name), color="#ba90c6", size=1.1 ) +
  geom_point(size = 6, color="#ba90c6", shape = 18) +
  scale_y_continuous(labels = unit_format(unit = "mill", scale = 1e-6)) +
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  facet_wrap(~country) +
  labs(title = '👣 👣 👣 👣\nMigración neta',
       subtitle = stringr::str_wrap('La migracion neta es el número total de inmigrantes (personas que llegan a un país), menos el número de emigrantes (personas que se van del país), respecto de los 5 años previos. Se expresa como un número promedio neto de migrantes anuales.', width = 65),
       x = '',
       y = '',
       caption = 'Fuente: OWID') +
  theme_minimal() +
  theme(text = element_text(size = 25, family = "Gotu"),
        axis.text.x = element_text(size = 10, angle = 30),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = rel(2.0), family = "Gotu", hjust = 0.5, color = "grey30"),
        plot.title.position = 'plot',
        plot.subtitle = element_text(color = 'grey50', size = rel(0.8), hjust = 0.5, face = "italic", family = "Gotu"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = '#fdf4f5', color = NA),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")) 

gg_save_desuc(gg, 
              width = 25,
              height = 25,
              'day_6_data_day_OWID_1.png')

## Gráfico Twitter

gg <- df %>% 
  ggplot(aes(x = year, y = net_migration, color = country, group = country)) +
  geom_line(data = df %>% dplyr::select(-country), aes(group=country2), color="#A5D7E8", size=0.5, alpha=0.5) +
  geom_line(aes(color=name), color="#ba90c6", size=1.1 ) +
  geom_point(size = 6, color="#ba90c6", shape = 18) +
  scale_y_continuous(labels = unit_format(unit = "mill", scale = 1e-6)) +
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  facet_wrap(~country, nrow=2) +
  labs(title = '👣 👣 👣 👣\nMigración neta',
       subtitle = stringr::str_wrap('La migracion neta es el número total de inmigrantes (personas que llegan a un país), menos el número de emigrantes (personas que se van del país), respecto de los 5 años previos. Se expresa como un número promedio neto de migrantes anuales.'),
       x = '',
       y = '',
       caption = 'Fuente: OWID') +
  theme_minimal() +
  theme(text = element_text(size = 30, family = "Gotu"),
        axis.text.x = element_text(size = 18, angle = 30),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(size = rel(2.0), family = "Gotu", hjust = 0.5, color = "grey30"),
        plot.title.position = 'plot',
        plot.subtitle = element_text(color = 'grey50', size = rel(0.8), hjust = 0.5, face = "italic", family = "Gotu"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = '#fdf4f5', color = NA),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")) 

gg_save_desuc(gg, 
              width = 50,
              height = 30,
              'day_6_data_day_OWID_2.png')
