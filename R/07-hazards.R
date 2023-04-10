# Carga de paquetes --------

library(tidyverse)
library(readxl)

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

df <- readxl::read_xlsx('input/delitos.xlsx')  # obtenida en el siguiente link: http://cead.spd.gov.cl/wp-content/uploads/file-manager/Presentaci%C3%B3n-Estad%C3%ADsticas-anual-2022.pdf


# Gráfico 

## Gráfico Instagram

gg <- df %>% 
  ggplot(aes(x = ano, y = tasa_cada_cien_mil, color = caso_policial, group = caso_policial)) +
  geom_line(size = 3) +
  geom_point(size = 25, fill = "white") +
  scale_color_manual(values = c('#A3757C', '#C91333', '#FEFCDA', '#E48887'))+
  geom_text(aes(label=tasa_cada_cien_mil),
            color = '#301526',
            size = 7,
            family = 'Roboto Condensed') +
  scale_x_continuous(breaks = c(2021, 2022)) +
  ylim(-10,650) + 
  labs(title = 'Casos policiales',
     subtitle = stringr::str_wrap('Tasa cada cien mil habitantes de algunos delitos de alta connotación social entre 2021 y 2022.', width = 40),
     x = '',
     y = '',
     caption = 'Fuente: CEAD SPD') +
  theme_minimal() +
  theme(text = element_text(size = 30, family = "Roboto Condensed", color = '#D1B9AB'),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'black', color = NA),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        plot.title = element_text(size = rel(2.0), family = "Courier", hjust = 0.5, color = "#D1B9AB", face ='bold'),
        plot.subtitle = element_text(color = '#D1B9AB', size = rel(0.8), hjust = 0.5, face = "italic", family = "Courier"))
        
gg <- gg +
  annotate(geom = "text", y = 590, x = 2021.8, color = "#C91333", label = "Hurtos", family = "Roboto Condensed", size = 6, fontface = "italic") +
  annotate(geom = "text", y = 410, x = 2021.78, color = "#FEFCDA", label = "Robo con violencia", family = "Roboto Condensed", size = 6, fontface = "italic") +
  annotate(geom = "text", y = 190, x = 2021.78, color = "#E48887", label = "Robo de vehículo", family = "Roboto Condensed", size = 6, fontface = "italic") +
  annotate(geom = "text", y = 30, x = 2021.8, color = "#A3757C", label = "Homicidio", family = "Roboto Condensed", size = 6, fontface = "italic")

gg_save_desuc(gg, 
              width = 25,
              height = 25,
              'day_7_hazards_1.png')


