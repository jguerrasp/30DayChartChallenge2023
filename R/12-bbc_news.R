library(tidyverse)
library(bbplot)
library(readxl)

# Lectura base ----------

caracteristicas <- read_xlsx('input/day11_top50_chile.xlsx') # Se obtuvo a partir del trabajo con el paquete spotifyr. Acá pueden encontrar un tutorial: https://www.rcharlie.com/spotifyr/

caracteristicas <- caracteristicas %>% 
  select(track.name,danceability,energy,speechiness, valence) %>% 
  pivot_longer(!track.name, names_to = 'tipo', values_to = 'valor')

# Guardar gráfico --------

gg_save_desuc <- function(gg_chart, name,
                          width = 22,
                          height = 13) {
  ggsave(filename = paste0('output/', name),
         plot = gg_chart,
         width = width,
         height = height,
         units = 'cm')
}


gg <- caracteristicas %>% 
  ggplot(aes(x=valor, group = tipo, fill = tipo)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300")) + 
  geom_curve(x = .25, y = 5.8, xend = .12, yend = 6, curvature = 0.2, color = '#990000', linewidth = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", x = .26, y = 5.8, label = "Habla", family = 'Helvetica', hjust = "left", size = 7, color = "#990000", fontface = "bold") + 
  geom_curve(x = .45, y = 2.6, xend = .5, yend = 2.1, curvature = 0.2, color = '#588300', linewidth = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", x = .35, y = 2.8, label = "Positividad", family = 'Helvetica', hjust = "left", size = 7, color = "#588300",  fontface = "bold") +
  geom_curve(x = .5, y = 4.3, xend = .62, yend = 3.7, curvature = 0.2, color = '#1380A1', linewidth = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", x = .45, y = 4.45, label = "Energía", family = 'Helvetica', hjust = "left", size = 7, color = "#1380A1", fontface = "bold") +
  geom_curve(x = .72, y = 4.5, xend = .79, yend = 3.9, curvature = 0.2, color = '#FAAB18', linewidth = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", x = .65, y = 4.7, label = "Bailabilidad", family = 'Helvetica', hjust = "left", size = 7, color = "#FAAB18", fontface = "bold") +
  labs(title = '¿Cómo nos gusta la música? 🎶',
       subtitle = 'Características musicales del Top 50 Chile en Spotify',
       caption = 'Fuente: Datos obtenidos a partir de API Spotify, con paquete spotifyr\nHabla indica proporción de palabras habladas en la canción') + 
  bbc_style() +
  theme(legend.position = 'none',
        plot.caption = element_text(color = '#222222', size = rel(1.1)))

gg_save_desuc(gg,
              width = 28,
              height = 25,
              'day11_bbcnews_1.png')

gg2 <- caracteristicas %>% 
  ggplot(aes(x=valor, group = tipo, fill = tipo)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300")) + 
  geom_curve(x = .25, y = 5.8, xend = .12, yend = 6, curvature = 0.2, color = '#990000', linewidth = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", x = .26, y = 5.8, label = "Habla", family = 'Helvetica', hjust = "left", size = 7, color = "#990000", fontface = "bold") + 
  geom_curve(x = .45, y = 2.6, xend = .5, yend = 2.1, curvature = 0.2, color = '#588300', linewidth = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", x = .4, y = 2.8, label = "Positividad", family = 'Helvetica', hjust = "left", size = 7, color = "#588300",  fontface = "bold") +
  geom_curve(x = .5, y = 4.3, xend = .62, yend = 3.7, curvature = 0.2, color = '#1380A1', linewidth = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", x = .45, y = 4.45, label = "Energía", family = 'Helvetica', hjust = "left", size = 7, color = "#1380A1", fontface = "bold") +
  geom_curve(x = .72, y = 4.5, xend = .79, yend = 3.9, curvature = 0.2, color = '#FAAB18', linewidth = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  annotate(geom = "text", x = .65, y = 4.7, label = "Bailabilidad", family = 'Helvetica', hjust = "left", size = 7, color = "#FAAB18", fontface = "bold") +
  labs(title = '¿Cómo nos gusta la música? 🎶',
       subtitle = 'Características musicales del Top 50 Chile en Spotify',
       caption = 'Fuente: Datos obtenidos a partir de API Spotify, con paquete spotifyr\nHabla indica proporción de palabras habladas en la canción') + 
  bbc_style() +
  theme(legend.position = 'none',
        plot.caption = element_text(color = '#222222', size = rel(1.1)))

gg_save_desuc(gg2, 
              width = 50,
              height = 25,
              'day11_bbcnews_2.png') 
