# ---------------------------------------------------------------------------- #
#             Script #30DayChartChallenge 2023 - Día 17: networks
#
#  Analisis de redes sobre artistas que colaboraron con John Coltrane
#
#  Datos se pueden encontrar en: 
#           https://www.jazzdisco.org/john-coltrane/discography/
#
# ---------------------------------------------------------------------------- #


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(tidygraph)
library(rvest)
library(igraph)
library(ggraph)
library(extrafont)
library(grid)


# Funcion para guardar grafico
gg_save_desuc <- function(gg_chart, name,
                          width = 22,
                          height = 13) {
  ggsave(filename = paste0('output/', name),
         plot = gg_chart,
         width = width,
         height = height,
         units = 'cm')
}



# Webscrapping ------------------------------------------------------------

## Pagina web
url <- url('https://www.jazzdisco.org/john-coltrane/discography/')
html <- read_html(url)


## Recuperar informacion de musicos
artistas <- html %>% 
  html_nodes(c('h3+ p')) %>% 
  html_text()

## Recuperar nombre de albumes
album <- html %>% 
  html_nodes(c('#discography-data a')) %>% 
  html_text()

# Crear base de datos
df <- tibble(album = album,
             musicos = artistas)


# Procesamiento -----------------------------------------------------------

## Limpieza de strings
df <- df %>% 
  mutate(musicos = str_remove_all(musicos, '(?<=,)[[:space:]]*[^;]*;'),
         musicos = str_remove_all(musicos, '(?=\\+)[[:space:]]*[^:]*:'),
         musicos = str_remove_all(musicos, 'cello|piano|tenor sax|drums|bass|vocal|conductor|arranger|announcer|: same personnel|oud|percussion'),
         musicos = str_remove_all(musicos, '\\.'),
         musicos = str_remove_all(musicos, '[[:digit:]]'),
         musicos = str_remove_all(musicos, '[[:punct:]](?![[:space:],])'))


## Separar artistas, otras transformaciones
nom_col <- str_glue("musico_{seq(1:16)}")

df <- df %>% 
  separate(musicos,
           into = nom_col,
           sep = ",") %>% 
  mutate(across(starts_with("mu"), ~str_trim(.)),
         across(starts_with("mu"), ~str_remove_all(., "[:punct:]")),
         across(starts_with("mu"), ~ifelse(. == "", NA, .)),
         across(starts_with("mu"), ~str_remove(., "\\+ "))) %>% 
  filter(album != 'U.S. Navy Band') %>% 
  select(!album)


## Df en formato long
df_long <- df %>% 
  pivot_longer(cols = !musico_1) %>% 
  na.omit(value) %>% 
  select(de = musico_1,
         para = value) %>% 
  mutate(para = str_remove(para, " as Johnny Coltrane")) %>% 
  filter(para != 'unknown')


top_40 <- df_long %>% 
  count(de, para) %>% 
  arrange(desc(n)) %>% 
  distinct(para, .keep_all = T) %>% 
  slice_head(n = 40)


## Filtrar top 40 musicos
df_long2 <- df_long %>% 
  filter(de %in% top_40$de & para %in% top_40$para) %>% 
  distinct()
  




# Red ---------------------------------------------------------------------

## Crear red
jazz_red <- graph_from_data_frame(df_long2, directed = FALSE)


## Agregar grados
V(jazz_red)$Grados <- degree(jazz_red)


## Buscar comunalidades
jazz_red_clust <- cluster_louvain(jazz_red)


## Agregar comunalidades como factor a la red
V(jazz_red)$Clusters <- jazz_red_clust$membership
V(jazz_red)$Clusters <- factor(V(jazz_red)$Clusters)


## Eigen centralidad
Eigen.F <- eigen_centrality(jazz_red)$vector

## Layout
l <- layout_with_fr(jazz_red)


## Tamanios de nombre personalizados
tam_nomb <- V(jazz_red)$name %>% 
  as_tibble() %>% 
  mutate(size = case_when(value == 'John Coltrane' ~ 18,
                          TRUE ~ 10))


# Graficar red ------------------------------------------------------------

# Fuente: hhttps://www.flaticon.es/icono-gratis/saxofon_2002876
saxofon <- png::readPNG("./input/images/saxofon.png")

# Grafico
gg <- jazz_red %>% 
  ggraph(layout = l) +
  geom_edge_arc(lineend = "round",
                strength = .1,
                alpha = 0.25,
                color = "#C6C6C6") +
  geom_node_point(aes(size = Grados,
                      color = as.factor(Clusters),
                      shape = as.factor(Clusters))) + 
  scale_color_manual(values = c("#BF80E1", "#00FFFF", "#FB832D", "#1ABC9C")) +
  scale_shape_manual(values = c(15, 17, 18, 16)) +
  geom_node_text(aes(label = name, 
                     size = tam_nomb$size),
                 colour = "#FCCE48",
                 repel = TRUE, 
                 point.padding = unit(0.1, "lines")) +
  labs(title = "Red de John Coltrane",
       subtitle = "Músicos con quienes más colaboró",
       caption = "John Coltrane fue un saxofonista de jazz, considerado como uno de los más virtuosos de su generación.\nSe destaca su colaboración con otros dos grandes del género: Miles Davis y Thelonious Monk.\nFuente de datos: Jazzdisco") +
  annotation_custom(rasterGrob(saxofon, 
                               interpolate = TRUE,
                               height = unit(3, "cm"),
                               x = 0.08,
                               y = 0.1)) +
  theme(text = element_text(family = "Roboto"),
        plot.background = element_rect(fill = "#343434",
                                       colour = "#343434"),
        panel.background = element_rect(fill = "#343434",
                                        colour = "#343434"),
        legend.position = "none",
        plot.title = element_text(face = "bold", 
                                  colour = "white",
                                  hjust = 1,
                                  size = rel(5.0)),
        plot.subtitle = element_text(face = "bold", 
                                     colour = "white",
                                     hjust = 1,
                                     size = rel(3.0)),
        plot.caption = element_text(face = "bold", 
                                    colour = "white",
                                    hjust = 0,
                                    size = rel(1.13)),
        plot.caption.position = "plot",
        plot.margin = margin(1, 1, 1, 1, 'cm'))


## Exportar
gg_save_desuc(gg,
              name = "day17_networks.png",
              width = 25,
              height = 25)


