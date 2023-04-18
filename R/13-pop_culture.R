
colnames(series_streaming)
str(series_streaming)

series_streaming <- series_streaming %>% 
  mutate(streaming = as_factor(streaming))

levels(series_streaming$streaming)

colores_streaming <- c("#555555","#941DE8","#900b0b","#146EB4")
names(colores_streaming) <- levels(series_streaming$streaming)

series_streaming %>% 
  group_by(streaming, release_year) %>% 
  summarise(n = length(title)) %>% 
  print(n = 50)

series <- series_streaming %>% 
  ggplot(aes(x = release_year, y = imdb_votes, color = streaming, label = imdb_score)) +
  geom_point(size = 7) +
  geom_line() +
  geom_text(size = 4, color = "white", family = "Roboto Condensed", fontface = "bold") +
  geom_text(aes(label = title), data = series_streaming[series_streaming$imdb_votes > 500000,],
            color = "black", hjust = -0.2, family = "Roboto Condensed", fontface = "bold") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6),
                     breaks = seq(from = -500000, to = 2500000, by = 500000), expand = c(0.2,0.2)) +
  scale_x_continuous(breaks = seq(2000,2023,4)) +
  scale_color_manual(values = colores_streaming, guide = "none") +
  facet_wrap(~streaming, ncol = 1) +
  labs(title = "Las series mejor evaluadas según IMDB desde el 2000-2023",
       subtitle = "Relación entre la evaluación de las series y el número de votaciones en IMDB.\nEn el punto, el puntaje de evaluación de cada serie.",
       caption = "Fuente: Evaluaciones y número de votaciones en IMDB.", y = "Número de votaciones en IMDB", x = "Año de lanzamiento de la serie") +
  theme(axis.text = element_text(size = 12, family = "Hind Guntur", color = "#FFFCF2"),
        axis.title = element_text(size = 14, family = "Hind Guntur", color = "#FFFCF2"),
        plot.title = element_text(size = 20, color = "#EB5E28", face = "bold", family = "Roboto Condensed"),
        plot.subtitle = element_text(size = 16, color = "#FFFCF2", family = "Fira Sans Extra Condensed"),
        plot.caption = element_text(size = 12, color = "#FFFCF2", family = "Hind Guntur"),
        strip.text = element_text(size = 16, color = "black", family = "Fira Sans Extra Condensed", face = "bold"),
        strip.background = element_rect(fill = "#CCC5B9"),
        panel.background = element_rect(fill = "#FFFCF2"),
        panel.grid = element_line(colour = "#CCC5B9"),
        plot.background = element_rect(fill = "#403D39"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

series

ggsave(series, width = 25, height = 25, units = "cm",filename = "series.png")


series_streaming$title <- stringr::str_wrap(series_streaming$title, 20)

series_2 <- series_streaming %>% 
  ggplot(aes(x = release_year, y = imdb_votes, color = streaming, label = imdb_score)) +
  geom_point(size = 7, alpha = 0.3) +
  geom_line(alpha = 0.3) +
  geom_text(size = 4, color = "#FFFCF2", family = "Roboto Condensed", fontface = "bold") +
  geom_text(aes(label = title),
            color = "black", size = 3, hjust = "inward",
            family = "Roboto Condensed",
            fontface = "bold", angle = 90) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6),
                     breaks = seq(from = -500000, to = 2500000, by = 500000), expand = c(0.2,0.2)) +
  scale_x_continuous(breaks = seq(2000,2023,4)) +
  scale_color_manual(values = colores_streaming, guide = "none") +
  facet_wrap(~streaming, ncol = 1) +
  labs(title = "Las series mejor evaluadas según IMDB desde el 2000-2023",
       subtitle = "Relación entre la evaluación de las series y el número de votaciones en IMDB.\nEn el punto, el puntaje de evaluación de cada serie.",
       caption = "Fuente: Evaluaciones y número de votaciones en IMDB.", y = "Número de votaciones en IMDB", x = "Año de lanzamiento de la serie") +
  theme(axis.text = element_text(size = 12, family = "Hind Guntur", color = "#FFFCF2"),
        axis.title = element_text(size = 14, family = "Hind Guntur", color = "#FFFCF2"),
        plot.title = element_text(size = 20, color = "#EB5E28", face = "bold", family = "Roboto Condensed"),
        plot.subtitle = element_text(size = 16, color = "#FFFCF2", family = "Fira Sans Extra Condensed"),
        plot.caption = element_text(size = 12, color = "#FFFCF2", family = "Hind Guntur"),
        strip.text = element_text(size = 16, color = "black", family = "Fira Sans Extra Condensed", face = "bold"),
        strip.background = element_rect(fill = "#CCC5B9"),
        panel.background = element_rect(fill = "#FFFCF2"),
        panel.grid = element_line(colour = "#CCC5B9"),
        plot.background = element_rect(fill = "#403D39"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

series_2

ggsave(series_2, width = 25, height = 25, units = "cm",filename = "series_2.png")
