library(tuber) # youtube API
library(magrittr) # Pipes %>%, %T>% and equals(), extract().
library(tidyverse) # all tidyverse packages
library(purrr) # package for iterating/extracting data

# HYBE LABELS

b <- get_playlist_items(filter = 
                          c(playlist_id = "PL_Cqw69_m_ywY88NJ5SlpuaO_rmpKXO0D"), 
                        part = "contentDetails",
                        # set this to the number of videos
                        max_results = 200)

b_vector <- base::as.vector(b$contentDetails.videoId)

get_all_stats <- function(id) {
  tuber::get_stats(video_id = id)
} 

b_vector_stats <- purrr::map_df(.x = b_vector, 
                                .f = get_all_stats)

# Extraer el nombre de los videos

get_titles <- function(id){
  a <- get_video_details(id)
  b <- a$items[[1]]$snippet$title
}

res_df <- b_vector_stats %>% 
  rowwise() %>% 
  mutate(titles = get_titles(id))

get_published_dates <- function(id){
  a <- get_video_details(id)
  b <- a$items[[1]]$snippet$publishedAt
}

res_df <- res_df %>% 
  rowwise() %>% 
  mutate(published_dates = get_published_dates(id))

hybe_newjeans_data <- res_df %>% 
  mutate(canal = "HYBE LABELS")

# NewJeans

## Gráfico ----

library(png)
library(grid)
library(ggimage)

logo <- readPNG("images/newjeans_name.png")
youtube <- readPNG("images/hypeboy_youtube.png")

res_df <- res_df %>% 
  mutate(canal = "NewJeans")

res_df <- bind_rows(res_df, hybe_newjeans_data)

new_jeans <- tibble::rowid_to_column(res_df, "id_num")

new_jeans <- new_jeans %>% 
  mutate_at(vars(viewCount,likeCount,commentCount), ~as.numeric(.)) %>% 
  filter(viewCount > 10000000) %>% #NewJeans
  arrange(published_dates)


#5C62AC, #B8BFF3, #EBECEF, #6B73AA, #424C91

plot_nj <- ggplot(new_jeans, aes(x = fct_reorder(titles, viewCount), y = -viewCount, fill = canal)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(round(viewCount/1e6,1), "M"), family = "Roboto Condensed"), 
            hjust = "inward", color = "#424C91") +
  geom_hline(yintercept = 0, color = "#6B73AA") +
  geom_vline(xintercept = 0.4, color = "#6B73AA") +
  scale_fill_manual(values = c("#B8BFF3","#dbdff9"), name = "YouTube Channels:",
                    guide = guide_legend(title.position = "top",override.aes = aes(size = 5,shape = 19))) +
  scale_x_discrete(name = "", position = "top", labels = function(x) str_wrap(x, width = 60)) +
  scale_y_continuous(name = "",
                     breaks = c(-10000000,-20000000,-30000000,-40000000,-50000000,-60000000,-70000000,-80000000,-90000000,-100000000), 
                     labels = c("10M","20M","30M","40M","50M","60M","70M","80M","90M","100M"),
                     expand = c(0,0)) +
  labs(title = "NEWJEANS",
       subtitle = "Número de visitas de los videos más vistos de NewJeans en YouTube (>10M).",
       caption = "Fuente: Datos obtenidos desde Youtube el 10 de abril del 2022 a través de library(tuber).") +
  annotation_custom(rasterGrob(logo, 
                               interpolate = TRUE, 
                               width = unit(3.5,'cm'),
                               x = unit(2.05,"npc"), y = unit(1.09,"npc"),
                               #just = c("left","top")
  )) +
  annotation_custom(rasterGrob(youtube, 
                               interpolate = TRUE, 
                               width = unit(10,'cm'),
                               x = unit(0.35,"npc"), y = unit(0.25,"npc"),
                               #just = c("left","top"))
  )) +
  annotate("text", x = 9.5, y = -63000000, label = "'Hype Boy' Official MV es uno de los\nvideos más vistos del grupo.\nEste video fue subido el 18 de agosto del 2022\ny ya acumula más de 94M de reproducciones.",
           family = "Roboto Condensed", size = 3.5, color = "#424C91", fontface = 2) +
  geom_curve(aes(x = 19, xend = 10, y = -95000000, yend = -88000000),
             arrow = arrow(length = unit(0.03, "npc")), color = "#424C91", curvature = 0.3) +
  coord_flip(clip = "off") +
  theme(axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 65, color = "#424C91", family = "Village"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, color = "#424C91", family = "Roboto Condensed"),
        plot.caption = element_text(hjust = 0.9, size = 12, color = "#424C91", family = "Roboto Condensed"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 12, color = "#424C91", family = "Roboto Condensed"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "white"),
        legend.position = c("bottom"),
        legend.justification = c(0,5),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

ggsave(plot_nj, width = 25, height = 25, units = "cm",filename = "new_jeans.png")
