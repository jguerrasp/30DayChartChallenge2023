library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(desuctools)
library(readxl)

#Prep bbdd

df <- read_excel("input/incendios_forestales_2023.xlsx") 

df <- df %>%
  filter(nombre_region != "Total",
         hectareas_afectadas_23 >= 700) %>%
  mutate(label_var_ha_afectadas = paste0(label_var_ha_afectadas, "%"), 
         label_var_ha_afectadas = gsub("^(?!-)", "+", label_var_ha_afectadas, perl = TRUE),
         hectareas_afectadas_23 = round(hectareas_afectadas_23,0)) %>% 
  arrange(desc(hectareas_afectadas_23))

# grafico

df %>%
  mutate(
    place = if_else(row_number() == 1, 1, 0),
    ha_label = format(hectareas_afectadas_23, big.mark = ","),
    ha_label = paste(" ", as.character(ha_label), " ")
  ) %>%
  ggplot(aes(x = hectareas_afectadas_23, y = fct_reorder(nombre_region, hectareas_afectadas_23))) +
  geom_col(fill = "#FFA500") +
  geom_text(aes(label = ha_label, hjust = place), 
            size = 4, fontface = "bold") +
  geom_point(aes(x = max(hectareas_afectadas_23) + max(hectareas_afectadas_23) * 0.05, y = fct_reorder(nombre_region, hectareas_afectadas_23), 
                 size = abs(prop_var_ha_afectadas), color = factor(ifelse(grepl("-", label_var_ha_afectadas), "#2E8B57", "red4")))) +
  geom_text(aes(x = max(hectareas_afectadas_23) + max(hectareas_afectadas_23) * 0.12, y = fct_reorder(nombre_region, hectareas_afectadas_23), 
                label = as.factor(as.character(label_var_ha_afectadas))), size = 4) +
  scale_color_manual(values = c( "#2E8B57",  "red4"))+
  #  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  scale_size(range = c(5, 15))  +
  # Add a title and subtitle
  ggtitle("Hectáreas afectadas por incendios forestales en Chile") +
  labs(subtitle = "y su variación en el último periodo (2022-2023).",
       caption ="Fuente: Sistema de Información Digital para el Control de Operaciones – SIDCO CONAF") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#fff9e8"),
    axis.text.y = element_text(size = 14, hjust = 1),
    plot.margin = unit(c(30, 30, 30, 30), "pt"),
    legend.position = "none",
    plot.title = element_text(size = 30, face = "bold", margin = margin(b = 10), hjust=0.5, color="#575757"),
    plot.subtitle = element_text(size = 20, , margin = margin(b = 20), hjust=0.5, color="#636363")
  ) -> gg_incendio



# Guardar imagen

gg_save_desuc <- function(gg_chart, name,
                          width = 22,
                          height = 13) {
  ggsave(filename = paste0('output/', name),
         plot = gg_chart,
         width = width,
         height = height,
         units = 'cm')
}

gg_save_desuc(gg_incendio, 
              width = 32,
              height = 25,
              'day3_flora_fauna.png')

gg_save_desuc(gg_incendio, 
              width = 32,
              height = 18,
              'day3_flora_fauna_2.png')
