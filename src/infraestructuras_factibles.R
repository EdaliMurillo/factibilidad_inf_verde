library(dplyr)
library(sf)
library(ggplot2)
library(forcats)

source("./R/functions.R")

areas_infraestructura <- get_infraestructura_factible(areas_verdes)

inter <- areas_infraestructura %>% group_by(inf_factible) %>% count() %>% ungroup()

ggplot(data = inter, aes(x = forcats::fct_reorder(inf_factible, n), y = n, fill = inf_factible)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none",
        axis.text=element_text(size=12)) +
  geom_text(aes(label = n), vjust = 0.5, hjust = 3.5)

areas_join <- areas_verdes %>% 
  full_join(areas_infraestructura, join_by("id"), 
                                         multiple = "all")

sf::st_write(areas_join, dsn = "./results/areas_verdes_infraestructuras.geojson", 
             layer = "areas_verdes_infraestructuras.geojson", delete_layer = TRUE)

p_tipologias <- ggplot(data = areas_join %>% group_by(inf_factible) %>% count() %>% ungroup(), 
       aes(x = forcats::fct_reorder(inf_factible, n), y = n, fill = inf_factible)) + 
  geom_col() +
  coord_flip() + 
  labs(title = "Tipologías asociadas a polígonos de área verde",
       x = "Tipologías", 
       y = "Número de áreas verdes") + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 32)) + 
  geom_text(aes(label = n), vjust = 0.5, hjust = 3.5)
ggsave(filename = "./results/tipologias_areas_verdes.svg",
       plot = p_tipologias, width = 13, height = 9, dpi = 300)
ggsave(filename = "./results/tipologias_areas_verdes.png",
       plot = p_tipologias, width = 13, height = 9, dpi = 300)

###
p_tipologias_zona <- ggplot(data = areas_join %>% group_by(zona_num, inf_factible) %>% count(), 
       aes(x = forcats::fct_reorder(inf_factible, n), y = n, fill = inf_factible)) + 
  geom_col() + 
  facet_wrap(vars(zona_num), 
             labeller = as_labeller(
               c("1" = "Cuenca alta",
                 "2" = "Cuenca media",
                 "3" = "Cuenca baja"))
             ) + 
  coord_flip() + 
  labs(title = "Tipologías asociadas a polígonos de área verde",
       x = "Tipologías", 
       y = "Número de áreas verdes") + 
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 32)) + 
  geom_text(aes(label = n), vjust = 0.5, hjust = 0)
ggsave(filename = "./results/tipologias_areas_verdes_zona.svg",
       plot = p_tipologias_zona, width = 13, height = 9, dpi = 300)
ggsave(filename = "./results/tipologias_areas_verdes_zona.png",
       plot = p_tipologias_zona, width = 13, height = 9, dpi = 300)

### 
datos_spread <- areas_join %>% tidyr::spread(inf_factible, 1)

st_write(datos_spread, "./results/areas_verdes_infraestructuras_factibles", 
         driver = "ESRI Shapefile", delete_layer = TRUE)

ggplot(data = areas_join %>% group_by(inf_factible) %>% count() %>% ungroup(), 
        aes(x = forcats::fct_reorder(inf_factible, n), y = n, fill = inf_factible)) + 
  coord_flip() + 
  geom_col() +
  theme(legend.position = "none",
        axis.text=element_text(size=12)) + 
  geom_text(aes(label = n), vjust = 0.5, hjust = 3.5)
