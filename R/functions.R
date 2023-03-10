# areas_verdes <- st_read("./data/areas_verdes/areas_verdes.shp")
get_infraestructura_factible <- function(df_sf) {
  df_sf <- df_sf %>% as_tibble() %>% select(-c(geometry))
  
  # Busco cuáles registros cumplen con los valores de cada infraestructura y los guardo en un vector lógico
  es_biofiltro <- (df_sf$zona_num == 2 | df_sf$zona_num == 3) & df_sf$area >= 10 & df_sf$pendiente <= 3 & 
    (df_sf$infiltr == "Baja" | df_sf$infiltr == "Media") & df_sf$niv_freati >= 1
  es_humedales <- df_sf$zona_num == 3 & df_sf$area >= 35 & df_sf$pendiente <= 5 & 
    df_sf$infiltr == "Baja" & df_sf$niv_freati >= 0.6
  es_jardin_microcuenca <- (df_sf$zona_num == 1 | df_sf$zona_num == 2) & df_sf$area >= 8 & 
    df_sf$pendiente <= 3 & (df_sf$infiltr == "Media" | df_sf$infiltr == "Alta") & 
    df_sf$niv_freati >= 3 & df_sf$restringid == 0
  es_jardin_lluvia <- (df_sf$zona_num == 1 | df_sf$zona_num == 2) & df_sf$area >= 18 &
    df_sf$pendiente <= 8 & (df_sf$infiltr == "Media" | df_sf$infiltr == "Alta") & 
    df_sf$niv_freati >= 4 & df_sf$restringid == 0
  es_laguna_retencion <- (df_sf$zona_num == 1 | df_sf$zona_num == 2) & df_sf$area >= 100 & 
    df_sf$pendiente <= 5 & (df_sf$infiltr == "Media" | df_sf$infiltr == "Alta") & 
    df_sf$niv_freati >= 1 & df_sf$restringid == 0
  es_pavimento_permeable <- (df_sf$zona_num == 1 | df_sf$zona_num == 2) & df_sf$area >= 1 & 
    df_sf$pendiente <= 3 & (df_sf$infiltr == "Media" | df_sf$infiltr == "Alta") &
    df_sf$niv_freati >= 1.2 & df_sf$restringid == 0
  es_plaza_agua <- (df_sf$zona_num == 1 | df_sf$zona_num == 3) & df_sf$area >= 100 & df_sf$pendiente <= 5 &
    (df_sf$infiltr == "Baja" | df_sf$infiltr == "Media") & df_sf$niv_freati >= 2
  es_pozo_infiltracion <- (df_sf$zona_num == 1 | df_sf$zona_num == 2) & df_sf$area >= 1.5 & 
    df_sf$pendiente <= 5 & (df_sf$infiltr == "Media" | df_sf$infiltr == "Alta") & df_sf$niv_freati >= 11 & 
    df_sf$prox_pozo == 0 & df_sf$restringid == 0
  es_presa_filtrante <- df_sf$zona_num == 2 & df_sf$area >= 100 & df_sf$pendiente >= 15 &
    (df_sf$infiltr == "Media" | df_sf$infiltr == "Alta") & df_sf$niv_freati >= 1
  es_revegetacion_laderas <- (df_sf$zona_num == 1 | df_sf$zona_num == 2) & df_sf$area >= 1 & df_sf$pendiente >= 15 &
    (df_sf$infiltr == "Media" | df_sf$infiltr == "Alta") & df_sf$niv_freati >= 1
  es_zanja_bordo <- (df_sf$zona_num == 1 | df_sf$zona_num == 2) & df_sf$area >= 8 & 
    (df_sf$pendiente >= 5 & df_sf$pendiente <= 22) & (df_sf$infiltr == "Media" | df_sf$infiltr == "Alta") & 
    df_sf$niv_freati >= 1
  es_suelo_esponja <- (df_sf$zona_num == 1 | df_sf$zona_num == 2 | df_sf$zona_num == 3) & df_sf$area >= 1 &
    df_sf$pendiente <= 15 & df_sf$niv_freati >= 0.5
  
  # Cada vector
  df_biofiltro <- tibble(id = df_sf[es_biofiltro, ]$id, inf_factible = 1)
  df_humedales <- tibble(id = df_sf[es_humedales, ]$id, inf_factible = 2)
  df_jardin_microcuenca <- tibble(id = df_sf[es_jardin_microcuenca, ]$id, inf_factible = 3)
  df_jardin_lluvia <- tibble(id = df_sf[es_jardin_lluvia, ]$id, inf_factible = 4)
  df_laguna_retencion <- tibble(id = df_sf[es_laguna_retencion, ]$id, inf_factible = 5)
  df_pavimento_permeable <- tibble(id = df_sf[es_pavimento_permeable, ]$id, inf_factible = 6)
  df_plaza_agua <- tibble(id = df_sf[es_plaza_agua, ]$id, inf_factible = 7)
  df_pozo_infiltracion <- tibble(id = df_sf[es_pozo_infiltracion, ]$id, inf_factible = 8)
  df_presa_filtrante <- tibble(id = df_sf[es_presa_filtrante, ]$id, inf_factible = 9)
  df_revegetacion_laderas <- tibble(id = df_sf[es_revegetacion_laderas, ]$id, inf_factible = 10)
  df_zanja_bordo <- tibble(id = df_sf[es_zanja_bordo, ]$id, inf_factible = 11)
  df_suelo_esponja <- tibble(id = df_sf[es_suelo_esponja, ]$id, inf_factible = 12)
  
  df_aux <- tibble()
  df_aux <- rbind(df_aux, df_biofiltro, df_humedales, df_jardin_microcuenca, df_jardin_lluvia,
                  df_laguna_retencion, df_pavimento_permeable, df_plaza_agua, df_pozo_infiltracion, 
                  df_presa_filtrante, df_revegetacion_laderas, df_zanja_bordo, df_suelo_esponja)
  
  return(df_aux)
}