library(dplyr)
library(sf)

areas_verdes <- st_read("./data/areas_verdes/areas_verdes.shp")
areas_verdes <- areas_verdes %>% 
  mutate(
    niv_freati = case_when(
      niv_freati == "60-80" ~ "60",
      niv_freati == "80-100" ~ "80",
      niv_freati == "100-120" ~ "100",
      niv_freati == "120-140" ~ "120",
      niv_freati == "140-160" ~ "140",
      niv_freati == "160-180" ~ "160",
      niv_freati == ">180" ~ "180"
      ),
    niv_freati = niv_freati %>% as.integer())

areas_verdes <- areas_verdes %>% mutate(prox_pozo = ifelse(prox_pozo %in% "si", "1", "0"),
                        prox_pozo = prox_pozo %>% as.integer(),
                        restringid = ifelse(restringid %in% "si", "1", "0"),
                        restringid = restringid %>% as.integer())

areas_verdes <- areas_verdes %>% st_make_valid()
areas_verdes %>% st_is_valid()
                        
areas_verdes %>% str()
