
pacman::p_load("tidyverse", "data.table", "readxl", "janitor",  "writexl", "sf", "stringi", "viridis", "scales")

options(scipen = 999)


#Cargamos el catalogo de localidades





sucs_oxxo <- fread("01_datos/Sesion 5/INEGI_DENUE_23052026.csv",
                                       encoding = "Latin-1") %>% 
  clean_names() 

tabyl(sucs_oxxo$razon_social)

#encontramos que son las que contienen "CADENA" y "OXXO" 

sucs_7_eleven <- fread("01_datos/Sesion 5/INEGI_DENUE_23052026_7_eleven.csv", encoding = "Latin-1") %>% 
  clean_names()

tabyl(sucs_7_eleven$razon_social)

#En este caso, las que contienen "7" y "ELEVEN"

sucs_oxxo_clean <- sucs_oxxo %>% 
  filter(str_detect(razon_social, "CADENA") &
           str_detect(razon_social, "OXXO")) %>% 
  mutate(clave_entidad = str_pad(clave_entidad, side = "left", width = 2, pad = 0),
         clave_municipio = str_pad(clave_municipio, side = "left", width = 3, pad = 0),
         clave_inegi = paste0(clave_entidad, clave_municipio)) %>% 
  select(nombre_de_la_unidad_economica, clave_inegi, latitud, longitud)

sucs_7_eleven_clean <- sucs_7_eleven %>% 
  filter(str_detect(razon_social, "7") &
           str_detect(razon_social, "ELEVEN")) %>% 
  mutate(clave_entidad = str_pad(clave_entidad, side = "left", width = 2, pad = 0),
         clave_municipio = str_pad(clave_municipio, side = "left", width = 3, pad = 0),
         clave_inegi = paste0(clave_entidad, clave_municipio)) %>% 
  select(nombre_de_la_unidad_economica, clave_inegi, latitud, longitud)


#Cargamos cat municipal

catalogo_municipios <- read_xlsx("01_datos/Sesion 5/AGEEML_2026523041183.xlsx",
                                 skip = 3) %>% 
  clean_names() %>% 
  rename(clave_inegi = cvegeo) %>% 
  mutate(pob_total = as.numeric(pob_total),
         total_de_viviendas_habitadas = as.numeric(total_de_viviendas_habitadas)) %>% 
  select(clave_inegi, nom_ent, nom_mun, pob_total, pob_masculina, pob_femenina, total_de_viviendas_habitadas)


#Creamos conteos municipales de cada tienda y creamos un objeto

oxxo_mun <- sucs_oxxo_clean %>% 
  count(clave_inegi, name = "n_oxxos")

seven_mun <- sucs_7_eleven_clean %>% 
  count(clave_inegi, name = "n_seven")

sucursales_mun <- oxxo_mun %>% 
  full_join(seven_mun, by = "clave_inegi") %>% 
  mutate(across(where(is.numeric),
                ~ replace_na(.x, 0)))

#Juntamos con catálogo mun

municipios_con_sucursales <- catalogo_municipios %>% 
  left_join(sucursales_mun, by = "clave_inegi") %>% 
  mutate(tot_sucursales = n_oxxos+ n_seven,
         oxxos_hab = (n_oxxos/pob_total)*100000,
         seven_hab = (n_seven/pob_total)*100000,
         oxxos_viv = (n_oxxos/total_de_viviendas_habitadas)*100000,
         seven_viv = (n_seven/total_de_viviendas_habitadas)*100000) %>% 
  mutate(con_oxxo = ifelse(n_oxxos >0, 1,0),
         con_seven = ifelse(n_seven>0, 1,0),
         solo_oxxo = ifelse(con_oxxo == 1 & con_seven ==0, 1,0),
         solo_seven = ifelse(con_oxxo == 0 & con_seven == 1, 1,0)) 


tabla_entidad <- municipios_con_sucursales %>% 
  group_by(nom_ent) %>% 
  summarize(across(c(n_oxxos,
                     n_seven,
                     tot_sucursales,
                     pob_total,
                     total_de_viviendas_habitadas,
                     con_oxxo,
                     con_seven,
                     solo_oxxo,
                     solo_seven),
                   sum,
                   na.rm = TRUE)) %>% 
  mutate(oxxos_hab = (n_oxxos/pob_total)*100000,
         seven_hab = (n_seven/pob_total)*100000,
         
         oxxos_viv = (n_oxxos/total_de_viviendas_habitadas)*100000,
         seven_viv = (n_seven/total_de_viviendas_habitadas)*100000) 


shape_mun <- st_read("01_datos/Sesion 5/Mun_Simple/mun_sf_simple.shp") %>% 
  left_join(municipios_con_sucursales, by = c("cvegeo"= "clave_inegi"))


ggplot() +
  geom_sf(data = shape_mun,
          aes(fill = ifelse(n_oxxos == 0, NA, n_oxxos)),
          color = NA) +
  
  geom_sf(data = filter(shape_mun, n_oxxos == 0),
          fill = "grey92",
          color = NA) +
  
  scale_fill_gradient(low = "#ffd9df",
                      high = "#e80829",
                      trans = "sqrt",
                      labels = comma,
                      name = "N.º OXXOs",
                      na.value = "grey92") +
  
  theme_void()


ggsave("03_outputs/oxxo_mapa_twitter.png",
       width = 10,
       height = 5.625,
       dpi = 300)

ggplot() +
  geom_sf(data = shape_mun,
          aes(fill = ifelse(n_seven == 0, NA, n_seven)),
          color = NA) +
  
  geom_sf(data = filter(shape_mun, n_seven == 0),
          fill = "grey92",
          color = NA) +
  
  scale_fill_gradient(low = "#c9f2e8",
                      high = "#058667",
                      trans = "sqrt",
                      labels = comma,
                      name = "N.º Seven",
                      na.value = "grey92") +
  
  theme_void()

ggsave("03_outputs/seven_mapa_twitter.png",
       width = 10,
       height = 5.625,
       dpi = 300)


#Analisis de distancia ----

oxxo_sf <- st_as_sf(sucs_oxxo_clean, coords = c("longitud", "latitud"), crs = 4326)
seven_sf <- st_as_sf(sucs_7_eleven_clean, coords = c("longitud", "latitud"), crs = 4326)


oxxo_sf <- st_transform(oxxo_sf, 6372)
seven_sf <- st_transform(seven_sf, 6372)


dist <- st_distance(oxxo_sf, seven_sf)

oxxo_sf$dist_min_m <- apply(dist, 1, min)

summary(oxxo_sf$dist_min_m)


dist_mun <- oxxo_sf %>% 
  mutate(clave_inegi = str_pad(clave_inegi, 5, pad = "0")) %>% 
  st_drop_geometry() %>% 
  group_by(clave_inegi) %>% 
  summarize(dist_promedio_m = mean(dist_min_m, na.rm = TRUE),
    dist_mediana_m = median(dist_min_m, na.rm = TRUE),
    oxxos_cerca_100m = sum(dist_min_m < 100, na.rm = TRUE))


tmap_mode("view")
mun_id <- "19039"

tm_shape(shape_mun %>% filter(cvegeo == mun_id)) +
  tm_polygons(alpha = 0.3, col = "grey90") +
  
  tm_shape(oxxo_sf %>% filter(clave_inegi == mun_id)) +
  tm_symbols(col = "#e80829", border.col = "#e80829", size = 0.4) +
  
  tm_shape(seven_sf %>% filter(clave_inegi == mun_id)) +
  tm_symbols(col = "#058667", border.col = "#058667", size = 0.4)
