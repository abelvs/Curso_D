
pacman::p_load("tidyverse", "data.table", "readxl", "janitor",  "writexl", "sf", "stringi", "viridis", "scales")

options(scipen = 999)


#Cargamos el catalogo de localidades

cat_loc <- fread("01_datos/Sesion 4/AGEEML_20264121939532.csv",
                 encoding = "Latin-1") %>% 
  clean_names() %>% 
    mutate(across(c(contains("pob_"), total_de_viviendas_habitadas), as.numeric)) 

glimpse(cat_loc)


cat_clean <- cat_loc %>% 
  mutate(clave_localidad = str_pad(cvegeo, side = "left", width = 9, pad = "0"),
         clave_municipio = substr(clave_localidad, 1,5),
         clave_entidad = substr(clave_municipio, 1,2))


#Generamos catálogo municipal con poblacion y viviendas

cat_municipal <- cat_clean %>% 
  group_by(clave_municipio) %>% 
  summarise(clave_entidad = first(clave_entidad),
            nom_ent = first(nom_ent),
            nom_mun = first(nom_mun),
            pob_total = sum(pob_total, na.rm = T),
            pob_masculina = sum(pob_masculina, na.rm = T),
            pob_femenina = sum(pob_femenina, na.rm = T),
            total_de_viviendas_habitadas = sum(total_de_viviendas_habitadas, na.rm = T))


cat_entidad <- cat_municipal %>% 
  group_by(clave_entidad) %>% 
  summarise(nom_ent = first(nom_ent),
            nom_mun = first(nom_mun),
            pob_total = sum(pob_total, na.rm = T),
            pob_masculina = sum(pob_masculina, na.rm = T),
            pob_femenina = sum(pob_femenina, na.rm = T),
            total_de_viviendas_habitadas = sum(total_de_viviendas_habitadas, na.rm = T))



#Cargamos datos de la banca----

datos_cnvb <- fread("01_datos/Sesion 4/R2422_datos_40.csv") %>% 
  clean_names()


cat_municipal_cnbv <- fread("01_datos/Sesion 4/cat_estado_municipio_40.csv") %>% 
  clean_names() %>% 
  filter(id_estado_municipio != -1) %>% 
  mutate(clave_municipio = str_pad(substr(id_estado_municipio, 4,length(id_estado_municipio)),
                                   width = 5, side = "left", pad = "0")) 

cat_instituciones_cnvb <- fread("01_datos/Sesion 4/cat_instituciones_40.csv") %>% 
  clean_names()



#Creamos base completa----

comparativa_municipios <- cat_municipal_cnbv %>% 
  left_join((cat_municipal %>% select(clave_municipio, nom_mun) %>% mutate(en_inegi = 1)), by = "clave_municipio") %>% 
  filter(en_inegi == 1)



datos_cnvb_clean <- datos_cnvb %>% 
  filter(periodo == "202602") %>% 
  filter(tipo_info_operativa == 33) %>% 
  mutate(clave_municipio = str_pad(substr(id_estado_municipio, 4,length(id_estado_municipio)),
                                   width = 5, side = "left", pad = "0")) %>% 
  select(dato, clave_municipio, entidad) %>% 
  left_join((cat_municipal %>% select(clave_municipio, nom_ent, nom_mun, pob_total, total_de_viviendas_habitadas)), by = "clave_municipio") %>% 
  left_join((cat_instituciones_cnvb %>% select(entidad, nombre_entidad)), by = "entidad") %>% 
  select(clave_municipio, nom_ent, nom_mun, pob_total, total_de_viviendas_habitadas, nombre_entidad, dato)


#Creamos tablas agregadas ----

datos_cnvb_wide <- datos_cnvb_clean %>% 
  select(-c(pob_total, total_de_viviendas_habitadas, nom_ent, nom_mun)) %>% 
  pivot_wider(id_cols = c(clave_municipio),
              names_from = nombre_entidad,
              values_from = dato,
              values_fill = 0) %>% 
  clean_names() 


resumen_municipal <- cat_municipal %>% 
  left_join(datos_cnvb_wide, by = c("clave_municipio")) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  mutate(cajeros_por_hab = (total_banca_multiple/pob_total)*100000,
         cajeros_por_viv = total_banca_multiple/total_de_viviendas_habitadas)


resumen_estatal <- resumen_municipal %>% 
  group_by(nom_ent) %>% 
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate(cajeros_por_hab = (total_banca_multiple/pob_total)*100000,
         cajeros_por_viv = total_banca_multiple/total_de_viviendas_habitadas)

  
#Cargamos shapes----

shape_mun <- st_read("01_datos/Sesion 4/shape_mun/mun_sf_simple.shp") %>% 
  left_join(resumen_municipal, by = c("cvegeo" = "clave_municipio"))



ggplot(shape_mun)+
  geom_sf(aes(fill = cajeros_por_hab),
          color = NA)+
  scale_fill_viridis_c(option = "magma",
                       trans = "sqrt",
                       labels = label_number(accuracy = 1),
                       name = "Cajeros por\n100 mil hab.")+
  labs(title = "Disponibilidad de cajeros automáticos en México",
       subtitle = "Cajeros de banca múltiple por cada 100 mil habitantes",
       caption = "Fuente: CNBV e INEGI")+
  theme_void()+
  theme(plot.background = element_rect(fill = "#0E1117",
                                       color = NA),
        panel.background = element_rect(fill = "#0E1117",
                                        color = NA),
        plot.title = element_text(color = "white",
                                  size = 24,
                                  face = "bold"),
        plot.subtitle = element_text(color = "gray80",
                                     size = 14),
        plot.caption = element_text(color = "gray60",
                                    size = 10),
        legend.position = "right",
        legend.title = element_text(color = "white",
                                    face = "bold"),
        legend.text = element_text(color = "gray85"),
        legend.background = element_rect(fill = "#0E1117",
                                         color = NA))
