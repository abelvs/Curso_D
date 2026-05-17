pacman::p_load("tidyverse", "data.table", "readxl", "janitor", 
               "writexl", "sf", "stringi", "viridis", "scales")

options(scipen = 999)


# ── 1. CATÁLOGO DE LOCALIDADES ───────────────────────────────────────────────
# Carga el catálogo de localidades y dale una revisada con glimpse

cat_loc <- 
  
  
  glimpse(cat_loc)


# ── 2. CLAVES GEOGRÁFICAS ────────────────────────────────────────────────────
# Construye las claves de localidad, municipio y entidad

cat_clean <- 
  
  
  # ── 3. CATÁLOGO MUNICIPAL ────────────────────────────────────────────────────
  # Agrega la información a nivel municipio
  
  cat_municipal <- 
  
  
  # ── 4. CATÁLOGO ESTATAL ──────────────────────────────────────────────────────
  # Ahora agrégala a nivel entidad federativa
  
  cat_entidad <- 
  
  
  # ── 5. DATOS DE LA CNBV ──────────────────────────────────────────────────────
  # Carga los tres archivos de la CNBV
  
  datos_cnvb <- 
  
  cat_municipal_cnbv <- 
  
  cat_instituciones_cnvb <- 
  
  
  # ── 6. MUNICIPIOS COMPARATIVOS ───────────────────────────────────────────────
  # Cruza el catálogo de la CNBV con el de INEGI
  
  comparativa_municipios <- 
  
  
  # ── 7. LIMPIEZA DE DATOS CNBV ────────────────────────────────────────────────
  # Filtra y enriquece la tabla principal de datos
  
  datos_cnvb_clean <- 
  
  
  # ── 8. TABLA ANCHA POR INSTITUCIÓN ──────────────────────────────────────────
  # Pivotea para tener una columna por institución
  
  datos_cnvb_wide <- 
  
  
  # ── 9. RESUMEN MUNICIPAL ─────────────────────────────────────────────────────
  # Une todo y calcula los indicadores de cajeros
  
  resumen_municipal <- 
  
  
  # ── 10. RESUMEN ESTATAL ──────────────────────────────────────────────────────
  # Lo mismo pero a nivel estado
  
  resumen_estatal <- 
  
  
  # ── 11. MAPA ─────────────────────────────────────────────────────────────────
  
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
  theme(plot.background = element_rect(fill = "#0E1117", color = NA),
        panel.background = element_rect(fill = "#0E1117", color = NA),
        plot.title = element_text(color = "white", size = 24, face = "bold"),
        plot.subtitle = element_text(color = "gray80", size = 14),
        plot.caption = element_text(color = "gray60", size = 10),
        legend.position = "right",
        legend.title = element_text(color = "white", face = "bold"),
        legend.text = element_text(color = "gray85"),
        legend.background = element_rect(fill = "#0E1117", color = NA))