############################################
# Flujo de trabajo
############################################

library(tidyverse)
library(scales)
library(janitor)
library(readxl)
library(openxlsx)

#Cargamos base y catálogo

df <- read_csv("01_datos/vg_sales.csv") %>% 
  clean_names() %>% 
  mutate(year = as.integer(year)) 

cat <- read_xlsx("01_datos/Catalogo_consolas.xlsx")

#Creamos tabla final en un solo flujo

df_clean <- df %>% 
  left_join(cat, by = "platform") %>% 
  group_by(plataforma) %>% 
  mutate(ventas_totales = na_sales+eu_sales+jp_sales+other_sales,
         hit = ifelse(ventas_totales >= 3, 1,0)) %>% 
  summarise(ano_lanzamiento = min(year),
            total_titulos = comma(n()),
            ventas_promedio = round(mean(ventas_totales), digits = 2),
            titulo_mayor_venta = name[which.max(ventas_totales)],
            ventas_mayor = max(ventas_totales, na.rm = T)) %>% 
  arrange(-ano_lanzamiento)

write.xlsx(df_clean, "03_outputs/Ventas_por_consola.xlsx")


