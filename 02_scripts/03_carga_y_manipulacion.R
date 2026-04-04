############################################
# Tidyverse: Cargar y manipular bases
############################################

library(tidyverse)
library(scales)
library(janitor)
library(readxl)
library(openxlsx)


############################################
# 1. Cargar base de datos
############################################

df <- read_csv("01_datos/vg_sales.csv") %>% 
  clean_names() %>% 
  mutate(year = as.integer(year))

############################################
# 2. Exploración inicial
############################################

head(df)
glimpse(df)
summary(df)
str(df)

unique(df$genre)
table(df$platform)


############################################
# 3. filter(): seleccionar filas
############################################

df %>% 
  filter(platform == "N64")

df %>% 
  filter(genre == "Racing")

# Guardar un objeto filtrado
df_n64 <- df %>% 
  filter(platform == "N64")

df_xbox <- df %>% 
  filter(platform %in% c("X360", "XB", "XOne"))


############################################
# 3. TRANSFORMACION mutate(): crear nuevas columnas
############################################

# Filtrar por condiciones combinadas
df %>% 
  filter(publisher == "Ubisoft")

df %>% 
  filter(publisher == "Ubisoft" & genre == "Action")



# Crear columna combinada
df_estudio_genero <- df %>% 
  mutate(estudio_genero = paste(publisher, genre, sep = "-"))

df_estudio_genero %>% 
  filter(estudio_genero == "Ubisoft-Action")

# Manipulación de numericas

df_ventas <- df %>% 
  mutate(ventas_totales = na_sales + eu_sales + jp_sales + other_sales)


# Columna condicional simple
hist(df_ventas$ventas_totales)

summary(df_ventas$ventas_totales)


df_hits <- df_ventas %>% 
  mutate(hit = ifelse(ventas_totales >= 5, 1, 0)) 


df_hits %>% 
  count(hit)


# Columna condicional múltiple (case_when)

df_categorias_venta <- df_hits %>% 
  mutate(categoria_ventas = case_when(ventas_totales < 0.05 ~ "Nicho",
                                      ventas_totales <= 0.5 ~ "Exito moderado",
                                      ventas_totales <= 3 ~ "Buen desempeño",
                                      ventas_totales <= 5 ~ "Hit",
                                      ventas_totales <= 20 ~ "Super hit",
                                      T ~ "Mega hit"))




table(df_categorias_venta$categoria_ventas)

############################################
# 4. COLAPSADO group_by() + summarise()
############################################

df_categorias_venta %>% 
  group_by(platform)

df_categorias_venta %>% 
  group_by(platform) %>% 
  summarise(total_titulos = n(),
            ventas_promedio = mean(ventas_totales))

df_promedio_plataforma <- df_categorias_venta %>% 
  group_by(platform) %>% 
  summarise(total_titulos = n(),
            ventas_promedio = mean(ventas_totales),
            hits = sum(hit, na.rm = T),
            titulo_mayor_venta = name[which.max(ventas_totales)],
            ventas_mayor = max(ventas_totales),
            ano_lanzamiento = min(year, na.rm = T)) %>% 
  arrange(-ano_lanzamiento)


############################################
# 5. Formateo
############################################

#Como se puede mejorar?
view(df_promedio_plataforma)

unique(df_promedio_plataforma$platform)

df_clean <- df_promedio_plataforma %>% 
  mutate(plataforma = recode(platform, 
                                   "PS4" = "PlayStation 4",
                                   "XOne" = "Xbox One",
                                   "WiiU" = "Wii U",
                                   "3DS" = "Nintendo 3DS",
                                   "PSV" = "PlayStation Vita",
                                   "PS3" = "PlayStation 3",
                                   "X360" = "Xbox 360",
                                   "PSP" = "PlayStation Portable",
                                   "GC" = "Nintendo GameCube",
                                   "GBA" = "Game Boy Advance",
                                   "PS2" = "PlayStation 2",
                                   "XB" = "Xbox",
                                   "WS" = "Wonder Swan",
                                   "DC" = "SEGA Dreamcast",
                                   "N64" = "Nintendo 64",
                                   "PCFX" = "NEC PC-FX",
                                   "TG16" = "TurboGrafix-16",
                                   "3DO" = "3DO Interactive Multiplayer",
                                   "PS" = "PlayStation",
                                   "SAT" = "SEGA Saturn",
                                   "NG" = "Neo Geo",
                                   "SCD" = "Sega CD",
                                   "GG" = "Game Gear",
                                   "GEN" = "SEGA Genesis",
                                   "SNES" = "Super Nintendo Entertainment System",
                                   "GB" = "GameBoy",
                                   "DS" = "Nintendo DS",
                                   "PC" = "Personal Computer",
                                   "NES" = "Nintendo Entertainment System",
                                   "2600" = "Atari 2600")) %>% 
  mutate(ventas_promedio = round(ventas_promedio, digits = 2)) %>% 
  mutate(total_titulos = comma(total_titulos)) %>% 
  select(plataforma, ano_lanzamiento, total_titulos, ventas_promedio, titulo_mayor_venta, ventas_mayor)
  
############################################
# 6. Exportado
############################################

write.xlsx(df_clean, "03_outputs/Ventas_por_consola.xlsx")



