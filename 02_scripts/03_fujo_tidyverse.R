############################################
# Tidyverse: Exploración de datos del SESNSP
# Incidencia delictiva por entidad y subtipo
############################################

library(tidyverse)
library(scales)
library(janitor)
library(readxl)
library(openxlsx)


############################################
# 1. Cargar base de datos
############################################

df <- read_xlsx("01_datos/Ejemplo_2_incidencia_estatal_delitos_2015_2025_oct.xlsx") %>%
  clean_names() %>%
  mutate(ano = as.integer(ano))


############################################
# 2. Exploración inicial
############################################

head(df)
glimpse(df)
summary(df)

unique(df$subtipo_de_delito)
table(df$entidad)


############################################
# 3. filter(): seleccionar filas
############################################

df %>%
  filter(entidad == "Ciudad de México")

df %>%
  filter(subtipo_de_delito == "Homicidio doloso")

# Guardar objetos filtrados
df_cdmx <- df %>%
  filter(entidad == "Ciudad de México")

df_bajio <- df %>%
  filter(entidad %in% c("Guanajuato", "Jalisco", "Michoacán de Ocampo", "Querétaro"))


############################################
# 4. mutate(): crear y transformar columnas
############################################

# Filtrar por condiciones combinadas
df %>%
  filter(entidad == "Sonora" & subtipo_de_delito == "Extorsión")

# Columna condicional simple: ¿fue antes o después del cambio de gobierno?
df <- df %>%
  mutate(periodo = ifelse(ano <= 2018, "Antes 2018", "Después 2018"))

# Columna condicional múltiple con case_when
# Clasifica el volumen de delitos reportados por fila
df_niveles <- df %>%
  mutate(nivel_incidencia = case_when(
    total == 0          ~ "Sin registro",
    total <= 50         ~ "Baja",
    total <= 300        ~ "Media",
    total <= 1000       ~ "Alta",
    total <= 5000       ~ "Muy alta",
    TRUE                ~ "Crítica"))

table(df$nivel_incidencia)


############################################
# 5. group_by() + summarise(): colapsar y resumir
#
# IDEA CLAVE: group_by() no cambia los datos,
# solo le dice a R "de aquí en adelante, opera
# por grupos". summarise() es quien colapsa.
############################################

# 5a. Sin agrupar: total general de todo el país, todos los años
df %>%
  summarise(total_nacional = sum(total, na.rm = TRUE))

# 5b. Agrupando por una variable
df %>%
  group_by(entidad) %>%
  summarise(total_entidad = sum(total, na.rm = TRUE))

# 5c. Agrupando por dos variables
df %>%
  group_by(entidad, ano) %>%
  summarise(total_anual = sum(total, na.rm = TRUE))

# 5d. Resumen completo por entidad (el que vamos a usar)
df_entidad <- df %>%
  group_by(entidad) %>%
  summarise(
    total_delitos = sum(total, na.rm = TRUE), #agregador sum() Suma el total por grupo
    promedio_por_tipo = mean(total, na.rm = TRUE), #agregador mean() DA el promedio por grupo
    subtipos_registrados = n_distinct(subtipo_de_delito), # agregador n_distinct() el número de valores únicos por grupo de la col indicada
    delito_mas_frecuente = subtipo_de_delito[which.max(total)], #Combo: valor más común: "Qué valor de subtipo de delito tiene el total más alto por entidad
    maximo_registrado   = max(total, na.rm = TRUE), #agregador max() valor más alto en el grupo
    ano_inicio = min(ano, na.rm = TRUE)) %>% #Agregador min() valor más bajo en el grupo
  arrange(desc(total_delitos))


############################################
# 6. Formateo para presentación
############################################

df_clean <- df_entidad %>%
  mutate(promedio_por_tipo = round(promedio_por_tipo, 1),
         total_delitos      = comma(total_delitos)) %>%
  rename(Entidad = entidad,
         'Total delitos' = total_delitos,
         'Promedio por subtipo' = promedio_por_tipo,
         'Subtipos registrados' = subtipos_registrados,
         'Delito más frecuente' = delito_mas_frecuente,
         'Máximo registrado' = maximo_registrado,
         'Año inicio' = ano_inicio)

view(df_clean)


############################################
#EXTRA
# 7. Visualización con ggplot2
############################################

# Top 10 entidades con más delitos acumulados
df_top10 <- df %>%
  group_by(entidad) %>%
  summarise(total_delitos = sum(total, na.rm = TRUE)) %>%
  slice_max(order_by = total_delitos, n = 10)

ggplot(data = df_top10,
       aes(x = total_delitos,
           y = reorder(entidad, total_delitos))) +
  geom_col(fill = "#c0392b") +
  geom_text(aes(label = comma(total_delitos)),
            hjust = -0.1, size = 3) +
  scale_x_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Top 10 estados con más delitos registrados",
       subtitle = "Carpetas de investigación",
       x        = "Total de delitos",
       y        = NULL,
       caption  = "Elaboración propia con datos del SESNSP") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))


############################################
# 8. Exportar
############################################

write.xlsx(df_clean, "03_outputs/Incidencia_por_entidad.xlsx")




###########################################
# 9. Task
###########################################


#Generar una tabla que resuma el delito de extorsión por entidad
#La tabla debe contener entidad, total acumulado 2015-2025, promedio anual,
#año con mayor incidencia y el total de delitos con esa incidencia
