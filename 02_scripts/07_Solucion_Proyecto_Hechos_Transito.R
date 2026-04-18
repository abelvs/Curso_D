
pacman::p_load("tidyverse", "data.table", "readxl", "janitor",  "writexl")

options(scipen = 999)


#Cargamos data de lluvias----

catalogo_estaciones <- read_xlsx("03_Outputs/Catalogo_estaciones.xlsx")

path_lluvias <- "01_datos/Sesion 3/Lluvias_CONAGUA/"


enero <- fread("01_datos/Sesion 3/Lluvias_CONAGUA/ene.csv", encoding = "Latin-1") %>% 
  clean_names() %>% 
  filter(edo == "DF") %>% 
  select(clave, last_col())


febrero <- fread(paste0(path_lluvias, "feb.csv"), encoding = "Latin-1") %>% 
  clean_names() %>% 
  filter(edo == "DF") %>% 
  select(clave, last_col())


#Pegamos----

base_junta <- catalogo_estaciones %>% 
  left_join(enero, by = "clave") %>% 
  left_join(febrero, by = "clave")


#Generamos loop----

#Vector con lista de nombres
archivos <- list.files(path_lluvias)


for (i in archivos) {
  print(i)
}


#Creamos df sobre el cual iremos pegando valores
base_junta_loop <- catalogo_estaciones


for (i in archivos) {
  
  archivo <- fread(paste0(path_lluvias, i), encoding = "Latin-1") %>% 
    clean_names() %>% 
    select(clave, last_col())
  
  base_junta_loop <- base_junta_loop %>% 
    left_join(archivo, by = "clave")
}



#Hacemos la base tidy---

base_junta_tidy <- base_junta_loop %>% 
  select(-c(estacion, lat, lon, coords, alcaldia)) %>% 
  pivot_longer(cols = -c(clave, alcaldia_clean),
               names_to = "mes",
               values_to = "valor") %>% 
  mutate(valor = as.numeric(valor),
         mes_clean = substr(mes, 1,3),
         mes_num = case_when(mes_clean == "ene" ~ 1,
                             mes_clean == "feb" ~ 2,
                             mes_clean == "mar" ~ 3,
                             mes_clean == "abr" ~ 4,
                             mes_clean == "may" ~ 5,
                             mes_clean == "jun" ~ 6,
                             mes_clean == "jul" ~ 7,
                             mes_clean == "ago" ~ 8,
                             mes_clean == "sep" ~ 9,
                             mes_clean == "oct" ~ 10,
                             mes_clean == "nov" ~ 11,
                             mes_clean == "dic" ~ 12))

#Base colapsada por alcaldia----

lluvia_por_alcaldia_y_mes <- base_junta_tidy %>% 
  group_by(alcaldia_clean, mes_num) %>% 
  summarise(valor = mean(valor, na.rm = T)) %>% 
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

lluvia_por_mes <- base_junta_tidy %>% 
  group_by(mes_num) %>% 
  summarise(valor = mean(valor, na.rm = T)) %>% 
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))
 
#Grafiquita 
barplot(
  lluvia_por_mes$valor,
  names.arg = lluvia_por_mes$mes_num,
  xlab = "Mes",
  ylab = "Valor",
  col = "steelblue",
  main = "Precipitacion por mes en 2023"
)

#Cargamos base de hechos de tránsito----

hechos <- read_xlsx("01_datos/Sesion 3/Hechos_de_transito_clean.xlsx")


#Analisis Expoloratorio
glimpse(hechos)
summary(hechos)

#necesitamos generar una tabla por mes para comparar


tabla_hechos <- hechos %>% 
  group_by(mes,alcaldia, tipo_evento) %>% 
  tally()

#Queremos tipo de evento en columanas para pegarselo


tabla_wide_hechos <- tabla_hechos %>% 
  pivot_wider(id_cols = c(mes, alcaldia),
              names_from = tipo_evento,
              values_from = n,
              values_fill = 0) %>% 
  clean_names()


#Juntamos las dos bases para analisis----

tabla_analisis <- lluvia_por_alcaldia_y_mes %>% 
  left_join(tabla_wide_hechos, by = c("mes_num" = "mes", 
                                      "alcaldia_clean" = "alcaldia")) %>% 
  mutate(total_hechos = atropellado + caida_de_ciclista + choque + 
           derrapado + volcadura + caida_de_pasajero)



#Extra--- Analsisis y presentaicon visual



analisis_long <- tabla_analisis %>%
  pivot_longer(cols = atropellado:caida_de_pasajero,
               names_to = "tipo_hecho",
               values_to = "conteo")



ggplot(analisis_long, aes(x = valor, y = conteo)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  facet_wrap(~ tipo_hecho, ncol = 3, scales = "free_y") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")) +
  labs(title = "Precipitación vs hechos de tránsito por tipo en la CDMX (2023)",
       x = "Precipitación",
       y = "Número de incidentes",
       subtitle = "Incidentes totales y precipitación promedio por mes y alcaldía",
       caption = "Elaboración propia con datos del SMN y la SSPC-CDMX")



#Grafica de dos ejes (medio pecado, pero publicable)

# factor de escala para doble eje
max_lluvia <- max(tabla_analisis$valor, na.rm = TRUE)
max_hechos <- max(tabla_analisis$total_hechos, na.rm = TRUE)
factor_escala <- max_lluvia / max_hechos

# etiquetas de meses
mes_labels <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

# gráfico
ggplot(tabla_analisis, aes(x = mes_num)) +
  geom_col(aes(y = valor), fill = "steelblue", alpha = 0.7, na.rm = TRUE) +
  geom_line(aes(y = total_hechos * factor_escala), color = "red", linewidth = 1) +
  geom_point(aes(y = total_hechos * factor_escala), color = "red", size = 2) +
  facet_wrap(~ alcaldia_clean, ncol = 5) +
  scale_x_continuous(breaks = 1:12, labels = mes_labels) +
  scale_y_continuous(name = "Precipitación",
                     sec.axis = sec_axis(~ . / factor_escala, name = "Hechos de tránsito")) +
  labs(title = "Precipitación vs hechos de tránsito por alcaldía en 2023",
       x = "Mes",
       caption = "ELaboración propia con datos del SMN y la SSPC-CDMX") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))







