

#Generar una tabla que resuma el delito de extorsión por entidad
#La tabla debe contener entidad, total acumulado 2015-2025, promedio anual,
#año con mayor incidencia y el total de delitos con esa incidencia


df_extorsion <- df %>%
  filter(subtipo_de_delito == "Extorsión") %>%
  group_by(entidad) %>%
  summarise(total_acumulado = sum(total, na.rm = TRUE),
            promedio_anual = mean(total, na.rm = TRUE),
            anio_pico = ano[which.max(total)],
            total_pico = max(total, na.rm = TRUE)) %>%
  arrange(desc(total_acumulado)) %>%
  rename('Entidad' = entidad,
         'Total acumulado 2015-2025' = total_acumulado,
         'Promedio anual' = promedio_anual,
         'Año pico' = anio_pico,
         'Total año pico' = total_pico)

#BONUS. Generar la misma tabla para todos los subtipos de la base


generar_tabla_resumen <- function(subtipo){
  
  tabla <- df %>% 
    filter(subtipo_de_delito == subtipo) %>%
    group_by(entidad) %>%
    summarise(total_acumulado = sum(total, na.rm = TRUE),
              promedio_anual = mean(total, na.rm = TRUE),
              anio_pico = ano[which.max(total)],
              total_pico = max(total, na.rm = TRUE)) %>%
    arrange(desc(total_acumulado)) %>%
    rename('Entidad' = entidad,
           'Total acumulado 2015-2025' = total_acumulado,
           'Promedio anual' = promedio_anual,
           'Año pico' = anio_pico,
           'Total año pico' = total_pico)
  
  return(tabla)
  
  
}


for (i in unique(df$subtipo_de_delito)) {
  
  tabla <- generar_tabla_resumen(i)
  
  write.xlsx(tabla, paste0("03_outputs/Tablas_Resumen/Tabla_",
                           i,
                           ".xlsx"))
  
}



