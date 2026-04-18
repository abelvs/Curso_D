pacman::p_load("tidyverse", "nycflights13", "readxl")



#Cargamos bases----

vuelos <- read_xlsx("01_datos/Sesion 3/Ejemplo_3_Vuelos_NYC.xlsx")

aerolineas <- airlines



#Inner Join (sólo las filas que tienen match en ambas tablas)

inner <- vuelos %>% 
  inner_join(aerolineas)

#Left Join (todas las filas de vuelos. Se llena con NAs donde no hay match)

left <- vuelos %>% 
  left_join(aerolineas)

#Anti Join (sólo las filas que NO tienen match en ambas tablas)

anti <- vuelos %>% 
  anti_join(aerolineas)


#Full join (todas las filas de ambas tablas)

full <- vuelos %>% 
  full_join(aerolineas)


#Semi Join (Igual a inner join, pero no agrega columnas. Se usa para filtrar)
#Las dos hacen lo mismo

semi <- vuelos %>% 
  semi_join(aerolineas)

semi <- vuelos %>% 
  filter(carrier %in% unique(aerolineas$carrier))
  