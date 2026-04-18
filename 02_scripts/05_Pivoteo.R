pacman::p_load("tidyverse")

#Cargar df


df_mtcars <- mtcars %>% 
  rownames_to_column(var = "car") %>% 
  select(car, mpg, cyl, hp, wt)

#Funciones de exploración






#Pivot Longer----
#Convertimos las colunas de medidas en filas

?pivot_longer

mtcars_long <- df_mtcars %>% 
  pivot_longer(cols = -c(car),
               names_to = "variable",
               values_to = "valor")


#Pivot Wider----
#Usamos valores unicos de una columna para hacer nuevas

mtcars_wide <- mtcars_long %>% 
  pivot_wider(names_from = "variable",
              values_from = "valor")
