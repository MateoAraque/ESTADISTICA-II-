library(tidyverse)
library(nycflights13)
library(janitor)

#------------------------------------------------------------------------------
# ENCADENAN UNA FUNCION ->
# %>% o  >| SIRVE PARA ASIGNAR A UNA SOLA VARIABLE QUE HAGA LO SIGUIENTE 
# UNO VIENE CON R STUDIO Y OTRO CON LIBRERIA
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# 1. CARGAR Y LIMPIAR NOMBRES DE VARIABLES 
#------------------------------------------------------------------------------
vuelos <- flights %>%
  clean_names()

glimpse(vuelos)

#------------------------------------------------------------------------------
# 2. QUEDARSE CON LO UTIL | se usa: select() | PARA SELECCIONAR VARIABLES 
#------------------------------------------------------------------------------
# SIRVE PARA SELECCIONAR ALGUNAS VARIABLES DE LA BASE DE DATOS 
#------------------------------------------------------------------------------
vuelos_sel <- vuelos |>
  select(month, day, hour, carrier, origin, dest, 
         dep_delay, arr_delay, air_time, distance)

glimpse(vuelos_sel)

#------------------------------------------------------------------------------
# 3. DIAGNOSTICO DE CALIDAD | CUANTOS NA HAY EN CADA COLUMNA ? 
#------------------------------------------------------------------------------
# summarise(across()) APLICA LA MSIMA FUNCION CON VARIAS COLUMNAS A LA VEZ
# summarise(is.na(.x)) SE LEE: ( PARA CADA COLUMNA .X, CUENTA CUANTOS NA TIENE )
#------------------------------------------------------------------------------

vuelos_sel |>
  summarise(across(everything(), ~ sum(is.na(.x))))
#------------------------------------------------------------------------------
# EL RESULATDO SE MUESTAR EN UNA FILA CON CADA VARIABLE Y LA CANTIDAD DE NA 
#------------------------------------------------------------------------------
# pivot_longer() LA GIRA: A UNA FILA POR VARIABLE 
#------------------------------------------------------------------------------
faltantes <- vuelos_sel |>
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes")|>
  arrange(desc(n_faltantes))

View(faltantes) 

#------------------------------------------------------------------------------
# 4. DECIDIR Y TRANSFORMAR | filter() y mutate()
#------------------------------------------------------------------------------
# mutate() | ME PERMITE CREAR COLUMNAS EXTRA O REMPLAZA EXISTENTE Y VARIABLES 
#------------------------------------------------------------------------------
# DECISION: PARA ANALIZAR EL RETRASO DE LLEGADA, TRABAJAMOS CON LOS VUELSO QUE
# SI LEEGARON (SIN NA arr_delay). SE DOCUMENTA EN SCRIPT.
#------------------------------------------------------------------------------
vuelos_eda <-vuelos_sel |>
  filter(!is.na(arr_delay)) |>
  mutate(
    llego_tarde = arr_delay >0,                 # TRUE / FALSE
    velocidad = distance / (air_time / 60)      # MILLAS POR HORA 
)

nrow(vuelos_eda)

#------------------------------------------------------------------------------
# 5. 
#------------------------------------------------------------------------------