# =============================================================================
# ESTADÍSTICA II · Sesión 04 · Análisis exploratorio de datos (EDA) con tidyverse
# Versión ESTUDIANTE · Universidad UTE · Escuela de Tecnologías · 2026-2
# -----------------------------------------------------------------------------
# Propósito: aplicar el flujo completo de análisis exploratorio y comunicar
# hallazgos respaldados por gráficos.
# Complete los espacios ____ durante la demostración y el laboratorio.
# Guarde este archivo como scripts/S04_EDA.R y súbalo al portafolio de la
# Unidad 1 junto con sus tres hallazgos.
# =============================================================================

# Nombre: Mateo Araque

# DECLARACIÓN DE USO DE IA
# Herramienta: CLAUDE
# Nivel de la actividad: IAA (solo para depurar errores de código)
# Para qué la usé: PARA CORREGIR CODIGO Y EXPLICACION DE VARIOS CONCEPTOS 
# Qué hice yo: GRAFICAS Y RAZONAMIENTO 

# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------
library(tidyverse)
library(nycflights13)
library(janitor)


# =============================================================================
# 1. LABORATORIO · nivel de IA: IAA (solo para depurar errores)
# =============================================================================
# Haga al menos TRES gráficos que respondan preguntas DISTINTAS.
# Para cada uno: 1) escriba la pregunta en el título; 2) elija el geom
# adecuado; 3) anote debajo lo que observa (con sus palabras).
# Ideas de preguntas (o proponga las suyas):
#   - ¿En qué mes llegan tarde más vuelos?
#   - ¿Qué aerolíneas grandes se retrasan más?
#   - ¿El retraso de salida explica el de llegada?
#   - ¿A qué destinos se vuela más desde cada aeropuerto?

# =============================================================================
# 1. CARGAR Y LIMPIAR NOMBRES DE VARIABLES 
# =============================================================================
vuelos <- flights %>%
  clean_names()

glimpse(vuelos)

# =============================================================================
# 2. QUEDARSE CON LO UTIL | se usa: select() | PARA SELECCIONAR VARIABLES 
# =============================================================================

vuelos_sel <- vuelos |>
  select(month, day, hour, carrier, origin, dest, 
         dep_delay, arr_delay, air_time, distance)

glimpse(vuelos_sel)

# =============================================================================
# 3. DIAGNOSTICO DE CALIDAD | CUANTOS NA HAY EN CADA COLUMNA ? 
# =============================================================================

vuelos_sel |>
  summarise(across(everything(), ~ sum(is.na(.x))))

faltantes <- vuelos_sel |>
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes")|>
  arrange(desc(n_faltantes))

View(faltantes) 

# =============================================================================
# 4. DECIDIR Y TRANSFORMAR | filter() y mutate()
# =============================================================================
vuelos_eda <-vuelos_sel |>
  filter(!is.na(arr_delay)) |>
  mutate(
    llego_tarde = arr_delay >0,                 
    velocidad = distance / (air_time / 60)      
  )

nrow(vuelos_eda)

# Gráfico A
# Pregunta: ¿En qué mes llegan tarde más vuelos?
grafico_a <- vuelos_eda |>
  filter(llego_tarde == TRUE) |>
  ggplot(aes(x = month)) +
           geom_bar(fill = "steelblue")+
           labs(
             title = "¿En qué mes llegan tarde más vuelos?",
             x = "Mes",
             Y = "Retraso llegada"
           )
grafico_a
# Observación: Se puede observar que en el mes de diciembre hay un porcentaje ams alto de vuelos atrasados 

# Gráfico B
# Pregunta:¿Qué aerolíneas grandes se retrasan más?
grafico_b <- vuelos_eda |>
  filter(llego_tarde == TRUE) |>
  ggplot(aes(x = reorder(carrier, arr_delay, FUN = median), y = arr_delay)) +
  geom_boxplot(outlier.shape = NA, fill = "coral") +
  coord_cartesian(ylim = c(0, 120)) +
  labs(
    title = "¿Qué aerolíneas se retrasan mas?",
    x = "Aerolínea",
    y = "Retraso de llegada"
  )
grafico_b
# Observación: Gracias al grafico se logra observar que la aerolinea OO tiene mayor tiempo de retras y 
# y la aerolinea HA tiene menor tiempo de retraso 

# Gráfico C
# Pregunta:¿A qué destinos principales se vuela más desde cada aeropuerto de origen?
grafico_c <- vuelos_eda |>
  filter(dest %in% c("ORD", "ATL", "LAX", "BOS", "MCO")) |>
  ggplot(aes(x = dest, fill = origin)) +
  geom_bar(position = "dodge") +
  labs(
    title = "¿A qué destinos principales se vuela más desde cada aeropuerto?",
    x = "Aeropuerto de Destino",
    y = "Cantidad de Vuelos",
    fill = "Origen"
  ) 

grafico_c
# Observación: Se puede observar que el aeropueto LAX concentra la mayor cantidad de vuelos
# hacia JFK.

# -----------------------------------------------------------------------------
# 7. MIS TRES HALLAZGOS
# -----------------------------------------------------------------------------
# Un hallazgo es UNA frase, con UNA cifra, que alguien podría usar para
# decidir, y que se ve en su gráfico.
# Hallazgo 1 (gráfico A): En diciembre se observa que es el pico mas alto devuelos que llegan tarde,
# y que en septiembre es el mes en el que menos vuelos atrasados hay.
# Hallazgo 2 (gráfico B): Se registro que la aerolina OO ha tenido la mayor cantidad de retraso,
# y la aerolinea es la que menor cantidad de retraso tiene 
# Hallazgo 3 (gráfico C): Se observa que los destinos ORD Y LAX tienen una mayor cantidad de vuelos
