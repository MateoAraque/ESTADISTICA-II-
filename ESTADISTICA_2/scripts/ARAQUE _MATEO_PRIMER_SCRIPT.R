# =============================================================================
# ESTADÍSTICA II · Sesión 01 · Mi primer script reproducible
# Versión ESTUDIANTE · Universidad UTE · Período 2026-2
# -----------------------------------------------------------------------------
# Propósito: crear un proyecto ordenado en RStudio, leer un conjunto de datos
# grande y dar una primera mirada a su tamaño y estructura.
# Datos: nycflights13::flights (vuelos de Nueva York, 2013).
#
# Complete los espacios ____ mientras seguimos la demostración.
# Guarde este archivo como scripts/S01_primer_script.R dentro de su proyecto.
# =============================================================================

# Nombre: Mateo Araque

# DECLARACIÓN DE USO DE IA
# Herramienta: No se realizo uso de IA
# Nivel de la actividad: IANP (práctica en clase)
# Para qué la usé: ..
# Qué hice yo: ..
# Si no usó IA, escriba: «No usé IA en esta actividad».

# -----------------------------------------------------------------------------
# 0. ANTES DE EMPEZAR (una sola vez, en la CONSOLA, no en el script)
# -----------------------------------------------------------------------------
# install.packages(c("tidyverse", "nycflights13"))
#
# Estructura del proyecto (File › New Project › New Directory › estadistica2):
#   estadistica2/
#   ├── estadistica2.Rproj
#   ├── datos/        ← datos crudos: se leen, nunca se editan a mano
#   ├── scripts/      ← este archivo
#   └── resultados/   ← tablas y gráficos que genera el código

# -----------------------------------------------------------------------------
# 1. PAQUETES (siempre al inicio)
# -----------------------------------------------------------------------------
library(tidyverse)     # leer, transformar y graficar datos
library(nycflights13)  # trae la tabla flightsº
# -----------------------------------------------------------------------------
# 2. DATOS
# -----------------------------------------------------------------------------
# La flecha <- guarda lo de la derecha con el nombre de la izquierda.
vuelos <- flights

# -----------------------------------------------------------------------------
# 3. PRIMERA MIRADA: ¿qué tan grande es y qué contiene?
# -----------------------------------------------------------------------------
dim(vuelos)       # filas y columnas
nrow(vuelos)    # solo filas
ncol(vuelos)    # solo columnas

glimpse(vuelos) # cada variable, su tipo y sus primeros valores
# <int> entero · <dbl> decimal · <chr> texto · <dttm> fecha y hora

head(vuelos)    # las primeras 6 filas

# Responda en un comentario: ¿cuántos vuelos hay? ¿cuántas variables?
# Respuesta: Hay un total de 336776 vuelo y 19 variables en la base de datos.

# -----------------------------------------------------------------------------
# 4. UNA PRIMERA PREGUNTA A LOS DATOS
# -----------------------------------------------------------------------------
# count() cuenta cuántas filas hay por cada valor de una variable.
# %>% se lee «y luego»: toma vuelos y luego cuenta por origen.

# ¿Desde qué aeropuerto salen más vuelos?
vuelos %>% 
  count(origin)

# ¿Qué aerolínea (carrier) tiene más vuelos? sort = TRUE ordena de mayor a menor.
vuelos %>%
  count(carrier, sort = TRUE)

# RETO: ¿a qué destino (dest) van más vuelos?
vuelos %>%
  count(dest, sort = TRUE)
# Respuesta: Existen mas vuelos con destino a ORD

# -----------------------------------------------------------------------------
# 5. GUARDAR EL RESULTADO (flujo reproducible)
# -----------------------------------------------------------------------------
vuelos_por_origen <- vuelos %>%
  count(origin)

write_csv(vuelos_por_origen, "resultados/vuelos_por_origen.csv")
# Si aparece «cannot open file», revise que exista la carpeta resultados/
# y que el proyecto esté abierto (esquina superior derecha de RStudio).

# -----------------------------------------------------------------------------
# 6. LA REGLA DE ORO
# -----------------------------------------------------------------------------
# Session › Restart R y luego ejecute TODO el script (Ctrl + Shift + Enter).
# Si llega al final sin errores y vuelve a crear el archivo de resultados,
# su análisis es reproducible.

