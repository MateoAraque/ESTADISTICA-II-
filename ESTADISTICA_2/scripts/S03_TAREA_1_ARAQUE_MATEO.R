# =============================================================================
# ESTADÍSTICA II · Sesión 03 · Estadística descriptiva y exploratoria
#                              en contextos de Big Data
# Versión ESTUDIANTE · Universidad UTE · Escuela de Tecnologías · 2026-2
# -----------------------------------------------------------------------------
# Propósito: elegir y calcular el resumen adecuado según el tipo de variable
# sobre un conjunto de datos real (nycflights13::flights).
# Complete los espacios ____ durante la clase y el laboratorio.
# Guarde este archivo como scripts/S03_descriptiva.R y súbalo al portafolio
# de la Unidad 1 al terminar.
# =============================================================================

# Nombre: Mateo Araque

# DECLARACIÓN DE USO DE IA
# Herramienta: NA
# Nivel de la actividad: IAA (solo para depurar errores de código)
# Para qué la usé : NO USE IA  
# Qué hice yo: Todo el trabajo 
# Si no usó IA, escriba: «No usé IA en esta actividad».

# -----------------------------------------------------------------------------
# 0. PAQUETES Y DATOS
# -----------------------------------------------------------------------------
library(tidyverse)
library(nycflights13)
library(skimr)

vuelos <- flights

# -----------------------------------------------------------------------------
# 1. ¿QUÉ TIPO DE VARIABLE ES?
# -----------------------------------------------------------------------------
glimpse(vuelos)

# Clasifique (escriba el tipo al lado):
# arr_delay  → dbl
# distance   → dbl
# carrier    → chr
# origin     → chr
# month      → int
# flight     → int

# ¿Tiene sentido esta media? Explique en un comentario.
mean(vuelos$flight)
# Respuesta: No ya que existen datos muy extremos y haces que la media no sea
# normal

skim(vuelos)   # resumen rápido de todo el conjunto

# -----------------------------------------------------------------------------
# 2. TENDENCIA CENTRAL
# -----------------------------------------------------------------------------
mean(vuelos$arr_delay)                   # ¿qué devuelve? ¿por qué?
# DEVUELVE NA YA QUE ECISTEN VALORES FALTANTES OSEA NA 
mean(vuelos$arr_delay, na.rm = TRUE)     # media
median(vuelos$arr_delay, na.rm = TRUE)   # mediana

# Interprete: ¿qué dice la media y qué dice la mediana del retraso?
# Respuesta: La media intempreta que los vuelos apoximadamente tienen un
# retraso de 6.9 minutos, y la mediana me indica que el vuelo llega 5 minutos
# antes por eso el valor -5

# Moda de una variable cualitativa: la categoría más frecuente
vuelos %>%
  count(carrier, sort = TRUE)

vuelos %>%
  count(dest, sort = TRUE)               # moda del destino (dest)

# -----------------------------------------------------------------------------
# 3. DISPERSIÓN
# -----------------------------------------------------------------------------
range(vuelos$arr_delay, na.rm = TRUE)    # mínimo y máximo
sd(vuelos$arr_delay, na.rm = TRUE)       # desviación estándar
quantile(vuelos$arr_delay, na.rm = TRUE) # cuartiles
IQR(vuelos$arr_delay, na.rm = TRUE)     # rango intercuartílico (IQR)

# -----------------------------------------------------------------------------
# 4. FORMA
# -----------------------------------------------------------------------------

# ¿Es positivo o negativo? ¿Hacia qué lado está la cola?
# Respuesta: TIENE UNA ASIMETRIA POSITIVA CON LA COLA HACIA LA DERECHA
# YA QUE LA MEDIA ES MAYOR QUE LA MEDIANA 

# -----------------------------------------------------------------------------
# 5. TODO EN UNA TABLA: summarise()
# -----------------------------------------------------------------------------
vuelos %>%
  summarise(
    vuelos   = n(),
    media    = mean(arr_delay, na.rm = TRUE),
    mediana  = median(arr_delay, na.rm = TRUE),
    desv_est = sd(arr_delay, na.rm = TRUE),
    riq      = IQR(arr_delay, na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# 6. RESÚMENES POR GRUPO: group_by() + summarise()  (ejemplo resuelto)
# -----------------------------------------------------------------------------
retraso_aerolinea <- vuelos %>%
  group_by(carrier) %>%
  summarise(
    vuelos  = n(),
    media   = mean(arr_delay, na.rm = TRUE),
    mediana = median(arr_delay, na.rm = TRUE)
  ) %>%
  arrange(desc(media))

retraso_aerolinea

# =============================================================================
# 7. LABORATORIO (45 min) · nivel de IA: IAA (solo para depurar errores)
# =============================================================================
# Tarea 1. Retraso de llegada por MES: n, media, mediana y desviación estándar.
retraso_mes <- vuelos %>%
  group_by(month) %>%
  summarise(
    vuelos   = n(),
    media    = mean(arr_delay, na.rm = TRUE),
    mediana  = median(arr_delay, na.rm = TRUE),
    desv_est = sd(arr_delay, na.rm = TRUE)
  )
retraso_mes

# Tarea 2. Ordene los meses de mayor a menor retraso medio.
retraso_mes %>%
  arrange(desc(media))

# Tarea 3. Retraso de llegada por AEROPUERTO DE ORIGEN (origin):
# n, media y mediana. Escriba el código completo.
vuelos %>%
  group_by(origin) %>%
  summarise(
    vuelos  = n(),
    media   = mean(arr_delay, na.rm = TRUE),
    mediana = median(arr_delay, na.rm = TRUE)
  )

# Tarea 4. (Reto) Retraso de SALIDA (dep_delay) por aerolínea, ordenado de
# mayor a menor. ¿Coincide el orden con el de llegada?
vuelos %>%
  group_by(carrier) %>%
  summarise(
    media_salida   = mean(dep_delay, na.rm = TRUE),
    mediana_salida = median(dep_delay, na.rm = TRUE)
  ) %>%
  arrange(desc(media_salida))

# -----------------------------------------------------------------------------
# PREGUNTAS PARA LA PUESTA EN COMÚN (responda en comentarios)
# -----------------------------------------------------------------------------

# a) ¿Por qué la media es mayor que la mediana en casi todos los grupos?

# Respuesta: DEBIDO A QUE LOS DATOS TIENEN VALORES EXTREMOS O MUY GRANDES O 
# MUY PEQUEÑOS

# b) Si tuviera que reportar UN número del «retraso típico» por mes, ¿cuál
#    usaría y por qué?

# Respuesta: USARIA LA MEDIANA YA QUE NO SE VE AFECTADA POR LOS VALORES 
# EXTREMOS O ATIPICOS Y REPRESENTA DE MEJOR MANERA QUE LA MEDIA

# c) ¿Qué resumen usaría para describir origin o carrier por sí solas?

# Respuesta:SE USARIA LA MODA Y PORCENTAJES YA QUE SON VARIABLES CUALITATIVAS
# Y NO TENDRIA SENTIDO CALCULAR MEDIAS NI MEDIANAS

