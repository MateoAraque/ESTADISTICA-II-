# =============================================================================
# ESTADÍSTICA II · Sesión 03 · Estadística descriptiva y exploratoria
#                              en contextos de Big Data
# Versión DOCENTE (completa, con solución del laboratorio)
# Universidad UTE · Escuela de Tecnologías · Período 2026-2
# -----------------------------------------------------------------------------
# Propósito: elegir y calcular el resumen adecuado según el tipo de variable
# sobre un conjunto de datos real (nycflights13::flights, 336 776 vuelos).
# Secuencia: tipos de variables → tendencia central → dispersión → forma →
#            resúmenes por grupo con group_by() y summarise() → laboratorio.
# =============================================================================

# DECLARACIÓN DE USO DE IA
# Herramienta: ninguna
# Nivel de la actividad: IANP (programación en vivo) · IAA en el laboratorio

# -----------------------------------------------------------------------------
# 0. PAQUETES Y DATOS
# -----------------------------------------------------------------------------
# Si falta alguno, instálelo en la CONSOLA (no en el script):
# install.packages(c("tidyverse", "nycflights13", "skimr"))
library(tidyverse)
library(nycflights13)
library(skimr)

vuelos <- flights

# -----------------------------------------------------------------------------
# 1. ¿QUÉ TIPO DE VARIABLE ES? (antes de resumir, hay que clasificar)
# -----------------------------------------------------------------------------
glimpse(vuelos)

# Cuantitativas continuas: arr_delay, dep_delay, air_time, distance
# Cuantitativas discretas: conteos (por ejemplo, vuelos por día)
# Cualitativas nominales:  carrier, origin, dest, tailnum
# Cualitativas ordinales:  month (enero < febrero < …) usada como categoría
#
# Algunos números son en realidad CATEGORÍAS (códigos).
# flight es el número de vuelo: un identificador, no una cantidad.
mean(vuelos$flight)
# 1971.924  … ¿qué significa «el vuelo promedio número 1972»? ¡Nada!
# Resumen rápido de todo el conjunto: tipo, faltantes, media, cuartiles
skim(vuelos)

# -----------------------------------------------------------------------------
# 2. TENDENCIA CENTRAL: ¿cuál es el valor «típico»?
# -----------------------------------------------------------------------------
# Primer intento: la media del retraso de llegada
mean(vuelos$arr_delay)
# → NA. ¿Por qué? Hay 9 430 vuelos sin retraso registrado (sesión 02).
# Basta un NA para que el resultado sea NA: R no adivina.

# na.rm = TRUE («NA remove») le pide a R que ignore los faltantes
mean(vuelos$arr_delay, na.rm = TRUE)     # 6.895  → media
median(vuelos$arr_delay, na.rm = TRUE)   # -5     → mediana

# La media dice que un vuelo llega en promedio 6,9 min TARDE; la mediana, que el vuelo típico
# llega 5 min ANTES. ¿Cuál describe mejor «el vuelo de todos los días»?

# Moda: el valor más frecuente. Útil sobre todo en variables cualitativas.
# count() cuenta filas por categoría; sort = TRUE ordena de mayor a menor.
vuelos %>%
  count(carrier, sort = TRUE)            # moda de carrier: UA (58 665)

vuelos %>%
  count(dest, sort = TRUE)               # moda de dest: ORD (17 283)

# -----------------------------------------------------------------------------
# 3. DISPERSIÓN: ¿qué tan distintos son los vuelos entre sí?
# -----------------------------------------------------------------------------
min(vuelos$arr_delay, na.rm = TRUE)      # -86   (llegó 86 min antes)
max(vuelos$arr_delay, na.rm = TRUE)      # 1272  (¡21 horas tarde!)
range(vuelos$arr_delay, na.rm = TRUE)    # los dos a la vez

var(vuelos$arr_delay, na.rm = TRUE)      # 1992  (en minutos al cuadrado)
sd(vuelos$arr_delay, na.rm = TRUE)       # 44.6  (en minutos: se interpreta)

quantile(vuelos$arr_delay, na.rm = TRUE) # cuartiles: -86, -17, -5, 14, 1272
IQR(vuelos$arr_delay, na.rm = TRUE)      # 31 = Q3 - Q1 = 14 - (-17)

# La desviación estándar (44,6) se infla con los
# retrasos extremos; el rango intercuartílico (31) describe el 50 % central
# y no le afectan los extremos. Misma lógica que media vs. mediana.

# -----------------------------------------------------------------------------
# 4. FORMA: ¿la distribución es simétrica?
# -----------------------------------------------------------------------------
# Regla práctica: si la media es mucho mayor que la mediana, hay una cola
# larga hacia la derecha (asimetría positiva): unos pocos valores muy
# grandes «jalan» la media.

# Para VER la forma (ggplot2 se trabaja en la sesión 04; aquí solo se observa)
ggplot(vuelos, aes(x = arr_delay)) +
  geom_histogram(binwidth = 5, fill = "#09509E", colour = "white", na.rm = TRUE) +
  coord_cartesian(xlim = c(-90, 240)) +
  labs(title = "Distribución del retraso de llegada",
       x = "Retraso de llegada (minutos)", y = "Número de vuelos") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 5. TODO EN UNA TABLA: summarise()
# -----------------------------------------------------------------------------
# summarise() reduce toda la tabla a UNA fila con los resúmenes que pidamos.
vuelos %>%
  summarise(
    vuelos   = n(),                                  # cuántas filas
    media    = mean(arr_delay, na.rm = TRUE),
    mediana  = median(arr_delay, na.rm = TRUE),
    desv_est = sd(arr_delay, na.rm = TRUE),
    riq      = IQR(arr_delay, na.rm = TRUE),
    minimo   = min(arr_delay, na.rm = TRUE),
    maximo   = max(arr_delay, na.rm = TRUE)
  )

# -----------------------------------------------------------------------------
# 6. RESÚMENES POR GRUPO: group_by() + summarise()
# -----------------------------------------------------------------------------
# group_by() separa la tabla en grupos (uno por aerolínea) y summarise()
# calcula los resúmenes DENTRO de cada grupo: una fila por aerolínea.
retraso_aerolinea <- vuelos %>%
  group_by(carrier) %>%
  summarise(
    vuelos  = n(),
    media   = mean(arr_delay, na.rm = TRUE),
    mediana = median(arr_delay, na.rm = TRUE),
    desv_est = sd(arr_delay, na.rm = TRUE)
  ) %>%
  arrange(desc(media))          # arrange(desc()) ordena de mayor a menor

retraso_aerolinea
# F9 (Frontier): media 21,9 · mediana 6   → la peor en ambos
# EV (ExpressJet): media 15,8 · mediana -1 → media alta, vuelo típico puntual
# AS (Alaska): media -9,9 · mediana -17   → llega antes, en ambos

# NOTA PEDAGÓGICA: pregunte por EV: ¿cómo puede tener una media de 15,8 min
# si su vuelo típico llega 1 min antes? (pocos vuelos con retrasos enormes).
# Y por OO: solo 32 vuelos; ¿confiaría en su promedio?





































# =============================================================================
# 7. LABORATORIO (45 min) · SOLUCIÓN DE REFERENCIA
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
# Peores meses por media: julio (16,7), junio (16,5), diciembre (14,9)
# Mejor mes: septiembre (media -4,0, mediana -12)
# Diciembre es el ÚNICO mes con mediana positiva (+2): el vuelo típico llega tarde.

# Tarea 2. Ordene los meses de mayor a menor retraso medio.
retraso_mes |>
  arrange(desc(media))

# Tarea 3. Retraso de llegada por AEROPUERTO DE ORIGEN.
vuelos |>
  group_by(origin) |>
  summarise(
    vuelos  = n(),
    media   = mean(arr_delay, na.rm = TRUE),
    mediana = median(arr_delay, na.rm = TRUE)
  )
# EWR: media 9,1 · mediana -4 | JFK: 5,6 · -6 | LGA: 5,8 · -5

# Tarea 4. (Reto) Retraso de SALIDA (dep_delay) por aerolínea: ¿coincide el
# orden con el de llegada?
vuelos |>
  group_by(carrier) |>
  summarise(
    media_salida   = mean(dep_delay, na.rm = TRUE),
    mediana_salida = median(dep_delay, na.rm = TRUE)
  ) |>
  arrange(desc(media_salida))
# F9, EV y YV siguen arriba: los retrasos de llegada se explican en buena
# parte por los de salida.

# Preguntas para la puesta en común (respuestas esperadas):
# a) ¿Por qué la media es mayor que la mediana en casi todos los grupos?
#    → Asimetría positiva: pocos retrasos muy grandes jalan la media.
# b) Si tuviera que reportar UN número del «retraso típico» por mes, ¿cuál?
#    → La mediana; la media informa del costo total de los retrasos.
# c) ¿Qué resumen usaría para origin o carrier por sí solos?
#    → Conteos y la moda (count), no media ni mediana.

# -----------------------------------------------------------------------------
# 8. CIERRE
# -----------------------------------------------------------------------------
# Pregunta: si la media de retrasos es mucho mayor que la mediana, ¿qué le
# dice eso sobre la distribución? → que es asimétrica a la derecha: la
# mayoría de vuelos llega a tiempo y unos pocos llegan muy tarde.
