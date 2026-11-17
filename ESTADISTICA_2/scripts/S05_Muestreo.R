# =============================================================================
# ESTADÍSTICA II · Sesión 05 · Tipos de muestreo y técnicas de estimación
# Versión DOCENTE (completa, con solución del taller evaluado)
# Universidad UTE · Escuela de Tecnologías · Período 2026-2
# -----------------------------------------------------------------------------
# Idea central: tratamos los 328 521 vuelos con retraso de salida registrado
# como si fueran TODA la población. Como conocemos el valor verdadero (μ),
# podemos ver cuánto se equivoca cada tipo de muestra.
# Funciones nuevas: slice_sample(), replicate(), sqrt().
# =============================================================================

# DECLARACIÓN DE USO DE IA
# Herramienta: ninguna
# Nivel de la actividad: IANP (clase) · IAA (solo mensajes de error)
# Para qué la usé: —
# Qué hice yo: escribí y ejecuté este script en clase.

# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------
library(tidyverse)
library(nycflights13)

# -----------------------------------------------------------------------------
# 1. LA POBLACIÓN Y SU VALOR VERDADERO
# -----------------------------------------------------------------------------
poblacion <- flights |>
  filter(!is.na(dep_delay))

N     <- nrow(poblacion)                 # 328 521 vuelos
mu    <- mean(poblacion$dep_delay)       # 12,64 min  → parámetro poblacional (μ)
sigma <- sd(poblacion$dep_delay)         # 40,21 min  → desviación poblacional (σ)
N; mu; sigma

# NOTA PEDAGÓGICA: en la vida real NUNCA conocemos μ (por eso muestreamos).
# Aquí lo conocemos a propósito, para poder medir el error de cada muestra.

# -----------------------------------------------------------------------------
# 2. UNA MUESTRA ALEATORIA SIMPLE (MAS) DE 100 VUELOS
# -----------------------------------------------------------------------------
# slice_sample(n = 100) elige 100 filas al azar; todas tienen la misma
# probabilidad. set.seed() fija el azar para que todos obtengamos lo mismo.
set.seed(2026)
muestra <- poblacion |>
  slice_sample(n = 100)

media_m <- mean(muestra$dep_delay)                 # estimación puntual (x̄)
ee      <- sd(muestra$dep_delay) / sqrt(100)       # error estándar: s / √n
ic_inf  <- media_m - 1.96 * ee                     # intervalo de confianza 95 %
ic_sup  <- media_m + 1.96 * ee
c(media = media_m, ee = ee, ic_inf = ic_inf, ic_sup = ic_sup)

media_m - mu          # error de esta muestra respecto al valor verdadero
ic_inf <= mu & mu <= ic_sup   # ¿el intervalo contiene a μ?



# -----------------------------------------------------------------------------
# 3. REPETIR LA MUESTRA 200 VECES: n = 100 frente a n = 1 000
# -----------------------------------------------------------------------------
# replicate(200, { … }) ejecuta 200 veces el bloque entre llaves y guarda los
# resultados. Cada repetición devuelve la media y el error estándar.
set.seed(2026)
rep_100 <- replicate(200, {
  m <- slice_sample(poblacion, n = 100)
  c(media = mean(m$dep_delay), ee = sd(m$dep_delay) / sqrt(100))
})
set.seed(2026)
rep_1000 <- replicate(200, {
  m <- slice_sample(poblacion, n = 1000)
  c(media = mean(m$dep_delay), ee = sd(m$dep_delay) / sqrt(1000))
})

# replicate() devuelve una matriz de 2 filas × 200 columnas: la giramos con
# t() y la convertimos en tabla.
sim_100  <- as_tibble(t(rep_100))  |> mutate(diseno = "MAS n = 100")
sim_1000 <- as_tibble(t(rep_1000)) |> mutate(diseno = "MAS n = 1 000")

# -----------------------------------------------------------------------------
# 4. MUESTREO ESTRATIFICADO POR AEROPUERTO DE ORIGEN (n ≈ 1 000)
# -----------------------------------------------------------------------------
# Cada aeropuerto es un ESTRATO. group_by(origin) + slice_sample(prop = …)
# toma la misma proporción de cada estrato (asignación proporcional).
poblacion |>
  group_by(origin) |>
  summarise(vuelos = n(), media = mean(dep_delay), desv = sd(dep_delay))
# EWR 117 596 · 15,1 | JFK 109 416 · 12,1 | LGA 101 509 · 10,3

set.seed(2026)
rep_estr <- replicate(200, {
  m <- poblacion |>
    group_by(origin) |>
    slice_sample(prop = 1000 / N) |>
    ungroup()
  c(media = mean(m$dep_delay), ee = sd(m$dep_delay) / sqrt(nrow(m)))
})
sim_estr <- as_tibble(t(rep_estr)) |> mutate(diseno = "Estratificado n ≈ 1 000")

# -----------------------------------------------------------------------------
# 5. MUESTREO POR CONGLOMERADOS: DÍAS COMPLETOS 
# -----------------------------------------------------------------------------
# Cada DÍA es un conglomerado. Se eligen 3 días al azar y se toman TODOS sus
# vuelos (≈ 2 700). Es barato (se visitan pocos días), pero los vuelos de un
# mismo día se parecen entre sí (mismo clima, misma congestión).
dias <- poblacion |>
  distinct(month, day)

set.seed(2026)
rep_cong <- replicate(200, {
  elegidos <- slice_sample(dias, n = 3)
  m <- poblacion |>
    semi_join(elegidos, by = c("month", "day"))     # vuelos de esos 3 días
  c(media = mean(m$dep_delay), ee = sd(m$dep_delay) / sqrt(nrow(m)))
})
sim_cong <- as_tibble(t(rep_cong)) |> mutate(diseno = "Conglomerados: 3 días (≈ 2 700)")



# -----------------------------------------------------------------------------
# 6. TABLA COMPARATIVA 
# -----------------------------------------------------------------------------
comparacion <- bind_rows(sim_100, sim_1000, sim_estr, sim_cong) |>
  mutate(cubre = (media - 1.96 * ee) <= mu & mu <= (media + 1.96 * ee)) |>
  group_by(diseno) |>
  summarise(
    media_de_estimaciones = mean(media),
    error_medio           = mean(abs(media - mu)),   # cuánto se equivoca en promedio
    desv_estimaciones     = sd(media),               # error estándar observado
    ee_promedio           = mean(ee),                # error estándar estimado (s/√n)
    cobertura_ic95        = mean(cubre) * 100        # % de intervalos que contienen μ
  )

print(comparacion, width = Inf)
mu   # valor verdadero, para comparar

# -----------------------------------------------------------------------------
# 7. VER EL ERROR MUESTRAL: distribución de las 200 medias
# -----------------------------------------------------------------------------
bind_rows(sim_100, sim_1000) |>
  ggplot(aes(x = media)) +
  geom_histogram(binwidth = 0.5, fill = "#09509E") +
  geom_vline(xintercept = mu, colour = "#C2661C", linewidth = 1) +
  facet_wrap(~ diseno, ncol = 1) +
  labs(title = "Medias de 200 muestras: n = 100 frente a n = 1 000",
       subtitle = "La línea naranja es el valor verdadero (μ = 12,64 min)",
       x = "Media muestral del retraso de salida (min)", y = "Muestras")

# -----------------------------------------------------------------------------
# 8. INTERPRETACIÓN 
# -----------------------------------------------------------------------------
# - Al pasar de n = 100 a n = 1 000 el error medio baja de 3,2 a 1,1 min: a la
#   tercera parte, no a la décima, porque el error estándar depende de 1/√n
#   (√10 ≈ 3,2).
# - Cerca del 95 % de los intervalos contienen a μ (91,5 % con n = 100;
#   95,5 % con n = 1 000; 95 % estratificado): así se interpreta el «95 % de
#   confianza».
# - Estratificar por origen mejora muy poco la precisión (error medio 0,98
#   frente a 1,09 min), porque las medias de los tres aeropuertos son
#   parecidas (15,1 · 12,1 · 10,3) frente a una desviación de 40 min.
#   Estratificar ayuda cuando los estratos son distintos entre sí y parecidos
#   por dentro.
# - Con conglomerados (días completos) el error es mucho mayor aun con más
#   vuelos (5,3 min; solo el 11 % de los intervalos contiene a μ): los vuelos
#   de un mismo día se parecen entre sí, así que aportan menos información
#   nueva.