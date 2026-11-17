# =============================================================================
# ESTADÍSTICA II · Evaluación de la Unidad 1 (sesiones 01 a 06) · Parte práctica
# Universidad UTE · Escuela de Tecnologías · Período 2026-2
# -----------------------------------------------------------------------------
# Nombre: Mateo Gabriel Araque Basantes
# Tiempo: 40 minutos · Individual · Nivel de IA: IANP (sin IA ni ayuda de
# terceros). Puede consultar sus scripts de las sesiones 03 a 06.
# Datos: delivery_quito_clientes.csv
# Escriba sus interpretaciones como comentarios (#) debajo de cada resultado.
# Entregue este archivo .R en la tarea «Evaluación U1 – parte práctica».
# =============================================================================

library(tidyverse)
library(janitor)

# -----------------------------------------------------------------------------
# 1. PREPARACIÓN Y DIAGNÓSTICO
#    Lea la base, limpie los nombres, cuente los faltantes por variable,
#    y cree la base limpia.
# -----------------------------------------------------------------------------
glimpse(delivery_quito_clientes)

delivery <- delivery_quito_clientes %>%
  clean_names()

names(delivery)

# FALTANTES POR VARIABLE 

faltantes <- delivery %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>% 
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "n_faltantes")

View(faltantes)

# -----------------------------------------------------------------------------
# 2. ESTADÍSTICA DESCRIPTIVA POR GRUPOS
#    Tiempo de entrega por zona: número de clientes, media, mediana y
#    desviación estándar, y un diagrama de caja. Interprete.
# -----------------------------------------------------------------------------

# ELIMINA VALORES COMO 999 EN TIEMPO ENTREGA MIN

delivery_tiempo <- delivery %>% 
  filter(tiempo_entrega_min != 999)

# TIEMPO DE ENTREGA POR ZONA  

tabla_zona <- delivery_tiempo %>%
  group_by(zona) %>%
  summarise(
    n_clientes_zona  = n(),
    media_zona       = mean(tiempo_entrega_min),
    mediana_zona     = median(tiempo_entrega_min),
    desv_est_zona    = sd(tiempo_entrega_min)
  )

View(tabla_zona)

# DIAGRAMA DE CAJA 

ggplot(delivery_tiempo, aes(x = zona, y = tiempo_entrega_min)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Tiempo de entrega por zona)",
       x = "Zona", y = "Tiempo de entrega en min") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 3. MUESTREO Y ESTIMACIÓN: DOS DISEÑOS
#    a) Calcule la media verdadera del gasto (μ) en la base limpia.
#    b) Con set.seed(2026), tome una muestra aleatoria simple de 100 clientes
#       y una muestra estratificada por zona (prop = 100 / número de clientes).
#       Para la aleatoria simple, calcule el IC 95 % (media ± 1,96 · s/√n).
#       Calcule el error de cada muestra (media − μ).
#    c) Repita cada diseño 200 veces con replicate() y compare el error medio:
#       mean(abs(medias − μ)). ¿Qué diseño tiene menos error? ¿Por qué?
# -----------------------------------------------------------------------------

#  A: SE REALIZA LA MEDIDA VERDADERA EN LA BASE LIMPIA 

medida_verdadera <- mean(delivery$gasto_mensual_usd)

round(medida_verdadera,2)


# B: MUESTRA ALEATORIA 100 CLIENTES
set.seed(2026)

# MUESTREO ALEATORIO n = 100

m_muestras <- delivery %>% 
  slice_sample(n = 100)

media_muestras <- mean(m_muestras$gasto_mensual_usd)
sd_muestras <- sd(m_muestras$gasto_mensual_usd)
ic_muestras <- media_muestras + c(-1, 1) * 1.96 * sd_muestras / sqrt(100)   
error_muestras <- media_muestras - medida_verdadera

# MUESTRES ESTRATIFICADO POR ZONA PROP = 100 / 600
muestreo_estr <- delivery %>%
  group_by(zona) %>%
  slice_sample(prop = 100 / nrow(delivery)) %>%
  ungroup()
media_estr <- mean(muestreo_estr$gasto_mensual_usd)
error_estr <- media_estr - medida_verdadera

media_muestras ; ic_muestras ; error_muestras
media_estr ; error_estr ; nrow(muestreo_estr)

#  C: REPETIR 200 Y COMPARAR ERROR

set.seed(2026)

medias_aleatoria <- replicate(200, mean(slice_sample
                                        (delivery, n = 100)$gasto_mensual_usd))
medias_estratificado <- replicate(200, {
  delivery %>%
    group_by(zona) %>%
    slice_sample(prop = 100 / nrow(delivery)) %>%
    ungroup() %>%
    summarise(m = mean(gasto_mensual_usd)) %>%
    pull(m)
})

error_medio_aleatorio <- mean(abs(medias_aleatoria - medida_verdadera))
error_medio_estratificado <-mean(abs(medias_estratificado - medida_verdadera))   

round(error_medio_aleatorio,2)
round(error_medio_estratificado,2)


# -----------------------------------------------------------------------------
# 4. CORRELACIÓN Y REGRESIÓN
#    ¿Qué variable se relaciona mejor con el gasto mensual: pedidos_mes o edad?
#    Ajuste gasto ~ pedidos_mes, interprete la pendiente y el R², agregue la
#    edad y decida con el R² ajustado si conviene mantenerla.
# -----------------------------------------------------------------------------

# CORRELACIONES DEL GASTO CON CADA VARIABLE 

cor(delivery$gasto_mensual_usd, delivery$pedidos_mes)
cor(delivery$gasto_mensual_usd, delivery$edad)

# GASTO - PEDIDO_MES
m1 <- lm(gasto_mensual_usd ~ pedidos_mes, data = delivery)
summary(m1)

# AGRGANDO EDAD
m2 <- lm(gasto_mensual_usd ~ pedidos_mes + edad, data = delivery)
summary(m2)

# COMPARAR EL R²
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared

# GRAFICO DE DISPERCION 

ggplot(delivery, aes(pedidos_mes, gasto_mensual_usd)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 5. CONCLUSIÓN 
# -----------------------------------------------------------------------------

# 1. Preparación y diagnóstico
# La base tiene 600 clientes y 8 variables. 
# Solo calificacion tiene faltantes (25, 4,2 %).
# Además, tiempo_entrega_min tiene 18 valores 999 que son datos desconocidos disfrazados.

# 2. Estadística por grupos (tiempo de entrega por zona)
# in los 999, Valles es la zona más lenta (mediana 45 min) y Centro la más rápida (29 min).
# La dispersión es similar entre zonas (DE de 5 a 7 min). 
# Con los 999, las medias y desviaciones quedan infladas.
# 
# 
# 