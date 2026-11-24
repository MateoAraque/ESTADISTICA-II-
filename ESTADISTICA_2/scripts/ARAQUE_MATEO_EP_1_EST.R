# =============================================================================
# ESTADÍSTICA II · Evaluación parcial · Unidad 1 (sesiones 01 a 06) · Parte práctica
# Universidad UTE · Escuela de Tecnologías · Período 2026-2
# -----------------------------------------------------------------------------
# Nombre: Mateo Gabriel Araque Basantes
# Tiempo: Individual · Nivel de IA: IANP (sin IA ni ayuda de
# terceros). Evaluación sumativa. Puede consultar sus scripts de las sesiones 03 a 06.
# Datos: operadora_movil_usuarios.csv (en la carpeta de su proyecto).
# Escriba sus interpretaciones como comentarios (#) debajo de cada resultado.
# Entregue este archivo .R en la tarea «Evaluación parcial U1 – parte práctica».
# =============================================================================

library(tidyverse)
library(janitor)

# -----------------------------------------------------------------------------
# 1. PREPARACIÓN Y DIAGNÓSTICO
#    Lea la base, limpie los nombres, cuente los faltantes por variable,
#    detecte el código especial de la velocidad y cree la base limpia.
# -----------------------------------------------------------------------------
usu_raw <- operadora_movil_usuarios

usuarios <- usu_raw %>%
  clean_names()

glimpse(usuarios)

# CONTEO DE VALORES FALTANTES (NA)

faltantes <- usuarios %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "n_faltantes")
View(faltantes)

# LA VARIABLE SATISFACCION ES LA QUE PRESENTA VALORES FALTANTES ESTA ES LA UNICA
# QUE TIENE VALORES NA = 30

# REVISION ED VALORES EXTREMOS O CODIGOS EN VELOCIDAD_MBPS

summary(usuarios$velocidad_mbps)
usuarios %>%
  count(velocidad_mbps == 9999)

# GRACIAS A ESTO SE OBSERVA QUE LOS VALORES 9999 ERAN LOS CODIGOS EXTREMOS
# EXISTIENDO 21 VALORES = 9999

# CREACION DE LA BASE LIMPIA EXCLUYENDO LOS 9999 

usu_limpia <- usuarios %>%
  filter(velocidad_mbps != 9999)

nrow(usu_limpia)

# LA BASE LIMPIA QUITANDO LOS 21 USUARIOS CON EL CODIGO 999 QUEDA CON 619 USUARIOS

# -----------------------------------------------------------------------------
# 2. ESTADÍSTICA DESCRIPTIVA POR GRUPOS
#    Velocidad de conexión por ciudad: número de usuarios, media, mediana y
#    desviación estándar, y un diagrama de caja. Interprete.
# -----------------------------------------------------------------------------

tabla_ciudad <- usu_limpia %>%
  group_by(ciudad) %>%
  summarise(
    n_usuarios        = n(),
    media_vel   = mean(velocidad_mbps),
    mediana_vel = median(velocidad_mbps),
    sd_vel      = sd(velocidad_mbps),
    media_satis = mean(satisfaccion, na.rm = TRUE)
  ) %>%
  arrange(desc(media_vel))

View(tabla_ciudad)

# DIAGRAMA DE CAJA DE VELOCIADAD DE CONEXION POR CIUDAD

ggplot(usu_limpia, aes(x = ciudad, y = velocidad_mbps)) +
  geom_boxplot(fill = "#789") +
  labs(title = "Velocidad de conexión por ciudad",
       x = "Ciudad",
       y = "Velocidad de conexión") +
  theme_minimal()

# INTERPRETACION:
# 1. QUITO REGISTRA LA CONEXION MAS RAPIDA CON UNA MEDIA DE 85.21 Y MEDIANA DE 85.05
   
# 2. MANTA TIENE LA CONEXION MAS LENTA CON UNA MEDIA DE 48.49 Y UNA MEDIANA DE 48.40

# -----------------------------------------------------------------------------
# 3. MUESTREO Y ESTIMACIÓN: DOS DISEÑOS
#    a) Calcule la media verdadera del factura mensual (μ) en la base limpia.
#    b) Con set.seed(2611), tome una muestra aleatoria simple de 100 usuarios
#       y una muestra estratificada por ciudad (prop = 100 / número de usuarios).
#       Para la aleatoria simple, calcule el IC 95 % (media ± 1,96 · s/√n).
#       Calcule el error de cada muestra (media − μ).
#    c) Repita cada diseño 200 veces con replicate() y compare el error medio:
#       mean(abs(medias − μ)). ¿Qué diseño tiene menos error? ¿Por qué?
# -----------------------------------------------------------------------------

# A) PARAMETROS POBLACIONALES EN LA ABSE LIMPIA 
num     <- nrow(usu_limpia)
mu    <- mean(usu_limpia$factura_mensual_usd)
sigma <- sd(usu_limpia$factura_mensual_usd)

num
round(mu,2)
round(sigma,2)

# LA MEDIA VERDADERA DE LA FACTURA MENSUAL ES DE 22.81 USD .

# B) MUESTRA ALEATORIA SIMPLE (MAS) Y ESTRATIFICADA POR CIUDAD

set.seed(2611)

muestra_mas <- usu_limpia %>%
  slice_sample(n = 100)

media_mas <- mean(muestra_mas$factura_mensual_usd)
ee_mas    <- sd(muestra_mas$factura_mensual_usd) / sqrt(100)
ic_mas    <- media_mas + c(-1, 1) * 1.96 * ee_mas
error_mas <- media_mas - mu

round(media_mas,2)
round(ic_mas,2)
round(error_mas,2)

set.seed(2611)
muestra_estr <- usu_limpia %>%
  group_by(ciudad) %>%
  slice_sample(prop = 100 / nrow(usu_limpia)) %>%
  ungroup()

media_estr <- mean(muestra_estr$factura_mensual_usd)
error_estr <- media_estr - mu

round(media_estr,2)
round(error_estr,2)
nrow(muestra_estr)

# INTERPRETACION B):
# 1. LA MUESTTRA ALEATORIA SIMPLE OBTIENE UNA MEDIA MUESTRAL DE FACTURA MENSUAL
#    CON SU RESPECTIVO IC DEL 95%, EL CUAL CONTIENE LA MEDIA VERDADERA DE 22,81.

# C) SIMULACION DE 200 REPETICIONES PARA AMBOS DISEÑOS

set.seed(2611)
media_mas_200 <- replicate(200, {
  mean(slice_sample(usu_limpia, n = 100)$factura_mensual_usd)
})

set.seed(2611)
media_estr_200 <- replicate(200, {
  usu_limpia %>%
    group_by(ciudad) %>%
    slice_sample(prop = 100 / nrow(usu_limpia)) %>%
    ungroup() %>%
    summarise(m = mean(factura_mensual_usd)) %>%
    pull(m)
})

error_medio_mas  <- mean(abs(media_mas_200 - mu))
error_medio_estr <- mean(abs(media_estr_200 - mu))

round(error_medio_mas, 3)
round(error_medio_estr, 3)

# Interpretación c):
# 1. EL MUESTREO ESTRATIFICADO TIENE UN MENOR ERROR MEDIO QUE EL MUESTREO 
#    ALEATORIO SIMPLE.
# 2. PORQUE LAS CIUDADES TIENEN MASNIVELES DE FACTURACION PROMEDIO DISTINTOS ENTRE SI 
#    MIENTRAS QUE DENTRO DE CADA CIUDAD SON MAS SIMILARES.


# -----------------------------------------------------------------------------
# 4. CORRELACIÓN Y REGRESIÓN
#    ¿Qué variable se relaciona mejor con la factura mensual: datos_gb_mes o edad?
#    Ajuste factura ~ datos_gb_mes, interprete la pendiente y el R², agregue la
#    edad y decida con el R² ajustado si conviene mantenerla.
# -----------------------------------------------------------------------------

r_datos <- cor(usu_limpia$factura_mensual_usd, usu_limpia$datos_gb_mes)
r_edad  <- cor(usu_limpia$factura_mensual_usd, usu_limpia$edad)

r_datos
r_edad

# LA VARIABLE QUE SE RELACIONA MEJOR CON LA FACTURA MENSUAL ES datos_gb_mes CON UNA 
# CORRELACION LINEAL POSITIVA DE 0.922, MIENTRAS QUE LA EDAD NO TIENE UNA 
# CORRELACION LINEAL CONTRA EL MONTO FACTURADO SIENDO DE 0.009

# MODELO 1 :  REGRESION LINEAL SIMPLE (factura ~ datos_gb_mes)

m1 <- lm(factura_mensual_usd ~ datos_gb_mes, data = usu_limpia)

summary(m1)

# ESTIMACION DE FACTURA PARA USUARIOS CONSUMIENDO 20 GB

predict(m1, newdata = tibble(datos_gb_mes = 20))

# INTERPRETACION MODELO 1:
# 1. POR CADA 1 GB MAS CONSUMIDO AL MES, LA FACTURA MENSUAL AUMENTA EN PROMEDIO 1.50 USD.
# 2. R² = 0,8511: EL CONSUMO DE DATOS MOVILES EXPLICA EL 58.11% DE LA VARIABILIDAD


# -----------------------------------------------------------------------------
# 5. CONCLUSIÓN (máximo 5 líneas, para el gerente comercial de la operadora)
# -----------------------------------------------------------------------------

# LA FACTURACION MENSUAL MEDIA DE 22.81 USD DEPENDE MAS DEL USO DE DATOS MOVILES
# CON UN INCREMENTO PROMEDIO DE 1.50 USD POR GB ADICIONAL, MIENTRAS QUE LA EDAD
# NO APORTA PODER EXPLICATIVO.PARA ESTIMAR LOS INGRESOS POR CIUDAD CON MAYOR
# PRECISION, CONVIENE USUAR UN MUESTREO ESTRATIFICADO POR CIUDAD. ADEMAS LA VELOCIDAD
# ENTRE CIUDADES Y LOS VALORES ATIPICOS FALTANTES REQUIEREN REVISION TECNICA Y DE 
# CALIDAD DE DATOS ANTES DE TOMAR UNA DECICION COMERCIAL.

