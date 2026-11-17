# =============================================================================
# ESTADÍSTICA II · Sesión 07 · Pruebas de hipótesis para comparar grupos
# Versión DOCENTE (completa, con solución del taller evaluado)
# Universidad UTE · Escuela de Tecnologías · Período 2026-2
# -----------------------------------------------------------------------------
# Pregunta: ¿difieren las horas de uso de internet según el nivel de
# instrucción? Datos: ENEMDU-TIC 2024 (INEC), archivo de personas de julio.
# Funciones nuevas: read_delim() con locale(), as.numeric(), str_replace(),
# t.test(), var.test(), bartlett.test(), aov(), TukeyHSD(), oneway.test().
# =============================================================================

# DECLARACIÓN DE USO DE IA
# Herramienta: ninguna
# Nivel de la actividad: IANP (clase) · IAA en el taller (solo depurar código)
# Para qué la usé: —
# Qué hice yo: escribí y ejecuté este script en clase.

# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------
library(tidyverse)

# -----------------------------------------------------------------------------
# 1. LEER LA BASE
# -----------------------------------------------------------------------------
# El archivo del INEC usa punto y coma (;) como separador y coma (,) como
# separador decimal. Debe estar en la carpeta de su proyecto.
enemdu <- read_delim("~/Desktop/Estadistica-II_UTE/2_BDD_DATOS_ABIERTOS_ENEMDU_TICS_2024_07_CSV/enemdu_persona_tics_2024_07.csv",
                     delim = ";",
                     locale = locale(decimal_mark = ","))
dim(enemdu)                              # 28 696 personas y 171 variables
# NOTA: el archivo está en UTF-8 y solo contiene números y códigos (sin
# tildes), así que con read_delim() da lo mismo indicar encoding = "latin1" o
# no. Con read.csv(..., fileEncoding = "latin1") la primera columna sale como
# «ï..area»: por eso usamos readr.

# -----------------------------------------------------------------------------
# 2. ELEGIR LAS VARIABLES (con nombres claros)
# -----------------------------------------------------------------------------
# Diccionario: area (1 urbano, 2 rural) · p02 sexo (1 hombre, 2 mujer) ·
# p03 edad · nnivins nivel de instrucción (1 ninguno, 2 centro de
# alfabetización, 3 básica, 4 media, 5 superior) · pt3b horas de uso de
# internet al día · ingpc ingreso per cápita del hogar.
tic <- enemdu |>
  select(area, sexo = p02, edad = p03, nivel_instruccion = nnivins,
         horas_internet = pt3b, ingreso_percapita = ingpc)

str(tic)

# Descubrimiento 1: nivel_instruccion, horas_internet e ingreso_percapita
# aparecen como <chr> (texto). ¿Por qué? Las personas a las que no se les hizo
# la pregunta tienen un espacio en blanco " ", y eso obliga a leer la columna
# como texto. En ingreso_percapita, además, la coma decimal queda dentro del
# texto. Pida que lo descubran (View(tic)) antes de decirlo.

# -----------------------------------------------------------------------------
# 3. CORREGIR LOS TIPOS
# -----------------------------------------------------------------------------
# as.numeric() convierte el texto en número; los espacios en blanco pasan a NA.
# ingreso_percapita trae coma decimal dentro del texto: primero la cambiamos
# por punto con str_replace().
tic <- tic |>
  mutate(nivel_instruccion = as.numeric(nivel_instruccion),
         horas_internet    = as.numeric(horas_internet),
         ingreso_percapita = as.numeric(str_replace(ingreso_percapita, ",", ".")))

summary(tic)
# Descubrimiento 2: horas_internet tiene máximo 888 y media 35,4 horas al día.
# ¡Imposible! 888 es un código del INEC (sin información del módulo TIC), no
# un dato. Si no se filtra, todo el análisis queda contaminado.

# -----------------------------------------------------------------------------
# 4. DIAGNÓSTICO DE FALTANTES Y CÓDIGOS ESPECIALES
# -----------------------------------------------------------------------------
tic |>
  summarise(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "faltantes")
# horas_internet: 6 173 NA (no usan internet o no se les preguntó);
# nivel_instruccion: 1 521 NA (niños pequeños); ingreso: 150 NA.

tic |>
  count(horas_internet == 888)               # 825 personas con el código 888

# -----------------------------------------------------------------------------
# 5. FILTRAR Y RECODIFICAR
# -----------------------------------------------------------------------------
# Nos quedamos con quienes reportan horas de uso y nivel de instrucción.
# Los niveles 1 (ninguno) y 2 (alfabetización) tienen muy pocos casos
# (114 y 6): los unimos en un solo grupo.
tic_limpio <- tic |>
  filter(!is.na(horas_internet), horas_internet != 888,
         !is.na(nivel_instruccion)) |>
  mutate(
    area = case_when(area == 1 ~ "Urbano", area == 2 ~ "Rural"),
    sexo = case_when(sexo == 1 ~ "Hombre", sexo == 2 ~ "Mujer"),
    nivel_instruccion = case_when(
      nivel_instruccion <= 2 ~ "Ninguno o alfabetización",
      nivel_instruccion == 3 ~ "Básica",
      nivel_instruccion == 4 ~ "Media",
      nivel_instruccion == 5 ~ "Superior"),
    nivel_instruccion = factor(nivel_instruccion,
                               levels = c("Ninguno o alfabetización", "Básica", "Media", "Superior")))

str(tic_limpio)

# =============================================================================
# PRUEBAS DE HIPÓTESIS
# =============================================================================
# -----------------------------------------------------------------------------
# 6. DESCRIBIR ANTES DE PROBAR
# -----------------------------------------------------------------------------
tic_limpio |>
  group_by(nivel_instruccion) |>
  summarise(personas = n(),
            media    = mean(horas_internet),
            desv     = sd(horas_internet),
            mediana  = median(horas_internet))
# Medias: 1,67 · 2,15 · 3,11 · 4,02 h. Desviaciones: 1,26 · 1,33 · 1,81 · 2,24.
# Ya se ve que la media Y la dispersión crecen con el nivel.

tic_limpio |>
  ggplot(aes(x = nivel_instruccion, y = horas_internet)) +
  geom_boxplot(fill = "#D6E4F7") +
  labs(title = "Horas de uso de internet al día según nivel de instrucción",
       subtitle = "ENEMDU-TIC 2024 (julio), personas que usan internet",
       x = NULL, y = "Horas al día")

# -----------------------------------------------------------------------------
# 7. DOS GRUPOS: PRUEBA t DE WELCH
# -----------------------------------------------------------------------------
# H0: la media de horas es igual en los dos grupos.
# H1: las medias son diferentes.
# t.test() usa por defecto la versión de Welch, que NO supone varianzas iguales.

# a) ¿Hombres y mujeres usan internet un número distinto de horas?
t.test(horas_internet ~ sexo, data = tic_limpio)
# Hombres 2,97 h, mujeres 2,93 h; t = 1,46; p = 0,145. No se rechaza H0:
# no hay evidencia de diferencia entre hombres y mujeres.

# b) ¿Las personas del área urbana y rural usan internet un número distinto de horas?
t.test(horas_internet ~ area, data = tic_limpio)
# Rural 2,47 h, urbano 3,08 h; t = −22,3; p < 0,001; IC 95 % de la diferencia:
# 0,56 a 0,66 h. Se rechaza H0. Diferencia: 0,61 h ≈ 37 minutos al día.
# NOTA: el orden de los grupos es alfabético (Rural − Urbano); por eso t es
# negativo.

# Escriba H0, H1 y su conclusión para a) y b):
#
# a) H0: μ hombres = μ mujeres. p = 0,145 > 0,05 → no se rechaza H0.
# b) H0: μ urbano = μ rural. p < 0,001 → se rechaza H0. En el área urbana se
#    usa internet unos 37 minutos más al día (3,08 frente a 2,47 horas).

# -----------------------------------------------------------------------------
# 8. TRES O MÁS GRUPOS: SUPUESTOS, ANOVA Y COMPARACIONES
# -----------------------------------------------------------------------------
# H0: las cuatro medias son iguales. H1: al menos una es diferente.

# Supuesto 1: normalidad aproximada. Con miles de datos por grupo, la media
# es aproximadamente normal (teorema central del límite, sesión 05), aunque
# las horas no lo sean. El grupo más pequeño tiene 120 personas.
tic_limpio |>
  ggplot(aes(x = horas_internet)) +
  geom_histogram(binwidth = 1, fill = "#09509E") +
  facet_wrap(~ nivel_instruccion, scales = "free_y") +
  labs(title = "Distribución de horas por nivel de instrucción", x = "Horas al día", y = "Personas")

# Supuesto 2: homogeneidad de varianzas. H0: las varianzas son iguales.
bartlett.test(horas_internet ~ nivel_instruccion, data = tic_limpio)
# K² = 1 876,7; p < 0,001 → se rechaza: las varianzas NO son iguales
# (las desviaciones van de 1,26 a 2,24).

# Como las varianzas difieren, la prueba adecuada es el ANOVA de Welch:
oneway.test(horas_internet ~ nivel_instruccion, data = tic_limpio,
            var.equal = FALSE)
# F = 1 258,8; p < 0,001 → se rechaza H0: al menos un nivel difiere.

# Para comparar con el ANOVA clásico (supone varianzas iguales):
anova_clasico <- aov(horas_internet ~ nivel_instruccion, data = tic_limpio)
summary(anova_clasico)
# F = 1 289; p < 0,001. Misma conclusión, pero su supuesto no se cumple.

# ¿Qué pares difieren? Comparaciones múltiples de Tukey
TukeyHSD(anova_clasico)
# Todos los pares difieren (p ajustado < 0,05). Básica − Ninguno: 0,48 h
# (p = 0,015, la menor diferencia); Superior − Básica: 1,87 h.
# NOTA: Tukey también supone varianzas iguales; aquí se usa como guía
# descriptiva. Con diferencias tan grandes y tantos datos, la conclusión no
# cambia. Mencione que existen pruebas para varianzas distintas
# (Games-Howell), que no veremos.