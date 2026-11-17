# =============================================================================
# ESTADÍSTICA II · Sesión 06 · Correlación y regresión lineal simple y múltiple
# Universidad UTE · UAEFTT · Período 2026-2
# -----------------------------------------------------------------------------
# Idea central: ¿los países más ricos viven más? Medimos la relación con la
# correlación, la modelamos con una recta y comprobamos que agregar variables
# no siempre mejora el modelo.
# Funciones nuevas: cor(), cor.test(), lm(), summary(), coef(), predict(),
# fitted(), residuals(), geom_smooth(), geom_tile(), geom_text(), log10().
# =============================================================================

# DECLARACIÓN DE USO DE IA
# Herramienta: ninguna
# Nivel de la actividad: IANP (clase) · IAA en la práctica (solo depurar código)
# Para qué la usé: —
# Qué hice yo: escribí y ejecuté este script en clase.

# -----------------------------------------------------------------------------
# 0. PAQUETES
# -----------------------------------------------------------------------------
# Si no tiene gapminder: install.packages("gapminder") en la Consola.
library(tidyverse)
library(gapminder)

# -----------------------------------------------------------------------------
# 1. LOS DATOS: 142 PAÍSES EN 2007
# -----------------------------------------------------------------------------
# gapminder tiene 1 704 filas: 142 países observados cada 5 años (1952-2007).
# Usamos solo 2007 para tener UNA fila por país (observaciones independientes).
glimpse(gapminder)

paises <- gapminder |>
  filter(year == 2007)

nrow(paises)                              # 142 países

# -----------------------------------------------------------------------------
# 2. ACTIVACIÓN: ¿LOS PAÍSES MÁS RICOS VIVEN MÁS?
# -----------------------------------------------------------------------------
# Antes de ejecutar: escriba su predicción. ¿Qué forma tendrá la nube de puntos?
paises |>
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(colour = "#09509E", alpha = 0.7) +
  labs(title = "¿Los países más ricos viven más? (2007)",
       x = "PIB per cápita (USD de 2005, PPA)", y = "Esperanza de vida (años)")
#la relación es positiva pero CURVA: sube rápido en los países
# pobres y se aplana en los ricos. Guarde esta observación para la sección 5.

# -----------------------------------------------------------------------------
# 3. CORRELACIÓN DE PEARSON
# -----------------------------------------------------------------------------
# r va de -1 a 1: el signo indica la dirección y el valor absoluto, la fuerza
# de la relación LINEAL. r = 0 no significa «no hay relación»: puede ser curva.
cor(paises$gdpPercap, paises$lifeExp)     # r = 0,679
cor(paises$pop, paises$lifeExp)           # r = 0,048

# ¿Es r distinto de 0 en la población? H0: ρ = 0
cor.test(paises$gdpPercap, paises$lifeExp)
# t = 10,93; p < 0,001; IC 95 %: 0,58 a 0,76. La correlación es moderada-fuerte,
# positiva y significativa. Con pop: r = 0,05, sin relación lineal.
# Insista: correlación no implica causalidad. El ingreso se asocia con salud,
# educación, agua potable…; la recta no dice qué causa qué.

# -----------------------------------------------------------------------------
# 4. REGRESIÓN LINEAL SIMPLE: lifeExp = b0 + b1 · gdpPercap
# -----------------------------------------------------------------------------
m1 <- lm(lifeExp ~ gdpPercap, data = paises)
summary(m1)
# b0 = 59,57: esperanza de vida estimada con PIB per cápita igual a 0 (sin
# sentido práctico, solo ubica la recta).
# b1 = 0,000637: por cada USD adicional, +0,000637 años. Mejor leerlo así:
# por cada 1 000 USD más de PIB per cápita, la esperanza de vida sube en
# promedio 0,64 años.
# R² = 0,461: el PIB per cápita explica el 46 % de la variación entre países.

paises |>
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(colour = "#09509E", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "#C2661C") +
  labs(title = "Modelo 1: una recta para una relación curva",
       x = "PIB per cápita (USD)", y = "Esperanza de vida (años)")

# Supuesto de linealidad: los residuos (dato − valor ajustado) no deberían
# mostrar ningún patrón.
tibble(ajustado = fitted(m1), residuo = residuals(m1)) |>
  ggplot(aes(x = ajustado, y = residuo)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, colour = "#C2661C") +
  labs(title = "Residuos del modelo 1", x = "Valor ajustado", y = "Residuo")
# Los residuos forman una curva: la recta sobreestima a los países ricos y
# subestima a los de ingreso medio. Se incumple el supuesto de linealidad.
# Los otros supuestos (independencia, varianza constante, residuos
# aproximadamente normales) se mencionan; no se prueban formalmente hoy.

# =============================================================================
# PRÁCTICA DE LABORATORIO (secciones 5 a 8)
# =============================================================================

# -----------------------------------------------------------------------------
# 5. TRANSFORMAR LA VARIABLE: logaritmo del PIB per cápita
# -----------------------------------------------------------------------------
# log10() convierte 1 000 → 3, 10 000 → 4, 100 000 → 5. Así cada unidad de
# log_pib significa «multiplicar el ingreso por 10».
paises <- paises |>
  mutate(log_pib = log10(gdpPercap))

paises |>
  ggplot(aes(x = log_pib, y = lifeExp)) +
  geom_point(colour = "#09509E", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "#C2661C") +
  labs(title = "Modelo 2: esperanza de vida y logaritmo del PIB per cápita",
       x = "log10(PIB per cápita)", y = "Esperanza de vida (años)")

cor(paises$log_pib, paises$lifeExp)       # r = 0,809
m2 <- lm(lifeExp ~ log_pib, data = paises)
summary(m2)

# Interprete b1 y R² del modelo 2 (un párrafo):
#
# b1 = 16,59: cuando el PIB per cápita se multiplica por 10, la esperanza de
# vida sube en promedio 16,6 años. R² = 0,654: el modelo explica el 65 % de la
# variación (antes 46 %). No agregamos información: solo describimos mejor la
# forma de la relación.

# -----------------------------------------------------------------------------
# 6. MATRIZ DE CORRELACIÓN CON MAPA DE CALOR
# -----------------------------------------------------------------------------
# cor() con varias columnas numéricas devuelve una matriz. La convertimos en
# tabla larga (una fila por par de variables) para graficarla con geom_tile().
matriz <- paises |>
  select(lifeExp, gdpPercap, log_pib, pop) |>
  cor()
round(matriz, 2)

matriz |>
  as_tibble(rownames = "var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "r") |>
  ggplot(aes(x = var1, y = var2, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 5) +
  scale_fill_gradient2(low = "#C2661C", mid = "white", high = "#09509E", limits = c(-1, 1)) +
  labs(title = "Matriz de correlación (142 países, 2007)", x = NULL, y = NULL)
# Lectura: lifeExp con log_pib 0,81; con gdpPercap 0,68; con pop 0,05.
# pop casi no se relaciona con nada: anticipa que no aportará al modelo.
# NOTA PEDAGÓGICA: solo entran variables numéricas continuas; country y
# continent son categóricas y no tienen correlación de Pearson.

# -----------------------------------------------------------------------------
# 7. ¿MÁS VARIABLES = MEJOR MODELO? (regresión lineal múltiple)
# -----------------------------------------------------------------------------
# Modelo 3: agregamos la población.
m3 <- lm(lifeExp ~ log_pib + pop, data = paises)
summary(m3)

# Modelo 4: agregamos una columna de NÚMEROS AL AZAR (puro ruido).
set.seed(2026)
paises <- paises |>
  mutate(ruido = rnorm(n()))
m4 <- lm(lifeExp ~ log_pib + ruido, data = paises)
summary(m4)
#Baja el modelo, no hace más cambios solo añade variables que ayudar a los valores del
# r2 y r2ajustado

# Modelo 5: agregamos el continente (variable categórica: R crea una columna
# 0/1 por cada continente, y África queda como referencia).
m5 <- lm(lifeExp ~ log_pib + continent, data = paises)
summary(m5)
# Mejora considerablemente  
# Tabla comparativa: R² siempre sube al agregar variables; el R² AJUSTADO
# penaliza las variables que no aportan.
comparacion <- tibble(
  modelo = c("1: gdpPercap", "2: log_pib", "3: log_pib + pop",
             "4: log_pib + ruido", "5: log_pib + continent"),
  r2 = c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared,
         summary(m4)$r.squared, summary(m5)$r.squared),
  r2_ajustado = c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared,
                  summary(m3)$adj.r.squared, summary(m4)$adj.r.squared,
                  summary(m5)$adj.r.squared)
)
comparacion |>
  mutate(across(c(r2, r2_ajustado), \(x) round(x * 100, 2)))   # en porcentaje
# Resultados: m1 0,4606 / 0,4567 · m2 0,6544 / 0,6520 · m3 0,6589 / 0,6540
# · m4 0,6553 / 0,6503 · m5 0,7674 / 0,7588.
# pop: R² sube a 0,6589 pero el coeficiente no es significativo (p = 0,18) y
# el R² ajustado casi no cambia (+0,002).
# ruido: R² sube (¡con números al azar!) pero el R² ajustado BAJA.
# continent: R² ajustado sube a 0,759: sí aporta información nueva.
# Mensaje: se agrega una variable si tiene sentido, es significativa y mejora
# el R² ajustado; no porque haga subir el R².

# Interprete la tabla (un párrafo por modelo):
#
# Modelo 3: la población no aporta; su p = 0,18 y el R² ajustado apenas cambia.
# Modelo 4: el ruido hace subir el R² (0,6544 → 0,6553), pero el R² ajustado
# baja (0,6520 → 0,6503): la variable sobra y se elimina.
# Modelo 5: el continente sí aporta (R² ajustado 0,759). Con el mismo ingreso,
# un país de América vive en promedio 11,7 años más que uno de África.

# -----------------------------------------------------------------------------
# 8. ECUADOR FRENTE AL MODELO 2
# -----------------------------------------------------------------------------
ecuador <- paises |>
  filter(country == "Ecuador")
ecuador |> select(country, lifeExp, gdpPercap)
predict(m2, newdata = ecuador)            # 68,6 años
ecuador$lifeExp - predict(m2, newdata = ecuador)   # residuo de Ecuador
# Ecuador vive 75,0 años: 6,4 años más de lo que predice su ingreso
# (6 873 USD). Buena pregunta para el grupo: ¿qué factores podrían explicarlo?

# Trabajo autónomo: interprete b1 y el R² ajustado de cada modelo (un párrafo
# por modelo) y responda: si el R² ajustado baja al agregar una variable,
# ¿qué debería hacer con esa variable?

