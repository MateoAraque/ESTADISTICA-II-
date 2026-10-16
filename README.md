# 📊 Análisis Estadístico con RStudio

Repositorio dedicado al análisis de datos, procesamiento estadístico y visualización utilizando **R** y **RStudio**. Este proyecto contiene scripts prácticos que abarcan desde conceptos básicos de estadística descriptiva hasta modelos de inferencia y regresión.

---

## 🛠️ Contenido del Repositorio

El proyecto está organizado en las siguientes secciones/scripts:

1. **Estadística Descriptiva (`01_descriptiva.R`)**
   - Cálculo de medidas de tendencia central (media, mediana, moda).
   - Medidas de dispersión (desviación estándar, varianza, rangos).
   - Generación de tablas de frecuencias y resúmenes estadísticos.

2. **Visualización de Datos (`02_visualizacion.R`)**
   - Gráficos de barras, histogramas y diagramas de caja (Boxplots) utilizando `ggplot2`.
   - Personalización de temas, títulos y exportación de gráficos de alta calidad.

3. **Pruebas de Hipótesis (`03_pruebas_hipotesis.R`)**
   - Pruebas t de Student para muestras independientes y pareadas.
   - Análisis de Varianza (ANOVA).
   - Pruebas de Chi-cuadrada ($\chi^2$) para variables categóricas.

4. **Modelos de Regresión (`04_regresion.R`)**
   - Modelos de regresión lineal simple y múltiple.
   - Validación de supuestos estadísticos (normalidad, homocedasticidad).

---

## ⚙️ Requisitos e Instalación

Para ejecutar este proyecto de manera local, asegúrate de tener instalado:
* [R](https://cran.r-project.org/) (versión 4.0 o superior recomendada)
* [RStudio](https://posit.co/downloads/)

### Paquetes necesarios
Puedes instalar las librerías necesarias ejecutando el siguiente código en tu consola de RStudio:

```R
install.packages(c("tidyverse", "ggplot2", "dplyr", "car", "nortest"))
