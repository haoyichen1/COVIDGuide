rm(list = ls())

# Cargando bibliotecas -------------------------------------------------------

library(pacman)
p_load(
  tidyverse,
  # cargando paquete para manipulación de datos
  readr,
  # cargando paquete para leer archivos de texto
  vcd,
  # cargando paquete para análisis de tablas de contingencia
  DescTools,
  # cargando paquete con funciones para describir datos
  magrittr,
  # cargando paquete para operaciones pipe
  PracTools,
  # cargando paquete para muestreo aleatorio
  sampling,
  # cargando paquete para muestreo aleatorio
  TeachingSampling,
  # cargando paquete con herramientas de enseñanza para muestreo
  psych # cargando paquete para análisis psicométrico
)

# Leyendo datos ------------------------------------------------------------

# HFPS = Encuesta telefónica de alta frecuencia - Malawi
# IHPS = Encuesta de panel integrado de hogares - Malawi

# leyendo los datos del registro de hogares del archivo hh_mod_b_19.csv y asignándolos a la variable HFPS_hh_roster
HFPS_hh_roster <- read.csv("Data/hh_mod_b_19.csv")

# leyendo los datos de educación del hogar del archivo hh_mod_c_19.csv y asignándolos a la variable HFPS_hh_edu
HFPS_hh_edu <- read.csv("Data/hh_mod_c_19.csv")

# leyendo los datos económicos del hogar del archivo hh_mod_e_19.csv y asignándolos a la variable HFPS_hh_eco
HFPS_hh_eco <- read.csv("Data/hh_mod_e_19.csv")

# leyendo los datos de filtro del hogar del archivo hh_mod_a_filt_19.csv y asignándolos a la variable IHPS_hh_filter
IHPS_hh_filter <- read.csv("Data/hh_mod_a_filt_19.csv")

# Limpieza de datos -----------------------------------------------------------

# Unir y seleccionar variables relevantes
HFPS <- HFPS_hh_roster %>%
  inner_join(HFPS_hh_edu) %>%
  inner_join(HFPS_hh_eco) %>%
  inner_join(IHPS_hh_filter) %>%
  select(
    y4_hhid,
    # identificador del hogar
    id_code,
    # identificador individual
    hh_b04,
    # si el individuo es jefe de hogar
    hh_b06_4,
    # si el hogar tiene teléfono activo
    hh_c05_1,
    # estado de alfabetización del individuo
    hh_e06_8a,
    # estado de empleo del individuo
    hh_wgt,
    # peso del hogar
    panelweight_2019,
    # peso individual
    region,
    # región del hogar
    reside,
    # tipo de residencia (urbana o rural)
    hhsize           # tamaño del hogar
  ) %>%
  drop_na() %>%      # Eliminar filas con valores faltantes
  filter(hh_e06_8a != 4)      # Excluir aprendices no remunerados

# Datos del jefe de hogar -----------------------------------------------------

# Identificar si el hogar tiene un teléfono que funciona
hh_phone <- HFPS %>%
  group_by(y4_hhid) %>%
  summarise(hh_phone = min(hh_b06_4, na.rm = TRUE),
            # Valor mínimo de hh_b06_4 por grupo
            tt_wgt = sum(hh_wgt))                  # Peso total del hogar

# Identificar a los jefes de hogar
head_hh <- HFPS %>%
  filter(hh_b04 == 1)        # Filtrar por jefes de hogar

# Crear variables adicionales
hh_temp <- head_hh %>%
  inner_join(hh_phone) %>%     # Unirse con el conjunto de datos hh_phone
  drop_na() %>%                # Eliminar filas con valores faltantes
  mutate(
    Phone =                      # Crear variable Phone en función de hh_b06_4
      case_when(hh_b06_4 == 1 ~ "Yes",
                TRUE ~ "No"),
    Literacy =                   # Crear variable Literacy en función de hh_c05_1
      case_when(hh_c05_1 == 1 ~ "Literate",
                TRUE ~ "Illiterate")
  )


# Pruebas de Chi-cuadrado y Cramer-V -------------------------------------------

# La propiedad del teléfono está correlacionada con la alfabetización
# De un subconjunto de teléfonos se selecciona una muestra en una segunda fase adicional
# Los datos del teléfono se recopilan mediante el teléfono

# Crear una tabla de contingencia de la propiedad del teléfono y el estado de alfabetización
crosstable1 <- table(hh_temp$Phone, hh_temp$Literacy)
crosstable1

# Realizar una prueba de chi-cuadrado para verificar la asociación entre la propiedad del teléfono y la alfabetización
chisq.test(crosstable1)

# Calcular la estadística V de Cramer para medir la fuerza de la asociación
CramerV(crosstable1,
        conf.level = 0.95)

# Calcular otras estadísticas de asociación basadas en la tabla de contingencia
assocstats(crosstable1)

# Crear una tabla de contingencia de la propiedad del teléfono y el estado de aprendizaje no remunerado
crosstable2 <- table(hh_temp$Phone, hh_temp$hh_e06_8a)

# Calcular la proporción de propietarios de teléfonos que tienen un estado de aprendizaje no remunerado
prop.table(crosstable2, margin = 2)[2, ]

# Modelo de puntaje de propensión -----------------------------------------

# Modelo de regresión logística
ps_fit1 <- glm(
  formula = factor(Phone) ~ 1 + factor(hh_c05_1) + factor(hh_e06_8a),
  family = binomial(link = "logit"),
  data = hh_temp
)

# Imprimir el resumen del modelo
summary(ps_fit1)

# Agregar el puntaje de propensión predicho como una nueva columna en el conjunto de datos
hh_temp$ps <- predict(ps_fit1, type = "response")

# Calcular la suma del puntaje de propensión
sum(hh_temp$ps)

# Crear una tabla de propiedad del teléfono
table(hh_temp$Phone)

# Crear un histograma del puntaje de propensión
hist(hh_temp$ps)

# Imprimir las estadísticas resumidas del puntaje de propensión
summary(hh_temp$ps)

# Configurar un gráfico de 3x1
par(mfrow = c(3, 1))

# Crear un histograma del puntaje de propensión para todas las unidades
hist(hh_temp$ps,
     main = "Todas las unidades",
     xlab = "Puntaje de propensión estimado",
     xlim = c(0, 1))

# Crear un histograma del puntaje de propensión para los encuestados
hist(hh_temp$ps[hh_temp$Phone == "Yes"],
     main = "Encuestados",
     xlab = "Puntaje de propensión estimado",
     xlim = c(0, 1))

# Crear un histograma del puntaje de propensión para los no encuestados
hist(hh_temp$ps[hh_temp$Phone == "No"],
     main = "No encuestados",
     xlab = "Puntaje de propensión estimado",
     xlim = c(0, 1))

# Cerrar el gráfico
dev.off()

# Configurar un gráfico de 2x2
par(mfrow = c(2, 2))

# Crear un boxplot de PS por alfabetización para encuestados
with(
  hh_temp[hh_temp$Phone == "Yes",],
  boxplot(
    ps ~ factor(hh_c05_1),
    main = "Encuestados",
    ylab = "PS estimado",
    xlab = "Alfabetización"
  )
)

# Crear un boxplot de PS por alfabetización para no encuestados
with(
  hh_temp[hh_temp$Phone == "No",],
  boxplot(
    ps ~ factor(hh_c05_1),
    main = "No encuestados",
    ylab = "PS estimado",
    xlab = "Alfabetización"
  )
)

# Crear un boxplot de PS por estado de empleo para encuestados
with(
  hh_temp[hh_temp$Phone == "Yes",],
  boxplot(
    ps ~ factor(hh_e06_8a),
    main = "Encuestados",
    ylab = "PS estimado",
    xlab = "Estado de empleo"
  )
)

# Crear un boxplot de PS por estado de empleo para no encuestados
with(
  hh_temp[hh_temp$Phone == "No",],
  boxplot(
    ps ~ factor(hh_e06_8a),
    main = "No encuestados",
    ylab = "PS estimado",
    xlab = "Estado de empleo"
  )
)

# Cerrar el gráfico
dev.off()

# Indicadores de representatividad ---------------------------------------------

## A nivel nacional

# Resumir el conjunto de datos hh_temp a nivel nacional
summary_ps_national <- hh_temp %>%
  summarise(
    n = n(),
    # contar el número de observaciones
    urp = mean(ps),
    # calcular la media no ponderada de ps
    rho.bar = weighted.mean(ps, hh_wgt),
    # calcular la media ponderada de ps utilizando hh_wgt como pesos
    urr = sum(Phone == "Yes") / n(),
    # calcular la proporción no ponderada de Phone == "Yes"
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    # calcular la proporción ponderada de Phone == "Yes" utilizando hh_wgt como pesos
    sdp = sd(ps),
    # calcular la desviación estándar de ps
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt)) # calcular el valor de R-cuadrado-hat
  )

# Imprimir summary_ps_national
summary_ps_national

# Calcular la desviación estándar de ps en el conjunto de datos hh_temp
sd(hh_temp$ps)

# Calcular el número estimado de hogares en la población
N.hat <- sum(hh_temp$hh_wgt)

# Calcular la media ponderada de ps utilizando hh_wgt como pesos
rho.bar <- sum(hh_temp$ps * hh_temp$hh_wgt) / N.hat

# Calcular el valor de R-cuadrado-hat
R.hat <- 1 - 2 *
  sqrt((1 / (N.hat - 1)) * sum(hh_temp$hh_wgt * (hh_temp$ps - rho.bar) ^ 2))

# Imprimir las estadísticas resumidas de ps en el conjunto de datos hh_temp
summary(hh_temp$ps)

## A nivel desagregado

# Alfabetización
# Crea un diagrama de caja de ps por nivel de alfabetización
with(hh_temp, boxplot(ps ~ Literacy))

# Crea tres histogramas de ps, uno para cada nivel de alfabetización
par(mfrow = c(1, 3))
hist(hh_temp$ps)
hist(hh_temp$ps[hh_temp$Literacy == "Literate"])
hist(hh_temp$ps[hh_temp$Literacy == "Illiterate"])
dev.off()

# Resume el conjunto de datos hh_temp por nivel de alfabetización
summary_ps_Literacy <- hh_temp %>%
  group_by(Literacy) %>%
  summarise(
    n = n(),
    # cuenta el número de observaciones
    urp = mean(ps),
    # calcula la media no ponderada de ps
    rho.bar = weighted.mean(ps, hh_wgt),
    # calcula la media ponderada de ps usando hh_wgt como pesos
    urr = sum(Phone == "Yes") / n(),
    # calcula la proporción no ponderada de Phone == "Yes"
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    # calcula la proporción ponderada de Phone == "Yes" usando hh_wgt como pesos
    sdp = sd(ps),
    # calcula la desviación estándar de ps
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt)) # calcula el valor de R-cuadrado
  ) %>% as.data.frame()

# Imprime summary_ps_Literacy
summary_ps_Literacy

# Actividad

# Crea un diagrama de caja de la variable 'ps' agrupada por 'hh_e06_8a'
with(hh_temp, boxplot(ps ~ hh_e06_8a))

# Crea un histograma de la variable 'ps'
hist(hh_temp$ps)

# Crea una cuadrícula 2x2 de histogramas de la variable 'ps' agrupados por los valores de 'hh_e06_8a'
par(mfrow = c(2, 2))
hist(hh_temp$ps[hh_temp$hh_e06_8a == 1])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 2])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 3])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 5])

# Desactiva la cuadrícula de histogramas
dev.off()

# Crea una tabla resumen de estadísticas de la variable 'ps' agrupadas por los valores de 'hh_e06_8a'
summary_ps_Activity <- hh_temp %>%
  group_by(hh_e06_8a) %>%
  summarise(
    n = n(),
    urp = mean(ps),
    rho.bar = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    sdp = sd(ps),
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt))
  ) %>% as.data.frame()

# Imprime summary_ps_Activity
summary_ps_Activity

# Actividad y alfabetización

# Este código agrupa hh_temp por alfabetización y hh_e06_8a, y calcula estadísticas resumidas para cada grupo.
# El marco de datos resultante se guarda como summary_ps_LiteracyActivity.

summary_ps_LiteracyActivity <- hh_temp %>%
  group_by(Literacy, hh_e06_8a) %>%
  summarise(
    n = n(),
    urp = mean(ps),
    rho.bar = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    sdp = sd(ps),
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt))
  ) %>% as.data.frame()

# Imprimir el marco de datos resultante
summary_ps_LiteracyActivity

# Peso de variable de clase ------------------------------------------------

# Agrupar los datos de hh_temp por tres variables y calcular el conteo y peso total para cada grupo
hh_temp_agg <- hh_temp %>%
  group_by(hh_c05_1, hh_e06_8a, hh_b06_4) %>%
  summarise(n = n(),
            thh_wgt = sum(panelweight_2019))

# Filtrar los datos de hh_temp_agg en dos data frames basados en el valor de la variable hh_b06_4
hh_temp_agg_resp <- hh_temp_agg %>% filter(hh_b06_4 == 1)
hh_temp_agg_nonr <- hh_temp_agg %>% filter(hh_b06_4 == 2)

# Combinar los datos de hh_temp_agg_nonr con los datos de hh_temp_agg_resp, con las variables hh_c05_1, hh_e06_8a y hh_b06_4 eliminadas de los datos de hh_temp_agg_resp
Tabla11 <- data.frame(
  hh_temp_agg_nonr,
  hh_temp_agg_resp %>% ungroup %>% select(-hh_c05_1, -hh_e06_8a, -hh_b06_4)
)

# Calcular el peso total para los datos de hh_temp_agg_resp y hh_temp_agg_nonr
sum(hh_temp_agg_resp$thh_wgt)
sum(hh_temp_agg_nonr$thh_wgt)
sum(hh_temp$panelweight_2019)

# Agregar una nueva columna 'ac' a los datos de hh_temp_agg_resp, que calcula la proporción del peso total de ambos hh_temp_agg_resp y hh_temp_agg_nonr al peso total de hh_temp_agg_resp
hh_temp_agg_resp$ac <-
  (hh_temp_agg_resp$thh_wgt + hh_temp_agg_nonr$thh_wgt) / hh_temp_agg_resp$thh_wgt

# Unir los datos de hh_temp con los datos de hh_temp_agg_resp basados en tres variables y calcular el promedio ponderado de la variable panelweight_2019 para cada hogar en los datos de hh_temp_resp
hh_temp_resp <- hh_temp %>%
  inner_join(hh_temp_agg_resp %>% select(-n, -thh_wgt))

hh_temp_resp$wac <- with(hh_temp_resp, panelweight_2019 * ac * hhsize)
hh_temp_resp$w0 <- hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize

#!/bin/bash

# Calcular el peso total para las variables w0 y wac en los datos de hh_temp_resp
sum(hh_temp_resp$w0)
sum(hh_temp_resp$wac)

# Plotear las variables wac contra w0 en los datos de hh_temp_resp y agregar una línea con pendiente 1 e intercepción 0
plot(
  hh_temp_resp$w0,
  hh_temp_resp$wac,
  xlab = "w0",
  ylab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)
abline(a = 0, b = 1, col = 2)

# Crear histogramas de las variables panelweight_2019 y wac en los datos de hh_temp_resp
hist(hh_temp_resp$panelweight_2019)
hist(hh_temp_resp$wac)

# Clases para PS ----------------------------------------------------------

# Definir un modelo de puntaje de propensión utilizando la función pclass
psclass = pclass(
  factor(Phone) ~ 1 + factor(hh_c05_1) +  # Variables independientes
    factor(hh_e06_8a),
  # Variable independiente
  type = "unwtd",
  # Tipo de análisis a realizar (no ponderado)
  data = hh_temp  # Conjunto de datos a utilizar
)

# Crear una tabla de las clases de puntaje de propensión
table(psclass$p.class, useNA = "always")

# Resumir las propensiones
summary(psclass$propensities)

# Predecir el puntaje de propensión para cada observación en el conjunto de datos
hh_temp$ps <- predict(ps_fit1, type = "response")

# Clasificar cada observación en una clase de puntaje de propensión
hh_temp$class <- psclass$p.class

# Crear una tabla de resumen del conjunto de datos con cinco clases de puntaje de propensión
summary_ps_5classes <- hh_temp %>%
  group_by(class) %>%
  summarise(
    n = n(),
    urp = mean(ps),
    # Puntaje de propensión promedio no ponderado
    wrp = weighted.mean(ps, hh_wgt),
    # Puntaje de propensión promedio ponderado
    urr = sum(Phone == "Yes") / n(),
    # Tasa de respuesta no ponderada
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    # Tasa de respuesta ponderada
    mps = median(ps) # Puntaje de propensión mediano
  )

# Remover la variable n de la tabla de resumen
ps_classified <- summary_ps_5classes %>% select(-n)

# Unir el propensity score clasificado con el dataset original
hh_temp_resp <- hh_temp %>%
  inner_join(ps_classified) %>%
  inner_join(hh_temp_resp)

# Crear variables para los cálculos del propensity score ponderado
hh_temp_resp$w1ps <- with(hh_temp_resp, hhsize * panelweight_2019 / urp)
hh_temp_resp$w2ps <- with(hh_temp_resp, hhsize * panelweight_2019 / wrp)
hh_temp_resp$w3ps <- with(hh_temp_resp, hhsize * panelweight_2019 / urr)
hh_temp_resp$w4ps <- with(hh_temp_resp, hhsize * panelweight_2019 / wrr)

# Calcular sumas ponderadas de las diferentes variables
sum(hh_temp_resp$w0)
sum(hh_temp_resp$w1ps)
sum(hh_temp_resp$w2ps)
sum(hh_temp_resp$w3ps)
sum(hh_temp_resp$w4ps)

# Establecer la disposición de la gráfica en 2 x 2
par(mfrow = c(2, 2))

# Plot w1ps against wac and set the axis labels and limits
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w1ps,
  ylab = "w1ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)

# Add a diagonal line with intercept 0 and slope 1 in red
abline(a = 0, b = 1, col = 2)

# Plot w2ps against wac and set the axis labels and limits
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w2ps,
  ylab = "w2ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)

# Add a diagonal line with intercept 0 and slope 1 in red
abline(a = 0, b = 1, col = 2)

# Plot w3ps against wac and set the axis labels and limits
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w3ps,
  ylab = "w3ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)

# Add a diagonal line with intercept 0 and slope 1 in red
abline(a = 0, b = 1, col = 2)

# Plot w4ps against wac and set the axis labels and limits
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w4ps,
  ylab = "w4ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)

# Add a diagonal line with intercept 0 and slope 1 in red
abline(a = 0, b = 1, col = 2)

# Turn off the graphics device
dev.off()

# Calibration -------------------------------------------------------------

# Reside and Region
# Counts the number of observations in HFPS$reside
table(HFPS$reside)

# Counts the number of observations in hh_temp$reside
table(hh_temp$reside)

# Counts the number of observations in HFPS$region
table(HFPS$region)

# Counts the number of observations in hh_temp$region
table(hh_temp$region)

# Creates a contingency table with HFPS$reside and HFPS$region
table(paste(HFPS$reside, HFPS$region))

# Calculates the sum of HFPS$hh_wgt
sum(HFPS$hh_wgt)

# Groups HFPS by region and calculates the number of households and total weight
HFPS %>%
  group_by(region) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

# Groups HFPS by reside and calculates the number of households and total weight
HFPS %>%
  group_by(reside) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

# Groups HFPS by reside and region and calculates the number of households and total weight
totals <- HFPS %>%
  group_by(reside, region) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

# Crear una matriz de dominios usando las columnas reside y region de hh_temp_resp
x0s <- as.matrix(Domains(paste(hh_temp_resp$reside, hh_temp_resp$region)))

# Crear una matriz de valores de población total
tx0 <- as.matrix(totals$N)

# Sumar los valores en tx0
sum(tx0)

# Realizar la calibración utilizando el método lineal en las matrices x0s y tx0
g0k <- calib(x0s, d = hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize, total = tx0, method = "linear")

# Calcular los pesos calibrados y almacenarlos en hh_temp_resp$wcal
hh_temp_resp$wcal <- hh_temp_resp$panelweight_2019 * g0k * hh_temp_resp$hhsize

# Sumar los pesos calibrados en hh_temp_resp$wcal
sum(hh_temp_resp$wcal)

# Imprimir tx0
tx0

# Imprimir las sumas de columna de x0s multiplicadas por panelweight_2019 y hhsize
colSums(x0s * hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize)

# Calcular la relación de tx0 con las sumas de columna de x0s multiplicadas por panelweight_2019 y hhsize
tx0 / colSums(x0s * hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize)

# Crear una tabla de los pesos calibrados
table(g0k)

# Sumar los pesos en hh_temp_resp$wac
sum(hh_temp_resp$wac)

# Sumar los pesos calibrados en hh_temp_resp$wcal
sum(hh_temp_resp$wcal)

# Crear un gráfico de dispersión de wac vs. wcal
plot(hh_temp_resp$wac, hh_temp_resp$wcal, xlab = "wac", ylab = "wcal")

# Agregar una línea diagonal al gráfico
abline(a = 0, b = 1, col = 2)

# PS + Calibration --------------------------------------------------------

# Gráfico de w1ps vs wcal
plot(hh_temp_resp$w1ps, hh_temp_resp$wcal)

# Añadir una línea diagonal al gráfico
abline(a = 0, b = 1, col = 2)

# Crear un histograma de w1ps
hist(hh_temp_resp$w1ps)

# Crear un histograma de wcal
hist(hh_temp_resp$wcal)

# Aplicar una función de calibración a w1ps
g1k <- calib(x0s,
             d = hh_temp_resp$w1ps,
             total = tx0,
             method = "linear")

# Escalar w1ps por el factor de calibración
hh_temp_resp$wpscal <- hh_temp_resp$w1ps * g1k

# Sumar los valores de w0
sum(hh_temp_resp$w0)

# Sumar los valores de w1ps
sum(hh_temp_resp$w1ps)

# Sumar los valores de wcal
sum(hh_temp_resp$wcal)

# Sumar los valores de wpscal
sum(hh_temp_resp$wpscal)

# Gráfico de w1ps vs wpscal
plot(hh_temp_resp$w1ps,
     hh_temp_resp$wpscal,
     ylab = "wpscal",
     xlab = "wps")

# Añadir una línea diagonal al gráfico
abline(a = 0, b = 1, col = 2)

# Crear un histograma de w1ps
hist(hh_temp_resp$w1ps)

# Crear un histograma de wpscal
hist(hh_temp_resp$wpscal)

# Resumen de los valores de w1ps
summary(hh_temp_resp$w1ps)



# Final plots -------------------------------------------------------------

# Aquí está el código con comentarios agregados:

# Seleccionar las columnas de interés del dataframe hh_temp_resp
dataweights <- hh_temp_resp %>%
  select(w0, wac, w1ps, wcal, wpscal)

# Calcular la matriz de correlación de las columnas seleccionadas
cor(dataweights)

# Crear una matriz de gráficos de dispersión para las columnas seleccionadas
pairs.panels(dataweights)

