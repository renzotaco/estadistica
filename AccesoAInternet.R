install.packages(c("apaTables","tseries","lubridate","tidyverse","car",
                   "astsa","foreign","timsac","vars","lmtest","mFilter","dynlm","nlme",
                   "lmtest","broom","kableExtra","knitr","MASS","parallel","mlogit","dplyr",
                   "tidyr", "forecast", "fpp2", "stats", "quantmod"), dep=T)

library(tseries)
library(lubridate)
library(tidyverse)
library(car)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(lmtest)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(car)
library(mlogit)
library(dplyr)
library(tidyr)
library(forecast)
library(fpp2)
library(stats)
library(quantmod)
library(ggplot2)
library(ggrepel)

setwd("C:/Users/Renzo Taco/OneDrive - UNIVERSIDAD PRIVADA DE TACNA/SPSS/R/SeriesIncore")

df <- read.csv("accesoainternet.csv", sep=";")
df
datos <- select(df, "Región","Valor","Año")
str(df)
str(datos)
datos
## filtrar solo por región
filtered_df <- datos %>% filter(Año == 2013)
print(filtered_df)

datos_wide <- datos %>%
  pivot_wider(
    names_from = Año,    # Los años se convertirán en nombres de columnas
    values_from = Valor, # Los valores que llenarán las nuevas columnas
    values_fill = list(valor = 0) # Opcional: rellena NA con 0
  )

# Ver resultado
print(datos_wide)
datos_wide

# Crear el gráfico
ggplot(datos, aes(x = Año, y = Valor, color = Región, group = Región)) +
  geom_line() +
  geom_point() +
  labs(title = "Porcentaje de Analfabetismo por Región (2012-2023)",
       x = "Año",
       y = "Porcentaje de Analfabetismo") +
  theme_minimal()

###############
data_summary <- datos %>%
  group_by(Región) %>%
  summarise(Analfabetismo_Promedio = mean(Valor))

region_mayor <- data_summary %>%
  filter(Analfabetismo_Promedio == max(Analfabetismo_Promedio)) %>%
  pull(Región)

region_menor <- data_summary %>%
  filter(Analfabetismo_Promedio == min(Analfabetismo_Promedio)) %>%
  pull(Región)

cat("Región con mayor analfabetismo promedio:", region_mayor, "\n")
cat("Región con menor analfabetismo promedio:", region_menor, "\n")

#################

# Crear el gráfico
# Crear el gráfico con facet_wrap
ggplot(df, aes(x = Año, y = Valor, group = Región)) +
  geom_line() +
  geom_point() +
  labs(title = "Porcentaje de Acceso a Internet por Región (2012-2023)",
       x = "Año",
       y = "Porcentaje de Acceso a Internet") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Región)

################

# Calcular la región con mayor y menor analfabetismo promedio
data_summary <- df %>%
  group_by(Región) %>%
  summarise(Analfabetismo_Promedio = mean(Valor))

region_mayor <- data_summary %>%
  filter(Analfabetismo_Promedio == max(Analfabetismo_Promedio)) %>%
  pull(Región)

region_menor <- data_summary %>%
  filter(Analfabetismo_Promedio == min(Analfabetismo_Promedio)) %>%
  pull(Región)

# Crear el gráfico con facet_wrap y colores diferentes
str(df)
ggplot(df, aes(x = Año, y = Valor, group = Región, color = Región)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Porcentaje de Analfabetismo por Región (2012-2023)",
       x = "Año",
       y = "Porcentaje de Analfabetismo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Región) +
  scale_color_manual(values = c("Región1" = "blue", "Región2" = "green", "Región3" = "red")) +
  
  cat("Región con mayor analfabetismo promedio:", region_mayor, "\n")
cat("Región con menor analfabetismo promedio:", region_menor, "\n")

##################

# Crear el gráfico con facet_wrap y colores diferentes
ggplot(df, aes(x = Año, y = Valor, group = Región, color = Región)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Porcentaje de Acceso a Internet por Región (2012-2023)",
       x = "Año",
       y = "Porcentaje de Acceso a Internet") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Región) +
  scale_color_manual(values = c("Lima*" = "blue", "Huancavelica" = "green", "Moquegua" = "red", "Tacna" = "purple"))

#####################
# Calcular el promedio de analfabetismo para cada región
data_summary <- df %>%
  group_by(Región) %>%
  summarise(Analfabetismo_Promedio = mean(Valor)) %>%
  arrange(Analfabetismo_Promedio)

# Mostrar la tabla ordenada de menor a mayor analfabetismo
print(data_summary)
#######################

# Calcular el promedio de analfabetismo para cada región
data_summary <- df %>%
  group_by(Región) %>%
  summarise(Analfabetismo_Promedio = mean(Valor)) %>%
  arrange(Analfabetismo_Promedio)

# Crear la tabla gráfica
ggplot(data_summary, aes(x = reorder(Región, Analfabetismo_Promedio), y = Analfabetismo_Promedio, fill = Región)) +
  geom_bar(stat = "identity") +
  labs(title = "Promedio de Accesoa Internet por Región (2012-2023)",
       x = "Región",
       y = "Promedio de Accesoa Internet (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Lima*" = "blue", "Huancavelica" = "green", "Moquegua" = "red", "Tacna" = "purple"))

##################

data <- read.csv("cor_anal_inter.csv", sep=";")
data
str(data)
# Crear el gráfico de dispersión
ggplot(data, aes(x = Analfabetismo, y = Internet)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Correlación entre Analfabetismo y Acceso a Internet",
       x = "Acceso a Internet (%)",
       y = "Analfabetismo (%)") +
  theme_minimal()
