# Instalación de paquete
install.packages("tidyverse")
library(tidyverse)

# Instalación de paquete
install.packages("readxl")
library(readxl)

# Carga de datos y analisis incial
data <- read_csv("Diplomado en Analítica y Ciencia de Datos/R Studio/Titanicv2.csv")
glimpse(data)
summary(data)
head(data)

## Análisis de las características de los pasajeros

# Visualización de los datos con histograma de edad
ggplot(data, aes(x = Age )) +
  geom_histogram(binwidth =5, fill = "pink", color = "yellow") +
  labs(title = "Histograma de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

# Segundo histograma para el precio del boleto
ggplot(data, aes(x = Fare)) +
  geom_histogram(binwidth = 10, fill = "purple", color = "orange") +
  labs(title = "Histograma del Precio del Boleto", x = "Precio", y = "Frecuencia") +
  theme_minimal()

# Análisis de dispersión para ver conformación de familias de los pasajeros
ggplot(data, aes(x = SibSp, y = Parch)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Gráfico de Dispersión: Número de Hermanos/Esposas vs. Número de Padres/Hijos",
       x = "Número de Hermanos/Esposas",
       y = "Número de Padres/Hijos") +
  theme_minimal()

# Diagráma de pastel para anlisis de clases sociales
class_distribution <- data %>%
  group_by(Pclass) %>%
  summarize(Count = n(), .groups = 'drop')

ggplot(class_distribution, aes(x = "", y = Count, fill = factor(Pclass))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribución de Clases Sociales de los Pasajeros",
       fill = "Clase") +
  theme_minimal()

## Análisis de sobrevivientes

# Contar el número total de pasajeros antes de la limpieza
total_passengers <- nrow(data)
print(paste("Número total de pasajeros (antes de la limpieza):", total_passengers))

total_survivors <- data %>%
  filter(Survived == "Yes") %>%
  count()
print(paste("Número total de pasajeros sobrevivientes:", total_survivors$n))

women_survivors <- data %>%
  filter(Sex == "female", Survived == "Yes") %>%
  count()
print(paste("Número de mujeres sobrevivientes:", women_survivors$n))

men_survivors <- data %>%
  filter(Sex == "male", Survived == "Yes") %>%
  count()
print(paste("Número de hombres sobrevivientes:", men_survivors$n))
