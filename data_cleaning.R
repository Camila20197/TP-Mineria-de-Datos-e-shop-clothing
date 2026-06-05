library("dplyr")
library("tidyverse")
library("skimr")

library("sf")
library("rnaturalearth")
library("ggplot2")

df = read.csv2("./data/e-shop clothing 2008.csv")

#Primera visualizacion de los datos
str(df)

#Con el uso de la libreria skimr realizamos un summary que hace un analisis mas profundo de las variables.
#Realizamos un primer summary para ver que tenemos dentro

skim(df)

#Explorando el dataframe observamos que los datos están en formato single

names(df)[names(df) == "page.1..main.category."] <- "main_category"
names(df)[names(df) == "page.2..clothing.model."] <- "clothing_model"
names(df)[names(df) == "model.photography"] <- "model_photography"
names(df)[names(df) == "session.ID"] <- "session_id"


# 1. Categoría Principal de la Ropa
df$main_category <- factor(
  df$main_category,
  levels = c(1, 2, 3, 4),
  labels = c("Trousers", "Skirts", "Blouses", "Sale")
)

# 2. Color del Producto
df$colour <- factor(
  df$colour,
  levels = 1:14,
  labels = c("Beige", "Black", "Blue", "Brown", "Burgundy",
             "Gray", "Green", "Navy Blue", "Polyamide",
             "Purple", "Red", "White", "Yellow", "Other")
)

# 3. Ubicación en la Pantalla
df$location <- factor(
  df$location,
  levels = 1:6,
  labels = c("Top left", "Top middle", "Top right",
             "Bottom left", "Bottom middle", "Bottom right")
)

# 4. Fotografía del Modelo
df$model_photography <- factor(
  df$model_photography,
  levels = c(1, 2),
  labels = c("En face", "Profile")
)

# 5. Categoría de Precio (1 = Mayor al promedio, 2 = Menor al promedio)
df$price.2 <- factor(
  df$price.2,
  levels = c(1, 2),
  labels = c("Above average", "Below average")
)

# 6. País de origen (Country)
# Convierte los códigos del 1 al 47 en sus nombres reales correspondientes
df$country <- factor(
  df$country,
  levels = 1:47,
  labels = c(
    "Australia", "Austria", "Belgium", "British Virgin Islands",
    "Cayman Islands", "Christmas Island", "Croatia", "Cyprus",
    "Czech Republic", "Denmark", "Estonia", "Unidentified",
    "Faroe Islands", "Finland", "France", "Germany",
    "Greece", "Hungary", "Iceland", "India",
    "Ireland", "Italy", "Latvia", "Lithuania",
    "Luxembourg", "Mexico", "Netherlands", "Norway",
    "Poland", "Portugal", "Romania", "Russia",
    "San Marino", "Slovakia", "Slovenia", "Spain",
    "Sweden", "Switzerland", "Ukraine", "United Arab Emirates",
    "United Kingdom", "USA", "biz", "com",
    "int", "net", "org"
  )
)

# 7. Modelo de Ropa (Ej. "A13")
# Ya contiene letras, pero es bueno que R lo trate como categoría.
df$clothing_model <- as.factor(df$clothing_model)

# Revisamos la estructura del dataframe para confirmar los cambios
str(df$colour)

summary(df$country)

#Eliminamos la variable año ya que no aporta información relevante a nuestro estudio
df <- df %>% select(-year)

#Pasamos la variable session id a factor
df$session_id <- as.factor(df$session_id)

#Controlamos los cambios realizados
str(df)

#Volvemos a realizar un análisis estadístico a los datos luego de ser procesados 
skim(df)


#Analisis gráfico


#¿Los usuarios exploran mucho o abandonan rápido?

summary(df$order)

clicks_sesion <- df %>%
  group_by(session_id) %>%
  summarise(order = n())


ggplot(clicks_sesion, aes(x = order)) +
  geom_histogram(
    bins = 30,
    fill = "#7FFFD4",
    color = "white"
  ) +
  labs(
    title = "Distribución de clicks por sesión",
    x = "Cantidad de clicks",
    y = "Frecuencia"
  ) +
  theme_minimal()

#¿Qué tan diversa es la exploración de productos?

exploracion <- df %>%
  group_by(session_id) %>%
  summarise(
    order = n(),
    productos = n_distinct(clothing_model)
  )

ggplot(exploracion,
       aes(x = order,
           y = productos)) +
  geom_point(
    alpha = 0.4,
    color = "#D95F0E"
  ) +
  labs(
    title = "Exploración de productos por sesión",
    x = "Clicks",
    y = "Productos distintos"
  ) +
  theme_minimal()

#¿Qué categorías son más fuertes en cada país?


pais_categoria <- df %>%
  count(country, main_category)

ggplot(pais_categoria,
       aes(x = main_category,
           y = country,
           fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B"
  ) +
  labs(
    title = "Interés por categoría según país",
    x = "Categoría",
    y = "País",
    fill = "Clicks"
  ) +
  theme_minimal()