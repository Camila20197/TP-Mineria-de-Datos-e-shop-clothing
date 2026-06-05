library("dplyr")
library("tidyverse")
library("skimr")

df = read.csv2("./data/e-shop clothing 2008.csv")

#Primera visualizacion de los datos
str(df)

#Con el uso de la libreria skimr realizamos un summary que hace un analisis mas profundo de las variables.
#Realizamos un primer summary para ver que tenemos dentro

skim(df)


#_______________________________________________________________________________________________________
#Esta en formato single

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
-------------------------------------------------------------------------------------------
library(dplyr)
library(tidyverse)

# Leer los datos
df <- read.csv2("./data/e-shop clothing 2008.csv")

# Diccionario de mapeo de países
country_mapping <- tibble(
  pais = 1:42,
  nombre_pais = c(
    "Australia", "Austria", "Belgium", "British Virgin Islands", "Cayman Islands",
    "Christmas Island", "Croatia", "Cyprus", "Czech Republic", "Denmark",
    "Estonia", "unidentified", "Faroe Islands", "Finland", "France",
    "Germany", "Greece", "Hungary", "Iceland", "India",
    "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
    "Mexico", "Netherlands", "Norway", "Poland", "Portugal",
    "Romania", "Russia", "San Marino", "Slovakia", "Slovenia",
    "Spain", "Sweden", "Switzerland", "Ukraine", "United Arab Emirates",
    "United Kingdom", "USA"
  )
)

# Pipeline final de limpieza, transformación y normalización
df_clean <- df %>%
  # 1. Renombrar columnas
  rename(
    anio = year,
    mes = month,
    dia = day,
    orden = order,
    pais = country,
    id_sesion = session.ID,
    categoria_principal = page.1..main.category.,
    modelo_ropa = page.2..clothing.model.,
    color = colour,
    ubicacion_pagina = location,
    tipo_foto = model.photography,
    precio = price,
    precio_sobre_promedio = price.2,
    numero_pagina = page
  ) %>%
  
  # 2. Filtrar dominios de red de la variable país
  filter(!pais %in% 43:47) %>%
  
  # 3. Mapear nombres de países
  left_join(country_mapping, by = "pais") %>%
  mutate(pais = nombre_pais) %>%
  select(-nombre_pais) %>%
  
  # 4. Mapear valores del diccionario y convertir a FACTOR simultáneamente
  mutate(
    categoria_principal = factor(categoria_principal, 
                                 levels = 1:4, 
                                 labels = c("pantalones", "faldas", "blusas", "oferta")),
    
    color = factor(color, 
                   levels = 1:14, 
                   labels = c("beige", "negro", "azul", "marron", "bordo", "gris", "verde", 
                              "azul_marino", "multicolor", "oliva", "rosa", "rojo", "violeta", "blanco")),
    
    ubicacion_pagina = factor(ubicacion_pagina, 
                              levels = 1:6, 
                              labels = c("arriba_izq", "arriba_centro", "arriba_der", 
                                         "abajo_izq", "abajo_centro", "abajo_der")),
    
    tipo_foto = factor(tipo_foto, 
                       levels = 1:2, 
                       labels = c("de_frente", "de_perfil")),
    
    precio_sobre_promedio = factor(precio_sobre_promedio, 
                                   levels = 1:2, 
                                   labels = c("si", "no")),
    
    # Las que no necesitan cambio de nombre, solo las convertimos a factor
    pais = as.factor(pais),
    modelo_ropa = as.factor(modelo_ropa),
    numero_pagina = as.factor(numero_pagina)
  ) 
# Verificar la estructura final
str(df_clean)
skim(df_clean)