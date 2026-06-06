library("dplyr")
library("tidyverse")
library("skimr")

df = read.csv2("./data/e-shop clothing 2008.csv")

# Leer los datos
df <- read.csv2("./data/e-shop clothing 2008.csv")

#Primera visualizacion de los datos
str(df)

#Con el uso de la libreria skimr realizamos un summary que hace un analisis mas profundo de las variables.
#Realizamos un primer summary para ver que tenemos dentro

skim(df)

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

#C. Evolución de los clicks de navegación a lo largo de los meses ---

evolucion_clicks <- df_clean %>%
  group_by(mes) %>%
  summarise(total_clicks = n()) %>%
  arrange(mes) # Aseguramos que estén en orden cronológico

# Ver la tabla en consola
print(evolucion_clicks)

# 2. Visualizar la evolución con ggplot2
ggplot(evolucion_clicks, aes(x = as.factor(mes), y = total_clicks, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(
    title = "Evolución de los Clicks de Navegación",
    subtitle = "De abril (4) a agosto (8) de 2008",
    x = "Mes",
    y = "Cantidad Total de Clicks"
  )

#D. Número de transacciones e ítems ---

# Número de transacciones (sesiones únicas)
numero_transacciones <- n_distinct(df_clean$id_sesion)

# Número total de ítems (total de clicks en el dataframe)
numero_items_total <- nrow(df_clean)

cat("Total de transacciones (sesiones):", numero_transacciones, "\n")
cat("Total de ítems interactuados:", numero_items_total, "\n\n")

#. Detalle por Transacción (Preparación para Market Basket Analysis) ---

# Agrupamos por sesión para ver cuántos y cuáles ítems tiene cada transacción
detalle_transacciones <- df_clean %>%
  group_by(id_sesion) %>%
  summarise(
    cantidad_items = n(), # Cuántos ítems tiene esta sesión
    # Juntamos los modelos de ropa consultados separados por coma
    lista_items = paste(modelo_ropa, collapse = ", ") 
  ) %>%
  ungroup()

# Ver las primeras 10 transacciones
head(detalle_transacciones, 10)

# Un pequeño extra: ¿Cuál es el promedio de ítems por sesión?
summary(detalle_transacciones$cantidad_items)