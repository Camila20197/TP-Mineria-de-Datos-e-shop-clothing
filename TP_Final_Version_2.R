library("knitr")
library("dplyr")
library("tidyverse")
library("skimr")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("networkD3")
library("arules")
library("arulesViz")
library("arulesSequences")
library("scales")


df = read.csv2("./data/e-shop clothing 2008.csv")

# ============================================================
# ACTIVIDAD a: Exploración de Datos
# ============================================================

#Primera visualizacion de los datos
str(df)

#Con el uso de la libreria skimr realizamos un summary que hace un analisis mas profundo de las variables.
#Realizamos un primer summary para ver que tenemos dentro

skim(df)

#Explorando el dataframe observamos que los datos están en formato single

# ============================================================
# LIMPIEZA Y PREPARACIÓN DE DATOS
# ============================================================

df <- df %>%
  # 1. Renombrar estrictamente las variables conflictivas
  rename(
    main_category = page.1..main.category.,
    clothing_model = page.2..clothing.model.,
    model_photography = model.photography,
    session_id = session.ID,
    price_category = price.2
  ) %>%
  
  # 2. Eliminar la variable 'year' (todos son 2008)
  select(-year) %>%
  
  # 3. Filtrar los códigos de dominio de red (43 al 47) en la variable country
  filter(!country %in% 43:47) %>%
  
  # 4. Transformar tipos de datos y asignar etiquetas en español
  mutate(
    # Transformaciones a factor con diccionario
    main_category = factor(main_category, 
                           levels = 1:4, 
                           labels = c("pantalones", "faldas", "blusas", "oferta")),
    
    colour = factor(colour, 
                    levels = 1:14, 
                    labels = c("beige", "negro", "azul", "marron", "bordo", "gris", "verde", 
                               "azul_marino", "multicolor", "oliva", "rosa", "rojo", "violeta", "blanco")),
    
    location = factor(location, 
                      levels = 1:6, 
                      labels = c("arriba_izq", "arriba_centro", "arriba_der", 
                                 "abajo_izq", "abajo_centro", "abajo_der")),
    
    model_photography = factor(model_photography, 
                               levels = 1:2, 
                               labels = c("de_frente", "de_perfil")),
    
    price_category = factor(price_category, 
                            levels = 1:2, 
                            labels = c("mayor_promedio", "menor_promedio")),
    
    country = factor(country, 
                     levels = 1:42, 
                     labels = c(
                       "Australia", "Austria", "Belgium", "British Virgin Islands", "Cayman Islands",
                       "Christmas Island", "Croatia", "Cyprus", "Czech Republic", "Denmark",
                       "Estonia", "unidentified", "Faroe Islands", "Finland", "France",
                       "Germany", "Greece", "Hungary", "Iceland", "India",
                       "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                       "Mexico", "Netherlands", "Norway", "Poland", "Portugal",
                       "Romania", "Russia", "San Marino", "Slovakia", "Slovenia",
                       "Spain", "Sweden", "Switzerland", "Ukraine", "United Arab Emirates",
                       "United Kingdom", "USA"
                     )),
    
    # Transformaciones a factor directas
    clothing_model = as.factor(clothing_model),
    session_id = as.factor(session_id),
    page = as.factor(page)
  )

# Verificar la estructura final
str(df)
skim(df)

# ============================================================
# ACTIVIDAD b: Análisis Gráfico
# ============================================================

#¿Los usuarios exploran mucho o abandonan rápido? 
#Para mejorar la visualización tomamos hasta los 100 clicks ya que el 75% de los datos se 
#concentra en entre los 1 a 12 click

summary(df$order)

clicks_sesion <- df %>%
  group_by(session_id) %>%
  summarise(order = n())

ggplot(clicks_sesion, aes(x = order)) +
  geom_histogram(
    bins = 50,
    fill = "#7FFFD4",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 100)) +  
  scale_x_continuous(
    name = "Cantidad de clicks",
    breaks = seq(0, 100, by = 5),
    minor_breaks = seq(0, 100, by = 5)
  ) +
  theme_minimal() +
  labs(
    title = "Distribución de clicks por sesión (0-100 clicks)",
    subtitle = "Comportamiento de navegación de los usuarios",
    y = "Frecuencia"
  )

#¿Qué tan diversa es la exploración de productos?
#Aegmentamos por cantidad de clicks ya que al querer hacerlos en un mismo gráfico 
#dificultaba la visualizacion de la información

exploracion <- df %>%
  group_by(session_id) %>%
  summarise(
    order = n(),
    productos = n_distinct(clothing_model)
  )

exploracion <- exploracion %>%
  mutate(rango_clicks = case_when(
    order <= 10 ~ "1-10 clicks",
    order <= 50 ~ "11-50 clicks",
    order <= 100 ~ "51-100 clicks",
    TRUE ~ ">100 clicks"
  ))

ggplot(exploracion, aes(x = order, y = productos)) +
  geom_point(alpha = 0.4, color = "#D95F0E") +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  facet_wrap(~rango_clicks, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Exploración de productos por sesión",
    subtitle = "Relación entre navegación y diversidad de productos",
    x = "Cantidad de clicks",
    y = "Productos distintos"
  )
#¿Qué categorías son más fuertes en cada país?
#Filtramos para ver los primeros 20 paises para facilitar la visualización del gráfico
top_paises <- df %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(country)

pais_categoria <- df %>%
  filter(country %in% top_paises) %>%
  count(country, main_category)

ggplot(pais_categoria,
       aes(x = main_category,
           y = reorder(country, n),
           fill = n)) +
  
  geom_tile(color = "white") +
  
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B"
  ) +
  
  labs(
    title = "Interés por categoría según país",
    subtitle = "Top 20 países con mayor actividad",
    x = "Categoría",
    y = "País",
    fill = "Clicks"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold")
  )

# ¿Qué países generan más tráfico?

# Crear líneas de grilla personalizadas
map_data <- world %>%
  left_join(sesiones_pais, by = c("name" = "country"))

# Crear grilla (asegurarse que custom_grid existe)
custom_grid <- st_graticule(
  lat = c(-60, -30, 0, 30, 60),  
  lon = c(-120, -60, 0, 60, 120)  
)
ggplot(map_data) +
  geom_sf(aes(fill = n), color = "white", size = 0.2) +
  geom_sf(data = custom_grid, color = "gray80", linewidth = 0.3, alpha = 0.5) +
  
  # Usamos gradientn para soportar múltiples colores
  scale_fill_gradientn(
    name = "Sesiones por país",
    
    # Aquí agregas los colores en orden (de menor a mayor valor)
    # Puedes usar nombres, códigos HEX o paletas predefinidas
    colors = c("#440154", "#31688E", "#35B779", "#FDE725", "#FF8C00", "#D73027"), 
    
    # Definimos los saltos de la escala de 2500 en 2500
    breaks = scales::breaks_width(2500), 
    
    # Mantenemos el formato de los números cortos (ej. 2.5K, 5K)
    labels = scales::label_number(scale_cut = scales::cut_short_scale()), 
    na.value = "gray60",
    
    # Hacemos la barra de la escala físicamente más ancha y controlamos su grosor
    guide = guide_colorbar(barwidth = 20, barheight = 1) 
  ) +
  
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 90),
    expand = FALSE
  ) +
  labs(
    title = "Distribución global de sesiones de usuario",
    subtitle = "Intensidad de color indica mayor actividad por país",
    caption = "Nota: Países en gris claro sin datos disponibles",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    # Eliminamos legend.key.width de aquí, ya que ahora lo controla guide_colorbar() arriba
    axis.text = element_text(size = 8, color = "gray40"),
    axis.title = element_text(size = 10, color = "gray50"),
    panel.grid.major = element_line(color = "gray40", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray30")
  )
# Precio vs Clicks

clicks_producto <- df %>%
  
  group_by(clothing_model, price_category) %>%
  
  summarise(
    order = n(),
    .groups = "drop"
  )

ggplot(clicks_producto,
       aes(x = price_category,
           y = order,
           fill = price_category)) +
  
  geom_boxplot(alpha = 0.7) +
  
  scale_fill_manual(values = c(
    "#2C7FB8",
    "#41AB5D"
  )) +
  
  labs(
    title = "Distribución de clicks según rango de precio",
    subtitle = "Comparación entre productos caros y económicos",
    x = "Precio",
    y = "Cantidad de clicks"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none"
  )

#categorías/países

# Contar combinaciones
heatmap_data <- df %>%
  count(location, main_category)

# Heatmap
ggplot(
  heatmap_data,
  
  aes(
    x = main_category,
    y = location,
    fill = n
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(label = n),
    color = "white",
    size = 4
  ) +
  
  scale_fill_gradient(
    low = "#9ECAE1",
    high = "#08519C"
  ) +
  
  labs(
    title = "Interacción entre ubicación y categoría",
    subtitle = "Distribución de clicks según posición en pantalla",
    x = "Categoría",
    y = "Ubicación",
    fill = "Clicks"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    
    panel.grid = element_blank()
  )

# =========================================================
# 1. Crear secuencias de navegación
# =========================================================

flujo <- df %>%
  
  arrange(session_id, order) %>%
  
  group_by(session_id) %>%
  
  mutate(
    siguiente_categoria = lead(main_category)
  ) %>%
  
  ungroup() %>%
  
  filter(!is.na(siguiente_categoria))

# =========================================================
# 2. Contar transiciones
# =========================================================

links <- flujo %>%
  
  count(
    main_category,
    siguiente_categoria,
    sort = TRUE
  )

# =========================================================
# 3. ELIMINAR auto-conexiones
# =========================================================

links <- links %>%
  
  filter(main_category != siguiente_categoria)

# =========================================================
# 4. Quedarse SOLO con las más frecuentes
# =========================================================

links <- links %>%
  
  slice_max(n, n = 6)

# =========================================================
# 5. Crear nodos
# =========================================================

nodos <- data.frame(
  name = unique(c(
    links$main_category,
    links$siguiente_categoria
  ))
)

# =========================================================
# 6. Crear índices
# =========================================================

links$source <- match(
  links$main_category,
  nodos$name
) - 1

links$target <- match(
  links$siguiente_categoria,
  nodos$name
) - 1

# =========================================================
# 7. Sankey limpio
# =========================================================

sankeyNetwork(
  Links = links,
  Nodes = nodos,
  
  Source = "source",
  Target = "target",
  Value = "n",
  NodeID = "name",
  
  fontSize = 18,
  nodeWidth = 40,
  sinksRight = TRUE
)

# ============================================================
# ACTIVIDAD c: Evolución de los clicks de navegación a lo largo de los meses
# ============================================================

evolucion_clicks <- df %>%
  group_by(month) %>%
  summarise(total_clicks = n()) %>%
  arrange(month) # Aseguramos que estén en orden cronológico

# Ver la tabla en consola
print(evolucion_clicks)

# 2. Visualizar la evolución con ggplot2
ggplot(evolucion_clicks, aes(x = as.factor(month), y = total_clicks, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 3) +
  theme_minimal() +
  labs(
    title = "Evolución de los Clicks de Navegación",
    subtitle = "De abril (4) a agosto (8) de 2008",
    x = "Mes",
    y = "Cantidad Total de Clicks"
  )

# ============================================================
# ACTIVIDAD d: Número de transacciones e ítems
# ============================================================

# Número de transacciones (sesiones únicas)
numero_transacciones <- n_distinct(df$session_id)

# Número total de ítems (total de clicks en el dataframe)
numero_items_total <- nrow(df)

cat("Total de transacciones (sesiones):", numero_transacciones, "\n")
cat("Total de ítems interactuados:", numero_items_total, "\n\n")

#. Detalle por Transacción (Preparación para Market Basket Analysis) ---

# Agrupamos por sesión para ver cuántos y cuáles ítems tiene cada transacción
detalle_transacciones <- df %>%
  group_by(session_id) %>%
  summarise(
    cantidad_items = n(), # Cuántos ítems tiene esta sesión
    # Juntamos los modelos de ropa consultados separados por coma
    lista_items = paste(clothing_model, collapse = ", ") 
  ) %>%
  ungroup()

# Ver las primeras 10 transacciones
head(detalle_transacciones, 10)

# ¿Cuál es el promedio de ítems por sesión?
summary(detalle_transacciones$cantidad_items)

# ============================================================
# ACTIVIDAD e: Conjunto de itemsets frecuentes
# ============================================================

# Se realizarán los items set frecuentes teniendo en cuenta las categarías principales 
# y otras características

# Primero análizamos solo las catagorías principales
df_general <- df %>%
  select(session_id, main_category) %>%
  # Eliminar duplicados en la misma sesión
  distinct(session_id, main_category) %>%
  group_by(session_id) %>%
  summarise(
    items = list(as.character(main_category)),
    .groups = "drop"
  )

# Convertir a transacciones
transacciones_general <- as(df_general$items, "transactions")

# Resumen
summary(transacciones_general)
# Verás: 4 ítems únicos (trousers, skirts, blouses, sale)

# Apriori
cat("APRIORI")
tiempo_apriori_general <- system.time({
  itemsets_general_apriori <- apriori(
    transacciones_general,
    parameter = list(support = 0.02, minlen = 2, target = "frequent")
  )
})

# ECLAT para el mismo conjunto
cat("ECLAT")
tiempo_eclat_general <- system.time({
  itemsets_general_eclat <- eclat(
    transacciones_general,
    parameter = list(support = 0.02, minlen = 2, target = "frequent")
  )
})

# Comparar resultados
cat("COMPARACIÓN GENERAL:")
cat("Apriori:", length(itemsets_general_apriori), "itemsets | Tiempo:", tiempo_apriori_general[3], "seg\n")
cat("ECLAT :", length(itemsets_general_eclat), "itemsets | Tiempo:", tiempo_eclat_general[3], "seg\n")

# Verificar si son iguales (deberían serlo)
if (length(itemsets_general_apriori) == length(itemsets_general_eclat)) {
  cat("Ambos algoritmos encontraron los mismos itemsets\n")
} else {
  cat("Diferencia en cantidad de itemsets. Revisar parámetros.")
}

# Mostrar top 10 de cada uno
cat("TOP 10 - APRIORI")
inspect(head(sort(itemsets_general_apriori, by = "support"), 10))

cat("TOP 10 - ECLAT ")
inspect(head(sort(itemsets_general_eclat, by = "support"), 10))

# Crear ítems (categoría + modelo)
df_categoria_modelo <- df %>%
  select(session_id, main_category, clothing_model) %>%
  mutate(item = paste(main_category, clothing_model, sep = "_")) %>%
  distinct(session_id, item) %>%  
  group_by(session_id) %>%
  summarise(
    items = list(item),
    .groups = "drop"
  )

# Convertir a transacciones
transacciones_categoria_modelo <- as(df_categoria_modelo$items, "transactions")

# Resumen
summary(transacciones_categoria_modelo)
# Verás: ~217 ítems únicos (todos los productos)

cat("categoría + modelo - APRIORI")
tiempo_apriori_especifico <- system.time({
  itemsets_especifico_apriori <- apriori(
    transacciones_categoria_modelo,
    parameter = list(support = 0.02, minlen = 2, target = "frequent")
  )
})

cat("categoría + modelo - ECLAT")
tiempo_eclat_especifico <- system.time({
  itemsets_especifico_eclat <- eclat(
    transacciones_categoria_modelo,
    parameter = list(support = 0.02, minlen = 2, target = "frequent")
  )
})

cat("Apriori:", length(itemsets_especifico_apriori), "itemsets | Tiempo:", tiempo_apriori_especifico[3], "seg\n")
cat("ECLAT :", length(itemsets_especifico_eclat), "itemsets | Tiempo:", tiempo_eclat_especifico[3], "seg\n")

# Mostrar top 10 de cada uno
cat("TOP 10 - APRIORI")
inspect(head(sort(itemsets_especifico_apriori, by = "support"), 10))

cat("TOP 10 - ECLAT ")
inspect(head(sort(itemsets_especifico_eclat, by = "support"), 10))


# Crear ítems (categoria + color) - SIN ACENTOS
df_categoria_color <- df %>%
  select(session_id, main_category, colour) %>%
  mutate(item = paste(main_category, colour, sep = "_")) %>%
  distinct(session_id, item) %>%
  group_by(session_id) %>%
  summarise(
    items = list(item),
    .groups = "drop"
  )

# Convertir a transacciones
transacciones_categoria_color <- as(df_categoria_color$items, "transactions")

# Resumen
summary(transacciones_categoria_color)

cat("(categoria + color) - APRIORI")
tiempo_apriori_moda <- system.time({
  itemsets_moda_apriori <- apriori(
    transacciones_categoria_color,
    parameter = list(support = 0.02, minlen = 2, maxlen = 3, target = "frequent")
  )
})

cat("(categoria + color) - ECLAT")
tiempo_eclat_moda <- system.time({
  itemsets_moda_eclat <- eclat(
    transacciones_categoria_color,
    parameter = list(support = 0.02, minlen = 2, maxlen = 3, target = "frequent")
  )
})

cat("Apriori:", length(itemsets_moda_apriori), "itemsets | Tiempo:", tiempo_apriori_moda[3], "seg\n")
cat("ECLAT :", length(itemsets_moda_eclat), "itemsets | Tiempo:", tiempo_eclat_moda[3], "seg\n")

# Top 10
cat("TOP 10 - APRIORI")
inspect(head(sort(itemsets_moda_apriori, by = "support"), 10))

cat("TOP 10 - ECLAT")
inspect(head(sort(itemsets_moda_eclat, by = "support"), 10))

# Analisis de colores
if (length(itemsets_moda_eclat) > 0) {
  df_moda_itemsets <- as(sort(itemsets_moda_eclat, by = "support"), "data.frame")
  
  colores_mencionados <- df_moda_itemsets %>%
    separate_rows(items, sep = ", ") %>%
    mutate(color = str_extract(items, "_[^_]+$") %>% str_remove("^_")) %>%
    filter(!is.na(color)) %>%
    count(color, sort = TRUE)
  
  cat(" COLORES MAS FRECUENTES EN COMBINACIONES:")
  print(head(colores_mencionados, 10))
}


# ============================================================
# ACTIVIDAD i: Minería de Secuencias Frecuentes
# ============================================================

# Preparar datos para secuencias (solo categorías)

df_secuencias <- df %>%
  select(session_id, order, main_category) %>%
  rename(sequenceID = session_id, eventID = order, item = main_category) %>%
  mutate(
    sequenceID = as.integer(sequenceID),
    eventID = as.integer(eventID),
    item = as.character(item),
    SIZE = 1
  ) %>%
  filter(!is.na(item))

# Guardar a archivo temporal
temp_file <- tempfile()
write.table(
  df_secuencias[, c("sequenceID", "eventID", "SIZE", "item")],
  file = temp_file,
  row.names = FALSE, col.names = FALSE, sep = " "
)

# Leer como secuencias
secuencias <- read_baskets(temp_file, info = c("sequenceID", "eventID", "SIZE"))

# Encontrar secuencias frecuentes con cSPADE

secuencias_frecuentes <- cspade(
  secuencias,
  parameter = list(
    support = 0.02,    # Soporte mínimo 2%
    maxlen = 5,        # Máximo 5 elementos
    mingap = 1,        # Mínimo 1 evento de diferencia
    maxgap = 10
  ),
  control = list(verbose = TRUE)
)

cat("Secuencias frecuentes encontradas:", length(secuencias_frecuentes), "\n")

# Ordenar y mostrar top 15
secuencias_ordenadas <- sort(secuencias_frecuentes, by = "support", decreasing = TRUE)
cat(" TOP 15 SECUENCIAS:")
inspect(secuencias_ordenadas[1:min(15, length(secuencias_ordenadas))])

# Generar reglas secuenciales 
reglas_secuenciales <- ruleInduction(
  secuencias_frecuentes,
  confidence = 0.3
)

cat("REGLAS SECUENCIALES:", length(reglas_secuenciales))
if (length(reglas_secuenciales) > 0) {
  reglas_ordenadas <- sort(reglas_secuenciales, by = "confidence", decreasing = TRUE)
  inspect(head(reglas_ordenadas, 10))
}

tabla_secuencias <- as(secuencias_ordenadas[1:15], "data.frame")

kable(tabla_secuencias, 
      caption = "Top 15 Secuencias Frecuentes de Navegación",
      row.names = FALSE)

# 4. Limpiar archivo temporal

# ============================================================
# ACTIVIDAD F: Encontrar las reglas de asociación de Polonia en blusas.
# ============================================================

# 1. Filtrar datos y agrupar ítems por sesión
lista_polonia <- df %>%
  filter(country == "Poland", main_category == "blusas") %>%
  group_by(session_id) %>%
  summarise(items = list(as.character(clothing_model)), .groups = "drop") %>%
  pull(items)

# 2. Convertir a transacciones
transacciones_polonia <- as(lista_polonia, "transactions")

# 3. Aplicar Apriori (soporte mínimo 2%, confianza 20%)
reglas_polonia <- apriori(
  transacciones_polonia,
  parameter = list(support = 0.02, confidence = 0.20, target = "rules")
)

# 4. Mostrar las 10 reglas con mayor soporte
reglas_polonia_top10 <- head(sort(reglas_polonia, by = "support"), 10)
cat("\n--- TOP 10 REGLAS: POLONIA (BLUSAS) ---\n")
inspect(reglas_polonia_top10)

tabla_polonia <- as(reglas_polonia_top10, "data.frame")

kable(tabla_polonia, 
      digits = 3,
      caption = "Top 10 Reglas de Asociación: Polonia (Blusas)",
      row.names = FALSE)

# ============================================================
# ACTIVIDAD G: Encontrar las reglas de asociación de Republica Checa en blusas.
# ============================================================
# 1. Filtrar datos y agrupar ítems por sesión
lista_checa <- df %>%
  filter(country == "Czech Republic", main_category == "blusas") %>%
  group_by(session_id) %>%
  summarise(items = list(as.character(clothing_model)), .groups = "drop") %>%
  pull(items)

# 2. Convertir a transacciones
transacciones_checa <- as(lista_checa, "transactions")

# 3. Aplicar Apriori (soporte mínimo 4%, confianza 25%)
reglas_checa <- apriori(
  transacciones_checa,
  parameter = list(support = 0.04, confidence = 0.25, target = "rules")
)

# 4. Mostrar las 10 reglas con mayor soporte
reglas_checa_top10 <- head(sort(reglas_checa, by = "support"), 10)
cat("\n--- TOP 10 REGLAS: REP. CHECA (BLUSAS) ---\n")
inspect(reglas_checa_top10)

tabla_checa <- as(reglas_checa_top10, "data.frame")

kable(tabla_checa, 
      digits = 3,
      caption = "Top 10 Reglas de Asociación: República Checa (Blusas)",
      row.names = FALSE)


