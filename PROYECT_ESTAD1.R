install.packages("ggplot2")
install.packages("tidyr")
install.packages("sf")
install.packages("tmap")
install.packages("scales")
install.packages("nls2")
install.packages("spdep")
install.packages("tseries")
install.packages("ggfortify")
install.packages("lmtest")

library(readxl)
library(ggplot2)
library(tidyr)
library(sf)
library(tmap)
library(scales)
library(dplyr)
library(nls2)
library(spdep)
library(tseries)
library(ggfortify)
library(lmtest)

# Cargar el archivo Excel
file_path <- "PROYECT_ESTAD.xlsx"
datos <- read_excel(file_path)

# Ver primeras filas
head(datos)

#Estadistica descriptiva (Excluye columnas de códigos, nombres, latitud y longitud)
summary(datos[,-c(1,2,19,20)]) 

#### Convertir los datos en formato largo para análisis y Convertir a formato numérico
datos_largos <- tidyr::pivot_longer(datos, cols = starts_with("POBTOT"), names_to = "Año", values_to = "Poblacion")
datos_largos$Año <- as.numeric(gsub("POBTOT", "", datos_largos$Año))

# Histogramas y Boxplots
p1 <- ggplot(datos_largos, aes(x = Poblacion)) +
  geom_histogram(binwidth = 5000, fill = "blue", alpha = 0.7) +
  theme_minimal() + scale_y_continuous(labels = scales::comma) +
  labs(title = "Histograma distribución de la Población", x = "Población", y = "Frecuencia")

p2 <- ggplot(datos_largos, aes(x = as.factor(Año), y = Poblacion)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  theme_minimal() + scale_y_continuous(labels = scales::comma) +
  labs(title = "Boxplot de la Población por Año", x = "Año", y = "Población")

print(p1)
print(p2)

#Graficas de dispersión
ggplot(datos_largos, aes(x = Año, y = Poblacion, group = NOMBRE, color = as.factor(NOMBRE))) + 
  geom_line(alpha = 0.7, linewidth = 1) + theme_minimal() + scale_y_continuous(labels = scales::comma) +
  labs(title = "Evolución de la Población por Municipio (2020-2035)",
       x = "Año", y = "Población", color = "Municipio")

####mapas tematicos

# Cargar el shapefile del Cesar
cesar_map1 <- st_read("C:/CESAR_MUNICIPIOS/CESAR_MUNICIPIOS.shp")

# Unir datos de población con el shapefile 
cesar_map1 <- cesar_map1 %>%
  inner_join(datos, by = "COD_MPIO")

cesar_map1_largo <- cesar_map1 %>%
  pivot_longer(cols = starts_with("POBTOT"), 
               names_to = "Año", 
               values_to = "Poblacion") %>%
  mutate(Año = as.numeric(gsub("POBTOT", "", Año)))  # Extraer año del nombre de la columna

# Graficar mapas con facet_wrap
mapa <- ggplot(cesar_map1_largo) +
  geom_sf(aes(fill = Poblacion)) +
  facet_wrap(~Año) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Evolución de la Población en el Cesar")

print(mapa)

###MODELO DE REGRESIÓN

# Seleccionar un municipio de ejemplo (por ejemplo, Valledupar, Aguachica o Chimichagua)
valledupar <- subset(datos_largos, NOMBRE == "Valledupar")
aguachica <- subset(datos_largos, NOMBRE == "Aguachica")
chimichagua <- subset(datos_largos, NOMBRE == "Chimichagua")

# Modelo de regresión lineal
mlineal_valledupar <- lm(Poblacion ~ Año, data = valledupar)
mlineal_aguachica <- lm(Poblacion ~ Año, data = aguachica)
mlineal_chimichagua <- lm(Poblacion ~ Año, data = chimichagua)

# Crear predicciones para el futuro
futuro <- data.frame(Año = 2020:2040)
futuro$Poblacionlineal_valledupar <- predict(mlineal_valledupar, newdata = futuro)
futuro$Poblacionlineal_aguachica <- predict(mlineal_aguachica, newdata = futuro)
futuro$Poblacionlineal_chimichagua <- predict(mlineal_chimichagua, newdata = futuro)

# Visualización de resultados
ggplot() +
  geom_point(data = valledupar, aes(x = Año, y = Poblacion), color = "black") +
  geom_line(data = futuro, aes(x = Año, y = Poblacionlineal_valledupar), color = "blue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Proyección Poblacional de Valledupar",
       x = "Año", y = "Población",
       caption = "Modelo: Lineal (azul)")
ggplot() +
  geom_point(data = aguachica, aes(x = Año, y = Poblacion), color = "black") +
  geom_line(data = futuro, aes(x = Año, y = Poblacionlineal_aguachica), color = "blue") + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Proyección Poblacional de Aguachica",
       x = "Año", y = "Población",
       caption = "Modelo: Lineal (azul)")
ggplot() +
  geom_point(data = chimichagua, aes(x = Año, y = Poblacion), color = "black") +
  geom_line(data = futuro, aes(x = Año, y = Poblacionlineal_chimichagua), color = "blue") + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Proyección Poblacional de Chimichagua",
       x = "Año", y = "Población",
       caption = "Modelo: Lineal (azul)")


# Correlograma de residuos
acf(residuals(mlineal_valledupar), main = "Residuos del Modelo de Regresión Valledupar")

acf(residuals(mlineal_aguachica), main = "Residuos del Modelo de Regresión Aguachica")

acf(residuals(mlineal_chimichagua), main = "Residuos del Modelo de Regresión Chimichagua")


### AUTOCORRELACIÓN ESPACIAL

# Crear matriz de vecinos espaciales con 4 vecinos más cercanos
coordenadas <- cbind(datos$LONGITUD, datos$LATITUD)
nb <- knn2nb(knearneigh(coordenadas, k = 4))
listw <- nb2listw(nb, style = "W")

# Prueba de Moran para cada año
años <- c("2020", "2025", "2030", "2035")
for (a in años) {
  variable <- paste0("POBTOT", a)
  print(paste("Moran's I para", a))
  print(moran.test(datos[[variable]], listw))
}

# Diagrama de dispersión de Moran
moran.plot(datos$POBTOT2020, listw, main = "Diagrama de Dispersión de Moran (2020)")
moran.plot(datos$POBTOT2025, listw, main = "Diagrama de Dispersión de Moran (2025)")
moran.plot(datos$POBTOT2030, listw, main = "Diagrama de Dispersión de Moran (2030)")
moran.plot(datos$POBTOT2035, listw, main = "Diagrama de Dispersión de Moran (2035)")

# Mapas de Clusters de LISA para cada año

for (a in años) {
  variable <- paste0("POBTOT", a)
  localmoran_res <- localmoran(datos[[variable]], listw)
  
  # Asegurar que los valores de LISA se asignen correctamente al dataset espacial
  cesar_map1$LISA <- localmoran_res[, 1]
  
  # Crear y mostrar el mapa
  p <- ggplot(cesar_map1) +
    geom_sf(aes(fill = LISA)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(title = paste("Mapa de Clusters de Moran (", a, ")", sep = ""), 
         fill = "Índice Local de Moran")
  
  print(p)  # Asegurar que los gráficos se muestren dentro del bucle
}

for (a in años) {
  variable <- paste0("POBTOT", a)
  localmoran_res <- localmoran(datos[[variable]], listw)
  datos$LISA <- localmoran_res[, 1]
  
  cesar_map1$LISA <- localmoran_res[, 1]
  
 p <- ggplot(cesar_map1) +
    geom_sf(aes(fill = LISA)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(title = paste("Mapa de Clusters de Moran (", a, ")", sep = ""), 
         fill = "Índice Local de Moran")
 print(p)
}
