

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)

emergencias2019 <- read.csv("C:/Users/tania/Documents/estadistica/Emergencias_UNGRD_2019_20240312.csv")


sapply(emergencias2019, function(x) sum(is.na(x)))


emergenciascompleto <- emergencias2019 [!is.na (emergencias2019$HERIDOS),]


sapply(emergenciascompleto, function(x) sum(is.na(x)))


##cálculo media, varianza y desviación estandar 

##escoger una variable ej heridos

###media

mediaheridos <- mean(emergenciascompleto$HERIDOS)

mediaheridos


##varianza 


varheridos <- var(emergenciascompleto$HERIDOS)

varheridos

##desviacionestandar

desestandar <- sd(emergenciascompleto$HERIDOS)


desestandar


##tabladefrecuencias
## ¿cuantas veces se repite el mismo dato en una columna?
##pipe CRT + SHIFT +M


##Calcular la longitud de la base de datos
n <- nrow(emergenciascompleto)

n


frec_dpto <- emergenciascompleto %>% 
  group_by(DEPARTAMENTO) %>% 
  summarise(Frec_abs=n(), Frec_rel= (Frec_abs/n)*100)


##acumulados con mutate

frec_dpto <- frec_dpto %>% 
  mutate(Frec_abs_acum=cumsum(Frec_abs) , Frec_rel_acum=cumsum(Frec_rel))

##tabla de frecuencia compuesta 
##ejemplo, cantidad total de fallecidos x emergencia registrada, promedio de heridos 
## funcion mean para promedio, funcion sum para total
### nos interesa la columna de eventos 

eventofallecidos <- emergenciascompleto %>% 
  group_by(EVENTO) %>% 
  summarise(Fallecidos_tot=sum(FALLECIDOS),
            Heridos_prom=mean(HERIDOS),
            desap_prom=mean(DESAPARECIDOS))
      

##GRÁFICOS

##hay varios themes

barras1 <- ggplot(frec_dpto,
                 aes(x=DEPARTAMENTO,y=Frec_abs,fill=DEPARTAMENTO))+ geom_bar(stat="identity", width=1) 

barras1



barras <- ggplot(frec_dpto, aes(x = DEPARTAMENTO, y = Frec_abs, fill = DEPARTAMENTO)) +
  geom_bar(stat = "identity", width = 0.7) + # Cambié el ancho de las barras para mejor visualización
  theme_minimal() +                         # Tema limpio y moderno
  labs(title = "Frecuencia por Departamento", # Título del gráfico
       x = "Departamento",                   # Etiqueta para el eje X
       y = "Frecuencia Absoluta",            # Etiqueta para el eje Y
       fill = "Departamento") +              # Leyenda para los colores
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotación de texto en el eje X para legibilidad 


##SI QUEREMOS LOS COLORES MANUALMENTE:

##scale_fill_manual(values = c("red", "blue", "green", "orange", "purple")) + # Cambia los colores

barras

pastel1 <-  ggplot(frec_dpto, aes(x="" , y=Frec_abs, fill=DEPARTAMENTO)) + 
  geom_bar(stat="identity", width = 1)+
  coord_polar("y", start =0)

pastel1


pastel

pastel <- ggplot(frec_dpto, aes(x = "", y = Frec_abs, fill = DEPARTAMENTO)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución por Departamento") +
  theme_void() + # Elimina las cuadrículas y ejes
  theme(legend.position = "right") # Posiciona la leyenda a la derecha

pastel <- pastel +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


pastel



pastel2 <- ggplot(frec_dpto, aes(x =3, y = Frec_abs, fill = DEPARTAMENTO)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  xlim(2, 4) +
  labs(title = "Distribución por Departamento") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), # Centra el título
    legend.position = "right")

pastel2



###Más cerca del borde: Asignar x = 2 significa que todas las barras estarán centradas en el valor 2 del eje x, dejando un espacio entre el centro (x = 0) y donde comienzan las barras.
###Hueco ajustable: El valor de x junto con xlim() controla el tamaño del hueco. Por ejemplo:
##Un x más grande (por ejemplo, x = 3) hará que las barras estén aún más lejos del centro, aumentando el tamaño del hueco.
#Ajustar xlim(1, 2.5) define el rango visible del eje x para cortar el gráfico y crear el hueco.





###VARIABLES CUANTITATIVAS

##Creación intervalos
##clasifico en categorias o intervalos ordenados / vuelvo mis datos cualitativos
clases <- nclass.Sturges(emergenciascompleto$HERIDOS)



clases


intervalos <- cut(emergenciascompleto$HERIDOS, breaks = clases, include.lowest=TRUE, ordered_result = TRUE)

intervalos


emergenciasclass <- emergenciascompleto %>% 
  mutate(Int_Heridos=intervalos) %>% 
  group_by(Int_Heridos) %>% 
  summarise(Frec_Abs=n()) %>% 
  mutate(Frec_Rel=Frec_Abs/n, Frec_Abs_Acum=cumsum((Frec_Abs)), Frec_Rel_Acum=cumsum(Frec_Rel))


##histograma


histograma <- ggplot(emergenciascompleto, aes(x=HERIDOS, y= ..count..))+
  geom_histogram(bins=clases, fill=c("#ff5733"))+
  ggtitle("Histograma de Heridos por Emergencias")+
  theme_light()+
  xlab("Heridos")+
  ylab("Frecuencia Absoluta")

histograma

##Tabla contingencia

tabla_contingencia <- emergenciascompleto %>%
  count(EVENTO, DEPARTAMENTO) %>%  # Cuenta las frecuencias por EVENTO y REGION
  pivot_wider(names_from = DEPARTAMENTO, values_from = n, values_fill = 0)  # Convierte a formato tabla
print(tabla_contingencia)


##diagrama de dispersion
###¿quiero hacer una linea de identidad?
##una línea calculada estadísticamente que resume la relación promedio entre dos
##variables (en este caso, heridos y fallecidos) mediante un modelo de regresión lineal.


plot(emergenciascompleto$FALLECIDOS, emergenciascompleto$HERIDOS,
     main = "Relación entre Fallecidos y Heridos",
     xlab = "Cantidad de Fallecidos",
     ylab = "Cantidad de Heridos",
     pch = 19,  # Forma de los puntos
     col = "blue")  # Color de los puntos

ggplot(emergenciascompleto, aes(x = FALLECIDOS, y = HERIDOS, color = EVENTO)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre Accidentes y Heridos por Evento",
       x = "Cantidad de Fallecidos",
       y = "Cantidad de Heridos") +
  theme_minimal()


##coeficiente de relacion


cor(emergenciascompleto$HERIDOS, emergenciascompleto$FALLECIDOS)



cor(emergenciascompleto$VIVIENDAS.DESTRUIDAS, emergenciascompleto$VIVIENDAS.AVERIADAS)

