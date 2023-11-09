library(readxl)
library(Hmisc)
library(ggplot2)
library(arm)
library(sjstats)
library(moments)
library(plotly)
library(rcompanion)
library(magrittr) 
library(dplyr)
 
##EL PROFESOR ES UN TRIPEHIJUEPUTA
##VADiscretas

#Media y Varianza

x=c(0,1,2,3,4)
p_x=c(0.15,0.35,0.3,0.15,0.05)
media=sum(x*p_x)
varianza=sum(((x-media)^2)*p_x)
media
varianza
##acumulada

F_x=cumsum(c(0,p_x))
##Binomial y acumulada

dbinom(7, size = 10,prob = 0.5)
pbinom(7, size = 10,prob = 0.5)
##Poisson y acumulada

lbd= 35
dpois(50,lambda = lbd)
ppois(45,lambda = lbd)
##VACONTINUAS

####áreabajolacurva

library(ggplot2)
valor_limite <- 1
x1 <- seq(-3, 3, length = 1000)
pdf <- dnorm(x1)
data <- data.frame(x1,pdf)
ggplot(data, aes(x = x1 , y = pdf)) +
  geom_line(color = "blue") +  
  geom_area(data = subset(data, x <= valor_limite), fill = "blue", alpha = 0.3) +
  xlab("Variable Aleatoria Continua") +
  ylab("Densidad de Probabilidad") +
  ggtitle("Función de Densidad de Probabilidad con Área Sombreada")

##Densidad Uniforme

###menorque

punif(2,min=1,max=10)
###mayorque

1-punif(4.1,min=1.1,max=9.9)
punif(4.1,min=1.1,max=9.9,lower.tail = FALSE)

###mediana y cuantil

qunif(0.5,min=1.1,max=9.9)
qunif(0.25,min=1.1,max=9.9)

##Distribución Normal

###menorq

pnorm (35, mean = 40, sd = 12.5)

###intervalo

pnorm (55, mean = 40, sd = 12.5) - pnorm (45, mean = 40, sd = 12.5)

###mayorq

pnorm (60, mean = 40, sd = 12.5, lower.tail = FALSE)
1 - pnorm (60, mean = 40, sd = 12.5)
##cuantiles

qnorm(0.9,mean = 40, sd = 12.5) 
qnorm(0.05,mean = 40, sd = 12.5) 
qnorm(0.95,mean = 40, sd = 12.5)

#DESCRIPTIVA

##Histograma v continuas 

conti=c(1,3,4,5,1,2,3,1,3,2,2,4,5,1,3,5,4,5,5,5,2,1,1,1,1,2,3,4,1,2,2,4,5,5,5,5,2)
hist(conti,main = "Histograma conti")

###Histograma v discretas

ejemplo<-c(1,3,4,5,1,2,3,1,3,2,2,4,5,1,3,5)
disc<-as.data.frame(ejemplo)
ggplot(disc,aes(ejemplo))+geom_bar()
arm::discrete.histogram(disc$ejemplo)

##V Categoricas

xcategorica=c("juan","juan","juan","maria","roberto","roberto","joaquin","maria","juan","pedro","sergio","aaaaaaaa","maria")
Tablaxcat=table(xcategorica)
barplot(Tablaxcat)
Tablaxcat

###Proporciones

Tablaxcat_prop=Tablaxcat/sum(Tablaxcat)
Tablaxcat_prop

##Medidas de Tendencia
media_conti<-mean(conti)
media_conti

mediana_conti<-quantile(conti,probs = 0.5)
mediana_conti

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda_conti<-getmode(conti) 
moda_conti

##grafiquito con tendencias

hist(conti,col = "white")
abline(v=c(media_conti,mediana_conti,moda_conti),col=c("red","blue","green"))
legend("topleft", legend = c("Media", "Mediana", "Moda"), col = c("red", "blue", "green"), lty = 1, lwd = 2,cex = 0.5)

##Cuantiles

quantile(conti,probs = c(0.1))
quantile(conti,probs = c(0.45))

##Boxplot
boxplot(conti)
boxplot(conti,horizontal = TRUE)

##PARAEXCEL

BaseEjemplo <- read_excel("C:/Users/a/Downloads/BaseEjemplo.xlsx")

fig_gasto <- plot_ly(x = BaseEjemplo$gastosemanal, type = "box")
fig_gasto <- fig_gasto %>% add_boxplot(x = BaseEjemplo$gastosemanal, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                                       marker = list(color = 'rgb(7,40,89)'),
                                       line = list(color = 'rgb(7,40,89)'),
                                       name = "All Points")
fig_gasto

#dispersión o variabilidad, asimetría y sesgo

####Coeficiente de variación
sjstats::cv(conti)  
####Desviación estándar
sd(conti) 
####Varianza
sd(conti)^2 
####Rango
max(conti)-min(conti) 
####Asimetría
skewness(conti) 
####curtosis
kurtosis(conti)

##Comparación (ESTO ES UN COPYPAST EDEL PROFESOR XQ NO ENTEIDNO)

Base_EncHog2022<-read_excel("C:/Users/a/Downloads/EncHog2022.xlsx",col_types = c("text", "text", "numeric", "numeric", "text", "numeric", "numeric", "numeric"))
str(Base_EncHog2022)

##Calculo de la media para el grupo de los que si tienen contrato
Base_EncHog2022%>%filter(Contrato_lab=="1")%>%select(Gan_Neta)%>%summarise(media_Gan_Neta = mean(Gan_Neta, na.rm = TRUE))

filtrado_si_contrato_Gan_Neta<-Base_EncHog2022%>%filter(Contrato_lab=="1")%>%select(Gan_Neta)

##Histograma Creado directamente
Base_EncHog2022%>%filter(Contrato_lab=="1")%>%select(Gan_Neta)%>%ggplot(aes(x=Gan_Neta)) + 
  geom_histogram(color="black", fill="white")
histogram_plot <- plot_ly(data = filtrado_si_contrato_Gan_Neta, x = ~Gan_Neta, type = "histogram") %>%
  layout(title = "Histograma de Gan_Neta",
         xaxis = list(title = "Gan_Neta"),
         yaxis = list(title = "Frecuencia"))
# Mostrar el histograma
histogram_plot
# Crear el boxplot interactivo
boxplot_plot <- plot_ly(data = filtrado_si_contrato_Gan_Neta, y = ~Gan_Neta, type = "box") %>%
  layout(title = "Boxplot de Gan_Neta",
         yaxis = list(title = "Gan_Neta"))
# Mostrar el boxplot
boxplot_plot
##Boxplot Creado directamente
Base_EncHog2022%>%filter(Contrato_lab=="1")%>%select(Gan_Neta)%>%ggplot(aes(x=Gan_Neta)) + 
  geom_boxplot(outlier.colour="blue", outlier.shape=16,
               outlier.size=2)

####RELACIONES (tambien copaido del profesor jajajaaja)
####Relación Lineal 
long=100
a=1
b=2
x_simul<-rnorm(long,0,1)
y_simul<-a+b*x_simul+rnorm(long,0,1)
plot(x_simul,y_simul,pch=20,main="Relación Lineal")
cor(x_simul,y_simul)
###Relación Cuadrática
long=100
a=1
b=4
x_simul<-rnorm(long,0,1)
y_simul<-a-b*x_simul^2+rnorm(long,0,1)
plot(x_simul,y_simul,pch=20,main="Relación Cuadrática")
cor(x_simul,y_simul)
#####Relación Polinomial
c=5
d=2
long=100
a=1
b=2
x_simul<-rnorm(long,0,1)
y_simul<-a+b*x_simul+c*x_simul^2+d*x_simul^3+rnorm(long,0,1)
plot(x_simul,y_simul,pch=20,main="Relación Polinomial")
cor(x_simul,y_simul)
#####Relación exponencial
long=100
a=1
b=5
x_simul<-rnorm(long,0,1)
y_simul<-a+exp(-b*x_simul)+rnorm(long,0,1)
plot(x_simul,y_simul,pch=20,main="Relación Exponencial")
cor(x_simul,y_simul)
#####Relación sinusoidal
long=100
a=1
b=2
x_simul<-rnorm(long,0,1)
y_simul<-a+sin(2*pi*x_simul/b)+rnorm(long,0,1)
plot(x_simul,y_simul,pch=20,main="Relación Sinusoidal")
cor(x_simul,y_simul)
### No relación(Independencia)
long=100
a=1
x_simul<-rnorm(long,0,1)
y_simul<-a +rnorm(long,0,1)
plot(x_simul,y_simul,pch=20,main="No Relación")
cor(x_simul,y_simul)


##Tabla cruzada categorica
str(BaseEjemplo)
BaseEjemplo%>%select(Genero,marcacel)

table(BaseEjemplo%>%select(Genero,marcacel))

cramerV(BaseEjemplo$Genero, BaseEjemplo$marcacel, bias.correct = FALSE)

#####COASS
Cuando el intervalo captura el 1, entonces el conciente pdría valer 1,implicando que las 2 varianzas son iguales 

Note que el I.C del 95% para $\sigma_1^2/\sigma_2^2$ es $(0.01994435; 0.49759399)$, lo cual, dado que no contiene al 1, podría decidir con esa confianza, que las varianzas poblaciones no son iguales. Lo cual requerirá que usemos un intervalo de confianza para la diferencia de medias diferente al que presentamos anteriormente.
