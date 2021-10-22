###################################################
## Analisis exploratorio de datos                ##
## Por: Camilo Barrios Perez - Universidad EAFIT ##
###################################################

###########################################
### Configurar el directorio de trabajo ###
###########################################

setwd("C:/Users/camil/Dropbox/Projects 2021/Proyecto EAFIT/Clases/Semana 14/Base de datos_An�lisis exploratorio")

#####################################################
## Importar librerias y la base de datos principal #
#####################################################

library(ggplot2)
library(dbplyr)
library(tidyverse)
library(funModeling)

### Import database ###

data=read.table("Base de datos_An�lisis exploratorio_Cultivo de arroz.csv",head=T,sep=",")

#####################################################
## Evaluar valores faltante, ceros y tipo de datos ##
#####################################################

# Perfil de los datos de entrada
df_status(data)

# q_zeros: cuantifica la cantidad de ceros (p_zeros: in percent)
# q_inf: cuantifica valores infinitos (p_inf: in percent)
# q_na: cuantifiva valores NA (p_na: in percent)
# type: factor n�mero
# unique: quantity of unique values

#######################################
## Filtrado de variables no deseadas ##
#######################################

my_data = df_status(data, print_results = F)

# Remover variables con m�s del 30% de los registros iguales a cero
vars_to_remove=filter(my_data, p_zeros > 30)  %>% .$variable
vars_to_remove

# Mantener todas las variables excepto las columnas en el vector "vars_to_remove"
my_data_2=select(data, -one_of(vars_to_remove))

# Ordenar las variables de acuerdo al porcentaje de ceros
arrange(my_data, -p_zeros) %>% select(variable, q_zeros, p_zeros)

# Reemplazar valores NA

# Reemplazar NA por 0
my_data_2[is.na(my_data_2)]=0
my_data_2

###################################################
### Seleccionar un grupo espec�fico de variables ##
###################################################

data_2=subset(data, select = c(Station, Soil_2, PARCUM, Total_Pre_all_CS, Total_Rice_CWR_all_CS, Total_IWR_all_CS,  
                               Num_DD,Number_DS,Num_WD,Number_WS,  
                               Num_HD_Tmax, Num_HW_Tmax,Num_HD_Tmin, 
                               Num_HW_Tmin, Yield))


## Obtener otras estad�sticas: filas totales, columnas totales y nombres de columnas

# Total filas
nrow(data_2)

# Total columnas
ncol(data_2)

# Column names
colnames(data_2)

####################################
## Explorar variables categoricas ##
####################################

unique(data_2$Station)

##############
## Boxplots ##
##############

# Opci�n 1

ggplot(data_2, aes(x=Station, y=Yield)) + 
  geom_boxplot()

# Opci�n 2
ggplot(data_2, aes(x=reorder(Station,-Total_Pre_all_CS,na.rm = TRUE), y=Total_Pre_all_CS)) + 
  geom_boxplot()

# Opci�n 3
ggplot(data_2, aes(x=reorder(Station,-Total_Pre_all_CS,na.rm = TRUE), y=Total_Pre_all_CS)) + 
  geom_boxplot()+
  labs(title="Gr�fica de precipitaci�n acumulada \n por estaci�n meteorol�gica", x ="Estaci�n meteorol�gica", y = "Rendimiento (kg/ha)") +
  coord_flip()

#################
## Histogramas ##
#################

# Histograma b�sico
ggplot(data_2, aes(x=Yield)) + geom_histogram()

# Cambiar el ancho de las barras
ggplot(data_2, aes(x=Total_Pre_all_CS)) + 
  geom_histogram(binwidth=5)

# Cambiar el color
p = ggplot(data_2, aes(x=Total_Pre_all_CS)) + 
    geom_histogram(color="black", fill="white")

# Adicionar una linea en el promedio de los datos
p + geom_vline(aes(xintercept=mean(Total_Pre_all_CS)),
              color="blue", linetype="dashed", size=1)


# histograma con gr�fica de densidad
ggplot(data_2, aes(x=Total_Pre_all_CS)) + 
#  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

# Comparar histogramas entre estaciones
# Primero seleccionamos los datos de la base de datos principal

Ibague = subset(data_2, Station == "11_Tolima_Ibague", select=c(Station, Total_Pre_all_CS, PARCUM, Num_HD_Tmin, Yield))
Neiva = subset(data_2, Station == "9_Huila_Neiva", select=c(Station, Total_Pre_all_CS, PARCUM, Num_HD_Tmin, Yield))
estaciones = rbind(Neiva, Ibague)

# Interponemos los histogramas de las dos estaciones
ggplot(estaciones, aes(x=Total_Pre_all_CS, color=Station)) +
  geom_histogram(fill="white", alpha=0.5, position="dodge")+
  labs(title="Histogramas", x ="Precipitaci�n acumulada")+
  theme_bw() # fondo blanco y l�neas de cuadr�cula grises

#############################
## Diagramas de disperci�n ##
#############################

# Diagramas de disperci�n b�sico
ggplot(estaciones, aes(x=PARCUM, y=Yield)) + geom_point()

# Adicionamos la linea de regresi�n
ggplot(estaciones, aes(x=PARCUM, y=Yield)) + 
  geom_point()+
  geom_smooth(method=lm) # M�todo lineal


# M�todo Loess (Regresi�n ajustada a los datos)
ggplot(estaciones, aes(x=Num_HD_Tmin, y=Yield)) + 
  geom_point()+
  geom_smooth() +
  theme_bw() # fondo blanco y l�neas de cuadr�cula grises

###############################################################
## An�lisis gr�fico para evaluar la relaci�n entre variables ##
###############################################################

library("GGally") # Esta librer�a permite aplicar  varias funciones gr�ficas para el an�lisis de los datos. 
ggpairs(estaciones)


data_2=subset(data, select = c(PARCUM, Total_Pre_all_CS, Total_Rice_CWR_all_CS, Total_IWR_all_CS,  
                               Num_DD,Number_DS,Num_WD,Number_WS,  
                               Num_HD_Tmax, Num_HW_Tmax,Num_HD_Tmin, 
                               Num_HW_Tmin, Yield))

ggpairs(data_2)


##################################################################
## VIF Funci�n para detectar Multicolinearidad en las variables ##
##################################################################

library(usdm)

# Utiliza el �ndice variance inflation factor (VIF) al grupo de variables y excluye aquellas en las cuales existe alta correlaci�n.

VIF_var <- vifstep(data_2,th = 10)
VIF_var

# Reduce the predictor stack to the relevant layers
New_data_2 <- exclude(data_2, VIF_var) # Use only the selected variables

