## Librerías
library(ggplot2)
library(plotly)



## Función para crear Dataframe a partir de valores estadísticos por año para el conjunto de autos
get_new_dataframe <- function(x) {
  
  years <- c(2019, 2020, 2021, 2022)
  cols <- c("year", "media", "mediana", "varianza", "desviacion", "max", "min")
  first_record <- TRUE
  
  ## Crea listas con valores estadísticos para crear un nuevo dataframe
  for (year in years) {
    print(paste("Año-->", year))
    indices <- x$year == year
    marcas_anual <- x[indices,]
    suma<-sum(marcas_anual$count)
    n=length(marcas_anual$count)
    media <- suma/n
    mean <- mean(marcas_anual$count)
    
    y<-marcas_anual$count^2
    varianza=(sum(y)/n)-media^2
    
    desviacion<-sqrt(varianza)
    max<-max(marcas_anual$count)
    min<-min(marcas_anual$count)
    
    if (first_record) { 
      year_list <- c(year)
      media_list <- c(media)
      mean_list <- c(mean)
      varianza_list <- c(varianza)
      desviacion_list <- c(desviacion)
      max_list <- c(max)
      min_list <- c(min)
      first_record <- FALSE
    }else{
      year_list <- c(year_list, year)
      media_list <- c(media_list, media)
      mean_list <- c(mean_list, mean)
      varianza_list <- c(varianza_list, varianza)
      desviacion_list <- c(desviacion_list, desviacion)
      max_list <- c(max_list, max)
      min_list <- c(min_list, min)
    }
  }
  df<- data.frame(year_list, media_list, mean_list, varianza_list, desviacion_list, max_list, min_list)
  # Cambia nombre a la columnas
  colnames(df) <- cols
  df
}

## Lee archivo de auto eléctricos
autos <- read.csv("./autos-electricos.csv", sep=";") 
summary(autos)


## Lee archivo de auto según tipo, marca y año
marcas <- read.csv("./marcas.csv", sep=";")
summary(marcas)
print(marcas)


## Divide el Dataframe según el  tipo
marcas_tipo <- split(marcas, marcas$type)
print(marcas_tipo)






## Crea nuevo dataframe con valores estadísticos por año

######################
## Autos combustión
marcas_combustion <- marcas_tipo$combustion
head(marcas_combustion)
summary(marcas_combustion)
dfAutoComb <- get_new_dataframe(marcas_combustion)

######################
## Autos electrícos
marcas_electrico <- marcas_tipo$electrico
head(marcas_electrico)
summary(marcas_electrico)
dfAutoElec <- get_new_dataframe(marcas_electrico)



## Muestra cantidad de autos eléctricos por año y según tecnología
ggplot(data=autos, aes(x=year, y = count, color = technology)) + 
  geom_line() +
  labs(x = "Año", y = "Cantidad")



## Muestra cantidad de autos por año según el tipo
ggplot(data=marcas, aes(x=year, y = count, color = type)) + 
  geom_line() +
  labs(x = "Año", y = "Cantidad")



## Muestra autos de combustión por año y agrupados por marca
ggplot(data=marcas_tipo$combustion, aes(x=year, y = count, color = brand)) + 
  geom_line() +
  labs(x = "Año", y = "Cantidad")


## Muestra autos eléctricos por año y agrupados por marca
ggplot(data=marcas_tipo$electrico, aes(x=year, y = count, color = brand)) + 
  geom_line() +
  labs(x = "Año", y = "Cantidad")

