#Cargando librerias
library(dplyr)
library(ggplot2)

# Cargando los datos
datos <- read.csv("C:/Users/Alvaro/Dropbox/Director Quant Company/Clientes/Chec/Datos/DatosChecLimpios.csv", stringsAsFactors=FALSE)

# Procesando los datos
datos$ip <- factor(datos$ip)
datos$timestamp <- as.POSIXct(strptime(datos$timestamp, "%m/%d/%Y %H:%M"))
datos$dia_semana <- factor(weekdays(datos$timestamp),
                           ordered = TRUE,
                           levels = c("Monday", "Tuesday", "Wednesday", 'Thursday', "Friday")
                           )

datos$genero <- factor(datos$genero, labels = c("Masculino","Femenino"))

datos$edad <- factor(datos$edad, ordered = TRUE,
                     labels = c("menor de 20", '20-29', '30-39', '40-49', 'mayor 50')
                     )

datos$escuchando_radio <- factor(datos$escuchando_radio, labels = c("Si","No"))
datos$escucha_manana <- factor(datos$escucha_manana, labels = c("No", "Si"))
datos$escucha_mediodia <- factor(datos$escucha_mediodia, labels = c("No", "Si"))
datos$escucha_tarde <- factor(datos$escucha_tarde, labels = c("No", "Si"))
datos$escucha_noche <- factor(datos$escucha_noche, labels = c("No", "Si"))
datos$escucha_finde <- factor(datos$escucha_finde, labels = c("No", "Si"))

datos$ext_ref <- NULL

# Limpiando encuestas que sirven.
# Encuestas de prueba
datos <- filter(datos, timestamp > as.POSIXct("2016-06-26 13:30:00"))
# Valor nulo en escuchando radio
datos <- datos[!is.na(datos$escuchando_radio),]
# Inconsistentes
datos <- filter(datos, !(emisora_usual == 1 & escuchando_radio == "Si"))
datos <- datos[-c(which(datos$time_taken > 3000), which(is.na(datos$genero))), ]

View(datos)
