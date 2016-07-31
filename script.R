#Cargando librerias
library(dplyr)
library(ggplot2)
library(stringdist)
theme_set(theme_bw(base_size = 9))

# Cargando los datos
datos <- read.csv("C:/Users/Alvaro/Dropbox/Director Quant Company/Clientes/Chec/Datos/DatosChecLimpios.csv", stringsAsFactors=FALSE)

# Pre-procesamiento =============
datos$ip <- factor(datos$ip)
datos$timestamp <- as.POSIXct(strptime(datos$timestamp, "%m/%d/%Y %H:%M"))
datos$dia_semana <- factor(weekdays(datos$timestamp),
                           ordered = TRUE,
                           levels = c("Monday", "Tuesday", "Wednesday", 'Thursday', "Friday"),
                           labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")
                           )
datos$hora <- as.POSIXlt(datos$timestamp)$hour - 5


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
datos <- datos[-c(which(datos$time_taken > 4000), which(is.na(datos$genero))), ]
dim(datos)
View(datos)



### 

#Limpiando las emisoras
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

names(table(datos$emisora_ahora))
lista_emisora_ahora <- list(
    "no_esta_er" = datos$emisora_ahora[datos$emisora_ahora == "0"],
    "no_sabe" = datos$emisora_ahora[datos$emisora_ahora == "1"],
    "emisora" = datos$emisora_ahora[!(datos$emisora_ahora %in% c("0","1"))]
)

lista_emisora_ahora$emisora <- tolower(lista_emisora_ahora$emisora)
lista_emisora_ahora$emisora <- trim(lista_emisora_ahora$emisora)

# Limpiando manualmente
jjj <- lista_emisora_ahora$emisora
jjj[jjj == "blu"] <- 'blue'
jjj[jjj == "caroñosa"] <- 'cariñosa'
jjj[jjj == "la cariñosa"] <- 'cariñosa'
jjj[jjj == "olímpica"] <- 'olimpica'
jjj[jjj == "oxígeno"] <- 'oxigeno'
jjj[jjj == "pereiraalaire"] <- 'pereira al aire'
jjj[jjj == "pereirastéreo"] <- 'pereira estéreo'
jjj[jjj == "policía"] <- 'policia'
jjj[jjj == "radiomaría"] <- 'radiomaria'
jjj[jjj == "x"] <- 'radiox'
jjj[jjj == "trpicana"] <- 'tropicana'
jjj[jjj == "amvida"] <- 'vidaam'

lista_emisora_ahora$emisora <- jjj

datos$emisora_ahora[which(!(datos$emisora_ahora %in% c("0","1")))] <- lista_emisora_ahora$emisora

table(datos$emisora_ahora, datos$hora)

## Usualmente que emisoras escucha
head(datos$emisora_usual)

sum(datos$emisora_usual == '0')
# 397 personas contestaron que no escuchaban radio

datos$emisora_usual[!(datos$emisora_usual %in% c("0","1"))]
ls_emisora_usual <- list()
for(i in 1:2000){
    ls_emisora_usual[[i]] <- strsplit(x = datos$emisora_usual[i], split = '[-]+')[[1]]
}

emisoras_usuales <- character()

for(i in 1:2000){
    emisoras_usuales <- c(emisoras_usuales, ls_emisora_usual[[i]])
}

emisoras_usuales <- tolower(emisoras_usuales)
emisoras_usuales <- trim(emisoras_usuales)

emisoras_populares <- c(
    "olimpica", "caracol", "radio1", "tropicana", "cariñosa",
    "radiotiempo", "todelar", "andes", "oxigeno",
    "policia", "mega", "manantial", "energia", 'blue',
    'radiomaria', 'caldasfm'
)

tabla_em_populares_bf <- sort(table(emisoras_usuales), decreasing = TRUE)


for(emisora in emisoras_populares){
    ind <- agrep(emisora, emisoras_usuales, max.distance = 0.25)
    emisoras_usuales[ind] <- emisora
}

# Esta es la tabla de las emisoras populares por recordación
tabla_em_populares <- sort(table(emisoras_usuales), decreasing = TRUE)


## Programas que escucha en la radio

## La lista de programas sirve para asociar cada respuesta con una lista
## de programas, lo mismo que la lista de "emisoras escucha usualmente"
programas <- character()
ls_programas <- list()

for(i in 1:2000){ 
    ls_programas[[i]] <- tolower(
        trim(
            strsplit(x = datos$programas[i], split = '[-]+')[[1]]
            )
        )
} 

for(i in 1:2000){
    progs_i <- ls_programas[[i]]
    programas <- c(programas, progs_i)
}

programas_mencionados <- unique(programas)


like_musica <- agrep("sica$", programas_mencionados, value = TRUE)
like_deporte <- agrep("deporte", programas_mencionados, value = TRUE)
like_noticias <- grep("notic[A-z]+$", programas_mencionados, value = TRUE)

programas[programas %in% like_musica] <- 'musica'
programas[programas %in% like_deporte] <- 'deportes'
programas[programas %in% like_noticias] <- 'noticias'

sort(table(programas), decreasing = TRUE)

# Estos vienen del archivo de Excel limpieza.xlsx
progs <- readClipboard()
progs_limpio <- readClipboard()
progs_limpio[progs_limpio == "NA"] <- NA

for(i in 1:length(progs)){
    programas[programas == progs[i]] <- progs_limpio[i]
}
sort(table(programas), decreasing = TRUE)
#Los programas estan limpios

## Programas que escucha en la radio en Manizales========

## La lista de programas sirve para asociar cada respuesta con una lista
## de programas, lo mismo que la lista de "emisoras escucha usualmente"
programas_mz <- character()
ls_programas_mz <- list()

for(i in 1:2000){ 
    ls_programas_mz[[i]] <- tolower(
        trim(
            strsplit(x = datos$programas_mz[i], split = '[-]+')[[1]]
        )
    )
} 

for(i in 1:2000){
    progs_i <- ls_programas_mz[[i]]
    programas_mz <- c(programas_mz, progs_i)
}

programas_mencionados_mz <- unique(programas_mz)


like_musica_mz <- agrep("sica", programas_mencionados_mz, value = TRUE)
like_deporte_mz <- agrep("deporte", programas_mencionados_mz, value = TRUE)
like_noticias_mz <- grep("notic[A-z]+$", programas_mencionados_mz, value = TRUE)

programas_mz[programas_mz %in% like_musica_mz] <- 'musica'
programas_mz[programas_mz %in% like_deporte_mz] <- 'deportes'
programas_mz[programas_mz %in% like_noticias_mz] <- 'noticias'

sort(table(programas_mz), decreasing = TRUE)

progs_mz <- readClipboard()
progs_limpio_mz <- readClipboard()
progs_limpio_mz[progs_limpio_mz == "NA"] <- NA

for(i in 1:length(progs_mz)){
    programas_mz[programas_mz == progs_mz[i]] <- progs_limpio_mz[i]
}
sort(table(programas_mz), decreasing = TRUE)
programas_mz[programas_mz == ""] <- NA

# Periodistas Radiales Manizales ===========

periodistas <- character()
ls_periodistas <- list()

for(i in 1:2000){ 
    ls_periodistas[[i]] <- tolower(
        trim(
            strsplit(x = datos$periodistas_mz[i], split = '[-]+')[[1]]
        )
    )
} 

for(i in 1:2000){
    per_i <- ls_periodistas[[i]]
    periodistas <- c(periodistas, per_i)
}

periodistas_mencionados <- unique(periodistas)

tabla_periodistas1 <- sort(table(periodistas), decreasing = TRUE)

periodistas[agrep("yesid lópez", periodistas, max.distance = 0.27)] <- "yesid lópez"
periodistas[agrep("javier giraldo neira", periodistas, max.distance = 0.27)] <- "javier giraldo neira"
periodistas[agrep("reinel llano", periodistas, max.distance = 0.27)] <- "reinel llano"
periodistas[agrep("hernando marín", periodistas, max.distance = 0.15)] <- "hernando marín"
periodistas[agrep("wilmar torres", periodistas)] <- "wilmar torres"
periodistas[agrep("rafael torregrosa", periodistas)] <- "rafael torregrosa"

periodistas_mencionados2 <- unique(periodistas)

periodistas2 <- periodistas
for(nombre in periodistas_mencionados2){
    if(nchar(nombre) > 14 && !is.na(nombre)){
        periodistas2[agrep(nombre, periodistas, value = TRUE)] <- nombre
    }
}

# Estos vienen del archivo de Excel limpieza.xlsx
period <- readClipboard()
period_limpio <- readClipboard()
period_limpio[period_limpio == "NA"] <- NA

for(i in 1:length(period)){
    periodistas[periodistas == period[i]] <- period_limpio[i]
}

periodistas[periodistas == ""] <- NA
sort(table(periodistas), decreasing = TRUE)


### Analisis de la informacion =====

info <- character()
ls_info <- list()

for(i in 1:2000){ 
    ls_info[[i]] <- tolower(
        trim(
            strsplit(x = datos$tipo_info[i], split = '[-]+')[[1]]
        )
    )
} 

for(i in 1:2000){
    info_i <- ls_info[[i]]
    info <- c(info, info_i)
} 

info_menciondada <- unique(info)
sort(table(info), decreasing = TRUE)[1:30]

# Tipos de información ====
# Ahorro
datos$ahorro <- FALSE
datos$ahorro[agrep('ahorro', datos$tipo_info)] <- TRUE
table(datos$ahorro)

# Factura
datos$factura <- FALSE
datos$factura[agrep('factura', datos$tipo_info)] <- TRUE
table(datos$factura)

# Costo
datos$costo <- FALSE
datos$costo[agrep('costo', datos$tipo_info)] <- TRUE
table(datos$costo)
agrep('costo', datos$tipo_info, value = TRUE)

# Tarifa
datos$tarifa <- FALSE
datos$tarifa[agrep('tarifa', datos$tipo_info)] <- TRUE
table(datos$tarifa)
agrep('tarifa', datos$tipo_info, value = TRUE)

# Factura/Costos/Tarifa
datos$factura_y_rel <- datos$factura | datos$costo | datos$tarifa
table(datos$factura_y_rel)

# Cortes
datos$cortes <- FALSE
datos$cortes[agrep('corte', datos$tipo_info)] <- TRUE
table(datos$cortes)

# Boletin de gestion
datos$boletin <- FALSE
datos$boletin[agrep('boletin', datos$tipo_info)] <- TRUE
table(datos$boletin)

# Proyectos
datos$proyectos <- FALSE
datos$proyectos[agrep('proyecto', datos$tipo_info)] <- TRUE
table(datos$proyectos)

# RSE
datos$rse <- FALSE
datos$rse[grep('\\brse\\b', datos$tipo_info)] <- TRUE
datos$rse[agrep('responsabilidad', datos$tipo_info)] <- TRUE
table(datos$rse)

# Medio ambiente
datos$ambiente <- FALSE
datos$ambiente[agrep('ambiente', datos$tipo_info)] <- TRUE
agrep('ambiente', datos$tipo_info, value = TRUE)
table(datos$ambiente)

# Información General
datos$general <- FALSE
datos$general[agrep('general', datos$tipo_info)] <- TRUE
table(datos$general)

#Servicios
datos$servicios <- FALSE
datos$servicios[agrep('servicio', datos$tipo_info)] <- TRUE
table(datos$servicios)


otros_info <- filter(datos, !ahorro, !proyectos, !rse, !factura_y_rel, 
           !cortes, !boletin, !ambiente, !general, !servicios, 
           !is.na(tipo_info)) %>%
    select(tipo_info)

write.csv(otros_info, file = 'otros_info.csv')

grep('\\bsi\\b', datos$tipo_info, value = TRUE)

# Utility functions ======
porcentaje <- function(vec){
    n <- sum(!is.na(vec))
    print(n)
    print(table(vec))
    print(100*table(vec)/n)
}