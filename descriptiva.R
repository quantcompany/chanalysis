# Estadística descriptiva ====

table(datos$ip)
table(datos$genero)

# [1] "id"               "ip"               "timestamp"       
# [4] "time_taken"       "genero"           "edad"            
# [7] "escuchando_radio" "emisora_ahora"    "emisora_usual"   
# [10] "programas"        "escucha_manana"   "escucha_mediodia"
# [13] "escucha_tarde"    "escucha_noche"    "escucha_finde"   
# [16] "programas_mz"     "periodistas_mz"   "info_chec"       
# [19] "tipo_info"        "dia_semana" 

#Genero =======
ggplot(datos, aes(x = genero)) + 
    geom_bar(fill = "#99BE00") +
    scale_fill_brewer(palette="Set1") +
    geom_text(stat = 'count',
              aes(y = ..count.., label = ..count..),
              vjust = -0.4
    ) +
    scale_y_continuous(limits = c(0,1400), breaks = seq(0,1400,200)) +
    labs(x = "Género", y = "Número de encuestados")

ggsave('img/genero.png', dpi = 400, width = 4, height = 3, units = "in")

ggplot(datos, aes(x = 1, fill = genero )) + 
    geom_bar(position = "fill") +
    scale_fill_brewer(palette="Set1") +
    geom_text(stat = 'count', position = 'fill', vjust = c(4,8), size = 4,
              aes(ymax = ..count..,
                  label = paste0(round(100*..count../sum(..count..),1),'%'))
    ) +
    labs(x = " ", y = "Proporción de encustados por género")

ggsave('img/genero_prop.png', dpi = 400, width = 3.3, height = 2.5, units = "in")

#Edad =====
ggplot(datos, aes(x = edad)) + 
    geom_bar(fill = "#99BE00") +
    geom_text(stat = 'count',
              aes(y = ..count.., label = ..count..),
              vjust = -0.4
    ) +
    scale_y_continuous(limits = c(0,1300), breaks = seq(0,1300,200)) +
    labs(x = "Grupo de Edad", y = "Número de encuestados")

ggsave('img/edad.png', dpi = 400, width = 4, height = 3, units = "in")

# Hora Escucha Radio =====
hora_escucha_radio <- data.frame(
    Hora = c("Mañanas", "Medio día", "Tarde", "Noche", "Fin de semana"),
    Porcentaje = round(100*c(1159, 434, 519, 328, 229)/1603,2)
)

hora_escucha_radio$Hora <- factor(hora_escucha_radio$Hora,
                                  ordered = TRUE,
                                  levels= c("Mañanas", "Medio día", "Tarde", "Noche", "Fin de semana"))

ggplot(hora_escucha_radio, aes(x = Hora, y = Porcentaje)) + 
    geom_bar(stat = 'identity', fill = "#99BE00") +
    geom_text(stat = 'identity',
              aes(y = Porcentaje, label = paste0(Porcentaje, "%")),
              vjust = -0.4
    ) +
    scale_y_continuous(limits = c(0,80), breaks = seq(0,80,10)) +
    labs(x = "Momento del Día", y = "Porcentaje")

ggsave('img/momento_escucha.png', dpi = 400, width = 4, height = 3, units = "in")

#Escuchando Radio =====
ggplot(datos, aes(x = escuchando_radio)) + 
    geom_bar(fill = "#99BE00") +
    geom_text(stat = 'count',
              aes(y = ..count.., label = ..count..),
              vjust = -0.4
    ) +
    scale_y_continuous(limits = c(0,1800), breaks = seq(0,1800,300)) +
    labs(x = "Respuesta", y = "Número de encuestados")

ggsave('img/escuchando_radio.png', dpi = 400, width = 4, height = 3, units = "in")

# Escuchando Radio por horarios
audiencia_hora <- group_by(datos, hora) %>%
                    summarise(Porcentaje = round(sum(100*(escuchando_radio == "Si"))/n(),2) )

horarios <- c("8-9 am","9-10 am","10-11 am",'11-12 am',
              '2-3 pm', '3-4 pm', '4-5 pm', '5-6 pm')
ggplot(audiencia_hora, aes(x = factor(hora), y = Porcentaje, group=1)) + 
    geom_line(size = 1.5, color = "#99BE00") +
    geom_point(size = 2) + 
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Porcentaje, label = paste0(Porcentaje, '%') ),
              vjust = -0.8
    ) +
    scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
    scale_x_discrete(labels = horarios) + 
    labs(x = "Hora", y = "Porcentaje de encuestados")

ggsave('img/audiencia_hora.png', dpi = 400, width = 4, height = 3, units = "in")


# Escuchando Radio por días
audiencia_dia <- group_by(datos, dia_semana) %>%
    summarise(Porcentaje = round(sum(100*(escuchando_radio == "Si"))/n(),2) )

ggplot(audiencia_dia, aes(x = dia_semana, y = Porcentaje, group=1)) + 
    geom_line(size = 1.5, color = "#99BE00") +
    geom_point(size = 2) + 
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Porcentaje, label = paste0(Porcentaje, '%') ),
              vjust = -0.8
    ) +
    scale_y_continuous(limits = c(10,25), breaks = seq(10,25,2)) +
    labs(x = "Hora", y = "Porcentaje de encuestados")

ggsave('img/audiencia_dia.png', dpi = 400, width = 4, height = 3, units = "in")

## Emisora escucha ahora ========

emisora_escucha_ahora <- as.data.frame(sort(table(datos$emisora_ahora), decreasing = TRUE), stringsAsFactors = FALSE)
names(emisora_escucha_ahora) <- c("Emisora", "Oyentes")
emisora_escucha_ahora <- emisora_escucha_ahora[-c(1,4),]
emisora_escucha_ahora <- rbind(emisora_escucha_ahora, data.frame("Emisora" = "No sé", "Oyentes"=42))
emisora_escucha_ahora$Emisora <- factor(emisora_escucha_ahora$Emisora, 
                                        ordered = TRUE,
                                        levels = emisora_escucha_ahora$Emisora[order(emisora_escucha_ahora$Oyentes)])

emisora_escucha_ahora$Emisora <- reorder(emisora_escucha_ahora$Emisora, emisora_escucha_ahora$Oyentes)

ggplot(emisora_escucha_ahora, aes(x = Emisora, y = Oyentes)) + 
    geom_bar(stat = 'identity', fill = "#99BE00", width = 0.4) +
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Oyentes, label = Oyentes ),
              hjust = -1) +
    scale_x_discrete(labels = toupper(levels(emisora_escucha_ahora$Emisora))) +
    coord_flip() + 
    labs(x = "", y = "Oyentes")
ggsave('img/emisora_escucha_ahora.png', dpi = 400, width = 7, height = 5, units = "in")


## Emisora usual ========
emisoras_usuales[emisoras_usuales == "0"] <- "No escucho radio"
emisoras_usuales <- emisoras_usuales[emisoras_usuales != "1"]
emisoras_usuales_df <- as.data.frame(sort(table(emisoras_usuales), decreasing = TRUE))
emisoras_usuales_df <- emisoras_usuales_df[-1,]  # Borrando "No escucho radio"
emisoras_usuales_df <- emisoras_usuales_df[-1,]  # Borrando radios solo una mención
names(emisoras_usuales_df) <- c("Emisora", "Menciones")
emisoras_usuales_df <- emisoras_usuales_df[emisoras_usuales_df$Menciones > 1,]


emisoras_usuales_df$Emisora <- reorder(emisoras_usuales_df$Emisora, emisoras_usuales_df$Menciones)

ggplot(emisoras_usuales_df, aes(x = Emisora, y = Menciones)) + 
    geom_bar(stat = 'identity', fill = "#99BE00", width = 0.4) +
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Menciones, label = Menciones),
              hjust = -0.5) +
    scale_x_discrete(labels = toupper(levels(emisoras_usuales_df$Emisora))) +
    coord_flip() + 
    labs(x = "", y = "Menciones")
ggsave('img/emisora_usual.png', dpi = 400, width = 7, height = 5, units = "in")

## Programas mencionados =====
programas_df <- as.data.frame(sort(table(programas), decreasing = TRUE), stringsAsFactors = FALSE)
names(programas_df) <- c("Programa", "Menciones")
programas_df <- programas_df[-1, ]
programas_df <- programas_df[programas_df$Menciones > 1, ]
programas_df$Programa <- reorder(programas_df$Programa, programas_df$Menciones)

ggplot(programas_df, aes(x = Programa, y = Menciones)) + 
    geom_bar(stat = 'identity', fill = "#99BE00", width = 0.4) +
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Menciones, label = Menciones),
              hjust = -0.5) +
    scale_x_discrete(labels = toupper(levels(programas_df$Programa))) +
    coord_flip() + 
    labs(x = "", y = "Menciones")
ggsave('img/programas.png', dpi = 400, width = 7, height = 5, units = "in")


## Programas manizales =====
programas_mz_df <- as.data.frame(sort(table(programas_mz), decreasing = TRUE), stringsAsFactors = FALSE)
names(programas_mz_df) <- c("Programa", "Menciones")
programas_mz_df <- programas_mz_df[-2, ] # Borrando "música"
programas_mz_df <- programas_mz_df[programas_mz_df$Menciones > 1, ]
programas_mz_df <- programas_mz_df[programas_mz_df$Programa != 'la luciernaga', ]
programas_mz_df$Programa <- reorder(programas_mz_df$Programa, programas_mz_df$Menciones)

programas_mz_labs <- substr(toupper(levels(programas_mz_df$Programa)),1,18)
ggplot(programas_mz_df, aes(x = Programa, y = Menciones)) + 
    geom_bar(stat = 'identity', fill = "#99BE00", width = 0.4) +
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Menciones, label = Menciones),
              hjust = -0.5) +
    scale_x_discrete(labels = programas_mz_labs) +
    coord_flip() + 
    labs(x = "", y = "Menciones")
ggsave('img/programas_manizales.png', dpi = 400, width = 7, height = 5, units = "in")

## Periodistas/personalidades radiales =====
periodistas_df <- as.data.frame(sort(table(periodistas), decreasing = TRUE), stringsAsFactors = FALSE)
names(periodistas_df) <- c("Periodista", "Menciones")
periodistas_df <- periodistas_df[periodistas_df$Menciones > 1, ]
periodistas_df$Periodista <- reorder(periodistas_df$Periodista, 
                                     periodistas_df$Menciones)

periodistas_labs <- substr(toupper(levels(periodistas_df$Periodista)),1,26)

ggplot(periodistas_df, aes(x = Periodista, y = Menciones)) + 
    geom_bar(stat = 'identity', fill = "#99BE00", width = 0.4) +
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Menciones, label = Menciones),
              hjust = -0.5) +
    scale_x_discrete(labels = periodistas_labs) +
    coord_flip() + 
    labs(x = "", y = "Menciones")
ggsave('img/periodistas.png', dpi = 400, width = 7, height = 7, units = "in")


## Ha escuchado informacion de la CHEC ====
info_chec_df <- data.frame(
    Respuesta = c("Sí", "No/No Recuderda/NR"),
    Cantidad = c(568, 1035),
    Porcentaje = c(35.4, 64.6)
)

ggplot(info_chec_df, aes(x = Respuesta, y = Cantidad)) + 
    geom_bar(stat = 'identity', fill = "#99BE00") +
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Cantidad, label = Cantidad),
              vjust = -0.5) +
    scale_y_continuous(limits = c(0,1200), breaks = seq(0,1200,200)) +
    labs(x = "Respuesta", y = "Cantidad")
ggsave('img/info_chec.png', dpi = 400, width = 4, height = 3, units = "in")

## Tipo info le interesa ======

no_le_interesa <- c("no", "no.", "ninguna", 'no le interesa',
                    'ninguna','ninguna en especial')
sum(datos$tipo_info %in% no_le_interesa, na.rm = TRUE)

## Tipo información CHEC
tipo_info_df <- data.frame(
    Tipo = c("Tarifas/Factura", "Ahorro",
             "Cortes", "Proyectos",
             "RSE", "Boletín",
             "Info. General", "Servicios",
             "Otros"),
    Cantidad = c(229+10, 195,
                 168+2, 40+2,
                 22+7, 30,
                 64+81, 79,
                 103)
)

tipo_info_df$Porcentaje <- round(100*tipo_info_df$Cantidad / 892, 2)
tipo_info_df$Tipo <- reorder(tipo_info_df$Tipo, -tipo_info_df$Cantidad, order = TRUE)

ggplot(tipo_info_df, aes(x = Tipo, y = Cantidad)) + 
    geom_bar(stat = 'identity', fill = "#99BE00") +
    geom_text(stat = 'identity',
              size = 2.5,
              aes(y = Cantidad, label = Cantidad),
              vjust = -0.5) +
    scale_y_continuous(limits = c(0,250), breaks = seq(0,250,25)) +
    labs(x = "Tipo de información", y = "Cantidad")
ggsave('img/tipo_info.png', dpi = 400, width = 5.5, height = 3, units = "in")

    

