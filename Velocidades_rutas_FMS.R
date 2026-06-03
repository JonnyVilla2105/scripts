

#**********************************************************************************************
#  @Nombre: Analisis de velocidades rutas zonales por semana
#  @Autor: Fernando Roa
#  @Fecha: 20200501
#  @Cambios:
#  @Ayudas:
#**********************************************************************************************

# Cargue de librerías --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)
library(Hmisc)
library(openxlsx)
library(readxl)
options(scipen = 999)

# Importar calendario

festivos <- ymd(c("2026-01-01", "2026-01-12",
                  "2026-03-23", "2026-04-02", "2026-04-03",
                  "2026-05-01", "2026-05-18", "2026-06-08",
                  "2026-06-15", "2026-06-29", "2026-07-20",
                  "2026-08-07", "2026-08-17", "2026-10-12",
                  "2026-11-02", "2026-11-16", "2026-12-08",
                  "2026-12-25"))

# Calendario
calendario <- read.xlsx(
  "C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/calendario_2026.xlsx",
  sheet = "DimDate"
) %>% 
  janitor::clean_names() %>% 
  mutate(as.Date(as.character(date), format = "%Y%m%d"),
         day_of_week = as.integer(day_of_week),
         is_holiday = if_else(date %in% festivos, 1L, is_holiday),
         tipo_dia = if_else(day_of_week >= 1L &
                              day_of_week <= 5L &
                              is_holiday == 0L, "Habil", "Sabado"),
         tipo_dia = if_else(is_holiday == 1L, "Festivo", tipo_dia),
         day_name = case_when(day_of_week == 1 ~ "Lunes",
                              day_of_week == 2 ~ "Martes",
                              day_of_week == 3 ~ "Miercoles",
                              day_of_week == 4 ~ "Jueves",
                              day_of_week == 5 ~ "Viernes",
                              day_of_week == 6 ~ "Sabado",
                              TRUE ~ "Domingo"),
         week_of_year = as.integer(week_of_year),
         semana = case_when(month(date) == 1 & week_of_year >= 50 ~
                              str_c(year(date) - 1, week_of_year),
                            month(date) == 12 & week_of_year <= 1 ~
                              str_c(year(date) + 1, week_of_year),
                            TRUE ~ str_c(year(date), week_of_year)),
         semana = if_else(nchar(semana) == 5,
                          as.numeric(str_c(str_sub(semana, 1L,4L), "0", str_sub(semana, 5L, 5L))),
                          as.numeric(semana))) %>% 
  group_by(week_of_year, tipo_dia) %>% 
  mutate(consec_tipo_dia_en_week = 1:n()) %>% 
  ungroup() %>% 
  select(date_key, "tipodia" = tipo_dia, semana, is_holiday, day_of_week) %>% 
  mutate(date = date_key,
         date_key = ymd(date_key),
         mes = as.numeric(str_sub(date, 1L, 6L)))

  semana_actual <- calendario %>% 
  filter(date_key == today()) %>% 
  select(semana) %>% 
  unname() %>% 
  unlist()


semana_actual <- 209617

listado_semanas <- sort(unique(calendario$semana))

semana_estudio <- calendario %>% 
  filter(semana == semana_actual)

# Establecer datos de entrada

tipo_dia <- "habil"  # Las opciones son "habil" - "sabado" - "festivo" o "todos" en minúscula
percentil <- 0.70
# dia_referente <- 20201113
# desde <- 20201109
# hasta <- 20201115
dia_referente <- semana_estudio %>% 
  filter(tipodia == str_to_sentence(tipo_dia)) %>% 
  filter(date == first(date)) %>%
  select(date) %>%
  unname() %>% 
  unlist()

desde <- semana_estudio %>% 
  filter(date == first(date)) %>%
  select(date) %>%
  unname() %>% 
  unlist()

hasta <- semana_estudio %>% 
  filter(date == last(date)) %>%
  select(date) %>%
  unname() %>% 
  unlist()

vel_limite <- 50 # Velocidad en Km/h
intervalo <- 24.0 # Intervalo de analisis en horas
hora_inicio <- 4
hora_fin <- 23

week <- semana_estudio %>% 
  filter(date == first(date)) %>%
  select(semana) %>%
  unname() %>% 
  unlist()

# Hora de inicio del proceso
hora_inicio_proceso <- now()
# Cargue de Base de Datos de Fechas - dimdate -------------------------------------------------

# bd_fechas <- fread("c:/Users/PLANEACION/Desktop/bd/dim_date/dim_date.csv", encoding = "UTF-8") %>% 
#   janitor::clean_names()

# Listado de archivos de viajes desglosados y actividad bus -----------------------------------

vd_list <- list.files("Z:/01 base_datos/01 viajes_desglosados_FMS/", full.names = T)
ac_list <- list.files("Z:/01 base_datos/03 actividad_bus_FMS/", full.names = T)

vd_list_data <- data.frame("vd" = vd_list, stringsAsFactors = F) %>% 
  mutate(date_key = as.integer(str_sub(vd, -28L, -21L))) %>% 
  select(date_key, everything())

ac_list_data <- data.frame("ac_bus" = ac_list, stringsAsFactors = F) %>% 
  mutate(date_key = as.integer(str_sub(ac_bus, -26L, -19L))) %>% 
  select(date_key, everything())

archivos_disponibles <- vd_list_data %>% 
  left_join(ac_list_data, by = "date_key") %>% 
  na.omit() %>% 
  left_join(calendario %>% select(-date_key), by = c("date_key" = "date")) %>% 
  mutate(tipodia = case_when(tipo_dia == "todos" ~ "todos",
                             is_holiday == 1 ~ "festivo",
                             day_of_week == 6 ~ "sabado",
                             TRUE ~ "habil")) %>% 
  filter(tipodia == tipo_dia) %>% 
  filter(date_key >= desde & date_key <= hasta)

compilado_velocidades <- readxl::read_excel("Z:/01 base_datos/55 informe velocidades/compilado/Velocidad_rutas_zonales.xlsx", 
                                            sheet = "vel_compilado") %>% 
  mutate(Duracion = as.character(str_sub(Duracion, -8L, -1L)))

# Lectura y procesamiento de viajes desglosados, actividad bus / Consolidado de ac_bus --------

ac_bus_compilado_aux <- NULL

for(i in 1:length(archivos_disponibles$date_key)) { 
  
  #i<-1
  
  # Viajes desglosados
  vd <- fread(archivos_disponibles$vd[i],
              stringsAsFactors = F) %>% 
    janitor::clean_names() %>%
    select(fecha, servicio_bus = "servicio", id_viaje, linea = "id_l_nea", ruta) %>% 
    mutate(key_viaje = str_c(fecha, servicio_bus, id_viaje, sep = "-")) %>% 
    select(key_viaje, linea, ruta)
  
  
  # Actividad bus
  
  ac_bus <- fread(
    archivos_disponibles$ac_bus[i],
    stringsAsFactors = FALSE,
    na.strings = ""
  ) %>%
    janitor::clean_names() %>%
    select(
      -matches(
        "concesionario|codigo_bus|fms_bus|tabla|viaje_linea|orden_viaje|
       tipo_de_nodo|alarma_exceso_de_tiempo|conductor"
      )
    ) %>% 
    rename(hora_teorica = hora_te_rica) %>%
    mutate(hora_teorica = parse_date_time(hora_teorica, orders = "%H:%M:%S"),
           hora_llegada = parse_date_time(hora_llegada, orders = "%H:%M:%S"),
           hora_salida = parse_date_time(hora_salida, orders = "%H:%M:%S"),
           key_viaje = str_c(fecha, servicio_bus, id_viaje, sep = "-")) %>% 
    arrange(linea = id_l_nea, servicio_bus, id_viaje, hora_teorica) %>% 
    group_by(key_viaje) %>% 
    mutate(consecutivo_parada = 1:n(),
           total_paradas = max(consecutivo_parada)) %>% 
    ungroup() %>% 
    mutate(
      nodo_real = case_when(
        str_detect(evento, "Fin Viaje") == T ~ str_c(id_nodo, "Fin Viaje", sep = "-"),
        TRUE ~ as.character(id_nodo)),
      h_real = case_when(
        str_detect(evento, "Fin Viaje") == T ~
          if_else(day(hora_llegada) >= 2, 24*60 + hour(hora_llegada) * 60 +
                    minute(hora_llegada) + second(hora_llegada) / 60,
                  hour(hora_llegada) * 60 +
                    minute(hora_llegada) + second(hora_llegada) / 60),
        TRUE ~
          if_else(day(hora_salida) >= 2, 24*60 + hour(hora_salida) * 60 +
                    minute(hora_salida) + second(hora_salida) / 60,
                  hour(hora_salida) * 60 +
                    minute(hora_salida) + second(hora_salida) / 60)),
      h_real = case_when(consecutivo_parada == 1 ~
                           if_else(lead(key_viaje) == key_viaje & lead(h_real) > h_real,
                                   h_real, NA_real_),
                         str_detect(nodo_real, "Fin Viaje") ~
                           if_else(lag(key_viaje) == key_viaje & lag(h_real) < h_real,
                                   h_real, NA_real_),
                         TRUE ~ if_else(lag(key_viaje) == key_viaje & lead(key_viaje) == key_viaje &
                                          lag(h_real) < h_real & lead(h_real) > h_real,
                                        h_real, NA_real_)),
      t_viaje = case_when(
        (lag(consecutivo_parada) > consecutivo_parada) ~ as.numeric(0),
        (lag(h_real) > h_real) == T ~ NA_real_,
        (lag(h_real) > h_real) == NA ~ NA_real_,
        h_real - lag(h_real) > 120 ~ NA_real_,
        TRUE ~ 
          h_real - lag(h_real)),
      hora_paso = ((h_real / 60) %/% intervalo) * intervalo
    ) %>% 
    left_join(vd, by = "key_viaje") %>%
    filter(total_paradas > 2 | is.na(evento)) %>% 
    mutate(ruta_nodo = str_c(id_ruta, nodo_real, sep = "-")) %>%
    filter(h_real >= hora_inicio * 60,
           h_real <= hora_fin * 60) %>% 
    select(ruta_nodo, hora_paso, t_viaje)
  
  ac_bus_compilado_aux <- rbind(ac_bus_compilado_aux, ac_bus)  
  
}

# Calculo estadistico para tiempos entre paraderos de cada RutasSae ---------------------------

tiempos_prom_desv <- ac_bus_compilado_aux %>%
  group_by(ruta_nodo, hora_paso) %>% 
  summarise(t_viaje_prom = mean(t_viaje, na.rm = T),
            t_viaje_desv = sd(t_viaje, na.rm = T)) %>% 
  ungroup()

ac_bus_compilado <- ac_bus_compilado_aux %>%
  left_join(tiempos_prom_desv, by = c("ruta_nodo", "hora_paso")) %>%
  # Eliminacion de datos atipicos
  mutate(t_viaje = if_else(t_viaje >= (t_viaje_prom - 3 * t_viaje_desv) & 
                             t_viaje <= (t_viaje_prom + 3 * t_viaje_desv),
                           t_viaje, NA_real_)) %>% 
  group_by(ruta_nodo, hora_paso) %>% 
  summarise(duracion = quantile(t_viaje, probs = percentil, na.rm = T),
            n_datos = sum(!is.na(t_viaje))) %>%  # Número de datos de la franja
  ungroup() %>%
  arrange(ruta_nodo, hora_paso) %>% 
  mutate(duracion = case_when(n_datos >= 3 ~ duracion,
                              TRUE ~ NA_real_))

horas <- data.frame("hora" = unique(ac_bus_compilado$hora_paso)) %>%
  na.omit() %>% 
  arrange(hora) %>% 
  mutate(hora = as.character(hora))

# Cargue de v_desg del día referente para analizar para determinar las RutasSae vigentes ------
vd_actual <- fread(
  str_c(
    "Z:/01 base_datos/01 viajes_desglosados_FMS/",
    dia_referente,
    "_viajedesglosado.csv"
  )
) %>%
  janitor::clean_names() %>%
  rename(linea = id_l_nea) |>
  transmute(
    key_viaje = str_c(fecha, servicio_bus = servicio, id_viaje, sep = "-"),
    linea,
    ruta
  )

# RutasSae vigentes----------------------------------------------------------------------------
rutas_sae <- data.frame("ruta" = unique(vd_actual$ruta)) %>% 
  na.omit()


# Matriz de distancias----------------------------------------------------------------------
md <- fread(
  str_c(
    "Z:/01 base_datos/06 matriz_distancia_FMS/",
    dia_referente,
    "_matriz distancias.csv"
  )
) %>%
  janitor::clean_names() %>%
  #rename(id_linea = id_l_nea, posicion = posici_n, linea = l_nea)|>
  arrange(id_linea, id_ruta, posicion)

# Tiempos entre paraderos y cantidad de datos ------------------------------------------------
tiempos_y_n_datos <- md %>% 
  select(linea, id_ruta, id_linea, id_nodo, nombre_nodo, posicion) %>% 
  mutate(nodo_real = case_when(id_ruta != lead(id_ruta) ~
                                 str_c(id_nodo, "Fin Viaje", sep = "-"),
                               TRUE ~ 
                                 as.character(id_nodo)),
         ruta_nodo = str_c(id_ruta, nodo_real, sep = "-")) %>% 
  group_by(id_linea, id_ruta, id_nodo, nombre_nodo, nodo_real, ruta_nodo) %>% 
  summarise(posicion = max(posicion)) %>% 
  left_join(ac_bus_compilado %>% select(ruta_nodo, hora_paso, duracion, n_datos), 
            by = "ruta_nodo") %>% 
  arrange(id_linea, id_ruta, posicion, hora_paso) %>% 
  mutate(duracion = case_when(posicion == 0 ~ as.numeric(0), 
                              TRUE ~ duracion),
         n_datos = case_when(posicion == 0 ~ as.integer(0), 
                             TRUE ~ n_datos)) %>% 
  ungroup()

tiempos <- tiempos_y_n_datos %>%
  select(-n_datos) %>% 
  pivot_wider(id_cols = id_linea:posicion, names_from = hora_paso, values_from = duracion) %>%
  mutate(dist_relativa = if_else(id_ruta == lag(id_ruta), 
                                 posicion - lag(posicion),
                                 as.integer(0))) %>%
  filter(id_ruta %in% rutas_sae$ruta) %>% 
  select(id_linea:posicion, dist_relativa, horas$hora)

n_datos <- tiempos_y_n_datos %>%
  select(-duracion) %>% 
  pivot_wider(id_cols = id_linea:posicion, names_from = hora_paso, values_from = n_datos) %>%
  mutate(dist_relativa = if_else(id_ruta == lag(id_ruta), 
                                 posicion - lag(posicion),
                                 as.integer(0))) %>%
  filter(id_ruta %in% rutas_sae$ruta) %>% 
  select(id_linea:posicion, dist_relativa, horas$hora)

# Llenado de datos faltantes en la matriz de tiempos ------------------------------------------
# Creacion de matrices auxiliares de tiempos para llenado de datos faltantes 

tiempos2 <- tiempos %>% 
  mutate(aux1 = 0.1, aux2 =0.2) # columnas auxiliares creadas provisionalmente para 
# funcionamiento del loop de llenado de datos faltantes

## Llenado de datos faltantes matriz tiempos (primer loop)
for(i in 1:as.integer(length(tiempos2$id_ruta))){
  for(j in str_which(names(tiempos2), str_c("^", first(horas$hora), "$")):
      str_which(names(tiempos2), str_c("^", last(horas$hora), "$"))){
    tiempos2[[i,j]]  <-  case_when(!is.na(tiempos2[[i,j]]) ~ as.numeric(tiempos2[[i,j]]),
                                   TRUE ~ case_when(
                                     (j  >= (str_which(names(tiempos2), str_c("^", first(horas$hora), "$")) + 2)) &
                                       (j  <= (str_which(names(tiempos2), str_c("^", last(horas$hora), "$")) - 2)) &
                                       (tiempos2[[i, j-1L]] < 10) & 
                                       (tiempos2[[i, j+1L]] < 10) & 
                                       (!is.na(tiempos2[[i, j-1L]]) | !is.na(tiempos2[[i, j+1L]])) ~
                                       quantile(c(tiempos2[[i,j-1L]],
                                                  tiempos2[[i,j+1L]]), probs = 0.6, na.rm = T),
                                     (j  < (str_which(names(tiempos2), str_c("^", first(horas$hora), "$")) + 2)) &
                                       (tiempos2[[i,j+1L]] < 10) & 
                                       (tiempos2[[i,j+2L]] < 10) & 
                                       (!is.na(tiempos2[[i,j+1L]]) | !is.na(tiempos2[[i,j+2L]])) ~ 
                                       as.numeric(mean(c(tiempos2[[i,j+1L]], 
                                                         tiempos2[[i,j+2L]]), na.rm = T)),
                                     (j  > (str_which(names(tiempos2), str_c("^", last(horas$hora), "$")) - 2)) &
                                       (tiempos2[[i,j-1L]] < 10) & 
                                       (tiempos2[[i,j-2L]] < 10) & 
                                       (!is.na(tiempos2[[i,j-1L]]) | !is.na(tiempos2[[i,j-2L]])) ~ 
                                       as.numeric(mean(c(tiempos2[[i,j-1L]],
                                                         tiempos2[[i,j-2L]]), na.rm = T)),
                                     TRUE ~ tiempos2[[i,j]]))
    
    tiempos2[[i,j]]  <-  case_when(!is.na(tiempos2[[i,j]]) ~ 
                                     case_when(is.na((tiempos2$dist_relativa[i]/1000) / 
                                                       (tiempos2[[i,j]]/60)) ~ 
                                                 as.numeric(0),
                                               (tiempos2$dist_relativa[i]/1000) / 
                                                 (tiempos2[[i,j]]/60) > vel_limite ~
                                                 tiempos2$dist_relativa[i]/(vel_limite*1000/60),
                                               TRUE ~ tiempos2[[i,j]]),
                                   TRUE ~ tiempos2[[i,j]])
  }
}

tiempos2 <- tiempos2 %>% # Eliminacion de columnas auxiliares aux1 y aux2 
  select(-c(aux1, aux2)) # luego de ejecutar el loop de llenado

# Creacion de matriz de velocidades ---------------------------------------------------------
velocidad <- tiempos2
for(i in 1L:as.integer(length(velocidad$id_ruta))){
  for(j in str_which(names(velocidad), str_c("^", first(horas$hora), "$")):
      str_which(names(velocidad), str_c("^", last(horas$hora), "$"))){
    velocidad[[i,j]]  <- (velocidad$dist_relativa[i] * 60) / (velocidad[[i,j]] * 1000)
  }
}

# Creacion de matriz de velocidades promedio-------------------------------------------------

velocidad_aux <- velocidad %>%
  pivot_longer(cols = -c(id_linea:dist_relativa), 
               names_to = "hora", values_to = "velocidad") %>% 
  group_by(id_linea, id_ruta, hora) %>% 
  summarise(vel_prom = mean(velocidad, na.rm = T),
            longitud = max(posicion, na.rm = T)) %>% # SE PUEDE ELIMINAR ESTE CALCULO????
  ungroup()

velocidad_prom <- velocidad %>%
  pivot_longer(cols = -c(id_linea:dist_relativa), 
               names_to = "hora", values_to = "velocidad") %>% 
  left_join(velocidad_aux %>% select(-longitud), 
            by = c("id_linea", "id_ruta", "hora")) %>% 
  select(-velocidad) %>% 
  pivot_wider(id_cols = c(id_linea, id_ruta, id_nodo, nombre_nodo, nodo_real, ruta_nodo, 
                          posicion, dist_relativa), names_from = hora, values_from = vel_prom)


# Llenado de datos faltantes matriz de tiempo a partir de vel prom de la ruta (2do loop)-----

tiempos3 <- tiempos2

for(i in 1L:as.integer(length(tiempos3$id_ruta))){
  for(j in str_which(names(tiempos3), str_c("^", first(horas$hora), "$")):
      str_which(names(tiempos3), str_c("^", last(horas$hora), "$"))){
    tiempos3[[i,j]]  <- case_when(is.na(tiempos3[[i,j]]) ~ 
                                    tiempos3$dist_relativa[i] / 
                                    (velocidad_prom[[i,j]] *1000/60),
                                  TRUE ~ tiempos3[[i,j]])
    tiempos3[[i,j]]  <- case_when(is.na(tiempos3[[i,j]]) ~ 0,
                                  TRUE ~ tiempos3[[i,j]]) 
  }
}

# Cálculo de matriz de tiempos acumulados por ruta y franja horaria. --------------------------
# La matriz se fila a fila y luego en columnas.

tiempos3 <- tiempos3 %>% mutate(aux1 = 0.1, aux2 = 0.1, aux3 = 0.1, aux4 = 0.1, aux5 = 0.1, 
                                aux6 = 0.1, aux7 = 0.1, aux8 = 0.1, aux9 = 0.1, aux10 = 0.1)
tiempos_cum <- tiempos3

for(j in str_which(names(tiempos_cum), str_c("^", first(horas$hora), "$")):
    str_which(names(tiempos_cum), str_c("^", last(horas$hora), "$"))){
  for(i in 2L:as.integer(length(tiempos_cum$id_ruta))){
    tiempos_cum[[i,j]]  <- case_when(tiempos_cum$id_ruta[[i-1]] != tiempos_cum$id_ruta[[i]] ~ tiempos3[[i,j]],
                                     ((tiempos_cum[[i-1L,j]] + tiempos3[[i,j]]) %/% (intervalo * 60)) < 1L ~ 
                                       tiempos_cum[[i-1L,j]] + tiempos3[[i,j]],
                                     (j + ((tiempos_cum[[i-1L,j]] + tiempos3[[i,j + (tiempos_cum[[i-1L,j]] %/% (intervalo * 60))]]) %/% (intervalo * 60)))
                                     <= str_which(names(tiempos_cum), str_c("^", last(horas$hora), "$")) ~ 
                                       tiempos_cum[[i-1L,j]] + 
                                       tiempos3[[i,j + ((tiempos_cum[[i-1L,j]] + tiempos3[[i,j + (tiempos_cum[[i-1L,j]] %/% (intervalo * 60))]]) %/% (intervalo * 60))]],
                                     TRUE ~ tiempos_cum[[i-1,j]] + tiempos3[[i,str_which(names(tiempos_cum), str_c("^", last(horas$hora), "$"))]])
  }
}

tiempos_cum <- tiempos_cum %>% 
  select(-c(aux1:aux10))

tiempos3 <- tiempos3 %>% 
  select(-c(aux1:aux10))
# Resumen de tiempos de ruta por franja horaria -----------------------------------------------

tiempos_resumen <- tiempos_cum %>% 
  pivot_longer(cols = -c(id_linea, id_ruta, id_nodo, nombre_nodo, nodo_real, ruta_nodo, 
                         posicion, dist_relativa), 
               names_to = "hora", values_to = "time_cum") %>% 
  group_by(id_linea, id_ruta, hora) %>% 
  summarise(time_cum = hms::as_hms(round(max(time_cum, na.rm = T)*60, digits = 0)),
            longitud = max(posicion, na.rm = T)) %>%
  ungroup() %>% 
  mutate(time_cum = if_else(str_detect(id_linea, "16-10"), hms::as_hms("00:00:00"),
                            time_cum)) %>% 
  select(id_linea, id_ruta, longitud, duracion_vuelta = time_cum) %>% 
  arrange(id_linea)

velocidad_resumen <- tiempos_cum %>% 
  pivot_longer(cols = -c(id_linea, id_ruta, id_nodo, nombre_nodo, nodo_real, ruta_nodo, 
                         posicion, dist_relativa), 
               names_to = "hora", values_to = "time_cum") %>% 
  group_by(id_linea, id_ruta, hora) %>% 
  summarise(time_cum = max(time_cum, na.rm = T)/60,
            longitud = max(posicion, na.rm = T)/1000) %>%
  ungroup() %>%
  mutate(vel_prom = if_else(time_cum == 0 | str_detect(id_linea, "16-10"),
                            0, longitud / time_cum),
         vel_prom = round(vel_prom, digits = 2),
         longitud = round(longitud, digits = 2)) %>% 
  left_join(tiempos_resumen %>% 
              select(id_ruta, duracion_prom = duracion_vuelta), by = "id_ruta") %>% 
  select(id_linea, id_ruta, longitud, vel_prom, duracion_prom) %>%
  mutate(fecha_inicio = desde,
         fecha_fin = hasta,
         semana = week,
         llave_ruta = str_c(id_linea, " - ", id_ruta),
         duracion_prom = as.character(duracion_prom),
         duracion_ = as.numeric(str_sub(duracion_prom, -8L, -7L)) +
           as.numeric(str_sub(duracion_prom, -5L, -4L)) / 60 +
           as.numeric(str_sub(duracion_prom, -2L, -1L)) / 3600) %>% 
  arrange(id_linea) %>%
  mutate(Rutasae = id_ruta)|>
  select(
    Str_linea = id_linea,
    Str_ruta = id_ruta,
    Rutasae,
    `Llave Ruta` = llave_ruta,
    Longitud = longitud,
    Velocidad = vel_prom,
    Duracion = duracion_prom,
    Duracion_ = duracion_,
    Fecha_inicio = fecha_inicio,
    Fecha_fin = fecha_fin,
    Semana = semana
  )


vel_historico <- rbind(compilado_velocidades, velocidad_resumen)

# Se escribe el archivo de velocidades semanal

write.csv(velocidad_resumen, str_c("C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/",
                                   "vel_duracion_ruta_sem_", week, ".csv"), row.names = F)

# Se escriibe el compilado de velocidades
# 
# wb <- openxlsx::loadWorkbook(str_c("Z:/01 base_datos/",
#                                    "55 informe velocidades/compilado/" , 
#                                    "Velocidad_rutas_zonales.xlsx"))
# openxlsx::removeWorksheet(wb, "vel_compilado")
# compilado_km <- openxlsx::addWorksheet(wb, sheetName = as.character("vel_compilado"))
# openxlsx::writeDataTable(wb, "vel_compilado", as.data.frame(vel_historico), rowNames = F)
# openxlsx::saveWorkbook(wb, str_c("Z:/01 base_datos/",
#                                  "55 informe velocidades/compilado/" , 
#                                  "Velocidad_rutas_zonales.xlsx"), overwrite = T)

hora_fin_proceso <- now()
print(c(hora_inicio_proceso, hora_fin_proceso))

