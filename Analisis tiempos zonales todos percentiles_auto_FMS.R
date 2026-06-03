# Cargue de librerías --------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)
library(Hmisc)
library(openxlsx)
options(scipen = 999)
#library(openxlsx)

# Establecer datos de entrada

date_key_inicio <- 20260522
date_key_fin    <- 20260522

nombre_periodo <- "70.4 Abr-26"
TipoDia <- c("todos")  # Las opciones son "habil" - "sabado" - "festivo" o "todos". Se deben separar por coma e ir dentro de c()
vel_limite <- 40 # Velocidad en Km/h
intervalo <- 1.0 # Intervalo de analisis en horas

calendario_ejecucion <- readxl::read_excel("C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/Calendario_general.xlsx",
                                           sheet = "calendario") %>% 
  janitor::clean_names() %>% 
  mutate_at(.vars = c("desde", "hasta", "fecha_publicacion", "ref_habil", "ref_sabado", "ref_festivo", "ref_todos"), .funs = ymd) %>% 
  filter(nombre_carpeta == nombre_periodo)

exclusiones <- readxl::read_excel("C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/calendario_general.xlsx",
                                  sheet = "exclusiones") %>% 
  janitor::clean_names() %>% 
  mutate(fecha_exclusion = ymd(fecha_exclusion)) %>% 
  filter(nombre_carpeta == nombre_periodo)

# Hora de inicio del proceso
hora_inicio_proceso <- now()
# Cargue de Base de Datos de Fechas - dimdate -------------------------------------------------

calendario <- openxlsx::read.xlsx(
  "C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/calendario_2026.xlsx",
  sheet = "DimDate"
) %>% 
  janitor::clean_names() %>% 
  mutate(
    date_key = as.integer(date_key),
    is_weekday = if_else(is_weekday, 1L, 0L),
    is_holiday = as.integer(is_holiday),
    day_of_week = as.integer(day_of_week),
    date = as.Date(as.character(date_key), format = "%Y%m%d")
  ) %>% 
  filter(date_key >= date_key_inicio & date_key <= date_key_fin)


# Listado de archivos de viajes desglosados y actividad bus ----

vd_list <- list.files("Z:/01 base_datos/01 viajes_desglosados_FMS/", full.names = T)
ac_list <- list.files("Z:/01 base_datos/03 actividad_bus_FMS/", full.names = T)
md_list <- list.files("Z:/01 base_datos/06 matriz_distancia_FMS/", full.names = T)
iph_list <- list.files("Z:/01 base_datos/12 iph FMS/", full.names = T)

vd_list_data <- data.frame("vd" = vd_list, stringsAsFactors = FALSE) %>% 
  mutate(date_key = as.integer(str_extract(vd, "[0-9]{8}"))) %>% 
  filter(date_key >= date_key_inicio & date_key <= date_key_fin) %>% 
  select(date_key, everything())

ac_list_data <- data.frame("ac_bus" = ac_list, stringsAsFactors = FALSE) %>% 
  mutate(date_key = as.integer(str_extract(ac_bus, "[0-9]{8}"))) %>% 
  filter(date_key >= date_key_inicio & date_key <= date_key_fin) %>% 
  select(date_key, everything())

md_list_data <- data.frame("md" = md_list, stringsAsFactors = FALSE) %>% 
  mutate(date_key = as.integer(str_extract(md, "[0-9]{8}"))) %>% 
  filter(date_key >= date_key_inicio & date_key <= date_key_fin) %>% 
  select(date_key, everything())

iph_list_data <- data.frame("iph" = iph_list, stringsAsFactors = FALSE) %>% 
  mutate(date_key = as.integer(str_extract(iph, "[0-9]{8}"))) %>% 
  filter(date_key >= date_key_inicio & date_key <= date_key_fin) %>% 
  select(date_key, everything())


vd_list_data$date_key  <- as.integer(vd_list_data$date_key)
ac_list_data$date_key  <- as.integer(ac_list_data$date_key)
md_list_data$date_key  <- as.integer(md_list_data$date_key)
iph_list_data$date_key <- as.integer(iph_list_data$date_key)

for(tipo_dia in TipoDia){
  
  archivos_disponibles <- vd_list_data %>% 
    left_join(ac_list_data, by = "date_key") %>% 
    left_join(md_list_data, by = "date_key") %>%
    left_join(iph_list_data, by = "date_key") %>% 
    left_join(calendario, by = "date_key") %>%
    
    # Filtrar completos
    filter(!is.na(date)) %>%
    
    mutate(tipodia = case_when(
      (is_holiday == 1 | day_of_week == 7) ~ "festivo",
      day_of_week == 6 ~ "sabado",
      TRUE ~ "habil"
    ))
  
  # Filtrar tipo día SOLO si no es "todos"
  if(tipo_dia != "todos"){
    
    archivos_disponibles <- archivos_disponibles %>%
      filter(tipodia == tipo_dia)
  }
  
  # Exclusiones
  archivos_disponibles <- archivos_disponibles %>%
    filter(!date %in% exclusiones$fecha_exclusion)
  
  print(nrow(archivos_disponibles))
  
  # Lectura y procesamiento de viajes desglosados, actividad bus / Consolidado de ac_bus --------
  
  ac_bus_compilado_aux <- NULL
  
  for(i in 1:length(archivos_disponibles$date_key)){
  
    # Viajes desglosados
    vd <- fread(archivos_disponibles$vd[i],
                stringsAsFactors = F, skip = "Fecha") %>% 
      janitor::clean_names() %>%
      rename(linea = id_l_nea) %>%  
      select(fecha, servicio, id_viaje, linea, ruta) %>% 
      mutate(key_viaje = str_c(fecha, servicio, id_viaje, sep = "-")) %>% 
      select(key_viaje, linea, ruta)
  
    
    # Actividad bus
    
    ac_bus <- fread(archivos_disponibles$ac_bus[i],
                    stringsAsFactors = F, na.strings = "") %>% 
      janitor::clean_names() %>%
      rename(hora_teorica = hora_te_rica,
             id_linea = id_l_nea) %>% 
      select(
        -c(
          concesi_n,
          concesionario_de_operaci_n,
          c_digo_bus,
          n_mero_fms_bus,
          tabla,
          viaje_linea,
          orden_viaje,
          tipo_de_nodo,
          alarma_exceso_de_tiempo_en_parada,
          conductor,
          nombre_de_conductor,
          ruta
        )
      ) %>%
      mutate(hora_teorica = parse_date_time(hora_teorica, orders = "%H:%M:%S"),
             hora_llegada = parse_date_time(hora_llegada, orders = "%H:%M:%S"),
             hora_salida = parse_date_time(hora_salida, orders = "%H:%M:%S"),
             key_viaje = str_c(fecha, servicio_bus, id_viaje, sep = "-")) %>% 
      arrange(id_linea, servicio_bus, id_viaje, hora_teorica) %>% 
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
      mutate(ruta_nodo = str_c(ruta, nodo_real, sep = "-")) %>%
      select(ruta_nodo, hora_paso, t_viaje)
    
    ac_bus_compilado_aux <- rbind(ac_bus_compilado_aux, ac_bus)  
    
  }
  
  #-------------------------------------------------------------------------------------------------------#
  
  print("bandera 1")
  for(percentil in c(0.5, 0.6, 0.7, 0.75)){
    # Calculo estadistico para tiempos entre paraderos de cada RutasSae ---------------------------
    desde <- as.numeric(str_replace_all(calendario_ejecucion$desde[1], "-", ""))
    hasta <- as.numeric(str_replace_all(calendario_ejecucion$hasta[1], "-", ""))
    print("bandera 1 a")
    tiempos_prom_desv <- ac_bus_compilado_aux %>%
      group_by(ruta_nodo, hora_paso) %>% 
      summarise(t_viaje_prom = mean(t_viaje, na.rm = T),
                t_viaje_desv = sd(t_viaje, na.rm = T)) %>% 
      ungroup()
    print("bandera 1 b")
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
    print("bandera 1 c")
    horas <- data.frame("hora" = unique(ac_bus_compilado$hora_paso)) %>%
      na.omit() %>% 
      arrange(hora) %>% 
      mutate(hora = as.character(hora)) %>%
      filter(hora != 2)
    print("bandera 2")
    # Cargue de v_desg del día referente para analizar para determinar las RutasSae vigentes ------
    if(tipo_dia == "habil"){
      path_vd_actual <- (archivos_disponibles %>% 
                           filter(date == calendario_ejecucion$ref_habil[1]))$vd[1]
      path_md_actual <- (archivos_disponibles %>% 
                           filter(date == calendario_ejecucion$ref_habil[1]))$md[1]
      path_iph_actual <- (archivos_disponibles %>% 
                            filter(date == calendario_ejecucion$ref_habil[1]))$iph[1]
      dia_referente <- as.numeric(str_replace_all(calendario_ejecucion$ref_habil[1], "-", ""))
      
    }else if(tipo_dia == "sabado"){
      path_vd_actual <- (archivos_disponibles %>% 
                           filter(date == calendario_ejecucion$ref_sabado[1]))$vd[1]
      path_md_actual <- (archivos_disponibles %>% 
                           filter(date == calendario_ejecucion$ref_sabado[1]))$md[1]
      path_iph_actual <- (archivos_disponibles %>% 
                            filter(date == calendario_ejecucion$ref_sabado[1]))$iph[1]
      dia_referente <- as.numeric(str_replace_all(calendario_ejecucion$ref_sabado[1], "-", ""))
      
    }else if(tipo_dia == "festivo"){
      path_vd_actual <- (archivos_disponibles %>% 
                           filter(date == calendario_ejecucion$ref_festivo[1]))$vd[1]
      path_md_actual <- (archivos_disponibles %>% 
                           filter(date == calendario_ejecucion$ref_festivo[1]))$md[1]
      path_iph_actual <- (archivos_disponibles %>% 
                            filter(date == calendario_ejecucion$ref_festivo[1]))$iph[1]
      dia_referente <- as.numeric(str_replace_all(calendario_ejecucion$ref_festivo[1], "-", ""))
      
    }else{
      path_vd_actual <- (archivos_disponibles %>% 
                           filter(date == calendario_ejecucion$ref_todos[1]))$vd[1]
      path_md_actual <- (archivos_disponibles %>% 
                           filter(date == calendario_ejecucion$ref_todos[1]))$md[1]
      path_iph_actual <- (archivos_disponibles %>% 
                            filter(date == calendario_ejecucion$ref_todos[1]))$iph[1]
      dia_referente <- as.numeric(str_replace_all(calendario_ejecucion$ref_todos[1], "-", ""))
      
    }
    
    print("bandera 3")
    vd_actual <- fread(path_vd_actual, stringsAsFactors = F) %>%
      janitor::clean_names() %>%
      rename(id_linea = id_l_nea) %>%
      transmute(key_viaje = str_c(fecha, servicio, id_viaje, sep = "-"), id_linea, ruta)
    
    # RutasSae vigentes----------------------------------------------------------------------------
    rutas_sae <- data.frame("ruta" = unique(vd_actual$ruta)) %>% 
      na.omit()
    
    # Matriz de distancias----------------------------------------------------------------------
    md <- fread(path_md_actual) %>% 
      janitor::clean_names() %>% 
      arrange(id_linea, id_ruta, posicion)
    
    tabla_rutas <- md %>%
      rename(ruta = linea) %>%
      select(id_linea, id_ruta, ruta) %>%
      distinct()
    
    # Tiempos entre paraderos y cantidad de datos ------------------------------------------------
    tiempos_y_n_datos <- md %>%
      select(id_linea, id_ruta, id_nodo, nombre_nodo, posicion) %>%
      mutate(
        nodo_real = case_when(
          id_ruta != lead(id_ruta) ~
            str_c(id_nodo, "Fin Viaje", sep = "-"),
          TRUE ~
            as.character(id_nodo)
        ),
        ruta_nodo = str_c(id_ruta, nodo_real, sep = "-")
      ) %>%
      group_by(id_linea,
               id_ruta,
               id_nodo,
               nombre_nodo,
               nodo_real,
               ruta_nodo) %>%
      summarise(posicion = max(posicion)) %>%
      left_join(ac_bus_compilado %>% select(ruta_nodo, hora_paso, duracion, n_datos),
                by = "ruta_nodo") %>%
      arrange(id_linea, id_ruta, posicion, hora_paso) %>%
      mutate(
        duracion = case_when(posicion == 0 ~ as.numeric(0), TRUE ~ duracion),
        n_datos = case_when(posicion == 0 ~ as.integer(0), TRUE ~ n_datos)
      ) %>%
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
    
    # Datos imputados tras relleno de datos usando laterales y velocidades prom ------------------
    imputados_laterales <- tiempos2
    imputados_vel <- tiempos3
    
    for(i in 1L:as.integer(length(imputados_vel$id_ruta))){
      for(j in str_which(names(imputados_vel), str_c("^", first(horas$hora), "$")):
          str_which(names(imputados_vel), str_c("^", last(horas$hora), "$"))){
        imputados_laterales[[i,j]] <- case_when(!is.na(tiempos[[i,j]]) ~ 0L,
                                                is.na(tiempos[[i,j]]) & !is.na(tiempos2[[i,j]])~ 1L,
                                                TRUE ~ 0L)
        imputados_vel[[i,j]] <- case_when(!is.na(tiempos2[[i,j]]) ~ 0L,
                                          is.na(tiempos2[[i,j]]) & !is.na(tiempos3[[i,j]]) ~ 1L,
                                          TRUE ~ 0L)
      }
    }
    
    # Verificación de tiempos que superen en más de 3 veces sus laterales -------------------------
    
    tiempos3 <- tiempos3 %>% mutate(aux1 = 0.1, aux2 =0.2) # columnas auxiliares para loop
    
    for(i in 1L:as.integer(length(tiempos3$id_ruta))){
      for(j in str_which(names(tiempos3), str_c("^", first(horas$hora), "$")):
          str_which(names(tiempos3), str_c("^", last(horas$hora), "$"))){
        tiempos3[[i,j]]  <-  case_when((j  >= (str_which(names(tiempos3), str_c("^", first(horas$hora), "$")) + 2)) & 
                                         (j  <= (str_which(names(tiempos3), str_c("^", last(horas$hora), "$")) - 2)) ~ 
                                         case_when(tiempos3[[i,j]] > 10 & tiempos3[[i,j-1L]] != 0 &
                                                     tiempos3[[i,j+1L]] != 0 & 
                                                     (tiempos3[[i,j]] > 5 + tiempos3[[i,j-1L]] | 
                                                        tiempos3[[i,j]] > 5 + tiempos3[[i,j+1L]]) ~
                                                     5 + min(c(tiempos3[[i,j-1L]], 
                                                               tiempos3[[i,j+1L]]), na.rm = T),
                                                   tiempos3[[i,j]] > 10 & tiempos3[[i,j-1L]] != 0 &
                                                     tiempos3[[i,j+1L]] == 0 & 
                                                     tiempos3[[i,j]] > (5 + tiempos3[[i,j-1L]]) ~
                                                     5 + tiempos3[[i,j-1L]],
                                                   tiempos3[[i,j]] > 10 & tiempos3[[i,j-1L]] == 0 &
                                                     tiempos3[[i,j+1L]] != 0 & 
                                                     tiempos3[[i,j]] > (5 + tiempos3[[i,j+1L]]) ~
                                                     5 + tiempos3[[i,j+1L]],
                                                   tiempos3[[i,j]] <= 10 & tiempos3[[i,j-1L]] != 0 &
                                                     tiempos3[[i,j+1L]] != 0 &
                                                     (tiempos3[[i,j]] > 3 * tiempos3[[i,j-1L]] | 
                                                        tiempos3[[i,j]] > 3 * tiempos3[[i,j+1L]]) ~
                                                     3 * min(c(tiempos3[[i,j-1L]], 
                                                               tiempos3[[i,j+1L]]), na.rm = T),
                                                   tiempos3[[i,j]] <= 10 & tiempos3[[i,j-1L]] != 0 &
                                                     tiempos3[[i,j+1L]] == 0 &
                                                     tiempos3[[i,j]] > (3 * tiempos3[[i,j-1L]]) ~
                                                     3 * tiempos3[[i,j-1L]],
                                                   tiempos3[[i,j]] <= 10 & tiempos3[[i,j-1L]] == 0 &
                                                     tiempos3[[i,j+1L]] != 0 &
                                                     tiempos3[[i,j]] > (3 * tiempos3[[i,j+1L]]) ~
                                                     3 * tiempos3[[i,j+1L]],
                                                   TRUE ~ tiempos3[[i,j]]),
                                       (j  < (str_which(names(tiempos3), str_c("^", first(horas$hora), "$")) + 2)) ~ 
                                         case_when(tiempos3[[i,j]] > 10 & tiempos3[[i,j+1L]] != 0 & 
                                                     tiempos3[[i,j]] > (5 + tiempos3[[i,j+1L]]) ~
                                                     5 + tiempos3[[i,j+1L]],
                                                   tiempos3[[i,j]] <= 10 & tiempos3[[i,j+1L]] != 0 &
                                                     tiempos3[[i,j]] > (3 * tiempos3[[i,j+1L]]) ~
                                                     3 * tiempos3[[i,j+1L]],
                                                   TRUE ~ tiempos3[[i,j]]),
                                       (j  > (str_which(names(tiempos3), str_c("^", last(horas$hora), "$")) - 2)) ~ 
                                         case_when(tiempos3[[i,j]] > 10 & tiempos3[[i,j-1L]] != 0 & 
                                                     tiempos3[[i,j]] > (5 + tiempos3[[i,j-1L]]) ~
                                                     5 + tiempos3[[i,j-1L]],
                                                   tiempos3[[i,j]] <= 10 & tiempos3[[i,j-1L]] != 0 &
                                                     tiempos3[[i,j]] > (3 * tiempos3[[i,j-1L]]) ~
                                                     3 * tiempos3[[i,j-1L]],
                                                   TRUE ~ tiempos3[[i,j]]),
                                       TRUE ~ tiempos3[[i,j]])
      }
    }
    
    tiempos3 <- tiempos3 %>% 
      select(-c(aux1, aux2))
    
    # Cálculo de matriz de tiempos acumulados por ruta y franja horaria. --------------------------
    # La matriz se fila a fila y luego en columnas.
    
    tiempos3 <- tiempos3 %>% mutate(aux1 = 0.1, aux2 = 0.1, aux3 = 0.1, aux4 = 0.1, aux5 = 0.1, 
                                    aux6 = 0.1, aux7 = 0.1, aux8 = 0.1, aux9 = 0.1, aux10 = 0.1)
    tiempos_cum <- tiempos3
    
    for (j in str_which(names(tiempos_cum), str_c("^", first(horas$hora), "$")):str_which(names(tiempos_cum), str_c("^", last(horas$hora), "$"))) {
      for (i in 2L:as.integer(length(tiempos_cum$id_ruta))) {
        tiempos_cum[[i, j]]  <- case_when(
          tiempos_cum$id_ruta[[i - 1]] != tiempos_cum$id_ruta[[i]] ~ tiempos3[[i, j]],
          ((tiempos_cum[[i - 1L, j]] + tiempos3[[i, j]]) %/% (intervalo * 60)) < 1L ~
            tiempos_cum[[i - 1L, j]] + tiempos3[[i, j]],
          (j + ((tiempos_cum[[i - 1L, j]] + tiempos3[[i, j + (tiempos_cum[[i -
                                                                             1L, j]] %/% (intervalo * 60))]]) %/% (intervalo * 60)
          ))
          <= str_which(names(tiempos_cum), str_c("^", last(horas$hora), "$")) ~
            tiempos_cum[[i - 1L, j]] +
            tiempos3[[i, j + ((tiempos_cum[[i -
                                              1L, j]] + tiempos3[[i, j + (tiempos_cum[[i - 1L, j]] %/% (intervalo * 60))]]) %/% (intervalo * 60))]],
          TRUE ~ tiempos_cum[[i - 1, j]] + tiempos3[[i, str_which(names(tiempos_cum), str_c("^", last(horas$hora), "$"))]]
        )
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
      pivot_wider(id_cols = c(id_linea, id_ruta, longitud), names_from = "hora", 
                  values_from = "time_cum") %>% 
      select(id_linea, id_ruta, longitud, horas$hora) %>% 
      arrange(id_linea)
    
    velocidad_resumen <- tiempos_cum %>% 
      pivot_longer(cols = -c(id_linea, id_ruta, id_nodo, nombre_nodo, nodo_real, ruta_nodo, 
                             posicion, dist_relativa), 
                   names_to = "hora", values_to = "time_cum") %>% 
      group_by(id_linea, id_ruta, hora) %>% 
      summarise(time_cum = max(time_cum, na.rm = T)/60,
                longitud = max(posicion, na.rm = T)/1000) %>%
      mutate(vel_ruta_franja = if_else(time_cum == 0, 0, longitud / time_cum)) %>% 
      pivot_wider(id_cols = c(id_linea, id_ruta, longitud), names_from = "hora", 
                  values_from = "vel_ruta_franja") %>% 
      select(id_linea, id_ruta, longitud, horas$hora) %>% 
      arrange(id_linea)
    
    
    # Tiempos programados con dia referente --------------------------------------------------
    
    # Se debe modificar al igual que el comparativo de tiempos para hacer un left join y dejar los valores de todos los percentiles en un solo file
    
    
    iph <- fread(
      path_iph_actual,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA", "NULL")
    ) %>% 
      
      janitor::clean_names() %>%
      
      rename(id_linea = id_l_nea) %>%
      
      filter(!is.na(id_ruta)) %>% 
      
      select(
        instante,
        servicio_bus,
        evento,
        id_linea,
        tabla,
        id_ruta,
        id_nodo,
        viaje,
        tipo_vehiculo
      ) %>% 
      
      mutate(
        
        # Limpiar texto
        instante = str_trim(as.character(instante)),
        
        # Extraer HH:MM:SS
        instante = str_extract(instante, "\\d{1,2}:\\d{2}:\\d{2}"),
        
        # Separar componentes
        hora = suppressWarnings(as.numeric(str_extract(instante, "^\\d{1,2}"))),
        
        minuto = suppressWarnings(as.numeric(str_extract(instante, "(?<=:)\\d{2}(?=:)"))),
        
        segundo = suppressWarnings(as.numeric(str_extract(instante, "\\d{2}$"))),
        
        # Validación
        instante_min = case_when(
          
          !is.na(hora) &
            !is.na(minuto) &
            !is.na(segundo) ~
            
            hora * 60 +
            minuto +
            segundo / 60,
          
          TRUE ~ NA_real_
        ),
        
        # Franja horaria
        hora_paso = ((instante_min / 60) %/% intervalo) * intervalo
      ) %>% 
      
      # Eliminar registros inválidos
      filter(
        !is.na(instante_min),
        instante_min >= 0
      ) %>% 
      
      arrange(servicio_bus, viaje, instante_min) %>% 
      
      mutate(
        fila = 1:length(instante),
        
        duracion = case_when(
          fila == 1 ~ 0,
          
          servicio_bus == lag(servicio_bus) &
            viaje == lag(viaje) ~
            instante_min - lag(instante_min),
          
          TRUE ~ as.numeric(0)
        )
      ) %>% 
      
      group_by(servicio_bus, viaje) %>% 
      
      mutate(
        duracion_cum = cumsum(duracion),
        
        hora_salida = min(hora_paso, na.rm = TRUE),
        
        duracion_viaje = max(duracion_cum, na.rm = TRUE)
      ) %>% 
      
      ungroup() %>% 
      
      filter(evento == 4 | evento == 12) %>% 
      
      group_by(id_linea, id_ruta, hora_salida) %>% 
      
      summarise(
        tiempo_prog = hms::as_hms(
          round(mean(duracion_cum, na.rm = TRUE) * 60, 0)
        ),
        .groups = "drop"
      ) %>% 
      
      arrange(id_linea, id_ruta, hora_salida)
    
    
    # Validación opcional para revisar instantes inválidos ------------------------------------
    
    iph_invalidos <- fread(
      path_iph_actual,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA", "NULL")
    ) %>% 
      
      janitor::clean_names() %>%
      
      transmute(
        
        instante_original = instante,
        
        instante = str_trim(as.character(instante)),
        
        instante = str_extract(instante, "\\d{1,2}:\\d{2}:\\d{2}"),
        
        hora = suppressWarnings(as.numeric(str_extract(instante, "^\\d{1,2}"))),
        
        minuto = suppressWarnings(as.numeric(str_extract(instante, "(?<=:)\\d{2}(?=:)"))),
        
        segundo = suppressWarnings(as.numeric(str_extract(instante, "\\d{2}$")))
        
      ) %>% 
      
      filter(
        is.na(hora) |
          is.na(minuto) |
          is.na(segundo)
      )
    
    print(unique(iph_invalidos$instante_original))
    
    
    # Comparativo entre tiempos reales y tiempos programados ---------------------------------
    
    comparativo_tiempos <- tiempos_resumen %>%
      pivot_longer(
        cols = -c(id_linea, id_ruta, longitud),
        names_to = "hora_salida",
        values_to = "tiempo_real"
      ) %>%
      mutate(
        hora_salida = as.numeric(hora_salida),
        tiempo_real = hms::as_hms(tiempo_real)
      ) %>%
      left_join(iph, by = c("id_linea", "id_ruta", "hora_salida")) %>%
      mutate(
        diferencia = round((tiempo_prog - tiempo_real) / 60, 2),
        tiempo_prog = as.character(tiempo_prog),
        tiempo_real = as.character(tiempo_real)
      )
    
    
    # Resumen comparativo tiempos ------------------------------------------------------------
    
    balance_comparativo <- comparativo_tiempos %>%
      filter(!is.na(tiempo_prog)) %>%
      group_by(id_linea, id_ruta, longitud) %>%
      summarise(
        falta_tiempo = sum(diferencia < -10, na.rm = T),
        sobra_tiempo = sum(diferencia > 10, na.rm = T)
      ) %>%
      ungroup() %>%
      arrange(id_linea, id_ruta)
    
    if(percentil == 0.5){
      comparativo_tiempos_0.50 <- comparativo_tiempos
    }else if(percentil == 0.6){
      comparativo_tiempos_0.60 <- comparativo_tiempos
    }else if(percentil == 0.7){
      comparativo_tiempos_0.70 <- comparativo_tiempos
    }else{
      comparativo_tiempos_0.75 <- comparativo_tiempos
      
      comparativo_tiempos_todos <- comparativo_tiempos_0.50 %>% 
        mutate(operacion = "Zonal",
               fecha_desde = ymd(calendario_ejecucion$desde),
               fecha_hasta = ymd(calendario_ejecucion$hasta),
               fecha_ref = ymd(archivos_disponibles$date[length(archivos_disponibles$date)]),
               tipo_dia = str_to_sentence(tipo_dia)) %>% 
        left_join(comparativo_tiempos_0.60 %>%
                    ungroup() %>% 
                    select(id_linea, id_ruta, hora_salida, tiempo_real),
                  by = c("id_linea", "id_ruta", "hora_salida"),
                  suffix = c("_0.50", "_0.60")) %>% 
        left_join(comparativo_tiempos_0.70 %>%
                    ungroup() %>% 
                    select(id_linea, id_ruta, hora_salida, tiempo_real),
                  by = c("id_linea", "id_ruta", "hora_salida")) %>%
        left_join(comparativo_tiempos_0.75 %>% 
                    ungroup() %>% 
                    select(id_linea, id_ruta, hora_salida, tiempo_real),
                  by = c("id_linea", "id_ruta", "hora_salida"),
                  suffix = c("_0.70", "_0.75")) %>% 
        select(operacion:tipo_dia, id_linea:hora_salida, tiempo_prog, 
               "tiempo_perc_0.50" = tiempo_real_0.50, "tiempo_perc_0.60" = tiempo_real_0.60, 
               "tiempo_perc_0.70" = tiempo_real_0.70, "tiempo_perc_0.75" = tiempo_real_0.75)
      
      comparativo_tiempos_todos_min <- comparativo_tiempos_todos %>% 
        mutate(tiempo_prog = round(as.numeric(str_sub(tiempo_prog, 1L, 2L)) * 60 +
                                     as.numeric(str_sub(tiempo_prog, 4L, 5L)) + 
                                     as.numeric(str_sub(tiempo_prog, 7L, 8L)) / 60, digits = 4),
               tiempo_perc_0.50 = round(as.numeric(str_sub(tiempo_perc_0.50, 1L, 2L)) * 60 +
                                          as.numeric(str_sub(tiempo_perc_0.50, 4L, 5L)) + 
                                          as.numeric(str_sub(tiempo_perc_0.50, 7L, 8L)) / 60, digits = 4),
               tiempo_perc_0.60 = round(as.numeric(str_sub(tiempo_perc_0.60, 1L, 2L)) * 60 +
                                          as.numeric(str_sub(tiempo_perc_0.60, 4L, 5L)) + 
                                          as.numeric(str_sub(tiempo_perc_0.60, 7L, 8L)) / 60, digits = 4),
               tiempo_perc_0.70 = round(as.numeric(str_sub(tiempo_perc_0.70, 1L, 2L)) * 60 +
                                          as.numeric(str_sub(tiempo_perc_0.70, 4L, 5L)) + 
                                          as.numeric(str_sub(tiempo_perc_0.70, 7L, 8L)) / 60, digits = 4),
               tiempo_perc_0.75 = round(as.numeric(str_sub(tiempo_perc_0.75, 1L, 2L)) * 60 +
                                          as.numeric(str_sub(tiempo_perc_0.75, 4L, 5L)) + 
                                          as.numeric(str_sub(tiempo_perc_0.75, 7L, 8L)) / 60, digits = 4))
      
      comparativo_tiempos_todos_min <- comparativo_tiempos_todos_min %>%
        left_join(
          tabla_rutas %>%
            select(id_linea, id_ruta, ruta),
          by = c("id_linea", "id_ruta")
        ) %>%
        select(operacion:tipo_dia, ruta, id_linea:tiempo_perc_0.75)
      
      #comparativo_tiempos_todos %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
      #                                                         "_comparativo_tiempos.xlsx"))
      #comparativo_tiempos_todos_min %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
      #                                                             "_comparativo_tiempos_min.xlsx"))
      comparativo_tiempos_todos %>% write.csv(str_c("C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/zonal/", 
                                                    desde, "_", hasta, "_", tipo_dia,
                                                    "_comparativo_tiempos.csv"), row.names = F, na = "")
      
    }
    
    
    # Generación de archivos  csv y xlsx ----------------------------------------------------------
    if(percentil == 0.5){
      dir.create(str_c("C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/01_ZONAL/", nombre_periodo, "/"))
      
      dir.create(str_c("C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/01_ZONAL/", nombre_periodo, "/", desde,
                       "_", hasta, "_", tipo_dia, "_int_", intervalo, "_ref_", 
                       dia_referente, "/"))
      
      carpeta <- str_c("C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/01_ZONAL/", nombre_periodo, "/", desde,
                       "_", hasta, "_", tipo_dia, "_int_", intervalo, "_ref_", 
                       dia_referente, "/")
    }
     
    
    tiempos3 <- tiempos3 %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:"24")
    
    tiempos_cum <- tiempos_cum %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:"24")
    
    tiempos_resumen <- tiempos_resumen %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:"24")
    
    comparativo_tiempos <- comparativo_tiempos %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:diferencia)
    
    balance_comparativo <- balance_comparativo %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:sobra_tiempo)
    
    velocidad <- velocidad %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:"24")
    
    velocidad_resumen <- velocidad_resumen %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:"24")
    
    n_datos <- n_datos %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:"24")
    
    imputados_laterales <- imputados_laterales %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:"24")
    
    imputados_vel <- imputados_vel %>%
      left_join(
        tabla_rutas %>% 
          select(id_linea, id_ruta, ruta),
        by = c("id_linea", "id_ruta")
      ) %>%
      select(ruta, id_linea:"24")
    
    tiempos3 %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                            "_tiempos_p", percentil, ".xlsx"))
    tiempos_cum %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia, 
                                              "_tiempos_acumulados_p", percentil, ".xlsx"))
    tiempos_resumen %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                                   "_tiempos_resumen_p", percentil, ".xlsx"))
    comparativo_tiempos %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                                       "_comparativo_tiempos_p", percentil, ".xlsx"))
    balance_comparativo %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                                       "_balance_comparativo_p", percentil, ".xlsx"))
    velocidad %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                             "_velocidad_p", percentil, ".xlsx"))
    velocidad_resumen %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                                     "_velocidad_resumen_p", percentil, ".xlsx"))
    n_datos %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                           "_cantidad_datos_p", percentil, ".xlsx"))
    imputados_laterales %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                                       "_imputados_lateral_p", percentil, ".xlsx"))
    imputados_vel %>% openxlsx::write.xlsx(str_c(carpeta, desde, "_", hasta, "_", tipo_dia,
                                                 "_imputados_vel_p", percentil, ".xlsx"))
    
  }
  
} # Este for cierra el recorrido por los TipoDia
# Hora de finalizacion del proceso
hora_fin_proceso <- now()
print(c(hora_inicio_proceso, hora_fin_proceso)) 