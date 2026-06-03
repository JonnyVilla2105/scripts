# =====================================================================================
# ANALISIS TIEMPOS ZONALES - VERSION OPTIMIZADA
# =====================================================================================

# -----------------------------------------------------------------------------
# LIBRERIAS
# -----------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(openxlsx)
library(janitor)
library(hms)

options(scipen = 999)

# -----------------------------------------------------------------------------
# PARAMETROS
# -----------------------------------------------------------------------------

date_key_inicio <- 20260428
date_key_fin    <- 20260428

nombre_periodo <- "67.4 Mar-26"

TipoDia <- c("todos")

vel_limite <- 40

# IMPORTANTE:
# aumentar intervalo reduce MUCHISIMO tiempo de proceso
intervalo <- 2

# -----------------------------------------------------------------------------
# TIEMPO INICIO
# -----------------------------------------------------------------------------

hora_inicio_proceso <- Sys.time()

print(hora_inicio_proceso)

# -----------------------------------------------------------------------------
# FUNCION AUXILIAR DE TIEMPO
# -----------------------------------------------------------------------------

log_tiempo <- function(texto){
  
  cat(
    "\n====================================================\n",
    texto,
    " - ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "\n====================================================\n"
  )
  
  gc()
}

# -----------------------------------------------------------------------------
# CALENDARIO
# -----------------------------------------------------------------------------

log_tiempo("Leyendo calendario")

calendario_ejecucion <- readxl::read_excel(
  "C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/Calendario_general.xlsx",
  sheet = "calendario"
) %>%
  clean_names() %>%
  mutate(
    across(
      c(desde, hasta, fecha_publicacion,
        ref_habil, ref_sabado,
        ref_festivo, ref_todos),
      ymd
    )
  ) %>%
  filter(nombre_carpeta == nombre_periodo)

# -----------------------------------------------------------------------------
# EXCLUSIONES
# -----------------------------------------------------------------------------

exclusiones <- readxl::read_excel(
  "C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/calendario_general.xlsx",
  sheet = "exclusiones"
) %>%
  clean_names() %>%
  mutate(fecha_exclusion = ymd(fecha_exclusion)) %>%
  filter(nombre_carpeta == nombre_periodo)

# -----------------------------------------------------------------------------
# DIMDATE
# -----------------------------------------------------------------------------

log_tiempo("Leyendo DimDate")

calendario <- openxlsx::read.xlsx(
  "C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/calendario_2026.xlsx",
  sheet = "DimDate"
) %>%
  clean_names() %>%
  mutate(
    date_key = as.integer(date_key),
    is_weekday = if_else(is_weekday, 1L, 0L),
    is_holiday = as.integer(is_holiday),
    day_of_week = as.integer(day_of_week),
    date = as.Date(as.character(date_key), format = "%Y%m%d")
  ) %>%
  filter(date_key >= date_key_inicio &
           date_key <= date_key_fin)

# -----------------------------------------------------------------------------
# LISTADO ARCHIVOS
# -----------------------------------------------------------------------------

log_tiempo("Listando archivos")

vd_list <- list.files(
  "Z:/01 base_datos/01 viajes_desglosados_FMS/",
  full.names = TRUE
)

ac_list <- list.files(
  "Z:/01 base_datos/03 actividad_bus_FMS/",
  full.names = TRUE
)

md_list <- list.files(
  "Z:/01 base_datos/06 matriz_distancia_FMS/",
  full.names = TRUE
)

iph_list <- list.files(
  "Z:/01 base_datos/12 iph FMS/",
  full.names = TRUE
)

# -----------------------------------------------------------------------------
# DATASETS ARCHIVOS
# -----------------------------------------------------------------------------

crear_dataset_archivos <- function(lista, nombre_col){
  
  data.frame(path = lista) %>%
    mutate(
      date_key = as.integer(
        str_extract(path, "[0-9]{8}")
      )
    ) %>%
    filter(
      date_key >= date_key_inicio &
        date_key <= date_key_fin
    ) %>%
    rename(!!nombre_col := path)
}

vd_list_data  <- crear_dataset_archivos(vd_list,  "vd")
ac_list_data  <- crear_dataset_archivos(ac_list,  "ac_bus")
md_list_data  <- crear_dataset_archivos(md_list,  "md")
iph_list_data <- crear_dataset_archivos(iph_list, "iph")

# -----------------------------------------------------------------------------
# UNION ARCHIVOS
# -----------------------------------------------------------------------------

archivos_disponibles <- vd_list_data %>%
  left_join(ac_list_data,  by = "date_key") %>%
  left_join(md_list_data,  by = "date_key") %>%
  left_join(iph_list_data, by = "date_key") %>%
  left_join(calendario, by = "date_key") %>%
  filter(!is.na(date))

# -----------------------------------------------------------------------------
# TIPO DIA
# -----------------------------------------------------------------------------

archivos_disponibles <- archivos_disponibles %>%
  mutate(
    tipodia = case_when(
      (is_holiday == 1 | day_of_week == 7) ~ "festivo",
      day_of_week == 6 ~ "sabado",
      TRUE ~ "habil"
    )
  )

# -----------------------------------------------------------------------------
# EXCLUSIONES
# -----------------------------------------------------------------------------

archivos_disponibles <- archivos_disponibles %>%
  filter(!date %in% exclusiones$fecha_exclusion)

# -----------------------------------------------------------------------------
# RECORRIDO TIPO DIA
# -----------------------------------------------------------------------------

for(tipo_dia in TipoDia){
  
  log_tiempo(paste("Procesando", tipo_dia))
  
  archivos_tipo <- archivos_disponibles
  
  if(tipo_dia != "todos"){
    
    archivos_tipo <- archivos_tipo %>%
      filter(tipodia == tipo_dia)
  }
  
  print(nrow(archivos_tipo))
  
  # ===========================================================================
  # ACUMULADOR
  # ===========================================================================
  
  lista_ac <- list()
  
  # ===========================================================================
  # RECORRIDO ARCHIVOS
  # ===========================================================================
  
  for(i in seq_len(nrow(archivos_tipo))){
    
    log_tiempo(
      paste(
        "Archivo",
        i,
        "de",
        nrow(archivos_tipo)
      )
    )
    
    # -------------------------------------------------------------------------
    # VIAJES DESGLOSADOS
    # -------------------------------------------------------------------------
    
    vd <- fread(
      archivos_tipo$vd[i],
      skip = "Fecha",
      showProgress = FALSE
    ) %>%
      clean_names() %>%
      rename(linea = id_l_nea) %>%
      transmute(
        key_viaje = str_c(
          fecha,
          servicio,
          id_viaje,
          sep = "-"
        ),
        linea,
        ruta
      )
    
    # -------------------------------------------------------------------------
    # ACTIVIDAD BUS
    # -------------------------------------------------------------------------
    
    ac_bus <- fread(
      archivos_tipo$ac_bus[i],
      na.strings = "",
      showProgress = FALSE
    )
    
    setDT(ac_bus)
    
    names(ac_bus) <- make_clean_names(names(ac_bus))
    
    setnames(
      ac_bus,
      old = c("hora_te_rica", "id_l_nea"),
      new = c("hora_teorica", "id_linea"),
      skip_absent = TRUE
    )
    
    columnas_keep <- c(
      "fecha",
      "servicio_bus",
      "id_viaje",
      "id_linea",
      "id_nodo",
      "evento",
      "hora_teorica",
      "hora_llegada",
      "hora_salida"
    )
    
    columnas_keep <- columnas_keep[
      columnas_keep %in% names(ac_bus)
    ]
    
    ac_bus <- ac_bus[, ..columnas_keep]
    
    # -------------------------------------------------------------------------
    # KEY VIAJE
    # -------------------------------------------------------------------------
    
    ac_bus[, key_viaje := paste(
      fecha,
      servicio_bus,
      id_viaje,
      sep = "-"
    )]
    
    # -------------------------------------------------------------------------
    # JOIN RAPIDO
    # -------------------------------------------------------------------------
    
    vd <- as.data.table(vd)
    
    ac_bus <- merge(
      ac_bus,
      vd,
      by = "key_viaje",
      all.x = TRUE
    )
    
    # -------------------------------------------------------------------------
    # FECHAS
    # -------------------------------------------------------------------------
    
    ac_bus[, hora_salida := suppressWarnings(
      parse_date_time(hora_salida, orders = "%H:%M:%S")
    )]
    
    # -------------------------------------------------------------------------
    # MINUTOS
    # -------------------------------------------------------------------------
    
    ac_bus[, h_real :=
             hour(hora_salida) * 60 +
             minute(hora_salida) +
             second(hora_salida) / 60]
    
    # -------------------------------------------------------------------------
    # ORDEN
    # -------------------------------------------------------------------------
    
    setorder(
      ac_bus,
      key_viaje,
      h_real
    )
    
    # -------------------------------------------------------------------------
    # TIEMPO VIAJE
    # -------------------------------------------------------------------------
    
    ac_bus[, t_viaje :=
             h_real - shift(h_real),
           by = key_viaje]
    
    ac_bus[
      t_viaje < 0 |
        t_viaje > 120,
      t_viaje := NA
    ]
    
    # -------------------------------------------------------------------------
    # HORA PASO
    # -------------------------------------------------------------------------
    
    ac_bus[, hora_paso :=
             floor((h_real / 60) / intervalo) * intervalo]
    
    # -------------------------------------------------------------------------
    # RUTA NODO
    # -------------------------------------------------------------------------
    
    ac_bus[, ruta_nodo :=
             paste(ruta, id_nodo, sep = "-")]
    
    # -------------------------------------------------------------------------
    # COLUMNAS FINALES
    # -------------------------------------------------------------------------
    
    ac_bus <- ac_bus[
      ,
      .(
        ruta_nodo,
        hora_paso,
        t_viaje
      )
    ]
    
    lista_ac[[i]] <- ac_bus
    
    rm(ac_bus)
    
    gc()
  }
  
  # ===========================================================================
  # CONSOLIDADO
  # ===========================================================================
  
  log_tiempo("Consolidando actividad bus")
  
  ac_bus_compilado_aux <- rbindlist(
    lista_ac,
    fill = TRUE
  )
  
  rm(lista_ac)
  
  gc()
  
  # ===========================================================================
  # CALCULO TODOS LOS PERCENTILES EN UNA SOLA VEZ
  # ===========================================================================
  
  log_tiempo("Calculando percentiles")
  
  percentiles <- ac_bus_compilado_aux[
    ,
    .(
      p50 = quantile(t_viaje, 0.50, na.rm = TRUE),
      p60 = quantile(t_viaje, 0.60, na.rm = TRUE),
      p70 = quantile(t_viaje, 0.70, na.rm = TRUE),
      p75 = quantile(t_viaje, 0.75, na.rm = TRUE),
      promedio = mean(t_viaje, na.rm = TRUE),
      desviacion = sd(t_viaje, na.rm = TRUE),
      n_datos = sum(!is.na(t_viaje))
    ),
    by = .(
      ruta_nodo,
      hora_paso
    )
  ]
  
  # ===========================================================================
  # ELIMINAR ATIPICOS
  # ===========================================================================
  
  log_tiempo("Eliminando atipicos")
  
  ac_bus_compilado_aux <- merge(
    ac_bus_compilado_aux,
    percentiles[
      ,
      .(
        ruta_nodo,
        hora_paso,
        promedio,
        desviacion
      )
    ],
    by = c("ruta_nodo", "hora_paso"),
    all.x = TRUE
  )
  
  ac_bus_compilado_aux[
    t_viaje < (promedio - 3 * desviacion) |
      t_viaje > (promedio + 3 * desviacion),
    t_viaje := NA
  ]
  
  # ===========================================================================
  # RECALCULAR
  # ===========================================================================
  
  log_tiempo("Recalculando percentiles")
  
  resultados <- ac_bus_compilado_aux[
    ,
    .(
      tiempo_perc_0_50 = quantile(t_viaje, 0.50, na.rm = TRUE),
      tiempo_perc_0_60 = quantile(t_viaje, 0.60, na.rm = TRUE),
      tiempo_perc_0_70 = quantile(t_viaje, 0.70, na.rm = TRUE),
      tiempo_perc_0_75 = quantile(t_viaje, 0.75, na.rm = TRUE),
      n_datos = sum(!is.na(t_viaje))
    ),
    by = .(
      ruta_nodo,
      hora_paso
    )
  ]
  
  # ===========================================================================
  # EXPORTAR SOLO CSV
  # ===========================================================================
  
  log_tiempo("Exportando CSV")
  
  fwrite(
    resultados,
    paste0(
      "C:/Users/Jonny Villareal/OneDrive - Gmovil SAS/Escritorio/Jonny/Control 2026/Velocidad/zonal/",
      tipo_dia,
      "_resultados.csv"
    )
  )
  
  # ===========================================================================
  # LIMPIEZA
  # ===========================================================================
  
  rm(
    resultados,
    ac_bus_compilado_aux,
    percentiles
  )
  
  gc()
}

# -----------------------------------------------------------------------------
# FINAL
# -----------------------------------------------------------------------------

hora_fin_proceso <- Sys.time()

print(hora_inicio_proceso)
print(hora_fin_proceso)

print(
  difftime(
    hora_fin_proceso,
    hora_inicio_proceso,
    units = "mins"
  )
)