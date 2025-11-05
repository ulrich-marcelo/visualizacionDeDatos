imputar_por_vecinos <- function(data, departamentos, sentinel = -99, agg_fun = median, strict_border = FALSE) {
  library(dplyr)
  library(sf)
  library(purrr)
  library(tidyr)
  library(lubridate)
  
  # --- Normalización básica ---
  data <- data %>%
    mutate(
      fecha = as.Date(fecha),
      mes = floor_date(fecha, "month"),
      codigo_departamento_indec = as.integer(codigo_departamento_indec),
      id_provincia_indec = as.integer(id_provincia_indec),
      w_median = as.numeric(w_median)
    )
  
  # --- Determinar columnas de agrupamiento adicionales ---
  group_cols <- c("codigo_departamento_indec", "mes")
  if ("clae2" %in% names(data)) {
    group_cols <- c(group_cols, "clae2")
  }
  
  # --- Reemplazar sentinel (-99) por NA ---
  data <- data %>%
    mutate(w_median = ifelse(w_median == sentinel, NA, w_median))
  
  # --- Armar panel completo ---
  ids <- departamentos %>%
    st_drop_geometry() %>%
    transmute(codigo_departamento_indec = as.integer(codigo_departamento_indec),
              id_provincia_indec = as.integer(codpcia)) %>%
    distinct()
  
  # Si hay clae2, cruzamos también por eso
  if ("clae2" %in% names(data)) {
    clae_levels <- unique(data$clae2)
    panel <- tidyr::crossing(ids, meses = unique(data$mes), clae2 = clae_levels)
  } else {
    panel <- tidyr::crossing(ids, meses = unique(data$mes))
  }
  
  # --- Detectar pares faltantes ---
  panel <- panel %>%
    left_join(data %>% select(all_of(group_cols), w_median),
              by = group_cols)
  
  faltantes_pairs <- panel %>%
    filter(is.na(w_median)) %>%
    select(all_of(group_cols))
  
  # --- Calcular vecinos (st_touches o st_relate según strict_border) ---
  nb_list <- if (strict_border) st_touches(departamentos) else st_relate(departamentos, pattern = "F***T****")
  
  # Mapeo ID -> vecinos
  map_tbl <- tibble(
    codigo_departamento_indec = departamentos$codigo_departamento_indec,
    vecinos = nb_list
  ) %>%
    unnest_longer(vecinos, values_to = "id_vecino") %>%
    mutate(id_vecino = departamentos$codigo_departamento_indec[id_vecino])
  
  # --- Traer valores de vecinos ---
  vals_vecinos <- map_tbl %>%
    left_join(data %>% select(all_of(group_cols), w_median),
              by = c("id_vecino" = "codigo_departamento_indec",
                     setdiff(group_cols, c("codigo_departamento_indec"))))
  
  # --- Promedio de vecinos ---
  prom_vecinos <- vals_vecinos %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(w_med_vecinos = agg_fun(w_median, na.rm = TRUE), .groups = "drop")
  
  # --- Crear nuevas filas imputadas ---
  dept_meta <- ids %>% select(codigo_departamento_indec, id_provincia_indec)
  
  nuevas_filas <- faltantes_pairs %>%
    left_join(prom_vecinos, by = group_cols) %>%
    filter(!is.na(w_med_vecinos)) %>%
    left_join(dept_meta, by = "codigo_departamento_indec") %>%
    mutate(
      fecha = mes,
      w_median = w_med_vecinos,
      imputado_vecinos = TRUE
    ) %>%
    select(all_of(names(data)), imputado_vecinos)
  
  # --- Agregar y ordenar ---
  if (!"imputado_vecinos" %in% names(data)) data$imputado_vecinos <- FALSE
  
  total_final <- bind_rows(data, nuevas_filas) %>%
    arrange(across(all_of(group_cols)))
  
  return(total_final)
}



# --- wrapper con group_cols (sin duplicar columnas) ---
imputar_por_vecinos <- function(data,
                                departamentos,
                                group_cols = NULL,
                                sentinel = -99,
                                agg_fun = median,
                                strict_border = FALSE) {
  
  if (is.null(group_cols) || length(group_cols) == 0) {
    return(.imputar_por_vecinos_base(data, departamentos, sentinel, agg_fun, strict_border))
  }
  
  # validar que existan
  faltan <- setdiff(group_cols, names(data))
  if (length(faltan) > 0) {
    stop("Las columnas de 'group_cols' no existen en 'data': ", paste(faltan, collapse = ", "))
  }
  
  out <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::group_split(.keep = TRUE) %>%
    purrr::map_dfr(function(df){
      # claves del grupo (una fila)
      keys <- df %>% dplyr::select(dplyr::all_of(group_cols)) %>% dplyr::slice(1)
      
      # correr la imputación base en esta partición
      res <- .imputar_por_vecinos_base(df, departamentos, sentinel, agg_fun, strict_border)
      
      # si faltara alguna col de grupo en res, la agregamos; si ya está, no tocamos
      for (nm in group_cols) {
        if (!nm %in% names(res)) res[[nm]] <- keys[[nm]][1]
      }
      
      # ordenar dejando las cols de grupo al comienzo (solo las que existan)
      res %>% dplyr::relocate(dplyr::any_of(group_cols), .before = 1)
    })
  
  out
}




# =======================
# EJEMPLO con tus archivos
# =======================

# departementos (lauti.geojson)
departamentos <- sf::st_read("data/integrador1/lauti.geojson", quiet = TRUE)

# total (total.csv)
total <- read.csv("data/integrador1/total.csv", stringsAsFactors = FALSE)
total_clae2 <- read.csv("data/integrador1/total_clae2.csv", stringsAsFactors = FALSE)
priv <- read.csv("data/integrador1/priv.csv", stringsAsFactors = FALSE)


# Ejecutar imputación:
total_imputado <- imputar_por_vecinos(
  data = total,
  departamentos = departamentos,
  group_cols = NULL,
  sentinel = -99,
  agg_fun = median,
  strict_border = FALSE
)


total_clae2imputado <- imputar_por_vecinos(
  data = total_clae2,
  departamentos = departamentos,
  group_cols = "clae2",   
  sentinel = -99,
  agg_fun = median,
  strict_border = FALSE
)

priv_imputado <- imputar_por_vecinos(
  data = priv,
  departamentos = departamentos,
  group_cols = NULL,   
  sentinel = -99,
  agg_fun = median,
  strict_border = FALSE
)

### HACER ESTO CON TODOS LOS CSV ###

write.csv(total_imputado, "data/integrador1/total_imputado.csv", row.names = FALSE)
write.csv(total_clae2imputado, "data/integrador1/total_clae2imputado.csv", row.names = FALSE)
write.csv(priv_imputado, "data/integrador1/priv_imputado.csv", row.names = FALSE)
