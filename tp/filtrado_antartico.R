# Requiere: sf
# install.packages("sf")  # si hace falta

library(sf)
library(dplyr)

# 1) Leer el GeoJSON oficial de departamentos
url <- "https://infra.datos.gob.ar/georef-dev/departamentos.geojson"
d <- read_sf(url)
d <- st_make_valid(d)

# 2) Excluir Antártida Argentina
d <- d %>%
  filter(!(tolower(nombre) %in% c("antártida argentina", "antartida argentina")))

# 3) Mantener SOLO Malvinas dentro de "Islas del Atlántico Sur"
#    Recortamos por una bbox de Malvinas (aprox): lon -62 a -57; lat -54.7 a -51
bbox_malvinas <- st_as_sfc(
  st_bbox(c(xmin = -62, ymin = -54.7, xmax = -57, ymax = -51), crs = st_crs(d))
)

islas_mask <- tolower(d$nombre) == "islas del atlántico sur"
islas <- d[islas_mask, , drop = FALSE]

# Intersección para quedarnos sólo con la parte de Malvinas (descarta Georgias/Sandwich)
islas_malvinas <- st_intersection(islas, bbox_malvinas)
islas_malvinas <- islas_malvinas[!st_is_empty(islas_malvinas), ]

# 4) Reemplazar en el dataset original
d <- d[!islas_mask, ]
d <- bind_rows(d, islas_malvinas)

# (Opcional) reparar geometrías multipart/colecciones
d <- st_collection_extract(st_make_valid(d), "POLYGON", warn = FALSE)

# 5) Guardar el archivo final
out <- "argentina_sin_antartida_y_islitas.geojson"
if (file.exists(out)) file.remove(out)
write_sf(d, out, delete_dsn = TRUE)

message("Listo: ", out)