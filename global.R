# Librerias
library(dplyr)
library(sf)
library(leaflet)
library(readxl)
library(kableExtra)

# Mapas ----
map <- readRDS("www/BasesDeDatos/mapa.RDS")
  #st_read("www/BasesDeDatos/mapa.geojson")

# Tablas de la documentacion 
cat <- read_xlsx("www/Documentos/tablasDocumentacion.xlsx", sheet = 1)
vartab <- read_xlsx("www/Documentos/tablasDocumentacion.xlsx", sheet = 2)

# Escudos ---- 
coats <- read.csv("www/BasesDeDatos/coats.csv")

# Funcion de info ----
# Funcion de info ----
info <- function(edo = "MORELOS"){
  
  # Base de datos ----
  bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx")  %>% 
    mutate(Entidad = toupper(Entidad)) %>% 
    filter(Entidad == edo) %>% 
    arrange(`Clasificación`)
  
  medidas <- bd %>% 
    group_by(Clasificación) %>% 
    summarise(medidas = paste0("<b>", first(Clasificación), "</b>",
                               "<ul>",  
                               paste0("<li>", Medida, "</li>", collapse = ""),
                               "</ul>")) %>% 
    pull(medidas)  %>% 
    paste(collapse = "<br>")

  # Fuente ----
  fuente <- unique(paste("<br><h4>Fuente(s):</h4><ul>", 
                         paste0(paste0("<li><a href = ", 
                                       unique(bd$Link), ">", 
                                       unique(bd$Fuente), "</a>"), 
                                collapse = "</li>"), "</ul>"))
  # Texto ----
  texto <- paste(medidas,fuente)
  return(texto)
}

