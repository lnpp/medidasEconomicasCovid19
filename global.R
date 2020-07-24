# Librerias
library(dplyr)
library(sf)
library(leaflet)
library(readxl)
library(kableExtra)

bdEcon <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx") %>% 
  rename("Clasificación" = `Población Objetivo`)
# write.csv(bdEcon,
#           "www/BasesDeDatos/medidas.csv",
#           fileEncoding = "UTF-8",
#           row.names = F,
#           na = ""
#           )

# Fecha de actualizacion 
# corte <- "24 de julio del 2020"
# write.csv(corte, "www/BasesDeDatos/fechaDeCorte.csv", fileEncoding = "UTF-8", na = "", row.names = F)
corte <- read.csv("www/BasesDeDatos/fechaDeCorte.csv")[1,1] %>% 
  as.character()

# Mapas ----
map <- readRDS("www/BasesDeDatos/mapa2.RDS")
  #st_read("www/BasesDeDatos/mapa.geojson")

# Tablas de la documentacion 
cat <- read_xlsx("www/Documentos/tablasDocumentacion.xlsx", sheet = 1)
vartab <- read_xlsx("www/Documentos/tablasDocumentacion.xlsx", sheet = 2)

# Escudos ---- 
# coats <- read.csv("www/BasesDeDatos/coats.csv")

# Enlaces Conamer y Micrositios
enlaces <- read_xlsx("www/BasesDeDatos/LigasEstadosCovid.xlsx")

# Funcion de info ----
info <- function(edo = "JALISCO"){
  
  # Base de datos ----
  bd <- bdEcon  %>% 
    mutate(Entidad = toupper(Entidad)) %>% 
    filter(Entidad == edo) %>% 
    arrange(`Clasificación`)
  
  medidas <- bd %>% 
    group_by(Clasificación) %>% 
    summarise(medidas = paste0("<b>", first(Clasificación), "</b>",
                               "<ul>",  
                               paste0("<li>",
                                      "<a target='_blank' rel='noopener noreferrer' href = '", 
                                      Link, "'>",
                                      Medida, 
                                      "</a>",
                                      "</li>", 
                                      collapse = ""),
                               "</ul>")) %>% 
    pull(medidas)  %>% 
    paste(collapse = "<br>")

  # Fuente ----
  fuente <- unique(paste("<br><h4><b>Fuente(s):</b></h4><ul>", 
                         paste0(unique(paste0("<li><a target='_blank' rel='noopener noreferrer' href = ", 
                                              bd$Link, ">", 
                                              bd$Fuente, "</a>")), 
                                       collapse = "</li>") , "</ul>"))
  
  # Enlaces ----
  enlaces1 <- enlaces %>% filter(Estado == edo)
  enlacesYmicrositios <- paste0("<br><h4><b>Enlace a respuestas regulatorias CONAMER:</b></h4><ul>", 
                                "<img src = 'https://raw.githubusercontent.com/lnpp/medidasEconomicasCovid19/master/www/multimedia/LOGO_CONAMER.png' height = 40px style = 'display: block;
                                  margin-left: auto;
                                  margin-right: auto;'>",
                                "<li><a target='_blank' rel='noopener noreferrer' href = ", enlaces1$Liga, '>', enlaces1$Liga, "</a></li>", 
                                "</ul>", 
                                "<br><h4><b>Enlace micrositio estatal sobre COVID-19:</b></h4><ul>",
                                "<li><a target='_blank' rel='noopener noreferrer' href = ", enlaces1$Micrositio, ">", enlaces1$Micrositio, "</a></li>", 
                                "</ul>") %>% 
    str_replace_all(pattern = "<a href = No hay un micrositio dedicado>No hay un micrositio dedicado</a>", 
                replacement = "No hay un micrositio dedicado")
  
  # Texto ----
  texto <- paste(medidas,enlacesYmicrositios,fuente)
  return(texto)
}



