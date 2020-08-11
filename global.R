# Librerias
options(scipen = 999)
library(dplyr)
library(stringr)
library(sf)
library(leaflet)
library(readxl)
library(kableExtra)
library(shinyWidgets)

bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx") %>% 
  rename("Clasificación" = `Población Objetivo`) %>% 
  mutate(Clasificación = str_remove(Clasificación, 
                                    pattern = "\n"))

corte <- read.csv("www/BasesDeDatos/fechaDeCorte.csv")[1,1] %>% 
  as.character()

# Mapas ----
map <- readRDS("www/BasesDeDatos/mapa2.RDS")
  #st_read("www/BasesDeDatos/mapa.geojson")

# Tablas de la documentacion 
cat <- read_xlsx("www/Documentos/tablasDocumentacion.xlsx", sheet = 1)
vartab <- read_xlsx("www/Documentos/tablasDocumentacion.xlsx", sheet = 2)

temporalidad <- read_xlsx("www/BasesDeDatos/cuadros1.xlsx", sheet = 1)
tipoDePrograma <- read_xlsx("www/BasesDeDatos/cuadros1.xlsx", sheet = 2)
CategoriaDeApoyo <- read_xlsx("www/BasesDeDatos/cuadros1.xlsx", sheet = 3)
poblacionObjetivo <- read_xlsx("www/BasesDeDatos/cuadros1.xlsx", sheet = 4)
names(CategoriaDeApoyo)[3] <- "Descripción"


# Escudos ---- 
# coats <- read.csv("www/BasesDeDatos/coats.csv")

# Enlaces Conamer y Micrositios
enlaces <- read_xlsx("www/BasesDeDatos/LigasEstadosCovid.xlsx")

# Funcion de info ----
info <- function(edo = "JALISCO"){
  
  # Base de datos ----
  bd <- bd  %>% 
    mutate(Entidad = toupper(Entidad)) %>% 
    filter(Entidad == edo) %>% 
    arrange(`Clasificación`)
  
  medidas <- bd %>% 
    group_by(Clasificación) %>% 
    summarise(medidas = paste0("<p style = 'text-align:center;   
    background-color: #cfc;
    padding: 5px ;
    border: 1px solid #cfc;'><b style = 'font-family:Poppins-bold;'>", first(Clasificación), "</b></p>",
                               "<ul>",  
                               paste0("<li>",
                                      "<a style = 'text-align:justify;' target='_blank' rel='noopener noreferrer' href = '", 
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
                                "<br>",
                                "<li><a target='_blank' rel='noopener noreferrer' href = ", enlaces1$Liga, '>', paste0("Enlace ", as.character(edo)), "</a></li>", 
                                "</ul>", 
                                "<br><h4><b>Enlace micrositio estatal sobre COVID-19:</b></h4><ul>",
                                "<li><a target='_blank' rel='noopener noreferrer' href = ", enlaces1$Micrositio, ">", str_trunc(str_remove(enlaces1$Micrositio, pattern = "(?:http://|https://)"), 35), "</a></li>", 
                                "</ul>") %>% 
    str_replace_all(pattern = "<a href = No hay un micrositio dedicado>No hay un micrositio dedicado</a>", 
                replacement = "No hay un micrositio dedicado")
  
  # Texto ----
  texto <- paste(medidas,enlacesYmicrositios,fuente)
  return(texto)
}


# Mi picker personalizado
myPickerInput <- function(inputId, label = NULL, choices, title = "", conteo = "Opciones seleccionadas") {
  pickerInput(inputId = inputId, 
              label = label, 
              choices = choices,
              multiple = TRUE, 
              #width = "75%",
              options = list(
                title = title, 
                `multiple` = TRUE, 
                `actions-box` = TRUE, 
                size = 10, 
                `selected-text-format` = "count > 0", 
                `select-all-text` = "Seleccionar todo", 
                `deselect-all-text` = "Deseleccionar todo", 
                `count-selected-text` = paste0("{0} ", conteo) 
              ), 
              choicesOpt = list(
                content = choices %>% str_wrap(width = 50) %>% str_replace_all("\\n", "<br>")
              ) 
  )
}

# Mi updater personalizado
myUpdatePickerInput <- function(session, inputId, label = NULL, choices){
  updatePickerInput(session, inputId, label, selected = NULL, choices, choicesOpt = list(content = choices %>% 
                                                                                           str_wrap(width = 50) %>%
                                                                                           str_replace_all("\\n", "<br>")))
}

# Controles para el mapa
seleccion <- c("Tipo de Programa", 
               "Categoría de Apoyo", 
               "Población Objetivo")


norte <- function(mapa_leaflet, 
                  ancho = 40, 
                  position = 'topleft', 
                  direccion_img_norte = "http://ian.umces.edu/imagelibrary/albums/userpics/10002/normal_ian-symbol-north-arrow-2.png"){
  # 1. Descargamos la imagen
  
  north.arrow.icon <- paste0("<img src='", 
                             direccion_img_norte,
                             "' style='width:",
                             as.character(ancho), "px;'>")
  # Lo incluimos en una funcion de RLeaflet
  if (!require("leaflet")) install.packages("leaflet") # Asegurarnos que este instalado Leaflet
  addControl(mapa_leaflet, 
             html = north.arrow.icon, position = position, 
             className = "fieldset {
             border: 0;}") 
}

