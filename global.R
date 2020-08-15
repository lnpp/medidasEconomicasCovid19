# Librerias

Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)
library(dplyr)
library(stringr)
library(sf)
library(leaflet)
library(readxl)
library(kableExtra)
library(shinyWidgets)
library(janitor)
library(lubridate)
library(viridis)

bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx") %>% 
  rename("Clasificación" = `Población Objetivo`) %>% 
  mutate(Clasificación = str_remove(Clasificación, 
                                    pattern = "\n"),
         Entidad = str_replace_all(Entidad, pattern = "De", replacement = "de"), 
         Entidad = str_replace_all(Entidad, pattern = "La", replacement = "la"))


# bd %>%
#   write.csv("www/BasesDeDatos/medidas.csv",
#             na = "", fileEncoding = "UTF-8",
#             row.names = FALSE)

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
    pull(medidas) %>% 
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



banner <- fluidRow(
  column(12,
         wellPanel(id = "barraInferior",
                   br(),
                   fluidRow(
                     column(5, offset = 1,
                            # fluidRow(
                            #   column(12,
                                     HTML('<div class="row">
                            <div class="column">
                              <img src="https://www.lnpp.mx/wp-content/themes/lnpp/images/logo_footer.svg" alt="Snow" style="width:calc(200px + 8vw); padding-left:10px;">
                            </div>
                          </div>'),
                                     HTML("<p class = 'downbartext'>2020. Laboratorio Nacional de Políticas Públicas</p>")
                            #   )
                            # )
                     ),
                     column(6,
                           HTML("<p class = 'downbartext'><b>CONTACTO</b></p>"),
                           HTML("<p class = 'downbartext'>Carretera México – Toluca 3655
                           Lomas de Santa Fe<br>
                           01210. México, D.F.<br>
                           57279800 ext. 2443<br>
                           lnpp@cide.edu<br>
                           <b>Información sobre los datos:</b><br>
                           juvenal.campos@cide.edu</p>")
                     )
                   )
         )
  )
)


### Funcion para graficar ----

graficas_medidas <- function(tipo_de_grafica,estado){
  
  if (tipo_de_grafica == "Categorías de Apoyo") {
    
    medidas_graf %>% 
      group_by(entidad, categoria_de_apoyo) %>% 
      count() %>%
      ungroup() %>% 
      filter(entidad == estado) %>% 
      ggplot(aes(x = fct_reorder(categoria_de_apoyo, n), y = n, fill = n))+
      geom_col(col = "black",
               width = .8)+
      geom_text(aes(label= n), 
                size=6, hjust = 1.7, 
                col = "white", 
                fontface = "bold")+
      coord_flip()+
      scale_y_continuous(breaks = seq(0, 12, 1),
                         expand = c(0, 0))+
      scale_fill_viridis(direction = -1)+
      labs(y = "Número de medidas",
           x = "",
           title = "Número de medidas por categoría de apoyo",
           subtitle = estado)+
      tema_medidas()+
      theme(text = element_text(size = 16), 
            legend.position = "",
            axis.line = element_line(linetype = 1),
            plot.title = element_text(size = 15, color = "gray50", face = "bold", hjust = .5),
            plot.subtitle = element_text(size = 14, color = "gray", face = "bold", hjust = .5))
  } else if (tipo_de_grafica == "Fecha de medidas") {
    
    medidas_graf %>% 
      filter(entidad == estado) %>% 
      ggplot(aes(fecha, entidad, col = tipo_de_programa))+
      geom_jitter(size = 5, 
                  alpha = 0.5,
                  width = .5)+
      scale_x_date(date_labels = "%d-%b")+
      labs(x = "Fecha",
           col = "Tipo de Programa",
           title = "Línea cronológica de las medidas anunciadas",
           subtitle = estado)+
      tema_medidas()+
      theme(legend.text.align = 0,
            legend.text = element_text(size = 12),
            axis.line = element_line(linetype = 1),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_line(linetype = 2, colour = "gray", size = .2),
            legend.position = "bottom",
            plot.title = element_text(size = 15, color = "gray50", face = "bold", hjust = .5),
            plot.subtitle = element_text(size = 14, color = "gray", face = "bold", hjust = .5)) + 
      guides(color=guide_legend(ncol=1,title.position="top", title.hjust = 0.5))
    
  } else if (tipo_de_grafica == "Población Objetivo") {
    
    medidas_graf %>% 
      group_by(entidad, poblacion_objetivo) %>% 
      count() %>% 
      ungroup() %>% 
      filter(entidad == estado) %>% 
      ggplot(aes(fct_reorder(poblacion_objetivo, n), n, fill = poblacion_objetivo))+
      geom_col(col = "black")+
      geom_text(aes(label= n), size=6, hjust = 1.7, col = "white", 
                fontface = "bold")+
      labs(y = "Número de medidas",
           x = "",
           title = "Número de medidas por población objetivo",
           subtitle = estado)+
      scale_y_continuous(breaks = seq(0, 10, 2),
                         expand = c(0,0))+
      coord_flip()+
      tema_medidas()+
      theme(legend.position = "",
            axis.line = element_line(linetype = 1),
            plot.title = element_text(size = 15, color = "gray50", face = "bold", hjust = .5),
            plot.subtitle = element_text(size = 14, color = "gray", face = "bold", hjust = .5))
  } else if (tipo_de_grafica == "Tipo de Programa"){
    
    medidas_graf %>% 
      group_by(entidad, tipo_de_programa) %>% 
      count(tipo_de_programa) %>% 
      ungroup() %>% 
      filter(entidad == estado)%>% 
      ggplot(aes(tipo_de_programa, n, fill = tipo_de_programa))+
      geom_col(col = "black")+
      coord_flip()+
      labs(x = "",
           y = "Número de medidas",
           title = "Número de medidas por programa",
           subtitle = estado)+
      tema_medidas()+
      geom_text(aes(label= n), size=6, hjust = 1.7, col = "white", 
                fontface = "bold")+
      scale_y_continuous(breaks = seq(0, 16, 1),
                         expand = c(0, 0))+
      theme(legend.position = "none",
            axis.line = element_line(linetype = 1),
            plot.title = element_text(size = 15, color = "gray50", face = "bold", hjust = .5),
            plot.subtitle = element_text(size = 14, color = "gray", face = "bold", hjust = .5))
  } else if (tipo_de_grafica == "Transparencia"){
    
    medidas_transparencia <- medidas_graf %>% 
      mutate(monto_presupuestado = as.numeric(monto_presupuestado)) %>% 
      select(entidad, monto_presupuestado) %>% 
      group_by(entidad) %>% 
      summarise(si_tiene = sum(monto_presupuestado >= 0, na.rm = TRUE),
                no_tiene = sum(is.na(monto_presupuestado))) %>% 
      mutate(total = si_tiene + no_tiene,
             si = si_tiene/total,
             no = no_tiene/total)
    
    medidas_transparencia %>% 
      pivot_longer(cols = si:no ,
                   names_to = "si_o_no",
                   values_to = "porcentaje") %>% 
      select(-si_tiene, -no_tiene, -total) %>% 
      filter(entidad == estado) %>% 
      ggplot(aes(si_o_no, porcentaje, fill = si_o_no ))+
      geom_col()+
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,1.08),
        labels = scales::percent)+
      labs(x= "¿Cuenta con presupuesto anunciado?",
           y = "",
           title = "Porcentaje de medidas anunciadas con presupuesto",
           subtitle = estado)+
      geom_text(aes(label = scales::percent(porcentaje)),
                position = position_dodge(width = .9),    
                vjust = -0.5,    
                size = 6, 
                fontface = "bold")+
      tema_medidas()+
      theme(legend.position = "none",
            axis.line = element_line(linetype = 1),
            plot.title = element_text(size = 15, color = "gray50", face = "bold", hjust = .5),
            plot.subtitle = element_text(size = 14, color = "gray", face = "bold", hjust = .5))
  }
  

  
  
}

tipos_graf <- c("Tipo de Programa", "Categorías de Apoyo", "Población Objetivo", "Fecha de medidas", "Transparencia")

medidas_graf <- read_excel("www/BasesDeDatos/medidas.xlsx",
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "numeric", "text", 
                                         "text", "date")) %>% 
  clean_names() %>% 
  mutate(fecha = ymd(fecha))

#### Tema ----

tema_medidas <- function(){
  
  theme(
    ## Eliminar el borde del panel 
    panel.border = element_blank(),
    ## Color del fondo 
    panel.background = element_blank(), 
    text = element_text(size = 16)
  )
}

