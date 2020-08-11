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


# Mapa de medidas: 
bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx") %>% 
  rename("Clasificación" = `Población Objetivo`) %>% 
  mutate(Clasificación = str_remove(Clasificación, 
                                    pattern = "\n"), 
         Entidad = str_replace_all(Entidad, pattern = "De", replacement = "de"), 
         Entidad = str_replace_all(Entidad, pattern = "La", replacement = "la"))

# unique(bd$Entidad)[!(unique(bd$Entidad) %in% unique(map$ENTIDAD))]
# unique(map$ENTIDAD)[!(unique(map$ENTIDAD) %in% unique(bd$Entidad))]

seleccion <- c("Tipo de Programa", 
               "Categoría de Apoyo", 
               "Clasificación")

sel <- "Tipo de Programa"

if (sel == seleccion[1]){
  opts <- bd$`Tipo de Programa` %>% unique() %>% sort()

} else if (sel == seleccion[2]){
  opts <- bd$`Categoría de Apoyo` %>% unique() %>% sort()
  
} else if (sel == seleccion[3]) {
  opts <- bd$Clasificación %>% unique() %>% sort()
}

seleccionado <- opts[2]

if (sel == seleccion[1]){
  bd_fil <- bd %>% 
    filter(`Tipo de Programa` == seleccionado)
} else if (sel == seleccion[2]){
  bd_fil <- bd %>% 
    filter(`Categoría de Apoyo` == seleccionado)
} else if (sel == seleccion[3]) {
  bd_fil <- bd %>% 
    filter(Clasificación == seleccionado)
}

bd_count <- bd_fil %>% 
  group_by(Entidad) %>% 
  count() %>% 
  rename(No_medidas = n, 
         ENTIDAD = Entidad)

mapa_medidas <- merge(map, bd_count, by = "ENTIDAD")

paleta <- colorFactor("viridis",
                      domain = seq(0,max(mapa_medidas$No_medidas)))

info2 <- function(edo = "JALISCO"){
  
  if (sel == seleccion[1]){
    bd <- bd_fil  %>% 
      filter(Entidad == edo) %>% 
      arrange(`Tipo de Programa`)
    
  } else if (sel == seleccion[2]){
    bd <- bd_fil  %>% 
      filter(Entidad == edo) %>% 
      arrange(`Categoría de Apoyo`)
  } else if (sel == seleccion[3]) {
    bd <- bd_fil  %>% 
      filter(Entidad == edo) %>% 
      arrange(`Clasificación`)
  }
  
  medidas <- bd %>% 
    # group_by(Clasificación) %>% 
    summarise(medidas = paste0("<p style = 'text-align:center;   
    background-color: #cfc;
    padding: 5px ;
    border: 1px solid #cfc;'><b style = 'font-family:Poppins-bold;'>", 
                               seleccionado, ": <br>", first(Entidad),
                               "</b></p>",
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
  
  medidas <- paste0("<div class='well' id='tPanel' style='overflow-y:scroll; max-height: 400px; background:white; '>",
                    medidas,
                    "</div>")
  
  # Texto ----
  texto <- paste(medidas)
  return(texto)
}

popup <- lapply(sort(unique(bd_fil$Entidad)), info2) %>% unlist()

leaflet(mapa_medidas) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = map,
              color = "gray", 
              weight = 1,
              fill = NA, 
              dashArray = c(2,2)) %>% 
  addPolygons(color = "black",
              popup = popup,
              weight = 0.5,
              opacity = 1, 
              fillOpacity = 0.9,
              fillColor = paleta(mapa_medidas$No_medidas)) %>% 
  addLegend(title = paste0(sel, ": <br>", 
                           "<b style = 'color:green;'>", seleccionado, "</b>", "<br>",
                           "Número de medidas:"), 
            pal = paleta, 
            values = mapa_medidas$No_medidas, 
            position = "bottomleft") %>% 
  norte(position = "topright") %>% 
  addScaleBar(position = "bottomright")

