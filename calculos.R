# Librerias
library(tidyverse)
library(sf)
library(leaflet)
library(readxl)

# Mapas ----
map <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson")
# Escribimos un mapa como archivo local
saveRDS(map, "www/BasesDeDatos/mapa.RDS")
# st_write(map, "www/BasesDeDatos/mapa.geojson")

# Escudos ---- 
coats <- c("https://upload.wikimedia.org/wikipedia/commons/thumb/d/d5/Coat_of_arms_of_Aguascalientes.svg/50px-Coat_of_arms_of_Aguascalientes.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Coat_of_arms_of_Baja_California.svg/50px-Coat_of_arms_of_Baja_California.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Coat_of_arms_of_Baja_California_Sur.svg/50px-Coat_of_arms_of_Baja_California_Sur.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a8/Coat_of_arms_of_Campeche.svg/50px-Coat_of_arms_of_Campeche.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e9/Coat_of_arms_of_Chiapas.svg/1024px-Coat_of_arms_of_Chiapas.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/Coat_of_arms_of_Chihuahua.svg/50px-Coat_of_arms_of_Chihuahua.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Coat_of_arms_of_Mexican_Federal_District.svg/50px-Coat_of_arms_of_Mexican_Federal_District.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c0/Coat_of_arms_of_Coahuila.svg/50px-Coat_of_arms_of_Coahuila.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Coat_of_arms_of_Colima.svg/50px-Coat_of_arms_of_Colima.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Coat_of_arms_of_Durango.svg/50px-Coat_of_arms_of_Durango.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d7/Escudo_de_armas_de_la_Ciudad_y_Estado_de_Guanajuato.svg/50px-Escudo_de_armas_de_la_Ciudad_y_Estado_de_Guanajuato.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1f/Coat_of_arms_of_Guerrero.svg/50px-Coat_of_arms_of_Guerrero.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Escudo_del_Estado_Libre_y_Soberano_de_Hidalgo.png/60px-Escudo_del_Estado_Libre_y_Soberano_de_Hidalgo.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Coat_of_arms_of_Jalisco.svg/50px-Coat_of_arms_of_Jalisco.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5b/Coat_of_arms_of_Mexico_State.svg/50px-Coat_of_arms_of_Mexico_State.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/Escudo_de_Michoacán.png/50px-Escudo_de_Michoacán.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/0/01/Coat_of_arms_of_Morelos.svg/50px-Coat_of_arms_of_Morelos.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/Coat_of_arms_of_Nayarit.svg/50px-Coat_of_arms_of_Nayarit.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dc/Coat_of_arms_of_Nuevo_Leon.svg/50px-Coat_of_arms_of_Nuevo_Leon.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Coat_of_arms_of_Oaxaca.svg/50px-Coat_of_arms_of_Oaxaca.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Coat_of_arms_of_Puebla.svg/50px-Coat_of_arms_of_Puebla.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Coat_of_arms_of_Queretaro.svg/50px-Coat_of_arms_of_Queretaro.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Coat_of_arms_of_Quintana_Roo.svg/50px-Coat_of_arms_of_Quintana_Roo.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ec/Coat_of_arms_of_San_Luis_Potosi.svg/50px-Coat_of_arms_of_San_Luis_Potosi.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6d/Coat_of_arms_of_Sinaloa.svg/50px-Coat_of_arms_of_Sinaloa.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8b/Coat_of_arms_of_Sonora.svg/50px-Coat_of_arms_of_Sonora.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b0/Coat_of_arms_of_Tabasco_%28México%29.svg/50px-Coat_of_arms_of_Tabasco_%28México%29.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/9/90/Coat_of_arms_of_Tamaulipas.svg/50px-Coat_of_arms_of_Tamaulipas.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/Coat_of_arms_of_Tlaxcala.svg/50px-Coat_of_arms_of_Tlaxcala.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/8/84/Coat_of_arms_of_Veracruz.svg/50px-Coat_of_arms_of_Veracruz.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/5/54/Coat_of_arms_of_Yucatan.svg/50px-Coat_of_arms_of_Yucatan.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Coat_of_arms_of_Zacatecas.svg/50px-Coat_of_arms_of_Zacatecas.svg.png")

# Escudos
coats <- cbind.data.frame(map$ENTIDAD, coats) %>% 
  arrange(`map$ENTIDAD`) %>% 
  cbind.data.frame(list.files("www/multimedia") %>% sort())

# Descarga de los escudos ----
# for (i in 7){
# print(i)
# curl::curl_download(url = coats[i,2] %>% as.character(), 
#                     destfile = paste0("www/multimedia/", coats[i,1] %>% as.character(), ".png")
#                     )
# }

# Escribimos la base de los escudos ----
write.csv(coats, 
          "www/BasesDeDatos/coats.csv", 
          row.names = F, 
          na = "", fileEncoding = "UTF-8")


bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx") %>% 
  filter(Evento == "COVID-19")

# Grafica de políticas por frecuencia
bd %>% 
  group_by(Clasificación) %>% 
  count() %>% 
  arrange(-n)


tibble(d = unique(bd$Clasificación) %>% sort()) %>% 
  write.csv("Categos.csv")


openxlsx::write.xlsx(bd, 
          "www/BasesDeDatos/medidas.xlsx")

unique(bd$Evento)

# Analisis de las medidas
bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx")

unique(bd$Clasificación) %>%
  as_tibble() %>% 
  write.csv("www/BasesDeDatos/Categorias.csv", fileEncoding = "UTF-8", row.names = F)

# Funcion de info ----
info <- function(edo = "MORELOS"){

    # Base de datos ----
    bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx")  %>% 
      mutate(Entidad = toupper(Entidad)) %>% 
      filter(Entidad == edo)
    
    # Armamos el popup ---
    estado <- paste0("<img src='", paste0("www/multimedia/", 
                                          coats[coats[,1] == estado,3]), 
                     "' width='42' height='42' align='left'>", 
                     "<p>Estado de</p><h4>", 
                     bd$Entidad[1], 
                     "</h4><br>") 
      
    medidas <- paste0("<ul>", 
                      paste0(paste0("<li>", "<b>", bd$Clasificación, "</b><br>", 
                                    bd$Medida, "</li>"), collapse = ""), 
                      "</ul>")
    # Fuente ----
    fuente <- unique(paste("<br><h5>Fuente(s):</h5>", 
                           paste0(paste0("<a href = ", 
                                         unique(bd$Link), ">", 
                                         unique(bd$Fuente), "</a>"), 
                                  collapse = "<br>")
                           ))
    # Texto ----
    texto <- paste(estado, 
                   "<h2>Medidas económicas</h2>", 
                   medidas, 
                   fuente)
    
  return(texto)

}

# Prueba del texto
info(edo = "MORELOS")


str_to_sentence(i)
"Queretaro de arteaga" %>% 
  str_to_title()
