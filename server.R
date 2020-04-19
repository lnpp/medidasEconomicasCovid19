
library(shiny)
library(DT)

shinyServer(function(input, output, session) {

  # Poligono de clickeo
  # Captamos donde da el click el usuario
  pol_of_click <- reactiveValues(clickedShape = NULL)
  observeEvent(input$mapa_shape_click, 
               {
                 pol_of_click <- input$pol_of_click
                 pol_of_click$clickedShape <- input$mapa_shape_click$id
               })
  
output$mapa <- renderLeaflet({
    leaflet(map, options = leafletOptions(zoomControl = FALSE, 
                                          maxZoom = 5, minZoom = 5)) %>%
      setMaxBounds(lng1 = -86.72, 
                   lat1 = 14.52, 
                   lng2 = -117.13, 
                   lat2 = 32.53) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
      addPolygons(color = "black",
                  weight = 1,
                  opacity = 1, 
                  popup = map$ENTIDAD,
                  label = lapply(paste0("<b style = 'color:green;'>Estado: </b><br>", map$ENTIDAD), htmltools::HTML),
                  layerId = map$ENTIDAD,
                  fillColor = "blue", 
                  fillOpacity =  0.4, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) 
  })
  
output$contenido <- renderUI({
    i <- input$mapa_shape_click$id
    if(is.null(input$mapa_shape_click$id)) i <- "MORELOS"
    print(i)
    info(edo = i) %>% htmltools::HTML() 
  })

output$nombre <- renderUI({
  i <- input$mapa_shape_click$id
  if(is.null(input$mapa_shape_click$id)) i <- "MORELOS"
  
  if(i == "CIUDAD DE MÉXICO"){
    # Armamos el popup ---
    fluidPage(
      fluidRow(
        column(12, paste0("<p style = 'padding-left: 15px; text-align:center;'> ", 
                          str_to_title(i), 
                          "<br><b style = 'font-size: 30px; padding-left: 0px;'> Medidas Económicas</b></p>") %>% htmltools::HTML())
        # , 
        # column(12, "<h3>Medidas económicas</h3>" %>% htmltools::HTML())
      )
    )
  } else {
    fluidPage(
      fluidRow(
        column(12, paste0("<p style = 'padding-left: 15px; text-align:center;'> Estado de ", 
                          str_to_title(i), 
                          "<br><b style = 'font-size: 30px; padding-left: 0px;'> Medidas Económicas</b></p>") %>% htmltools::HTML())
        # , 
        # column(12, "<h3>Medidas económicas</h3>" %>% htmltools::HTML())
      )
    )
  }
  
})


output$coat <- renderImage({
  
  i <- input$mapa_shape_click$id
  if(is.null(input$mapa_shape_click$id)) i <- "MORELOS"
  
  if(i == "CIUDAD DE MÉXICO") i <- "CIUDAD DE MEXICO"
  

  filename <- paste0("www/multimedia/", i,'.png')
  
  list(src = filename,
       contentType = "image/png", 
       height = "70px",
       alt = "coat", 
       class = "center"
       )
}, deleteFile = FALSE)
  

baseDeDatosGeneral <- reactive({
  bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx") %>% 
    select(-Fecha, -Evento) %>% 
    arrange(Entidad)
})


output$tabla <- DT::renderDT({
  
  DT::datatable(baseDeDatosGeneral(), 
                extensions = 'FixedColumns',
                rownames= FALSE,
                filter = 'top',
                options = list(
                  pageLength = 5,
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                  #autoWidth = TRUE, 
                  #scroller = TRUE,
                  scrollX = TRUE,
                  escape = T) 
  )
  
})

# Create a download handler
output$download_data <- downloadHandler(
  
  filename = "Datos_Politica_Economica_COVID19.csv",
  content = function(file) {
    data <- baseDeDatosGeneral()
    write.csv(data, file, row.names = FALSE)
  }
)

output$tabCats <- function() {
  cat %>%
    knitr::kable("html") %>%
    kable_styling("striped", full_width = F) %>% 
    column_spec(column = 1, bold = TRUE)
  }

output$tabVars <- function() {
  vartab %>%
    knitr::kable("html") %>%
    kable_styling("striped", full_width = F) %>% 
    column_spec(column = 1, bold = TRUE)
}

})
