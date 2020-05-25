
library(shiny)
library(DT)

shinyServer(function(input, output, session) {

  output$fechaDeCorte <- renderUI({
    HTML("<h4 style = 'color:gray; text-align:center;'>Corte realizado hasta el ", 
    corte, 
    "</h4>")
  })
  
  
  # Poligono de clickeo
  # Captamos donde da el click el usuario
  pol_of_click <- reactiveValues(clickedShape = NULL)
  observeEvent(input$mapa_shape_click, 
               {
                 pol_of_click <- input$pol_of_click
                 pol_of_click$clickedShape <- input$mapa_shape_click$id
               })
  
  output$mapa <- renderLeaflet({
    
    map$ENTIDAD <- str_to_title(map$ENTIDAD)
    
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
  info(edo = str_to_upper(i)) %>% htmltools::HTML() 
})  

output$nombre <- renderUI({
  i <- toupper(input$mapa_shape_click$id)
  
  if(is.null(input$mapa_shape_click$id)) i <- "MORELOS"
  
  if(i == "CDMX"){
    # Armamos el popup ---
    fluidPage(
      fluidRow(
        column(12, paste0("<p style = 'padding-left: 15px; text-align:center;'> ", 
                          "Ciudad de México", 
                          "<br><b style = 'font-size: 20px; padding-left: 0px;'>Medidas Económicas</b></p>") %>% htmltools::HTML())
        # , 
        # column(12, "<h3>Medidas económicas</h3>" %>% htmltools::HTML())
      )
    )
  } else {
    fluidPage(
      fluidRow(
        column(12, paste0("<p style = 'padding-left: 15px; text-align:center;'> Estado de ", 
                          str_to_title(i), 
                          "<br><b style = 'font-size: 20px; padding-left: 0px;'> Medidas Económicas</b></p>") %>% htmltools::HTML())
        
      )
    )
  }
  
})


output$coat <- renderImage({
  i <- toupper(input$mapa_shape_click$id)
  if(is.null(input$mapa_shape_click$id)) i <- "MORELOS"
  if(i == "CDMX") i <- "CIUDAD DE MÉXICO"
  filename <- paste0("www/multimedia/", i,'.png')
  list(src = filename,
       contentType = "image/png", 
       height = "70px",
       alt = "coat", 
       #class = "img-responsive"
       style = "display: block; margin-left: auto; margin-right: auto;"
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

output$tabEnlacesCONAMER <- function() {
  enlaces %>%
    rename("Clave Estado" = "Id", 
           "Enlace CONAMER" = "Liga") %>% 
    select(`Clave Estado`, Estado,`Enlace CONAMER`,Micrositio) %>% 
    mutate(Micrositio = cell_spec(Micrositio, "html", link = Micrositio)) %>% 
    mutate(Micrositio = str_replace_all(Micrositio, pattern = "<a href=\"No hay un micrositio dedicado\" style=\"     \" >No hay un micrositio dedicado</a>", 
                                        replacement = "No hay un micrositio dedicado")) %>% 
    mutate(`Enlace CONAMER` = cell_spec(`Enlace CONAMER`, "html", link = `Enlace CONAMER`)) %>% 
    knitr::kable("html", escape = FALSE) %>%
    kable_styling("striped",full_width = F) %>% 
    column_spec(column = 1, bold = TRUE)
}

output$tabVars <- function() {
  vartab %>%
    knitr::kable("html") %>%
    kable_styling("striped", full_width = F) %>% 
    column_spec(column = 1, bold = TRUE)
}


# output$mobile <- renderUI({
#   
#   # if (session$clientData$output_imgFichaMpios_width < 1100) {
#   #   width <- session$clientData$output_imgFichaMpios_width
#   # } else {
#   #   width <- 1100
#   # }
#   
#   print(session$clientData)
#   if(session$clientData$output_tabEnlacesCONAMER_hidden < 390){
#     
#     ui <- fluidRow( # Inicio del UI de escritorio
#               column(7, box(width = 12, 
#                             solidHeader = TRUE,
#                             title = "Mapa de los estados de México",
#                             status = "warning", 
#                             fluidPage(
#                               fluidRow(
#                                 column(11, offset = 1, htmltools::HTML("<b style = 'color:#545454; text-align: center; padding-top:0%;'>&nbspSeleccione un estado para visualizar una lista de las políticas públicas.</b>")
#                                 )
#                               )
#                             ),
#                             withSpinner(leaflet::leafletOutput("mapa", height = "600px"))
#               )
#               ), 
#               column(5, 
#                      wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 685px; background:white; ",
#                                
#                                fluidPage(
#                                  fluidRow(
#                                    column(1, offset = 5, imageOutput("coat", height = "50px"))
#                                  ), 
#                                  br(), br(),
#                                  fluidRow(
#                                    column(12,uiOutput("nombre", height = "50px"))
#                                  ),
#                                  fluidRow(
#                                    column(12, uiOutput("contenido"))
#                                  )
#                                )   
#                      )  
#               )
#             ) # Fin del UI de escritorio.
#     
#     
#   } else {
#     ui <- fluidRow(
#       column(12, 
#              wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 685px; background:white; ",
#                        
#                        fluidPage(
#                          fluidRow(
#                            column(1, offset = 5, imageOutput("coat", height = "50px"))
#                          ), 
#                          br(), br(),
#                          fluidRow(
#                            column(12,uiOutput("nombre", height = "50px"))
#                          ),
#                          fluidRow(
#                            column(12, uiOutput("contenido"))
#                          )
#                        )   
#              )  
#       )
#     )
#     
#   }
# })


})
