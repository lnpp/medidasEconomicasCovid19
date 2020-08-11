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
      # print(i)
      info(edo = str_to_upper(i)) %>% htmltools::HTML() 
    })  
    
    output$contenido2 <- renderUI({
      i <- toupper(input$selEstado2)
      # if(is.null(input$mapa_shape_click$id)) i <- "MORELOS"
      # print(i)
      info(edo = str_to_upper(i)) %>% htmltools::HTML() 
    })  
    
    
    output$nombre <- renderUI({
      i <- toupper(input$selEstado2)
      
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
      # toupper("Nuevo León")
      i <- toupper(input$selEstado2)
      
      # i <- "MORELOS"
      # i <- toupper(input$mapa_shape_click$id)
      # i <- str_replace_all(string = i,  c("Á" = "A", 
      #                                     "É" = "E", 
      #                                     "Í" = "I", 
      #                                     "Ó" = "O", 
      #                                     "Ú" = "U"))
      # if(is.null(input$mapa_shape_click$id)) i <- "MORELOS"
      if(i == "CDMX") i <- "CIUDAD DE MEXICO"
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
      
      entidad <- input$selEdoFiltro
      catApoyo <- input$selCategoriaApoyoFiltro
      pobObjetivo <- input$selPobObjetivoFiltro
      periodo <- input$selPeriodoFiltro
      montoPresupuestado <- input$selMontoFiltro
      
      if(is.null(entidad)) entidad <- unique(bd$Entidad)
      if(entidad == "Todas las entidades") entidad <- unique(bd$Entidad)
      
      if(catApoyo == "Todas las categorías") catApoyo <- unique(bd$`Categoría de Apoyo`)
      if(pobObjetivo == "Todas las Poblaciones Objetivo") pobObjetivo <- unique(bd$Clasificación)
      if(periodo == "Todos los periodos") periodo <- unique(bd$Periodo)
    
      bd2 <- bd %>% 
        select(-Fecha, -Evento, -`Clasificación (Vieja Categoría)`) %>% 
        arrange(Entidad)  %>% 
        filter(Entidad %in% entidad) %>%
        filter(`Categoría de Apoyo` %in% catApoyo) %>%
        filter(`Clasificación` %in% pobObjetivo) %>% 
        filter(Periodo %in% periodo) 
      
      if (montoPresupuestado == "Con monto reportado"){
        bd2 <- bd2 %>% 
          filter(!is.na(`Monto Presupuestado`))
      } else if (montoPresupuestado == "Sin monto reportado"){
        bd2 <- bd2 %>% 
          filter(is.na(`Monto Presupuestado`))
      } else {
        bd2 <- bd2
      }
      # 
      # print(bd)
      
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
                      autoWidth = TRUE,
                      columnDefs = 
                        list(list(width = '300px', targets = c(5, 8))),
                      scrollX = TRUE,
                      escape = T) 
      )
    })
    
    # Create a download handler
    output$download_data <- downloadHandler(
      
      filename = "Datos_Politica_Economica_COVID19.xlsx",
      content = function(file) {
        data <- baseDeDatosGeneral() %>% janitor::clean_names()
        openxlsx::write.xlsx(data, file)
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

    
    
    
    output$tabTemporalidad <- function() {
      temporalidad %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F) %>% 
        column_spec(column = 1, bold = TRUE)
    }
    
    output$tabTipoDePrograma <- function() {
      tipoDePrograma %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F) %>% 
        column_spec(column = 1, bold = TRUE)
    }
    
    output$tabCategoriaDeApoyo <- function() {
      CategoriaDeApoyo %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F) %>% 
        column_spec(column = 1, bold = TRUE)
    }
    
    output$tabPoblacionObjetivo <- function() {
      poblacionObjetivo %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F) %>% 
        column_spec(column = 1, bold = TRUE)
    }
    
    
    
    
    opts <- reactive({
      sel <- input$selMapa1
      
      if (sel == seleccion[1]){
        opts <- bd$`Tipo de Programa` %>% unique() %>% sort()
        
      } else if (sel == seleccion[2]){
        opts <- bd$`Categoría de Apoyo` %>% unique() %>% sort()
        
      } else if (sel == seleccion[3]) {
        opts <- bd$Clasificación %>% unique() %>% sort()
      }
      
    })
    
    output$mostrarOpts <- renderUI({
      selectizeInput(inputId = "selMapa2", 
                     label = "Seleccione categoría de la información", 
                     choices = opts(), 
                     width = "100%")
    })

  output$mapa2 <- renderLeaflet({
    
    sel <- input$selMapa1
    seleccionado <- input$selMapa2
    
    # Mapa de medidas: 
    bd <- read_xlsx(path = "www/BasesDeDatos/medidas.xlsx") %>% 
      rename("Clasificación" = `Población Objetivo`) %>% 
      mutate(Clasificación = str_remove(Clasificación, 
                                        pattern = "\n"), 
             Entidad = str_replace_all(Entidad, pattern = "De", replacement = "de"), 
             Entidad = str_replace_all(Entidad, pattern = "La", replacement = "la"))
    
    if (sel == seleccion[1]){
      opts <- bd$`Tipo de Programa` %>% unique() %>% sort()
      
    } else if (sel == seleccion[2]){
      opts <- bd$`Categoría de Apoyo` %>% unique() %>% sort()
      
    } else if (sel == seleccion[3]) {
      opts <- bd$Clasificación %>% unique() %>% sort()
    }
    
    
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
                          domain = seq(0,max(mapa_medidas$No_medidas, na.rm = TRUE)))
    
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
      
      medidas <- paste0("<div class='well' id='tPanel' style='overflow-y:scroll; max-height: 400px; width: 400px; background:white; '>",
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
    
  })    
    
})


