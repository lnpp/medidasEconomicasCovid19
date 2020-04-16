library(shiny)
library(shinydashboard)
library(stringr)
library(shinycssloaders)

dbHeader <- dashboardHeader(title = "Medidas económicas ante COVID-19", titleWidth = 360,
                           tags$li(a(href = 'https://www.cide.edu',
                                     img(src = 'https://www.cide.edu/wp-content/themes/cide_general/img/logo_cide.png',
                                         title = "CIDE", height = "30px"),
                                     style = "padding-top:10px; padding-bottom:10px;"),
                                   class = "dropdown"),

                           tags$li(a(href = 'http://lnpp.cide.edu',
                                     img(src = 'http://lnpp.cide.edu/wp-content/themes/lnpp/images/logo.svg',
                                         title = "LNPP", height = "30px"),
                                     style = "padding-top:10px; padding-bottom:10px;"),
                                   class = "dropdown"))

sidebar <- dashboardSidebar(width = 160, 
                            sidebarMenu(
                              menuItem(htmltools::HTML("<b>Tablero</b>"), tabName = "PLAT", icon = icon("table")),
                              menuItem(htmltools::HTML("<b>Datos</b>"), tabName = "DATOS", icon = icon("download"))
                              # , 
                              # menuItem(htmltools::HTML("<b>Documentación</b>"), tabName = "DOCUM", icon = icon("book"))
                            ))

body <- dashboardBody(  
               tags$head(
                 includeCSS("styles.css")),
tabItems(    
  tabItem("PLAT",                
    fluidPage(
      fluidRow(
        column(12, 
               HTML("<h2>Mapa de las medidas económicas ante la pandemia COVID-19</h2>
                     <h4 style = 'color:gray; text-align:center;'>Información recopilada de notas periodísticas, reportes de prensa y canales oficiales</h4>
                     <h4 style = 'color:gray; text-align:center;'>Corte realizado hasta el 15 de abril del 2020</h4>
                     <p>En el presente tablero de información, elaborado por el <a href = 'https://www.lnpp.mx'>Laboratorio Nacional de Políticas Públicas del CIDE</a>, se muestran los planes económicos que los diferentes gobiernos estatales están planeando ejecutar en los próximos días para afrontar y recuperarse del shock económico que representa la pandemía actual del COVID-19.</p> ")
               )
      ),
      
      fluidRow(
        column(7, box(width = 12, 
                      solidHeader = TRUE,
                      title = "Mapa de los estados de México",
                      # footer = htmltools::HTML("<b style = 'color:#545454;'>Seleccione un estado para visualizar una lista de las políticas públicas.</b>"),
                      status = "warning", 
                      fluidPage(
                        fluidRow(
                          column(11, offset = 1, htmltools::HTML("<b style = 'color:#545454; text-align: center; padding-top:0%;'>&nbspSeleccione un estado para visualizar una lista de las políticas públicas.</b>")
                          )
                        )
                      ),
                      withSpinner(leaflet::leafletOutput("mapa", height = "600px"))
                      )
               ), 
        column(5, 
          wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 685px; background:white; ",
                         
            fluidPage(
              fluidRow(
                column(1, offset = 5, imageOutput("coat", height = "50px"))
            ), 
            br(), br(),
              fluidRow(
              column(12,uiOutput("nombre", height = "50px"))
            ),
              fluidRow(
                column(12, uiOutput("contenido"))
              )
            )   
          )  
        )
      ) 
    )
  ), # FIN DE PLAT

  tabItem("DATOS",                
          h2("Sección de descarga de datos"), 
          br(),
          fluidPage(
            fluidRow(
              column(1, offset = 5, downloadButton("download_data", label = "Descargar"))
            )
          ),
          br(),
          DT::dataTableOutput("tabla")
  ) # FIN DE DATOS 
  
  # ,
  # tabItem("DOCUM", 
  #         includeMarkdown("/Users/admin/Downloads/Medidas\ economicas\ ante\ Covid\ 19.md")
  #         )
  
)  
  
)

ui <- dashboardPage(skin = "black", dbHeader, sidebar, body)
