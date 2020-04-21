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
                                   class = "dropdown"), 
                           
                           tags$li(a(href = 'https://github.com/lnpp/medidasEconomicasCovid19',
                                     img(src = 'https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png',
                                         title = "LNPP", height = "30px"),
                                     style = "padding-top:10px; padding-bottom:10px;"),
                                   class = "dropdown")
                           
                           
                           )

sidebar <- dashboardSidebar(width = 160, 
                            sidebarMenu(
                              menuItem(htmltools::HTML("<b>Tablero</b>"), tabName = "PLAT", icon = icon("table")),
                              menuItem(htmltools::HTML("<b>Documentación</b>"), tabName = "DOCUM", icon = icon("book")), 
                              menuItem(htmltools::HTML("<b>Datos</b>"), tabName = "DATOS", icon = icon("download"))
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
                     <h4 style = 'color:gray; text-align:center;'>Corte realizado hasta el 20 de abril del 2020</h4>
                     <p>En el presente tablero de información, elaborado por el <a href = 'https://www.lnpp.mx'>Laboratorio Nacional de Políticas Públicas del CIDE</a>, se muestran los planes económicos que los diferentes gobiernos estatales están planeando ejecutar en los próximos días para afrontar y recuperarse del shock económico que representa la pandemía actual del COVID-19.</p>")
               )
      ),
      
      fluidRow(
        column(7, box(width = 12, 
                      solidHeader = TRUE,
                      title = "Mapa de los estados de México",
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
  
  ,
  tabItem("DOCUM",
          h2("Medidas económicas ante Covid-19"),
          HTML("<h2 style = 'color: black; font-size: 20px;'>Tablero de información.</h2>"), 
          h2("Propósito"), 
          p("Este micrositio contiene información sobre las medidas que está tomando el gobierno de cada entidad de la República Mexicana para proteger o reactivar la economía ante la contingencia causada por el COVID-19, y las estrategias a tomar para la recuperación económica posterior."),
          h2("Base de datos"), 
          p("La base de datos es un esfuerzo conjunto de los miembros del Laboratorio Nacional de Políticas Públicas para monitorear las medidas económicas del gobierno, y es actualizada de manera constante todos los días, ante nuevas noticias relativas a las medidas ante la contingencia."),
          p("La información que conforma la base de datos proviene de la revisión de fuentes de comunicación oficiales de los gobiernos de los estados (secciones de prensa de las páginas web estatales, perfiles de Twitter o videos de Youtube) y de publicaciones periodísticas basadas en las declaraciones de funcionarios estatales. En algunos casos, se incluyen propuestas de acciones provenientes de organismos coordinadores estatales. El registro de estas fuentes se hace de manera diaria."), 
          p('Esta base se encuentra libre para su descarga en la sección de "Datos".'),
          HTML("<h2 style = 'color: black; font-size: 20px;'>Variables de la base de datos.</h2>"), 
          fluidPage(
            fluidRow(
              column(10, offset = 1, tableOutput("tabVars"))
            )
          ),
          HTML("<h2 style = 'color: black; font-size: 20px;'>Categorias de las medidas.</h2>"), 
          fluidPage(
            fluidRow(
              column(10, offset = 1, tableOutput("tabCats"))
            )
          ),
          h2("Equipo"), 
          p("El equipo del LNPP encargado del presente trabajo está conformado por: Eduardo Sojo, Cristina Galíndez y Alaín de Remes como coordinadores, así como Juvenal Campos, Nayeli Aguirre, Isabel Maya, Jorge Puga, Victor León, Josué González y Ángel Pérez como responsables de la base de datos.")
        )
)  
  
)

ui <- dashboardPage(skin = "black", dbHeader, sidebar, body)
