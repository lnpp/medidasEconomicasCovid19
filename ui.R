library(shiny)
library(shinydashboard)
library(stringr)
library(shinycssloaders)
library(leaflet)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- navbarPage(title = HTML("Mapa de medidas económicas <br> COVID-19"), id = "intro",

                 tabPanel("Introducción",
                          fluidPage(
                            fluidRow(
                              column(12,
                                     HTML("<h2>Mapa de las medidas económicas ante la pandemia COVID-19</h2>
                                      <h4 style = 'color:gray; text-align:center;'>Información recopilada de notas periodísticas, reportes de prensa y canales oficiales</h4>"),
                                     uiOutput("fechaDeCorte"),
                                     HTML("<p>En el presente tablero de información, elaborado por el <a target='_blank' rel='noopener noreferrer' href = 'https://www.lnpp.mx'>Laboratorio Nacional de Políticas Públicas del CIDE</a>, se muestran los planes económicos que los diferentes gobiernos estatales están planeando ejecutar en los próximos días para afrontar y recuperarse del shock económico que representa la pandemía actual del COVID-19.</p>
                                      <p>A partir del 30 de Abril, al final de las propuestas se incluye el enlace a la <a target='_blank' rel='noopener noreferrer' href = 'https://www.gob.mx/conamer'>página de la Comisión Nacional de Mejora Regulatoria (CONAMER)</a> en donde se registran <b>las medidas que en materia regulatoria están adoptando los estados para atender la contingencia por COVID 19</b>. Igualmente, <b>se añaden los enlaces a los diferentes micrositios estatales</b> elaborados por las Entidades Federativas para informar a la población sobre la enfermedad Covid-19.</p><br>
                                     ")
                              )
                            ),
                          ),
                          
                          h2("Medidas económicas ante Covid-19"),
                          HTML("<h2 style = 'color: black; font-size: 20px;'>Tablero de información.</h2>"),
                          h2("Propósito"),
                          p("Este micrositio contiene información sobre las medidas que está tomando el gobierno de cada entidad de la República Mexicana para proteger o reactivar la economía ante la contingencia causada por el COVID-19, y las estrategias a tomar para la recuperación económica posterior."),
                          h2("Base de datos"),
                          p("La base de datos es un esfuerzo conjunto de los miembros del Laboratorio Nacional de Políticas Públicas para monitorear las medidas económicas de los gobiernos locales y es actualizada de manera constante."),
                          p("La información que conforma la base de datos proviene de la revisión de fuentes de comunicación oficiales de los gobiernos de los estados (secciones de prensa de las páginas web estatales, perfiles de Twitter o videos de Youtube) y de publicaciones periodísticas basadas en las declaraciones de funcionarios estatales."),
                          p('Esta base se encuentra libre para su descarga en la sección de "Datos".'),
                          HTML("<h2 style = 'color: black; font-size: 20px;'>Variables de la base de datos.</h2>"),
                          fluidPage(
                            fluidRow(
                              column(10, offset = 1, tableOutput("tabVars"))
                            )
                          ),
                          HTML("<h2 style = 'color: green; font-size: 20px;'>Categorías de las variables</h2>"),
                          HTML("<h2 style = 'color: black; font-size: 20px;'>Temporalidad de las medidas.</h2>"),
                          fluidPage(
                            fluidRow(
                              column(10, offset = 1, tableOutput("tabTemporalidad"))
                            )
                          ),
                          HTML("<h2 style = 'color: black; font-size: 20px;'>Tipo de Programa.</h2>"),
                          fluidPage(
                            fluidRow(
                              column(10, offset = 1, tableOutput("tabTipoDePrograma"))
                            )
                          ),
                          HTML("<h2 style = 'color: black; font-size: 20px;'>Tipo de Categoría de Apoyo.</h2>"),
                          fluidPage(
                            fluidRow(
                              column(10, offset = 1, tableOutput("tabCategoriaDeApoyo"))
                            )
                          ),
                          HTML("<h2 style = 'color: black; font-size: 20px;'>Población Objetivo.</h2>"),
                          fluidPage(
                            fluidRow(
                              column(10, offset = 1, tableOutput("tabPoblacionObjetivo"))
                            )
                          ),
                          
                          h2("Equipo"),
                          p("El equipo del LNPP encargado del presente trabajo está conformado por: Eduardo Sojo, Cristina Galíndez y Alaín de Remes como coordinadores, así como Juvenal Campos, Nayeli Aguirre, Isabel Maya, Jorge Puga, Victor León, Josué González y Ángel Pérez como responsables de la base de datos."),
                          p("Posteriormente se retoma el trabajo con la incormporación al equipo de Fernando Valdez Benavides, Helios Omar García Martínez y Ami Gabriela Sosa Vera, del programa de Prácticas Profesionales del LNPP."),
                          p("Igualmente, agradecemos al Equipo del CONAMER por el interés y la ayuda para trabajar juntos en el mejoramiento de esta Base de Datos, así como a la cuenta de Twitter de @sanavigilancia por publicar información sobre los micrositios estatales que informan sobre la enfermedad Covid-19 a la población.")
                        
                          ),                 
                        
        tabPanel("Medidas por Entidad", 
                 fluidPage(
                   fluidRow(
                     column(6, offset = 3, 
                            selectInput(inputId = "selEstado2",
                                        width = "100%",
                                        label = "Seleccione entidad para visualizar sus políticas económicas",
                                        choices = sort(unique(bd$Entidad))
                                        )
                            )
                   )
                 ),
                 br(),
                  fluidPage(
                    fluidRow(
                      column(8, offset = 2, 
                            imageOutput("coat", height = "50px"),
                            br(),
                            uiOutput("nombre", height = "20px"),
                            uiOutput("contenido2")
                         )
                    )
                  )
        ),         
                          
        tabPanel("Mapa",
                          div(class="outer",
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")),  
                              
                              leafletOutput("mapa2", width="100%", height="100%"),    
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 150, left = "auto", right = 30, bottom = "auto",
                                            width = 400, height = 250,   
                                            br(),
                                            selectInput(inputId = "selMapa1", 
                                                        label = "Seleccione información", 
                                                        choices = c("Tipo de Programa", 
                                                                    "Categoría de Apoyo", 
                                                                    "Población Objetivo"), width = "100%"), 
                                            br(), 
                                            uiOutput("mostrarOpts")

                              )
                          )
                 ), #### 

      tabPanel("Enlaces",
            HTML("<img src = 'https://raw.githubusercontent.com/lnpp/medidasEconomicasCovid19/master/www/multimedia/LOGO_CONAMER.png' height = 60px style = 'display: block;
        margin-left: auto;
        margin-right: auto;'>"),
            HTML("<p style = 'text-align:center;'>Enlaces a las páginas del CONAMER por Entidad Federativa y a los Micrositios COVID-19 Estatales</p>"),
          wellPanel(id = "tablaPanel",style = "overflow-x:scroll; max-height: auto; background:white; ",
            tableOutput("tabEnlacesCONAMER")
          ) # Fin del Panel
        ), 
      
      tabPanel("Datos",                
                h2("Sección de descarga de datos"),
                br(),
                HTML("<p style = 'text-align:center;'>En esta sección se pueden descargar los datos del tablero, los cuales contienen información adicional sobre las políticas económicas llevadas a cabo por los Estados.</p>"),

    wellPanel(id = "tablaPanel",style = "max-height: auto; background:white;",
         
                fluidPage(
                     fluidRow(
                       column(4, 
                              HTML("<b>Seleccione Entidad: </b>"), br(),
                              myPickerInput(inputId = "selEdoFiltro", 
                                                   title = "Todas las entidades",
                                                   # multiple = TRUE, 
                                                   # selected = "Todas las entidades", 
                                                   choices = c("Todas las entidades", 
                                                               sort(unique(bd$Entidad))))
                              ), 
                       column(4, selectInput(inputId = "selCategoriaApoyoFiltro", 
                                             label = "Seleccione Categoría de Apoyo", 
                                             # multiple = TRUE, 
                                             selected = "Todas las categorías",
                                             choices = c("Todas las categorías", 
                                                         sort(unique(bd$`Categoría de Apoyo`))))), 
                       column(4, selectInput(inputId = "selPobObjetivoFiltro", 
                                             label = "Seleccione Población Objetivo", 
                                             # multiple = TRUE, 
                                             selected = "Todas las Poblaciones Objetivo", 
                                             choices = c("Todas las Poblaciones Objetivo", 
                                                         sort(unique(bd$Clasificación)))))
                       ), 
                     fluidRow(
                       column(4, selectInput(inputId = "selPeriodoFiltro", 
                                             label = "Seleccione Periodo", 
                                             # multiple = TRUE, 
                                             selected = "Todos los periodos",
                                             choices = c("Todos los periodos",
                                                         sort(unique(bd$Periodo))))), 
                       column(4, selectInput(inputId = "selMontoFiltro", 
                                             label = "Seleccione Monto", 
                                             # multiple = TRUE, 
                                             selected = "Con y sin monto reportado",
                                             choices = c("Con y sin monto reportado", "Con monto reportado", "Sin monto reportado"))), 
                       column(4, br(), downloadButton("download_data", style = "    padding-top: 5px;margin-top: 6px;", label = "Descargar"))        
                 
                   )      
               )
            ),
               
                br(),
                br(),
                withSpinner(DT::dataTableOutput("tabla"))
         # FIN DE DATOS
      )
)
