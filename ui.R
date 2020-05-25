library(shiny)
library(shinydashboard)
library(stringr)
library(shinycssloaders)

dbHeader <- dashboardHeader(title = "Medidas económicas ante COVID-19", 
                            titleWidth = 360,
                           tags$li(a(href = 'https://www.cide.edu',
                                     img(src = 'https://www.cide.edu/wp-content/themes/cide_general/img/logo_cide.png',
                                         title = "CIDE", height = "30px", id = "optionalstuff"),
                                     style = "padding-top:10px; padding-bottom:10px;" 
                                     ),
                                   class = "dropdown"),

                           tags$li(a(href = 'http://lnpp.cide.edu',
                                     img(src = 'http://lnpp.cide.edu/wp-content/themes/lnpp/images/logo.svg',
                                         title = "LNPP", height = "30px", id = "optionalstuff3"),
                                     style = "padding-top:10px; padding-bottom:10px;"),
                                   class = "dropdown"), 
                           
                           tags$li(a(href = 'https://www.gob.mx/conamer',
                                     img(src = 'https://raw.githubusercontent.com/lnpp/medidasEconomicasCovid19/master/www/multimedia/LOGO_CONAMER.png',
                                         title = "CONAMER", height = "30px", id = "optionalstuff"),
                                     style = "padding-top:10px; padding-bottom:10px;"),
                                   class = "dropdown"), 
                           
                           tags$li(a(href = 'https://github.com/lnpp/medidasEconomicasCovid19',
                                     img(src = 'https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png',
                                         title = "REPO", height = "30px", id = "optionalstuff2"),
                                     style = "padding-top:10px; padding-bottom:10px;"),
                                   class = "dropdown")
                           
                           
                           )

sidebar <- dashboardSidebar(width = 210, 
                            sidebarMenu(
                              menuItem(htmltools::HTML("<b>Tablero</b>"), tabName = "PLAT", icon = icon("table")),
                              menuItem(htmltools::HTML("<b>Documentación</b>"), tabName = "DOCUM", icon = icon("book")), 
                              menuItem(htmltools::HTML("<b>Respuestas<br>regulatorias<br>CONAMER</b>"), tabName = "ENLACES", icon = icon("gavel")),
                              menuItem(htmltools::HTML("<b>Datos</b>"), tabName = "DATOS", icon = icon("download"))
                            ))

body <- dashboardBody(  
               tags$head(
                 includeCSS("styles.css"), 
                 HTML("<!-- Google Tag Manager -->
<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
})(window,document,'script','dataLayer','GTM-KFZPZ7N');</script>
<!-- End Google Tag Manager -->
")), 
               
               tags$head(HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=UA-164240496-1'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  gtag('config', 'UA-164240496-1');
</script>
")),
                 tags$body(
                   HTML('<!-- Google Tag Manager (noscript) -->
<noscript><iframe src="https://www.googletagmanager.com/ns.html?id=GTM-KFZPZ7N"
height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
<!-- End Google Tag Manager (noscript) -->
')),
               
tabItems(    
  tabItem("PLAT",                
          fluidPage(
            fluidRow(
              column(12, 
                     HTML("<h2>Mapa de las medidas económicas ante la pandemia COVID-19</h2>
                     <h4 style = 'color:gray; text-align:center;'>Información recopilada de notas periodísticas, reportes de prensa y canales oficiales</h4>"), 
                     uiOutput("fechaDeCorte"),
                     HTML("<p>En el presente tablero de información, elaborado por el <a href = 'https://www.lnpp.mx'>Laboratorio Nacional de Políticas Públicas del CIDE</a>, se muestran los planes económicos que los diferentes gobiernos estatales están planeando ejecutar en los próximos días para afrontar y recuperarse del shock económico que representa la pandemía actual del COVID-19.</p>
                     <p>A partir del 30 de Abril, al final de las propuestas se incluye el enlace a la <a href = 'https://www.gob.mx/conamer'>página de la Comisión Nacional de Mejora Regulatoria (CONAMER)</a> en donde se registran <b>las medidas que en materia regulatoria están adoptando los estados para atender la contingencia por COVID 19</b>. Igualmente, <b>se añaden los enlaces a los diferentes micrositios estatales</b> elaborados por las Entidades Federativas para informar a la población sobre la enfermedad Covid-19.</p><br>
                    ")
              )
            ),
            
            fluidRow( # Inicio del UI de escritorio
              column(8, box(width = 12,
                            solidHeader = TRUE,
                            title = HTML("<b style = 'color:white;'>Mapa de los estados de México</b>"),
                            status = "warning",
                            fluidPage(
                              fluidRow(
                                column(12,  htmltools::HTML("<b class = 'center' style = 'color:black; text-align:center; padding-top:0%;'>Seleccione un estado para visualizar una lista de las políticas públicas.</b>")
                                )
                              )
                            ),
                            withSpinner(leaflet::leafletOutput("mapa", height = "600px"))
              )
              ),
              column(4,
                     wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 685px; background:white; ",
                               
                               fluidPage(
                                 fluidRow(
                                   column(12, imageOutput("coat", height = "50px"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(12,uiOutput("nombre", height = "20px"))
                                 ),
                                 fluidRow(
                                   column(12, uiOutput("contenido"))
                                 )
                               )
                     )
              )
            ) # Fin del UI de escritorio.
          )
  ), 

  tabItem("DATOS",                
          h2("Sección de descarga de datos"), 
          br(),
          HTML("<p style = 'text-align:center;'>En esta sección se pueden descargar los datos del tablero, los cuales contienen información adicional sobre las políticas económicas llevadas a cabo por los Estados.</p>"),
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
          p("El equipo del LNPP encargado del presente trabajo está conformado por: Eduardo Sojo, Cristina Galíndez y Alaín de Remes como coordinadores, así como Juvenal Campos, Nayeli Aguirre, Isabel Maya, Jorge Puga, Victor León, Josué González y Ángel Pérez como responsables de la base de datos."), 
          p("Igualmente, agradecemos al Equipo del CONAMER por el interés y la ayuda para trabajar juntos en el mejoramiento de esta Base de Datos, así como a la cuenta de Twitter de @sanavigilancia por publicar información sobre los micrositios estatales que informan sobre la enfermedad Covid-19 a la población.")
        ), 
  
  tabItem("ENLACES",
      HTML("<img src = 'https://raw.githubusercontent.com/lnpp/medidasEconomicasCovid19/master/www/multimedia/LOGO_CONAMER.png' height = 60px style = 'display: block;
  margin-left: auto;
  margin-right: auto;'>"),    
      HTML("<p style = 'text-align:center;'>Enlaces a las páginas del CONAMER por Entidad Federativa y a los Micrositios COVID-19 Estatales</p>"), 
      tableOutput("tabEnlacesCONAMER")
  )
  
)  
  
)

ui <- dashboardPage(skin = "black", dbHeader, sidebar, body)
