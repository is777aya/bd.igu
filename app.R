library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(mapview)
library(esquisse)
library(shinyWidgets)
library(xml2)
library(magrittr)
library(rvest)
library(readxl)
library(maps)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(plotly)
library(geojsonio)
library(shinydashboard)
library(shinythemes)
library(lessR)
library(plyr)
library(usethis)
library(devtools)
library(treemapify)
library(htmltools)
library(htmlwidgets)
library(webshot)
library(leaflet.extras)
library(tidyr)
library(viridisLite)
library(viridis)
library(tidyverse)
library(writexl)

#cultivo <- cbba.agricola$producto   #filter(agricola, desc_departamento == "Cochabamba" & cantidad > 10) #
#cbba.agricola.mun <- filter(cbba.agricola, desc_municipio == "Villa Tunari") #
#comunidad <- cbba.agricola$desc_comunidad

#cleantable_3 <- cleantable_3 %>%
#   select(
#    Departamento = desc.departamento,
#    Municipio = desc.municipio,
#Comunidad = desc_comunidad,
#    Grupo.Cultivo = grupo.cultivo,
#    Cultivo = producto,
#    Estacion.Agricola = estacion,
#    Gestion = gestion,
#    Produccion = prod,
#    Superficie = sup,
#    Rendimiento = rend,
#    Lat = lat,
#    Long = long
#  )

#cleantable_3 <-cleantable_3
options(scipen=999, digits=2)


vars <- c(
  "Dep" = "Departamento",
  "Cul" = "Cultivo",  
  "Mun" = "Municipio",
  "Date" = "Gestion"
        )

dep <- c("La Paz","Cochabamba","Santa Cruz","Oruro","Beni","Pando","Tarija","Potosi","Chuquisaca")
choices <- c("Achiote","Ají","Ajo","Alfalfa","Algodón en fibra","Arroz con cáscara","Arveja verde","Avena","Avena forrajera","Betarraga","Cacao","Café","Camote","Caña de azúcar","Cañawa","Cebada en grano","Cebada forrajera","Cebolla","Centeno","Chía","Chirimoya","Ciruelo","Coles","Coliflor","Durazno","Frijol","Frutilla","Garbanzo","Grano de girasol","Guinda","Haba verde","Higo","Hualuza","Lechuga","Lima","Limón","Locoto","Maíz","Maíz choclo","Mandarina","Mangos","Maní","Manzana","Membrillo","Naranja","Oca","Palta","Papa","Papaliza","Papaya","Pepino","Pera","Piña","Plátano (Banano)","Plátano (Postre)","Quinua","Rábano","Racacha","Sandía","Sésamo","Sorgo","Soya","Tabaco","Té","Tomate","Toronja","Trigo","Tuna","Uva","Vainita","Yuca","Zanahoria","Zapallo")
mun <- c("Acasio","Achacachi","Achocalla","Aiquile","Alalay","Alto Beni","Ancoraimes","Antequera (Bolivar)","Anzaldo","Apolo","Arampampa","Arani","Arbieto","Arque","Ascencion de Guarayos","Atocha","Aucapata","Ayata","Ayo Ayo","Azurduy","Batallas","Baures","Belén de Andamarca","Bella Flor","Bermejo","Betanzos","Bolivar","Bolpebra (Mukden)","Boyuibe","Buena Vista","Cabezas","Cairoma","Caiza D","Cajuata","Calacoto","Calamarca","Camargo","Camiri","Capinota","Caquiaviri","Caracollo","Caranavi","Carangas","Carapari","Caripuyo","Carmen Rivero Torrez","Catacora","Chacarilla","Challapata","Chaqui","Charagua","Charaña","Charazani (Gral. Perez)","Chayanta","Chimore","Chipaya","Choquecota","Chua Cocani","Chulumani","Chuma","Chuquihuta","Ckochas","Cliza","Cobija","Cocapata","Coipasa","Colcha K","Collana","Colomi","Colpa Belgica","Colquechaca","Colquencha","Colquiri","Comanche","Comarapa","Combaya","Concepcion","Copacabana","Coripata","Coro Coro","Coroico","Corque","Cotagaita","Cotoca","Cruz de Machacamarca","Cuatro Cañadas","Cuchumuela","Cuevo","Culpina","Curahuara de Carangas","Curva","Desaguadero","El Alto","El Choro","El Puente","El Sena","El Torno","El Villar","Entre Rios","Entre Ríos","Escara","Escoma","Esmeralda","Eucaliptus","Exaltacion","Fernandez Alonso","Filadelfia","Gonzalo Moreno","Gral. Saavedra","Guanay","Guaqui","Guayaramerin","Gutierrez","Huacareta","Huacaya","Huachacalla","Huaracaje","Huarina","Huatajata","Humanata","Ichoca","Icla","Incahuasi","Independencia","Ingavi","Inquisivi","Irupana","Ixiamas","Jesus de Machaca","La Asunta","La Guardia","La Rivera","Lagunillas","Laja","Las Carreras","Llallagua","Llica","Loreto","Luribay","Machacamarca","Machareti","Magadalena","Mairana","Malla","Mapiri","Mecapaca","Mineros","Mizque","Mocomoco","Mojinete","Mojocoya","Monteagudo","Montero","Moro Moro","Morochata","Muyupampa","Nazacara de Pacajes","Nuestra Señora de La Paz","Nuevo Manoa (Nueva Esperanza)","Ocuri","Okinawa Uno","Omereque","Padcaya","Padilla","Pailon","Palca","Palos Blancos","Pampa Aullagas","Pampa Grande","Papel Pampa","Paria (Soracachi)","Pasorapa","Patacamaya","Pazña","Pelechuco","Pocoata","Pocona","Pojo","Poopó","Porco","Poroma","Porongo","Portachuelo","Porvenir","Postrer Valle","Potosi","Presto","Pucara","Pucarani","Puerto Acosta","Puerto Carabuco","Puerto Perez","Puerto Quijarro","Puerto Rico","Puerto Siles","Puerto Suarez","Puerto Villarroel","Puna","Punata","Quiabaya","Quillacollo","Quime","Quirusillas","Ravelo","Reyes","Riberalta","Robore","Rurrenabaque","S. P. de Buena Vista","Sabaya","Sacaba","Sacabamba","Saipina","Salinas de Garci Mendoza","Samaipata","San Agustin","San Andres","San Andres de Machaca","San Antonio de Esmoruco","San Antonio de Lomerio","San Benito","San Borja","San Buena Ventura","San Carlos","San Ignacio","San Ignacio de Velasco","San Javier","San Joaquin","San Jose de Chiquitos","San Juan de Yapacani","San Julian","San Lorenzo","San Lucas","San Matias","San Miguel de  Velasco","San Pablo de Lipez","San Pedro","San Pedro (Conquista)","San Pedro de Curahuara","San Pedro de Quemes","San Pedro de Tiquina","San Pedro de Totora","San Rafael","San Ramon","Santa Ana de Yacuma","Santa Cruz","Santa Rosa de Yacuma","Santa Rosa del Abuna","Santa Rosa del Sara","Santiago de Andamarca","Santiago de Callapa","Santiago de Huari","Santiago de Huata","Santiago de Huayllamarca","Santiago de Machaca","Santivañez","Santos Mercado","Santuario de Quillacas","Sapahaqui","Shinaota","Sica sica","Sicaya","Sipesipe","Sopachuy","Sorata","Sucre","Tacachi","Tacacoma","Tacobamba","Tacopaya","Tahua","Tapacarí","Tarabuco","Taraco","Tarata","Tarija","Tarvita","Teoponte","Tiawanacu","Tinguipaya","Tipuani","Tiquipaya","Tiraque","Tito Yupanqui","Toco","Todos Santos","Tolata","Toledo","Tomave","Tomina","Toro Toro","Totora","Trigal","Trinidad","Tupiza","Turco","Umala","Uncia","Uriondo","Urmiri","Urubicha","Uyuni","Vacas","Valle Grande","Viacha","Vila Vila","Villa Abecia","Villa Alcala","Villa Charcas","Villa de Sacaca","Villa Huanuni","Villa Libertad Licoma","Villa Nueva (Loma Alta)","Villa Rivero","Villa Serrano","Villa Tunari","Villamontes","Villazon","Vinto","Vitichi","Waldo Ballivian","Warnes","Yaco","Yacuiba","Yamparaez","Yanacachi","Yapacani","Yocalla","Yotala","Yunchara","Zudañez")
anios <- c("2013","2014","2015","2016","2017","2018","2019","2020","2021")

######para descargar .csv
.write_file <- function(file_path, data)
{
  data.table::fwrite(x = data, file = file_path)
}
##########################

ui <- fluidPage(
  navbarPage(
    title="Vocacion Agricola", id="nav",
    
    tabPanel("Mapa",
             div(class="outer",
                 
                 tags$head(
                   # aqui incluyo los archivos .css
                   includeCSS("styles.css"),
                   includeScript("gomap.js")
                 ),
                 
                 leafletOutput("map", width="100%", height="100%"),
                 # Shiny versions prior to 0.11 should use class = "modal" instead.
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                               width = "400", height = "auto", 
                               
                               h2("Eliga Cultivo & Municipio"),
                               
                               selectInput("dep", "Departamento", unique(as.character(cleantable_3$Departamento)) ),
                               #selectInput("mun", "Municipio", unique(as.character(cleantable_3$Municipio))  ),
                               selectInput("cultiv", "Tipo Cultivo",choices=choices),
                               verbatimTextOutput("choices"),
                               #selectInput("myselect", "Select box", choices=choices, multiple = TRUE, selected = "choice 1"),
                               
                               
                               
                               selectizeInput("years", "Gestion", choices=anios),
                               
                               #radioButtons(inputId = "image", label = "Seleccion formato Imagen", choices = c("png", "pdf")),
                               
                               
                               #seleccion con barra deslizadora
                               #sliderInput("years", "Gestion",
                                #          min=2010, max=2020,
                                 #        value = c(2010:2020), sep = "", step = 1),
                               
                               
                               conditionalPanel("input.mun == 'cleantable_3' || input.cultiv == 'cleantable_3'"),
                               
                               
                               plotOutput("scatterCollegeIncome", height = 250), #, width = "auto"
                               #plotOutput("serie.tiempo", height = 250, width = "auto"),
                               plotlyOutput("histCentile", height = 250)#, # grafico histogramas                 
                               
                               #downloadButton('image', 'Descargue Imagen')
                 ),
                 
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 500, left = 20 , right = "auto", bottom = 0,
                               width = 330, height = "auto", 
                               
                               plotlyOutput("rank", height = "550"),
                               
                               
                               #tags$div(id="cite",
                               #'Fuente ', tags$em('Censo Agricola-2013, Encuesta Agricola-2015', 'PSARDI-2017'), ' igu15111986')
                 )
             )
    ),
    ###################################################################################################
    ###################################################################################################
    tabPanel("Tabla de Datos",
             fluidRow(
               column(3,
                      selectInput("depa", "Departamento", dep)
                      
               ),
               
               column(3,
                      conditionalPanel("depa",
                                       selectInput("cultivo1", "Municipios", mun)
                      )
               ),
               
               column(3,
                      conditionalPanel("depa",
                                       selectInput("cultivo2", "Tipo Cultivo", choices),
                                       
                                       downloadButton("downloadData", "Descargar") #,
                                       
                                       
                                       #downloadButton("downloadImagen", "Download")
                                       
                      )
               )
             ),
             
             hr(),
             DT::dataTableOutput("ziptable")
    ),
    
    conditionalPanel("false", icon("crosshair"))
  )
)


library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)
#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#cbba.agricola <- cbba.agricola[order(cbba.agricola$prod, decreasing = F) ,  ]

cleantable_3 <- cleantable_3[order(cleantable_3$Produccion, decreasing = T) ,  ]

##################################################################################################################
##################################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  # seleccionar choices _except_ "Select All"  del panel de control
  observe({
    if("all" %in% input$cultiv)
      selected_choices=choices[-1] # choose all the choices _except_ "Select All"
    else
      selected_choices=input$cultiv # update the select input with choice selected by user
    updateSelectInput(session, "cultiv", selected = selected_choices)
  })
  
  ## Interactive Map ###########################################
  # Create the map
  
  output$map <- renderLeaflet({
    
    
    leaflet() %>%
      
      
      
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom Nivel: 7",
        onClick=JS("function(btn, map){ map.setZoom(5); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Mi Ubicacion",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      
      addTiles(group = "OpenStreeMap.Default") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri World Gray") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      setView(lng = -64.85, lat = -16.86, zoom = 6) %>%
      
      
      addLayersControl(baseGroups = c("Esri World Gray", "OSM", "Esri World Imagery"), 
                       overlayGroups = c("Mapa Base", "Capitales", "Municipio"), 
                       options = layersControlOptions(collapsed = T)) %>%
      
      
      addWMSTiles("http://geo.gob.bo/geoserver/ows",
                  layers = 'municipal',  #LimitesMunicipales
                  options = WMSTileOptions(format = "image/png", transparent = T),
                  attribution = "GEOBOLIVIA",
                  group = "Municipio" )  %>%
      
      addWMSTiles("http://geo.gob.bo/geoserver/ows",
                  layers = 'CapitalesMunicipales',  #CapitalesMunicipales
                  options = WMSTileOptions(format = "image/png", transparent = T),
                  attribution = "GEOBOLIVIA",
                  group = "Capitales" )  %>%
      
      addWMSTiles("http://geo.gob.bo/geoserver/ows",
                  layers = 'departamento1',  #Limite Departamental
                  options = WMSTileOptions(format = "image/png", transparent = T),
                  attribution = "GEOBOLIVIA",
                  group = "Mapa Base") %>%
      
      
      #addSearchFeatures(
      # targetGroups = 'municipal', # group should match addMarkers() group
      # options = searchFeaturesOptions(
      #  zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
      #  autoCollapse = TRUE, hideMarkerOnCollapse = TRUE
      #)
      #) %>%
      
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE))  %>%
      
      hideGroup("Municipio") %>%
      hideGroup("Capitales") %>%
      hideGroup("Mapa Base") %>%
      hideGroup("Esri World Imagery") %>%
      hideGroup("OSM")
    
  }
  )
  
  ###########################################################################################    
  
  # codigo reactiva que devuelve el conjunto de puntos que esten dentro de los límites geograficos
  
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(cleantable_3[FALSE,])
    
    
    bounds <- input$map_bounds
    latRng <- range(bounds$north,bounds$south)
    lngRng <- range(bounds$east,bounds$west) 
    date <-input$years
    #depto <-input$dep
    #mun.1 <-input$mun
    cul.1 <-input$cultiv
    
    
    subset(cleantable_3,
           Lat >= latRng[1] & Lat <= latRng[2] &
             Long >= lngRng[1] & Long <= lngRng[2] & 
             Gestion == date &  
             #Departamento == depto & 
             #Municipio == mun.1 &
             Cultivo == cul.1 
    )
  })      
  #############################################################################################
  ###### grafico serie de tiempo
  
  #output$serie.tiempo <- renderPlot({
  
  #cultivoIn <- input$cultiv
  #municipioIn <- input$mun
  #yearsIn <-input$years
  #ini.gestion <- input$years[1] // este Input sirve para la pocion la barra deslizadora (slider)
  #fin.gestion <- input$years[2]
  
  #if (nrow(zipsInBounds()) == 0)
  # return(NULL)
  
  #plot(cbba.agricola$superficie, cbba.agricola$cantidad, data = zipsInBounds())
  #filt.comunidad.0 <- cleantable_3[(cleantable_3$Cultivo == cultivoIn) & (cleantable_3$Municipio == municipioIn) & (cleantable_3$Gestion == yearsIn)   ,]
  
  
  #plot(cleantable_3$Gestion, cleantable_3$Produccion, data = zipsInBounds(),   type = "l")
  #})
  #############################################################################################   
  
  
  
  #############################################################################################
  ###### Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, cbba.agricola$cantidad, breaks = 20)$breaks
  
  output$scatterCollegeIncome <- renderPlot({
    
    
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    
    ########graffito dispersion antiguo
    #filt.comunidad.0 <- cleantable_3[(cleantable_3$Cultivo == cultivoIn) & (cleantable_3$Municipio == municipioIn) & (cleantable_3$Gestion == yearsIn)   ,] // codigo antiguo de grafico barras
    #xyplot(Rendimiento ~ Produccion , data = zipsInBounds(), grid=T, xlab = "Prod (Tm)", ylab = "Rend (Tm/Ha)", pch=19, main = "Produccion VS Rendemiento")  // codigo antiguo de grafico barras
    
    ########----- grafico dispersion con ggplot -----####
    ggplot(zipsInBounds()) +
      aes(x = Superficie, y = Rendimiento) +
      geom_point(size = 2.5, colour = "#0d0887") +
      geom_text(data=zipsInBounds(), mapping=aes(x=Superficie, y= Rendimiento , label=round(Rendimiento,digits = 2 )), vjust=2, hjust=0.5, size=4) +
      labs(x = "Sup. (Ha)", y = "Rend. (Tm/Ha)", title = "Superficie VS Rendimiento", color = "black") +
      ggthemes::theme_solarized()
    
  })
  
  ########----- grafico dispersion con ploty -----####
  
  #p <-ggplot(zipsInBounds()) +
  #          aes(x=Superficie, y=Rendimiento) +
  #      geom_point(size = 1.5, colour = "#0d0887") +
  #geom_text(mapping=aes(x=Superficie, y= Rendimiento , label=round(Rendimiento,digits = 2 )), vjust=2, hjust=0.5, size=4) +
  #     labs(x = "Sup. (Ha)", y = "Rend. (Tm/Ha)", title = "Superficie VS Rendimiento", color = "black") +
  #     ggthemes::theme_solarized()
  #   ggplotly(p)
  
  # plot_ly(data = zipsInBounds(), x = ~Superficie, y = ~Rendimiento, 
  #         marker = list(
  #                        size = 10, 
  #                        color = 'rgba(255, 182, 193, .9)',
  #                        line = list(color = 'rgba(152, 0, 0, .8)')))
  
  #})
  
  #############################################################################################    
  
  
  output$histCentile <- renderPlotly({
    
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    #grafico porcentaje
    
    plot_ly(zipsInBounds(), labels = ~Estacion.Agricola, values = ~Produccion, type = 'pie')
    
  })
  
  ########grafico rankin produccion por comunidad  
  
  output$rank <- renderPlotly({
    
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    #filt_comunidad1 <- cleantable_3[(cleantable_3$Cultivo == cultivoIn) & (cleantable_3$Municipio == municipioIn) & (cleantable_3$Gestion == yearsIn)   ,] // codigo antiguo de grafico barras
    #barchart(zipsInBounds()$Comunidad ~ Produccion, data = filt_comunidad1,  col = "red", xlab = "Prod (Tm)", main = "Rank Comunidades Productoras") // codigo antiguo de grafico barras
    
    ####### -------- grafico barras ggplot
    
    # ggplot(zipsInBounds()) +
    #  aes(x = Municipio, weight = Produccion ) +
    #  geom_bar(fill = "#006d2c") +
    #  geom_text(aes(label = round(Produccion, digits = 2), y = Produccion, hjust = 2), color = "white") + 
    #  labs(x = "Cultivos", y = "Produccion (Tm)", title = "Rankin Cultivos") +
    #  coord_flip() +
    #  theme_minimal()
    
    #  })
    
    ####### -------- grafico barras plotly
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Produccion (Tm)",
      titlefont = f,
      tickformat = "digits"
    )
    y <- list(
      title = "Municipios",
      titlefont = f
    )
    
    
    
    fig <- plot_ly(zipsInBounds(), x = ~Produccion, y = ~Municipio, name = 'Rankin de produccion (Tm)', 
                   type = 'bar', 
                   text = ~Produccion, textposition = 'auto',
                   orientation = 'h',
                   marker = list(color = ~Produccion, colorscale = "Viridis", showscale = FALSE,    #color = ~Produccion     #marker = list(color = 'rgba(50, 171, 96, 0.6)'
                   
                                 line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
    
    
    fig <- fig %>% layout(xaxis = x, yaxis = list(categoryorder = "category descending"))   
    
    
    
    fig
    
  })
  
  
  ######################################################################  
  ######################################################################
  observe({
    
    cultivoIn <- input$cultiv
    #municipioIn <- input$mun
    yearIn <-input$years
    deptoIn <-input$dep
    
    
    filt_delays_1 <- cleantable_3[(cleantable_3$Cultivo == cultivoIn) & (cleantable_3$Gestion == yearIn) & (cleantable_3$Departamento == deptoIn),] # & (cleantable_3$Gestion >= ini.gestion) & (cleantable_3$Gestion <= fin.gestion)   & cleantable_3$Gestion >= input$years[1] & cleantable_3$Gestion <= input$years[2]
    
    colorData <- filt_delays_1[["Produccion"]]
    
    #pal <- colorBin("viridis", colorData, 3, pretty = TRUE)
    
    pal <- colorBin(
      palette = "viridis",
      domain = colorData,
      bins = 3,
      pretty = TRUE,
      na.color = "#808080",
      alpha = FALSE,
      reverse = FALSE,
      right = FALSE
    )
    
    radius <- log(filt_delays_1[["Produccion"]]) * 500 #*0.09 
    
    leafletProxy("map", data = filt_delays_1) %>% 
      clearShapes() %>%
      addCircles(~Long, ~Lat, radius=radius, data = filt_delays_1,
                 stroke=F, fillOpacity=0.7, fillColor=pal(colorData)) %>%
      addLegend("bottomright", pal=pal, values=colorData, title="Produccion Tm", 
                layerId="colorLegend",
                
      )
    
  })
  
  
  ######################################################################  
  ######################################################################
  
  #### trial of pops
  
  # Show a popup of the zip code and probability of accident
  showZipcodePopup <- function(lat,lng) {
    
    cultivoIn <-input$cultiv
    #municipioIn <- input$mun
    yearIn <-input$years
    deptoIn <-input$dep
    
    filt_delays <- cleantable_3[(cleantable_3$Cultivo == cultivoIn)  & (cleantable_3$Gestion == yearIn) & (cleantable_3$Departamento == deptoIn),] # & (agricola$codigo_municipal == cultivo2In)
    
    selectedAirport <- filt_delays[(filt_delays$Lat == lat) & (filt_delays$Long == lng),]
    
    content <- as.character(tagList(
      
      tags$h5("Municipio:", selectedAirport$Municipio),
      tags$h5("Cultivo:", selectedAirport$Cultivo),
      tags$h5("Campaña:", selectedAirport$Estacion.Agricola),
      tags$h5("Grupo Cultivo:", selectedAirport$Grupo.Cultivo),
      tags$h5("Produccion (Tm):", selectedAirport$Produccion),
      tags$h5("Superficie (Ha):", selectedAirport$Superficie),
      tags$h5("Rendimiento (Tm/Ha):", selectedAirport$Rendimiento)
      
    ))
    
    leafletProxy("map") %>% addPopups(lng,lat,content)
    
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showZipcodePopup(event$lat, event$lng)
      
    })
  })
  
  
  
  ############################################################################################### 
  ## Data Explorer ###########################################
  
  observe({
    cultivo1 <- if (is.null(input$depa)) character(0) else {
      filter(cleantable_3, Departamento %in% input$depa) %>%
        `$`('Municipio') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cultivo1[input$cultivo1 %in% cultivo1])
    updateSelectizeInput(session, "cultivo1", choices = cultivo1,
                         selected = stillSelected, server = TRUE)
  })
  
  
  
  observe({
    cultivo2 <- if (is.null(input$depa)) character(0) else {
      cleantable_3 %>%
        filter(Departamento %in% input$depa,
               is.null(input$cultivo1) | Municipio %in% input$cultivo1) %>%
        `$`('Cultivo') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cultivo2[input$cultivo2 %in% cultivo2])
    updateSelectizeInput(session, "cultivo2", choices = cultivo2,
                         selected = stillSelected, server = TRUE)
  })  
  
  
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      #zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  
  
  
  output$ziptable <- DT::renderDataTable(server = FALSE,{
    
    
    df <- cleantable_3 %>%
      
      filter(
        is.null(input$depa) | Departamento %in% input$depa,
        is.null(input$cultivo1) | Municipio  %in% input$cultivo1,
        is.null(input$cultivo2) | Cultivo %in% input$cultivo2
        
      ) %>%
      
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long,  '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    
  })    
  
  
  ########script REACTIVE para los datos filtrados del DATA EXPLORER
  
  filter.csv <- reactive({
    cleantable_3 %>%
      
      filter(
        is.null(input$depa) | Departamento %in% input$depa,
        is.null(input$cultivo1) | Municipio  %in% input$cultivo1,
        is.null(input$cultivo2) | Cultivo %in% input$cultivo2
        
      )
  })
  
  
  ####### DESCARGAR DATOS en formato CSV
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$cultivo2, Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filter.csv(), file, row.names = F) #cbba.agricola
      
    }
  )
  
  
  
  
  # Create a separate map object for printing - same properties as what's on the screen
  #map_reactive <- reactive({
  #  leaflet(zipsInBounds) %>%
  #    addProviderTiles(providers$OpenStreetMap)
  
  #  })
  
  
  # user_created_map <- reactive({
  #  m = map_reactive() %>%
  #    setView(lng = input$map_center$lng, lat = input$map_center$lat, 
  #            zoom = input$map_zoom)
  
  #  m
  
  # })
  
  # Print the map to the working directory
  
  # output$downloadImagen <- downloadHandler(
  #  filename = "eiffelmap.html",
  #  content = function(file){
  
  
  #   saveWidget(
  #      widget = user_created_map()
  #      , file = file
  #    )
  #  }
  # )
  
  
  
  
}   

shinyApp(ui = ui, server = server)


#ggplot(cleantable_3,
#      aes(x = factor(""), fill = Estacion.Agricola) ) +
#geom_bar() +
#coord_polar(theta = "y") +
#scale_x_discrete("")


#ggplot(cleantable_3, aes(x=factor(1), fill=Estacion.Agricola))+
# geom_bar(width = 1)+
#coord_polar("y", start=0)

#PieChart(Estacion.Agricola, hole = 0, values = "%", data = cleantable_3,
#        fill = c("lightblue", "pink"), main = "")


#ggplot(cleantable_3, aes(area = Produccion, fill = Cultivo, label = Cultivo,
#             subgroup = Municipio)) +
#geom_treemap() +
#geom_treemap_subgroup_border() +
#geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
#                            "black", fontface = "italic", min.size = 0) +
#geom_treemap_text(colour = "white", place = "topleft", reflow = T)

#ggplot(cleantable_3, aes(area = Produccion, fill = Superficie, label = Cultivo)) +
# geom_treemap() +
#geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
#                 grow = TRUE)




#library(writexl)

#write_xlsx(x = cleantable_3, path = "cleantable_3.xlsx", col_names = TRUE)

#plot_ly(data = cleantable_3, x = ~Superficie, y = ~Rendimiento, 
#        marker = list(
#                      size = 10, 
#                      color = 'rgba(255, 182, 193, .9)',
#                      line = list(color = 'rgba(152, 0, 0, .8)',
#                                  width = 2)))


#p <-ggplot(cleantable_3, aes(x=Superficie, y=Rendimiento)) +
#  geom_point(shape=1)
#fig <- ggplotly(p)
#fig

#fig <- plot_ly(cleantable_3, x = ~Municipio, weight = ~Produccion, type  = 'bar', orientation = 'h', name = 'SF Zoo',
#              marker = list(color = 'rgba(246, 78, 139, 0.6)',
#                            line = list(color = 'rgba(246, 78, 139, 1.0)',
#                                        width = 3)))
#fig <- fig %>% add_trace(x = ~Estacion.Agricola, name = 'LA Zoo',
#                        marker = list(color = 'rgba(58, 71, 80, 0.6)',
#                                      line = list(color = 'rgba(58, 71, 80, 1.0)',
#                                                  width = 3)))
#fig <- fig %>% layout(barmode = 'stack',
#                     xaxis = list(title = ""),
#                     yaxis = list(title =""))

#fig

#fig1 <- plot_ly(cleantable_3, x = ~Produccion, y = ~Municipio, name = 'Rankin de produccion',
#                type = 'bar', orientation = 'h',
#                marker = list(color = 'rgba(50, 171, 96, 0.6)',
#                              line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)))

#fig1

