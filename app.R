library(leaflet)
library(shiny)
library(sf)
library(stringr)
library(dplyr)
library(slickR)

### Datos relacionados con las estaciones y los archivos de sus gráficas
estaciones_sf <- st_read("./data/av_estaciones/av_estaciones.shp")
estaciones_archivos <- read.csv("./data/av_estaciones/estaciones_id_graficas.csv")
###
### Datos relacionados a las zonas de la cuenca y sus gráficas
zonas_archivos <- read.csv("./data/zonas_id_graficas.csv")
###

lista_tipologias <- list("Biofiltro" = 1,"Humedales de tratamiento" = 2, 
                         "Jardín microcuenca" = 3, "Jardín de lluvia" = 4,
                         "Laguna de retención" = 5, "Pavimento permeable" = 6,
                         "Plaza de agua" = 7, "Pozo de infiltración" = 8,
                         "Presa filtrante" = 9, "Revegetación de laderas" = 10,
                         "Zanja-bordo" = 11, "Suelo esponja" = 12)

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  modalDialog(
    title = "Esta es una ventana de diálogo",
    h4("Lee este mensaje antes de empezar a usar la aplicación."),
    tags$img(src = "https://i0.wp.com/www.printmag.com/wp-content/uploads/2021/02/4cbe8d_f1ed2800a49649848102c68fc5a66e53mv2.gif?fit=476%2C280&ssl=1", width = "25%"),
    size = "l",
    easyClose = FALSE
  ),
  
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ), 
  
  titlePanel(
    div(id="title-background-color",
        h1("Factibilidad de infraestructuras verdes en la Microcuenca del Río Tacubaya", 
           style = 'text-align: center; padding-left: 30px; padding-top: 25px; 
            font-family: Antonio; font-weight: 700'), 
        style = "height: 80px;")
  ),
  
  sidebarPanel(
    id = "sidebar",
    width = 2,
    h3("¿Cómo funciona?", style = 'font-family: Nunito; font-weight: 600;'),
    p("Esta herramienta muestra una selección de tipologías de infraestructura verde para el control de escorrentías, basada en la determinación de variables físicas del territorio, como la tasa de infiltración, la superficie del sitio, la pendiente y la distancia al nivel freático.",
      id = "sidebar-paragraph"),
    p("Para conocer la metodología ", a("click aqui.", href = "https://github.com/EdaliMurillo/factibilidad_inf_verde"), 
      id = "sidebar-paragraph"),
    p("Para conocer el código del visualizador ", a("click aquí.", href = "https://github.com/EdaliMurillo/factibilidad_inf_verde"), 
      id = "sidebar-paragraph"),
    div(
      style = 'font-family: Nunito; font-weight: 600;',
      checkboxGroupInput(
        "checkGroupTipologia",
        h3("Filtrar por tipología"),
        choices = lista_tipologias
      )
    ),
    
    div(
      style = 'font-family: Nunito; font-weight: 600;',
      actionButton("restaura_areas_verdes",
                   label = "Limpiar selección",
                   width = "100%")
    ),
    
    br(),
    div(align = "center",
      downloadButton("download_ficha", "Descargar ficha")
    )
    
  ), 
  
  mainPanel(width = 10, 
            
    div( # Texto introductorio
      h2("Mapa interactivo de selección de áreas verdes", 
         style = 'font-family: Nunito; font-weight: 600;'),
      p("Explora el mapa haciendo click en el polígono de área verde de tu interés para observar las características del sitio y las tipologías de infraestructura verde que son más adecuadas para intervenir.", 
        style = "text-align: justify; font-size: 18px; font-family: Nunito; font-weight: 600;"),
      p("También puedes filtrar tu selección por tipología, en el panel del lado izquierdo.", 
        style = "text-align: justify; font-size: 18px; font-family: Nunito; font-weight: 600;")
      ),
    div( # Mapa con áreas verdes
      h3("Microcuenca del Río Tacubaya, Ciudad de México",
         style = "color: #ab94c2; font-family: Nunito; font-weight: 600;"),
      leafletOutput(outputId = "mymap", 
                    height = 500),
      hr(),
      ),
    div(id="title-background-color", 
        h2("Características del sitio",
           style = "text-align: center; padding-top: 25px; 
            font-family: Antonio; font-weight: 500;"),
        style = "height: 80px;"
        ),
      fluidRow(
        br(),
        column(5, align="center",
               uiOutput("precipitacion") # Renderiza la gráfica de la estación)
               ),
        column(5, align="center", 
               uiOutput("zonaCuenca")
               )
        ),
    div(id="title-background-color",
        h2("Tipologías factibles",
           style = "text-align: center; padding-top: 25px; 
            font-family: Antonio; font-weight: 500;"),
        style = "height: 80px;"
        ),
      fluidRow(
        br(),
        fluidRow(
          column(12, 
                 slickR::slickROutput("images", width='100%', height='20px')
                 )
          )
        )
    )
)

server <- function(input, output, session) {
  
  cuenca_limite <- sf::st_read("./data/limite_cuenca/Microcuenca_RT.shp")
  datos_geojson <- rgdal::readOGR("./results/areas_verdes_infraestructuras.geojson")
  df_tipologias <- read.csv("./data/tabla_tipologias.csv")
  
  ### Initialize reactive values
  reactive_values <- reactiveValues(mymap = NULL)
  
  rv_shape <- reactiveVal(FALSE) # whether a click happened on polygon
  rv_location <- reactiveValues(id=NULL,lat=NULL,lng=NULL) # the location of mouse event
  rv_location_move_old <- reactiveValues(lat=NULL,lng=NULL) # store the previous location of mouseover event to check if moving out to the background map
  
  ### Output ### Esto es lo que se manda a la interfaz de usuario (UI)
  output$mymap <- renderLeaflet({
    
    reactive_values$mymap <- reactive_values %>% 
      leaflet()  %>%
      setView(-99.22683, 19.38413, 13) %>%
      addProviderTiles("CartoDB.VoyagerLabelsUnder",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = cuenca_limite, color = "#b091d4", weight = 1, 
            smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.4) %>%
      addPolygons(data = datos_geojson, color = "#73af37", weight = 1, 
                  smoothFactor = 0.5, opacity = 1.0, fillOpacity = 1, 
                  fillColor = "#73af37", 
                  group = "myGroup",
                  highlightOptions = highlightOptions(
                    color = "white",  weight = 2, bringToFront = TRUE),
                  layerId = datos_geojson$id,
                  popup = ~as.character(paste0("ID: ", id, "<br/>", 
                                               "Categoría: ", categoria, "<br/>",
                                               "Subcategoría: ", subcat, "<br/>",
                                               "Pendiente: ", pendiente, "%", "<br/>",
                                               "Infiltración: ", infiltr, "<br/>",
                                               "Nivel freático: ", niv_freati, " metros", "<br/>"
                                               )))
    })
  
  ### Inicio de búsqueda de las imágenes de tipologías
  imagen_tipologia = eventReactive(input$mymap_shape_click, {
    req(input$mymap_shape_click$id)
    
    # Aquí le digo que espere a que haya un click en un polígono
    mymap_shape_click_info <- input$mymap_shape_click
    rv_location$id_tipologia <- as.integer(paste(
      datos_geojson[datos_geojson@data$id == mymap_shape_click_info$id, ]$inf_factible))
    
    nombre_archivo_tipologia <- df_tipologias %>% 
      filter(id_tipologia %in% rv_location$id_tipologia) %>% 
      .$nombre_archivo_imagen
    descripcion_tipologia <- df_tipologias %>% 
      filter(id_tipologia %in% rv_location$id_tipologia) %>% 
      .$descripcion
    nombre_tipologia <- df_tipologias %>% 
      filter(id_tipologia %in% rv_location$id_tipologia) %>% 
      .$nombre_descriptivo
    
    dat <- data.frame(
      archivo = nombre_archivo_tipologia,
      descripcion = descripcion_tipologia,
      nombre = nombre_tipologia
      )
    })
  
  ### Inicio de los elementos de la sección de Características
  ## Inicio Gráficas de precipitación
  get_path_grafica_precipitacion <- reactive({
    req(input$mymap_shape_click$id)
    
    id_poligono <- input$mymap_shape_click$id # Extraigo el id del polígono según la selección del mapa
    id_estacion <- estaciones_sf %>% filter(id == id_poligono) %>% .$id_esta # Con el id del polígono extraigo el id de la estación
    nombre_archivo_estacion <- estaciones_archivos %>% filter(id_esta == id_estacion) %>% .$grafica_estacion
    ruta_archivo_estacion <- paste0("./imgs/graficas_estaciones/", nombre_archivo_estacion)
  })
  
  output[["precipitacion"]] <- renderUI({
    tags$img(
      src = get_path_grafica_precipitacion(), 
      width = "75%")
  })
  ## Fin Gráficas de precipitación
  ## Inicio gráficas de zonas de la cuenca
  get_path_graficas_zonas <- reactive({
    req(input$mymap_shape_click$id)
    
    id_poligono <- input$mymap_shape_click$id # Extraigo el id del polígono según la selección del mapa
    id_zona_av <- datos_geojson@data %>% filter(id == id_poligono) %>% .$zona_num %>% unique()
    nombre_archivo_zona <- zonas_archivos %>% filter(id_zona == id_zona_av) %>% .$nombre_imagen
    ruta_archivo_estacion <- paste0("./imgs/graficas_zonas/", nombre_archivo_zona)
  })
  
  output[["zonaCuenca"]] <- renderUI({
    tags$img(
      src = get_path_graficas_zonas(), 
      width = "75%")  
  })
  ## Fin gŕaficas de zonas de la cuenca
  ### Fin de los elementos de la sección de Características
  
  # La salida de la imagen
  output[["images"]] <- slickR::renderSlickR({
    
    titulo_tipologia <- lapply(
      imagen_tipologia()$nombre, function(x) {
      htmltools::tags$p(x, style = htmltools::css(color='black',
                                                  'font-family' = "Nunito",
                                                  'font-size' = "28px"))
    })
    slick_nombre_tipologia <- slickR::slickR(
      titulo_tipologia,
      slideType = 'p',
      width = "80%") + slickR::settings(
        autoplay = TRUE, autoplaySpeed = 5000, 
        centerMode = TRUE, arrows = FALSE)
    
    slick_imagen_tipologia <- slickR::slickR(
      paste0("./imgs/", imagen_tipologia()$archivo),
      height = "600px",
      width = "100%") + slickR::settings(
        autoplay = TRUE, autoplaySpeed = 5000, 
        centerMode = TRUE, arrows = TRUE)
    
    descripcion <- lapply(
      imagen_tipologia()$descripcion, function(x) {
        htmltools::tags$p(x, width = "50%",
                          style = htmltools::css(color='black',
                                                 'font-family' = "Nunito",
                                                 'font-size' = "20px",
                                                 'border' = "blue",
                                                 ))
      })
    slick_descripcion_tipologia <- slickR::slickR(
      obj = descripcion,
      slideType = "p",
      width = "50%") + slickR::settings(
        autoplay = TRUE, autoplaySpeed = 5000, 
        centerMode = TRUE, arrows = FALSE)
    
    carousel <- slick_nombre_tipologia %synch% 
      slick_imagen_tipologia %synch% 
      slick_descripcion_tipologia
        
      # )  %stack% ( 
      #    + slickR::settings(centerMode = TRUE, arrows = FALSE, )
      #   )
    })
  ### Fin de búsqueda de las imágenes de tipologías
  
  ### Inicio de la descarga de las fichas de las tipologías
  output$download_ficha <- downloadHandler(
    filename = function() {
      "ficha-tecnica.png"
    },
    content = function(file) {
      file.copy(c("www/fichas_tecnicas/biofiltro.png"), file)
      }
    )
  ### Fin de la descarga de las fichas de las tipologías
  
  #### observe mouse events ####
  ## when any click happens, identify clicks on map and log new location info
  observeEvent(input$mymap_shape_click, {
    mymap_shape_click_info <- input$mymap_shape_click
    print(paste0("Shape click: ", mymap_shape_click_info))
    mymap_click_info <- input$mymap_click
    print(paste0("Click info: ", "<br/>", mymap_click_info))
    
    rv_location$id_poligono <-
      mymap_shape_click_info$id # take the second part which is county name
    rv_location$id_tipologia <-
      as.integer(paste(datos_geojson[datos_geojson@data$id == rv_location$id_poligono,]$inf_factible))
    
    rv_location$lat <- round(mymap_shape_click_info$lat, 4)
    rv_location$lng <- round(mymap_shape_click_info$lng, 4)
    
    
    if (is.null(mymap_shape_click_info)) {
      # this happens when there hasn't been any click on polygons -> no shape click
      rv_shape(FALSE)
      rv_location$lat <-
        paste(rv_location$lat, '(outside of the state)')
      rv_location$lng <-
        paste(rv_location$lng, '(outside of the state)')
    } else if (!all(unlist(mymap_shape_click_info[c('lat', 'lng')]) == unlist(mymap_click_info[c('lat', 'lng')]))) {
      # this happens when there has been click on polygon
      rv_shape(FALSE)
      rv_location$lat <-
        paste(rv_location$lat, '(outside of the state)')
      rv_location$lng <-
        paste(rv_location$lng, '(outside of the state)')
    } else{
      rv_shape(TRUE)
    }
    
  })
  
  ### Inicio filtro de las tipologías en los check box
  filtro_tipologia <- reactive({
    # shinyjs::logjs(input$checkGroupTipologia)
    datos_filtrados <- datos_geojson[datos_geojson@data$inf_factible %in% as.integer(input$checkGroupTipologia), ]
  })

  observeEvent(input$checkGroupTipologia, {
    
    leafletProxy(mapId = "mymap", data = filtro_tipologia()) %>%
        setView(-99.22683, 19.38413, 13) %>%
        clearGroup ("myGroup") %>% 
        addProviderTiles("CartoDB.VoyagerLabelsUnder",
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addPolygons(data = filtro_tipologia(), color = "#73af37", weight = 1, 
                    smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.7, 
                    fillColor = "#73af37", 
                    group = "myGroup",
                    highlightOptions = highlightOptions(
                      color = "white",  weight = 2, bringToFront = TRUE),
                    layerId = filtro_tipologia()$id,
                    popup = ~as.character(
                      paste0(
                        "ID: ", id, "<br/>", 
                        "Categoría: ", categoria, "<br/>",
                        "Subcategoría: ", subcat)
                      )
                    )
  })
  ### Fin filtro de las tipologías en los check box
  
  ### Inicio del evento para restaurar los polígonos en el mapa cuando se usan los checkbox
  observeEvent(input$restaura_areas_verdes, {
    ## Aquí se limpia la selección de las tipologías en los checkbox
    updateCheckboxGroupInput(session, inputId = "checkGroupTipologia",
                             choices = lista_tipologias)
    
    leafletProxy(mapId = "mymap", data = datos_geojson) %>%
      setView(-99.22683, 19.38413, 13) %>%
      clearGroup ("myGroup") %>% 
      addProviderTiles("CartoDB.VoyagerLabelsUnder",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = datos_geojson, color = "#73af37", weight = 1, 
                  smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.7, 
                  fillColor = "#73af37", 
                  group = "myGroup",
                  highlightOptions = highlightOptions(
                    color = "white",  weight = 2, bringToFront = TRUE),
                  layerId = datos_geojson$id,
                  popup = ~as.character(
                    paste0(
                      "ID: ", id, "<br/>", 
                      "Categoría: ", categoria, "<br/>",
                      "Subcategoría: ", subcat)
                  )
      )
  })
  ### Fin del evento para restaurar los polígonos en el mapa cuando se usan los checkbox
}

shinyApp(ui, server)