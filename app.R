# Verificar e instalar paquetes necesarios
required_packages <- c(
    "shiny",
    "shinythemes",
    "leaflet",
    "leaflet.extras"
    "rstac",
    "sf",
    "terra",
    "dplyr",
    "purrr",
    "DT",
    "ggplot2",
    "rmarkdown",
    "htmlwidgets",
    "tidyr",
    "ggrepel",
    "shinyjs",
    "lubridate"  # Para manejar fechas y tiempos
)

install_if_missing <- function(packages) {
    installed_packages <- installed.packages()[,"Package"]
    for (pkg in packages) {
        if (!pkg %in% installed_packages) {
            message(paste("Instalando paquete:", pkg))
            install.packages(pkg, dependencies = TRUE)
        }
        library(pkg, character.only = TRUE)
    }
}

install_if_missing(required_packages)

options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Define la interfaz de usuario
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Visualización Satelital y Análisis de Imágenes"),
    
    tabsetPanel(
        tabPanel("Visualización de Datos",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Parámetros de Búsqueda"),
                         p("Ingrese los parámetros para buscar imágenes satelitales."),
                         textInput("api_url", "STAC API URL:", value = "https://planetarycomputer.microsoft.com/api/stac/v1"),
                         dateInput("start_date", "Fecha de Inicio:", value = Sys.Date() - 30),
                         dateInput("end_date", "Fecha de Fin:", value = Sys.Date()),
                         sliderInput("cloud_cover", "Máxima Nubosidad (%)", min = 0, max = 100, value = 20),
                         actionButton("search", "Buscar Datos", class = "btn btn-primary"),
                         downloadButton("download", "Descargar Geometría Seleccionada"),
                         br(), br(),
                         width = 3
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Mapa General",
                                      h4("Instrucciones"),
                                      p("Utilice las herramientas de dibujo en el mapa para seleccionar una área de interés o marcar su ubicación. Después, haga clic en 'Buscar Datos' para obtener imágenes satelitales disponibles en esa área."),
                                      leafletOutput("map", height = "500px"),
                                      verbatimTextOutput("selected_location"),
                                      br(),
                                      h4("Resultados de la Búsqueda"),
                                      DTOutput("results")
                             ),
                             tabPanel("Imagen Seleccionada",
                                      h4("Instrucciones"),
                                      p("Seleccione una imagen de la lista desplegable para visualizarla en el mapa. Puede agregar marcadores en la imagen para analizar los valores espectrales en esos puntos."),
                                      selectInput("select_image", "Seleccione una imagen para visualizar:", choices = NULL),
                                      leafletOutput("selected_image_map", height = "500px"),
                                      br(),
                                      h4("Valores Espectrales en los Puntos Seleccionados"),
                                      plotOutput("band_analysis_plot"),
                                      downloadButton("download_graph", "Descargar Gráfico"),
                                      downloadButton("download_data", "Descargar Datos de Puntos")
                             )
                         )
                     )
                 )
        ),
        tabPanel("Análisis Temporal",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Análisis Temporal de un Punto"),
                         p("Seleccione un punto en el mapa para analizar cómo varían los valores espectrales a lo largo del tiempo en ese punto."),
                         dateInput("temporal_start_date", "Fecha de Inicio:", value = Sys.Date() - 365),
                         dateInput("temporal_end_date", "Fecha de Fin:", value = Sys.Date()),
                         sliderInput("temporal_cloud_cover", "Máxima Nubosidad (%)", min = 0, max = 100, value = 20),
                         actionButton("analyze_point", "Analizar Punto", class = "btn btn-primary"),
                         width = 3
                     ),
                     mainPanel(
                         h4("Mapa de Selección de Punto"),
                         p("Haga clic en el mapa para seleccionar el punto de interés."),
                         leafletOutput("temporal_map", height = "500px"),
                         br(),
                         h4("Resultados del Análisis Temporal"),
                         plotOutput("temporal_plot"),
                         DTOutput("temporal_data_table"),
                         downloadButton("download_temporal_data", "Descargar Datos")
                     )
                 )
        ),
        tabPanel("Acerca del Proyecto",
                 h3("Visualización Satelital y Análisis de Imágenes"),
                 p("Esta aplicación permite a los usuarios buscar y analizar imágenes satelitales de Landsat 8 a través de la interfaz STAC API."),
                 h4("Instrucciones Generales"),
                 p("1. En la pestaña 'Visualización de Datos', utilice el mapa para dibujar un polígono o marcar su ubicación de interés."),
                 p("2. Ajuste los parámetros de búsqueda según sus necesidades y haga clic en 'Buscar Datos'."),
                 p("3. Revise los resultados de la búsqueda y seleccione una imagen para visualizar."),
                 p("4. En la pestaña 'Imagen Seleccionada', puede agregar marcadores para analizar los valores espectrales."),
                 p("5. En la pestaña 'Análisis Temporal', seleccione un punto y un rango de fechas para analizar la variación temporal de los valores espectrales."),
                 h4("Citas y Referencias"),
                 p("Esta aplicación utiliza los siguientes recursos y bibliotecas:"),
                 tags$ul(
                     tags$li("Paquete R 'shiny' para la creación de aplicaciones web interactivas."),
                     tags$li("Paquete R 'leaflet' para mapas interactivos."),
                     tags$li("Paquete R 'rstac' para interactuar con STAC API."),
                     tags$li("Datos satelitales proporcionados por el Programa Landsat de la NASA y el Servicio Geológico de los Estados Unidos (USGS).")
                 ),
                 p("Cualquier uso de los datos debe cumplir con las políticas de licencia y uso de los proveedores de datos."),
                 h4("Contacto"),
                 p("Para más información, puede contactar al desarrollador del proyecto.")
        )
    )
)

# Define la lógica del servidor
server <- function(input, output, session) {
    # Valores reactivos para almacenar la geometría seleccionada y los resultados de búsqueda
    values <- reactiveValues(
        geometry = NULL,
        stac_results = NULL,
        stac_items = NULL,
        selected_image_url = NULL,
        spectral_data = data.frame(),  # Para almacenar los valores espectrales de todos los marcadores
        user_location = NULL,
        clipped_images = list(),
        marker_id = 0,  # Contador para identificar los marcadores
        markers = list(),
        # Para análisis temporal
        temporal_point = NULL,
        temporal_data = NULL
    )
    
    # Renderiza el mapa base con leaflet para la pestaña "Visualización de Datos"
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -89.62, lat = 20.97, zoom = 6) %>%
            addDrawToolbar(
                targetGroup = "draw",
                rectangleOptions = TRUE,
                polylineOptions = FALSE,
                circleOptions = FALSE,
                polygonOptions = TRUE,
                markerOptions = TRUE,
                editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
            ) %>%
            addLayersControl(
                overlayGroups = c("draw"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    
    # Captura el evento del dibujo de un polígono o marcador en el mapa general
    observeEvent(input$map_draw_new_feature, {
        feature <- input$map_draw_new_feature
        if (feature$geometry$type == "Polygon") {
            coords <- feature$geometry$coordinates[[1]]
            polygon <- st_sfc(st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE))), crs = 4326)
            values$geometry <- st_sf(geometry = polygon)
        } else if (feature$geometry$type == "Point") {
            lng <- feature$geometry$coordinates[[1]]
            lat <- feature$geometry$coordinates[[2]]
            values$user_location <- c(lat, lng)
            # Añadir marcador al mapa
            leafletProxy("map") %>%
                addMarkers(
                    lng = lng, lat = lat,
                    label = "Ubicación del Usuario",
                    labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
                )
        }
    })
    
    # Mostrar la ubicación seleccionada
    output$selected_location <- renderPrint({
        if (!is.null(values$geometry)) {
            print(st_as_text(st_geometry(values$geometry)))
        } else if (!is.null(values$user_location)) {
            cat("Ubicación del Usuario: Latitud =", values$user_location[1], ", Longitud =", values$user_location[2])
        } else {
            "Seleccione una ubicación en el mapa."
        }
    })
    
    # Buscar datos en la API STAC
    observeEvent(input$search, {
        req(values$geometry)
        
        # Crear el bounding box (bbox) para la búsqueda
        bbox <- st_bbox(values$geometry)
        bbox_str <- paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ",")
        
        # Definir la URL de búsqueda y parámetros
        stac_source <- rstac::stac(input$api_url)
        
        # Buscar en el STAC API con los parámetros proporcionados
        search_query <- rstac::stac_search(
            q = stac_source,
            collections = "landsat-c2-l2",
            bbox = bbox_str,
            datetime = paste(input$start_date, input$end_date, sep = "/"),
            limit = 100
        ) %>%
            rstac::get_request()
        
        # Filtrar los resultados por nubosidad si la propiedad existe
        results <- search_query$features %>%
            keep(~ {
                if (!is.null(.x$properties$`eo:cloud_cover`)) {
                    .x$properties$`eo:cloud_cover` <= input$cloud_cover
                } else {
                    FALSE
                }
            })
        
        # Almacenar los ítems completos del STAC
        values$stac_items <- results
        
        # Crear un data frame para mostrar en la tabla
        values$stac_results <- map_dfr(results, ~ {
            data.frame(
                ID = .x$id,
                Fecha = as.Date(.x$properties$`datetime`),
                Nubosidad = if (!is.null(.x$properties$`eo:cloud_cover`)) .x$properties$`eo:cloud_cover` else NA,
                URL = if (!is.null(.x$assets$`rendered_preview`)) .x$assets$`rendered_preview`$href else NA
            )
        })
        
        # Actualizar las opciones del selector con los IDs de las imágenes encontradas
        updateSelectInput(session, "select_image", choices = values$stac_results$ID)
        
        # Mostrar los resultados en una tabla interactiva usando DT
        output$results <- renderDT({
            datatable(
                values$stac_results %>%
                    mutate(URL = ifelse(
                        !is.na(URL),
                        paste0('<a href="', URL, '" target="_blank">Ver Imagen</a>'),
                        "No disponible"
                    )),
                escape = FALSE,
                options = list(pageLength = 5, autoWidth = TRUE),
                rownames = FALSE
            )
        })
    })
    
    # Mostrar la imagen seleccionada en el mapa interactivo usando leaflet
    output$selected_image_map <- renderLeaflet({
        req(input$select_image)
        
        # Reiniciar el contador de marcadores y datos cuando se selecciona una nueva imagen
        values$marker_id <- 0
        values$spectral_data <- data.frame()
        values$markers <- list()
        
        # Obtener el ítem seleccionado del STAC
        selected_item <- values$stac_items %>% keep(~ .x$id == input$select_image) %>% first()
        
        # Obtener la URL de la imagen
        selected_image_url <- selected_item$assets$`rendered_preview`$href
        
        # Verificar si la URL está disponible
        req(selected_image_url)
        
        # Obtener el bounding box de la imagen (xmin, ymin, xmax, ymax)
        selected_item_bbox <- selected_item$bbox
        
        # Crear el mapa leaflet
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            fitBounds(
                lng1 = selected_item_bbox[1], lat1 = selected_item_bbox[2],
                lng2 = selected_item_bbox[3], lat2 = selected_item_bbox[4]
            ) %>%
            addDrawToolbar(
                targetGroup = "draw_selected_image",
                rectangleOptions = TRUE,
                polylineOptions = FALSE,
                circleOptions = FALSE,
                polygonOptions = TRUE,
                markerOptions = TRUE,  # Habilitar la opción de marcador
                editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
            ) %>%
            addLayersControl(
                overlayGroups = c("draw_selected_image"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>%
            onRender(
                "
                function(el, x, data) {
                    var imageUrl = data.url;
                    var imageBounds = [[data.bounds[0][0], data.bounds[0][1]], [data.bounds[1][0], data.bounds[1][1]]];
                    L.imageOverlay(imageUrl, imageBounds).addTo(this);
                }
                ",
                data = list(
                    url = selected_image_url,
                    bounds = list(
                        c(selected_item_bbox[2], selected_item_bbox[1]),
                        c(selected_item_bbox[4], selected_item_bbox[3])
                    )
                )
            )
    })
    
    # Capturar el evento de agregar un marcador en el mapa "selected_image_map"
    observeEvent(input$selected_image_map_draw_new_feature, {
        feature <- input$selected_image_map_draw_new_feature
        if (feature$geometry$type == "Point") {
            lng <- feature$geometry$coordinates[[1]]
            lat <- feature$geometry$coordinates[[2]]
            values$marker_id <- values$marker_id + 1
            marker_id <- as.character(values$marker_id)
            
            # Simular los valores espectrales en ese punto
            spectral_values <- data.frame(
                MarkerID = marker_id,
                Banda = c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7"),
                Valor = runif(7, 0, 1)
            )
            
            # Almacenar los valores espectrales
            values$spectral_data <- bind_rows(values$spectral_data, spectral_values)
            
            # Almacenar la información del marcador
            values$markers[[marker_id]] <- c(lat, lng)
            
            # Añadir el marcador al mapa con etiqueta numérica visible
            leafletProxy("selected_image_map") %>%
                addMarkers(
                    lng = lng, lat = lat,
                    label = marker_id,
                    labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
                )
            
            # Actualizar el gráfico para incluir todos los marcadores
            output$band_analysis_plot <- renderPlot({
                req(nrow(values$spectral_data) > 0)
                ggplot(values$spectral_data, aes(x = Banda, y = Valor, fill = MarkerID)) +
                    geom_bar(stat = "identity", position = "dodge") +
                    labs(
                        title = "Valores espectrales en los puntos seleccionados",
                        x = "Banda",
                        y = "Valor Espectral",
                        fill = "Marcador"
                    ) +
                    theme_minimal()
            })
        }
    })
    
    # Descargar datos seleccionados
    output$download <- downloadHandler(
        filename = function() {
            paste("geometria_seleccionada_", Sys.Date(), ".geojson", sep = "")
        },
        content = function(file) {
            if (!is.null(values$geometry)) {
                st_write(values$geometry, file, driver = "GeoJSON")
            }
        }
    )
    
    # Descargar el gráfico de valores espectrales
    output$download_graph <- downloadHandler(
        filename = function() {
            paste("valores_espectrales_", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            req(nrow(values$spectral_data) > 0)
            # Generar el gráfico y guardarlo en el archivo
            g <- ggplot(values$spectral_data, aes(x = Banda, y = Valor, fill = MarkerID)) +
                geom_bar(stat = "identity", position = "dodge") +
                labs(
                    title = "Valores espectrales en los puntos seleccionados",
                    x = "Banda",
                    y = "Valor Espectral",
                    fill = "Marcador"
                ) +
                theme_minimal()
            ggsave(file, plot = g, device = "png", width = 8, height = 6)
        }
    )
    
    # Descargar los datos de los puntos seleccionados
    output$download_data <- downloadHandler(
        filename = function() {
            paste("datos_puntos_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            req(nrow(values$spectral_data) > 0)
            # Unir los datos de valores espectrales con las coordenadas de los marcadores
            markers_df <- do.call(rbind, lapply(names(values$markers), function(id) {
                data.frame(
                    MarkerID = id,
                    Latitud = values$markers[[id]][1],
                    Longitud = values$markers[[id]][2]
                )
            }))
            data_to_save <- values$spectral_data %>%
                left_join(markers_df, by = "MarkerID")
            # Guardar en un archivo CSV
            write.csv(data_to_save, file, row.names = FALSE)
        }
    )
    
    # -------------------------------------------------------------------------------------
    # Análisis Temporal
    # -------------------------------------------------------------------------------------
    
    # Renderizar el mapa para seleccionar el punto en la pestaña "Análisis Temporal"
    output$temporal_map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -89.62, lat = 20.97, zoom = 6)
    })
    
    # Capturar el clic en el mapa para seleccionar el punto
    observeEvent(input$temporal_map_click, {
        click <- input$temporal_map_click
        values$temporal_point <- data.frame(
            lon = click$lng,
            lat = click$lat
        )
        
        # Añadir marcador al mapa
        leafletProxy("temporal_map") %>%
            clearMarkers() %>%
            addMarkers(
                lng = click$lng, lat = click$lat,
                popup = "Punto Seleccionado"
            )
    })
    
    # Realizar el análisis temporal cuando se hace clic en "Analizar Punto"
    observeEvent(input$analyze_point, {
        req(values$temporal_point)
        
        # Definir el punto como objeto sf
        point_sf <- st_sfc(st_point(c(values$temporal_point$lon, values$temporal_point$lat)), crs = 4326)
        
        # Crear el bbox para la búsqueda (puede ser un pequeño buffer alrededor del punto)
        buffer_dist <- 0.0001  # Aproximadamente 11 metros
        bbox <- st_bbox(st_buffer(point_sf, dist = buffer_dist))
        bbox_str <- paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ",")
        
        # Definir la URL de búsqueda y parámetros
        stac_source <- rstac::stac(input$api_url)
        
        # Buscar en el STAC API con los parámetros proporcionados
        search_query <- rstac::stac_search(
            q = stac_source,
            collections = "landsat-c2-l2",
            bbox = bbox_str,
            datetime = paste(input$temporal_start_date, input$temporal_end_date, sep = "/"),
            limit = 500
        ) %>%
            rstac::get_request()
        
        # Filtrar los resultados por nubosidad
        results <- search_query$features %>%
            keep(~ {
                if (!is.null(.x$properties$`eo:cloud_cover`)) {
                    .x$properties$`eo:cloud_cover` <= input$temporal_cloud_cover
                } else {
                    FALSE
                }
            })
        
        if (length(results) == 0) {
            showNotification("No se encontraron imágenes en el rango de fechas y condiciones especificadas.", type = "error")
            return()
        }
        
        # Extraer los valores de las bandas en el punto para cada imagen
        temporal_data <- map_dfr(results, ~ {
            item <- .x
            # Simular la extracción de valores espectrales en el punto
            # En una implementación real, se descargarían las bandas y se extraerían los valores en el punto
            data.frame(
                Fecha = as.Date(item$properties$`datetime`),
                Banda = c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7"),
                Valor = runif(7, 0, 1)
            )
        })
        
        # Almacenar los datos temporales
        values$temporal_data <- temporal_data
        
        # Generar el gráfico temporal
        output$temporal_plot <- renderPlot({
            req(values$temporal_data)
            ggplot(values$temporal_data, aes(x = Fecha, y = Valor, color = Banda)) +
                geom_line() +
                geom_point() +
                labs(
                    title = "Análisis Temporal de Valores Espectrales",
                    x = "Fecha",
                    y = "Valor Espectral",
                    color = "Banda"
                ) +
                theme_minimal()
        })
        
        # Mostrar los datos en una tabla
        output$temporal_data_table <- renderDT({
            datatable(values$temporal_data, options = list(pageLength = 5))
        })
    })
    
    # Descargar los datos del análisis temporal
    output$download_temporal_data <- downloadHandler(
        filename = function() {
            paste("analisis_temporal_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            req(values$temporal_data)
            write.csv(values$temporal_data, file, row.names = FALSE)
        }
    )
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
