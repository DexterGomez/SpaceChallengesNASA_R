

library(shiny)
library(leaflet)
library(leaflet.extras)
library(rstac)
library(sf)
library(terra)
library(dplyr)
library(purrr)
library(DT)
library(ggplot2)

# Define la interfaz de usuario
ui <- fluidPage(
    titlePanel("STAC Landsat Data Downloader"),
    
    tabsetPanel(
        tabPanel("Visualización de Datos",
                 sidebarLayout(
                     sidebarPanel(
                         textInput("api_url", "STAC API URL:", value = "https://planetarycomputer.microsoft.com/api/stac/v1"),
                         dateInput("start_date", "Fecha de Inicio:", value = Sys.Date() - 30),
                         dateInput("end_date", "Fecha de Fin:", value = Sys.Date()),
                         sliderInput("cloud_cover", "Máxima Nubosidad (%)", min = 0, max = 100, value = 20),
                         actionButton("search", "Buscar Datos"),
                         downloadButton("download", "Descargar Datos"),
                         br(), br(),
                         selectInput("select_image", "Seleccione una imagen para visualizar:", choices = NULL),
                         width = 3
                     ),
                     
                     mainPanel(
                         fluidRow(
                             column(
                                 width = 6,
                                 leafletOutput("map", height = "500px"),
                                 verbatimTextOutput("selected_location"),
                                 br(),
                                 h4("Resultados de la Búsqueda"),
                                 DTOutput("results")
                             ),
                             column(
                                 width = 6,
                                 wellPanel(
                                     h4("Imagen Seleccionada"),
                                     uiOutput("image_viewer")
                                 )
                             )
                         )
                     )
                 )
        ),
        tabPanel("Combinación de Bandas",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Combinar Bandas de Landsat"),
                         selectInput("band_1", "Seleccione la Banda 1:", choices = NULL),
                         selectInput("band_2", "Seleccione la Banda 2:", choices = NULL),
                         selectInput("band_3", "Seleccione la Banda 3:", choices = NULL),
                         actionButton("combine_bands", "Combinar Bandas"),
                         downloadButton("download_combined", "Descargar Combinación"),
                         width = 3
                     ),
                     mainPanel(
                         plotOutput("combined_image", height = "600px"),
                         plotOutput("spectral_plot", height = "300px"),
                         downloadButton("download_spectral_data", "Descargar Datos Espectrales")
                     )
                 )
        )
    )
)

# Define la lógica del servidor
server <- function(input, output, session) {
    
    # Valores reactivos para almacenar la geometría seleccionada y los resultados de búsqueda
    values <- reactiveValues(
        geometry = NULL,
        stac_results = NULL,
        selected_image_url = NULL,
        selected_bands = list(),
        spectral_data = NULL,
        combined_image_data = NULL
    )
    
    # Renderiza el mapa Leaflet
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = -89.62, lat = 20.97, zoom = 6) %>%
            addDrawToolbar(
                targetGroup = "draw",
                rectangleOptions = TRUE,
                polylineOptions = FALSE,
                circleOptions = FALSE,
                polygonOptions = TRUE,
                markerOptions = TRUE,
                editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
            )
    })
    
    # Captura el evento del dibujo de un polígono en el mapa
    observeEvent(input$map_draw_new_feature, {
        feature <- input$map_draw_new_feature
        if (feature$geometry$type == "Polygon") {
            coords <- feature$geometry$coordinates[[1]]
            polygon <- st_sfc(st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE))), crs = 4326)
            values$geometry <- st_sf(geometry = polygon)
        }
    })
    
    # Mostrar la ubicación seleccionada
    output$selected_location <- renderPrint({
        if (!is.null(values$geometry)) {
            print(st_as_text(st_geometry(values$geometry)))
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
            limit = 10
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
        
        # Almacenar los resultados
        values$stac_results <- map_dfr(results, ~ {
            data.frame(
                ID = .x$id,
                Cloud_Cover = if (!is.null(.x$properties$`eo:cloud_cover`)) .x$properties$`eo:cloud_cover` else NA,
                URL = if (!is.null(.x$assets$`rendered_preview`)) .x$assets$`rendered_preview`$href else NA,
                BANDS = paste(names(.x$assets), collapse = ", ")
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
                        paste0('<a href="', URL, '" target="_blank">Ver Imagen</a> | <button onclick="navigator.clipboard.writeText(\'', URL, '\');">Copiar URL</button>'),
                        "No disponible"
                    )),
                escape = FALSE,
                options = list(pageLength = 5, autoWidth = TRUE)
            )
        })
    })
    
    # Mostrar la imagen seleccionada en el panel de imagen
    observeEvent(input$select_image, {
        req(input$select_image)
        
        # Obtener la URL de la imagen seleccionada
        selected_image <- values$stac_results %>% filter(ID == input$select_image)
        selected_image_url <- selected_image$URL
        
        # Obtener las bandas disponibles de la imagen seleccionada
        bands <- strsplit(selected_image$BANDS, ", ")[[1]]
        
        # Actualizar las opciones de selección de bandas
        updateSelectInput(session, "band_1", choices = bands)
        updateSelectInput(session, "band_2", choices = bands)
        updateSelectInput(session, "band_3", choices = bands)
        
        # Verificar si la URL es válida
        if (!is.na(selected_image_url)) {
            # Mostrar la imagen en el panel de imagen
            output$image_viewer <- renderUI({
                tags$img(src = selected_image_url, width = "100%")
            })
        }
    })
    
    # Mostrar la combinación de bandas seleccionadas
    observeEvent(input$combine_bands, {
        req(input$band_1, input$band_2, input$band_3, input$select_image)
        
        # Obtener las URLs de las bandas seleccionadas
        selected_image <- values$stac_results %>% filter(ID == input$select_image)
        band_names <- c(input$band_1, input$band_2, input$band_3)
        band_urls <- map(band_names, ~ {
            asset <- selected_image$BANDS %>% strsplit(", ") %>% unlist()
            url_index <- which(asset == .x)
            if (length(url_index) > 0) {
                return(selected_image[[paste0("URL_", .x)]]$href)
            } else {
                return(NULL)
            }
        })
        
        # Descargar y combinar las bandas usando terra
        valid_band_urls <- band_urls[!sapply(band_urls, is.null)]
        if (length(valid_band_urls) == 3) {
            bands <- lapply(valid_band_urls, function(url) {
                tryCatch({
                    rast(url)  # `rast` de `terra` puede leer archivos raster desde URLs directamente
                }, error = function(e) {
                    NULL
                })
            })
            bands <- bands[!sapply(bands, is.null)]
            if (length(bands) == 3) {
                combined_raster <- rast(bands)
                values$combined_image_data <- combined_raster
                
                # Mostrar la imagen combinada en un panel separado
                output$combined_image <- renderPlot({
                    req(values$combined_image_data)
                    plotRGB(values$combined_image_data, r = 1, g = 2, b = 3, main = paste("Combinación de Bandas:", input$band_1, input$band_2, input$band_3))
                })
            } else {
                showNotification("No se pudieron descargar todas las bandas seleccionadas.", type = "error")
            }
        } else {
            showNotification("No se pudieron obtener todas las URLs de las bandas seleccionadas.", type = "error")
        }
    })
    
    # Renderizar la gráfica de los datos espectrales
    output$spectral_plot <- renderPlot({
        req(values$spectral_data)
        ggplot(values$spectral_data, aes(x = Wavelength, y = Reflectance, color = Point)) +
            geom_line() +
            theme_minimal() +
            labs(title = "Valores Espectrales", x = "Longitud de Onda (nm)", y = "Reflectancia")
    })
    
    # Descargar datos seleccionados
    output$download <- downloadHandler(
        filename = function() {
            paste("landsat_data_", Sys.Date(), ".geojson", sep = "")
        },
        content = function(file) {
            if (!is.null(values$geometry)) {
                st_write(values$geometry, file, driver = "GeoJSON")
            }
        }
    )
    
    # Descargar combinación de bandas
    output$download_combined <- downloadHandler(
        filename = function() {
            paste("landsat_combined_bands_", Sys.Date(), ".tif", sep = "")
        },
        content = function(file) {
            if (!is.null(values$combined_image_data)) {
                writeRaster(values$combined_image_data, file, format = "GTiff")
            }
        }
    )
    
    # Descargar datos espectrales
    output$download_spectral_data <- downloadHandler(
        filename = function() {
            paste("spectral_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(values$spectral_data, file, row.names = FALSE)
        }
    )
}

# Ejecuta la aplicación Shiny
shinyApp(ui = ui, server = server)