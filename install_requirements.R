required_packages <- c(
    "rstac",
    "shiny",
    "shinythemes",
    "shinyjs",
    "leaflet",
    "leaflet.extras",
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
    "lubridate",
    "httr",  # Para realizar solicitudes HTTP a la API de seguimiento
    "jsonlite"  # Para procesar las respuestas en formato JSON
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