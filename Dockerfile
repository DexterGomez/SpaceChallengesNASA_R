FROM rocker/shiny:latest

RUN apt-get update && apt-get install libcur14-gnutls-dev libssl-dev

RUN Rscript -e "install.packages(c('leaflet', 'leaflet.extras', 'rstac', 'sf', 'terra', 'dplyr', 'purrr', 'DT', 'ggplot2', 'rmarkdown', 'htmlwidgets', 'tidyr', 'ggrepel', 'shinyjs', 'lubridate'))"

COPY ./app.R /srv/shiny-server/

CMD ["/usr/bin/shiny-server"]