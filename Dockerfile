# Base R Shiny image
FROM ubuntu:24.04

RUN apt-get update
RUN apt-get install -y r-base libgdal-dev libudunits2-dev

RUN R -e "install.packages(c( \
    'rstac','shiny','shinythemes','shinyjs','leaflet','leaflet.extras',\
    'sf','terra','dplyr','purrr','DT','ggplot2','rmarkdown','htmlwidgets',\
    'tidyr','ggrepel','lubridate','httr','jsonlit'))"

# Make a directory in the container
RUN mkdir /home/shiny-app

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R
#COPY install_requirements.R /home/shiny-app/install_requirements.R

# Install requirements
#RUN Rscript /home/shiny-app/install_requirements.R

# Expose the application port
EXPOSE 8080

CMD Rscript -e "shiny::runApp('/home/shiny-app/app.R', host='0.0.0.0', port=8080)"