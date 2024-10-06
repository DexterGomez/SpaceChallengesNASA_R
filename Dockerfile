# Base R Shiny image
FROM rocker/shiny


#FROM hvalev/shiny-server-arm:latest

#RUN apt-get update
#RUN apt-get install -y r-base

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN Rscript -e 'install.packages(c("leaflet", "leaflet.extras", "rstac", "sf", "terra", "dplyr", "purrr", "DT", "ggplot2", "rmarkdown", "htmlwidgets", "tidyr", "ggrepel", "shinyjs", "lubridate"), repos = "https://cran.rstudio.com/", dependencies = TRUE)'

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD Rscript /home/shiny-app/app.R