# Base R Shiny image
FROM ubuntu:24.04

RUN apt-get update
RUN apt-get install -y r-base libgdal-dev libudunits2-dev

# Make a directory in the container
RUN mkdir /home/shiny-app

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R
COPY install_requirements.R /home/shiny-app/install_requirements.R

# Install requirements
RUN Rscript /home/shiny-app/install_requirements.R

# Expose the application port
EXPOSE 8080

CMD Rscript -e "shiny::runApp('/home/shiny-app/app.R', host='0.0.0.0', port=8080)"