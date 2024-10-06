# Base R Shiny image
FROM rocker/shiny


#FROM hvalev/shiny-server-arm:latest

#RUN apt-get update
#RUN apt-get install -y r-base

# Make a directory in the container
RUN mkdir /home/shiny-app

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
CMD Rscript /home/shiny-app/app.R