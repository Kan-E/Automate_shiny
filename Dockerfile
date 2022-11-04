FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y \
    build-essential \
    libglpk40 \
    libbz2-dev \
    liblzma-dev \
    libgsl-dev \
    r-cran-gsl

RUN R -e "install.packages('BiocManager')" && R -e "BiocManager::install('shiny', update = F)"  && R -e "BiocManager::install('gdata', update = F)" && R -e "BiocManager::install('rstatix', update = F)" && R -e "BiocManager::install('multcomp', update = F)" && R -e "BiocManager::install('ggpubr', update = F)" && R -e "BiocManager::install('shinyBS', update = F)" && R -e "BiocManager::install('DT', update = F)"
RUN sudo rm -rf /srv/shiny-server/sample-apps /srv/shiny-server/index.html /srv/shiny-server/01_hello /srv/shiny-server/02_text /srv/shiny-server/03_reactivity /srv/shiny-server/04_mpg /srv/shiny-server/05_sliders /srv/shiny-server/06_tabsets /srv/shiny-server/07_widgets /srv/shiny-server/08_html /srv/shiny-server/09_upload /srv/shiny-server/10_download /srv/shiny-server/11_timer
COPY /automate_docker /srv/shiny-server/
COPY shiny-server_forAutomate.conf /etc/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server
EXPOSE 3838
CMD exec shiny-server >> /var/log/shiny-server.log 2>&1

