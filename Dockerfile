# Base image https://hub.docker.com/u/rocker/

FROM rocker/shiny:3.6.3

# system libraries of general use - install debian packages

RUN apt-get --allow-releaseinfo-change update 
RUN apt-get -y --no-install-recommends install libssl-dev
RUN apt-get -y --no-install-recommends install libcurl4-openssl-dev
RUN apt-get -y --no-install-recommends install libxml2-dev
RUN apt-get -y --no-install-recommends install libssh2-1-dev
RUN apt-get -y --no-install-recommends install libxt-dev

# copy necessary files

# COPY shiny_renv.lock renv.lock
COPY renv.lock renv.lock

# install renv & restore packages

RUN Rscript -e "install.packages('renv')"
RUN Rscript -e "renv::isolate()"
RUN Rscript -e "renv::restore()"
COPY . /srv/shiny-server/

# expose port

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
