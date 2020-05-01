# use official R image and add a label
FROM r-base:3.5.1
LABEL maintainer="Amir Abu Jandal <jandal487@gmail.com>"

# refresh apt-get
RUN apt-get update  \
    && apt-get install -yq --no-install-recommends groff \
    && rm -rf /var/lib/apt/lists/*

# create dir
RUN mkdir /home/covid19
RUN mkdir -p /home/covid19/R
RUN mkdir -p /home/covid19/data

# copy files to dir
#COPY ./* /home/covid19/
COPY /R/covid19.r /home/covid19/R/
COPY /R/install_packages.r /home/covid19/R/
COPY /data/covid_19_data.csv /home/covid19/data/

# install R-packages
RUN Rscript /home/covid19/R/install_packages.r

# run whenever the container is launched
CMD R -e "source('/home/covid19/R/covid19.r')"

## exporting result of container execution
# create dir on host to share results
#mkdir ~/docker_results/covid19/results

# use -v (volume) to make it persistent for container
#docker run -v ~/docker_results/covid19/results:/home/covid19/results img_covid19
