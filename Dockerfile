# alpine-python-ecmwfapi
FROM rocker/tidyverse:3.6.0

MAINTAINER zappingseb "sebastian@mail-wolf.de"

WORKDIR /usr/src/app

RUN R -e "install.packages('RSelenium', repos='https://cran.rstudio.com/') "

COPY RSelenium.R /tmp/RSelenium.R
COPY run_tests.R /tmp/run_tests.R


RUN apt-get update -qq \
  && apt-get install -y \
  python-pip

RUN pip install pytest-shutil
RUN pip install --upgrade google-api-python-client google-auth-httplib2 google-auth-oauthlib
