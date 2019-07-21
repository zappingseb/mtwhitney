# alpine-python-ecmwfapi
FROM rocker:tidyverse

MAINTAINER zappingseb "sebastian@mail-wolf.de"

WORKDIR /usr/src/app

RUN R -e "install.packages('RSelenium', repos='https://cran.rstudio.com/') "

COPY /RSelenium.R /tmp/RSelenium.R
COPY /run_test.R /tmp/run_test.R

RUN pip install pytest-shutil
RUN pip install --upgrade google-api-python-client google-auth-httplib2 google-auth-oauthlib

