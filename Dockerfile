# alpine-python-ecmwfapi
FROM rocker/tidyverse:3.6.0

MAINTAINER zappingseb "sebastian@mail-wolf.de"

WORKDIR /usr/src/app

RUN R -e "install.packages('RSelenium', repos='https://cran.rstudio.com/') "



RUN apt-get update -qq \
  && apt-get install -y \
  python-pip

RUN pip install pytest-shutil
RUN pip install --upgrade numpy google-api-python-client google-auth-httplib2 google-auth-oauthlib

RUN apt-get install -y build-essential chrpath libssl-dev libxft-dev
RUN apt-get install -y libfreetype6 libfreetype6-dev
RUN apt-get install -y libfontconfig1 libfontconfig1-dev
RUN export PHANTOM_JS="phantomjs-1.9.8-linux-x86_64"
RUN wget https://bitbucket.org/ariya/phantomjs/downloads/$PHANTOM_JS.tar.bz2
RUN tar xvjf $PHANTOM_JS.tar.bz2
RUN RUN mv $PHANTOM_JS /usr/local/share
RUN ln -sf /usr/local/share/$PHANTOM_JS/bin/phantomjs /usr/local/bin
RUN phantomjs --webdriver=4444

COPY RSelenium.R /tmp/RSelenium.R
COPY run_tests.R /tmp/run_tests.R


