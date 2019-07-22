# alpine-python-ecmwfapi
FROM rocker/tidyverse:3.6.0

MAINTAINER zappingseb "sebastian@mail-wolf.de"

WORKDIR /usr/src/app

RUN R -e "install.packages(c('RSelenium','mailR'), repos='https://cran.rstudio.com/') "



RUN apt-get update -qq \
  && apt-get install -y \
  python-pip

RUN pip install pytest-shutil
RUN pip install --upgrade numpy smtplib email

RUN apt-get install -y build-essential chrpath libssl-dev libxft-dev
RUN apt-get install -y libfreetype6 libfreetype6-dev
RUN apt-get install -y libfontconfig1 libfontconfig1-dev

RUN wget https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-1.9.8-linux-x86_64.tar.bz2
RUN tar xvjf phantomjs-1.9.8-linux-x86_64.tar.bz2
RUN mv phantomjs-1.9.8-linux-x86_64 /usr/local/share
RUN ln -sf /usr/local/share/phantomjs-1.9.8-linux-x86_64/bin/phantomjs /usr/local/bin

COPY RSelenium.R /tmp/RSelenium.R
COPY run_tests.R /tmp/run_tests.R
COPY sendmail.py /tmp/sendmail.py

RUN apt-get install -y vim
