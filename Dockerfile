# alpine-python-ecmwfapi
FROM rocker/tidyverse:3.6.0

MAINTAINER zappingseb "sebastian@mail-wolf.de"

WORKDIR /usr/src/app

RUN R -e "install.packages(c('RSelenium'), repos='https://cran.rstudio.com/') "

RUN apt-get update -qq \
  && apt-get install -y \
  python-pip \
  vim

RUN pip install pytest-shutil
RUN pip install --upgrade numpy secure-smtplib email

COPY run_tests.R /tmp/run_tests.R
COPY sendmail.py /tmp/sendmail.py

RUN apt-get update && apt-get -y install cron
RUN echo "0 */12 * * * root Rscript /tmp/run_tests.R" >> /etc/crontab
RUN service cron start
