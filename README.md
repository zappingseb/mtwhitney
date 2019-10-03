# Crawling Mt Whitney Permits

![](https://upload.wikimedia.org/wikipedia/commons/thumb/f/f9/Mount_Whitney_2003-03-25.jpg/640px-Mount_Whitney_2003-03-25.jpg)

This repository enables to crawl the slots
on Mount Whitney on its website at: [https://www.recreation.gov/permits/233260/](https://www.recreation.gov/permits/233260)

It follows some steps:

- Crawl the website by selecting the route, number of people, going to month "October"
- Write the available first date into an `output.txt` file
- Send an email with `python` to myself

All steps should run at 12.00 and 00.00 automatically by a `cron`task

## Prequisites

Two containers are needed for the automation task to work. It needs a docker selenium started by:

```
docker run -d -p 4445:4444 --name seleniumcontainer --net mynet selenium/standalone-chrome
```

The name `seleniumcontainer` is necessary for it to work.

The docker for this package is build by:

```
docker build --net mynet --name mtwhitney -f Dockerfile .
```

## Running the script

```
docker run -it mtwhitney /bin/bash
```

inside the container run:

```
sudo Rscript /tmp/run_tests.R
```

disconnect by `Crtl + p` and `Crtl + q`
