# Methodology Dashboard

Methodology Dashboard is a web application which visualizes the overall project
progress based on data which is extracted from OpenAir, LabCase and ARIS BS.

Methodology Dashboard consists of two parts:

* A separate __R script__ for loading and processing the data. The script creates
several .csv files which are consumed by the UI layer for visualization purposes
(The script needs to be set up as a Task (Windows))
* A __MashZone application__ visualizing the processed data

Only the first part of the application (R script) is part of the repository.

The relevant ARIS BS data is provided by the server via a JavaScript report. The report is also part of this repository

## Installation

* [R 3.1.2](http://www.r-project.org)
* You will need the following R libraries:
  * RCurl
  * httr
  * XML
  * RSelenium
  * rvest
  * lubridate
  * yaml
  * xlsx
  * reshape2
  * dplyr
  * magrittr
* [Java SE 7](http://www.java.com/en/): the xlsx package depends on Apache POI


The following are installation instructions for Windows:

* You have to set up the following folder app structure under the `resources` folder
of your ARIS MashZone installation:
      * `prime dashboard`: Not included in Git repo. Place the entire repository content here. Only exclude the jsScript folder. In addition you have to create the following folders below the `prime dashboard` folder
      * `rawData`: Downloaded data will get stored here
      * `rOutput`: Processed data wil get stored here
* The path in the `setwd()` command in `main.R` needs to be changed accordingly
* `phantomjs` setup:
      * Download the relevant zip file form [here](http://phantomjs.org/download.html)
      * Extract the zip file in a location of your choice
      * Place the location of the directory containing `phantomjs.exe` in the user path
* Moreover you have to add the necessary credentials and urls to the `config.yml` file
* Set up a Scheduled Task (Windows) using `[path to your R installation]/R/R-3.1.2/bin/x64/Rscript.exe` with the following argument `[your path to]/resources/prime dashboard/rScripts/main.R` 