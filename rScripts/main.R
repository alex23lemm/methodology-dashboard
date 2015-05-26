# Purpose: main.R sources load_data.R, process_data.R.
# The downloaded, processed and saved data of that workflow serves as the input 
# for the data visualization on the UI layer.

# Load libraries and utility functions -----------------------------------------

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")


library(RCurl)
library(httr)
library(lubridate)
library(yaml)

library(xlsx)
library(reshape2)
library(dplyr)
#library(RSelenium)
library(magrittr)
library(rvest)
library(XML)

#setwd('C:/Program Files/ARIS MashZone/resources/prime dashboard')

source('./rScripts/download_utils.R')
config <- yaml.load_file('config.yml')

# Download and process data ----------------------------------------------------

source('./rScripts/load_data.R')
source('./rScripts/process_data.R')

rm(list=ls())
