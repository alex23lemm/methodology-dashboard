# Purpose: main.R sources load_data.R, process_data.R.
# The downloaded, processed and saved data of that workflow serves as the input 
# for the data visualization on the UI layer.

# Load libraries and utility functions -----------------------------------------


library(RCurl)
library(httr)
library(lubridate)
library(yaml)

library(readxl)
library(reshape2)
library(plyr)
library(dplyr)
#library(RSelenium)
library(magrittr)
library(rvest)
library(XML)
library(stringr)

#setwd("C:/SoftwareAG/ppmmashzone/server/bin/work/work_mashzone_m/mashzone_data/resources/prime dashboard")

source('./rScripts/download_utils.R')
config <- yaml.load_file('config.yml')

# Download and process data ----------------------------------------------------

source('./rScripts/load_data.R')
source('./rScripts/process_data.R')

rm(list=ls())
