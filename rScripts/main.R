# Purpose: main.R sources load_data.R, process_data.R.
# The downloaded, processed and saved data of that workflow serves as the input 
# for the data visualization on the UI layer.

library(RCurl)
library(httr)
library(lubridate)
library(yaml)

library(xlsx)
library(reshape2)
library(plyr)

setwd('C:/Program Files/ARIS MashZone/resources/prime dashboard 2013')
# Load config file
config <- yaml.load_file('config.yml')

source('./rScripts/load_data.R')
source('./rScripts/process_data.R')
rm(list=ls())