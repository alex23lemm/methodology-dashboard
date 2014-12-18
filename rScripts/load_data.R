# The load_data.R script extracts the relevant raw data by fetching 
# several reports and additonal files from OpenAir and LabCase. 
# The script aborts without saving anything should an error occur in between.
#
# load_data.R consists of three mayor parts:
#  1. Load raw data from OpenAir
#  2. Load raw data from LabCase
#  3. Save the dowonloaded raw data
#


# download.file does not work with rscript.exe due to https
# see: http://stackoverflow.com/questions/7715723/sourcing-r-script-over-https
# Instead methods from the httr (url_ok) and the RCurl (getURL, getBinaryURL) 
# package were used to download the data.



# 1. Download OpenAir raw data -------------------------------------------------

error <- FALSE

report_list <- download_openair_data(c(config$openair$billable_report_id,
                                     config$openair$voluntary_report_id))

if(class(report_list) != 'list'){
  error <- TRUE
  
} else {
  tmp.billable.df <- report_list[[1]]
  tmp.voluntary.df <- report_list[[2]]
}


# oa.billable.exists <- url_ok(config$url$oa_prime_bookable)
# 
# if (oa.billable.exists) {
#   url <- try(getURL(config$url$oa_prime_bookable, ssl.verifypeer = FALSE), 
#              silent = TRUE)    
#   if (class(url) == 'try-error') {
#     error <- TRUE
#   } else {
#     tmp.bookable.df <- read.csv(textConnection(url))
#   }
# } else {
#   error <- TRUE
# }
# 
# if (!error) {
#   oa.voluntary.exists <- url_ok(config$url$oa_prime_voluntary)
# }
# 
# if (oa.voluntary.exists && !error) {
#   url <- try(getURL(config$url$oa_prime_voluntary, ssl.verifypeer = FALSE), 
#              silent = TRUE)
#   if (class(url) == 'try-error') {
#     error <- TRUE
#   } else {
#     tmp.voluntary.df <- read.csv(textConnection(url))
#   }
# } else {
#   error <- TRUE
# }


# 2. Download LabCase raw data -------------------------------------------------

if (!error) {
  lc.empl.exists <- url_ok(config$url$lc_employee_list_check)
}

if (lc.empl.exists && !error) {
  employee.binary <- try(getBinaryURL(config$url$lc_employee_list,
                                      ssl.verifypeer = FALSE, 
                                      userpwd = config$credentials$primeuser),
                         silent = TRUE)
  if (class(employee.binary) == 'try-error') {
    error <- TRUE
  }
} else {
  error <- TRUE
}

if (!error) {
  lcTaskExists <- url_ok(config$url$lc_task_list)
}

if (lcTaskExists && !error) {
  #Download task list
  lc.tasks.binary <- try(getBinaryURL(config$url$lc_task_list, 
                                      ssl.verifypeer = FALSE), silent = TRUE)
  if (class(lc.tasks.binary) == 'try-error') {
    error <- TRUE
  }
} else {
  error <- TRUE
}


# 3. Save the dowonloaded raw data ---------------------------------------------

# Only save data and make it available for processing when every download 
# before was successfull

if (!error) {
  #Date of downloading and processing
  date <- format(now(), '%b %d, %Y %X') 
  write.csv(as.data.frame(date), file = './rOutput/dateOfRetrieval.csv', 
            row.names = FALSE)
  write.csv(tmp.billable.df, file = './rawData/prime_bookable.csv', 
            row.names = FALSE)
  write.csv(tmp.voluntary.df, file = './rawData/prime_voluntary.csv',
            row.names = FALSE)
  
  con <- file('./rawData/employeeCountryMapping', open = 'wb')
  writeBin(employee.binary, con)
  close(con)
  
  con <- file('./rawData/lcPrimeTasks.xls', open = 'wb')
  writeBin(lc.tasks.binary, con)
  close(con)
}