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



# 1. Download OpenAir raw data -------------------------------------------------


report_list <- try(download_openair_data_rvest(c(config$openair$project_billable_report_id,
                                                 config$openair$timesheet_billable_report_id)),
                   silent = TRUE)

if (class(report_list) == 'try-error' | class(report_list) != 'list') {
   stop("OpenAir: Download of report data via rvest failed.")
} 

tmp.proj.billable.df <- report_list[[1]]
tmp.timesheet.billable.df <- report_list[[2]]


# 2. Download LabCase raw data -------------------------------------------------


employee.binary <- try(GET(config$url$lc_employee_list, 
                       authenticate(config$labcase$user, 
                                    config$labcase$password)))

if (class(employee.binary) == c('try-error') || 
   status_code(employee.binary) != 200) {
  stop("LabCase: Download of Excel file via httr failed")
}

employee.binary <- content(employee.binary)


# Download task list
lc_tasks <- try(download_planio_report_api(config$labcase$report_id,
                                            config$labcase$project_name,
                                            config$labcase$api_key),
                  silent = TRUE)
  
if (class(lc_tasks) == 'try-error') {
    stop("LabCase: Download of report data via LC REST API failed.")
  }



# 3. Save the dowonloaded raw data ---------------------------------------------

# Only save and make data available for processing when every download 
# was successfull

#Date of downloading and processing
date <- format(now(), '%b %d, %Y %X') 
write.csv(as.data.frame(date), file = './rOutput/dateOfRetrieval.csv', 
          row.names = FALSE)
write.csv(tmp.proj.billable.df, file = './rawData/oa_proj_billable.csv',
          row.names = FALSE)
write.csv(tmp.timesheet.billable.df, file = './rawData/oa_timesheet_billable.csv', 
          row.names = FALSE)


con <- file('./rawData/employeeCountryMapping.xlsx', open = 'wb')
writeBin(employee.binary, con)
close(con)

write.csv(lc_tasks, file = './rawData/lc_tasks.csv', row.names = FALSE)

