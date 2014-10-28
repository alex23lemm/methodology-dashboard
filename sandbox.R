# sandbox.R is used to capture intermediate results of potential upcoming 
# features which might make it into production.

# Check out the RSelenium package
#

# Load libraries and config file -----------------------------------------------

library(RSelenium)
library(yaml)

config <- yaml.load_file('config.yml')

# ID of report which should be downloaded
report_id = 29191

# Start Selenium Server --------------------------------------------------------

startServer()

remDrv <- remoteDriver()
remDrv$open()


# Simulate browser session and navigate to report download section -------------

remDrv$navigate('https://www.openair.com/index.pl')

# Fill out and submit form
remDrv$findElement(using = 'name', 'account_nickname')$sendKeysToElement(list(config$openair$company))
remDrv$findElement(using = 'name', 'user_nickname')$sendKeysToElement(list(config$openair$user))
remDrv$findElement(using = 'name', 'password')$sendKeysToElement(list(config$openair$password))
remDrv$findElement(using = 'css selector', '.loginFormBtn')$clickElement()

# Open menu and navigate to proxy user page
remDrv$findElement(using = 'css selector', '.nav_user')$clickElement()
remDrv$findElement(using = 'link text', 'Log in as')$sendKeysToElement(list(
  key = 'enter'))

# Switch to proxy user who has report execution/download rights
remDrv$findElement(using = 'link text', config$openair$proxy)$sendKeysToElement(list(key = 'enter'))
remDrv$switchToWindow(remDrv$getWindowHandles()[[1]][2])

# Navigate to report page
remDrv$findElement(using = 'link text', 'Reports')$sendKeysToElement(list(key = 'enter'))
remDrv$findElement(using = 'link text', 'Saved reports')$sendKeysToElement(list(key = 'enter'))

report_list <- remDrv$findElements(using = 'css selector', 
                                   '.listCell a:nth-child(2)')
report_links <- sapply(report_list, 
                       function(x) x$getElementAttribute('href')[[1]])

# Identify report of choice in the list
index <- which(grepl(report_id , report_links))
# Navigate to specific report download page
report_list[[index]]$sendKeysToElement(list(key = 'enter'))

# Download report --------------------------------------------------------------

remDrv$findElements("link text", 'Click here')[[1]]$sendKeysToElement(list(
  key = 'enter'))

remDrv$quit()
remDrv$closeServer()






