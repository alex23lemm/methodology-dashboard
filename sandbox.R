# sandbox.R is used to capture intermediate results of potential upcoming 
# features which might make it into production.

# Check out the RSelenium package
#

# Load libraries and config file -----------------------------------------------

library(RSelenium)
library(yaml)
library(dplyr)
library(httr)

config <- yaml.load_file('config.yml')

# ID of report which should be downloaded
report_id = 29191
base_url = "https://www.openair.com/"

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
proxy_page <- remDrv$findElement(using = 'xpath', "//a[contains(text(), 'Log in as')]")$getElementAttribute('href')[[1]]
remDrv$navigate(proxy_page)

# Switch to proxy user who has report execution/download rights
remDrv$findElement(using = 'link text', config$openair$proxy)$sendKeysToElement(list(key = 'enter'))
remDrv$switchToWindow(remDrv$getWindowHandles()[[1]][2])

# Navigate to reports section
remDrv$findElement(using = 'link text', 'Reports')$sendKeysToElement(list(key = 'enter'))
remDrv$findElement(using = 'link text', 'Saved reports')$sendKeysToElement(list(key = 'enter'))

# Identify report of choice ----------------------------------------------------

report_links <- remDrv$getPageSource()[[1]] %>% htmlParse %>% xmlRoot %>%
  xpathSApply('//a[@title="Download"]/@href')

index <- which(grepl(report_id , report_links))

remDrv$navigate(paste0(base_url, report_links[index]))

# Download report --------------------------------------------------------------

download_link <- remDrv$findElements("link text", "Click here")[[1]]$getElementAttribute('href')[[1]]

my_cookies <- remDrv$getAllCookies()
my_cookies <- do.call(rbind.data.frame, my_cookies)
my_cookies <- my_cookies %>% transmute(
  name = as.character(name), 
  value = as.character(value)
  )

cookies <- my_cookies$value
names(cookies) <- my_cookies$name

parsed_csv <- GET(download_link, set_cookies(.cookies = cookies)) %>% 
  content('parsed')

write.csv(parsed_csv, 'report_firefox.csv', row.names = FALSE)

remDrv$quit()
remDrv$closeServer()







