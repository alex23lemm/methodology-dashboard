# download_utilis.R includes all utility functions used by the load_data.R 
# script to download the data 

download_openair_data <- function(report_ids){
  # Downloads report csv data from OpenAir
  #
  # Args:
  #   report_ids: Unique report IDs
  #        
  # Returns:
  #   list containing parsed csv data as data frames

base_url = "https://www.openair.com/"

#  Start phantomjs in webdriver mode -------------------------------------------

pJS <- phantom(extras = c('--ignore-ssl-errors=yes', '--ssl-protocol=tlsv1'))
Sys.sleep(5) 
remDrv <- remoteDriver(browserName = 'phantomjs')
remDrv$open()


# Enter site and navigate to reports section -----------------------------------

remDrv$navigate(paste0(base_url, 'index.pl'))
Sys.sleep(5)

# Fill out login form and enter site
remDrv$findElement(using = 'name', 'account_nickname')$sendKeysToElement(list(config$openair$company))
Sys.sleep(2)
remDrv$findElement(using = 'name', 'user_nickname')$sendKeysToElement(list(config$openair$user))
Sys.sleep(2)
remDrv$findElement(using = 'name', 'password')$sendKeysToElement(list(config$openair$password))
Sys.sleep(2)
remDrv$findElement(using = 'css selector', '.loginFormBtn')$clickElement()
Sys.sleep(2)

# Open menu and navigate to proxy user page
remDrv$findElement(using = 'css selector', '.nav_user')$clickElement()
Sys.sleep(2)
proxy_page <- remDrv$findElement(using = 'xpath', "//a[contains(text(), 'Log in as')]")$getElementAttribute('href')[[1]]
remDrv$navigate(proxy_page)

# Continue browsing with proxy user
remDrv$findElement(using = 'xpath', paste0("//a[text()='", config$openair$proxy, "']"))$sendKeysToElement(list(key = 'enter'))
Sys.sleep(5)

window_handles <- remDrv$getWindowHandles()

if (length(window_handles[[1]] == 2)) {
  remDrv$switchToWindow(remDrv$getWindowHandles()[[1]][2])
} 
Sys.sleep(10)

# Navigate to reports section
remDrv$findElement(using = 'xpath', "//a[text()='Reports']")$sendKeysToElement(list(key = 'enter'))
remDrv$findElement(using = 'xpath', "//a[text()='Saved reports']")$sendKeysToElement(list(key = 'enter'))


# Identify and download reports of choice --------------------------------------

report_links <- remDrv$getPageSource()[[1]] %>% htmlParse %>% xmlRoot %>%
  xpathSApply('//a[@title="Download"]/@href')

my_cookies <- remDrv$getAllCookies()
my_cookies <- do.call(rbind.data.frame, my_cookies)
my_cookies <- my_cookies %>% transmute(
  name = as.character(name), 
  value = as.character(value)
)

cookies <- my_cookies$value
names(cookies) <- my_cookies$name


report_list <- list()

for (i in seq_along(report_ids)) {
  
  index <- which(grepl(report_ids[i] , report_links))
  remDrv$navigate(paste0(base_url, report_links[index]))
  Sys.sleep(5)
  download_link <- remDrv$findElements(using = 'xpath', "//a[text()='Click here']")[[1]]$getElementAttribute('href')[[1]]
  parsed_csv <- GET(download_link, set_cookies(.cookies = cookies)) %>% 
    content('parsed')
  report_list[[i]] <- parsed_csv
  if (i < length(report_ids)) {
    remDrv$goBack()  
  }
}

remDrv$close()
pJS$stop() 

return (report_list)

}




