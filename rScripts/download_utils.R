# download_utilis.R includes all utility functions used by the load_data.R 
# script to download the data 


download_openair_data_rvest <- function(report_ids) {
  # Downloads report csv data from OpenAir using curl
  #
  # Args:
  #   report_ids: Unique report IDs
  #        
  # Returns:
  #   list containing parsed csv data as data frames
  
  base_url = "https://www.openair.com/"
  openair <- html_session(paste0(base_url, 'index.pl'))
  
  # Enter site and navigate to reports section ---------------------------------
  
  # Fill out login form and enter site
  login <- html_form(openair) %>%
    extract2(1) %>%
    set_values(
      account_nickname = config$openair$company,
      user_nickname = config$openair$user,
      password = config$openair$password
    )
  
  login$url <- 'https://www.openair.com/index.pl'
  
  openair %<>% submit_form(login) %>%
    follow_link('Dashboard')
  
  proxy_section_link <- html(openair) %>% 
    html_node(xpath = "//script[contains(text(),'OA3.ui.transform.nav.header.init')]") %>%
    xmlValue
  proxy_section_link <- regexpr("Support(.*)dashboard.pl(.*?)proxy_as", 
                                proxy_section_link) %>% 
    regmatches(proxy_section_link, .)
  proxy_section_link <- regexpr("dashboard.pl(.*?)proxy_as", 
                                proxy_section_link) %>%
    regmatches(proxy_section_link, .)  
  
  
  # Continue browsing with proxy user
  openair %<>% jump_to(paste0(base_url, proxy_section_link)) %>% 
    follow_link(config$openair$proxy) %>%
    follow_link('Reports') %>% follow_link('Saved reports')
  
  # Identify and download reports of choice ------------------------------------
  
  my_cookies <- cookies(openair) %>% unlist
  report_links <- html(openair) %>%
    xmlRoot %>% xpathSApply('//a[@title="Download"]/@href')
  report_list <- list()
  
  for (i in seq_along(report_ids)) {
    
    index <- which(grepl(report_ids[i] , report_links))
    openair %<>% jump_to(paste0(base_url, report_links[index]))
    # Navigate to download section
    download_url <- html(openair) %>% xmlRoot %>% 
      xpathSApply("//a[text()='Click here']/@href")
    # Download and store csv data
    parsed_csv <- GET(paste0(base_url, download_url[[1]]), 
                      set_cookies(.cookies = my_cookies)) %>% content('parsed')
    report_list[[i]] <- parsed_csv
    
    if (i < length(report_ids)) {
      openair %<>% back
    }
  }
  
  return(report_list)
  
}


download_openair_data_rselenium <- function(report_ids){
  # Downloads report csv data from OpenAir using RSelenium
  #
  # Args:
  #   report_ids: Unique report IDs
  #        
  # Returns:
  #   list containing parsed csv data as data frames

  base_url = "https://www.openair.com/"
  
  #  Start phantomjs in webdriver mode -----------------------------------------
  pJS <- phantom(extras = c('--ignore-ssl-errors=yes', '--ssl-protocol=tlsv1'))
  Sys.sleep(5) 
  remDrv <- remoteDriver(browserName = 'phantomjs')
  remDrv$open()
  Sys.sleep(5) 
   
  # Enter site and navigate to reports section ---------------------------------
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
  Sys.sleep(5)
  
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
  Sys.sleep(5)
  remDrv$findElement(using = 'xpath', "//a[text()='Saved reports']")$sendKeysToElement(list(key = 'enter'))
  Sys.sleep(5)
  
  # Identify and download reports of choice ------------------------------------
  
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
    Sys.sleep(2)
    parsed_csv <- GET(download_link, set_cookies(.cookies = cookies)) %>% 
      content('parsed')
    report_list[[i]] <- parsed_csv
    if (i < length(report_ids)) {
      remDrv$goBack() 
      Sys.sleep(2)
    }
  }
  remDrv$close()
  pJS$stop() 
  
  return(report_list)
}


download_openair_data_mix <- function(report_ids) {
  # Downloads report csv data from OpenAir using RSelenium and curl.
  # The login is performed using RSelenium. After that the cookies are extracted
  # and passed on to a rvest session object.
  #
  # This version of the function was necessary after switching to rvest 0.3.0
  #
  #
  # Args:
  #   report_ids: Unique report IDs
  #        
  # Returns:
  #   list containing parsed csv data as data frames
  
  base_url = "https://www.openair.com/"
  
  pJS <- phantom(extras = c('--ignore-ssl-errors=yes', '--ssl-protocol=tlsv1'))
  Sys.sleep(5) 
  remDrv <- remoteDriver(browserName = 'phantomjs')
  Sys.sleep(5)
  remDrv$open()
  Sys.sleep(5) 
  
  # Simulate browser session and navigate to report download section -------------
  
  remDrv$navigate('https://www.openair.com/index.pl')
  Sys.sleep(5)
  
  # Fill out and submit form
  remDrv$findElement(using = 'name', 'account_nickname')$sendKeysToElement(list(config$openair$company))
  Sys.sleep(5)
  remDrv$findElement(using = 'name', 'user_nickname')$sendKeysToElement(list(config$openair$user))
  Sys.sleep(5)
  remDrv$findElement(using = 'name', 'password')$sendKeysToElement(list(config$openair$password))
  Sys.sleep(5)
  remDrv$findElement(using = 'css selector', '.loginFormBtn')$clickElement()
  Sys.sleep(15)
  
  # Extract cookies and pass them to rvest session object
  my_cookies <- remDrv$getAllCookies()
  Sys.sleep(10)
  
  my_cookies <- do.call(rbind.data.frame, my_cookies)
  my_cookies <- my_cookies %>% transmute(
    name = as.character(name), 
    value = as.character(value)
  )
  
  cookies <- my_cookies$value
  names(cookies) <- my_cookies$name
  
  openair <- html_session(remDrv$getCurrentUrl()[[1]], set_cookies(.cookies = cookies))
  
  # Stop phantom session and continue with curl
  remDrv$close()
  pJS$stop() 

  openair %<>% follow_link('Dashboard')
  
  proxy_section_link <- read_html(openair) %>% 
    html_node(xpath = "//script[contains(text(),'OA3.ui.transform.nav.header.init')]") %>%
    html_text
  proxy_section_link <- regexpr("Support(.*)dashboard.pl(.*?)proxy_as", 
                                proxy_section_link) %>% 
    regmatches(proxy_section_link, .)
  proxy_section_link <- regexpr("dashboard.pl(.*?)proxy_as", 
                                proxy_section_link) %>%
    regmatches(proxy_section_link, .)  
  
  
  # Continue browsing with proxy user
  openair %<>% jump_to(paste0(base_url, proxy_section_link)) %>% 
    follow_link(config$openair$proxy) %>%
    follow_link('Reports') %>% follow_link('Saved reports')
  
  # Identify and download reports of choice ------------------------------------
  
  report_links <- read_html(openair) %>%
    html_nodes(xpath = '//a[@title="Download"]/@href') %>% html_text
  report_list <- list()
  
  for (i in seq_along(report_ids)) {
    
    index <- which(grepl(report_ids[i] , report_links))
    openair %<>% jump_to(paste0(base_url, report_links[index]))
    # Navigate to download section
    download_url <- read_html(openair) %>% 
      html_nodes(xpath = "//a[text()='Click here']/@href") %>% html_text
    # Download and store csv data
    parsed_csv <- openair %>% 
      jump_to(paste0(base_url, download_url[[1]])) %$%
      response %>% content("parsed")
    
    report_list[[i]] <- parsed_csv
  }
  return(report_list)
}



download_planio_report <- function(report_id, project_name, api_key) {
  # Downloads a task reportfrom LabCase using curl
  #
  # Returns:
  #   Character vector containing JSON data 
  
  base_url <- "https://labcase.softwareag.com"
  
  report_json <- GET(paste0(base_url, '/issues.json?query_id=',
                            report_id, '&project_id=', project_name, '&limit=10000'),
                add_headers(
                  `X-Redmine-API-Key` = api_key
                ),
                content_type_json()
  ) %>% content(as = "text") 
  
  
  return(report_json)
}
