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
  
  openair %<>% submit_form(login) %>%
    follow_link('Dashboard')
  
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
    follow_link('Reports') %>% 
    follow_link('Saved reports')
  

  # Identify and download reports of choice ------------------------------------
  
  report_links <- read_html(openair) %>%
    html_nodes(xpath = '//a[i/@title="Download"]/@href') %>% html_text
  
  report_list <- list()
  
  for (i in seq_along(report_ids)) {
    
    index <- which(grepl(report_ids[i] , report_links))
    openair %<>% jump_to(paste0(base_url, report_links[index]))
    # Navigate to download section
    download_url <- read_html(openair) %>% 
      html_nodes(xpath = "//a[text()='Click here']/@href") %>%
      html_text %>% extract2(1)
    # Download and store csv data
    parsed_csv <- jump_to(openair, paste0(base_url, download_url)) %$% 
      content(response, "parsed") 
    
    
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
  # INFO: This function was implemented as a backup option for 
  #       download_openair_data_rvest(). Add library(RSelenium) and library(XML) 
  #       to main.R if you would like to use it.
  #       
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


download_planio_report_api <- function(report_id, project_name, api_key) {
  # Downloads a task report from LabCase using curl
  #
  # Returns:
  #   data frame containing parsed CSV report data
  
  base_url <- "https://labcase.softwareag.com"
  
  response <- GET(paste0(base_url, '/issues.json?query_id=',
                         report_id, '&project_id=', project_name),
                add_headers(
                  `X-Redmine-API-Key` = api_key
                ),
                content_type_json()
  ) %>% content(as = "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) 
  
  total_count <- response$total_count
  offset <- 0
  limit <- 100
  report <- data_frame()
  
  while (total_count - offset > limit) {
    report <- GET(paste0(base_url, '/issues.json?query_id=',
                           report_id, '&project_id=', project_name,
                           '&offset=', offset, '&limit=', limit),
                    add_headers(
                      `X-Redmine-API-Key` = api_key
                    ),
                    content_type_json()
    ) %>% content(as = "text") %>%
      jsonlite::fromJSON(., flatten = TRUE) %>%
      extract2(1) %>%
      bind_rows(report, .)
    
    offset <- offset + 100
  }
  
  report <- GET(paste0(base_url, '/issues.json?query_id=',
                       report_id, '&project_id=', project_name,
                       '&offset=', offset, '&limit=', limit),
                add_headers(
                  `X-Redmine-API-Key` = api_key
                ),
                content_type_json()
  ) %>% content(as = "text") %>%
    jsonlite::fromJSON(., flatten = TRUE) %>%
    extract2(1) %>%
    bind_rows(report, .) %>%
    transmute(
      methodology = project.name,
      tracker = tracker.name,
      subject = str_trim(subject),
      done = as.numeric(done_ratio),
      lc.issue.numb = as.numeric(id),
      parent.task = parent.id
    ) 
  
  return(report)
}

download_planio_report_ws <- function(report_id, project_name, user_name, 
                                      password) {
  # Downloads task report Excel data from LabCase using web scraping
  #
  # Returns:
  #   data frame containing parsed CSV report data
  
  base_url <- "https://labcase.softwareag.com"
  
  planio <- html_session(base_url)
  
  login <- html_form(planio) %>%
    extract2(1) %>%
    set_values(  
      username = user_name,
      password = password
    )
  
  planio %<>% submit_form(login) %>% jump_to(paste0("projects/", project_name, 
                                                    "/issues?query_id=", report_id))
  
  report <- planio %>% 
    jump_to(paste0(base_url, "/projects/prime/issues.csv")) %$%
    response %>% content("parsed") %>% 
    transmute(
      lc.issue.numb = as.numeric(X.),
      methodology = Project,
      tracker = Tracker,
      subject = str_trim(Subject),
      done = as.numeric(X..Done),
      parent.task = Parent.task
    )
  
  report$parent.task <- as.numeric(sapply(regmatches(report$parent.task, 
                                                     regexec('.*#([0-9]+).*', 
                                                             report$parent.task)),
                                          function(x)x[2]))
  return(report)
  
}
