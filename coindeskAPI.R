## Wrap Coindesk API into R function
library(curl)
library(httr)
library(stringr)
library(jsonlite)
library(lubridate)
#https://api.coindesk.com/v1/bpi/currentprice.json
#https://api.coindesk.com/v1/bpi/historical/close.json

coindesk_api <- function(path) {
  
  url <- modify_url("https://api.coindesk.com/", path = path)
  resp <- GET(url)
  #browser()

  parsed <- jsonlite::fromJSON(content(resp, "text"))
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "Coindesk API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(
      content = data.frame(parsed),
      path = path,
      response = resp
    ),
    class = "coindesk_api"
  )
}

getCurrentPrice <- function(){
  coindesk_api('v1/bpi/currentprice.json')$content
}

getHistoricPrice <- function(index = NULL, currency = NULL, start = NULL, end = NULL, yesterday = F){
  #?index=[USD/CNY]
  #?currency=<VALUE>
  #?start=<VALUE>&end=<VALUE>
  #?for=yesterday
  
  if(is.null(index)){
    index <- ''
  }
  else{
    index <- paste0("?index=", index)
  }
  
  if(is.null(currency)){
    currency <- ''
  }
  else{
    currency <- paste0("?currency=", currency)
  }
  
  if(is.null(start) && is.null(end)){
    start_end <- paste0("?start=",today(), "&end=", today()-31)
  }
  else{
    start_end = paste0("?start=", start, "&end=", end)
  }
  if(yesterday){
    start_end = ''
    yesterday <- paste0("?for=yesterday")
  }
  else{
    yesterday = ''
  }
  
  query <- paste0('v1/bpi/historical/close.json', index, currency, start_end, yesterday)
  #browser()
  coindesk_api(query)$content
}
