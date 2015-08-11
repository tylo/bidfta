# global.R

library(rvest)
library(dplyr)
library(knitr)
library(data.table)
library(rjson)

auto_refresh_time <- 3600
curtime  <- Sys.time()

# Some constants
time_file_format  <- "%Y-%m-%d %H:%M:%S"

# Recording current time and checking timestamp from previously downloaded data

lasttime <- try(read.csv("CSV/timestamp.csv", stringsAsFactors = F), silent = T)
lasttime  <-  if (class(lasttime) == "try-error") {
  curtime  - auto_refresh_time

} else {
  lasttime[,1] %>%
    strptime(time_file_format) %>%
    as.POSIXct %>%
    print
}

clean_str <- function(str) {
  str %>%
    gsub("[\t\n\r\v\f]", " ", .) %>%
    gsub(" +"," ",.) %>%
    gsub("^\\s+|\\s+$", "", .)
}

# Pulls auction details from an auction description page link
auction_details  <- function(link) {
  a  <- list()

  tmp <- html(link) %>%
    html_nodes("table tr td")

  a$date  <- tmp[[6]] %>%
    html_text %>%
    gsub("\\.", ",", .) %>%
    gsub("(\\d+)(st|nd|rd|th)","\\1", .) %>%
    strptime("%B %e, %Y %R")

  a$date %>% paste("|", link) %>% print
  if (is.na(a$date) | is.null(a$date) | a$date < curtime) return(NULL)

  a$title  <- tmp %>%
    html_nodes("#auction_title") %>%
    .[[1]] %>%
    html_text

  # 8th entry / row in table contains auction location
  a$location  <- tmp[[8]] %>%
    html_text %>%
    clean_str

  a$link  <- link %>%
    gsub("mndetails","mnlist",.) %>%
    paste0("/category/ALL")
  print('ok')
  a
}


# Pulls item lists from an auction item list link
get_itemlist  <- function(lnk){

  lnk %>% paste("Working |", .) %>% print

  root_link <- lnk %>%
    gsub("category/ALL","", .)

  itemlist <- root_link %>%
    gsub("mnlist", "mnprint", .) %>%
    html %>%
    html_node("#DataTable") %>%
    html_table(header = T) %>%
    mutate(Item = gsub("[.]","", Item)) %>%
    mutate(Description = iconv(Description, to='UTF-8-MAC', sub = '')) %>%
    mutate(link = paste0(root_link, Item))

  print("itemlist ok")

  lnk %>% paste("item |", .)
  n <- nrow(itemlist)-1

  img_link <- itemlist$link %>%
    .[n] %>%
    html %>%
    html_node("#DataTable") %>%
    html_node("img") %>%
    html_attr("src")

  print("img_link ok")

  img_prefix <- img_link %>%
    gsub("/[^/]+$","/",.)

  img_suffix <- img_link %>%
    regexpr("\\.\\w+$",.) %>%
    regmatches(img_link, . )

  img_suffix <- img_link %>%
    gsub(paste0(img_prefix, itemlist$Item[n]), "", .)

  itemlist %>%
    mutate(img_src = paste0(img_prefix, Item, img_suffix))

}