# global.R

library(rvest)
library(dplyr)
library(knitr)
library(data.table)
library(rjson)
require(parallel)

auto_refresh_time <- 3600 * 6
curtime  <- Sys.time()

# Some constants
time_file_format  <- "%Y-%m-%d %H:%M:%S"

# Recording current time and checking timestamp from previously downloaded data

lasttime <-
    try(read.csv("CSV/timestamp.csv", stringsAsFactors = F), silent = T)
if (class(lasttime) == "try-error")
{
    lasttime <- curtime  - auto_refresh_time
    cat("No Scrape History Found", "\n\n")

} else {
    lasttime <- lasttime[,1] %>%
        strptime(time_file_format) %>%
        as.POSIXct

    cat("Last Scrape: ", lasttime, "\n\n")
}

cat("Current Time: ", format(curtime, time_file_format), "\n")
cat("Refresh Due: ",format(lasttime + auto_refresh_time, time_file_format), "\n\n")


######## HELPER FUNCTIONS ################

## RESCRAPE ##
rescrape <- function() {
    link  <- html("http://bidfta.com/") %>%
        html_nodes(".auction")  %>%
        html_node("a") %>%
        html_attr("href")

    fix_these  <- grep("mnlist",link)

    link[fix_these] <- link[fix_these] %>%
        gsub("mnlist","mndetails",.) %>%
        sub("/category/ALL","",.)

    Sys.time() %>% paste0("Starting Top Level Scrape: ", .) # Time the 1st retrieval

    ptm <- proc.time()
    system.time(auctions <- link %>%
                    #.[40:60] %>%
                    lapply(auction_details) %>%
                    #mclapply(auction_details, mc.preschedule = F, mc.cores = 4) %>%  #trying multithreaded
                    .[!sapply(.,is.null)])

    auctions_df <- auctions %>%
        lapply(data.frame, stringsAsFactors = FALSE) %>%
        do.call(rbind, .)

    #auctions_df %>% head %>% print

    #Output time to shell
    Sys.time() %>% paste0("Ending Top Level Scrape: ", .)
    print(proc.time() - ptm)

    # Good locations
    good_loc  <- c("Cincinnati", "Sharonville", "West Chester") %>%
        sapply(function(x)
            grepl(x, auctions_df$location, ignore.case = T)) %>%
        apply(1, any)
    auctions_df <- filter(auctions_df, good_loc)

    # Get Items
    "|----- GETTING ITEMS -----|" %>% print
    print(Sys.time()) # Time the 2nd retrieval
    ptm <- proc.time()

    items <- auctions_df$link %>%
        #mclapply(get_itemslist, mc.cores = 6)
        lapply(get_itemlist)
    names(items) <- auctions_df$title

    #Output time to shell
    print(Sys.time())
    print(proc.time() - ptm)

    items_df <- items %>%
        do.call(rbind, .) %>%
        mutate(Auction = gsub("\\.[0-9]+","", row.names(.)))

    write.csv(curtime, "CSV/timestamp.csv", row.names = F)
    write.csv(auctions_df, "CSV/auctions.csv", row.names = F)
    write.csv(items_df, "CSV/items.csv", row.names = F)

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

    try(a$date  <- tmp[[6]] %>%
            html_text %>%
            gsub("\\.", ",", .) %>%
            gsub("(\\d+)(st|nd|rd|th)","\\1", .) %>%
            strptime("%B %e, %Y %R"))

    try(a$date %>% paste("|", link) %>% print)
    try(if (is.na(a$date) |
            is.null(a$date) | a$date < curtime)
        return(NULL))

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
    #print('ok')
    a
}


# Pulls item lists from an auction item list link
get_itemlist  <- function(lnk) {
    lnk %>% cat("\nWorking: ", ., "\n")

    root_link <- lnk %>%
        gsub("category/ALL","", .)

    itemlist <- root_link %>%
        gsub("mnlist", "mnprint", .) %>%
        html %>%
        html_node("#DataTable") %>%
        html_table(header = T) %>%
        mutate(Item = gsub("[.]","", Item)) %>%
        mutate(Description = iconv(Description, to = 'UTF-8-MAC', sub = '')) %>%
        mutate(link = paste0(root_link, Item))

    cat("itemlist ok")
    #itemlist %>%  print

    n <- nrow(itemlist) - 5

    try(img_link <- itemlist$link %>%
            .[n] %>%
            html %>%
            html_node("#DataTable") %>%
            html_node("img") %>%
            html_attr("src"))

    cat(" | img_link ok")

    try(img_prefix <- img_link %>%
            gsub("/[^/]+$","/",.))

    try(img_suffix <- img_link %>%
            regexpr("\\.\\w+$",.) %>%
            regmatches(img_link, .))

    try(img_suffix <- img_link %>%
            gsub(paste0(img_prefix, itemlist$Item[n]), "", .))

    try(itemlist %>%
            mutate(img_src = paste0(img_prefix, Item, img_suffix)))

}