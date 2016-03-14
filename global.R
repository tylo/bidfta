# global.R

library(rvest)
library(DT)
library(dplyr)
library(knitr)
#library(data.table)
library(rjson)
require(parallel)
require(urltools)

auto_refresh_time <- 3600 * 3
ending_soon_time <- 3600 * 2

# Some constants
time_file_format  <- "%Y-%m-%d %H:%M:%S"
wishlist_loc <- "CSV/wishlist.csv"


######################################
#------------------------------------#
#------- HELPER FUNCTIONS -----------#
#------------------------------------#
######################################

######################################
#------ FUNCTION: GET_WISHLIST ------#
######################################
get_wishlist <- function() {

    read.csv(file = wishlist_loc,
             header = T,
             stringsAsFactors = F) %>%
        .[,1]
}

######################################
#------ FUNCTION: SAVE_WISHLIST -----#
######################################
save_wishlist <- function(tmp_wishlist) {

    tmp_wishlist %>%
        data.frame  %>%
        write.csv(file = wishlist_loc,
                  row.names = FALSE)
}


######################################
#------ FUNCTION: GEN_GCAL_URL ------#
######################################
gcal_base <- "https://www.google.com/calendar/render?action=TEMPLATE&text=[description]&dates=[start]&details=[details]&location=[location]"
gen_gcal_url <- function(event_title, stime, description, loc ) {

    start_time <- stime %>% strftime(format = "%Y%m%dT%H%M00Z")
    end_time <- (stime + 30*60) %>% strftime(format = "%Y%m%dT%H%M00Z")

    gcal_base %>%
        param_set( "text", event_title ) %>%
        param_set( "details", description ) %>%
        param_set( "location", loc ) %>%
        param_set( "dates", paste0( start_time,"/", end_time ))
}

######################################
#--- FUNCTION: CLEAN_DESCRIPTION ----#
######################################
clean_description <- function(description) {
    description %>%
        gsub("(Additional Information|MSRP|Retail):.*", "", .) %>%
        gsub("(\t|\n|\r).*", "", .)
}


######################################
#----- FUNCTION: GEN_AMAZON_URL -----#
######################################
amazon_base <- "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords="
gen_amazon_url <- function(description) {

    description %>%
        gsub("((Item)? ?Description|Brand): ?", " ", .) %>%
        gsub("(Additional Information|MSRP|Retail):.*", "", .) %>%
        gsub("(\t|\n|\r).*", "", .) %>%
        gsub(" ", "+", .) %>%
        paste0('<a href="', amazon_base, . ,
               '" target="_blank"><i class="fa fa-external-link-square fa-lg"></i></a>'
        )
}


######################################
#--------- FUNCTION: GEN_PINS -------#
######################################
pin_html <- '<div class="pin box"><a href="%s" target="_blank"><img src="%s"/><p>%s</p></div>'
gen_pins <- function( description, item_url, img_url,
                      auction_end="", location="", gcal_url="", amazon_url ="" ) {

    pin_html %>% sprintf( img_url, item_url, description)

}


######################################
#-------- FUNCTION: RESCRAPE --------#
######################################

rescrape <- function() {
    link  <- read_html("http://bidfta.com/") %>%
        html_nodes(".auction")  %>%
        html_node("a") %>%
        html_attr("href")

    ##########################################################
    ##### THIS STUFF SHOULD GO IN A VALIDATOR FUNCTION #######
    # Filter out links on other auction sites
    fix_these  <- grep("mnlist",link)

    link[fix_these] <- link[fix_these] %>%
        gsub("mnlist","mndetails",.) %>%
        sub("/category/ALL","",.)


    # Filter out blank links
    link <- link[link != '']

    # Filter out links on other auction sites
    bidfta_hosted <- grepl("bidfta", link, ignore.case = T)

    "total links found" %>% cat(link %>% length,.,"\n")
    "external auctions removed\n" %>% cat((!bidfta_hosted) %>% sum, .)
    link <- link[bidfta_hosted]

    ##### END LINK VALIDATION ################################
    ##########################################################


    # Time the 1st retrieval
    "|----- GETTING AUCTIONS -----|" %>% cat("\n",., "\n\n")
    #print(link)
    ptm <- proc.time()
    auctions <- link %>%
        #.[40:60] %>%
        lapply(auction_details)

    # Report how many null auctions
    auctions %>% sapply(is.null) %>% sum %>% cat("\n",.,"expired or invalid auctions removed\n")

    auctions <- auctions %>%
        #mclapply(auction_details, mc.preschedule = F, mc.cores = 4) %>%  #trying multithreaded
        .[!sapply(.,is.null)]

    auctions_df <- auctions %>%
        lapply(data.frame, stringsAsFactors = FALSE) %>%
        do.call(rbind, .)


    #Output time to shell
    print(proc.time() - ptm)



    #Cleaning locations and eliminating out-of-towners
    auctions_df$location <- auctions_df$location %>% gsub(" \\d{5}.*","",., ignore.case = T)
    auctions_df$location %>% table %>% data.frame %>% arrange(desc(Freq)) %>% print

    # Good locations
    good_loc  <- c("Cincinnati", "Sharonville", "West Chester") %>%
        sapply(function(x) grepl(x, auctions_df$location, ignore.case = T)) %>%
        apply(1, any)



    # Report how many auctions in different locations
    (good_loc) %>% sum %>% cat("\n",.,"local auctions\n")
    (!good_loc) %>% sum %>% cat("out-of-town auctions removed\n")

    auctions_df <- filter(auctions_df, good_loc)

    # Get Items
    "|----- GETTING ITEMS -----|" %>%  cat("\n",., "\n")

    ptm <- proc.time()

    items <- auctions_df$link %>%
        #mclapply(get_itemslist, mc.cores = 6)
        lapply(get_itemlist)
    names(items) <- auctions_df$title

    #Output time to shell
    print(proc.time() - ptm)

    items_df <- items %>%
        do.call(rbind, .) %>%
        mutate(Auction = gsub("\\.[0-9]+","", row.names(.)))

    write.csv(Sys.time(), "CSV/timestamp.csv", row.names = F)
    #print("done1")
    write.csv(auctions_df, "CSV/auctions.csv", row.names = F)
    #print("done2")
    write.csv(items_df, "CSV/items.csv", row.names = F)
    #print("done3")

}

######################################
#------- FUNCTION: CLEAN_STR --------#
######################################

clean_str <- function(str) {
    str %>%
        gsub("[\t\n\r\v\f]", " ", .) %>%
        gsub(" +"," ",.) %>%
        gsub("^\\s+|\\s+$", "", .)
}

######################################
#----- FUNCTION: AUCTION_DETAILS ----#
######################################

# Pulls auction details from an auction description page link
auction_details  <- function(link) {
    a  <- list()

    tmp <- read_html(link) %>%
        html_nodes("table tr td")

    cat(link)
    try(a$date  <- tmp[[6]] %>%
            html_text %>%
            gsub("\\.", ",", .) %>%
            gsub("(\\d+)(st|nd|rd|th)","\\1", .) %>%
            strptime("%B %e, %Y %I:%M %p", tz = 'EST5EDT')
    )

    #print(class(a$date))
    try(
        if (is.null(a$date) ) {
            cat (" | no date\n")
            return(NULL)
        }
        else if ( is.na(a$date) ){
            cat (" | no date\n")
            return(NULL)
        }
        else if (a$date - Sys.time() < 0 ) {
            cat (" | expired\n")
            return(NULL)
        }
        else {
            a$date %>% format(usetz = T) %>% cat(" | ", . ,"\n")
        }
    )

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
    #cat("OK\n")
    a
}

######################################
#----- FUNCTION: GET_ITEMSLIST ------#
######################################

# Pulls item lists from an auction item list link
get_itemlist  <- function(lnk) {
    lnk %>% cat("\n", .)

    root_link <- lnk %>%
        gsub("category/ALL","", .)

    itemlist <- root_link %>%
        gsub("mnlist", "mnprint", .) %>%
        read_html %>%
        html_node("#DataTable") %>%
        html_table(header = T, fill = T) %>%
        mutate(Item = gsub("[.]","", Item),
               Description = iconv(Description, to = 'UTF-8', sub = ' ')) %>%
        mutate(link = paste0(root_link, Item))

    cat(" |","itemlist ok")
    #itemlist %>%  print

    n <- nrow(itemlist) - 1

    try(img_link <- itemlist$link %>%
            .[n] %>%
            read_html %>%
            html_node("#DataTable") %>%
            html_node("img") %>%
            html_attr("src"))

    cat(" |","img_link ok")

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
######################################
#------ FUNCTION: GET_AMAZON --------#
######################################

## Get amazon information
get_amazon <- function(description) {
    description %>%
        clean_str %>%
        gsub(" ", "+", .) %>%
        paste0(amazon_base, .) %>%
        read_html %>%
        html_node(".s-item-container") %>%
        html_node("a") %>%
        html_attr("href")
}

####
get_amazon_full <- function(description) {
    url <- description %>%
        clean_str %>%
        gsub(" ", "+", .) %>%
        paste0(amazon_base, .)

    item_1 <- url %>%
        read_html %>%
        html_node(".s-item-container") %>%
        #        .[1] %>%
        html_node("a") %>%
        html_attr("href")


    img_src <- item_1 %>% html_nodes("img") %>%
        html_attr("src")

    price <- item_1 %>% html_node(".s-price") %>% html_text
}
