# global.R

library(rvest)
library(DT)
library(dplyr)
library(knitr)
#library(data.table)
library(rjson)
require(parallel)
require(urltools)
require(bubbles)

# Some constants
auto_refresh_time <- 3600 * 3
ending_soon_time <- 3600 * 2
scrape_locations <- c("Cincinnati", "Sharonville", "West Chester",
                      "Maineville", "Milford", "Fairfield")
description_end_regex <- "((Item )?Location:|Front Page:|Lotted By:|Load #:|Contact:|Facebook:|Pinterest:|Twitter:).*"
blurb_end_regex <- "<<(Additional Information|MSRP|Retail):.*"
section_names <- "(Brand|Item Description|MSRP|Model|Specifications|Width|Depth|Height|Weight|Item Link Calc|Additional Information) ?:"
amazon_base <- "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords="
gcal_base <- "https://www.google.com/calendar/render?action=TEMPLATE&text=[description]&dates=[start]&details=[details]&location=[location]"

pin_html <-
'<div class="pin box box-info">
<div class="box-header with-border">
<h3 class="box-title">%s</h3></div>
<div class="box-body">
<a href="%s" target="_blank"><img src="%s"/></a>
<p>%s</p></div>
<div class="box-footer text-center no-padding">
<div class="box-tools">
<a href="%s" class="btn btn-box-tool" target="_blank"><i class="fa fa-amazon"></i></a>
<a href="%s" class="btn btn-box-tool" target="_blank"><i class="fa fa-calendar-plus-o"></i></a>
</div></div>
</div>'

table_list_html <-
'<p>%s</p></div>
<a href="%s" class="btn btn-box-tool" target="_blank"><i class="fa fa-amazon"></i></a>
<a href="%s" class="btn btn-box-tool" target="_blank"><i class="fa fa-calendar-plus-o"></i></a>'

time_file_format  <- "%Y-%m-%d %H:%M:%S"
wishlist_loc <- "CSV/wishlist.csv"
searches_loc <- "log/searches.csv"
auctions_items_bar_split <- .5

version_hist <- readLines("VERSION")
version <- regmatches(version_hist[1], regexpr("\\d+[.]\\d+",version_hist[1]))


######################################
#------------------------------------#
#------- HELPER FUNCTIONS -----------#
#------------------------------------#
######################################


settingInput <- function(inputID, label, description,  default_value, type = "checkbox", ...) {
    if (type == "checkbox") {
        inputTag <- tags$input(id = inputID, type = "checkbox", class="pull-right")
        if (!is.null(default_value) && default_value) inputTag$attribs$checked <- "checked"
        tmp_div <- div(class = "form-group", tags$label(label, inputTag))

    } else if (type == "slider") {
        tmp_div <- sliderInput(inputID, label = label, value = default_value, ...)
    }

    tmp_div$children[[which(sapply(tmp_div$children, "[[", "name") == "label")]]$attribs$class <-
        "control-sidebar-subheading"

    tmp_div$children[[length(tmp_div$children) + 1]] <- p(HTML(description))
        # tags$label(class="control-sidebar-subheading", label, inputTag),
        #
    tmp_div
}


######################################
#--- FUNCTION: GET_LASTN_SEARCHES ---#
######################################
get_lastN_searches <- function(file_loc, n) {
    sprintf('tail -%s %s', n, file_loc) %>%
        system(intern = TRUE) %>%
        textConnection %>%
        read.csv(stringsAsFactors = F) %>%
        .[,2]
}

######################################
#-- FUNCTION: PARSE_SEARCH_STRING ---#
######################################
# The search string is first parsed for multiple search terms, then cleaned,
# then either the multiple terms are unlisted (if SPLIT = T) or written back
# out with some other separator charater, such as a newline.
parse_search_string <- function(str, split = F, sep_char = ", ") {
    cleaned <- gsub("[|]", "_", str) %>% gsub("\\W", " ", .) %>% clean_str()
    cleaned <- if (split) {
        strsplit(cleaned, "_") %>% unlist
    } else {
        gsub("_", sep_char, cleaned)
    }
    return(cleaned)
}

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
#------- FUNCTION: CLEAN_STR --------#
######################################
clean_str <- function(str) {
    str %>%
        gsub("[\t\n\r\v\f]", " ", .) %>%
        gsub("  +"," ",.) %>%
        gsub("^\\s+|\\s+$", "", .)
}

######################################
#----- FUNCTION: STRIP_SECTIONS -----#
######################################
strip_sections <- function(description) {
    gsub(blurb_end_regex, "", description) %>%
        gsub("<<(\\w| )*:>>", " ", .) %>%
        clean_str
}

######################################
#---- FUNCTION: STYLE_DESCRIPTIOn ----#
######################################
style_description <- function(description) {
    gsub("<<", "<br><strong>", description) %>%
        gsub(">>", "</strong>", .) %>%
        clean_str
}


######################################
#------- FUNCTION: GEN_TITLE --------#
######################################
gen_title <- function(description, num_words=3) {
    strsplit(description, " ") %>%
        sapply(function(x) paste0(x[1:num_words], collapse = " "))
}

######################################
#----- FUNCTION: GEN_AMAZON_URL -----#
######################################
gen_amazon_url <- function(description) {
        gsub(" +", "+", description) %>% paste0(amazon_base, . )
}

######################################
#------ FUNCTION: GEN_GCAL_URL ------#
######################################
gen_gcal_url <- function(event_title, stime, description, loc="" ) {

    start_time <- (stime - 15*60) %>% strftime(format = "%Y%m%dT%H%M00Z", tz="UTC" )
    end_time <- stime %>% strftime(format = "%Y%m%dT%H%M00Z", tz="UTC")

    gcal_base %>%
        rep_len( length( event_title ) ) %>%
        param_set( "text", event_title ) %>%
        param_set( "dates", paste0( start_time,"/", end_time )) %>%
        param_set( "details", description ) %>%
        param_set( "location", loc )
}

######################################
#---- FUNCTION: GEN_SEARCH_OUTPUT ----#
######################################
gen_search_output <- function(description, item_url, img_url="",
                              auction_end="", location="",
                              pins = T) {

    # short title for the pin
    title <- strip_sections(description) %>% gen_title(6)
    amazon_url <- gen_amazon_url(title) # create amazon url
    gcal_url <- gen_gcal_url(paste("Bid:", title) %>% url_encode, auction_end, item_url)

    # style the description
    description_html <- style_description(description)

    if (pins) {
        sprintf(pin_html,
            strftime(auction_end, format = "%a %r", tz="America/New_York"),
            item_url, img_url, description_html, amazon_url, gcal_url)
    } else { # table list
        sprintf(table_list_html, description_html, amazon_url, gcal_url)
    }
}


######################################
#--- FUNCTION: PARSE_DESCRIPTION ----#
######################################
parse_description <- function(description) {
    description %>%
        iconv( to = 'UTF-8', sub = ' ' ) %>%
        gsub(description_end_regex,"", .) %>%
        gsub(section_names, "<<\\1:>>", .) %>%
        clean_str
}

######################################
#---- FUNCTION: GROK_DESCRIPTION ----#
######################################
grok_description <- function(vec) {
    tabular <- vec[-1] %>% clean_str %>% matrix(byrow = T, ncol = 2)
    split(tabular[,2], tabular[,1])
}

######################################
#---- FUNCTION: RETRIEVE_FEATURE ----#
######################################
get_feature <- function(list_items, feature_name) {
    lapply(list_items, "[[", feature_name ) %>% as.character %>% unlist
}

######################################
#-------- FUNCTION: GET_MSRP --------#
######################################
get_MSRP <- function(list_items) {
    list_items %>% get_feature("MSRP") %>% gsub('[$]',"",.) %>% as.numeric
}

######################################
#------- FUNCTION: DO_SEARCH --------#
######################################
do_search <- function(search_string, df, col.name = "Description", whole_words = T) {

    # Write search string to log
    data.frame(time = Sys.time(), search_string = search_string) %>%
        write.table("log/searches.csv", append = T, sep = ",", row.names = F, col.names = F )

    ifelse(whole_words, paste0("\\W(", search_string, ')\\W'), search_string) %>%
        print %>%
        grepl(df[[col.name]], ignore.case = T) %>%
        df[.,]
}

######################################
#-------- FUNCTION: RESCRAPE --------#
######################################
rescrape <- function( use.progress = T ) {

    # Create a Progress object
    if ( use.progress ) {
        progress <- shiny::Progress$new()
        progress$set(message = "Scraping ...", value = 0)
        on.exit(progress$close())

        # Progress call-back
        incrementProgress <- function(incr) {
            value <- progress$getValue()
            progress$set(value = value + incr)
        }
    }

    # Reading in old auction links and comparing with currently-available links.
    # We only scrape links not previously known!
    known_links <- try(read.csv("CSV/known_links.csv", stringsAsFactors = F) %>% .[,1])

    current_links  <- read_html("http://bidfta.com/") %>%
        html_nodes(".auction")  %>%
        html_nodes("a[target=_blank]") %>%
        html_attr("href")

    new_links <- if ( class(known_links) == "try-error" )
        validate_links(current_links) else
            validate_links( setdiff(current_links, known_links) )


    # Add new timestamp
    data.frame( time = Sys.time(), method = ifelse( use.progress, "browser", "cron" ) ) %>%
        write.table( "CSV/timestamp.csv", append = T, sep = ",", row.names = F, col.names = F )

    # Exit if nothing new
    if (length(new_links) == 0)

    # GET AUCTION DETAILS (expiration, location, title)
    "|----- GETTING AUCTIONS -----|" %>% cat("\n",., "\n\n")
    if ( use.progress ) progress$set(detail = "Fetching auction list", value=0)
    auctions_incr <- auctions_items_bar_split/length(new_links)
    ptm <- proc.time()

    auctions <- 1:length( new_links ) %>%
        lapply( function(i) {
            cat( sprintf("%-4s", i))
            auction_details( new_links[i],
                             auctions_incr, if(use.progress) incrementProgress else NULL )})

    # Report how many null auctions and filter them out
    null.auctions <- auctions %>% sapply(is.null)
    cat("\n", sum( null.auctions ),"expired or invalid auctions removed\n")
    auctions <- auctions[ !null.auctions ]

    auctions_df <- auctions %>%
        lapply(data.frame, stringsAsFactors = FALSE) %>%
        do.call(rbind, .) %>%
        mutate( link.descpage = new_links[ !null.auctions ],
                link.pageditems  = link.descpage %>%
                    gsub("mndetails","mnlist",.) %>% paste0("/category/ALL")
        )

    # Output time to shell
    print(proc.time() - ptm)

    # Cleaning locations and eliminating out-of-towners
    auctions_df$location <- auctions_df$location %>% gsub(" \\d{5}.*","",., ignore.case = T)
    auctions_df$location %>% table %>% data.frame %>% arrange(desc(Freq)) %>% print

    # Remove locations not on the scrape_locations list
    good_loc  <- paste(scrape_locations, sep = "", collapse = "|") %>%
        grepl( auctions_df$location, ignore.case = T )

    # Exit if nothing new
    if ( sum(good_loc) == 0) return()

    # Report how many auctions in different locations
    cat((!good_loc) %>% sum, "out-of-town auctions removed\n")
    cat((good_loc) %>% sum ,"local auctions to be scraped\n")
    auctions_df <- filter(auctions_df, good_loc)

    # Append new auctions_df to old (unexpired) auctions_df to get current
    old_auctions_df  <- try(
        read.csv( "CSV/auctions.csv", stringsAsFactors = F) %>%
            mutate( date =  strptime(date, time_file_format, tz = "EST5EDT") %>% as.POSIXct ) %>%
            filter(date > Sys.time() )
    )
    current_auctions_df <- if( class(old_auctions_df) == "try-error" )
        auctions_df else rbind(old_auctions_df,auctions_df)

    # GET ITEMS
    "|----- GETTING ITEMS -----|" %>%  cat("\n",., "\n")
    ptm <- proc.time()
    if ( use.progress ) progress$set(detail = "Fetching auction items", value = auctions_items_bar_split)
    items_incr <- (1 - auctions_items_bar_split)/length(auctions_df$urlname)

    items <- 1:nrow(auctions_df) %>%
        lapply( function(i) {
            cat("\n", sprintf("%-4s", i) )
            get_itemlist(auctions_df$link.pageditems[i],
                         items_incr, if( use.progress ) incrementProgress else NULL )})

    names(items) <- auctions_df$title

    items_df <- items %>%
        do.call( rbind, . ) %>%
        mutate( Auction = gsub("\\.[0-9]+","", row.names(.)) )
    print(proc.time() - ptm)

    # Filter out items from old auctions that have passed, add in new ones, and save
    old_items_df  <- try(
        "CSV/items.csv" %>%
        read.csv(stringsAsFactors = F) %>%
        filter(Auction %in% old_auctions_df$title)
    )

    current_items_df <- if( class(old_items_df) == "try-error" )
        items_df else rbind(old_items_df,items_df)

    # Write new data files
    write.csv( current_links, "CSV/known_links.csv", row.names = F )
    write.csv( current_auctions_df, "CSV/auctions.csv", row.names = F )
    write.csv( current_items_df, "CSV/items.csv", row.names = F )
}

######################################
#----- FUNCTION: AUCTION_DETAILS ----#
######################################

# Pulls auction details from an auction description page link
auction_details  <- function(new_links, incr, progress_updater = NULL) {
    if( !is.null( progress_updater )) progress_updater(incr)

    a  <- list()
    a$urlname <- gsub(".*\\?","",new_links)
    cat( sprintf("%-18s", a$urlname ) )

    tmp <- read_html(new_links) %>%
        html_nodes("table tr td")

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
            cat(" |", a$date %>% format(usetz = T),"\n")
        }
    )

    a$title  <- tmp %>% html_nodes("#auction_title") %>% .[[1]] %>% html_text

    # 8th entry / row in table contains auction location
    a$location  <- tmp[[8]] %>% html_text %>% clean_str
    a
}

######################################
#----- FUNCTION: GET_ITEMSLIST ------#
######################################

# Pulls item lists from an auction item list link
get_itemlist  <- function(lnk, incr, progress_updater = NULL) {

    if( !is.null( progress_updater )) progress_updater(incr)

    urlname <-  gsub( ".*\\?", "", lnk) %>% gsub("/.*","",.)
    cat( sprintf("%-18s", urlname ) )

    root_link <- lnk %>% gsub("category/ALL","", .)

    itemlist <- root_link %>%
        gsub("mnlist", "mnprint", .) %>%
        read_html %>%
        html_node("#DataTable") %>%
        html_table(header = T, fill = T) %>%
        mutate( Item = gsub("[.]","", Item),
                Description = parse_description(Description),
                Features = lapply(strsplit(Description, "<<|:>>"), grok_description),
                MSRP = get_MSRP(Features),
                link.item = paste0(root_link, Item))

    cat(" |","itemlist ok")
    #itemlist %>%  print

    n <- nrow(itemlist)
    img_link <- try( itemlist$link.item %>% .[n] %>%
                         read_html %>%
                         html_node("#DataTable") %>%
                         html_node("img") %>%
                         html_attr("src") )

    cat(" |", ifelse( class(img_link) == "try-error", "!img_link missing!" , "img_link present"))

    try({

        # Form an image url by figuring out what goes before the //
        # and what goes after the item number
        img_prefix <- gsub("/[^/]+$", "/", img_link)

        # img_suffix <- gsub(paste0(img_prefix, itemlist$Item[n]), "", img_link)
        img_suffix <- gsub(img_prefix, "", img_link) %>%
            gsub("[a-zA-Z]*[0-9]+","",.)

        itemlist %>% mutate(img_src = paste0(img_prefix, Item, img_suffix)) %>%
            select(-Features)

    })

}

######################################
#----- FUNCTION: VALIDATE_LINKS -----#
######################################

# Checks whether auction links and consistent, valid, and bidfta-hosted.
validate_links <- function(lnks) {

    # Fix links that lead directly to page itemlist instead of auction description
    fix_these  <- grep("mnlist",lnks)
    if (length(fix_these) > 0 ) {
        lnks[fix_these] <- lnks[fix_these] %>%
            gsub("mnlist","mndetails",.) %>%
            sub("/category/ALL","",.)
    }

    # Filter out blank links
    lnks <- lnks[lnks != '']

    # Filter out links on other auction sites
    bidfta_hosted <- grepl("bidfta", lnks, ignore.case = T)

    cat( sum(bidfta_hosted) ,"New valid auction links found\n")
    cat( sum(!bidfta_hosted) , "New external auctions were ignored:\n")
    lnks[!bidfta_hosted] %>% paste("*", . , collapse = "\n") %>%
        cat("\n")

    lnks <- lnks[bidfta_hosted]
}
