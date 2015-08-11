# server.R
print(lasttime + auto_refresh_time)
print(curtime)

if ( curtime >= lasttime + auto_refresh_time ) {


  link  <- html("http://bidfta.com/") %>%
    html_nodes(".auction")  %>%
    html_node("a") %>%
    html_attr("href")

  fix_these  <- grep("mnlist",link)

  link[fix_these] <- link[fix_these] %>%
    gsub("mnlist","mndetails",.) %>%
    sub("/category/ALL","",.)

  auctions <- link %>%
    #.[40:60] %>%
    lapply(auction_details) %>%
    .[!sapply(.,is.null)]

  auctions_df <- auctions %>%
    lapply(data.frame, stringsAsFactors=FALSE) %>%
    do.call(rbind, .)

  # Good locations
  good_loc  <- c("Cincinnati", "Sharonville", "West Chester") %>%
    sapply(function(x) grepl(x, auctions_df$location, ignore.case = T)) %>%
    apply(1, any)
  auctions_df <- filter(auctions_df, good_loc)

  # Get Items
  print("GET ITEMS")
  items <- auctions_df$link %>%
    lapply(get_itemlist)
  names(items) <- auctions_df$title

  items_df <- items %>%
    do.call(rbind, .) %>%
    mutate(Auction = gsub("\\.[0-9]+","", row.names(.)))

  write.csv(curtime, "CSV/timestamp.csv", row.names = F)
  write.csv(auctions_df, "CSV/auctions.csv", row.names = F)
  write.csv(items_df, "CSV/items.csv", row.names = F)

} else {

  # Filter out auctions that have already passed
  auctions_df  <- "CSV/auctions.csv" %>%
    read.csv(stringsAsFactors = F) %>%
    filter(date > curtime)

  auctions_df$date  <- auctions_df$date %>%
    strptime(time_file_format) %>%
    as.POSIXct


  # Filter out items from auctions that have passed
  items_df  <- "CSV/items.csv" %>%
    read.csv(stringsAsFactors = F) %>%
    filter(Auction %in% auctions_df$title)
}

server <- function(input, output, session) {

  # Populating the index selector with stuff queried from database
  s_options <- unique(auctions_df$location)
  names(s_options) <- s_options
  updateSelectInput(session, "locSelect",
    choices = s_options,
    selected = NULL)


  output$auctions_df  <- renderDataTable({
    auctions_df %>%
      mutate(title = paste0('<a href="', link, '" target="_blank">',title,'</a>')) %>%
      select(date, title, location)
  }, escape = F)

  output$search_df  <- renderDataTable({
    if (is.null(search_res())) return()

    search_res() %>%
      mutate(Photo = paste0(
        '<a href="',
        link,
        '" target="_blank"><img src="',
        img_src,
        '" class="img-rounded" width="250"/></a>'
        )) %>%
      select(Photo, Description, contains("Current"), Item, Auction)

  }, escape = F)

  search_res  <- eventReactive( input$searchButton , {
    phrase = input$searchText
    items_df$Description %>%
      grep(phrase, ., value = F, ignore.case = T) %>%
      items_df[.,]
  })

  observeEvent(input$searchButton, {
    updateTabItems(session, "tabs", "search")
  })


}




