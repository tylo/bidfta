# server.R

if (curtime >= lasttime + auto_refresh_time) {
    rescrape()

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

############ SERVER ##################

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


    ### OUTPUT: SEARCH_DF ####
    output$search_df  <- renderDataTable({
        if (is.null(search_res()))
            return()

        search_res() %>%
            mutate(
                Photo = paste0(
                    '<a href="',
                    link,
                    '" target="_blank"><img src="',
                    img_src,
                    '" class="img-rounded" width="250"/></a>'
                ),
                Description = paste0(
                    Description,
                    Description %>%
                        gsub("((Item)? ?Description|Brand): ?", " ", .) %>%
                        gsub("(Additional Information|MSRP|Retail):.*", "", .) %>%
                        gsub("(\t|\n|\r).*", "", .) %>%
                        gsub(" ", "+", .) %>%
                        paste0('<br><br><a href="', amazon_base, . ,
                               '" target="_blank"><i class="fa fa-external-link-square fa-lg">
                                  </i></a>')
                )
            ) %>%
            select(Photo, Description, Item, Auction)

    }, escape = F)

    search_res  <- eventReactive(input$searchButton , {
        phrase = input$searchText

        items_df$Description %>%
            grep(paste0(paste0("(^|\\W)",phrase, "($|\\W)")), ., value = F, ignore.case = T) %>%
            items_df[.,]
    })

    observeEvent(input$searchButton, {
        updateTabItems(session, "tabs", "search")
    })


}
