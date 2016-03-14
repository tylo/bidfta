# server.R

############ SERVER ##################

server <- function(input, output, session) {

  ######################################
  #------------------------------------#
  #------- RUN ONCE PER SESSION -------#
  #------------------------------------#
  ######################################

  # Recording current time and checking timestamp from previously downloaded data
  curtime  <- Sys.time()
  lasttime <- try(
    read.csv("CSV/timestamp.csv", stringsAsFactors = F) %>% unlist,
    silent = T)

  if (class(lasttime) == "try-error")
  {
    lasttime <- curtime  - auto_refresh_time
    cat("No Scrape History Found", "\n\n")

  } else {
    lasttime <- lasttime %>%
      strptime(time_file_format) %>%
      as.POSIXct

    cat("Last Scrape:  ", format(lasttime, time_file_format,
                                 usetz = T, tz = "EST5EDT"),
        "\n")
  }

  # Status Updates
  cat("Current Time: ", format(curtime, time_file_format,
                               usetz = T, tz = "EST5EDT"),
      "\n")
  cat("Refresh Due:  ",format(lasttime + auto_refresh_time, time_file_format,
                              usetz = T, tz = "EST5EDT"),
      "\n\n")

  # Rescrape if due time
  if (curtime >= lasttime + auto_refresh_time) {
    rescrape()
  }

  # Filter out auctions that have already passed
  auctions_df  <- "CSV/auctions.csv" %>%
    read.csv(stringsAsFactors = F)

  # Make sure auction expiration has right time zone
  auctions_df$date  <- auctions_df$date %>%
    strptime(time_file_format, tz = "EST5EDT") %>%
      as.POSIXct()

  # After loading auctions, filter out those which have expired
  auctions_df <- auctions_df %>%
      filter(date > curtime)

  # Filter out items from auctions that have passed
  items_df  <- "CSV/items.csv" %>%
    read.csv(stringsAsFactors = F) %>%
    filter(Auction %in% auctions_df$title)

  # Populating the index selector with stuff queried from database
  s_options <- unique(auctions_df$location)
  names(s_options) <- s_options
  updateSelectInput(session, "locSelect",
                    choices = s_options,
                    selected = NULL)



  wishlist <- reactiveValues(data = get_wishlist())



  ######################################
  #------------------------------------#
  #-------- REACTIVE FUNCTIONS --------#
  #------------------------------------#
  ######################################

  #### REACTIVE: SEARCH_RES ####
  search_res  <- eventReactive(input$searchButton , {

      input$searchText %>%
          ifelse( input$wrap_whole, paste0( "\\W",.,'\\W' ), . ) %>%
          print %>%
          grepl( items_df$Description, ignore.case = T ) %>%
          items_df[.,]
  })

  #### OBSERVER: SEARCH BUTTON ####
  observeEvent(input$searchButton, {
    updateTabItems(session, "tabs", "search")
  })


  #### OBSERVER: ADD BUTTON ####
  observeEvent(input$add, {

    print("Clicked: Add")
    tmp <- input$add_term
    if (tmp != "") {

      cat('Adding: ', tmp)
      wishlist$data <- wishlist$data %>% c(tmp)
      wishlist$data %>% save_wishlist
      updateTextInput(session, "add_term",
                      value = "")
    }
  })


  #### OBSERVER: REMOVE_SELECTED BUTTON ####
  observeEvent(input$remove_selected, {

    print("Clicked: Remove")
    tmp <- input$wishlist_rows_selected
    print(tmp)
    if (!is.null(tmp)) {

      cat('Removing: ', wishlist$data[tmp] %>% paste(collapse = " | "))
      wishlist$data <- wishlist$data[-tmp]
      wishlist$data %>% save_wishlist
    }
  })


  ######################################
  #------------------------------------#
  #------- OUTPUT FUNCTIONS -----------#
  #------------------------------------#
  ######################################


  #### OUTPUT: LASTTIME ####
  output$lasttime <- renderText(
    lasttime %>% format("%a %b %d, %I:%M %p", usetz = T, tz = "EST5EDT")
  )


  #### OUTPUT: NUMAUCTIONS ####
  output$numauctions <- renderText(
    auctions_df %>% nrow
  )

  #### OUTPUT: ENDINGSOON ####
  output$endingsoon <- renderInfoBox({
      auc <- auctions_df %>%
          top_n(1, desc(date)) %>%
          mutate(pretty_date = format(date, "%I:%M %p", tz = "EST5EDT"),
                 pretty_name = gsub(",.*", "", title),
                 pretty = paste(pretty_date, pretty_name)
          )

      auc_html <- a(auc$pretty,
                    href = auc$link,
                    target="_blank")

      auc %>% print

      infoBox(
          "Ending Soon",
          auc_html,
          icon = icon("clock-o"),
          color = "yellow"
      )
  })



  #### OUTPUT: WISHLIST OUTPUT ####
  observeEvent(wishlist$data,  {

    tmp <- wishlist$data
    cat("\nWishlist Terms:", tmp %>% paste(collapse = " | "), '\n\n')
    output$wishlist <- DT::renderDataTable(tmp %>% data.frame,
                                           rownames = FALSE,
                                           colnames = NULL,
                                           #style = 'bootstrap',
                                           #width = "50%",
                                           server = F,
                                           class = 'hover',
                                           options = list(dom = 't',
                                                          pageLength = 25)
    )
  })


  ### OUTPUT: AUCTIONS_DF ###
  output$auctions_df  <- renderDataTable({auctions_df %>%
                                           mutate(title = paste0('<a href="', link, '" target="_blank">',title,'</a>')) %>%
                                           select(date, title, location)},
                                         escape = F
  )


  ### OUTPUT: PINS_DIV ###
  output$pins_div <- renderUI({
      validate(
          need( search_res(), "Hello")
      )

      search_res() %>%
          transmute(newcol = gen_pins( clean_description(Description), link, img_src )) %>%
          unlist %>%
          paste0( collapse="" ) %>%
          HTML
  })


  ### OUTPUT: SEARCH_DF ###
  output$search_df  <- DT::renderDataTable({
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
          Description %>% clean_description,
          Description %>%
            gsub("((Item)? ?Description|Brand): ?", " ", .) %>%
            clean_description %>%
            gsub(" ", "+", .) %>%
            paste0('<br><br>',
              '<a href="', amazon_base, . ,
              '" target="_blank"><i class="fa fa-external-link-square fa-lg">
                                  </i></a>'
            )
        )
      ) %>%
      select(Photo, Description, Item, Auction)

  },
  escape = F,
  extensions = 'Scroller',
  class = 'hover',
  options = list(dom = 'fritS',
                 scrollY = 800,
                 autoWidth = TRUE,
                 scrollCollapse = TRUE)
  )

  # User logging if checkbox is enabled for wrap_whole
  observeEvent( input$wrap_whole, {
      var <- "wrap_whole"
      cat( var, ":", input %>% .[[var]], "\n" )
  })
}
