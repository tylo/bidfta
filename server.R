# server.R

############ SERVER ##################

server <- function(input, output, session) {

  ######################################
  #------------------------------------#
  #------- RUN ONCE PER SESSION -------#
  #------------------------------------#
  ######################################

    # observeEvent(input$wishlist_search , {
    #     na <- names(session$clientData)
    #     lapply(na, function(x) cat(x, session$clientData[[x]],"\n"))
    # })

  # Recording current time and checking timestamp from previously downloaded data
  curtime  <- Sys.time()
  lasttime <- try( read.csv("CSV/timestamp.csv", stringsAsFactors = F, header = F) %>%
        .[,1] %>% tail(1) %>% unlist , silent = T)

  if (class(lasttime) == "try-error") {
    lasttime <- curtime  - auto_refresh_time
    cat("No Scrape History Found", "\n\n")
  }
  else {
    lasttime <- lasttime %>% strptime(time_file_format) %>% as.POSIXct
    cat("Last Scrape:  ", format(lasttime, time_file_format, usetz = T, tz = "EST5EDT"), "\n")
  }

  # Status Updates
  cat("Current Time: ", format(curtime, time_file_format, usetz = T, tz = "EST5EDT"), "\n")
  cat("Refresh Due:  ",format(lasttime + auto_refresh_time, time_file_format,
                              usetz = T, tz = "EST5EDT"), "\n\n")

  # Rescrape if due time
  if (curtime >= lasttime + auto_refresh_time) rescrape()

  # Filter out auctions that have already passed
  # Make sure auction expiration has right time zone
  # After loading auctions, filter out those which have expired
  auctions_df  <-  read.csv( "CSV/auctions.csv", stringsAsFactors = F) %>%
      mutate( date =  strptime(date, time_file_format, tz = "EST5EDT") %>% as.POSIXct ) %>%
      filter(date > curtime)

  # Filter out items from auctions that have passed
  items_df  <- "CSV/items.csv" %>%
    read.csv(stringsAsFactors = F) %>%
    filter(Auction %in% auctions_df$title)

  # Populating the index selector with stuff queried from database
  s_options <- unique(auctions_df$location)
  names(s_options) <- s_options
  updateSelectInput(session, "locSelect", choices = s_options, selected = NULL)

  version_hist <- readLines("VERSION")
  wishlist <- reactiveValues(data = get_wishlist())


  ######################################
  #------------------------------------#
  #-------- REACTIVE FUNCTIONS --------#
  #------------------------------------#
  ######################################


  # ============================================================
  # This part of the code monitors the file for changes once per
  # 60 second (500 milliseconds).
  fileReaderData <- reactiveFileReader(500, session, searches_loc, read.csv)

  #### REACTIVE: SEARCH_RES ####
  output$recent_searches <- renderUI(
      fileReaderData() %>% tail(10) %>% .[,2] %>% paste0(collapse="<br>") %>% HTML
  )

  # ============================================================

  #### REACTIVE: SEARCH_RES ####
  search_res <- reactiveValues( data = NULL )

  #### OBSERVER: SEARCH_INPUT SEARCH ####
  observeEvent(input$searchButton , {
      validate(need( input$searchText, "" ))
      search_res$data <-  do_search(input$searchText,
                                    items_df,
                                    whole_words = input$wrap_whole)
      updateTabItems(session, "tabs", "search")
  })

  #### OBSERVER: WISHLIST_SEARCH ####
  observeEvent(input$wishlist_search , {
      validate(need(input$wishlist_rows_selected, ''))
      search_res$data <- do_search(wishlist$data[input$wishlist_rows_selected],
                                   items_df,
                                   whole_words = input$wrap_whole)
      updateTabItems(session, "tabs", "search")

  })

  #### OBSERVER: ADD BUTTON ####
  observeEvent(input$wishlist_add, {

    print("Clicked: Add")
    tmp <- input$wishlist_add_term
    if (tmp != "") {

      cat('Adding: ', tmp)
      wishlist$data <- wishlist$data %>% c(tmp)
      wishlist$data %>% save_wishlist
      updateTextInput(session, "add_term",
                      value = "")
    }
  })


  #### OBSERVER: REMOVE_SELECTED BUTTON ####
  observeEvent(input$wishlist_remove_selected, {

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


  ##### OUTPUT: UPDATES #####
  output$version <- renderText(
      regmatches(version_hist[1], regexpr("\\d+[.]\\d+",version_hist[1]))
  )

  ##### OUTPUT: UPDATES #####
  output$updates <- renderUI(
      version_hist %>% paste(collapse="") %>% HTML
  )

  #### OUTPUT: LASTTIME ####
  output$lasttime <- renderText(
    lasttime %>% format("%a %b %d, %I:%M %p", usetz = T, tz = "EST5EDT")
  )

  #### OUTPUT: NUMAUCTIONS ####
  output$numauctions <- renderText(
    auctions_df %>% nrow
  )

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
                                             selection = "single",
                                             options = list(dom = 't',
                                                            pageLength = 25)
      )
  })

  #### OUTPUT: ENDINGSOON ####
  output$endingsoon <- renderUI({
      auc <- auctions_df %>%
          top_n(1, desc(date)) %>%
          mutate(pretty_date = format(date, "%I:%M %p", tz = "EST5EDT"),
                 pretty_name = gsub(",.*", "", title),
                 pretty = paste(pretty_date, pretty_name)
          )

      a(auc$pretty, href = auc$link.pageditems, target="_blank")
  })


  ### OUTPUT: AUCTIONS_DF ###
  output$auctions_df  <- renderDataTable({
      auctions_df %>%
          mutate(title = paste0('<a href="', link.pageditems, '" target="_blank">',title,'</a>')) %>%
          select(date, title, location)},
      escape = F
  )




  ### OUTPUT: PINS_DIV ###
  output$pins_div <- renderUI({

      validate(need( nrow(search_res$data) > 0, "No matching items found"))

      search_res$data %>%
          left_join(auctions_df, by=c("Auction"="title")) %>%
          transmute(newcol = gen_pins(Description,
                                      link.item,
                                      img_src,
                                      date)) %>%
          unlist %>%
          paste0( collapse="" ) %>%
          HTML
  })


  ### OUTPUT: SEARCH_DF ###
  output$search_df  <- DT::renderDataTable({
      validate(need( nrow(search_res$data) > 0, "No matching items found"))

    search_res$data %>%
      mutate(
        Photo = paste0(
          '<a href="',
          link.item,
          '" target="_blank"><img src="',
          img_src,
          '" class="img-rounded" width="250"/></a>'
        ),
        Description = paste0(
            Description,
            Description %>%
            gsub("((Item)? ?Description|Brand): ?", " ", .) %>%
            clean_description %>%
            gsub(" ", "+", .) %>%
            paste0('<br><br>',
              '<a href="', amazon_base, . ,
              '" target="_blank"><i class="fa fa-external-link-square fa-lg">
                                  </i></a>'
            ))
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
