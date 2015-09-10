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
                                 usetz = T, tz = "EST"), 
        "\n")
  }
  
  cat("Current Time: ", format(curtime, time_file_format, 
                               usetz = T, tz = "EST"), 
      "\n")
  cat("Refresh Due:  ",format(lasttime + auto_refresh_time, time_file_format, 
                              usetz = T, tz = "EST"), 
      "\n\n")
  
  
  
  if (curtime >= lasttime + auto_refresh_time) {
    rescrape()
  } 
  
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

  #### OBSERVER: WISHLIST OUTPUT ####
  observeEvent(wishlist$data,  {
    
    tmp <- wishlist$data
    cat("\nWishlist Terms:", tmp %>% paste(collapse = " | "), '\n\n')
    output$wishlist <- DT::renderDataTable(tmp %>% data.frame,
                                           rownames = FALSE,
                                           colnames = "Wishlist",
                                           #width = "50%",
                                           server = F,
                                           options = list(dom = 't',
                                                          pageLength = 25)
    )
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
  
  #### OBSERVER: ADD REMOVE_SELECTED ####
  observeEvent(input$remove_selected, {
    
    print("Clicked: Remove")
    tmp <- input$wishlist_rows_selected
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
    lasttime %>% format("%a %b %d, %I:%M %p", usetz = T, tz = "EST")
  )
  
  ### OUTPUT: AUCTIONS_DF ###
  output$auctions_df  <- renderDataTable({
    auctions_df %>%
      mutate(title = paste0('<a href="', link, '" target="_blank">',title,'</a>')) %>%
      select(date, title, location)
  }, escape = F)
  
  
  ### OUTPUT: SEARCH_DF ###
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
            paste0('<br><br>',
              '<a href="', amazon_base, . ,
              '" target="_blank"><i class="fa fa-external-link-square fa-lg">
                                  </i></a>'
            )
        )
      ) %>%
      select(Photo, Description, Item, Auction)
    
  }, escape = F)
  
  search_res  <- eventReactive(input$searchButton , {
    phrase = input$searchText
    
    items_df$Description %>%
      grepl(phrase, ., ignore.case = T) %>%
      items_df[.,]
  })
  
  observeEvent(input$searchButton, {
    updateTabItems(session, "tabs", "search")
  })
  
  
}
