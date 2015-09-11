# ui.R

## STUFFFF

library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "FastTrack Bidder",
    dropdownMenu(type = "notifications",
      notificationItem(
        text = "Shit is happening",
        icon("calendar")
      )
    )  
  ),
  
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
      label = "Search..."),
    sidebarMenu(id = "tabs",
                tags$li(class = "header", "MAIN MENU"),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
                         badgeLabel = "new", badgeColor = "green"),
                menuItem("Auctions", tabName = "widgets", icon = icon("th")),
                menuItem("Search", tabName = "search", icon = icon("search")),
                menuItem("Options", tabName = "options", icon = icon("cog"))
    ),
    
    selectInput("locSelect", label = "Select Locations",  multiple = TRUE,
      choices = list("temp" = NULL), 
      selected = NULL)
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          tags$section(class="content-header",
                       h1("Welcome to FastTrack Bidder",
                          tags$small("Version 0.2")
                       ),
                       #em("Brought to you by BBRI (Beaver Brigade Research & Innovation)"),
                       
                       tags$ol(class="breadcrumb", 
                               tags$li("Brought to you by BBRI (Beaver Brigade Research & Innovation)"),
                               tags$li(
                                 a(href="#",tags$i(class="fa fa-dashboard"),'Home')
                               ),
                               tags$li(class="active", "Dashboard")
                       )
          ),
          br(),
              
                        
          infoBox("Last Scraped", width = 4,
                  textOutput("lasttime"),
                  icon = icon("calendar"), fill = F, color = "green"
          ),
          infoBox("Local Auctions", width = 4,
                  textOutput("numauctions"),
                  icon = icon("calendar"), fill = F, color = "aqua"
          ),
          infoBox("Ending Soon", width = 4,
                  textOutput("endingsoon"),
                  icon = icon("calendar"), fill = F, color = "yellow"
          )
        ),
        fluidRow(
          column(8),
          column(4,
                 box(title = "Wishlist",
                     width = 12,
                     collapsible = TRUE,
                     # Output wishlist
                     DT::dataTableOutput('wishlist'),
                     br(),
                     # Add term / remove selected
                     div(class = "input-group",
                         tags$input(
                           id = "add_term",
                           type = "text",
                           class = "form-control",
                           placeholder = "Add term"
                         ),
                         
                         div(class = "input-group-btn",
                             actionButton(
                               "add", label = "", icon = icon("plus-square-o") #class = "btn-info"
                             ),
                             actionButton(
                               "remove_selected", label = "", icon = icon("minus-square-o") #class = "btn-info"
                             )
                         )
                     )
                 )
          )
        )
      ),
      
      tabItem(tabName = "widgets",
              h2("Auction List"),
        dataTableOutput(outputId="auctions_df")
      ),
      tabItem(tabName = "search",
        h2("Search Results"),
        dataTableOutput(outputId="search_df")
      ),
      tabItem(tabName = "options",
              h2("Configuration Options")
      )
    )
  )
  
)
