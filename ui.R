# ui.R
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
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
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
        badgeLabel = "new", badgeColor = "green"),
      menuItem("Auctions", tabName = "widgets", icon = icon("th")),
      menuItem("Search", tabName = "search", icon = icon("search"))
    ),
    
    selectInput("locSelect", label = "Select Locations",  multiple = TRUE,
      choices = list("temp" = NULL), 
      selected = NULL)
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          column(width = 8,
            h2("Welcome to FastTrack Bidder"),
            em("Brought to you by BBRI (Beaver Brigade Research & Innovation)"),
            hr()
            #strong("For now, only the top ~50 items from each auction are scraped :-(")
          ),
            
          infoBox("Last Scraped", width = 4,
                  textOutput("lasttime"),
                  icon = icon("calendar"), fill = F, color = "yellow")
        )
      ),
      
      tabItem(tabName = "widgets",
        h2("Auction List"),
        dataTableOutput(outputId="auctions_df")
      ),
      tabItem(tabName = "search",
        h2("Search Results"),
        dataTableOutput(outputId="search_df")
      )
    )
  )
  
)
