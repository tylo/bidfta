# ui.R

## STUFFFF

library(shiny)
library(shinydashboard)

ui <- dashboardPage( skin = "black",
  dashboardHeader( title = "FastTrack Bidder",

    dropdownMenu( type = "notifications",
      notificationItem(
        text = "Shit is happening",
        icon("calendar")
      )
    ),

    tags$li( class = "dropdown user user-menu",
             a( href="#", class="dropdown-toggle",
                'data-toggle'="dropdown", 'aria-expanded'="false",
                tags$img( src="assets/user2-160x160.jpg", class="user-image", alt="User Image"),
                tags$span( class="hidden-xs", "Eugene Pyatigorsky")
             ),
             HTML(
                 '<ul class="dropdown-menu">
              <!-- User image -->
                 <li class="user-header">
                 <img src="dist/img/user2-160x160.jpg" class="img-circle" alt="User Image">

                 <p>
                 Alexander Pierce - Web Developer
                 <small>Member since Nov. 2012</small>
                 </p>
                 </li>
                 <!-- Menu Body -->
                 <li class="user-body">
                 <div class="row">
                 <div class="col-xs-4 text-center">
                 <a href="#">Followers</a>
                 </div>
                 <div class="col-xs-4 text-center">
                 <a href="#">Sales</a>
                 </div>
                 <div class="col-xs-4 text-center">
                 <a href="#">Friends</a>
                 </div>
                 </div>
                 <!-- /.row -->
                 </li>
                 <!-- Menu Footer-->
                 <li class="user-footer">
                 <div class="pull-left">
                 <a href="#" class="btn btn-default btn-flat">Profile</a>
                 </div>
                 <div class="pull-right">
                 <a href="#" class="btn btn-default btn-flat">Sign out</a>
                 </div>
                 </li>
                 </ul>'
             )
    ),
    tags$li( class="dropdown",
             a( href="#", 'data-toggle'="control-sidebar",
                tags$i( class="fa fa-gears")
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
      tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "fasttrack.css")
      ),

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

                      infoBoxOutput("endingsoon"),

                      #           infoBox("Ending Soon", width = 4,
                      #                   textOutput("endingsoon"),
                      #                   icon = icon("calendar"), fill = F, color = "yellow"
                      #           ),
                      infoBox("Local Auctions", width = 4,
                              icon = icon("calendar"), fill = F, color = "aqua",
                              textOutput("numauctions")
                      )
                  ),
                  fluidRow(
                      column(8,
                             box(title = "Watching",
                                 width = 12,
                                 collapsible = TRUE,
                                 div(class = "input-group-btn",
                                     actionButton(
                                         "remove_watchlist", label = "", icon = icon("minus-square-o") #class = "btn-info"
                                     )
                                 )
                             )

                      ),
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
                  tabBox(
                      width = "100%",
                      selected = "List",

                      tabPanel( "Pins",
                                div( id="wrapper",
                                    div( id="columns",
                                         uiOutput( outputId="pins_div" )
                                    )
                                )
                      ),
                      tabPanel( "List",
                                DT::dataTableOutput(outputId="search_df")
                      )
                  )
          )
      ),
      tags$aside(
          class="control-sidebar control-sidebar-dark",
          HTML('<ul class="nav nav-tabs nav-justified control-sidebar-tabs">
                        <li class="active"><a href="#control-sidebar-settings-tab" data-toggle="tab"><i class="fa fa-wrench"></i></a></li>
                        <li><a href="#control-sidebar-home-tab" data-toggle="tab"><i class="fa fa-home"></i></a></li>
                   </ul>'),
          div( class = "tab-content",
               div( class = "tab-pane active", id="control-sidebar-settings-tab",

                    h3( class="control-sidebar-heading",
                        "Search Settings"
                    ),

                    ## <!-- /.form-group -->
                    div( class="form-group",
                         tags$label( class="control-sidebar-subheading",
                                     "Search whole words",
                                     tags$input( type="checkbox", class="pull-right",
                                                 id = "wrap_whole", checked = "")
                         ),
                         p( HTML("Search terms wrapped in &#92;W" ))
                    )
               )
          )
      )
  )
)



