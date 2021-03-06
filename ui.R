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
      # Search bar
      tags$form( class = "sidebar-form",
          div( class = "input-group",
               tags$input( id = "searchText", type = "text",
                           class = "form-control", placeholder = "Search..."),
               span(class = "input-group-btn",
                    tags$button(id = "searchButton", #type = "submit",
                                class = "btn btn-flat action-button", shiny::icon("search"))
               )
          )
      ),


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
                                      tags$small(version)
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
                      box(title = paste("What's New in Version", version, "!?!"), width = 12,
                          collapsible = TRUE, collapsed = T,
                          uiOutput("updates")),
                      br(),
                      infoBox("Ending Soon", width = 5,
                              icon = icon("clock-o"), fill = F, color = "yellow",
                              uiOutput("endingsoon")),
                      infoBox("Local Auctions", width = 3,
                              icon = icon("calendar"), fill = F, color = "aqua",
                              textOutput("numauctions")),
                      infoBox("Last Scraped", width = 4,
                              textOutput("lasttime"),
                              icon = icon("calendar"), fill = F, color = "green")),
                  fluidRow(
                      box(title = "Recent Searches", width = 8, collapsible = TRUE,
                          # uiOutput("recent_searches")
                          bubblesOutput("recent_searches", width = "100%", height = 350)
                      ),
                      box(title = "Wishlist",
                          # class = "col-xs-6 col-lg-4",
                          width = 4,
                          collapsible = TRUE,
                          # Output wishlist
                          DT::dataTableOutput('wishlist'),
                          br(),
                          # Add term / remove selected
                          div(class = "input-group",
                              div( class = "input-group-btn",
                                    actionButton( "wishlist_search", label = "",
                                                  icon = icon("search"), class = "btn-primary")
                              ),
                              tags$input(
                                  id = "wishlist_add_term",
                                  type = "text",
                                  class = "form-control",
                                  placeholder = "Add term"
                              ),

                              div(class = "input-group-btn",
                                  actionButton(
                                      "wishlist_add", label = "", icon = icon("plus-square-o")
                                  ),
                                  actionButton(
                                      "wishlist_remove_selected", label = "", icon = icon("minus-square-o")
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
                      selected = "Pins",

                      tabPanel( "Pins",
                                div( id="wrapper",
                                    div( id="columns",
                                         uiOutput( outputId="pins_div" )
                                    )
                                )
                      ),
                      tabPanel( "List",
                                DT::dataTableOutput(outputId="search_df")
                      ),
                      tabPanel( "Modal",
                                '<a class="btn btn-default"
                                    data-toggle="modal"
                                    data-keyboard="true"
                                    data-target="#bidfta_modal"
                                    href="http://www.google.com">Click to open Modal</a>
                                <div class="modal fade" id="bidfta_modal"
                                    tabindex="-1" role="dialog">
                                    <div class="modal-dialog modal-lg">
                                        <div class="modal-content">
                                            <div class="modal-body">
                                                <p>One fine body…</p>
                                            </div>
                                        </div>
                                        <!-- /.modal-content -->
                                    </div>
                                    <!-- /.modal-dialog -->
                                </div>' %>% HTML
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

                    h3(class="control-sidebar-heading",
                        "Search Settings"
                    ),

                    ## <!-- /.form-group -->
                    div(class="form-group",

                         #### ------ WRAP_WHOLE -------- ####
                         settingInput(inputID = "wrap_whole", label = "Search whole words",
                                      description = "Search terms wrapped in &#92;W",
                                      default_value = T
                         ),

                        #### ------ RECENT_SEARCHES_N -------- ####
                        settingInput(inputID = "recent_searches_N",
                                     label = "Recent search terms on chart",
                                     description = "Maximum number of search terms to show on the bubble chart",
                                     default_value = 25, type = "slider",
                                     min = 10, max = 100, ticks = F
                        ),

                         #### ------ SPLIT_SEARCH_STR -------- ####
                        settingInput(inputID = "split_search_str", label = "Separate search terms",
                                     description = "Break up compound searches into individual search terms on the home page bubble chart",
                                     default_value = T
                        )
                    )
               )
          )
      )
  )
)



