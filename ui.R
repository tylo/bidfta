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
        DT::dataTableOutput(outputId="search_df")
      ),
      tabItem(tabName = "options",
              h2("Configuration Options")
      )
    )
  ),
tags$aside( class="main-sidebar",
            HTML( 'class="control-sidebar control-sidebar-dark control-sidebar-open">
                  <!-- Create the tabs -->
                  <ul class="nav nav-tabs nav-justified control-sidebar-tabs">
                  <li class="active"><a href="#control-sidebar-theme-demo-options-tab" data-toggle="tab"><i class="fa fa-wrench"></i></a></li><li><a href="#control-sidebar-home-tab" data-toggle="tab"><i class="fa fa-home"></i></a></li>
                  <li><a href="#control-sidebar-settings-tab" data-toggle="tab"><i class="fa fa-gears"></i></a></li>
                  </ul>
                  <!-- Tab panes -->
                  <div class="tab-content">
                  <!-- Home tab content -->
                  <div class="tab-pane" id="control-sidebar-home-tab">
                  <h3 class="control-sidebar-heading">Recent Activity</h3>
                  <ul class="control-sidebar-menu">
                  <li>
                  <a href="javascript::;">
                  <i class="menu-icon fa fa-birthday-cake bg-red"></i>
                  
                  <div class="menu-info">
                  <h4 class="control-sidebar-subheading">Langdon\'s Birthday</h4>
                  
                  <p>Will be 23 on April 24th</p>
                  </div>
                  </a>
                  </li>
                  <li>
                  <a href="javascript::;">
                  <i class="menu-icon fa fa-user bg-yellow"></i>
                  
                  <div class="menu-info">
                  <h4 class="control-sidebar-subheading">Frodo Updated His Profile</h4>
                  
                  <p>New phone +1(800)555-1234</p>
                  </div>
                  </a>
                  </li>
                  <li>
                  <a href="javascript::;">
                  <i class="menu-icon fa fa-envelope-o bg-light-blue"></i>
                  
                  <div class="menu-info">
                  <h4 class="control-sidebar-subheading">Nora Joined Mailing List</h4>
                  
                  <p>nora@example.com</p>
                  </div>
                  </a>
                  </li>
                  <li>
                  <a href="javascript::;">
                  <i class="menu-icon fa fa-file-code-o bg-green"></i>
                  
                  <div class="menu-info">
                  <h4 class="control-sidebar-subheading">Cron Job 254 Executed</h4>
                  
                  <p>Execution time 5 seconds</p>
                  </div>
                  </a>
                  </li>
                  </ul>
                  <!-- /.control-sidebar-menu -->
                  
                  <h3 class="control-sidebar-heading">Tasks Progress</h3>
                  <ul class="control-sidebar-menu">
                  <li>
                  <a href="javascript::;">
                  <h4 class="control-sidebar-subheading">
                  Custom Template Design
                  <span class="label label-danger pull-right">70%</span>
                  </h4>
                  
                  <div class="progress progress-xxs">
                  <div class="progress-bar progress-bar-danger" style="width: 70%"></div>
                  </div>
                  </a>
                  </li>
                  <li>
                  <a href="javascript::;">
                  <h4 class="control-sidebar-subheading">
                  Update Resume
                  <span class="label label-success pull-right">95%</span>
                  </h4>
                  
                  <div class="progress progress-xxs">
                  <div class="progress-bar progress-bar-success" style="width: 95%"></div>
                  </div>
                  </a>
                  </li>
                  <li>
                  <a href="javascript::;">
                  <h4 class="control-sidebar-subheading">
                  Laravel Integration
                  <span class="label label-warning pull-right">50%</span>
                  </h4>
                  
                  <div class="progress progress-xxs">
                  <div class="progress-bar progress-bar-warning" style="width: 50%"></div>
                  </div>
                  </a>
                  </li>
                  <li>
                  <a href="javascript::;">
                  <h4 class="control-sidebar-subheading">
                  Back End Framework
                  <span class="label label-primary pull-right">68%</span>
                  </h4>
                  
                  <div class="progress progress-xxs">
                  <div class="progress-bar progress-bar-primary" style="width: 68%"></div>
                  </div>
                  </a>
                  </li>
                  </ul>
                  <!-- /.control-sidebar-menu -->
                  
                  
                  <!-- Settings tab content -->
                  <div class="tab-pane" id="control-sidebar-settings-tab">
                  <form method="post">
                  <h3 class="control-sidebar-heading">General Settings</h3>
                  
                  <div class="form-group">
                  <label class="control-sidebar-subheading">
                  Report panel usage
                  <input type="checkbox" class="pull-right" checked="">
                  </label>
                  
                  <p>
                  Some information about this general settings option
                  </p>
                  </div>
                  <!-- /.form-group -->
                  
                  <div class="form-group">
                  <label class="control-sidebar-subheading">
                  Allow mail redirect
                  <input type="checkbox" class="pull-right" checked="">
                  </label>
                  
                  <p>
                  Other sets of options are available
                  </p>
                  </div>
                  <!-- /.form-group -->
                  
                  <div class="form-group">
                  <label class="control-sidebar-subheading">
                  Expose author name in posts
                  <input type="checkbox" class="pull-right" checked="">
                  </label>
                  
                  <p>
                  Allow the user to show his name in blog posts
                  </p>
                  </div>
                  <!-- /.form-group -->
                  
                  <h3 class="control-sidebar-heading">Chat Settings</h3>
                  
                  <div class="form-group">
                  <label class="control-sidebar-subheading">
                  Show me as online
                  <input type="checkbox" class="pull-right" checked="">
                  </label>
                  </div>
                  <!-- /.form-group -->
                  
                  <div class="form-group">
                  <label class="control-sidebar-subheading">
                  Turn off notifications
                  <input type="checkbox" class="pull-right">
                  </label>
                  </div>
                  <!-- /.form-group -->
                  
                  <div class="form-group">
                  <label class="control-sidebar-subheading">
                  Delete chat history
                  <a href="javascript::;" class="text-red pull-right"><i class="fa fa-trash-o"></i></a>
                  </label>
                  </div>
                  <!-- /.form-group -->
                  </form>
                  </div>
                  <!-- /.tab-pane -->
                  </div>')
  )
)
