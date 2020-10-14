dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      id = "menutabs",
      menuItem("Artist Search", tabName = "input", icon = icon("music")),
      menuItem("Smart Playlist", tabName = "smartplay", icon = icon("play"))
      )
    ),
    dashboardBody(
      # shinyDashboardThemes(
        # theme = "blue_gradient"
        # ),
      customTheme,
      tags$head(
        tags$link(href="https://www.w3schools.com/w3css/4/w3.css", rel="stylesheet"),
        tags$style(HTML("
        .sidebar-toggle {
        display:none;
        }
        label{
        color: white;
        }
        .content-wrapper{
        background: black;
        }
        
        #tableHTML_header_1{
        display:none;
        }
        
        .mySlides {display:none;}
        #playlistupdated table{
        border-radius: 25px;
        background-color: #1DB954;
        color: white;
        border-collapse: collapse;
        border-top: 0px;
        text-align: center;
                        }
        #playlistupdated table thead{
        display: none;
                        }
        #playlistupdated table td{
        border-top: 0px;
                        }"))
      ),
      tabItems(
        tabItem(tabName = "input",
                fluidRow(
                  column(10, 
                         textInput(inputId = "artist", label = "Search Artist"),
                         actionButton(inputId = "artistsearch", label = "Search")
                         )
                ),
                fluidRow(
                  column(10,
                         uiOutput(outputId = "albumimage"))
                )
      ),
      tabItem(tabName = "smartplay",
              fluidRow(
                # column(4,
                       # shiny::HTML("")),
                column(6, 
                       textInput(inputId = "playlist", label = "Input Playlist ID"),
                       actionButton(inputId = "playlistsearch", label = "Search")
                )
              ),
              fluidRow(column(10, shiny::HTML("<br>"))),
              fluidRow(
                # column(4,
                #        shiny::HTML("")),
                column(6,
                       sortableTableOutput(outputId = "playlistupdated"))
              )
      )
    )
  )
)