# devtools::install_github('charlie86/spotifyr')
library(spotifyr)
library(dplyr)
library(ggplot2)
library(cluster)
library(forcats)
library(gganimate)
library(gifski)
library(imager)
library(png)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(dashboardthemes)
library(magick)
library(shinybusy)
library(shinycssloaders)
library(shinyjs)
library(RSQLite)
library(xtable)
library(tableHTML)
library(grid)
library(gridExtra)
library(shinyjqui)

## Need to set api keys in Rprofile file.
## Use file.edit(".Rprofile")
## In this file do 
## Sys.setenv(SPOTIFY_CLIENT_ID = '')
## Sys.setenv(SPOTIFY_CLIENT_SECRET = '')

# Initial access to Spotify API
access_token <- get_spotify_access_token()

# dbRemoveTable(conn, "album_art")

# Setup a Database to store the album cover data - speeds up processing time
conn <- dbConnect(RSQLite::SQLite(), "Spotify.db")

# Check if file is available in db, if not create an empty one
tables = dbListTables(conn)

table.check <- c('album_art')

for(f in unique(table.check)){
  if(length(tables[tables == f]) == 0){
    emptydf <- data.frame(hex.col = "", albumart = "", name = "")
    RSQLite::dbWriteTable(conn, f, emptydf)
  }
}


# Create a custom theme for the app
### creating custom theme object
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Helvetica"
  ,appFontColor = "rgb(255,255,255)"
  ,primaryFontColor = "rgb(255,255,255)"
  ,infoFontColor = "rgb(255,255,255)"
  ,successFontColor = "rgb(255,255,255)"
  ,warningFontColor = "rgb(255,255,255)"
  ,dangerFontColor = "rgb(255,255,255)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(0,0,0)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "rgb(0,0,0)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(0,0,0)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(0,0,0)"
    ,colorMiddle = "rgb(0,0,0)"
    ,colorEnd = "rgb(0,0,0)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "0px 0px 0px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(0,0,0)"
  ,sidebarSearchIconColor = "rgb(255,255,255)"
  ,sidebarSearchBorderColor = "rgb(0,0,0)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(0,0,0)"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(30,215,96)"
    ,colorMiddle = "rgb(30,215,96)"
    ,colorEnd = "rgb(30,215,96)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgb(30,215,96)"
    ,colorMiddle = "rgb(30,215,96)"
    ,colorEnd = "rgb(30,215,96)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(30,215,96)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(30,215,96)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(30,215,96)"
  ,textboxBorderColor = "rgb(30,215,96)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(30,215,96)"
  ,textboxBorderColorSelect = "rgb(30,215,96)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


