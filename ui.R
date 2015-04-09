# Import Shiny and Leaflet packages
library(shiny)
library(leaflet)
library(shinysky)

shinyUI(fluidPage(
  # Basic style
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
      }

    "))
  ),
  
  # Application title
  headerPanel("Sherlock App"),
  
  # Sidebar with a slider input for the number of bins
  sidebarPanel(
    fluidRow(
      column(12,
             h4("Statistical Result")
      )
    ),
    fluidRow(
      tabsetPanel(
        tabPanel("Attribute/Crime Type 1",
                 htmlOutput("details1"),
                 verbatimTextOutput("moran1Stat"),
                 verbatimTextOutput("moran1PValue"),
                 plotOutput('hist1', width='100%', height='250px'),
                 plotOutput('scatterPlot1', width='100%', height='250px')
        ),
        tabPanel("Attribute/Crime Type 2",
                 verbatimTextOutput("moran2Stat"),
                 verbatimTextOutput("moran2PValue"),
                 plotOutput('hist2', width='100%', height='250px'),
                 plotOutput('scatterPlot2', width='100%', height='250px')
        )
      )
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    fluidRow(
      column(12,
             shinyalert("upload_alert", FALSE,auto.close.after = 5)
      )
    ),
    fluidRow(
      column(4,
             textInput("attribute_name",label = h5("New Attribute Name"),value="")
      ),
      column(4,
             textInput("column_used",label = h5("Column To Compare"),value="")
      ),
      column(4,
             fileInput("file", label = h5("Upload data"))
      )
    ),
    fluidRow(
      column(4,
             selectInput("select1", label = h6("Select the first attribute/crime type"), 
                         choices = list(
                           "All" = c("All" = "All"),
                           "Attribute" = attributes,
                           "Crime Type" = crimeTypes
                         )
             )
      ),
      column(4,
             selectInput("select2", label = h6("Select the second attribute/crime type to compare"), 
                         choices = list(
                           "All" = c("All" = "All"),
                           "Attribute" = attributes,
                           "Crime Type" = crimeTypes
                         )
             )
      )
    ),
    fluidRow(
      column(4,
             selectInput("mapType1", label = h6("Select the map type for the first map"), 
                         choices = list(
                           "Choropleth Map" = "Choropleth Map",
                           "Moran I Map" = "Moran I Map"
                         )
             )
      ),
      column(4,
             selectInput("mapType2", label = h6("Select the map type for the second map"), 
                         choices = list(
                           "Choropleth Map" = "Choropleth Map",
                           "Moran I Map" = "Moran I Map"
                         )
             )
      ),
      column(4,
             selectInput("rangeType", label = h6("Select the range type (in case of choropleth map)"), 
                         choices = list(
                           "Equal Range" = "Equal Range",
                           "Natural Break" = "Natural Break"
                         )
             )
      )
    ),
    fluidRow(
      column(9,
             tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
             leafletMap(
               "map1", "100%", 400,
               initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
               options=list(
                 center = c(51.4758, -0.4195),
                 zoom = 10
               )
             )
      ),
      column(3,
             uiOutput("legendTable1")
      )
    ),
    fluidRow(
      column(12,
             leafletMap(
               "map2", "100%", 400,
               initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
               options=list(
                 center = c(51.4758, -0.4195),
                 zoom = 10
               )
             )
      )
    ),
    fluidRow(
      uiOutput("legendTable2")
    )
  )
))