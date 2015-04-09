# Import libraries
library(shiny)
library(leaflet)
library(sp)
library(ggplot2)
library(labeling)
library(maptools)
library(spatstat)
library(spdep)
library(classInt)
library(rgdal)
library(RJSONIO)
library(shinysky)

save(attribute_url,file="attribute_url.Rda")
save(attributes,file="attributes.Rda")
save(attribute_column,file="attribute_column.Rda")

#conn <- redshift.connect("jdbc:postgresql://localhost:5432/phamminhkhoa", "phamminhkhoa", "")
# conn is just a regular RJDBC connection object

# we can retrieve a list of tables
#tables <- redshift.tables(conn)

#strSQL = "
#SELECT gid, geom, \"crime id\", \"reported b\", \"falls with\", longitude, latitude, location, \"lsoa code\", \"lsoa name\", \"crime type\", \"last outco\", context
#FROM combined_point_crime"
#dfTemp <- dbGetQuery(conn, strSQL)
#row.names(dfTemp) = dfTemp$gid
#locationsDD <- SpatialPointsDataFrame(dfTemp, dfTemp[-2])

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Create the map; this is not the "real" map, but rather a proxy
  # object that lets us control the leaflet map on the page.
  map1 <- createLeafletMap(session, 'map1')
  map2 <- createLeafletMap(session, 'map2')
  
  evt1 <- reactive({
    print(input$select1)
    input$select1
  })
  
  evt2 <- reactive({
    input$select2
  })
  
  mapType1 <- reactive({
    input$mapType1
  })
  
  mapType2 <- reactive({
    input$mapType2
  })
  
  rangeType <- reactive({
    input$rangeType
  })
  
  mapZoom1 <- reactive({
    ifelse(is.null(input$map1_zoom),10,input$map1_zoom)
  })
  
  mapZoom2 <- reactive({
    ifelse(is.null(input$map2_zoom),10,input$map2_zoom)
  })
  
  inputFile <- reactive({
    input$file
  })
  
  #Write to json and add to attribute there's an input file from user
  observe({
    if(!is.null(inputFile())) {
      if(input$attribute_name == "" || input$column_used == "") {
        showshinyalert(session,"upload_alert",
                       paste("You haven't specified attribute name or column name.Please specify and upload again.")
                       ,"warning")
      } else {
        attribute_url[input$attribute_name] = inputFile()$datapath
        attributes[input$attribute_name] = input$attribute_name
        attribute_column[input$attribute_name] = input$column_used
        save(attribute_url,file="attribute_url.Rda")
        save(attributes,file="attributes.Rda")
        save(attribute_column,file="attribute_column.Rda")
        
        # Update select input choices
        updateSelectInput(session,
                          "select1",
                          choices = list(
                            "All" = c("All" = "All"),
                            "Attribute" = attributes,
                            "Crime Type" = crimeTypes
                          )
        )
        
        # Show success message to the user
        showshinyalert(session,"upload_alert",
                       paste("Data uploaded successfully")
                       ,"success")
      }
    }
  })
  
  #Define the default map zoom
  oldMapZoom1 = 10
  oldMapZoom2 = 10
  
  observe({
    if(!is.null(mapZoom1())) {
      if(mapZoom1() != oldMapZoom1) {
        if(mapZoom1() != mapZoom2()) {
          map2$setView(input$map2_lat, input$map2_lng, mapZoom1())
        }
        oldMapZoom1 = mapZoom1()
      }
    }
  })
  
  observe({
    if(!is.null(mapZoom2())) {
      print("hihi")
      if(mapZoom2() != oldMapZoom2) {
        print("haha")
        if(mapZoom2() != mapZoom1()) {
          print(input$map1_lat)
          print(input$map1_lng)
          print(mapZoom2())
          map1$setView(input$map1_lat, input$map1_lng, mapZoom2())
          print(input$map1_zoom)
        }
        oldMapZoom2 = mapZoom2()
      }
    }
  })
  
  observe({
    load("attribute_url.Rda")
    load("attributes.Rda")
    load("attribute_column.Rda")
    if(evt1() == "All"){
      vnm_inc <- readOGR("Crime-polygon/all_crime.geojson","OGRGeoJSON", verbose = FALSE)
      geojson1 <- fromJSON("Crime-polygon/all_crime.geojson")
    }else{
      print(attribute_url);
      vnm_inc <- readOGR(attribute_url[evt1()],"OGRGeoJSON", verbose = FALSE)
      geojson1 <- fromJSON(attribute_url[evt1()])
    }
    
    #create a (QUEEN) contiguity based neighbours
    vnm_cnq <- poly2nb(vnm_inc)
    
    #row-standardised weights matrix
    vnm_cnq_rsw <- nb2listw(vnm_cnq)
    
    #output$moran1 <- renderTable({
    #compute Moran's I
    #moran.test(vnminc$CRDENSITY, listw=vnm_cnq_rsw)
    #})
    
    #compute Monte Carlo Moranís I
    set.seed(1234)
    bperm =moran.mc(vnm_inc[[attribute_column[evt1()]]], listw=vnm_cnq_rsw, nsim=999)
    
    #computing local moran stats
    fips <- order(vnm_inc$row)
    nclocI <- localmoran(vnm_inc[[attribute_column[evt1()]]], vnm_cnq_rsw)
    
    #scatterplot of local moran
    nci <- moran.plot(vnm_inc$row, vnm_cnq_rsw, labels=as.character(vnm_inc$MSOA11NM), xlab="Crime Density", ylab="SL Crime Density")
    
    if(mapType1() == "Choropleth Map") {
      if(rangeType() == "Equal Range") {
        max1 = 0
        for(k in 1:length(geojson1$features)) {
          value = geojson1$features[[k]]$properties[[attribute_column[evt1()]]]
          if(value > max1) {
            max1 = value
          }
        }
        
        # Prepare data for the legend table
        densityBreaks <- c(0, max1*0.125, max1*0.25, max1*0.375, max1*0.5, max1*0.675, max1*0.75,
                           max1*0.875, max1)
        densityRanges <- data.frame(
          from = head(densityBreaks, length(densityBreaks)-1),
          to = tail(densityBreaks, length(densityBreaks)-1)
        )
        palette <- c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C",
                     "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
        
        for(k in 1:length(geojson1$features)) {
          value = geojson1$features[[k]]$properties[[attribute_column[evt1()]]]
          geojson1$features[[k]]$style <- c(
            "fillColor" = getColor(value/max1),
            "weight" = 2,
            "opacity" = 1,
            "color" = 'white',
            "dashArray" = '3',
            "fillOpacity" = 0.7
          )
        }  
      } else {
        #jenksBreak <- classIntervals(sapply(lapply(geojson1$features, `[[`, "properties"), '[[', attribute_column[evt1()]), n = 8, style = "jenks")
        jenksBreak <- classIntervals(sapply(lapply(geojson1$features, `[[`, "properties"), '[[', "CRDENSITY"), n = 8, style = "jenks")
        
        # Prepare data for legend table
        densityRanges <- data.frame(
          from = head(jenksBreak$brks, length(jenksBreak$brks)-1),
          to = tail(jenksBreak$brks, length(jenksBreak$brks)-1)
        )
        
        palette <- c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C",
                     "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
        
        for(k in 1:length(geojson1$features)) {
          for(l in 1:(length(jenksBreak$brks)-1)) {
            value = geojson1$features[[k]]$properties[[attribute_column[evt1()]]]
            if((l == 1 && value == 0) ||
                 value > jenksBreak$brks[l] && value <= jenksBreak$brks[l+1]) {
              geojson1$features[[k]]$style <- c(
                "fillColor" = getColorNaturalBreak(l),
                "weight" = 2,
                "opacity" = 1,
                "color" = 'white',
                "dashArray" = '3',
                "fillOpacity" = 0.7
              )  
            }
          }
        } 
      }
    } else {
      #Mapping local outliers
      inf1 <- apply(nci$is.inf, 1, any)
      x <- vnm_inc[[attribute_column[evt1()]]]
      lhx <- cut(x, breaks=c(min(x), mean(x), max(x)), labels=c("L", "H"), include.lowest=TRUE)
      wx <- lag(vnm_cnq_rsw, vnm_inc[[attribute_column[evt1()]]])
      lhwx <- cut(wx, breaks=c(min(wx), mean(wx), max(wx)), labels=c("L", "H"), include.lowest=TRUE)
      lhlh <- interaction(lhx, lhwx, inf1, drop=TRUE)
      cols <- rep(1, length(lhlh))
      cols[lhlh == "H.L.TRUE"] <- 2
      cols[lhlh == "L.H.TRUE"] <- 3
      cols[lhlh == "H.H.TRUE"] <- 4
      
      #Prepare data for legend table
      densityRanges <- data.frame(
        from = c("Low","High","Low","High"),
        to = c("Low","Low","High","High")
      )
      palette <- grey.colors(4, 0.95, 0.55, 2.2)
      
      for(k in 1:length(geojson1$features)) {
        value = geojson1$features[[k]]$properties[[attribute_column[evt1()]]]
        geojson1$features[[k]]$style <- c(
          "fillColor" = grey.colors(4, 0.95, 0.55, 2.2)[cols[[k]]],
          "weight" = 2,
          "opacity" = 1,
          "color" = 'white',
          "dashArray" = '3',
          "fillOpacity" = 0.7
        )
      }
    }
    
    session$onFlushed(once=TRUE, function() {
      
      output$moran1Stat <- renderText({
        print(paste("Statistics: ",bperm$statistic))
      })
      output$moran1PValue <- renderText({
        print(paste("p-value: ",bperm$p.value))
      })
      
      #histogram
      output$hist1 <- renderPlot({
        h <- ({
          hist(bperm$res, freq=TRUE, breaks=20, xlab="Simulated Moran's I", main= "Histogram from Monte Carlo Simulation")
          abline(v=unname(bperm$statistic), col="red")
        })
        print(h)
        # it'll show the moran's I statistics and a histogram (to test normality, in addition to p-value)
      })
      
      output$scatterPlot1 <- renderPlot({
        nci
      })
      map1$addGeoJSON(geojson1)
      output$legendTable1 <- renderUI({
        tags$table(class = "table",
                   tags$tbody(
                     mapply(function(from, to, color) {
                       tags$tr(
                         tags$td(tags$div(
                           style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
                         )),
                         tags$td(from, "-", to)
                       )
                     }, densityRanges$from, densityRanges$to, palette, SIMPLIFY=FALSE)
                   )
        )
      })
    })
  })
  
  values <- reactiveValues(selectedFeature1 = NULL,selectedFeature2 = NULL)
  
  observe({
    event1 <- input$map1_click
    if (is.null(event1))
      return()
    
    isolate({
      # An empty part of the map was clicked.
      # Null out the selected feature.
      values$selectedFeature1 <- NULL
    })
  })
  
  observe({
    event1 <- input$map1_geojson_click
    if (is.null(event1))
      return()
    
    isolate({
      # A GeoJSON feature was clicked. Save its properties
      # to selectedFeature.
      values$selectedFeature1 <- event1$properties
    })
  })
  
  output$details1 <- renderText({
    # Render values$selectedFeature, if it isn't NULL.
    if (is.null(values$selectedFeature1))
      return(NULL)
    
    as.character(tags$div(
      tags$h3(values$selectedFeature1$MSOA11NM),
      tags$div(
        evt1(),"'s ",attribute_column[evt1()],": ",
        values$selectedFeature1[[attribute_column[evt1()]]]
      )
    ))
  })
  
  
  observe({
    if(evt2() == "All"){
      vnm_inc2 <- readOGR("Crime-polygon/all_crime.geojson","OGRGeoJSON", verbose = FALSE)
      geojson2 <- fromJSON("Crime-polygon/all_crime.geojson")
    }else{
      vnm_inc2 <- readOGR(attribute_url[evt2()],"OGRGeoJSON", verbose = FALSE)
      geojson2 <- fromJSON(attribute_url[evt2()])
    }
    
    max2 = 0
    for(j in 1:length(geojson2$features)) {
      value = geojson2$features[[j]]$properties[[attribute_column[evt2()]]]
      if(value > max2) {
        max2 = value
      }
    }
    
    for(j in 1:length(geojson2$features)) {
      value = geojson2$features[[j]]$properties[[attribute_column[evt2()]]]
      geojson2$features[[j]]$style <- c(
        "fillColor" = getColor(value/max2),
        "weight" = 2,
        "opacity" = 1,
        "color" = 'white',
        "dashArray" = '3',
        "fillOpacity" = 0.7
      )
    }
    
    session$onFlushed(once=TRUE, function() {
      map2$addGeoJSON(geojson2,"choropleth")
      
      #create a (QUEEN) contiguity based neighbours
      
      vnm_cnq2 <- poly2nb(vnm_inc2)
      
      #row-standardised weights matrix
      vnm_cnq_rsw2 <- nb2listw(vnm_cnq2)
      
      #output$moran2 <- renderTable({
      #compute Moran's I
      #moran.test(vnminc2$CRDENSITY, listw=vnm_cnq_rsw2)
      #}
      
      #compute Monte Carlo Moranís I
      set.seed(1234)
      bperm2 = moran.mc(vnm_inc2$CRDENSITY, listw=vnm_cnq_rsw2, nsim=999)
      
      output$moran2Stat <- renderText({
        print(paste("Statistics: ",bperm2$statistic))
      })
      output$moran2PValue <- renderText({
        print(paste("p-value: ",bperm2$p.value))
      })
      
      #histogram
      output$hist2 <- renderPlot({
        h2 <- ({
          hist(bperm2$res, freq=TRUE, breaks=20, xlab="Simulated Moran's I", main= "Histogram from Monte Carlo Simulation")
          abline(v=unname(bperm2$statistic), col="red")
        })
        print(h2)
        # it'll show the moran's I statistics and a histogram (to test normality, in addition to p-value)
      })
      
      output$scatterPlot2 <- renderPlot({
        #computing local moran stats
        fips2 <- order(vnm_inc2$row)
        nclocI2 <- localmoran(vnm_inc2$CRDENSITY, vnm_cnq_rsw2)
        
        #scatterplot of local moran
        nci2 <- moran.plot(vnm_inc2$row, vnm_cnq_rsw2, labels=as.character(vnm_inc2$MSOA11NM), xlab="Crime Density", ylab="SL Crime Density")
      })
    })
  })
  
  # Chart
  
  #output$bargraph <- renderPlot({
  #  df = read.csv('~/Downlosads/all_crime2.csv')
  #  p <- ggplot(data=df,
  #              aes(x="Bar Graph",
  #                  y=E02000011,
  #                  fill = factor(crimetype)
  #              )
  #  )
  #  
  #  p=p + geom_bar(stat="identity") 
  #  
  #  print(p)
  #})
  
})
