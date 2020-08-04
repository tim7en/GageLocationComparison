library (shiny)
library (leaflet)
library (RColorBrewer)
library (ggmap)
library (rmapshaper)
library (foreign)
library (clipr)
library (e1071)
library (gridExtra)
library (labeling)
library (plogr)
library (readr)
library (reshape2)
library (viridisLite)
library (xfun)

################
datas <- read.csv ("datas.csv", stringsAsFactors = F)
datas <- datas[-c(which(datas$state %in% c("HI", "PR", "GM", "GU", "AK"))),]
datas$dif_area_g3 <- abs(datas$area_nav - datas$area_g3)
datas$dif_area_nwis <- abs (datas$area_nav - (datas$area_nwis*2.58999))
datas$dif_area_g3nwis <- abs (datas$area_g3 - (datas$area_nwis * 2.58999))

classify_gages <- function (x, y){
  x$class <- 0
  x$class[which(x[y] >= 0 & x[y] <= 10)] <- 1
  x$class[which(x[y] >= 10 & x[y] <= 50)] <-2
  x$class[which(x[y] >= 50 & x[y] <= 300)] <-3
  x$class[which(x[y] >= 300 & x[y] <= 500)] <-4
  x$class[which(x[y] >= 500 & x[y] <= 1000)] <-5
  x$class[which(x[y] > 1000)] <- 6
  return (x)
}
################

################
datas_nldi <- data.frame (id = datas$id, lat = datas$lat_ss, long = datas$long_ss, proxy = datas$nldi_proxy, ds = "Nldi", state = datas$state, active = datas$active)
datas_nldi <- classify_gages(datas_nldi, "proxy")

datas_g3 <- data.frame (id = datas$id, lat = datas$lat_g3, long = datas$long_g3, proxy = datas$gages3_proxy, ds = "gages3", state = datas$state, active = datas$active)
datas_g3 <- classify_gages(datas_g3, "proxy")

datas_nav <- data.frame (id = datas$id, lat = datas$lat_nav, long = datas$long_nav, proxy = datas$nav_proxy, ds = "SSNav", state = datas$state, active = datas$active)
datas_nav <- classify_gages(datas_nav, "proxy")

datas_nwis <- data.frame (id = datas$id, lat = datas$lat_nwis, long = datas$long_nwis, proxy = datas$nwis_proxy, ds = "Nwis", state = datas$state, active = datas$active)
datas_nwis <- classify_gages(datas_nwis, "proxy")

datas_nhd <- data.frame (id = datas$id, lat = datas$lat_nhd, long = datas$long_nhd, proxy = datas$nhd_proxy, ds = "Nhd", state = datas$state, active = datas$active)
datas_nhd <- classify_gages(datas_nhd, "proxy")

datas <- rbind (datas_g3, datas_nav, datas_nwis, datas_nldi, datas_nhd)
names (datas)[4] <- ("proxy_dist")

datas <- na.omit (datas)
datas_nldi <- na.omit (datas_nldi)
datas_nav <- na.omit (datas_nav)
datas_nwis <- na.omit (datas_nwis)
datas_g3 <- na.omit (datas_g3)
################

# #proximity difference between gages 
# length (which (datas_nldi$class == 6)) #>1000 
# length (which (datas_nav$class == 6))
# length (which (datas_nwis$class == 6))
# length (which (datas_g3$class == 6))
# 
# length (which (datas_nldi$class == 5)) #>500
# length (which (datas_nav$class == 5))
# length (which (datas_nwis$class == 5))
# length (which (datas_g3$class == 5))
# 
# length (which (datas_nldi$class == 4)) #>300
# length (which (datas_nav$class == 4))
# length (which (datas_nwis$class == 4))
# length (which (datas_g3$class == 4))



datas$id <- paste(datas$id, datas$ds, sep = "_")
datas$ds <- as.numeric (datas$ds)

ui <- bootstrapPage(
  tags$style(type = "text/css", 
             "html, body {width:100%;height:100%}",
             ),
  tags$head (includeCSS("style.css")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id  = "controls", top = 30, right = 10, bottom = "auto",
                class = "panel panel-default",
                draggable = T,
                sliderInput("range", "Class", min(datas$class), max(datas$class),
                            value = c(4,5), step = 1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE),
                checkboxInput("activeGages", "Show only active gages", TRUE),
                uiOutput ("url_description"),
                uiOutput ("url_repo"),
                uiOutput ("url_mrd"),
                downloadButton("downloadData", "Download")
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    if (input$activeGages){
      dat <- datas[which(datas$active == 1),] 
    } else {
      dat <- datas
    }
    dat <- dat[dat$class >= input$range[1] & dat$class <= input$range[2],]
    dat
  })
  
  url_repo <- a("Github", href="https://github.com/USGS-WiM/GageLocationComparison")
  output$url_repo <- renderUI({
    tagList("", url_repo)
  })
  
  url_description <- a("Description", href = "https://usgs-wim.github.io/GageLocationComparison/description.html")
  output$url_description <- renderUI({
    tagList("", url_description)
  })
  
  url_mrd <- a("How data generated ?", href = "https://usgs-wim.github.io/GageLocationComparison/gages.html")
  output$url_mrd <- renderUI({
    tagList("", url_mrd)
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, datas$ds)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(datas) %>% addProviderTiles('Esri.WorldImagery') %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("GageLocationComparison", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datas, file, row.names = FALSE)
    }
  )
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = ~10, weight = 1, color = "#777777", opacity = 0.5,
                 fillColor = ~pal(ds), fillOpacity = 0.7, popup = ~paste(id)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = datas)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~ds
      )
    }
  })
}

shinyApp(ui, server)