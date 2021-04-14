install.packages("shiny")
install.packages("tidyverse")
install.packages("leaflet")
install.packages("DT")
install.packages("geosphere")
install.packages("raster")
install.packages("tmap")
install.packages("RColorBrewer")
install.packages("rsconnect")
library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(geosphere)
library(raster)
library(tmap)
library(RColorBrewer)
library(rsconnect)

ui <- fluidPage(navbarPage(
    "Farms Selling Poultry and Eggs",
    id = "main",
    tabPanel("Registered Farms Map",leafletOutput("map")),
    tabPanel("Registered Farms in Range",
             textInput("lat", "Enter Latitude"),
             textInput("lon", "Enter Longitude"),
             textInput("range", "Enter distance(KM)"),
             DT::dataTableOutput("data_dist", width = '2000px')),
    tabPanel("Google Places Map", leafletOutput("placesmap"), textOutput("placestext")),
    tabPanel("Google Places Location in Range",
             textInput("lat2", "Enter Latitude"),
             textInput("lon2", "Enter Longitude"),
             textInput("range2", "Enter distance(KM)"),
             DT::dataTableOutput("data_dist2", width = '2000px'))
))

server <- function(input, output) {
    data <- read.csv("allregistries.csv")
    regcols <- c("name", "address","address_google","x","y","website","phone", "NPIP","NPIP Subpart E", 
                 "MDA","VA Grown","Local Harvest","Local Hens","Maryland's Best")
    
    # Function which returns a list of places within a certain distance from a coordinate. 
    distance <- function(default_lat, default_lon, range){
        data2<-data %>% filter(!is.na(x) & !is.na(y))
        dist <- c()
        for (i in 1:nrow(data2)) {
            lat <- data2[i,]$y
            lon <- data2[i,]$x
            temp <-
                distm(c(default_lat, default_lon), c(lat, lon), fun = distHaversine)
            dist <- rbind(dist, temp)
        }
        dist <- dist / 1000
        data3 <- cbind(data2, dist)
        list <- data3 %>%
            filter(dist < range)
        return(list)
    }
    output$data_dist <- DT::renderDataTable(datatable(
        distance(as.numeric(input$lat),as.numeric(input$lon),as.numeric(input$range)),
        colnames = regcols,
        options(searchDelay = 1),
        filter = 'top'
    ) )
    
    #Registered Farm Map#########################################################
    mapdf<-read.csv("allregistries_map.csv")
    col2 <- brewer.pal(n=10, "Spectral")
    color2 <- colorFactor(col2, mapdf$dset)
    
    output$map <- renderLeaflet({
        map <- leaflet(mapdf) %>%
            setView(lng = -77.46, lat = 38.59, zoom = 6) %>%
            addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(
                lng=~x,
                lat=~y,
                radius=3,
                stroke=FALSE,
                fillOpacity=1,
                color=~color2(dset),
                popup=~paste(
                    "<b>", name, "</b><br/>",
                    "address: ", address, "<br/>",
                    "address_google: ", address_google, "<br/>",
                    "website: ", website, "<br/>",
                    "phone: ", phone, "<br/>",
                    "source: ", DataSet, "<br/>"
                )
            ) %>%
            addLegend(
                "bottomleft", # Legend position
                pal=color2, # color palette
                values=~dset, # legend values
                opacity = 1,
                title="Data Set"
            ) 
    })
    
    # Google Places Map
    placesdf<-read.csv("places_review.csv")
    col1 <- rev(brewer.pal(n=10, "Spectral"))
    col1 <- c(col1[1:4],col1[7:9])
    color <- colorFactor(col1, placesdf$allwords)
    
    for_sale <-placesdf %>% filter(str_detect(keyword, 'for sale'))
    hatchery <-placesdf %>% filter(str_detect(keyword, 'hatchery'))
    fair <-placesdf %>% filter(str_detect(keyword, 'fair'))
    market <-placesdf %>% filter(str_detect(keyword, 'market'))
    farms <-placesdf %>% filter(str_detect(keyword, 'farms'))
    poultry_show <-placesdf %>% filter(str_detect(keyword, 'poultry show'))
    
    output$placesmap <- renderLeaflet({
        placesmap <- leaflet(placesdf) %>%
            setView(lng = -77.46, lat = 38.59, zoom = 6) %>%
            addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(data=farms, lng=~lon, lat=~lat, radius=~allwords, stroke=FALSE, 
                             fillOpacity=0.8, color=~color(allwords), 
                             popup=~paste(
                                 "<b>", name, "</b><br/>",
                                 "address: ", add, "<br/>",
                                 "website: ", web, "<br/>",
                                 "phone: ", phone, "<br/>",
                                 "status: ", status, "<br/>",
                                 "search term: ", keyword, "<br/>",
                                 "term count: ", allwords, "<br/>"),
                             group="farms") %>%
            addCircleMarkers(data=for_sale, lng=~lon, lat=~lat, radius=~allwords, stroke=FALSE,
                             fillOpacity=0.8, color=~color(allwords), 
                             popup=~paste(
                                 "<b>", name, "</b><br/>",
                                 "address: ", add, "<br/>",
                                 "website: ", web, "<br/>",
                                 "phone: ", phone, "<br/>",
                                 "status: ", status, "<br/>",
                                 "search term: ", keyword, "<br/>",
                                 "term count: ", allwords, "<br/>"),
                             group="for sale") %>%
            addCircleMarkers(data=hatchery, lng=~lon, lat=~lat, radius=~allwords, stroke=FALSE,
                             fillOpacity=0.8, color=~color(allwords), 
                             popup=~paste(
                                 "<b>", name, "</b><br/>",
                                 "address: ", add, "<br/>",
                                 "website: ", web, "<br/>",
                                 "phone: ", phone, "<br/>",
                                 "status: ", status, "<br/>",
                                 "search term: ", keyword, "<br/>",
                                 "term count: ", allwords, "<br/>"),
                             group="hatcheries") %>%
            addCircleMarkers(data=fair, lng=~lon, lat=~lat, radius=~allwords, stroke=FALSE,
                             fillOpacity=0.8, color=~color(allwords), 
                             popup=~paste(
                                 "<b>", name, "</b><br/>",
                                 "address: ", add, "<br/>",
                                 "website: ", web, "<br/>",
                                 "phone: ", phone, "<br/>",
                                 "status: ", status, "<br/>",
                                 "search term: ", keyword, "<br/>",
                                 "term count: ", allwords, "<br/>"),
                             group="fairs") %>%
            addCircleMarkers(data=market, lng=~lon, lat=~lat, radius=~allwords, stroke=FALSE,
                             fillOpacity=0.8, color=~color(allwords), 
                             popup=~paste(
                                 "<b>", name, "</b><br/>",
                                 "address: ", add, "<br/>",
                                 "website: ", web, "<br/>",
                                 "phone: ", phone, "<br/>",
                                 "status: ", status, "<br/>",
                                 "search term: ", keyword, "<br/>",
                                 "term count: ", allwords, "<br/>"),
                             group="markets") %>%
            addCircleMarkers(data=poultry_show, lng=~lon, lat=~lat, radius=~allwords, stroke=FALSE,
                             fillOpacity=0.8, color=~color(allwords), 
                             popup=~paste(
                                 "<b>", name, "</b><br/>",
                                 "address: ", add, "<br/>",
                                 "website: ", web, "<br/>",
                                 "phone: ", phone, "<br/>",
                                 "status: ", status, "<br/>",
                                 "search term: ", keyword, "<br/>",
                                 "term count: ", allwords, "<br/>"),
                             group="poultry shows") %>%
            addLegend("bottomleft", pal=color, values=~allwords, opacity = 1, title="Term Count") %>%
            addLayersControl(
                overlayGroups = c("for sale", "hatcheries", "fairs", "markets", "farms", "poultry shows"),
                options = layersControlOptions(collapsed = FALSE))
    })
    
    output$placestext<-renderText({paste("Note: Size of each dot represents the number of key terms detected in the reviews.",
                                         "The term \"for sale\" includes \"baby chicks for sale,\" \"baby ducklings for sale,\" and \"baby poultry for sale.\" ", 
                                         "The term \"hatcheries\" includes \"chick hatchery\" and \"poultry hatchery.\"", 
                                         "The term \"fairs\" includes \"county fair,\" \"farm fair,\" and \"state fair.\"",
                                         "The term \"markets\" includes \"farmers market\" and \"flea market.\"")})
    
    # Google Places Location within Radius
    output$data_dist2 <- DT::renderDataTable(datatable(
        distance(as.numeric(input$lat2),as.numeric(input$lon2),as.numeric(input$range2)),
        colnames = c("name","address","lat","lon","types","keyword","phone","website","status","distance"),
        options(searchDelay = 1),
        filter = 'top'
    ) )
}

# Run the application 
shinyApp(ui = ui, server = server)
