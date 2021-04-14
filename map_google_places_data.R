library(rgdal)
library(raster)
library(tmap)
library(RColorBrewer)
library(tigris)

placesdf<-read.csv("places_reviews.csv") %>%
  mutate(TermCount=as.factor(allwords))

col1 <- rev(brewer.pal(n=10, "Spectral"))
col1 <- c(col1[1:4],col1[7:9])

# Shapefiles for plotting
info <- ogrInfo("cb_2018_us_state_500k/cb_2018_us_state_500k.shp", require_geomType = "wkbPolygon")
states<-readOGR("cb_2018_us_state_500k/cb_2018_us_state_500k.shp",info[["layer"]],require_geomType = "wkbPolygon")
idx<-(states@data$STUSPS=="VA" | states@data$STUSPS=="MD" | states@data$STUSPS=="DE")
dmv<-states[idx,]

cnty<-counties(state=c("VA", "MD", "DE"))

farms <-placesdf %>% filter(str_detect(keyword, 'farms')) %>%
  mutate(allwords=allwords+1)
mapdf<-farms
coordinates(mapdf)=~lon+lat

map<-tm_shape(cnty)+tm_borders("gray80")+
  tm_shape(mapdf)+tm_dots("TermCount", palette=col1,size="allwords")+
  tm_layout(legend.text.size=1, legend.title.size=2, attr.outside=TRUE, 
            attr.position = c("left", "bottom"),
            title="Keyword \"farms\"", title.size=2)

tmap_options(output.dpi = 500)
tmap_save(tm=map, filename="places_farm_map.png")

######################################################################
hatchery <-placesdf %>% filter(str_detect(keyword, 'hatchery'))
mapdf<-hatchery
coordinates(mapdf)=~lon+lat

map<-tm_shape(cnty)+tm_borders("gray80")+
  tm_shape(mapdf)+tm_dots("TermCount", palette=col1,size=0.1)+
  tm_layout(legend.text.size=1, legend.title.size=2, attr.outside=TRUE, 
            attr.position = c("left", "bottom"),
            title="Keyword \"hatchery\"", title.size=2)

tmap_options(output.dpi = 500)
tmap_save(tm=map, filename="places_hatchery_map.png")
