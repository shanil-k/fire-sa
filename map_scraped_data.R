library(mapsapi)
library(tigris)
library(rgeos)
library(tidyverse)
library(rgdal)
library(spdplyr)
library(data.table)
library(tmap)
library(RColorBrewer)

display.brewer.pal(n = 4, name = "Spectral")
display.brewer.pal(n = 4, name = "RdYlBu")
color <- rev(brewer.pal(n=4, "Spectral"))
colorpal <- c(color[1], color[3:4])

# Data source
df<-read.csv(".csv") %>%
  mutate("BYC" = 1) %>%
  mutate("CF" = 0)

# Data source
df2<-read.csv(".csv") %>%
  mutate("BYC" = 0) %>%
  mutate("CF" = 1)

df3 <- rbind(df, df2) %>%
  na.omit() %>%
  mutate_at(c("username","location"), as.character)

df4 <- df3 %>%
  filter(
    str_detect(location, "maryland") |
      str_detect(location, "Maryland") |
      str_detect(location, "MD") |
      str_detect(location, "virginia") |
      str_detect(location, "Virginia") |
      str_detect(location, "VA") |
      str_detect(location, "delaware") |
      str_detect(location, "Delaware") |
      str_detect(location, "DE")
  ) %>%
  filter(!str_detect(location, "West Virginia")) %>%
  filter(!str_detect(location, "WV")) %>%
  mutate(state = ifelse(
    str_detect(location, "maryland") |
      str_detect(location, "Maryland") |
      str_detect(location, "MD"),
    "Maryland",
    ifelse(
      str_detect(location, "virginia") |
        str_detect(location, "Virginia") |
        str_detect(location, "VA"),
      "Virginia",
      ifelse(
        str_detect(location, "delaware") |
          str_detect(location, "Delaware") |
          str_detect(location, "DE"),
        "Delaware",
        NA
      )
    )
  ))


members<-df4 %>% group_by(state) %>% tally() %>%
  mutate(n=as.factor(n))

df4$messages<-as.character(df4$messages)

df4<-df4 %>% mutate(messages=str_remove(messages,",")) %>% 
  mutate(messages=as.numeric(messages))

messages<-df4 %>% group_by(state) %>% summarize(messages=sum(messages)) %>%
  mutate(messages=as.factor(messages))

# Need shapefiles of US
info <- ogrInfo("Shapefiles/cb_2018_us_state_500k/cb_2018_us_state_500k.shp", require_geomType = "wkbPolygon")
states<-readOGR("Shapefiles/cb_2018_us_state_500k/cb_2018_us_state_500k.shp",info[["layer"]],require_geomType = "wkbPolygon")
states2<-states %>% filter(STUSPS=="MD" | STUSPS=="VA" | STUSPS=="DE")

stdf2 <- geo_join(states2, members, 'NAME', 'state')

map<-tm_shape(stdf2) + 
  tm_fill("n",palette=colorpal, alpha=0.85, title="Number of Members") +
  tm_layout(legend.text.size=2, legend.title.size=4)
tmap_options(output.dpi = 500)
tmap_save(map, "membersmap.png")

stdf2 <- geo_join(states2, messages, 'NAME', 'state')

map<-tm_shape(stdf2) + 
  tm_fill("messages",palette=colorpal, alpha=0.85, title="Number of Messages") +
  tm_layout(legend.text.size=2, legend.title.size=4)
tmap_options(output.dpi = 500)
tmap_save(map, "messagesmap.png")