library(googleway)
library(data.table)
library(readxl)
library(dplyr)
library(janitor)
library(tidyr)

# Google Api key
key = ""

# Excel sheet containing coordinates for MD, VA and DE. Each coordinate cover area with 20 km radius.
centroids <- read_excel("G:/Shared drives/2020 FIRE-APHIS/DATA/Google Places/coordinates.xlsx")

# Search keyword
keywords <-
  c(
    "farms" ,
    "poultry show",
    "county fair",
    "state fair",
    "farm fair",
    "poultry hatchery",
    "chick hatchery",
    "flea market",
    "baby poultry for sale",
    "baby chicks for sale",
    "baby poults for sale",
    "baby ducklings for sale",
    "farmers market"
  )

# Automated procedure for all keywords and all coordinates and goes through all pages on Google Places.
for (i in 1:length(keywords)) {
  keyword <- keywords[i]
  final <- c()
  
  for (j in 1:nrow(centroids)) {
    temp_centroid <- centroids[j,]
    temp_coordinates <-
      separate(temp_centroid, coordinates, c("lat", "lon"), sep = ",")
    temp_coordinates$lat <- as.numeric(temp_coordinates$lat)
    temp_coordinates$lon <- as.numeric(temp_coordinates$lon)
    
    totalfarms <- c()
    
    for (k in 1:5) {
      if (k == 1) {
        farms <-
          google_places(
            search_string = keyword,
            location = c(temp_coordinates$lat, temp_coordinates$lon),
            radius = 20000,
            key = key
          )
      } else {
        farms <-
          google_places(
            search_string = keyword,
            location = c(temp_coordinates$lat, temp_coordinates$lon),
            radius = 20000,
            page_token = token,
            key = key
          )
      }
      
      token <- farms$next_page_token
      results <- as.data.frame(farms$results)
      lat <- results$geometry$location$lat
      lon <- results$geometry$location$lng
      add <- results$formatted_address
      name <- results$name
      place_id <- results$place_id
      types <- as.character(results$types)
      
      temp <-
        as.data.frame(cbind(name, add, lat, lon, place_id, types))
      
      totalfarms <- rbind(totalfarms, temp)
      
      Sys.sleep(1)
      
      if (is.null(token)) {
        break
      }
      
    }
    
    final <- rbind(final, totalfarms)
  }
  
  all <- unique(final)
  
  # Give output file name in path
  write.csv(all,
            file = paste(
              file.path(
                "",
                keyword
              ),
              ".csv"
            ),
            row.names = FALSE)
}


# Combining data from all keywords

baby_chicks_for_sale <-
  read.csv("baby chicks for sale .csv") %>%
  mutate(baby_chicks_for_sale = 1)

baby_ducklings_for_sale <-
  read.csv(
    "baby ducklings for sale .csv"
  ) %>%
  mutate(baby_ducklings_for_sale = 1)

baby_poultry_for_sale <-
  read.csv(
    "baby poultry for sale .csv"
  ) %>%
  mutate(baby_poultry_for_sale = 1)

baby_poults_for_sale <-
  read.csv("baby poults for sale .csv") %>%
  mutate(baby_poults_for_sale = 1)

chick_hatchery <-
  read.csv("chick hatchery .csv") %>%
  mutate(chick_hatchery = 1)

county_fair <-
  read.csv("county fair .csv") %>%
  mutate(county_fair = 1)

farm_fair <-
  read.csv("farm fair .csv") %>%
  mutate(farm_fair = 1)

farmers_market <-
  read.csv("farmers market .csv") %>%
  mutate(farmers_market = 1)

farms <-
  read_excel("farms.xlsx") %>%
  mutate(farms = 1)

flea_market <-
  read.csv("flea market .csv") %>%
  mutate(flea_market = 1)

poultry_hatchery <-
  read.csv("poultry hatchery .csv") %>%
  mutate(poultry_hatchery = 1)

poultry_show <-
  read.csv("poultry show .csv") %>%
  mutate(poultry_show = 1)

state_fair <-
  read.csv("state fair .csv") %>%
  mutate(state_fair = 1)


df <- Reduce(
  function(x, y)
    merge(x = x, y = y, all = TRUE),
  list(
    baby_chicks_for_sale,
    baby_ducklings_for_sale,
    baby_poultry_for_sale,
    baby_poults_for_sale,
    chick_hatchery,
    county_fair,
    farm_fair,
    farmers_market,
    farms,
    flea_market,
    poultry_hatchery,
    poultry_show,
    state_fair
  )
)

df2 <- df %>%
  filter(grepl('MD|VA|DE', add))

df2[is.na(df2)] <- 0

df3 <- df2 %>%
  mutate(
    keyword = paste0(
      ifelse(baby_chicks_for_sale == 1, "'baby chicks for sale' ", ""),
      ifelse(baby_ducklings_for_sale == 1, "'baby ducklings for sale' ", ""),
      ifelse(baby_poultry_for_sale == 1, "'baby poultry for sale' ", ""),
      ifelse(baby_poults_for_sale == 1, "'baby poults for sale' ", ""),
      ifelse(chick_hatchery == 1, "'chick hatchery' ", ""),
      ifelse(county_fair == 1, "'county fair' ", ""),
      ifelse(farm_fair == 1, "'farm fair' ", ""),
      ifelse(farmers_market == 1, "'farmers market' ", ""),
      ifelse(farms == 1, "'farms' ", ""),
      ifelse(flea_market == 1, "'flea market' ", ""),
      ifelse(poultry_hatchery == 1, "'poultry hatchery' ", ""),
      ifelse(poultry_show == 1, "'poultry show' ", ""),
      ifelse(state_fair == 1, "'state fair'", "")
    )
  )

df4 <- df3[, c(1, 2, 3, 4, 5, 6, 20)]

# writing final csv of all combined places
write.csv(df4, file = "places_combined.csv", row.names = FALSE)
