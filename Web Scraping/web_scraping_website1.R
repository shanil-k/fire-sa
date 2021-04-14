library("rvest")

# Always respect robots.txt while scraping 
# URL for website to scrape
main_url <- "https://www.backyardchickens.com/members/"
html <- read_html(main_url)
temp <- html_nodes(html, ".block-textHeader")
list <- html_children(temp)
url_list <- html_attr(list, "href") %>%
  url_absolute(main_url)

data <- c()

for (i in 1:length(url_list)) {
  url <- url_list[i]
  html <- read_html(url)
  # Node which contains the data we want
  temp_block <- html_nodes(html, ".block-row--separated")
  temp_data <- c()
  
  for (j in 1:length(temp_block)) {
    # Sub-Node which contains the data we want
    temp_name <- html_nodes(temp_block[[j]], ".username")
    name <- html_text(temp_name, trim = TRUE) %>%
      data.frame
    
    # Sub-Node which contains the data we want
    temp_locations <- html_nodes(temp_block[[j]], ".u-concealed")
    
    if (length(temp_locations) == 0) {
      locations[1, ] <- "NA"
    } else {
      locations <- html_text(temp_locations, trim = TRUE) %>%
        data.frame
    }
    
    temp_activity <- html_nodes(temp_block[[j]], "dd")
    
    activity <- html_text(temp_activity, trim = TRUE) %>%
      data.frame
    
    activity <- activity[1, ]
    temp_data <- rbind(temp_data, cbind(name, locations, activity))
  }
  
  data <- rbind(data, temp_data)
}

names(data) <- c("username", "location", "messages")

# Removing duplicate entries
final <- unique(data)

# Output csv, give location in file = ""
write.csv(final, file = "", row.names = FALSE)
