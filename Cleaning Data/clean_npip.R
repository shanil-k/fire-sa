library("readxl")
library("dplyr")
library("janitor")
library("tidyr")

# Path to excel file
path <- ""
tabs <- excel_sheets(path = path)

final <- c()

for (i in 1:length(tabs)) {
  df <- read_excel(path, sheet = tabs[i]) %>%
    clean_names() %>%
    remove_empty()
  
  formatted <- c()
  
  for (j in 1:nrow(df)) {
    df1 <- df[j, ]
    # Regex for extracting data rows
    rows <- grepl("\\d{3}", df1[, 1])
    df1 <- df1[rows, ]
    formatted <- rbind(formatted, df1)
  }
  
  if (i == 1 || i == 13 || i == 14) {
    col <- c("x1", "x2", "x3", "x4")
    formatted <- separate(formatted, 1, col, sep = "                     ")
    formatted <- formatted[, 2]
  } else {
    formatted <- formatted[, 3]
  }
  
  names(formatted) <- "firm"
  formatted <- remove_empty(formatted)
  
  if (i == 1 || i == 2 || i == 3 || i == 13) {
    formatted <- formatted %>%
      mutate(Hatchery = 1) %>%
      mutate(Independent = 0) %>%
      mutate(Dealer = 0)
  } else {
    formatted <- formatted %>%
      mutate(Hatchery = 0) %>%
      mutate(Independent = 1) %>%
      mutate(Dealer = 0)
  }
  formatted <- formatted %>%
    mutate(Dealer = 0)
  
  if(i == 1 || i == 2 || i == 3 || i == 13 || i == 14){
    formatted <- formatted %>%
      mutate(E = 0)
  } else {
    formatted <- formatted %>%
      mutate(E = 1)
  }
  
  final <- rbind(final, formatted)
  
}

final <-
  separate(final,
           1,
           into = c("Firm", "Phone"),
           sep = ":")

final <- final %>%
  mutate(Firm = str_remove_all(Firm, "Phone"))

# Output excel file
write_xlsx(final, "final.xlsx")
