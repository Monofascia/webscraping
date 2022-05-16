install.packages('rvest')
library(rvest)
library(dplyr)
library(xml2)

hot100page <- "https://www.billboard.com/charts/hot-100/" # link
hot100 <- read_html(hot100page) # leggo il link
hot100 # suddiviso in head e body, body contiene info che cerco

body_nodes <- hot100 %>% 
  html_node("body") %>% 
  html_children()
body_nodes # insieme di nodi interni al campo body!

body_nodes %>% html_children() # nodi figli

rank <- hot100 %>% 
  rvest::html_nodes("body") %>% 
  xml2::xml_find_all("//span[contains(@class, 'c-label  a-font-primary-bold-l u-font-size-32@tablet u-letter-spacing-0080@tablet')]") %>% 
  rvest::html_text() %>%
  as.numeric()

titleN1 <- hot100 %>%
  html_nodes("body") %>%
  xml_find_all("//h3[contains(@class, 'c-title  a-no-trucate a-font-primary-bold-s u-letter-spacing-0021 u-font-size-23@tablet lrv-u-font-size-16 u-line-height-125 u-line-height-normal@mobile-max a-truncate-ellipsis u-max-width-245 u-max-width-230@tablet-only u-letter-spacing-0028@tablet')]") %>%
  html_text() %>%
  as.character()
title <- hot100 %>%
  html_nodes("body") %>%
  xml_find_all("//h3[contains(@class, 'c-title  a-no-trucate a-font-primary-bold-s u-letter-spacing-0021 lrv-u-font-size-18@tablet lrv-u-font-size-16 u-line-height-125 u-line-height-normal@mobile-max a-truncate-ellipsis u-max-width-330 u-max-width-230@tablet-only')]") %>%
  html_text()
title <- append(titleN1, title)

artist <- hot100 %>%
  html_nodes("body") %>%
  xml_find_all("//span[contains(@class, 'c-label  a-no-trucate a-font-primary-s lrv-u-font-size-14@mobile-max u-line-height-normal@mobile-max u-letter-spacing-0021 lrv-u-display-block a-truncate-ellipsis-2line u-max-width-330 u-max-width-230@tablet-only')]") %>%
  html_text()

chart <- data.frame(rank, title, artist)

##### AUTOMATED ######
# se voglio cambiare data il link muta in questo modo:
# https://www.billboard.com/charts/hot-100/2022-03-05/
# quindi aggiunge solo la data dopo lo slash, automatizzabile

get_chart <- function(date = Sys.Date(), type = "hot-100") {
  
  # get url from input and read html
  input <- paste0("https://www.billboard.com/charts/", type, "/", date,"/") 
  chart_page <- xml2::read_html(input)
  
  
  # scrape data
  rank <- chart_page %>% 
    rvest::html_nodes("body") %>% 
    xml2::xml_find_all("//span[contains(@class, 'c-label  a-font-primary-bold-l u-font-size-32@tablet u-letter-spacing-0080@tablet')]") %>% 
    rvest::html_text() %>%
    as.numeric()
  
  artist <- chart_page %>%
    html_nodes("body") %>%
    xml_find_all("//span[contains(@class, 'c-label  a-no-trucate a-font-primary-s lrv-u-font-size-14@mobile-max u-line-height-normal@mobile-max u-letter-spacing-0021 lrv-u-display-block a-truncate-ellipsis-2line u-max-width-330 u-max-width-230@tablet-only')]") %>%
    html_text()
  
  titleN1 <- chart_page %>%
    html_nodes("body") %>%
    xml_find_all("//h3[contains(@class, 'c-title  a-no-trucate a-font-primary-bold-s u-letter-spacing-0021 u-font-size-23@tablet lrv-u-font-size-16 u-line-height-125 u-line-height-normal@mobile-max a-truncate-ellipsis u-max-width-245 u-max-width-230@tablet-only u-letter-spacing-0028@tablet')]") %>%
    html_text() %>%
    as.character()
  title <- chart_page %>%
    html_nodes("body") %>%
    xml_find_all("//h3[contains(@class, 'c-title  a-no-trucate a-font-primary-bold-s u-letter-spacing-0021 lrv-u-font-size-18@tablet lrv-u-font-size-16 u-line-height-125 u-line-height-normal@mobile-max a-truncate-ellipsis u-max-width-330 u-max-width-230@tablet-only')]") %>%
    html_text()
  title <- append(titleN1, title)
  
  
  # create dataframe, remove nas and return result
  chart_df <- data.frame(rank, artist, title)
  View(head(chart_df, n = 10L))
  
}

##### TEST #####
test <- get_chart(date = "2010-01-25", type = "hot-100")
test
# le top chart sono settimanali ogni Saturday