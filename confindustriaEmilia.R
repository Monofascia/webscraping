library(rvest)
library(dplyr)
library(xml2)

# tentativo 1 ------
table1 <- subset(table1, table1 != "ISTITUTO TECNICO INDUSTRIALE") 
table1 <- subset(table1, table1 != "CODICE FISCALE")
table1 <- data.frame(mycol = table1)
table1 %>% separate(mycol, 
           into = c("text", "num"), 
           sep = "(?<=[A-Za-z])(?=[0-9])")

# tentativo 2-----
library(stringr)
my.data.num <- as.numeric(str_extract(table1, "[0-9]+"))
my.data.num # toglie gli zeri iniziali
my.data.cha <- (str_extract(my.data, "[aA-zZ]+"))
my.data.cha

# tentativo 3----
library(readr)
number <- parse_number(table1) %>% as.data.frame() # salta gil zeri
character <- parse_character(table1) %>% as.data.frame()

# tentativo 4 ----
splitted <- strsplit(table1, "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)
#inutile

# tentativo 5 ----- 
#OK
table1 <- subset(table1, table1 != "ISTITUTO TECNICO INDUSTRIALE") 
table1 <- subset(table1, table1 != "CODICE FISCALE")
number <- gsub('\\D','', table1)  
number <- subset(number, number != "")     # replaces non-digits with blancs
name <- gsub('\\d','', table1)       # replaces digits with blanks
name <- subset(name, name != "")
df <- data.frame(number, name)

# FUNCTION ---------
get_chart <- function(cfiscale = as.character(), nome = as.character()) {
  link <- "https://www.confindustriaemilia.it/cfisc"
  page <- read_html(link)
  table1 <- page %>% html_node("body") %>% 
    html_node("table") %>% 
    xml_find_all("//div[contains(@class, 'viewTableCellText')]") %>%
    html_text()
  table1 <- subset(table1, table1 != "ISTITUTO TECNICO INDUSTRIALE") 
  table1 <- subset(table1, table1 != "CODICE FISCALE")
  number <- gsub('\\D','', table1)  
  number <- subset(number, number != "")     # replaces non-digits with blanks
  name <- gsub('\\d','', table1)       # replaces digits with blanks
  name <- subset(name, name != "")
  df <- data.frame(number, name)
  
  if (cfiscale != "") {
    df %>% subset(number %in% cfiscale) %>% View()
  }else{
    df %>% subset(name %in% nome) %>% View()
  }
  
}


# insert value between quotes
get_chart(cfiscale = "94058180368", nome =  "") 
# inutile il nome perchè richiede la ricerca completa e puntuale (case sensitive)











# Scraping within comments "<!-- -->" -----

# non eseguito perchè non è commentato

link <- "https://www.confindustriaemilia.it/cfisc"
page <- read_html(link)
comments <- page %>% xml_find_all("//comment()")


# tentativo 1 ----- 
# questo approccio si comprende facile
# testando ho scoperto cosa significa commentato
df <- page %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
  html_text() %>%    # extract comment text
  paste(collapse = '') %>%    # collapse to a single string
  read_html() %>%    # reparse to HTML
  html_node('inserire testo qui') %>%    # select the desired table
  html_table() %>%    # parse table
  .[colSums(is.na(.)) < nrow(.)]    # get rid of spacer columns

page %>% html_node('#blobTable-3')

# tentativo 2 ------
# anche questo metodo è molto efficente
url %>% read_html() %>%                   # parse html
  html_nodes('#all_team_stats') %>%     # select node with comment
  html_nodes(xpath = 'comment()') %>%   # select comments within node
  html_text() %>%                       # return contents as text
  read_html() %>%                       # parse text as html
  html_node('table') %>%                # select table node
  html_table()                          # parse table and return data.frame
# tentativo 3 -----
alt_tables <- xml2::xml_find_all(page,"//comment()") %>% {
  #Find only commented nodes that contain the regex for html table markup
  raw_parts <- as.character(.[grep("\\</?table", as.character())])
  # Remove the comment begin and end tags
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  # Loop through the pieces that have tables within markup and 
  # apply the same functions
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
}

alt_tables
