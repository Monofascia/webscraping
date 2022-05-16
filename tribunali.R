library(rvest)
library(dplyr)
library(xml2)

CodiceFiscaleTribunale <- function(cft = as.numeric()) {
  link <- "https://www.trascriviamo.it/cerca-codici-fiscali-tribunali-e-unep.html"
  page <- read_html(link)
  
  table <- page %>% html_nodes("table") %>% .[1] %>% html_table() %>% .[[1]]
  table %>% subset(`C.F. Tribunale` %in% cft) %>% View()
}

asd <- c(80028090829,82002030805, 
         86002040789, 80100020348,
         86002540838, 80015890181, 80005490547,
         860025408111) # vettore CF

cft = CodiceFiscaleTribunale(asd) # comando da lanciare
cft
